# 05_uesd_checks.R -------------------------------------------------------
# Diagnostics for the Unexpected Event during Surveys Design (Muñoz,
# Falcó-Gimeno & Hernández 2020, Political Analysis 28(2): 186-206):
#   1. balance of pre-treatment covariates between pre and post groups
#   2. item non-response before/after each event
#   3. bandwidths, sample sizes and statistical power
#   4. stability of outcomes in the pre-event period (block 0 vs block 1)
# Run after 03_event_analysis.R.

source(here::here("R", "00_setup.R"))
dat <- read_rds(file.path(dir_proc, "respondents.rds")) |>
  mutate(
    region = case_when(ccaa %in% c("1") ~ "Andalusia", ccaa == "9" ~ "Catalonia",
                       ccaa == "13" ~ "Madrid", ccaa == "10" ~ "Valencia",
                       TRUE ~ "Rest of Spain")
  )
primary <- read_csv(file.path(dir_tab, "event_effects_primary.csv"), show_col_types = FALSE)
allspecs <- read_csv(file.path(dir_tab, "event_effects_all_specs.csv"), show_col_types = FALSE)

event_days <- primary |> distinct(date, events, spec)

primary_sample <- function(event_date, spec) {
  surveys <- if (spec == "tracking") "tracking" else c("tracking", "preelectoral_I")
  dat |>
    filter(survey %in% surveys) |>
    mutate(exposure = event_exposure(start, end, event_date)) |>
    filter(exposure != "during") |>
    mutate(post = as.integer(exposure == "post"))
}

# 1. Balance -------------------------------------------------------------
# Quota variables are balanced by construction once each block is raked, so
# the informative rows are the non-quota covariates (education, employment,
# children) and recalled 2023 vote.
dummies <- function(d, var, prefix) {
  lv <- sort(unique(as.character(d[[var]])))
  set_names(map(lv, ~ as.integer(as.character(d[[var]]) == .x)), paste0(prefix, lv))
}
balance_vars <- function(d) {
  bind_cols(
    tibble(female = d$female, age = d$edad),
    as_tibble(dummies(d, "educacion_r", "educ_")),
    as_tibble(dummies(d, "situacion_laboral_r", "work_")),
    as_tibble(dummies(d, "hijos", "children_")),
    as_tibble(dummies(d, "clase_social_r", "class_")),
    as_tibble(dummies(d, "region", "region_")),
    select(d, rec_psoe, rec_pp, rec_vox, rec_sumar, rec_abst)
  )
}
covariate_type <- function(v) case_when(
  str_starts(v, "rec_") ~ "Recalled vote 2023",
  v %in% c("female", "age") | str_starts(v, "class_|region_") ~ "Quota variable",
  TRUE ~ "Non-quota covariate")

balance <- pmap_dfr(event_days, function(date, events, spec) {
  d <- primary_sample(date, spec)
  X <- balance_vars(d)
  rows <- map_dfr(names(X), function(v) {
    x <- X[[v]]; w <- d$w; p <- d$post == 1
    m1 <- weighted.mean(x[p], w[p]); m0 <- weighted.mean(x[!p], w[!p])
    sd_pool <- sqrt((var(x[p]) + var(x[!p])) / 2)
    fit <- lm_robust(x ~ post, data = mutate(d, x = x), weights = w, se_type = "HC2")
    tibble(covariate = v, mean_pre = m0, mean_post = m1,
           std_diff = if (sd_pool > 0) (m1 - m0) / sd_pool else 0,
           p.value = tidy(fit)$p.value[2])
  })
  # joint test: do the non-quota covariates and recall predict being post?
  Xj <- select(X, -starts_with("class_"), -starts_with("region_"), -female, -age)
  Xj <- Xj[, colSums(Xj) > 0 & map_lgl(Xj, ~ var(.x) > 0)]
  Xj <- Xj[, !duplicated(t(Xj))]
  # drop one category per factor to avoid perfect collinearity
  Xj <- select(Xj, -any_of(c("educ_1", "work_1", "children_1")))
  jd <- bind_cols(post = d$post, w = d$w, Xj)
  fj <- lm_robust(post ~ ., data = select(jd, -w), weights = jd$w, se_type = "HC2")
  k <- setdiff(names(coef(fj)), "(Intercept)"); k <- k[!is.na(coef(fj)[k])]
  W <- as.numeric(t(coef(fj)[k]) %*% solve(vcov(fj)[k, k]) %*% coef(fj)[k])
  rows |>
    mutate(date = date, events = events, spec = spec, type = covariate_type(covariate),
           joint_chi2 = W, joint_df = length(k), joint_p = pchisq(W, length(k), lower.tail = FALSE))
})
write_csv(balance, file.path(dir_tab, "uesd_balance.csv"))
message("Joint balance tests (non-quota covariates + recall):")
balance |> distinct(date, events, joint_chi2, joint_df, joint_p) |> print()

# 2. Item non-response ----------------------------------------------------
nr_vars <- c(nr_vote = "Refuses vote intention", dk_turnout = "DK turnout probability",
             dk_ideology = "DK/NA ideology", dk_ptv = "DK on any national-party PTV")
nonresponse <- pmap_dfr(event_days, function(date, events, spec) {
  d <- primary_sample(date, spec)
  map_dfr(names(nr_vars), function(v) {
    fit_lm(paste(v, "~ post +", controls_demo), d, "post") |>
      transmute(variable = v, label = nr_vars[v], estimate, std.error, p.value,
                pre_mean = weighted.mean(d[[v]][d$post == 0], d$w[d$post == 0]))
  }) |> mutate(date = date, events = events)
})
write_csv(nonresponse, file.path(dir_tab, "uesd_item_nonresponse.csv"))

# 3. Bandwidths and power -------------------------------------------------
# Sample windows only (the covariate variants are kept out of this table).
power_two_sided <- function(delta, se) pnorm(abs(delta) / se - 1.96) + pnorm(-abs(delta) / se - 1.96)
bandwidths <- allspecs |>
  filter(spec %in% c("adjacent_blocks", "tracking", "during_as_post", "with_preelectoral"),
         type == "outcome", !str_starts(outcome, "ptv_")) |>
  mutate(power_3pp = power_two_sided(0.03, std.error),
         power_5pp = power_two_sided(0.05, std.error),
         spec = factor(spec, levels = c("adjacent_blocks", "tracking", "during_as_post", "with_preelectoral")))
write_csv(bandwidths, file.path(dir_tab, "uesd_bandwidths_power.csv"))

# 4. Pre-period stability: Preelectoral I (2-4 Nov) vs block 1 (10-12 Nov) -----
# Not a clean placebo (E03-E05 fall in between and the instrument differs),
# but large differences warn that the block-1 baseline may be atypical.
pre_stab_outcomes <- c("vi_psoe", "vi_pp", "vi_vox", "vi_fa", "vi_podemos", "undecided", "likely",
                       "bestpm_sanchez", "bestpm_feijoo", "bestpm_abascal",
                       "rec_psoe", "rec_pp", "rec_vox", "rec_sumar")
pre_stability <- map_dfr(pre_stab_outcomes, function(o) {
  d <- filter(dat, block %in% 0:1) |> mutate(b1 = as.integer(block == 1))
  fit_lm(paste(o, "~ b1 +", controls_demo), d, "b1") |>
    transmute(outcome = o, label = outcome_labels[o], estimate, std.error, p.value)
})
write_csv(pre_stability, file.path(dir_tab, "uesd_pre_period_stability.csv"))
print(pre_stability)
