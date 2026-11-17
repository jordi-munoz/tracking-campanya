# 03_event_analysis.R ----------------------------------------------------
# Before/after comparisons around each campaign event.
#
# Design: the tracking is a sequence of independent daily samples drawn with
# the same quotas, so respondents interviewed after an event are compared with
# respondents interviewed before it (an "unexpected event during survey"
# design). Each block spans two calendar days and interview times are not in the
# data, so a block is "pre" only if it ended before the event day and "post"
# only if it started after it; blocks containing the event day are "during"
# and dropped (or coded as post in a robustness check). Events on the same day
# cannot be separated and are analysed jointly as an "event day".
#
# Identification rests on (a) no other change between pre and post blocks and
# (b) comparable samples across blocks. (b) is checked with placebo outcomes:
# recalled 2023 vote cannot be affected by the campaign.

source(here::here("R", "00_setup.R"))
dat <- read_rds(file.path(dir_proc, "respondents.rds"))

core_outcomes <- c("vi_psoe", "vi_pp", "vi_vox", "vi_fa", "vi_podemos", "undecided", "likely",
                   "ptv_psoe", "ptv_pp", "ptv_vox", "ptv_fa",
                   "bestpm_sanchez", "bestpm_feijoo", "bestpm_abascal", "bestpm_colau")
placebo_outcomes <- c("rec_psoe", "rec_pp", "rec_vox", "rec_sumar", "rec_abst")

# Event days within or just before fieldwork -----------------------------
event_days <- events |>
  filter(phase %in% c("between_surveys", "fieldwork")) |>
  group_by(date) |>
  summarise(events = paste(event_id, collapse = ", "),
            labels = paste(label, collapse = "; "),
            hyp_outcomes = paste(na.omit(outcomes), collapse = ";"))

# Specifications ---------------------------------------------------------
# Primary covariates are quota variables plus education, all pre-treatment.
# Recalled 2023 vote is not: recall drifts towards current preference, so an
# event that moves intention can also move recall (post-treatment bias,
# Montgomery et al. 2018). It enters only as a robustness check and as a placebo.
specs <- tribble(
  ~spec,                  ~surveys,                         ~during,  ~controls,        ~weights, ~drop_dups, ~adjacent,
  "tracking",             "tracking",                       "drop",   controls_demo,    "w",      FALSE,      FALSE,
  "with_preelectoral",    c("tracking", "preelectoral_I"),  "drop",   controls_demo,    "w",      FALSE,      FALSE,
  "during_as_post",       "tracking",                       "post",   controls_demo,    "w",      FALSE,      FALSE,
  "adjacent_blocks",      "tracking",                       "drop",   controls_demo,    "w",      FALSE,      TRUE,
  "with_recall_controls", "tracking",                       "drop",   controls_recall,  "w",      FALSE,      FALSE,
  "no_controls",          "tracking",                       "drop",   "1",              "w",      FALSE,      FALSE,
  "unweighted",           "tracking",                       "drop",   controls_demo,    "one",    FALSE,      FALSE,
  "drop_duplicates",      "tracking",                       "drop",   controls_demo,    "w",      TRUE,       FALSE
)

event_sample <- function(d, event_date, s) {
  d <- d |>
    filter(survey %in% s$surveys[[1]]) |>
    mutate(exposure = event_exposure(start, end, event_date), one = 1)
  if (s$during == "post") d <- mutate(d, exposure = if_else(exposure == "during", "post", exposure))
  d <- filter(d, exposure != "during")
  if (s$drop_dups) d <- filter(d, !dup_copy)
  if (s$adjacent && all(c("pre", "post") %in% d$exposure)) {
    last_pre   <- max(d$block[d$exposure == "pre"])
    first_post <- min(d$block[d$exposure == "post"])
    d <- filter(d, block %in% c(last_pre, first_post))
  }
  mutate(d, post = as.integer(exposure == "post"))
}

estimate_event <- function(event_date, outcome, s) {
  d <- event_sample(dat, event_date, s) |> filter(!is.na(.data[[outcome]]))
  if (n_distinct(d$post) < 2) return(NULL)
  fit_lm(paste(outcome, "~ post +", s$controls), d, "post", weights = s$weights) |>
    transmute(estimate, std.error, p.value, conf.low, conf.high, n,
              n_pre = sum(d$post == 0), n_post = sum(d$post == 1),
              pre_blocks = paste(sort(unique(d$block[d$post == 0])), collapse = ","),
              post_blocks = paste(sort(unique(d$block[d$post == 1])), collapse = ","),
              pre_mean = weighted.mean(d[[outcome]][d$post == 0], d$w[d$post == 0]))
}

grid <- crossing(date = event_days$date, outcome = c(core_outcomes, placebo_outcomes),
                 spec = specs$spec)

results <- grid |>
  mutate(res = pmap(list(date, outcome, spec), function(dt, o, sp) {
    s <- filter(specs, spec == sp)
    # placebo outcomes are recall itself: never control for recall
    if (o %in% placebo_outcomes && s$controls == controls_recall) s$controls <- controls_demo
    estimate_event(dt, o, s)
  })) |>
  unnest(res) |>
  left_join(event_days, by = "date") |>
  mutate(type = if_else(outcome %in% placebo_outcomes, "placebo", "outcome"),
         hypothesised = map2_lgl(outcome, hyp_outcomes, ~ .x %in% str_split_1(.y, ";")),
         label = outcome_labels[outcome],
         mde80 = 2.8 * std.error)   # minimum detectable effect, 80% power, alpha .05

# Primary specification: tracking-only when the event has pre and post tracking
# blocks; otherwise (events before 12 Nov) the preelectoral survey must serve as
# the pre-period.
primary_spec <- results |>
  distinct(date, spec) |>
  group_by(date) |>
  summarise(primary = if ("tracking" %in% spec) "tracking" else "with_preelectoral")
results <- results |>
  left_join(primary_spec, by = "date") |>
  mutate(is_primary = spec == primary) |>
  group_by(spec, type) |>
  mutate(q.value = p.adjust(p.value, method = "BH")) |>
  ungroup() |>
  select(date, events, spec, is_primary, type, outcome, label, hypothesised, estimate, std.error,
         conf.low, conf.high, p.value, q.value, mde80, pre_mean, n, n_pre, n_post,
         pre_blocks, post_blocks, labels)

write_csv(results, file.path(dir_tab, "event_effects_all_specs.csv"))
primary <- filter(results, is_primary)
write_csv(primary, file.path(dir_tab, "event_effects_primary.csv"))

message("Event days analysed (primary spec, pre vs post blocks):")
primary |> distinct(date, events, spec, pre_blocks, post_blocks, n_pre, n_post) |> print()
message("Placebo tests (recalled vote) with p < .05 in primary spec: ",
        sum(primary$type == "placebo" & primary$p.value < .05), " of ", sum(primary$type == "placebo"))

# Figure: primary estimates ---------------------------------------------
plot_dat <- primary |>
  mutate(event_day = paste0(format(date, "%d %b"), "\n", events),
         event_day = fct_reorder(event_day, as.numeric(date)),
         label = factor(label, levels = rev(outcome_labels[c(core_outcomes, placebo_outcomes)])),
         scale = if_else(str_starts(outcome, "ptv_"), "PTV (0-10 points)", "Proportion (pp)"),
         across(c(estimate, conf.low, conf.high), ~ if_else(scale == "Proportion (pp)", .x * 100, .x)),
         status = case_when(type == "placebo" ~ "Placebo (recalled 2023 vote)",
                            hypothesised ~ "Hypothesised outcome",
                            TRUE ~ "Other outcome"))

p_ev <- ggplot(filter(plot_dat, scale == "Proportion (pp)"),
               aes(estimate, label, colour = status)) +
  geom_vline(xintercept = 0, colour = "grey60") +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high), linewidth = 0.5) +
  geom_point(size = 1.8) +
  facet_wrap(~event_day, nrow = 1) +
  scale_colour_manual(values = c(`Hypothesised outcome` = "#D81B7A", `Other outcome` = "grey35",
                                 `Placebo (recalled 2023 vote)` = "#1D84CE"), name = NULL) +
  labs(x = "Post - pre difference (percentage points, 95% CI)", y = NULL,
       title = "Before/after differences around campaign event days",
       subtitle = paste("Weighted LPM with quota and education controls, HC2 SEs.",
                        "Pre = preelectoral survey when no earlier tracking block exists."))
ggsave(file.path(dir_fig, "event_effects_primary.png"), p_ev, width = 13, height = 6.5, dpi = 300, bg = "white")

# Figure: robustness for hypothesised outcomes ---------------------------
rob <- results |>
  filter(hypothesised, type == "outcome", !str_starts(outcome, "ptv_")) |>
  mutate(event_day = paste(format(date, "%d %b"), events),
         panel = paste0(event_day, "\n", label),
         spec = factor(spec, levels = rev(specs$spec)))
p_rob <- ggplot(rob, aes(estimate * 100, spec, colour = is_primary)) +
  geom_vline(xintercept = 0, colour = "grey60") +
  geom_linerange(aes(xmin = conf.low * 100, xmax = conf.high * 100)) +
  geom_point(size = 1.6) +
  facet_wrap(~panel, ncol = 4) +
  scale_colour_manual(values = c(`TRUE` = "#D81B7A", `FALSE` = "grey40"),
                      labels = c(`TRUE` = "Primary", `FALSE` = "Robustness"), name = NULL) +
  labs(x = "Post - pre difference (pp, 95% CI)", y = NULL,
       title = "Robustness of event estimates for hypothesised outcomes")
ggsave(file.path(dir_fig, "event_effects_robustness.png"), p_rob, width = 12,
       height = 2 + 1.6 * ceiling(n_distinct(rob$panel) / 4), dpi = 300, bg = "white", limitsize = FALSE)

# Block trajectories (event-study style) ----------------------------------
# Each block vs block 1 (10-12 Nov), adjusting for quota variables and education.
trajectory <- map_dfr(c(core_outcomes, placebo_outcomes), function(o) {
  ctrl <- controls_demo
  d <- filter(dat, !is.na(.data[[o]])) |> mutate(block_f = relevel(factor(block), ref = "1"))
  m <- lm_robust(as.formula(paste(o, "~ block_f +", ctrl)), data = d, weights = w, se_type = "HC2")
  tidy(m) |> filter(str_starts(term, "block_f")) |>
    mutate(block = as.numeric(sub("block_f", "", term)), outcome = o)
}) |>
  left_join(blocks, by = "block") |>
  mutate(label = outcome_labels[outcome])
write_csv(trajectory, file.path(dir_tab, "block_trajectories_vs_block1.csv"))

# Omnibus test: does the outcome change at all across tracking blocks?
omnibus <- map_dfr(c(core_outcomes, placebo_outcomes), function(o) {
  ctrl <- controls_demo
  d <- filter(dat, survey == "tracking", !is.na(.data[[o]])) |> mutate(block_f = factor(block))
  m1 <- lm_robust(as.formula(paste(o, "~ block_f +", ctrl)), data = d, weights = w, se_type = "HC2")
  k <- grep("^block_f", names(coef(m1)), value = TRUE)
  b <- coef(m1)[k]; V <- vcov(m1)[k, k]
  W <- as.numeric(t(b) %*% solve(V) %*% b)
  tibble(outcome = o, label = outcome_labels[o], wald_chi2 = W, df = length(k),
         p.value = pchisq(W, length(k), lower.tail = FALSE))
}) |>
  mutate(q.value = p.adjust(p.value, "BH"))
write_csv(omnibus, file.path(dir_tab, "omnibus_block_differences.csv"))
print(omnibus, n = Inf)
