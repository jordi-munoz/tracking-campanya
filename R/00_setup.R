# 00_setup.R -------------------------------------------------------------
# Packages, paths, codings and helper functions shared by all scripts.

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(readr)
  library(stringr)
  library(forcats)
  library(ggplot2)
  library(survey)
  library(estimatr)
  library(here)
})

options(dplyr.summarise.inform = FALSE, survey.lonely.psu = "adjust")
invisible(Sys.setlocale("LC_TIME", "C"))

# Paths -----------------------------------------------------------------
dir_raw    <- here("data", "raw")
dir_proc   <- here("data", "processed")
dir_fig    <- here("output", "figures")
dir_tab    <- here("output", "tables")
for (d in c(dir_proc, dir_fig, dir_tab)) dir.create(d, showWarnings = FALSE, recursive = TRUE)

source(here("R", "fieldwork.R"))

blocks <- read_csv(here("config", "fieldwork_blocks.csv"), show_col_types = FALSE) |>
  mutate(mid = start + (end - start) / 2)

# Each event's phase (before, between surveys, during fieldwork, after the data)
# is derived from the block dates, so new tracking days pull in later events.
events <- read_csv(here("events", "events_29N.csv"), show_col_types = FALSE) |>
  mutate(phase = event_phase(date, blocks))

# Codings ---------------------------------------------------------------
# Vote intention (p2) / sympathy (p3) codes, identical in tracking and preelectoral.
party_codes <- c(psoe = 1, pp = 2, vox = 3, fa = 4, podemos = 5, salf = 6)
party_names <- c(psoe = "PSOE", pp = "PP", vox = "Vox", fa = "Frente Amplio",
                 podemos = "Podemos", salf = "SALF")
# Party colours, adjusted from the brand colours to pass lightness, contrast and
# colour-blind separation checks on a light background.
party_cols  <- c(PSOE = "#E30613", PP = "#1D84CE", Vox = "#4E9A1A",
                 `Frente Amplio` = "#D81B7A", Podemos = "#8A4BB0", SALF = "#7F7F7F")

# p2 codes 1-20 are parties/other; 21 blank, 22 null, 23 would not vote,
# 24 don't know, 25 no answer.
# Recalled vote 2023 (p5): 1 PSOE, 2 PP, 3 Vox, 4 Sumar, 5-15 nationalist/regionalist,
# 16 other, 17 blank, 18 null, 19 did not vote, 20 too young, 21 DK, 22 NA.
recode_recall <- function(x) {
  case_when(
    x == 1 ~ "PSOE", x == 2 ~ "PP", x == 3 ~ "Vox", x == 4 ~ "Sumar",
    x %in% 5:15 ~ "Nationalist/regional",
    x %in% 16:20 ~ "Other/blank/abstained",
    TRUE ~ "DK/NA"
  ) |>
    factor(levels = c("PSOE", "PP", "Vox", "Sumar", "Nationalist/regional",
                      "Other/blank/abstained", "DK/NA"))
}

na_99 <- function(x) ifelse(x == 99, NA_real_, as.numeric(x))

# Builds analysis variables from harmonised raw columns (tracking numbering).
derive_vars <- function(d) {
  d |>
    mutate(
      # item non-response, before 99 is recoded to NA (PTVs also have structural
      # NAs where a party does not run, so only code 99 counts as don't know)
      nr_vote     = as.integer(p2 == 25),
      dk_turnout  = as.integer(p1 %in% 99),
      dk_ideology = as.integer(p6 %in% 99),
      dk_ptv      = as.integer(if_any(paste0("p4_", 1:5), ~ .x %in% 99)),
      across(c(p1, p6, starts_with("p4_")), na_99),
      # direct vote intention, share of all respondents
      vi_psoe = as.integer(p2 == 1), vi_pp = as.integer(p2 == 2),
      vi_vox  = as.integer(p2 == 3), vi_fa = as.integer(p2 == 4),
      vi_podemos = as.integer(p2 == 5), vi_salf = as.integer(p2 == 6),
      undecided  = as.integer(p2 == 24),
      abstain    = as.integer(p2 == 23),
      # vote + sympathy among undecided ("voto + simpatía")
      vs = if_else(p2 %in% 1:20, p2, if_else(!is.na(p3) & p3 %in% 1:20, p3, NA_real_)),
      vs_psoe = as.integer(vs %in% 1), vs_pp = as.integer(vs %in% 2),
      vs_vox  = as.integer(vs %in% 3), vs_fa = as.integer(vs %in% 4),
      vs_podemos = as.integer(vs %in% 5),
      turnout = p1,                        # 0-10 probability of voting
      likely  = as.integer(p1 >= 9),
      ptv_psoe = p4_1, ptv_pp = p4_2, ptv_vox = p4_3, ptv_fa = p4_4, ptv_podemos = p4_5,
      ideology = p6,
      bestpm_sanchez = as.integer(p7 == 1), bestpm_feijoo = as.integer(p7 == 2),
      bestpm_abascal = as.integer(p7 == 3), bestpm_colau = as.integer(p7 == 4),
      bestpm_montero = as.integer(p7 == 5),
      gov_psoe_fa = as.integer(p8 == 1), gov_pp_vox = as.integer(p8 == 5),
      gov_grand = as.integer(p8 == 3),
      recall = recode_recall(p5),
      # recalled vote dummies: used as placebo outcomes (fixed before fieldwork)
      rec_psoe = as.integer(p5 == 1), rec_pp = as.integer(p5 == 2),
      rec_vox = as.integer(p5 == 3), rec_sumar = as.integer(p5 == 4),
      rec_abst = as.integer(p5 == 19),
      female = as.integer(sexo == 2),
      sex_age = factor(paste(as.numeric(sexo), as.numeric(edad_r), sep = "_")),
      across(c(edad_r, hab, ccaa, educacion_r, clase_social_r), ~ factor(as.numeric(.x)))
    )
}

outcome_labels <- c(
  vi_psoe = "Vote intention: PSOE", vi_pp = "Vote intention: PP",
  vi_vox = "Vote intention: Vox", vi_fa = "Vote intention: Frente Amplio",
  vi_podemos = "Vote intention: Podemos", vi_salf = "Vote intention: SALF",
  undecided = "Undecided", abstain = "Would not vote",
  turnout = "Turnout probability (0-10)", likely = "Likely voter (9-10)",
  ptv_psoe = "PTV PSOE", ptv_pp = "PTV PP", ptv_vox = "PTV Vox",
  ptv_fa = "PTV Frente Amplio", ptv_podemos = "PTV Podemos",
  bestpm_sanchez = "Best PM: Sánchez", bestpm_feijoo = "Best PM: Feijóo",
  bestpm_abascal = "Best PM: Abascal", bestpm_colau = "Best PM: Colau",
  gov_psoe_fa = "Prefers PSOE-FA govt", gov_pp_vox = "Prefers PP-Vox govt",
  rec_psoe = "Recall 2023: PSOE", rec_pp = "Recall 2023: PP",
  rec_vox = "Recall 2023: Vox", rec_sumar = "Recall 2023: Sumar",
  rec_abst = "Recall 2023: abstained"
)

# Helpers ---------------------------------------------------------------

# Row fingerprint used to track respondents across cumulative tracking files
# (the `id` variable is renumbered in every file and weights are recomputed).
row_key <- function(d) {
  d <- zap_labels(select(d, -any_of(c("id", "ponde"))))
  do.call(paste, c(d, sep = "|"))
}

# Raking variables (40dB quotas: sex x age, region, municipality size, social class).
rake_vars <- c("sex_age", "ccaa", "hab", "clase_social_r")

# Population margins as proportions, from a weighted reference sample.
rake_targets <- function(d, w) {
  set_names(lapply(rake_vars, function(v) {
    tibble(!!v := d[[v]], w = w) |> count(.data[[v]], wt = w, name = "prop") |>
      mutate(prop = prop / sum(prop))
  }), rake_vars)
}

# Raking to fixed population margins; returns weights with mean 1.
rake_weights <- function(d, targets) {
  des <- svydesign(ids = ~1, data = d, weights = ~1)
  pop <- lapply(targets, function(t) {
    t <- filter(t, .data[[names(t)[1]]] %in% unique(d[[names(t)[1]]]))
    mutate(t, Freq = prop / sum(prop) * nrow(d)) |> select(-prop)
  })
  r <- rake(des, sample.margins = lapply(rake_vars, function(v) as.formula(paste0("~", v))),
            population.margins = pop, control = list(maxit = 200, epsilon = 1e-6))
  w <- weights(r)
  w / mean(w)
}

# Position of a fieldwork block relative to an event date.
# "pre"    : block ended before the event day
# "during" : event day falls inside the block (partially exposed)
# "post"   : block started after the event day
event_exposure <- function(start, end, event_date) {
  case_when(end < event_date ~ "pre",
            start > event_date ~ "post",
            TRUE ~ "during")
}

controls_demo   <- "female + edad_r + hab + ccaa + educacion_r + clase_social_r"
controls_recall <- paste(controls_demo, "+ recall")

# Weighted linear model with HC2 SEs; returns tidy row for `term`.
fit_lm <- function(formula, data, term, weights = "w") {
  data$.w <- data[[weights]]
  m <- lm_robust(as.formula(formula), data = data, weights = .w, se_type = "HC2")
  tidy(m) |> filter(.data$term == !!term) |> mutate(n = nobs(m))
}

theme_set(theme_minimal(base_size = 11) +
            theme(panel.grid.minor = element_blank(), legend.position = "bottom"))
