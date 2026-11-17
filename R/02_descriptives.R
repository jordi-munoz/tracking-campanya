# 02_descriptives.R ------------------------------------------------------
# Weighted daily series by fieldwork block and sample-composition checks.

source(here::here("R", "00_setup.R"))
dat <- read_rds(file.path(dir_proc, "respondents.rds"))

series_vars <- c("vi_psoe", "vi_pp", "vi_vox", "vi_fa", "vi_podemos", "vi_salf",
                 "undecided", "abstain", "likely", "turnout",
                 "ptv_psoe", "ptv_pp", "ptv_vox", "ptv_fa", "ptv_podemos",
                 "bestpm_sanchez", "bestpm_feijoo", "bestpm_abascal", "bestpm_colau",
                 "gov_psoe_fa", "gov_pp_vox",
                 "rec_psoe", "rec_pp", "rec_vox", "rec_sumar", "rec_abst")

# Weighted mean and 95% CI of every variable, by block --------------------
block_means <- function(d, vars) {
  des <- svydesign(ids = ~1, weights = ~w, data = d)
  map_dfr(vars, function(v) {
    r <- svyby(as.formula(paste0("~", v)), ~block, des, svymean, na.rm = TRUE)
    tibble(variable = v, block = r$block, est = r[[v]], se = SE(r))
  }) |>
    mutate(lo = est - 1.96 * se, hi = est + 1.96 * se)
}

series <- block_means(dat, series_vars) |>
  left_join(blocks, by = "block") |>
  mutate(label = outcome_labels[variable])
write_csv(series, file.path(dir_tab, "series_by_block.csv"))

# Composition check: is recalled 2023 vote stable across tracking blocks?
# Recall is fixed before fieldwork, so differences reflect sampling/composition,
# not campaign effects. Large differences call for recall-vote controls.
trk_des <- svydesign(ids = ~1, weights = ~w, data = filter(dat, survey == "tracking"))
comp_test <- svychisq(~recall + block, trk_des, statistic = "F")
comp <- svytable(~recall + block, trk_des) |>
  prop.table(margin = 2) |>
  as.data.frame() |>
  rename(share = Freq)
write_csv(comp, file.path(dir_tab, "composition_recall_by_block.csv"))
capture.output(comp_test, file = file.path(dir_tab, "composition_recall_test.txt"))
message("Recall-vote composition across tracking blocks: ",
        sprintf("F = %.2f, p = %.3f", comp_test$statistic, comp_test$p.value))

# Figure: vote intention by block with fieldwork events -------------------
fw_events <- events |>
  filter(phase %in% c("between_surveys", "fieldwork"), salience %in% c("high", "medium")) |>
  group_by(date) |>
  summarise(ev_label = paste(event_id, collapse = ", "))

vi <- series |>
  filter(variable %in% paste0("vi_", names(party_names)[1:5])) |>
  mutate(party = factor(party_names[sub("vi_", "", variable)], levels = party_names[1:5]))

p_vi <- ggplot(vi, aes(mid, est * 100, colour = party)) +
  geom_vline(data = fw_events, aes(xintercept = date), colour = "grey80", linewidth = 0.4) +
  geom_text(data = fw_events, aes(x = date, y = Inf, label = ev_label), inherit.aes = FALSE,
            angle = 90, hjust = 1.05, vjust = -0.4, size = 2.6, colour = "grey45") +
  geom_errorbarh(aes(xmin = start, xmax = end), height = 0, linewidth = 0.3, alpha = 0.5) +
  geom_linerange(aes(ymin = lo * 100, ymax = hi * 100), linewidth = 0.6) +
  geom_point(size = 2.2) +
  geom_line(data = filter(vi, survey == "tracking"), linewidth = 0.7) +
  facet_wrap(~party, scales = "free_y", ncol = 3) +
  scale_colour_manual(values = party_cols, guide = "none") +
  scale_x_date(date_breaks = "3 days", date_labels = "%d %b") +
  labs(x = NULL, y = "% of all respondents (weighted)",
       title = "Direct vote intention by fieldwork block",
       subtitle = paste("Preelectoral I (2-4 Nov) and daily tracking blocks; bars = 95% CI,",
                        "horizontal bars = fieldwork window; vertical lines = events (see events/)"))
ggsave(file.path(dir_fig, "vote_intention_by_block.png"), p_vi, width = 10, height = 6.5, dpi = 300, bg = "white")

# Figure: other indicators -----------------------------------------------
other <- series |>
  filter(variable %in% c("undecided", "abstain", "likely", "bestpm_sanchez", "bestpm_feijoo",
                         "bestpm_abascal", "gov_psoe_fa", "gov_pp_vox", "rec_psoe", "rec_pp",
                         "rec_vox", "rec_sumar")) |>
  mutate(label = factor(label, levels = outcome_labels[unique(variable)]))

p_other <- ggplot(other, aes(mid, est * 100)) +
  geom_vline(data = fw_events, aes(xintercept = date), colour = "grey85", linewidth = 0.4) +
  geom_linerange(aes(ymin = lo * 100, ymax = hi * 100), colour = "grey40") +
  geom_point(size = 1.8) +
  geom_line(data = filter(other, survey == "tracking")) +
  facet_wrap(~label, scales = "free_y", ncol = 4) +
  scale_x_date(date_breaks = "3 days", date_labels = "%d %b") +
  labs(x = NULL, y = "% (weighted)", title = "Other indicators by fieldwork block",
       subtitle = "Bottom row (recalled 2023 vote) is a composition check: it should not move")
ggsave(file.path(dir_fig, "other_indicators_by_block.png"), p_other, width = 11, height = 7.5, dpi = 300, bg = "white")
