# 04_heterogeneity.R -----------------------------------------------------
# Event-specific hypotheses: does the before/after change concentrate in the
# group the event should affect most? Model: y ~ post * group + controls.
# Also vote flows (recall 2023 -> intention) before and after 13 Nov.

source(here::here("R", "00_setup.R"))
dat <- read_rds(file.path(dir_proc, "respondents.rds")) |>
  mutate(young_male    = as.integer(female == 0 & edad_r %in% c("1", "2")),
         young         = as.integer(edad_r %in% c("1", "2")),
         left_2023     = as.integer(recall %in% c("PSOE", "Sumar")),
         right_2023    = as.integer(recall %in% c("PP", "Vox")),
         basque_nav    = as.integer(ccaa %in% c("15", "16")),
         canarias      = as.integer(ccaa == "5"))

# Hypotheses -------------------------------------------------------------
# surveys: "tracking" when pre and post tracking blocks exist; the strike
# (11 Nov) needs the preelectoral survey as pre-period.
hyps <- tribble(
  ~hyp, ~event_date,            ~surveys,                        ~group,        ~group_label,                 ~outcomes,
  "H1 Strike & police charges mobilise the left / move it away from PSOE",
        as.Date("2026-11-11"), c("preelectoral_I", "tracking"),  "left_2023",   "Recall PSOE or Sumar 2023",  c("vi_psoe", "vi_fa", "vi_podemos", "likely"),
  "H2 Campaign start, Abascal skips Jordi Wild: Vox loses young men",
        as.Date("2026-11-13"), "tracking",                       "young_male",  "Men 18-34",                  c("vi_vox", "ptv_vox", "bestpm_abascal"),
  "H3 Campaign start: Vox-PP flows among right-wing 2023 voters",
        as.Date("2026-11-13"), "tracking",                       "right_2023",  "Recall PP or Vox 2023",      c("vi_vox", "vi_pp", "bestpm_abascal", "bestpm_feijoo"),
  "H4 Attack on PP in Hernani: rally effect in Basque Country/Navarre",
        as.Date("2026-11-14"), "tracking",                       "basque_nav",  "Basque Country or Navarre",  c("vi_pp", "ptv_pp"),
  "H5 Record arrivals in Canarias: Vox gains in the islands",
        as.Date("2026-11-14"), "tracking",                       "canarias",    "Canary Islands",             c("vi_vox", "ptv_vox")
) |> unnest_longer(outcomes, values_to = "outcome")

estimate_het <- function(event_date, surveys, group, outcome) {
  d <- dat |>
    filter(survey %in% surveys, !is.na(.data[[outcome]])) |>
    mutate(exposure = event_exposure(start, end, event_date)) |>
    filter(exposure != "during") |>
    mutate(post = as.integer(exposure == "post"), g = .data[[group]])
  f <- as.formula(paste(outcome, "~ post * g +", controls_demo))
  m <- lm_robust(f, data = d, weights = w, se_type = "HC2")
  # post effect outside the group, inside the group, and their difference
  L <- rbind(outside = c(post = 1, `post:g` = 0), inside = c(1, 1), difference = c(0, 1))
  b <- coef(m)[c("post", "post:g")]; V <- vcov(m)[c("post", "post:g"), c("post", "post:g")]
  est <- drop(L %*% b); se <- sqrt(diag(L %*% V %*% t(L)))
  tibble(contrast = rownames(L), estimate = est, std.error = se,
         conf.low = est - 1.96 * se, conf.high = est + 1.96 * se,
         p.value = 2 * pnorm(-abs(est / se)),
         n_group_pre = sum(d$g == 1 & d$post == 0), n_group_post = sum(d$g == 1 & d$post == 1))
}

het <- hyps |>
  mutate(res = pmap(list(event_date, surveys, group, outcome), estimate_het)) |>
  unnest(res) |>
  mutate(label = outcome_labels[outcome], surveys = map_chr(surveys, paste, collapse = "+"))
write_csv(het, file.path(dir_tab, "heterogeneity_hypotheses.csv"))
het |>
  filter(contrast != "outside") |>
  transmute(hyp = str_sub(hyp, 1, 2), outcome, contrast, est = round(estimate, 3),
            se = round(std.error, 3), p = round(p.value, 3), n_group_pre, n_group_post) |>
  print(n = Inf)

p_het <- het |>
  mutate(scale = if_else(str_starts(outcome, "ptv_"), 1, 100),
         across(c(estimate, conf.low, conf.high), ~ .x * scale),
         contrast = factor(contrast, levels = c("difference", "inside", "outside"),
                           labels = c("Difference", "Group", "Rest")),
         panel = paste0(str_wrap(hyp, 45), "\n[group: ", group_label, "]")) |>
  ggplot(aes(estimate, label, colour = contrast)) +
  geom_vline(xintercept = 0, colour = "grey60") +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high), position = position_dodge(width = 0.6)) +
  geom_point(size = 1.8, position = position_dodge(width = 0.6)) +
  facet_wrap(~panel, scales = "free", ncol = 2) +
  scale_colour_manual(values = c(Group = "#D81B7A", Rest = "grey55", Difference = "#1D84CE"), name = NULL) +
  labs(x = "Post - pre difference (pp; PTV in 0-10 points), 95% CI", y = NULL,
       title = "Event effects by targeted group")
ggsave(file.path(dir_fig, "heterogeneity_hypotheses.png"), p_het, width = 12, height = 10, dpi = 300, bg = "white")

# Vote flows around 13 Nov: recall 2023 x current intention -----------------
flows <- dat |>
  filter(survey == "tracking") |>
  mutate(exposure = event_exposure(start, end, as.Date("2026-11-13"))) |>
  filter(exposure != "during", recall %in% c("PSOE", "PP", "Vox", "Sumar")) |>
  mutate(intention = case_when(p2 == 1 ~ "PSOE", p2 == 2 ~ "PP", p2 == 3 ~ "Vox",
                               p2 == 4 ~ "Frente Amplio", p2 == 5 ~ "Podemos",
                               p2 == 24 ~ "Undecided", p2 == 23 ~ "Would not vote",
                               TRUE ~ "Other/blank/NA")) |>
  count(exposure, recall, intention, wt = w) |>
  group_by(exposure, recall) |>
  mutate(share = n / sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = exposure, values_from = share, values_fill = 0) |>
  mutate(change_pp = 100 * (post - pre)) |>
  arrange(recall, desc(abs(change_pp)))
write_csv(flows, file.path(dir_tab, "vote_flows_13nov.csv"))
