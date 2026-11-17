# _common.R --------------------------------------------------------------
# Shared code for the Catalan, Spanish and English reports. Each .qmd sets
# `lang` ("ca", "es" or "en") before sourcing this file. Everything is read from
# output/tables, so run `Rscript run_all.R` first.

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(readr)
  library(stringr); library(forcats); library(ggplot2); library(knitr)
})
root <- here::here()
tab  <- function(f) read_csv(file.path(root, "output", "tables", f), show_col_types = FALSE)

primary   <- tab("event_effects_primary.csv")
allspecs  <- tab("event_effects_all_specs.csv")
series    <- tab("series_by_block.csv")
balance   <- tab("uesd_balance.csv")
nonresp   <- tab("uesd_item_nonresponse.csv")
bandw     <- tab("uesd_bandwidths_power.csv")
prestab   <- tab("uesd_pre_period_stability.csv")
het       <- tab("heterogeneity_hypotheses.csv")
omnibus   <- tab("omnibus_block_differences.csv")
flows     <- tab("vote_flows_13nov.csv")
comp_test <- readLines(file.path(root, "output", "tables", "composition_recall_test.txt"))
blocks    <- read_csv(file.path(root, "config", "fieldwork_blocks.csv"), show_col_types = FALSE)
events    <- read_csv(file.path(root, "events", "events_29N.csv"), show_col_types = FALSE)
resp      <- read_rds(file.path(root, "data", "processed", "respondents.rds"))

# Translations -------------------------------------------------------------
L <- function(ca, es, en) switch(lang, ca = ca, es = es, en = en)

month_name <- function(d) {
  m <- as.integer(format(d, "%m")) - 9   # the period covers October and November
  L(c("d'octubre", "de novembre"), c("de octubre", "de noviembre"), c("October", "November"))[m]
}
fmt_date  <- function(d) paste(as.integer(format(d, "%d")), month_name(d))
fmt_short <- function(d) paste0(as.integer(format(d, "%d")), " ",
                                L(c("oct.", "nov."), c("oct.", "nov."), c("Oct", "Nov"))[as.integer(format(d, "%m")) - 9])

outcome_lab <- function(o) {
  vi <- L("Intenció de vot", "Intención de voto", "Vote intention"); pm <- L("Millor president/a", "Mejor presidente/a", "Best prime minister")
  rc <- L("Record de vot 2023", "Recuerdo de voto 2023", "Recalled vote 2023")
  c(vi_psoe = paste0(vi, ": PSOE"), vi_pp = paste0(vi, ": PP"), vi_vox = paste0(vi, ": Vox"),
    vi_fa = paste0(vi, ": Frente Amplio"), vi_podemos = paste0(vi, ": Podemos"),
    undecided = L("Indecisos", "Indecisos", "Undecided"), likely = L("Votant probable (9-10)", "Votante probable (9-10)", "Likely voter (9-10)"),
    ptv_psoe = "PTV PSOE", ptv_pp = "PTV PP", ptv_vox = "PTV Vox", ptv_fa = "PTV Frente Amplio",
    bestpm_sanchez = paste0(pm, ": Sánchez"), bestpm_feijoo = paste0(pm, ": Feijóo"),
    bestpm_abascal = paste0(pm, ": Abascal"), bestpm_colau = paste0(pm, ": Colau"),
    rec_psoe = paste0(rc, ": PSOE"), rec_pp = paste0(rc, ": PP"), rec_vox = paste0(rc, ": Vox"),
    rec_sumar = paste0(rc, ": Sumar"), rec_abst = paste0(rc, L(": abstenció", ": abstención", ": abstained")))[o]
}

spec_lab <- function(s) c(
  tracking = L("Principal", "Principal", "Main"),
  adjacent_blocks = L("Només blocs adjacents", "Solo bloques adyacentes", "Adjacent blocks only"),
  during_as_post = L("Blocs amb l'esdeveniment com a post", "Bloques con el evento como post", "Event-day blocks as post"),
  with_preelectoral = L("Preelectoral I afegit al pre", "Preelectoral I añadido al pre", "Preelectoral I added to pre"),
  with_recall_controls = L("Controlant record de vot", "Controlando recuerdo de voto", "Controlling for recalled vote"),
  no_controls = L("Sense controls", "Sin controles", "No controls"),
  unweighted = L("Sense ponderar", "Sin ponderar", "Unweighted"),
  drop_duplicates = L("Sense entrevistes duplicades", "Sin entrevistas duplicadas", "Duplicate interviews dropped"))[s]

expect_lab <- function(x) c(unexpected = L("Inesperat", "Inesperado", "Unexpected"),
                            scheduled = L("Programat", "Programado", "Scheduled"),
                            mixed = L("Mixt", "Mixto", "Mixed"))[x]

event_lab <- function(id) {
  ca <- c(E01 = "Convocatòria anticipada", E02 = "Accident ferroviari d'Urda",
          E03 = "Dimissió del ministre Puente", E04 = "Zapatero continua investigat (Plus Ultra)",
          E05 = "Programa del PP", E06 = "Vaga general per l'habitatge i càrregues policials",
          E07 = "Pròrroga de la investigació a Zapatero",
          E08 = "Marlaska obre una investigació; Sánchez li dona suport",
          E09 = "Votació dels decrets d'habitatge", E10 = "Macroenquesta del CIS",
          E11 = "Inici oficial de la campanya; programa de Vox",
          E12 = "Entrevistes de Jordi Wild; Abascal hi renuncia",
          E13 = "Debat econòmic a laSexta", E14 = "Agressió a un acte del PP a Hernani",
          E15 = "Rècord d'arribades de pasteres a Canàries",
          E16 = "Programa del PSOE; Sánchez titlla Abascal de 'racista'",
          E17 = "Lula a la Moncloa; programa de Frente Amplio; informe del CNI sobre Ceuta",
          E18 = "Entrevista a Sánchez a Telecinco (Ceuta)", E19 = "Pujada de sou als funcionaris",
          E20 = "Cara a cara Sánchez–Feijóo", E21 = "Inici de la prohibició de publicar enquestes",
          E22 = "Debat a quatre", E23 = "Jornada electoral")
  es <- c(E01 = "Convocatoria anticipada", E02 = "Accidente ferroviario de Urda",
          E03 = "Dimisión del ministro Puente", E04 = "Zapatero sigue investigado (Plus Ultra)",
          E05 = "Programa del PP", E06 = "Huelga general por la vivienda y cargas policiales",
          E07 = "Prórroga de la investigación a Zapatero",
          E08 = "Marlaska abre una investigación; Sánchez lo respalda",
          E09 = "Votación de los decretos de vivienda", E10 = "Macroencuesta del CIS",
          E11 = "Inicio oficial de la campaña; programa de Vox",
          E12 = "Entrevistas de Jordi Wild; Abascal renuncia",
          E13 = "Debate económico en laSexta", E14 = "Agresión a un acto del PP en Hernani",
          E15 = "Récord de llegadas de cayucos a Canarias",
          E16 = "Programa del PSOE; Sánchez llama 'racista' a Abascal",
          E17 = "Lula en la Moncloa; programa de Frente Amplio; informe del CNI sobre Ceuta",
          E18 = "Entrevista a Sánchez en Telecinco (Ceuta)", E19 = "Subida de sueldo a los funcionarios",
          E20 = "Cara a cara Sánchez–Feijóo", E21 = "Inicio de la prohibición de publicar encuestas",
          E22 = "Debate a cuatro", E23 = "Jornada electoral")
  en <- c(E01 = "Snap election called", E02 = "Urda rail crash",
          E03 = "Transport minister Puente resigns", E04 = "Zapatero remains under investigation (Plus Ultra)",
          E05 = "PP programme", E06 = "General strike on housing and police charges",
          E07 = "Zapatero investigation extended",
          E08 = "Marlaska opens an inquiry; Sánchez backs him",
          E09 = "Vote on the housing decrees", E10 = "CIS macro-survey",
          E11 = "Official campaign start; Vox programme",
          E12 = "Jordi Wild interviews; Abascal pulls out",
          E13 = "Economic debate on laSexta", E14 = "Attack on a PP event in Hernani",
          E15 = "Record boat arrivals in the Canary Islands",
          E16 = "PSOE programme; Sánchez calls Abascal 'racist'",
          E17 = "Lula at Moncloa; Frente Amplio programme; CNI report on Ceuta",
          E18 = "Sánchez interview on Telecinco (Ceuta)", E19 = "Public-sector pay rise",
          E20 = "Sánchez–Feijóo head-to-head debate", E21 = "Poll publication ban begins",
          E22 = "Four-way debate", E23 = "Election day")
  L(ca, es, en)[id]
}

covariate_lab <- function(v) {
  lab <- c(
    female = L("Dona", "Mujer", "Woman"), age = L("Edat (anys)", "Edad (años)", "Age (years)"),
    educ_1 = L("Estudis: fins a 1a etapa secundària", "Estudios: hasta 1ª etapa secundaria", "Education: up to lower secondary"),
    educ_2 = L("Estudis: 2a etapa secundària", "Estudios: 2ª etapa secundaria", "Education: upper secondary"),
    educ_3 = L("Estudis: superiors, 1r cicle", "Estudios: superiores, 1er ciclo", "Education: tertiary, first cycle"),
    educ_4 = L("Estudis: superiors, 2n cicle o més", "Estudios: superiores, 2º ciclo o más", "Education: tertiary, second cycle or higher"),
    educ_5 = L("Estudis: altres", "Estudios: otros", "Education: other"),
    work_1 = L("Treballa", "Trabaja", "Employed"), work_2 = L("Pensionista", "Pensionista", "Retired"),
    work_3 = L("A l'atur", "En paro", "Unemployed"), work_4 = L("Estudiant", "Estudiante", "Student"),
    work_5 = L("Treball domèstic no remunerat", "Trabajo doméstico no remunerado", "Unpaid domestic work"),
    children_1 = L("Fills de 0-8 anys", "Hijos de 0-8 años", "Children aged 0-8"), children_2 = L("Fills de 9-12 anys", "Hijos de 9-12 años", "Children aged 9-12"),
    children_3 = L("Fills de 13-17 anys", "Hijos de 13-17 años", "Children aged 13-17"), children_4 = L("Fills de 18 anys o més", "Hijos de 18 años o más", "Children aged 18+"),
    children_5 = L("Sense fills a la llar", "Sin hijos en el hogar", "No children at home"),
    class_1 = L("Classe alta/mitjana-alta", "Clase alta/media-alta", "Upper/upper-middle class"), class_2 = L("Classe mitjana", "Clase media", "Middle class"),
    class_3 = L("Classe mitjana-baixa/baixa", "Clase media-baja/baja", "Lower-middle/lower class"),
    region_Andalusia = L("Andalusia", "Andalucía", "Andalusia"), region_Catalonia = L("Catalunya", "Cataluña", "Catalonia"),
    region_Madrid = L("Madrid", "Madrid", "Madrid"), region_Valencia = L("Comunitat Valenciana", "Comunitat Valenciana", "Valencian Community"),
    `region_Rest of Spain` = L("Resta d'Espanya", "Resto de España", "Rest of Spain"))
  out <- lab[v]
  ifelse(is.na(out), outcome_lab(v), out)
}

type_lab <- function(x) c(`Quota variable` = L("Variable de quota", "Variable de cuota", "Quota variable"),
                          `Non-quota covariate` = L("Covariable no de quota", "Covariable no de cuota", "Non-quota covariate"),
                          `Recalled vote 2023` = L("Record de vot 2023", "Recuerdo de voto 2023", "Recalled vote 2023"))[x]

# Number formatting -------------------------------------------------------
num <- function(x, d = 1) {
  s <- formatC(abs(x), format = "f", digits = d,
               decimal.mark = L(",", ",", "."), big.mark = L(".", ".", ","))
  ifelse(round(x, d) < 0, paste0("−", s), s)
}
signed <- function(x, d = 1) ifelse(round(x, d) > 0, paste0("+", num(x, d)), num(x, d))
pval <- function(p) ifelse(p < 0.001, paste0("p < ", num(0.001, 3)), paste0("p = ", num(p, 3)))
qval <- function(q) paste0("q = ", num(q, 2))

# Estimates for inline text: points for proportions, raw for PTVs.
get_est <- function(date, outcome, spec = NULL, data = allspecs) {
  r <- data |> filter(.data$date == as.Date(!!date), .data$outcome == !!outcome)
  r <- if (is.null(spec)) filter(r, is_primary) else filter(r, .data$spec == !!spec)
  stopifnot(nrow(r) == 1)
  r
}
pts <- function(date, outcome, spec = NULL) {
  r <- get_est(date, outcome, spec)
  k <- if (str_starts(outcome, "ptv_")) 1 else 100
  paste0(signed(k * r$estimate), L(" punts", " puntos", " points"), " (", pval(r$p.value), ")")
}
pts_q <- function(date, outcome) {
  r <- get_est(date, outcome)
  paste0(signed(100 * r$estimate), L(" punts", " puntos", " points"), " (", pval(r$p.value), "; ", qval(r$q.value), ")")
}
het_pts <- function(h, outcome, contrast = "inside") {
  r <- het |> filter(str_starts(hyp, h), .data$outcome == !!outcome, .data$contrast == !!contrast)
  k <- if (str_starts(outcome, "ptv_")) 1 else 100
  paste0(signed(k * r$estimate), L(" punts", " puntos", " points"), " (", pval(r$p.value), ")")
}
share <- function(variable, block) {
  100 * series$est[series$variable == variable & series$block == block]
}

# Figures -----------------------------------------------------------------
party_cols <- c(PSOE = "#E30613", PP = "#1D84CE", Vox = "#4E9A1A",
                `Frente Amplio` = "#D81B7A", Podemos = "#8A4BB0")
accent <- "#D81B7A"; neutral <- "grey40"; placebo_col <- "#1D84CE"
theme_set(theme_minimal(base_size = 11) +
            theme(panel.grid.minor = element_blank(), legend.position = "bottom",
                  plot.title.position = "plot"))
invisible(Sys.setlocale("LC_TIME", "C"))
date_axis <- function(breaks) scale_x_date(breaks = breaks, labels = fmt_short)

fig_timeline <- function() {
  b <- blocks |>
    mutate(lab = if_else(block == 0, "Preelectoral I",
                         paste(L("Bloc", "Bloque", "Block"), block)),
           lab = fct_rev(fct_inorder(lab)), n_lab = paste0("n = ", trimws(formatC(n, big.mark = L(".", ".", ","), format = "d"))))
  ev <- events |>
    filter(phase %in% c("between_surveys", "fieldwork")) |>
    mutate(expect = expect_lab(expectedness)) |>
    group_by(date) |>
    mutate(k = row_number()) |>
    ungroup()
  ggplot() +
    geom_vline(data = distinct(ev, date), aes(xintercept = date), colour = "grey88") +
    geom_segment(data = b, aes(x = start, xend = end + 0.9, y = lab, yend = lab),
                 linewidth = 5, colour = "grey55", lineend = "butt") +
    geom_text(data = b, aes(x = end + 1.2, y = lab, label = n_lab), hjust = 0, size = 3, colour = "grey30") +
    geom_point(data = ev, aes(x = date + 0.45, y = 6.6 + 0.45 * (k - 1), colour = expect), size = 2.6) +
    geom_text(data = ev, aes(x = date + 0.75, y = 6.6 + 0.45 * (k - 1), label = event_id),
              hjust = 0, size = 2.7, colour = "grey20") +
    scale_colour_manual(values = setNames(c(accent, "grey55", "#1D84CE"),
                                          expect_lab(c("unexpected", "scheduled", "mixed"))),
                        name = NULL) +
    scale_x_date(breaks = seq(as.Date("2026-11-02"), as.Date("2026-11-16"), by = 2),
                 labels = fmt_short, expand = expansion(add = c(0.5, 2))) +
    scale_y_discrete(expand = expansion(add = c(0.6, 2.2))) +
    labs(x = NULL, y = NULL)
}

fig_series <- function() {
  vi <- series |>
    filter(variable %in% c("vi_psoe", "vi_pp", "vi_vox", "vi_fa", "vi_podemos")) |>
    mutate(party = factor(c(vi_psoe = "PSOE", vi_pp = "PP", vi_vox = "Vox", vi_fa = "Frente Amplio",
                            vi_podemos = "Podemos")[variable], levels = names(party_cols)))
  ev <- events |> filter(phase == "fieldwork", date <= as.Date("2026-11-16")) |> distinct(date)
  ggplot(vi, aes(mid, est * 100, colour = party)) +
    geom_vline(data = ev, aes(xintercept = date), colour = "grey88") +
    geom_errorbarh(aes(xmin = start, xmax = end), height = 0, linewidth = 0.3, alpha = 0.6) +
    geom_linerange(aes(ymin = lo * 100, ymax = hi * 100), linewidth = 0.6) +
    geom_point(size = 2.2) +
    geom_line(data = filter(vi, survey == "tracking"), linewidth = 0.7) +
    facet_wrap(~party, scales = "free_y", ncol = 3) +
    scale_y_continuous(labels = \(x) num(x, 0)) +
    scale_colour_manual(values = party_cols, guide = "none") +
    date_axis(seq(as.Date("2026-11-03"), as.Date("2026-11-15"), by = 3)) +
    labs(x = NULL, y = L("% sobre el total d'enquestats (ponderat)", "% sobre el total de encuestados (ponderado)", "% of all respondents (weighted)"))
}

main_outcomes <- c("vi_psoe", "vi_pp", "vi_vox", "vi_fa", "vi_podemos", "undecided", "likely",
                   "bestpm_sanchez", "bestpm_feijoo", "bestpm_abascal", "bestpm_colau",
                   "rec_psoe", "rec_pp", "rec_vox", "rec_sumar", "rec_abst")

fig_events <- function(dates = as.Date(c("2026-11-11", "2026-11-12", "2026-11-13", "2026-11-14"))) {
  d <- primary |>
    filter(date %in% dates, outcome %in% main_outcomes) |>
    mutate(day = paste0(fmt_short(date), "\n", str_replace_all(events, ", ", "·")),
           day = fct_reorder(day, as.numeric(date)),
           lab = factor(outcome_lab(outcome), levels = rev(outcome_lab(main_outcomes))),
           status = case_when(type == "placebo" ~ L("Placebo (record de vot 2023)", "Placebo (recuerdo de voto 2023)", "Placebo (recalled vote 2023)"),
                              hypothesised ~ L("Resultat amb hipòtesi prèvia", "Resultado con hipótesis previa", "Outcome with prior hypothesis"),
                              TRUE ~ L("Altres resultats", "Otros resultados", "Other outcomes")))
  cols <- setNames(c(accent, neutral, placebo_col),
                   c(L("Resultat amb hipòtesi prèvia", "Resultado con hipótesis previa", "Outcome with prior hypothesis"),
                     L("Altres resultats", "Otros resultados", "Other outcomes"),
                     L("Placebo (record de vot 2023)", "Placebo (recuerdo de voto 2023)", "Placebo (recalled vote 2023)")))
  ggplot(d, aes(100 * estimate, lab, colour = status)) +
    geom_vline(xintercept = 0, colour = "grey60") +
    geom_linerange(aes(xmin = 100 * conf.low, xmax = 100 * conf.high), linewidth = 0.5) +
    geom_point(size = 1.8) +
    facet_wrap(~day, nrow = 1) +
    scale_colour_manual(values = cols, name = NULL) +
    scale_x_continuous(labels = \(x) num(x, 0)) +
    labs(x = L("Diferència post − pre (punts percentuals, IC 95%)", "Diferencia post − pre (puntos porcentuales, IC 95%)", "Post − pre difference (percentage points, 95% CI)"),
         y = NULL)
}

fig_robust <- function(date = as.Date("2026-11-13"),
                       outcomes = c("vi_vox", "vi_pp", "bestpm_abascal", "bestpm_feijoo")) {
  order <- c("tracking", "adjacent_blocks", "during_as_post", "with_preelectoral",
             "with_recall_controls", "no_controls", "unweighted", "drop_duplicates")
  d <- allspecs |>
    filter(.data$date == !!date, outcome %in% outcomes) |>
    left_join(select(bandw, date, spec, outcome, power_5pp), by = c("date", "spec", "outcome")) |>
    mutate(spec_l = factor(spec_lab(spec), levels = rev(spec_lab(order))),
           lab = factor(outcome_lab(outcome), levels = outcome_lab(outcomes)),
           n_lab = paste0("n = ", n_pre, "/", n_post,
                          if_else(is.na(power_5pp), "", paste0(L(" · potència 5 p.: ", " · potencia 5 p.: ", " · power 5 pts: "),
                                                               num(power_5pp, 2)))))
  ggplot(d, aes(100 * estimate, spec_l, colour = is_primary)) +
    geom_vline(xintercept = 0, colour = "grey60") +
    geom_linerange(aes(xmin = 100 * conf.low, xmax = 100 * conf.high)) +
    geom_point(size = 1.8) +
    geom_text(aes(x = Inf, label = n_lab), hjust = 1.02, vjust = -0.6, size = 2.3, colour = "grey40") +
    facet_wrap(~lab, ncol = 2) +
    scale_colour_manual(values = c(`TRUE` = accent, `FALSE` = neutral), guide = "none") +
    scale_x_continuous(labels = \(x) num(x, 0)) +
    labs(x = L("Diferència post − pre (punts percentuals, IC 95%)", "Diferencia post − pre (puntos porcentuales, IC 95%)", "Post − pre difference (percentage points, 95% CI)"),
         y = NULL)
}

fig_balance <- function(date = as.Date("2026-11-13")) {
  d <- balance |>
    filter(.data$date == !!date) |>
    mutate(lab = covariate_lab(covariate), type_l = type_lab(type),
           lab = fct_reorder(lab, std_diff))
  ggplot(d, aes(std_diff, lab, colour = type_l)) +
    annotate("rect", xmin = -0.1, xmax = 0.1, ymin = -Inf, ymax = Inf, fill = "grey94") +
    geom_vline(xintercept = 0, colour = "grey60") +
    geom_point(size = 2) +
    scale_colour_manual(values = setNames(c("grey60", accent, placebo_col),
                                          type_lab(c("Quota variable", "Non-quota covariate", "Recalled vote 2023"))),
                        name = NULL) +
    scale_x_continuous(limits = c(-0.2, 0.2), labels = \(x) num(x, 2)) +
    labs(x = L("Diferència estandarditzada (post − pre)", "Diferencia estandarizada (post − pre)", "Standardised difference (post − pre)"), y = NULL)
}

hyp_lab <- function(h) c(
  H1 = L("H1. Vaga i càrregues (11 nov.)\nGrup: votants de PSOE o Sumar el 2023",
         "H1. Huelga y cargas (11 nov.)\nGrupo: votantes de PSOE o Sumar en 2023", "H1. Strike and police charges (11 Nov)\nGroup: 2023 PSOE or Sumar voters"),
  H2 = L("H2. Inici de campanya i renúncia d'Abascal (13 nov.)\nGrup: homes de 18-34 anys",
         "H2. Inicio de campaña y renuncia de Abascal (13 nov.)\nGrupo: hombres de 18-34 años", "H2. Campaign start and Abascal's cancelled interview (13 Nov)\nGroup: men aged 18-34"),
  H3 = L("H3. Inici de campanya (13 nov.)\nGrup: votants de PP o Vox el 2023",
         "H3. Inicio de campaña (13 nov.)\nGrupo: votantes de PP o Vox en 2023", "H3. Campaign start (13 Nov)\nGroup: 2023 PP or Vox voters"),
  H4 = L("H4. Agressió a Hernani (14 nov.)\nGrup: País Basc i Navarra",
         "H4. Agresión en Hernani (14 nov.)\nGrupo: País Vasco y Navarra", "H4. Attack in Hernani (14 Nov)\nGroup: Basque Country and Navarre"),
  H5 = L("H5. Arribades a Canàries (14 nov.)\nGrup: Canàries",
         "H5. Llegadas a Canarias (14 nov.)\nGrupo: Canarias", "H5. Arrivals in the Canary Islands (14 Nov)\nGroup: Canary Islands"))[h]

fig_het <- function() {
  d <- het |>
    filter(!str_starts(outcome, "ptv_")) |>
    mutate(h = str_sub(hyp, 1, 2), panel = hyp_lab(h),
           lab = outcome_lab(outcome),
           contrast_l = factor(c(inside = L("Grup", "Grupo", "Group"), outside = L("Resta", "Resto", "Rest"),
                                 difference = L("Diferència", "Diferencia", "Difference"))[contrast],
                               levels = c(L("Diferència", "Diferencia", "Difference"), L("Resta", "Resto", "Rest"), L("Grup", "Grupo", "Group"))))
  ggplot(d, aes(100 * estimate, lab, colour = contrast_l)) +
    geom_vline(xintercept = 0, colour = "grey60") +
    geom_linerange(aes(xmin = 100 * conf.low, xmax = 100 * conf.high), position = position_dodge(width = 0.6)) +
    geom_point(size = 1.7, position = position_dodge(width = 0.6)) +
    facet_wrap(~panel, scales = "free", ncol = 2) +
    scale_colour_manual(values = setNames(c(accent, "grey60", placebo_col),
                                          c(L("Grup", "Grupo", "Group"), L("Resta", "Resto", "Rest"), L("Diferència", "Diferencia", "Difference"))),
                        breaks = c(L("Grup", "Grupo", "Group"), L("Resta", "Resto", "Rest"), L("Diferència", "Diferencia", "Difference")), name = NULL) +
    scale_x_continuous(labels = \(x) num(x, 0)) +
    labs(x = L("Diferència post − pre (punts percentuals, IC 95%)", "Diferencia post − pre (puntos porcentuales, IC 95%)", "Post − pre difference (percentage points, 95% CI)"),
         y = NULL)
}

# Tables -----------------------------------------------------------------
tab_events <- function() {
  ev <- events |> filter(phase %in% c("between_surveys", "fieldwork"))
  ev |>
    transmute(
      ID = event_id,
      !!L("Data", "Fecha", "Date") := paste0(fmt_short(date), if_else(date_precision == "approximate", "*", "")),
      !!L("Esdeveniment", "Evento", "Event") := event_lab(event_id),
      !!L("Tipus", "Tipo", "Type") := expect_lab(expectedness),
      !!L("Blocs abans", "Bloques antes", "Blocks before") := map_chr(date, \(d) {
        x <- blocks$block[blocks$end < d]; if (length(x)) paste(x, collapse = ", ") else "—" }),
      !!L("Blocs després", "Bloques después", "Blocks after") := map_chr(date, \(d) {
        x <- blocks$block[blocks$start > d]; if (length(x)) paste(x, collapse = ", ") else "—" })
    )
}

tab_diagnostics <- function() {
  days <- primary |> distinct(date, events, spec) |> filter(date >= as.Date("2026-11-11"))
  bal <- balance |> distinct(date, joint_chi2, joint_df, joint_p)
  plc <- primary |> filter(type == "placebo") |> group_by(date) |>
    summarise(placebo = paste0(sum(p.value < 0.05), "/", n()))
  nr  <- nonresp |> select(date, variable, estimate, p.value) |>
    mutate(cell = paste0(signed(100 * estimate), " (", num(p.value, 2), ")")) |>
    select(-estimate, -p.value) |>
    pivot_wider(names_from = variable, values_from = cell)
  power <- bandw |> filter(outcome == "vi_vox", spec %in% c("tracking", "with_preelectoral")) |>
    semi_join(days, by = c("date", "spec")) |> select(date, n_pre, n_post, power_5pp)
  days |>
    left_join(bal, by = "date") |> left_join(plc, by = "date") |>
    left_join(nr, by = "date") |> left_join(power, by = "date") |>
    transmute(
      !!L("Dia", "Día", "Day") := paste(fmt_short(date), events),
      !!L("Grup pre", "Grupo pre", "Pre group") := if_else(spec == "tracking", L("Tracking", "Tracking", "Tracking"), L("Preelectoral I", "Preelectoral I", "Preelectoral I")),
      `n pre/post` = paste0(n_pre, "/", n_post),
      !!L("Balanç conjunt", "Balance conjunto", "Joint balance") := paste0("χ²(", joint_df, ") = ", num(joint_chi2, 1), ", ", pval(joint_p)),
      !!L("Placebos p<0,05", "Placebos p<0,05", "Placebos p<0.05") := placebo,
      !!L("NS/NC intenció", "NS/NC intención", "Refuses vote intention") := nr_vote,
      !!L("NS participació", "NS participación", "DK turnout") := dk_turnout,
      !!L("Potència (5 p.)", "Potencia (5 p.)", "Power (5 pts)") := num(power_5pp, 2)
    )
}

tab_13nov <- function() {
  o <- c("vi_psoe", "vi_pp", "vi_vox", "vi_fa", "vi_podemos", "undecided", "likely",
         "bestpm_sanchez", "bestpm_feijoo", "bestpm_abascal", "bestpm_colau")
  primary |>
    filter(date == as.Date("2026-11-13"), outcome %in% o) |>
    mutate(outcome = factor(outcome, levels = o)) |>
    arrange(outcome) |>
    transmute(
      !!L("Indicador", "Indicador", "Indicator") := outcome_lab(as.character(outcome)),
      !!L("Pre (%)", "Pre (%)", "Pre (%)") := num(100 * pre_mean),
      !!L("Diferència (punts)", "Diferencia (puntos)", "Difference (points)") := signed(100 * estimate),
      !!L("IC 95%", "IC 95%", "95% CI") := paste0("[", num(100 * conf.low), "; ", num(100 * conf.high), "]"),
      p = num(p.value, 3), q = num(q.value, 2))
}

# Scalars used in the text ---------------------------------------------------
comp_txt <- paste(comp_test, collapse = " ")
comp_F <- as.numeric(str_match(comp_txt, "F = ([0-9.]+)")[, 2])
comp_p <- as.numeric(str_match(comp_txt, "p-value = ([0-9.]+)")[, 2])
n_dup_trk   <- sum(resp$dup_copy & resp$survey == "tracking")
pct_dup_b5  <- 100 * mean(resp$dup_copy[resp$block == 5])
n_dup_pre   <- sum(resp$dup_any & resp$block == 0)
omni_min_q  <- min(omnibus$q.value[!str_starts(omnibus$outcome, "rec_")])
omni_p      <- function(o) omnibus$p.value[omnibus$outcome == o]
n_placebo_sig <- sum(primary$type == "placebo" & primary$p.value < 0.05)
n_placebo     <- sum(primary$type == "placebo")
prestab_pts <- function(o) {
  r <- prestab[prestab$outcome == o, ]
  paste0(signed(100 * r$estimate), L(" punts", " puntos", " points"), " (", pval(r$p.value), ")")
}
flow <- function(rec, int) flows$change_pp[flows$recall == rec & flows$intention == int]
het_n <- function(h, outcome = NULL) {
  r <- het |> filter(str_starts(hyp, h), contrast == "inside")
  if (!is.null(outcome)) r <- filter(r, .data$outcome == !!outcome)
  r[1, c("n_group_pre", "n_group_post")]
}
power_of <- function(date, spec = "tracking", outcome = "vi_vox", col = "power_5pp")
  bandw[[col]][bandw$date == as.Date(date) & bandw$spec == spec & bandw$outcome == outcome]
mde <- function(date, outcome = "vi_vox") 100 * get_est(date, outcome)$mde80

# Plain-language helpers for the results section -----------------------------
# Weighted level (%) of a variable in a set of blocks, optionally in a subgroup.
lvl <- function(var, blks, cond = TRUE) {
  keep <- resp$block %in% blks & cond
  num(100 * weighted.mean(resp[[var]][keep], resp$w[keep], na.rm = TRUE))
}
young_men <- resp$female == 0 & resp$edad_r %in% c("1", "2")
# Absolute size of an estimate in points, no sign or p-value.
ap <- function(date, outcome, spec = NULL) num(abs(100 * get_est(date, outcome, spec)$estimate))
ap_het <- function(h, outcome) num(abs(100 * het$estimate[str_starts(het$hyp, h) & het$outcome == outcome &
                                                          het$contrast == "inside"]))

# Before/after levels around 13 Nov: block 1 vs blocks 4-5 (weighted raw shares).
fig_dumbbell <- function() {
  ind <- c(vi_pp = "PP", vi_psoe = "PSOE", vi_vox = "Vox", vi_fa = "Frente Amplio",
           bestpm_feijoo = "PP", bestpm_sanchez = "PSOE", bestpm_abascal = "Vox", bestpm_colau = "Frente Amplio")
  lev <- function(v, b) 100 * weighted.mean(resp[[v]][resp$block %in% b], resp$w[resp$block %in% b], na.rm = TRUE)
  d <- tibble(var = names(ind), party = unname(ind)) |>
    mutate(pre = map_dbl(var, lev, b = 1), post = map_dbl(var, lev, b = 4:5),
           panel = if_else(str_starts(var, "vi_"),
                           L("Intenció de vot (%)", "Intención de voto (%)", "Vote intention (%)"),
                           L("Millor president/a (%)", "Mejor presidente/a (%)", "Best prime minister (%)")),
           who = c(vi_pp = "PP", vi_psoe = "PSOE", vi_vox = "Vox", vi_fa = "Frente Amplio",
                   bestpm_feijoo = "Feijóo", bestpm_sanchez = "Sánchez", bestpm_abascal = "Abascal",
                   bestpm_colau = "Colau")[var],
           who = fct_reorder(who, pre),
           panel = fct_inorder(panel))
  ggplot(d, aes(y = who, colour = party)) +
    geom_segment(aes(x = pre, xend = post, yend = who), linewidth = 1,
                 arrow = arrow(length = unit(0.18, "cm"), type = "closed")) +
    geom_point(aes(x = pre), size = 2.6, shape = 21, fill = "white", stroke = 1.2) +
    geom_text(aes(x = pmin(pre, post), label = num(pmin(pre, post))), hjust = 1.4, size = 3, colour = "grey30") +
    geom_text(aes(x = pmax(pre, post), label = num(pmax(pre, post))), hjust = -0.4, size = 3, colour = "grey30") +
    facet_wrap(~panel, ncol = 2, scales = "free_y") +
    scale_colour_manual(values = party_cols, guide = "none") +
    scale_x_continuous(labels = \(x) num(x, 0), expand = expansion(add = 4)) +
    labs(x = NULL, y = NULL,
         caption = L("Cercle buit: 10-12 de novembre (1.000 entrevistes). Fletxa: 14-16 de novembre (1.000 entrevistes).",
                     "Círculo vacío: 10-12 de noviembre (1.000 entrevistas). Flecha: 14-16 de noviembre (1.000 entrevistas).", "Open circle: 10-12 November (1,000 interviews). Arrow: 14-16 November (1,000 interviews)."))
}
