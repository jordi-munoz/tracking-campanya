# Campaign events and vote intention: 40dB tracking, 29-N 2026

This project studies whether events during the campaign for the 29 November 2026 Spanish general election moved vote intention. It uses the 40dB (El País / SER) Preelectoral I survey and the daily tracking files, comparing respondents interviewed before and after each event.

## Structure

```
tracking-campanya/
├── run_all.R                    # runs the whole pipeline
├── tracking-campanya.Rproj
├── config/fieldwork_blocks.csv  # fieldwork window and size of each sample block
├── data/
│   ├── raw/                     # 40dB deliverables, unmodified (questionnaire, notes, data, tables, report)
│   │   ├── 40dB_preelectoral_I/
│   │   └── 40dB_tracking_dia02 ... dia05/
│   └── processed/respondents.{rds,csv}   # one row per unique interview (generated)
├── events/
│   ├── events_29N.csv           # dated events, hypotheses, sources (used by the code)
│   └── README.md                # timeline vs fieldwork + source list
├── R/
│   ├── 00_setup.R               # packages, codings, helpers
│   ├── 01_build_data.R          # de-duplicate tracking files, harmonise Preelectoral I, weights
│   ├── 02_descriptives.R        # daily series, composition checks
│   ├── 03_event_analysis.R      # before/after estimates per event day, robustness, placebos
│   ├── 04_heterogeneity.R       # event-specific hypotheses by target group, vote flows
│   └── 05_uesd_checks.R         # UESD diagnostics: balance, item non-response, bandwidths/power, pre-period stability
├── reports/
│   ├── _common.R                # shared figures/tables/translations for the reports
│   ├── informe_ca.qmd / .html   # report in Catalan
│   ├── informe_es.qmd / .html   # report in Spanish
│   ├── report_en.qmd / .html    # report in English
│   └── references.bib
└── output/{figures,tables}/      # generated
```

Run with `Rscript run_all.R` from the project root (R 4.5; packages: haven, dplyr, tidyr, purrr, readr, stringr, forcats, ggplot2, survey, estimatr, here, knitr). Then render the reports with `quarto render reports/informe_ca.qmd`, `quarto render reports/informe_es.qmd` and `quarto render reports/report_en.qmd` (self-contained HTML).

## Data sources

The data are not included in the repository (see `.gitignore`); download them as described in [data/README.md](data/README.md). Raw files as published by 40dB on the El País website (unzipped into `data/raw/`):

- Preelectoral I (2–4 Nov): https://ep00.epimg.net/infografias/encuestas40db/2026/11-preelectoral/2026_11_preelectoral_descargables.zip
- Tracking, day NN: `https://ep00.epimg.net/infografias/encuestas40db/2026/11-tracking-electoral/40dB_tracking_diaNN.zip` (days 02–05 used so far; day 1 is contained in day 2)

## Data construction (01_build_data.R)

- **Tracking files are cumulative/rolling.** Day 2 holds 1,500 interviews: the 10–12 Nov block (1,000) plus 12–13 Nov (500). Days 3 and 4 append 500 each. From day 5 on, the oldest block is dropped as each new one comes in. `id` is renumbered and `ponde` recomputed in every file, so interviews are matched on their full response profile and position. The script checks that every file is "previous file minus oldest rows plus new rows" and stops if not. The result is **3,000 unique tracking interviews in 5 blocks**.
- **Block dates** come from 40dB's methodology notes and report labels. They are in `config/fieldwork_blocks.csv`. Each daily block spans two calendar days (e.g. 13–14 Nov), and interview dates and times are not in the data.
- **Preelectoral I (2–4 Nov, n = 2,000)** is harmonised to the tracking numbering (its p3 = tracking p2, and so on). Value labels were checked to be identical.
- **Weights**: every block is raked separately to the same margins (sex × age, region, municipality size, social class). These are 40dB's quota variables, and the targets come from the weighted day-4 file. 40dB's original weights are kept as `ponde_src`.
- **Duplicates**: 162 interviews (3%) are exact copies of an earlier one on all 31 variables, almost all on a *different* fieldwork day. Their share rises over time (9% of block 5). They are probably duplicated records or re-interviewed panelists. They are flagged (`dup_copy`, `dup_any`) and dropped in a robustness check. **Worth raising with 40dB.**

**To add new tracking days:** put the folder in `data/raw/40dB_tracking_diaNN/`, add a row to `config/fieldwork_blocks.csv` (new block, its file folder, dates, n = 500), and rerun. Also add new events (e.g. the 19 Nov head-to-head debate) to `events/events_29N.csv`.

## Design (03_event_analysis.R, 05_uesd_checks.R)

An Unexpected Event during Surveys Design (UESD; Muñoz, Falcó-Gimeno & Hernández 2020, *Political Analysis* 28(2): 186–206): daily samples are independent and drawn with the same quotas, so respondents interviewed after an event are compared with those interviewed before.

- A block is **pre** if it ended before the event day and **post** if it started after it. Blocks containing the event day are **during** and are dropped in the main specification.
- Events on the same day are analysed jointly as an **event day**; they cannot be separated.
- The model is a weighted linear probability model, `y ~ post + sex + age + region + habitat + education + class`, with HC2 SEs. Only pre-treatment covariates are used: recalled 2023 vote can drift toward current preference, which would make it post-treatment, so it enters only as a robustness check and as a placebo. BH q-values are computed across all event × outcome tests.
- Events are coded as unexpected, scheduled or mixed (`expectedness` in `events/events_29N.csv`); most campaign events are scheduled, so anticipation is possible. Effects are read as ITT.
- **Primary spec**: tracking-only when pre and post tracking blocks exist (13 and 14 Nov). For earlier events (6–12 Nov) the Preelectoral I is the only pre-period. That makes it a cross-survey comparison, with a different questionnaire context and several events in between, so treat those estimates as descriptive.
- **Robustness**: preelectoral added to the pre-period; during-blocks coded as post; adjacent blocks only; with recall controls; no controls; unweighted; duplicates dropped.
- **UESD diagnostics** (05): joint balance tests of non-quota covariates and recall; item non-response (refusals, DK on turnout, ideology and PTVs, counting only code 99 because regional-party PTVs have structural NAs); power and MDE for each bandwidth; Preelectoral I vs block 1 as a check on pre-period stability.
- **Placebos**: recalled 2023 vote cannot react to campaign events, so a shift there indicates changes in sample composition.
- Outcomes: direct vote intention (PSOE, PP, Vox, Frente Amplio, Podemos, SALF), undecided, would not vote, likely voter, PTVs, best PM, preferred government.

## First results (data through 16 Nov)

See `reports/informe_ca.html`, `reports/informe_es.html` or `reports/report_en.html` for the full write-up. In brief, using the primary specification:

- Balance holds for every event day (joint p between 0.38 and 0.63). Recall-vote composition is stable across blocks (p = 0.92), and 2 of 30 placebo tests are significant, both in comparisons with the Preelectoral I.
- **13 Nov** (campaign start, Vox programme, Abascal skips Jordi Wild, laSexta debate): Vox −4.5 pp (p = .009), Abascal as best PM −5.4 pp (p = .002, q = .06), PP +3.6 pp (p = .04). The Vox and Abascal drops hold in all 8 specifications; the PP gain holds in 4 of 8. Among men aged 18–34, Vox is −16 pp (n ≈ 100 per period, driven mainly by block 4). Block 1 was already high for Vox compared with 2–4 Nov (+2.1, n.s.); with the Preelectoral I added to the pre-period, Vox is −3.1 pp (p = .02).
- **11–12 Nov** (strike, police charges, Marlaska): no detectable shifts (comparison with the Preelectoral I only).
- **14 Nov** (Hernani, Canarias): PP +4.8 pp (p = .02), but this cannot be separated from the 13 Nov trend; the regional falsification tests have under 30 cases in the post group.
- Item non-response is stable, apart from a 1–2 pp rise in DK on turnout after 13–14 Nov. The MDE (80% power) is about 5 pp.

## Caveats

- This is raw direct vote intention, not 40dB's vote estimate (which models undecideds and turnout).
- Events cluster on the same days and block windows overlap calendar days, so effects are attributed to *event days*, not single events.
- Duplicated interviews and the cross-instrument comparison with the Preelectoral I are the main data-quality threats.
- Some event dates are approximate (Urda crash, Puente's resignation, Canarias arrivals, the Telecinco interview); see `events/README.md`.
