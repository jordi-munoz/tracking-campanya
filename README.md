# Campaign events and vote intention: 40dB tracking, 29-N 2026

This project studies whether events during the campaign for the 29 November 2026 Spanish general election moved vote intention. It uses the 40dB (El País / SER) Preelectoral I survey and the daily tracking files, comparing respondents interviewed before and after each event.

## Structure

```
tracking-campanya/
├── run_all.R                    # runs the whole pipeline (--update, --reports)
├── tracking-campanya.Rproj
├── config/fieldwork_blocks.csv  # fieldwork window and size of each sample block
├── data/
│   ├── raw/                     # 40dB deliverables, unmodified (questionnaire, notes, data, tables, report)
│   │   ├── 40dB_preelectoral_I/
│   │   └── 40dB_tracking_dia02 ... dia08/
│   └── processed/respondents.{rds,csv}   # one row per unique interview (generated)
├── events/
│   ├── events_29N.csv           # dated events, hypotheses, sources (used by the code)
│   └── README.md                # timeline vs fieldwork + source list
├── R/
│   ├── update_data.R            # downloads 40dB files, registers new tracking days in the config
│   ├── fieldwork.R              # calendar helpers shared by pipeline, reports and updater (base R)
│   ├── 00_setup.R               # packages, codings, helpers
│   ├── 01_build_data.R          # de-duplicate tracking files, harmonise Preelectoral I, weights
│   ├── 02_descriptives.R        # daily series, composition checks
│   ├── 03_event_analysis.R      # before/after estimates per event day, robustness, placebos
│   ├── 04_heterogeneity.R       # event-specific hypotheses by target group, vote flows
│   └── 05_uesd_checks.R         # UESD diagnostics: balance, item non-response, bandwidths/power, pre-period stability
├── reports/
│   ├── _common.R                # shared figures/tables/translations + checks of the text's claims
│   ├── check_claims.R           # prints which statements in the text still hold (no rendering)
│   ├── informe_ca.qmd / .html   # report in Catalan
│   ├── informe_es.qmd / .html   # report in Spanish
│   ├── report_en.qmd / .html    # report in English
│   └── references.bib
├── tests/testthat/              # tests for R/fieldwork.R
└── output/{figures,tables}/      # generated
```

From the project root (R 4.5; packages: haven, dplyr, tidyr, purrr, readr, stringr, forcats, ggplot2, survey, estimatr, here, knitr, rmarkdown; Quarto for the reports):

```
Rscript run_all.R                      # analysis only
Rscript run_all.R --update --reports   # download new days, rerun everything, render the three reports
Rscript -e 'testthat::test_dir("tests/testthat")'
```

The reports are self-contained HTML. On a fresh clone, `Rscript R/update_data.R` downloads all the raw data.

## Adding a new tracking day

1. `Rscript run_all.R --update --reports`. The updater (`R/update_data.R`) finds every newly published day, unpacks it into `data/raw/40dB_tracking_diaNN/` and appends its block to `config/fieldwork_blocks.csv`. New blocks span two calendar days, starting on the day the previous one ended, with n = 500. `01_build_data.R` stops if a file does not add exactly those 500 interviews. Check the dates against the methodology note.
2. Add new events to `events/events_29N.csv`, with new IDs (E32, E33, …) in date order, and their translations to `event_lab()` in `reports/_common.R`. The render stops if a label is missing. An event's phase (`pre_survey`, `between_surveys`, `fieldwork`, `post_data`) is not stored: it is derived from the block dates, so events after the last block enter the analysis automatically once a new day covers them.
3. Numbers, dates, block ranges, download links and figure sizes in the reports are computed. Sentences that describe results in words ("not significant", "the only estimates with q close to 0.05"…) are each checked against the data by `report_claims()` in `reports/_common.R`. `run_all.R` prints the check, and rendering stops if any statement no longer holds. Revise those paragraphs in all three reports and update the matching rows. `ALLOW_STALE_CLAIMS=1 quarto render …` renders a draft anyway.
4. Update the "First results" section below.

## Data sources

The data are not included in the repository (see `.gitignore`); download them as described in [data/README.md](data/README.md). Raw files as published by 40dB on the El País website (unzipped into `data/raw/`):

- Preelectoral I (2–4 Nov): https://ep00.epimg.net/infografias/encuestas40db/2026/11-preelectoral/2026_11_preelectoral_descargables.zip
- Tracking, day NN: `https://ep00.epimg.net/infografias/encuestas40db/2026/11-tracking-electoral/40dB_tracking_diaNN.zip` (days 02–08 used so far; day 1 is contained in day 2)

## Data construction (01_build_data.R)

- **Tracking files are cumulative/rolling.** Day 2 holds 1,500 interviews: the 10–12 Nov block (1,000) plus 12–13 Nov (500). Days 3 and 4 append 500 each. From day 5 on, the oldest block is dropped as each new one comes in. `id` is renumbered and `ponde` recomputed in every file, so interviews are matched on their full response profile and position. The script checks that every file is "previous file minus oldest rows plus new rows" and stops if not. The result is **4,500 unique tracking interviews in 8 blocks** (data through 19 Nov).
- **Block dates** come from 40dB's methodology notes and report labels. They are in `config/fieldwork_blocks.csv`. Each daily block spans two calendar days (e.g. 13–14 Nov), and interview dates and times are not in the data. The notes give only the window of the whole file (day 8: 15–19 Nov), so each new block is dated from the day the previous one ended.
- **Preelectoral I (2–4 Nov, n = 2,000)** is harmonised to the tracking numbering (its p3 = tracking p2, and so on). Value labels were checked to be identical.
- **Weights**: every block is raked separately to the same margins (sex × age, region, municipality size, social class). These are 40dB's quota variables, and the targets come from the weighted day-4 file. 40dB's original weights are kept as `ponde_src`.
- **Duplicates**: 294 interviews (4.5%) are exact copies of an earlier one on all 31 variables, almost all on a *different* fieldwork day. Their share rises from 2% of block 1 to 7–10% of each block from block 3 on. They are probably duplicated records or re-interviewed panelists. They are flagged (`dup_copy`, `dup_any`) and dropped in a robustness check. **Worth raising with 40dB.**

## Design (03_event_analysis.R, 05_uesd_checks.R)

An Unexpected Event during Surveys Design (UESD; Muñoz, Falcó-Gimeno & Hernández 2020, *Political Analysis* 28(2): 186–206): daily samples are independent and drawn with the same quotas, so respondents interviewed after an event are compared with those interviewed before.

- A block is **pre** if it ended before the event day and **post** if it started after it. Blocks containing the event day are **during** and are dropped in the main specification.
- Events on the same day are analysed jointly as an **event day**; they cannot be separated.
- The model is a weighted linear probability model, `y ~ post + sex + age + region + habitat + education + class`, with HC2 SEs. Only pre-treatment covariates are used: recalled 2023 vote can drift toward current preference, which would make it post-treatment, so it enters only as a robustness check and as a placebo. BH q-values are computed across all event × outcome tests.
- Events are coded as unexpected, scheduled or mixed (`expectedness` in `events/events_29N.csv`); most campaign events are scheduled, so anticipation is possible. Effects are read as ITT.
- **Primary spec**: tracking-only when pre and post tracking blocks exist (13 Nov onwards). For earlier events (6–12 Nov) the Preelectoral I is the only pre-period. That makes it a cross-survey comparison, with a different questionnaire context and several events in between, so treat those estimates as descriptive.
- **Robustness**: preelectoral added to the pre-period; during-blocks coded as post; adjacent blocks only; with recall controls; no controls; unweighted; duplicates dropped.
- **UESD diagnostics** (05): joint balance tests of non-quota covariates and recall; item non-response (refusals, DK on turnout, ideology and PTVs, counting only code 99 because regional-party PTVs have structural NAs); power and MDE for each bandwidth; Preelectoral I vs block 1 as a check on pre-period stability.
- **Placebos**: recalled 2023 vote cannot react to campaign events, so a shift there indicates changes in sample composition.
- Outcomes: direct vote intention (PSOE, PP, Vox, Frente Amplio, Podemos, SALF), undecided, would not vote, likely voter, PTVs, best PM, preferred government.

## First results (data through 19 Nov)

See `reports/informe_ca.html`, `reports/informe_es.html` or `reports/report_en.html` for the full write-up. This summary is written by hand: update it after each new day. In brief, using the primary specification (q-values are BH-adjusted over the 75 event-day × outcome tests of the main specification, so they rise as event days are added):

- Balance holds for every event day (joint p between 0.09 and 0.51). Recall-vote composition is stable across blocks (p = 0.99). 2 of the 45 placebo tests have p < .05 (recalled abstention, 12 and 14 Nov), about what chance would produce; none survives correction.
- **13 Nov** (campaign start, Vox programme, Abascal skips Jordi Wild, laSexta debate): Vox −3.9 pp (p = .009, q = .26), Abascal as best PM −4.5 pp (p = .002, q = .18). Both drops hold in all 8 specifications and are the lowest p-values in the data. PSOE +3.1 pp (p = .07) and PP +2.7 pp (p = .07), each significant in 2 of 8 specifications. Among men aged 18–34, Vox is −6.7 pp (p = .21; n = 103 pre, 250 post, driven mainly by block 4). Block 1 was already high for Vox compared with 2–4 Nov (+2.1, n.s.); with the Preelectoral I added to the pre-period, Vox is −2.5 pp (p = .02).
- **Vox by block**: from 20.0% (block 1) to a low of 13.4% (block 4, 14–15 Nov), back up to 18.9% on 17–18 Nov and 15.8% on 18–19 Nov.
- **11–12 Nov** (strike, police charges, Marlaska): no detectable shifts (comparison with the Preelectoral I only).
- **14 Nov** (Hernani, Canarias): PP +2.8 pp (p = .04, q = .47; significant in 6 of 8 specifications), which cannot be separated from the 13 Nov trend; the regional falsification tests have only 122 and 88 cases in the post group.
- **15 Nov** (PSOE programme, Sánchez calls Abascal "racist"): PSOE +2.0 pp (p = .21), Vox −2.1 pp (p = .12), both smaller than with block 6 alone; the pre-period includes the days before 13 Nov.
- **16 Nov** (Lula, Frente Amplio programme, Telecinco interview) and **17 Nov** (pay and minimum wage rises, Abascal's wife and the heating subsidy): no outcome moves significantly. After 17 Nov Vox is −1.6 pp (p = .37) and Abascal −2.1 pp (p = .26), from block 8 alone.
- Refusals are stable; DK on turnout goes from about 1% to 2% after 13–14 Nov (n.s.). The MDE (80% power) for Vox is 4.1 pp for 13 Nov, 3.7 pp for 14 Nov and 5.1 pp for 17 Nov.
- 18 Nov (Feijóo podcast, Sánchez on Antena 3) and 19 Nov have no later block yet. On 19 Nov the head-to-head debate (64% say Feijóo won, SocioMétrica/El Español) coincides with Puigdemont's *El Mundo* interview and a police report saying the Ceuta mass entry was planned, so the three cannot be separated.

## Caveats

- This is raw direct vote intention, not 40dB's vote estimate (which models undecideds and turnout).
- Events cluster on the same days and block windows overlap calendar days, so effects are attributed to *event days*, not single events.
- Duplicated interviews and the cross-instrument comparison with the Preelectoral I are the main data-quality threats.
- Some event dates are approximate (Urda crash, Puente's resignation, Canarias arrivals, the Telecinco interview); see `events/README.md`.
