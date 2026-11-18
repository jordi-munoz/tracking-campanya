# Runs the full pipeline. From the project root:
#   Rscript run_all.R                      # analysis only (R/01-05)
#   Rscript run_all.R --update             # first download new tracking days (R/update_data.R)
#   Rscript run_all.R --update --reports   # ... and render the three reports with Quarto
#
# Every run ends by checking the statements in the reports' text against the new
# results (reports/check_claims.R), so stale prose is flagged before rendering.
args <- commandArgs(trailingOnly = TRUE)

if ("--update" %in% args) {
  message("\n==== update_data.R ====")
  source(here::here("R", "update_data.R"), local = new.env())
}

scripts <- c("01_build_data.R", "02_descriptives.R", "03_event_analysis.R", "04_heterogeneity.R",
             "05_uesd_checks.R")
for (s in scripts) {
  message("\n==== ", s, " ====")
  source(here::here("R", s), local = new.env())
}

message("\n==== check_claims.R ====")
claims_ok <- local({
  source(here::here("reports", "check_claims.R"), local = TRUE)
  all(claim_status$ok)
})

if ("--reports" %in% args) {
  if (!claims_ok) stop("Some statements in the reports no longer hold; revise them (see above) before rendering.")
  for (f in c("informe_ca.qmd", "informe_es.qmd", "report_en.qmd")) {
    message("\n==== quarto render ", f, " ====")
    status <- system2("quarto", c("render", shQuote(here::here("reports", f))))
    if (status != 0) stop("quarto render failed for ", f)
  }
}
