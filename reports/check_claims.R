# check_claims.R -----------------------------------------------------------
# Prints whether each statement in the reports' text still matches the results
# in output/tables, without rendering. Run after `Rscript run_all.R` (which
# calls it at the end) or on its own from the project root:
#   Rscript reports/check_claims.R
# The statements and their conditions are in report_claims() in _common.R.

lang <- "en"
old_allow <- Sys.getenv("ALLOW_STALE_CLAIMS", NA)
Sys.setenv(ALLOW_STALE_CLAIMS = "1")   # list every claim instead of stopping at the first failure
source(here::here("reports", "_common.R"), local = TRUE)
if (is.na(old_allow)) Sys.unsetenv("ALLOW_STALE_CLAIMS") else Sys.setenv(ALLOW_STALE_CLAIMS = old_allow)

for (i in seq_len(nrow(claim_status))) {
  message(sprintf("  %s  [%s] %s", if (claim_status$ok[i]) "ok   " else "STALE",
                  claim_status$section[i], claim_status$claim[i]))
}
n_bad <- sum(!claim_status$ok)
if (n_bad) {
  message(n_bad, " statement(s) in the reports no longer hold: revise those paragraphs in all three ",
          "reports and update report_claims() in reports/_common.R.")
} else {
  message("All ", nrow(claim_status), " statements in the reports still hold.")
}
