# Runs the full pipeline. From the project root:
#   Rscript run_all.R
scripts <- c("01_build_data.R", "02_descriptives.R", "03_event_analysis.R", "04_heterogeneity.R",
             "05_uesd_checks.R")
for (s in scripts) {
  message("\n==== ", s, " ====")
  source(here::here("R", s), local = new.env())
}
