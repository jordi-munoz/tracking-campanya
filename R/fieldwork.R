# fieldwork.R --------------------------------------------------------------
# Helpers about the fieldwork calendar, shared by the pipeline (00_setup.R),
# the reports (reports/_common.R) and the data updater (update_data.R).
# Base R only, so any of them can source this file without extra packages.

url_preelectoral <- paste0("https://ep00.epimg.net/infografias/encuestas40db/2026/",
                           "11-preelectoral/2026_11_preelectoral_descargables.zip")

# Tracking day NN is published as 40dB_tracking_diaNN.zip and unpacked into a
# folder of the same name.
tracking_dir <- function(day) sprintf("40dB_tracking_dia%02d", as.integer(day))
tracking_url <- function(day) {
  paste0("https://ep00.epimg.net/infografias/encuestas40db/2026/11-tracking-electoral/",
         tracking_dir(day), ".zip")
}
# Day number of a tracking folder name (NA for any other folder).
dir_day <- function(file_dir) {
  out <- rep(NA_integer_, length(file_dir))
  is_trk <- grepl("^40dB_tracking_dia[0-9]+$", file_dir)
  out[is_trk] <- as.integer(sub("^40dB_tracking_dia", "", file_dir[is_trk]))
  out
}
# Download URL of any folder listed in config/fieldwork_blocks.csv.
source_url <- function(file_dir) {
  ifelse(file_dir == "40dB_preelectoral_I", url_preelectoral,
         ifelse(is.na(dir_day(file_dir)), NA_character_, tracking_url(dir_day(file_dir))))
}

# Row to append to config/fieldwork_blocks.csv for a new tracking day. Each new
# day adds `n` interviews collected over two calendar days, starting on the day
# the previous block ended (e.g. day 6 = 16-17 Nov). 40dB's methodology note
# gives the window of the whole file, so these dates follow the convention of
# the existing rows; R/01_build_data.R checks that the file adds exactly `n` rows.
next_block <- function(blocks, day, n = 500) {
  trk <- blocks[blocks$survey == "tracking", ]
  last_day <- max(dir_day(trk$file_dir), na.rm = TRUE)
  if (as.integer(day) != last_day + 1)
    stop(sprintf("Day %d is not the next tracking day (last configured: %d). ", as.integer(day), last_day),
         "Files must be added in order because each one is aligned with the previous one.")
  start <- max(as.Date(trk$end))
  data.frame(block = max(blocks$block) + 1, survey = "tracking", file_dir = tracking_dir(day),
             start = start, end = start + 1, n = n)
}

# Phase of each event relative to the fieldwork calendar:
#   pre_survey       before the first survey started
#   between_surveys  from the first survey until tracking fieldwork started
#   fieldwork        during tracking fieldwork (up to the end of the last block)
#   post_data        after the last available block (not analysable yet)
# Derived from the block dates so that events enter the analysis automatically
# once fieldwork covers them.
event_phase <- function(date, blocks) {
  date <- as.Date(date)
  first     <- min(as.Date(blocks$start))
  trk_start <- min(as.Date(blocks$start[blocks$survey == "tracking"]))
  last      <- max(as.Date(blocks$end))
  ifelse(date < first, "pre_survey",
         ifelse(date > last, "post_data",
                ifelse(date >= trk_start, "fieldwork", "between_surveys")))
}
