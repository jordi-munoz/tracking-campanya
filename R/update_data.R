# update_data.R ------------------------------------------------------------
# Downloads the 40dB files into data/raw/ and registers new tracking days in
# config/fieldwork_blocks.csv. From the project root:
#
#   Rscript R/update_data.R           # fetch any configured folder that is missing,
#                                     # then every newer tracking day already published
#   Rscript R/update_data.R 7         # fetch tracking day 7 only (must be the next day)
#   Rscript R/update_data.R --check   # report what would be done; change nothing
#
# Also run by `Rscript run_all.R --update`. Base R only.
#
# After it adds a day: check the new row's dates against the methodology note
# (02_Nota_metodologica_*.pdf), add any new events to events/events_29N.csv,
# rerun the pipeline and render the reports. The reports refuse to render if a
# statement in their text no longer matches the data (see reports/_common.R).

root <- here::here()
source(file.path(root, "R", "fieldwork.R"))

args      <- commandArgs(trailingOnly = TRUE)
check     <- "--check" %in% args
req_days  <- suppressWarnings(as.integer(args[grepl("^[0-9]+$", args)]))
dir_raw   <- file.path(root, "data", "raw")
cfg_path  <- file.path(root, "config", "fieldwork_blocks.csv")

read_cfg <- function() {
  b <- utils::read.csv(cfg_path, stringsAsFactors = FALSE)
  b$start <- as.Date(b$start)
  b$end <- as.Date(b$end)
  b
}
write_cfg <- function(b) {
  b$start <- format(b$start)
  b$end <- format(b$end)
  utils::write.csv(b, cfg_path, row.names = FALSE, quote = FALSE)
}
has_data <- function(dir) length(list.files(file.path(dir_raw, dir), "^03_Datos.*\\.dta$")) == 1
is_published <- function(url) {
  status <- tryCatch(attr(curlGetHeaders(url), "status"), error = function(e) NA)
  isTRUE(status == 200)
}

# Downloads a zip and unpacks its files, without the zip's own subfolder, into
# data/raw/<dir>/. Unpacks to a temporary folder first and checks the contents,
# so a failed or unexpected download leaves data/raw/ untouched.
fetch <- function(url, dir) {
  zip <- tempfile(fileext = ".zip")
  tmp <- tempfile()
  on.exit(unlink(c(zip, tmp), recursive = TRUE), add = TRUE)
  utils::download.file(url, zip, mode = "wb", quiet = TRUE)
  utils::unzip(zip, exdir = tmp, junkpaths = TRUE)
  files <- list.files(tmp, full.names = TRUE)
  if (sum(grepl("^03_Datos.*\\.dta$", basename(files))) != 1)
    stop(url, " does not contain exactly one 03_Datos_*.dta file")
  dest <- file.path(dir_raw, dir)
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  stopifnot(all(file.copy(files, dest, overwrite = TRUE)))
  message(sprintf("  %s: %d files", dir, length(files)))
}

blocks <- read_cfg()

# 1. Folders already in the config but missing locally (e.g. a fresh clone).
missing <- Filter(Negate(has_data), unique(blocks$file_dir))
if (length(missing)) {
  message("Missing raw folders: ", paste(missing, collapse = ", "))
  if (!check) for (d in missing) fetch(source_url(d), d)
}

# 2. New tracking days: the ones requested, or every published day after the
#    last configured one.
last_day <- max(dir_day(blocks$file_dir), na.rm = TRUE)
if (length(req_days)) {
  days <- sort(req_days)
} else {
  days <- integer(0)
  d <- last_day + 1
  while (is_published(tracking_url(d))) {
    days <- c(days, d)
    d <- d + 1
  }
}
days <- setdiff(days, dir_day(blocks$file_dir))

if (!length(days)) {
  message("No new tracking days (last configured: day ", last_day, ").")
} else if (check) {
  message("New tracking days available: ", paste(days, collapse = ", "))
} else {
  for (d in days) {
    row <- next_block(blocks, d)   # stops if a day is skipped
    fetch(tracking_url(d), row$file_dir)
    blocks <- rbind(blocks, row)
    write_cfg(blocks)
    message(sprintf("  added block %d: %s, %s to %s, n = %d",
                    row$block, row$file_dir, format(row$start), format(row$end), row$n))
  }
  message("Check the new dates against the methodology note(s) and add any new events to ",
          "events/events_29N.csv, then run `Rscript run_all.R --reports`.")
}
