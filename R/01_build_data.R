# 01_build_data.R --------------------------------------------------------
# Builds a respondent-level dataset with one row per unique interview and its
# fieldwork block.
#
# 40dB publishes the tracking as cumulative/rolling files: each file is the
# previous one, minus the oldest block once the window is full, plus the newest
# 500 interviews appended at the end. `id` is renumbered and `ponde` recomputed
# in every file, so respondents are matched on their full response profile and
# by position. New tracking days are picked up automatically once their folder
# is added to data/raw/ and a row is added to config/fieldwork_blocks.csv.

source(here::here("R", "00_setup.R"))

read_raw <- function(dir) {
  f <- list.files(file.path(dir_raw, dir), pattern = "^03_Datos.*\\.dta$", full.names = TRUE)
  stopifnot(length(f) == 1)
  read_dta(f)
}

# Tracking -------------------------------------------------------------
trk_blocks <- filter(blocks, survey == "tracking")
trk_dirs   <- unique(trk_blocks$file_dir)

# Rows of `cur` that are new relative to `prev`: find the number of rows dropped
# from the start of `prev` such that the remainder is a prefix of `cur`.
new_rows <- function(prev, cur) {
  kp <- row_key(prev); kc <- row_key(cur)
  for (drop in 0:length(kp)) {
    keep <- length(kp) - drop
    if (keep <= length(kc) && identical(kc[seq_len(keep)], kp[drop + seq_len(keep)]))
      return(list(rows = setdiff(seq_along(kc), seq_len(keep)), dropped = drop))
  }
  stop("Could not align tracking file with the previous one.")
}

pieces <- list(); prev <- NULL
for (dir in trk_dirs) {
  cur <- read_raw(dir)
  if (is.null(prev)) {
    rows <- seq_len(nrow(cur)); dropped <- 0
  } else {
    nr <- new_rows(prev, cur); rows <- nr$rows; dropped <- nr$dropped
  }
  bl <- filter(trk_blocks, file_dir == dir)
  if (length(rows) != sum(bl$n))
    stop(sprintf("%s: %d new rows but config expects %d", dir, length(rows), sum(bl$n)))
  message(sprintf("%s: %d rows, %d dropped from previous, %d new -> block(s) %s",
                  dir, nrow(cur), dropped, length(rows), paste(bl$block, collapse = ",")))
  pieces[[dir]] <- cur[rows, ] |>
    mutate(block = rep(bl$block, bl$n), source_file = dir, ponde_src = ponde)
  prev <- cur
}
trk <- bind_rows(lapply(pieces, zap_labels))
message("Tracking respondents: ", nrow(trk))

# Preelectoral I (2-4 Nov): rename to tracking numbering. Value labels of all
# mapped items were checked to be identical in both questionnaires.
demo_vars <- c("id", "sexo", "edad", "edad_r", "hab", "hab_r", "prov", "ccaa", "hijos", "edu",
               "educacion_r", "situacion_laboral_r", "cs", "clase_social_r")
pre_map <- c(p1 = "p2", p2 = "p3", p3 = "p4", set_names(paste0("p5_", 1:10), paste0("p4_", 1:10)),
             p5 = "p6", p6 = "p7", p7 = "p20", p8 = "p21")
pre <- read_raw(filter(blocks, survey == "preelectoral_I")$file_dir) |>
  zap_labels() |>
  select(all_of(demo_vars), all_of(pre_map), ponde) |>
  mutate(block = 0, source_file = "40dB_preelectoral_I", ponde_src = ponde)
stopifnot(setequal(names(pre), names(trk)))

# Stack, derive variables, attach dates ---------------------------------
dat <- bind_rows(pre, trk) |>
  mutate(resp_id = row_number()) |>
  # Some interviews are exact copies of another one on all 31 variables (exact
  # age, province, ten PTVs...), mostly across different fieldwork days and
  # within the same delivered file: almost surely duplicated records or
  # re-interviewed panelists. Keep them, but flag later copies for robustness.
  mutate(profile = row_key(pick(all_of(c(demo_vars, names(pre_map))))),
         dup_copy = duplicated(profile),
         dup_any  = profile %in% profile[dup_copy]) |>
  select(-profile) |>
  derive_vars() |>
  left_join(select(blocks, block, survey, start, end, mid), by = "block")

# Weights: rake every block separately to the same population margins, taken
# from 40dB's weighted day-4 file (2,500 cases). This makes daily samples
# comparable on the quota variables regardless of which file they came from.
ref <- derive_vars(zap_labels(read_raw("40dB_tracking_dia04")))
targets <- rake_targets(ref, ref$ponde)
dat <- dat |>
  group_by(block) |>
  group_modify(~ mutate(.x, w = rake_weights(.x, targets))) |>
  ungroup() |>
  relocate(resp_id, block, survey, start, end)

message("Sample, duplicates and weights by block:")
dat |>
  group_by(block) |>
  summarise(n = n(), dup_copies = sum(dup_copy), w_min = min(w), w_max = max(w),
            deff_w = 1 + var(w), r_w_ponde = cor(w, ponde_src)) |>
  print()

write_rds(dat, file.path(dir_proc, "respondents.rds"))
write_csv(select(dat, resp_id:end, source_file, id, ponde_src, w, everything()),
          file.path(dir_proc, "respondents.csv"))
message("Saved ", nrow(dat), " respondents to data/processed/")
