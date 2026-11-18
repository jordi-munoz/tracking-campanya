# Tests for R/fieldwork.R. Run from the project root:
#   Rscript -e 'testthat::test_dir("tests/testthat")'

source(file.path(here::here(), "R", "fieldwork.R"))

blocks_fixture <- function() {
  data.frame(
    block    = 0:3,
    survey   = c("preelectoral_I", "tracking", "tracking", "tracking"),
    file_dir = c("40dB_preelectoral_I", "40dB_tracking_dia02", "40dB_tracking_dia02", "40dB_tracking_dia03"),
    start    = as.Date(c("2026-11-02", "2026-11-10", "2026-11-12", "2026-11-13")),
    end      = as.Date(c("2026-11-04", "2026-11-12", "2026-11-13", "2026-11-14")),
    n        = c(2000, 1000, 500, 500)
  )
}

test_that("tracking folder names and URLs follow 40dB's naming", {
  expect_equal(tracking_dir(6), "40dB_tracking_dia06")
  expect_equal(tracking_dir(12), "40dB_tracking_dia12")
  expect_match(tracking_url(7), "/11-tracking-electoral/40dB_tracking_dia07\\.zip$")
})

test_that("dir_day extracts the day and returns NA for other folders", {
  expect_equal(dir_day(c("40dB_tracking_dia02", "40dB_tracking_dia10", "40dB_preelectoral_I")),
               c(2L, 10L, NA_integer_))
  expect_equal(dir_day(character(0)), integer(0))
})

test_that("source_url covers the pre-election survey and tracking days", {
  expect_equal(source_url("40dB_preelectoral_I"), url_preelectoral)
  expect_equal(source_url("40dB_tracking_dia05"), tracking_url(5))
  expect_true(is.na(source_url("something_else")))
})

test_that("next_block continues the two-day convention", {
  row <- next_block(blocks_fixture(), 4)
  expect_equal(row$block, 4)
  expect_equal(row$file_dir, "40dB_tracking_dia04")
  expect_equal(row$start, as.Date("2026-11-14"))
  expect_equal(row$end, as.Date("2026-11-15"))
  expect_equal(row$n, 500)
})

test_that("next_block refuses to skip or repeat a day", {
  expect_error(next_block(blocks_fixture(), 5), "not the next tracking day")
  expect_error(next_block(blocks_fixture(), 3), "not the next tracking day")
})

test_that("event_phase places events relative to the fieldwork calendar", {
  b <- blocks_fixture()
  dates <- as.Date(c("2026-10-29", "2026-11-03", "2026-11-04", "2026-11-06",
                     "2026-11-10", "2026-11-14", "2026-11-15"))
  expect_equal(event_phase(dates, b),
               c("pre_survey", "between_surveys", "between_surveys", "between_surveys",
                 "fieldwork", "fieldwork", "post_data"))
})

test_that("event_phase moves an event into fieldwork when a new block covers it", {
  b <- blocks_fixture()
  expect_equal(event_phase("2026-11-15", b), "post_data")
  b2 <- rbind(b, next_block(b, 4))
  expect_equal(event_phase("2026-11-15", b2), "fieldwork")
})

test_that("event_phase reproduces the phases in the project's events file", {
  root <- here::here()
  b  <- utils::read.csv(file.path(root, "config", "fieldwork_blocks.csv"))
  ev <- utils::read.csv(file.path(root, "events", "events_29N.csv"), encoding = "UTF-8")
  ph <- stats::setNames(event_phase(ev$date, b), ev$event_id)
  expect_equal(unname(ph[c("E01", "E03", "E04", "E05", "E13")]),
               c("pre_survey", "between_surveys", "between_surveys", "fieldwork", "fieldwork"))
  expect_equal(unname(ph["E23"]), "post_data")   # election day is always after the data
})
