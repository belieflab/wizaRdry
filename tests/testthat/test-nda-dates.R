# NDA date contract: all API getters deliver ISO (YYYY-MM-DD) by default;
# the nda() pipeline must always write MM/DD/YYYY to every submission CSV,
# regardless of input class (Date, POSIXct, character ISO) or limited_dataset
# mode. Day-shifting to MM/01/YYYY for de-identification applies to
# interview_date only; all other Date fields keep the real day.
#
# Tests are ordered from smallest unit to full pipeline so failures point at
# the right layer.

# ---------------------------------------------------------------------------
# Helpers local to this file
# ---------------------------------------------------------------------------

# Seed .wizaRdry_prefs so to.nda() never fires an interactive readline().
seed_nda_prefs <- function(path) {
  saveRDS(
    list(shown_tree = TRUE, auto_create = TRUE, auto_clean = TRUE,
         auto_nda = TRUE, auto_nda_template = TRUE,
         auto_csv = TRUE, auto_rds = TRUE, auto_sav = TRUE),
    file.path(path, ".wizaRdry_prefs")
  )
}

# Fixture that includes a structure-declared Date field beyond interview_date.
make_dates_fixture_df <- function(n = 3) {
  data.frame(
    src_subject_id          = sprintf("SUB%03d", seq_len(n)),
    subjectkey              = sprintf("NDAR_INV%08d", seq_len(n)),
    interview_date          = rep("06/15/2024", n),
    interview_age           = rep(300L, n),
    sex                     = rep_len(c("F", "M"), n),
    wizardry_dates01_score  = seq_len(n),
    visit_date              = rep("06/20/2024", n),
    stringsAsFactors = FALSE
  )
}

# ============================================================================
# standardize_dates() — unit tests
# ============================================================================

test_that("standardize_dates converts character ISO to MM/DD/YYYY (limited_dataset=TRUE)", {
  df <- data.frame(interview_date = c("2024-06-15", "2024-01-01", NA),
                   stringsAsFactors = FALSE)
  out <- wizaRdry:::standardize_dates(df, verbose = FALSE, limited_dataset = TRUE)
  expect_equal(out$interview_date[1:2], c("06/15/2024", "01/01/2024"))
  expect_true(is.na(out$interview_date[3]))
})

test_that("standardize_dates day-shifts to MM/01/YYYY (limited_dataset=FALSE)", {
  df <- data.frame(interview_date = c("2024-06-15", "2024-01-31"),
                   stringsAsFactors = FALSE)
  out <- wizaRdry:::standardize_dates(df, verbose = FALSE, limited_dataset = FALSE)
  expect_equal(out$interview_date, c("06/01/2024", "01/01/2024"))
})

test_that("standardize_dates handles Date-class input column", {
  df <- data.frame(interview_date = as.Date(c("2024-06-15", "2024-01-01")),
                   stringsAsFactors = FALSE)
  out <- wizaRdry:::standardize_dates(df, verbose = FALSE, limited_dataset = TRUE)
  expect_equal(out$interview_date, c("06/15/2024", "01/01/2024"))
})

test_that("standardize_dates handles POSIXct input column", {
  df <- data.frame(interview_date = as.POSIXct("2024-06-15 10:00:00"),
                   stringsAsFactors = FALSE)
  out <- wizaRdry:::standardize_dates(df, verbose = FALSE, limited_dataset = TRUE)
  expect_equal(out$interview_date, "06/15/2024")
})

test_that("standardize_dates leaves already-MDY character input unchanged", {
  df <- data.frame(interview_date = "06/15/2024", stringsAsFactors = FALSE)
  out <- wizaRdry:::standardize_dates(df, verbose = FALSE, limited_dataset = TRUE)
  expect_equal(out$interview_date, "06/15/2024")
})

test_that("standardize_dates processes multiple date_cols in one call", {
  df <- data.frame(
    interview_date = "2024-06-15",
    visit_date     = "2024-06-20",
    stringsAsFactors = FALSE
  )
  out <- wizaRdry:::standardize_dates(
    df,
    date_cols = c("interview_date", "visit_date"),
    verbose = FALSE, limited_dataset = TRUE
  )
  expect_equal(out$interview_date, "06/15/2024")
  expect_equal(out$visit_date, "06/20/2024")
})

# ============================================================================
# convert_problematic_column_types() — unit tests
# ============================================================================

test_that("convert_problematic_column_types formats Date columns as MM/DD/YYYY", {
  df <- data.frame(visit_date = as.Date(c("2024-06-15", "2024-01-01")),
                   stringsAsFactors = FALSE)
  out <- wizaRdry:::convert_problematic_column_types(df, "test", verbose = FALSE)
  expect_equal(out$visit_date, c("06/15/2024", "01/01/2024"))
  expect_false(any(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", out$visit_date)))
})

test_that("convert_problematic_column_types formats POSIXct columns as MM/DD/YYYY", {
  df <- data.frame(ts = as.POSIXct(c("2024-06-15 10:00:00", "2024-01-01 00:00:00")),
                   stringsAsFactors = FALSE)
  out <- wizaRdry:::convert_problematic_column_types(df, "test", verbose = FALSE)
  expect_equal(out$ts, c("06/15/2024", "01/01/2024"))
})

test_that("convert_problematic_column_types leaves non-date columns untouched", {
  df <- data.frame(score = 42L, label = "foo", stringsAsFactors = FALSE)
  out <- wizaRdry:::convert_problematic_column_types(df, "test", verbose = FALSE)
  expect_equal(out$score, 42L)
  expect_equal(out$label, "foo")
})

# ============================================================================
# to.nda() — submission template direct tests
# These exercise the final write-to-CSV layer without going through the
# validator, which is the last safety net for date formatting.
# ============================================================================

test_that("to.nda() writes Date-class columns as MM/DD/YYYY", {
  td <- withr::local_tempdir()
  seed_nda_prefs(td)
  datestest_class <- data.frame(
    src_subject_id = "S1",
    interview_date = "06/01/2024",
    visit_date     = as.Date("2024-06-15"),
    stringsAsFactors = FALSE
  )
  assign("datestest_class", datestest_class, envir = .GlobalEnv)
  withr::defer(suppressWarnings(rm("datestest_class", envir = .GlobalEnv)))

  wizaRdry::to.nda("datestest_class", path = td)

  raw <- readLines(file.path(td, "tmp", "datestest_class_submission.csv"))
  expect_false(any(grepl("2024-06-15", raw, fixed = TRUE)))
  sub <- utils::read.csv(file.path(td, "tmp", "datestest_class_submission.csv"), skip = 1)
  expect_equal(sub$visit_date, "06/15/2024")
})

test_that("to.nda() writes POSIXct columns as MM/DD/YYYY", {
  td <- withr::local_tempdir()
  seed_nda_prefs(td)
  datestest_posix <- data.frame(
    src_subject_id = "S1",
    interview_date = "06/01/2024",
    collected_at   = as.POSIXct("2024-06-15 10:30:00"),
    stringsAsFactors = FALSE
  )
  assign("datestest_posix", datestest_posix, envir = .GlobalEnv)
  withr::defer(suppressWarnings(rm("datestest_posix", envir = .GlobalEnv)))

  wizaRdry::to.nda("datestest_posix", path = td)

  raw <- readLines(file.path(td, "tmp", "datestest_posix_submission.csv"))
  expect_false(any(grepl("2024-06-15", raw, fixed = TRUE)))
  sub <- utils::read.csv(file.path(td, "tmp", "datestest_posix_submission.csv"), skip = 1)
  expect_equal(sub$collected_at, "06/15/2024")
})

test_that("to.nda() converts character ISO columns to MM/DD/YYYY", {
  td <- withr::local_tempdir()
  seed_nda_prefs(td)
  datestest_iso <- data.frame(
    src_subject_id = "S1",
    interview_date = "06/01/2024",
    onset_date     = "2024-06-15",
    stringsAsFactors = FALSE
  )
  assign("datestest_iso", datestest_iso, envir = .GlobalEnv)
  withr::defer(suppressWarnings(rm("datestest_iso", envir = .GlobalEnv)))

  wizaRdry::to.nda("datestest_iso", path = td)

  sub <- utils::read.csv(file.path(td, "tmp", "datestest_iso_submission.csv"), skip = 1)
  expect_equal(sub$onset_date, "06/15/2024")
})

test_that("to.nda() handles NA and empty values in ISO columns", {
  td <- withr::local_tempdir()
  seed_nda_prefs(td)
  datestest_na <- data.frame(
    src_subject_id = c("S1", "S2", "S3"),
    interview_date = c("06/01/2024", "06/01/2024", "06/01/2024"),
    onset_date     = c("2024-06-15", NA, ""),
    stringsAsFactors = FALSE
  )
  assign("datestest_na", datestest_na, envir = .GlobalEnv)
  withr::defer(suppressWarnings(rm("datestest_na", envir = .GlobalEnv)))

  wizaRdry::to.nda("datestest_na", path = td)

  sub <- utils::read.csv(file.path(td, "tmp", "datestest_na_submission.csv"),
                         skip = 1, na.strings = "")
  expect_equal(sub$onset_date[1], "06/15/2024")
  expect_true(is.na(sub$onset_date[2]))
})

test_that("to.nda() does not mangle free-text containing ISO substrings", {
  td <- withr::local_tempdir()
  seed_nda_prefs(td)
  datestest_text <- data.frame(
    src_subject_id = "S1",
    interview_date = "06/01/2024",
    notes          = "visit on 2024-06-15 then follow-up",
    stringsAsFactors = FALSE
  )
  assign("datestest_text", datestest_text, envir = .GlobalEnv)
  withr::defer(suppressWarnings(rm("datestest_text", envir = .GlobalEnv)))

  wizaRdry::to.nda("datestest_text", path = td)

  sub <- utils::read.csv(file.path(td, "tmp", "datestest_text_submission.csv"), skip = 1)
  expect_equal(sub$notes, "visit on 2024-06-15 then follow-up")
})

# ============================================================================
# nda() pipeline — end-to-end via mocked NDA API
# ============================================================================

test_that("submission CSV has MM/DD/YYYY when interview_date supplied as Date class", {
  skip_if_no_nda_mock()
  proj <- local_wizardry_project()
  measure <- "wizardry01"
  local_globalenv_cleanup(measure)
  withr::local_options(wizaRdry.nda_base_url = nda_mock_url())

  df <- make_nda_fixture_df(measure)
  df$interview_date <- as.Date("2024-06-15")   # Date class: what REDCapR returns
  write_pathway_script(proj, "nda", "redcap", measure, df)

  capture_messages(nda(measure, skip_prompt = TRUE, limited_dataset = TRUE))

  sub_df <- utils::read.csv(
    file.path(proj, "tmp", paste0(measure, "_submission.csv")), skip = 1
  )
  expect_equal(unique(sub_df$interview_date), "06/15/2024")
})

test_that("submission CSV has MM/DD/YYYY when interview_date supplied as character ISO", {
  skip_if_no_nda_mock()
  proj <- local_wizardry_project()
  measure <- "wizardry01"
  local_globalenv_cleanup(measure)
  withr::local_options(wizaRdry.nda_base_url = nda_mock_url())

  df <- make_nda_fixture_df(measure)
  df$interview_date <- "2024-06-15"   # character ISO: what MongoDB / raw REDCap returns
  write_pathway_script(proj, "nda", "redcap", measure, df)

  capture_messages(nda(measure, skip_prompt = TRUE, limited_dataset = TRUE))

  sub_df <- utils::read.csv(
    file.path(proj, "tmp", paste0(measure, "_submission.csv")), skip = 1
  )
  expect_equal(unique(sub_df$interview_date), "06/15/2024")
})

test_that("submission CSV interview_date is day-shifted to MM/01/YYYY with limited_dataset=FALSE", {
  skip_if_no_nda_mock()
  proj <- local_wizardry_project()
  measure <- "wizardry01"
  local_globalenv_cleanup(measure)
  withr::local_options(wizaRdry.nda_base_url = nda_mock_url())

  df <- make_nda_fixture_df(measure)
  df$interview_date <- "2024-06-15"
  write_pathway_script(proj, "nda", "redcap", measure, df)

  capture_messages(nda(measure, skip_prompt = TRUE, limited_dataset = FALSE))

  sub_df <- utils::read.csv(
    file.path(proj, "tmp", paste0(measure, "_submission.csv")), skip = 1
  )
  # Day collapsed to 01; format must still be MM/DD/YYYY
  expect_equal(unique(sub_df$interview_date), "06/01/2024")
  expect_true(all(grepl("^[0-9]{2}/[0-9]{2}/[0-9]{4}$", sub_df$interview_date)))
})

test_that("structure-declared Date field (not interview_date) arrives as MM/DD/YYYY in submission", {
  # Uses wizardry_dates01 fixture which declares visit_date as type Date.
  # The validator runs convert_problematic_column_types early; this test
  # ensures Date-class columns are formatted correctly even when they reach
  # the submission template via that path.
  skip_if_no_nda_mock()
  proj <- local_wizardry_project()
  measure <- "wizardry_dates01"
  local_globalenv_cleanup(measure)
  withr::local_options(wizaRdry.nda_base_url = nda_mock_url())

  df <- make_dates_fixture_df()
  df$visit_date <- as.Date("2024-06-20")   # Date class
  write_pathway_script(proj, "nda", "redcap", measure, df)

  capture_messages(nda(measure, skip_prompt = TRUE, limited_dataset = TRUE))

  sub_df <- utils::read.csv(
    file.path(proj, "tmp", paste0(measure, "_submission.csv")), skip = 1
  )
  expect_equal(unique(sub_df$visit_date), "06/20/2024")
  expect_false(any(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", sub_df$visit_date)))
})

test_that("structure-declared Date field is not day-shifted even with limited_dataset=FALSE", {
  # Day-shifting (MM/01/YYYY) is exclusive to interview_date; all other Date
  # fields keep their real day even when de-identifying.
  skip_if_no_nda_mock()
  proj <- local_wizardry_project()
  measure <- "wizardry_dates01"
  local_globalenv_cleanup(measure)
  withr::local_options(wizaRdry.nda_base_url = nda_mock_url())

  df <- make_dates_fixture_df()
  df$interview_date <- "2024-06-15"
  df$visit_date     <- "2024-06-20"   # character ISO
  write_pathway_script(proj, "nda", "redcap", measure, df)

  capture_messages(nda(measure, skip_prompt = TRUE, limited_dataset = FALSE))

  sub_df <- utils::read.csv(
    file.path(proj, "tmp", paste0(measure, "_submission.csv")), skip = 1
  )
  expect_equal(unique(sub_df$interview_date), "06/01/2024")   # shifted
  expect_equal(unique(sub_df$visit_date),     "06/20/2024")   # real day preserved
})

test_that("no ISO date strings appear anywhere in the submission CSV", {
  # Smoke test: regardless of which pipeline path ran, the raw file content
  # must contain no cells that are solely an ISO date.
  skip_if_no_nda_mock()
  proj <- local_wizardry_project()
  measure <- "wizardry_dates01"
  local_globalenv_cleanup(measure)
  withr::local_options(wizaRdry.nda_base_url = nda_mock_url())

  df <- make_dates_fixture_df()
  df$interview_date <- as.Date("2024-06-15")   # Date class
  df$visit_date     <- as.Date("2024-06-20")   # Date class
  write_pathway_script(proj, "nda", "redcap", measure, df)

  capture_messages(nda(measure, skip_prompt = TRUE, limited_dataset = TRUE))

  raw <- readLines(file.path(proj, "tmp", paste0(measure, "_submission.csv")))
  data_rows <- raw[-(1:2)]   # skip structure-name line and header line
  # No quoted cell should start with a 4-digit year followed by dashes
  expect_false(any(grepl('"[0-9]{4}-[0-9]{2}-[0-9]{2}', data_rows, perl = TRUE)))
})
