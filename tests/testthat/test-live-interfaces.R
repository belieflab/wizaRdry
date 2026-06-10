# Live integration layer. Skipped entirely unless WIZARDRY_LIVE_PROJECT points
# at a real wizaRdry project directory containing a working config.yml and
# secrets.R. Each interface test additionally requires an env var naming a
# measure to pull. The clean()/nda() tests run scripts that already exist in
# the live project, against the real NDA data dictionary API.
#
# Example:
#   WIZARDRY_LIVE_PROJECT=~/studies/capr \
#   WIZARDRY_LIVE_REDCAP=demo_measure \
#   WIZARDRY_LIVE_NDA=eefrt01 \
#   Rscript -e 'testthat::test_file("tests/testthat/test-live-interfaces.R")'
#
# Env vars:
#   WIZARDRY_LIVE_PROJECT    path to a configured wizaRdry project (required)
#   WIZARDRY_LIVE_REDCAP     REDCap instrument name for redcap()
#   WIZARDRY_LIVE_QUALTRICS  Qualtrics survey alias for qualtrics()
#   WIZARDRY_LIVE_MONGO      MongoDB collection name for mongo()
#   WIZARDRY_LIVE_SQL        SQL table name for sql()
#   WIZARDRY_LIVE_ORACLE     Oracle table name for oracle()
#   WIZARDRY_LIVE_CLEAN      measure with an existing clean/{api}/ script
#   WIZARDRY_LIVE_NDA        structure with an existing nda/{api}/ script

local_live_project <- function(env = parent.frame()) {
  proj <- Sys.getenv("WIZARDRY_LIVE_PROJECT")
  withr::local_dir(proj, .local_envir = env)
  # Live credentials are loaded from the project's own secrets.R
  sys.source("secrets.R", envir = globalenv())
  proj
}

live_measure <- function(var) {
  measure <- Sys.getenv(var, "")
  testthat::skip_if(!nzchar(measure), sprintf("Set %s to run this test", var))
  measure
}

test_that("live: redcap() pulls real data", {
  skip_on_cran()
  skip_if_no_live_project()
  measure <- live_measure("WIZARDRY_LIVE_REDCAP")
  local_live_project()

  df <- redcap(measure)
  expect_s3_class(df, "data.frame")
  expect_gt(nrow(df), 0)
})

test_that("live: qualtrics() pulls real data", {
  skip_on_cran()
  skip_if_no_live_project()
  measure <- live_measure("WIZARDRY_LIVE_QUALTRICS")
  local_live_project()

  df <- qualtrics(measure)
  expect_s3_class(df, "data.frame")
  expect_gt(nrow(df), 0)
})

test_that("live: mongo() pulls real data", {
  skip_on_cran()
  skip_if_no_live_project()
  measure <- live_measure("WIZARDRY_LIVE_MONGO")
  local_live_project()

  df <- mongo(measure)
  expect_s3_class(df, "data.frame")
  expect_gt(nrow(df), 0)
})

test_that("live: sql() pulls real data", {
  skip_on_cran()
  skip_if_no_live_project()
  measure <- live_measure("WIZARDRY_LIVE_SQL")
  local_live_project()

  df <- sql(measure)
  expect_s3_class(df, "data.frame")
  expect_gt(nrow(df), 0)
})

test_that("live: oracle() pulls real data", {
  skip_on_cran()
  skip_if_no_live_project()
  measure <- live_measure("WIZARDRY_LIVE_ORACLE")
  local_live_project()

  df <- oracle(measure)
  expect_s3_class(df, "data.frame")
  expect_gt(nrow(df), 0)
})

test_that("live: full clean() run", {
  skip_on_cran()
  skip_if_no_live_project()
  measure <- live_measure("WIZARDRY_LIVE_CLEAN")
  local_live_project()
  local_globalenv_cleanup(measure)

  clean(measure, skip_prompt = TRUE)
  expect_true(exists(paste0(measure, "_clean"), envir = .GlobalEnv))
})

test_that("live: full nda() run against the real NDA data dictionary", {
  skip_on_cran()
  skip_if_no_live_project()
  measure <- live_measure("WIZARDRY_LIVE_NDA")
  proj <- local_live_project()
  local_globalenv_cleanup(measure)

  msgs <- capture_messages(nda(measure, skip_prompt = TRUE))
  expect_true(any(grepl("NDA processing complete", msgs)))
})
