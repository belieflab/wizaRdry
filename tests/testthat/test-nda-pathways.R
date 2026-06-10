# nda() decision-tree variants, exercised on a single interface (redcap):
# new structures, strict-mode validation failure, and lenient mode.

test_that("get_nda_base_url() honors the wizaRdry.nda_base_url option", {
  withr::local_options(wizaRdry.nda_base_url = "http://example.test")
  expect_equal(get_nda_base_url(), "http://example.test")
  withr::local_options(wizaRdry.nda_base_url = NULL)
  expect_equal(get_nda_base_url(), "https://nda.nih.gov/api/datadictionary/v2")
})

test_that("nda() treats an unknown structure as NEW and skips the submission file", {
  skip_if_no_nda_mock()
  proj <- local_wizardry_project()
  measure <- "wizardnew01"
  local_globalenv_cleanup(measure)
  withr::local_options(wizaRdry.nda_base_url = nda_mock_url())
  df <- make_nda_fixture_df("wizardry01")
  names(df)[names(df) == "wizardry01_score"] <- "wizardnew01_score"
  write_pathway_script(proj, "nda", "redcap", measure, df)

  msgs <- capture_messages(nda(measure, skip_prompt = TRUE))

  expect_true(any(grepl("Structure Type: NEW", msgs)))
  expect_false(file.exists(file.path(proj, "tmp", paste0(measure, "_submission.csv"))))
  expect_true(file.exists(file.path(proj, "tmp", paste0(measure, "_definitions.xlsx"))))
})

test_that("nda() strict mode fails validation and creates no submission file when required data is missing", {
  skip_if_no_nda_mock()
  proj <- local_wizardry_project()
  measure <- "wizardry01"
  local_globalenv_cleanup(measure)
  withr::local_options(wizaRdry.nda_base_url = nda_mock_url())
  df <- make_nda_fixture_df(measure)
  df$sex <- NA_character_
  write_pathway_script(proj, "nda", "redcap", measure, df)

  msgs <- capture_messages(nda(measure, skip_prompt = TRUE, strict = TRUE))

  expect_true(any(grepl("Status: FAILED", msgs)))
  expect_false(file.exists(file.path(proj, "tmp", paste0(measure, "_submission.csv"))))
})

test_that("nda() lenient mode creates files despite validation failures", {
  skip_if_no_nda_mock()
  proj <- local_wizardry_project()
  measure <- "wizardry01"
  local_globalenv_cleanup(measure)
  withr::local_options(wizaRdry.nda_base_url = nda_mock_url())
  df <- make_nda_fixture_df(measure)
  df$sex <- NA_character_
  write_pathway_script(proj, "nda", "redcap", measure, df)

  msgs <- capture_messages(nda(measure, skip_prompt = TRUE, strict = FALSE))

  expect_true(any(grepl("Status: FAILED", msgs)))
  submission_files <- list.files(
    file.path(proj, "tmp"),
    pattern = paste0("^", measure, "_submission.*\\.csv$")
  )
  expect_gt(length(submission_files), 0)
})
