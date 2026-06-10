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

  # Registration bundle: data definition, subject count, category/description
  defs_path <- file.path(proj, "tmp", paste0(measure, "_definitions.xlsx"))
  expect_true(file.exists(defs_path))
  defs <- openxlsx2::wb_to_df(openxlsx2::wb_load(defs_path), sheet = "Data Definitions")
  expect_true(all(
    c("subjectkey", "src_subject_id", "interview_date", "interview_age",
      "sex", paste0(measure, "_score")) %in% defs$ElementName
  ))

  count_path <- file.path(proj, "tmp", paste0(measure, "_subject_count.txt"))
  expect_true(file.exists(count_path))
  expect_equal(trimws(readLines(count_path, n = 1)), "3")

  expect_true(file.exists(
    file.path(proj, "tmp", paste0(measure, "_category-and-description.xlsx"))
  ))
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

test_that("nda() strict mode treats value-range violations as MODIFIED and creates no files", {
  skip_if_no_nda_mock()
  proj <- local_wizardry_project()
  measure <- "wizardry01"
  local_globalenv_cleanup(measure)
  withr::local_options(wizaRdry.nda_base_url = nda_mock_url())
  df <- make_nda_fixture_df(measure)
  # wizardry01_score has valueRange 0::100 in the mocked structure
  df$wizardry01_score <- c(150L, 200L, 175L)
  write_pathway_script(proj, "nda", "redcap", measure, df)

  msgs <- capture_messages(nda(measure, skip_prompt = TRUE, strict = TRUE))

  expect_true(any(grepl("Structure Type: MODIFIED", msgs)))
  expect_true(any(grepl("Status: FAILED", msgs)))
  expect_length(list.files(file.path(proj, "tmp")), 0)
})

test_that("nda() lenient mode creates draft submission and data definition for MODIFIED structures", {
  skip_if_no_nda_mock()
  proj <- local_wizardry_project()
  measure <- "wizardry01"
  local_globalenv_cleanup(measure)
  withr::local_options(wizaRdry.nda_base_url = nda_mock_url())
  df <- make_nda_fixture_df(measure)
  df$wizardry01_score <- c(150L, 200L, 175L)
  write_pathway_script(proj, "nda", "redcap", measure, df)

  msgs <- capture_messages(nda(measure, skip_prompt = TRUE, strict = FALSE))

  expect_true(any(grepl("Structure Type: MODIFIED", msgs)))
  # Modified structures must NOT produce a final submission file
  expect_false(file.exists(file.path(proj, "tmp", paste0(measure, "_submission.csv"))))

  # The draft must keep the NDA two-row format and preserve the violating
  # values verbatim (they are what the DCC needs to approve)
  draft_path <- file.path(proj, "tmp", paste0(measure, "_submission_draft.csv"))
  expect_true(file.exists(draft_path))
  expect_equal(readLines(draft_path, n = 1), "wizardry,01")
  draft <- utils::read.csv(draft_path, skip = 1)
  expect_equal(draft$wizardry01_score, c(150L, 200L, 175L))
  expect_equal(draft$src_subject_id, c("SUB001", "SUB002", "SUB003"))

  # The data definition must describe every submitted field
  defs_path <- file.path(proj, "tmp", paste0(measure, "_definitions.xlsx"))
  expect_true(file.exists(defs_path))
  defs <- openxlsx2::wb_to_df(openxlsx2::wb_load(defs_path), sheet = "Data Definitions")
  expect_setequal(
    defs$ElementName,
    c("subjectkey", "src_subject_id", "interview_date", "interview_age",
      "sex", "wizardry01_score")
  )
  expect_equal(defs$DataType[defs$ElementName == "wizardry01_score"], "Integer")
})

test_that("nda() with limited_dataset=FALSE date-shifts and age-caps the submission file", {
  skip_if_no_nda_mock()
  proj <- local_wizardry_project()
  measure <- "wizardry01"
  local_globalenv_cleanup(measure)
  withr::local_options(wizaRdry.nda_base_url = nda_mock_url())
  df <- make_nda_fixture_df(measure)
  df$interview_date <- "06/15/2024"
  df$interview_age <- c(300L, 900L, 1100L)
  write_pathway_script(proj, "nda", "redcap", measure, df)

  capture_messages(nda(measure, skip_prompt = TRUE, limited_dataset = FALSE))

  submission <- file.path(proj, "tmp", paste0(measure, "_submission.csv"))
  expect_true(file.exists(submission))
  sub_df <- utils::read.csv(submission, skip = 1)
  # Date-shifting: day collapses to 01 (MM/DD/YYYY -> MM/01/YYYY)
  expect_equal(unique(sub_df$interview_date), "06/01/2024")
  # Age-capping: ages above 1068 months (89 years) are capped; others untouched
  expect_equal(sub_df$interview_age, c(300L, 900L, 1068L))
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
