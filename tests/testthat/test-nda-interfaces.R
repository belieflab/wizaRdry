# nda() pathway, one test per data-source interface, validated against the
# mocked NDA data dictionary (see setup-nda-mock.R). The measure name must be
# wizardry01 to match the mocked "existing structure" route.

apis <- c("csv", "redcap", "qualtrics", "mongo", "sql", "oracle")

for (api in apis) {
  test_that(sprintf("nda() processes the %s interface against an existing structure", api), {
    skip_if_no_nda_mock()
    proj <- local_wizardry_project()
    measure <- "wizardry01"
    local_globalenv_cleanup(measure)
    withr::local_options(wizaRdry.nda_base_url = nda_mock_url())
    write_pathway_script(proj, "nda", api, measure, make_nda_fixture_df(measure))

    msgs <- capture_messages(nda(measure, skip_prompt = TRUE))

    expect_true(any(grepl("Status: PASSED", msgs)))
    expect_true(any(grepl("Structure Type: EXISTING", msgs)))

    submission <- file.path(proj, "tmp", paste0(measure, "_submission.csv"))
    expect_true(file.exists(submission))
    # NDA submission format: line 1 = "structurename,version", headers on line 2
    expect_equal(readLines(submission, n = 1), "wizardry,01")
    sub_df <- utils::read.csv(submission, skip = 1)
    expect_setequal(
      names(sub_df),
      c("subjectkey", "src_subject_id", "interview_date", "interview_age",
        "sex", "wizardry01_score")
    )
    # Data must survive the pipeline unchanged
    expect_equal(nrow(sub_df), 3)
    expect_equal(sub_df$src_subject_id, c("SUB001", "SUB002", "SUB003"))
    expect_equal(sub_df$wizardry01_score, 1:3)
    expect_equal(sub_df$interview_age, rep(300L, 3))
    expect_true(all(grepl("^NDAR_INV", sub_df$subjectkey)))
  })
}
