# clean() pathway, one test per data-source interface. Dispatch is decided by
# which clean/{api}/ directory holds the script, so a fixture-backed script
# exercises the full pathway (dispatch, sourcing, testSuite, export) offline.

apis <- c("csv", "redcap", "qualtrics", "mongo", "sql", "oracle")

for (api in apis) {
  test_that(sprintf("clean() dispatches and processes the %s interface", api), {
    proj <- local_wizardry_project()
    measure <- paste0("wiz", api)
    local_globalenv_cleanup(measure)
    write_pathway_script(
      proj, "clean", api, measure,
      make_fixture_df(measure, qualtrics = identical(api, "qualtrics"))
    )

    msgs <- capture_messages(clean(measure, csv = TRUE, skip_prompt = TRUE))

    clean_name <- paste0(measure, "_clean")
    expect_true(exists(clean_name, envir = .GlobalEnv))
    df_clean <- base::get(clean_name, envir = .GlobalEnv)
    expect_s3_class(df_clean, "data.frame")
    expect_equal(nrow(df_clean), 3)
    expect_true(paste0(measure, "_score") %in% names(df_clean))

    # The exported CSV must round-trip the cleaned data frame
    csv_path <- file.path(proj, "tmp", paste0(clean_name, ".csv"))
    expect_true(file.exists(csv_path))
    exported <- utils::read.csv(csv_path)
    expect_equal(nrow(exported), nrow(df_clean))
    expect_setequal(names(exported), names(df_clean))
    expect_equal(exported$src_subject_id, df_clean$src_subject_id)
    expect_equal(exported[[paste0(measure, "_score")]], df_clean[[paste0(measure, "_score")]])
  })
}
