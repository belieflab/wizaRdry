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
    expect_true(file.exists(file.path(proj, "tmp", paste0(clean_name, ".csv"))))
  })
}
