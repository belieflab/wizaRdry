# scry() bootstrap sanity: the structure it generates must be complete and the
# generated config.yml must be loadable by ConfigEnv out of the box.

test_that("scry() creates the full project structure", {
  proj <- local_wizardry_project()

  apis <- c("csv", "mongo", "qualtrics", "redcap", "oracle", "sql")
  for (api in apis) {
    expect_true(dir.exists(file.path(proj, "clean", api)), label = paste("clean/", api))
    expect_true(dir.exists(file.path(proj, "nda", api)), label = paste("nda/", api))
  }
  expect_true(dir.exists(file.path(proj, "tmp")))
  expect_true(file.exists(file.path(proj, "config.yml")))
  expect_true(file.exists(file.path(proj, "secrets.R")))
  expect_true(file.exists(file.path(proj, "main.R")))
})

test_that("the generated config.yml passes validate_config()", {
  local_wizardry_project()

  config <- validate_config()
  expect_equal(config$identifier, "src_subject_id")
  expect_equal(config$study_alias, "testproj")
})
