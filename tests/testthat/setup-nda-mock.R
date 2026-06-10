# Local mock of the NDA data dictionary API (https://nda.nih.gov/api/datadictionary/v2).
# Started once per test run; tests point the package at it via
# withr::local_options(wizaRdry.nda_base_url = nda_mock_url()).
# Serving fixture JSON through a real localhost server exercises the package's
# actual httr/jsonlite parsing code paths without touching the network.
#
# Handlers run in a separate process: they can only use objects stored in
# app$locals (copied to the server process), never functions from this file.

if (requireNamespace("webfakes", quietly = TRUE)) {

  nda_app <- webfakes::new_app()

  read_fixture <- function(name) {
    paste(
      readLines(testthat::test_path("fixtures", name), warn = FALSE),
      collapse = "\n"
    )
  }
  nda_app$locals$fixtures <- list(
    ndar_subject01     = read_fixture("ndar_subject01.json"),
    wizardry01         = read_fixture("wizardry01.json"),
    datastructure_list = read_fixture("datastructure_list.json")
  )

  # Route order matters: most specific first.
  nda_app$get("/datastructure/dataElement/:el", function(req, res) {
    # Element-existence probe: empty array means "not a known NDA element"
    res$set_header("Content-Type", "application/json")
    res$send("[]")
  })

  nda_app$get("/datastructure/ndar_subject01", function(req, res) {
    res$set_header("Content-Type", "application/json")
    res$send(res$app$locals$fixtures$ndar_subject01)
  })

  nda_app$get("/datastructure/wizardry01", function(req, res) {
    res$set_header("Content-Type", "application/json")
    res$send(res$app$locals$fixtures$wizardry01)
  })

  nda_app$get("/datastructure", function(req, res) {
    res$set_header("Content-Type", "application/json")
    res$send(res$app$locals$fixtures$datastructure_list)
  })

  # Everything else (unknown structures, element metadata) => 404,
  # which drives the "new structure" pathway in nda().
  nda_app$all(webfakes::new_regexp(".*"), function(req, res) {
    res$set_status(404L)
    res$send("")
  })

  .nda_mock <- webfakes::new_app_process(nda_app)
  withr::defer(.nda_mock$stop(), teardown_env())

  nda_mock_url <- function() sub("/+$", "", .nda_mock$url())
}
