#' Resolve the NDA data dictionary base URL
#'
#' Reads the `wizaRdry.nda_base_url` option (set in `.onLoad`), falling back
#' to the production NDA API. Tests point this option at a local mock server.
#'
#' @return Character scalar, base URL without trailing slash.
#' @keywords internal
#' @noRd
get_nda_base_url <- function() {
  getOption("wizaRdry.nda_base_url",
            "https://nda.nih.gov/api/datadictionary/v2")
}
