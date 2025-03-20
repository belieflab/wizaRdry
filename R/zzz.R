#' @importFrom utils capture.output head install.packages setTxtProgressBar txtProgressBar
NULL

.onLoad <- function(libname, pkgname) {
  # Initialize package environment
  .wizaRdry_env <- new.env(parent = emptyenv())

  # Populate with necessary variables
  assign("api_base_url", "https://nda.nih.gov/api/datadictionary/v2", envir = .wizaRdry_env)

  # Expose the environment to the namespace
  assign(".wizaRdry_env", .wizaRdry_env, envir = asNamespace(pkgname))
}
