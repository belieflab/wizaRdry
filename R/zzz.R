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

# run scry() on launch if no project structure created
.onAttach <- function(libname, pkgname) {
  if (interactive()) {
    # Check if the current directory has a wizaRdry structure
    current_dir <- getwd()
    has_config <- file.exists(file.path(current_dir, "config.yml"))
    has_clean <- dir.exists(file.path(current_dir, "clean"))
    has_nda <- dir.exists(file.path(current_dir, "nda"))

    structure_exists <- has_config || has_clean || has_nda

    if (!structure_exists) {
      packageStartupMessage("Welcome to wizaRdry! Use scry() to initialize a new project structure.")
    } else {
      # Check if structure is complete
      dirs_to_check <- c(
        file.path(current_dir, "clean"),
        file.path(current_dir, "clean", "mongo"),
        file.path(current_dir, "clean", "qualtrics"),
        file.path(current_dir, "clean", "redcap"),
        file.path(current_dir, "nda"),
        file.path(current_dir, "nda", "mongo"),
        file.path(current_dir, "nda", "qualtrics"),
        file.path(current_dir, "nda", "redcap"),
        file.path(current_dir, "nda", "tmp"),
        file.path(current_dir, "tmp")
      )

      files_to_check <- c(
        file.path(current_dir, "config.yml"),
        file.path(current_dir, "secrets.R"),
        file.path(current_dir, "main.R")
      )

      structure_complete <- all(sapply(dirs_to_check, dir.exists)) &&
        all(sapply(files_to_check, file.exists))

      if (!structure_complete) {
        packageStartupMessage("wizaRdry structure detected but incomplete. Use scry(repair = TRUE) to repair it.")
      } else {
        packageStartupMessage("wizaRdry project structure detected and complete.")
      }
    }
  }
}
