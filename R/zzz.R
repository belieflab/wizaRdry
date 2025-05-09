#' @importFrom utils capture.output head install.packages setTxtProgressBar txtProgressBar getFromNamespace
NULL

# Create a package environment to store state between functions
.pkg_env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  options(mongolite.quiet = TRUE)
  options(wizaRdry.nda_base_url = "https://nda.nih.gov/api/datadictionary/v2")

  # Try to load secrets silently without any messages
  if (file.exists("secrets.R")) {
    tryCatch({
      load_secrets("secrets.R")
    }, error = function(e) {
      # Store the error for potential use in .onAttach
      assign("secrets_load_error", TRUE, envir = .pkg_env)
      assign("secrets_error_message", conditionMessage(e), envir = .pkg_env)
    })
  }
}

# run scry() on launch if no project structure created
.onAttach <- function(libname, pkgname) {
  # Check if we need to report a secrets loading error
  if (exists("secrets_load_error", envir = .pkg_env)) {
    error_msg <- if (exists("secrets_error_message", envir = .pkg_env)) {
      base::get("secrets_error_message", envir = .pkg_env)
    } else {
      "unknown error"
    }
    packageStartupMessage("Note: Could not load secrets.R at package load time: ", error_msg)
  }

  if (interactive()) {
    # Check if the current directory has a wizaRdry structure
    current_dir <- getwd()
    has_config <- file.exists(file.path(current_dir, "config.yml"))
    has_clean <- dir.exists(file.path(current_dir, "clean"))
    has_nda <- dir.exists(file.path(current_dir, "nda"))
    structure_exists <- has_config || has_clean || has_nda

    if (!structure_exists) {
      packageStartupMessage("Welcome to wizaRdry! Use scry() to initialize a new project structure.")
      #packageStartupMessage("  Tip: Use scry(examples = FALSE) to skip creating example scripts if you're already familiar with wizaRdry.")
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

        # Check for duplicate script names - use get to access the internal function
        tryCatch({
          check_duplicate_scripts <- getFromNamespace("check_duplicate_scripts", "wizaRdry")
          dups <- check_duplicate_scripts(current_dir, quiet = TRUE)
          if (!is.null(dups)) {
            packageStartupMessage("\nWARNING: Duplicate script names found across folders.")
            packageStartupMessage("This may cause conflicts when processing data.")
          }
        }, error = function(e) {
          # Silently ignore any errors in startup checks
        })
      }
    }
  }
}
