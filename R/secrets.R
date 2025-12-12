# Create a dedicated secrets environment within the package
.secrets_env <- new.env(parent = emptyenv())

#' Load Secrets from File
#'
#' @param secrets_file Path to the secrets file
#' @return Invisibly returns TRUE if successful
#' @noRd
load_secrets <- function(secrets_file = "secrets.R") {
  if (!file.exists(secrets_file)) {
    stop(secrets_file, " file not found. Please create this file and define the required API variables.")
  }

  # Create a temporary environment to evaluate the file
  temp_env <- new.env()

  # Source the file into the temporary environment
  sys.source(secrets_file, envir = temp_env)

  # Clear any existing secrets
  rm(list = ls(.secrets_env), envir = .secrets_env)

  # Transfer objects to our secrets environment
  secret_names <- ls(temp_env)
  for (name in secret_names) {
    assign(name, temp_env[[name]], envir = .secrets_env)
  }

  return(invisible(TRUE))
}

#' Get a Secret Value
#'
#' @param name Name of the secret to retrieve
#' @return The secret value
#' @noRd
get_secret <- function(name) {
  if (!exists(name, envir = .secrets_env)) {
    stop("Secret '", name, "' not found. Ensure it is defined in your secrets file.")
  }

  return(.secrets_env[[name]])
}

#' Get an Optional Secret Value
#'
#' @param name Name of the secret to retrieve
#' @return The secret value, or NULL if not found
#' @noRd
get_secret_optional <- function(name) {
  if (!exists(name, envir = .secrets_env)) {
    return(NULL)
  }

  return(.secrets_env[[name]])
}

#' Assign a Secret Value
#'
#' @param name Name of the secret to set
#' @param value Value to assign
#' @return Invisibly returns TRUE if successful
#' @keywords internal
assign_secret <- function(name, value) {
  assign(name, value, envir = .secrets_env)
  return(invisible(TRUE))
}
