# First, install R6 if you don't have it

#' Secrets Environment Class
#'
#' The SecretsEnv class handles validation of API credentials stored in a secrets file.
#' It checks for the presence of required variables, validates their types, and ensures
#' they are properly formatted.
#'
#' @title Secrets Environment Class
#' @description A class to manage and validate API credentials and secrets.
#' @export
#' @importFrom R6 R6Class
SecretsEnv <- R6::R6Class("SecretsEnv",
                          public = list(
                            #' @field config_specs List of specifications for each API
                            config_specs = list(
                              redcap = list(
                                required = c("uri", "token"),
                                types = c(uri = "character", token = "character")
                              ),
                              mongo = list(
                                required = c("connectionString"),
                                types = c(connectionString = "character")
                              ),
                              qualtrics = list(
                                required = c("apiKeys", "baseUrls"),
                                types = c(apiKeys = "vector", baseUrls = "vector")
                              ),
                              sql = list(
                                required = c("host", "uid", "pwd"),
                                types = c(host = "character", uid = "character", pwd = "character")
                              )
                            ),

                            #' @field secrets_file Path to the secrets file
                            secrets_file = NULL,

                            #' @field config Configuration object loaded from config.yml
                            config = NULL,

                            #' @description Initialize a new SecretsEnv object
                            #' @param secrets_file Path to the secrets file
                            #' @param config_file Path to the configuration file
                            #' @return A new SecretsEnv object
                            initialize = function(secrets_file = "secrets.R", config_file = "config.yml") {
                              # Check if secrets file exists
                              if (!file.exists(secrets_file)) {
                                stop(secrets_file, " file not found. Please create this file and define the required API variables.")
                              }

                              # Store the secrets file path
                              self$secrets_file <- secrets_file

                              # Load secrets if not already loaded
                              load_secrets(secrets_file)

                              # Try to load config to determine which APIs are configured
                              tryCatch({
                                self$config <- config::get(file = config_file)
                              }, error = function(e) {
                                warning("Could not load config.yml: ", e$message,
                                        ". Will validate all API types.")
                              })

                              # Only validate APIs that are configured
                              apis_to_validate <- self$get_configured_apis()

                              for (api_type in apis_to_validate) {
                                self$validate_config(api_type)
                              }
                            },

                            #' @description Determine which APIs are configured
                            #' @return Character vector of configured API names
                            get_configured_apis = function() {
                              if (is.null(self$config)) {
                                # If config couldn't be loaded, return an empty list
                                return(character(0))
                              }

                              # Check which API sections exist in the config
                              configured_apis <- character(0)

                              if (!is.null(self$config$mongo)) {
                                configured_apis <- c(configured_apis, "mongo")
                              }

                              if (!is.null(self$config$qualtrics)) {
                                configured_apis <- c(configured_apis, "qualtrics")
                              }

                              if (!is.null(self$config$redcap)) {
                                configured_apis <- c(configured_apis, "redcap")
                              }

                              if (!is.null(self$config$sql)) {
                                configured_apis <- c(configured_apis, "sql")
                              }

                              return(configured_apis)
                            },

                            #' @description Validate the configuration for a specific API
                            #' @param api_type The API type to validate
                            #' @return TRUE if validation passes
                            validate_config = function(api_type) {
                              if (!api_type %in% names(self$config_specs)) {
                                stop("Unknown API type: '", api_type, "'. Valid types are: ",
                                     base::paste(names(self$config_specs), collapse=", "))
                              }

                              specs <- self$config_specs[[api_type]]
                              all_errors <- c()

                              # Check that required variables exist in secrets environment
                              missing_vars <- specs$required[!base::sapply(specs$required, function(var) {
                                tryCatch({
                                  get_secret(var)
                                  return(TRUE)
                                }, error = function(e) {
                                  return(FALSE)
                                })
                              })]

                              if (length(missing_vars) > 0) {
                                all_errors <- c(all_errors, paste("Missing variables:",
                                                                  base::paste(missing_vars, collapse=", ")))
                              }

                              # Only check existing variables for type and emptiness
                              existing_vars <- specs$required[base::sapply(specs$required, function(var) {
                                tryCatch({
                                  get_secret(var)
                                  return(TRUE)
                                }, error = function(e) {
                                  return(FALSE)
                                })
                              })]

                              # Check variable types and emptiness for variables that exist
                              for (type_name in unique(specs$types)) {
                                # Get variables expected to be of this type that exist
                                vars_of_type_names <- names(specs$types[specs$types == type_name])
                                vars_of_type <- vars_of_type_names[vars_of_type_names %in% existing_vars]

                                if (length(vars_of_type) > 0) {
                                  # Check which ones fail the type check
                                  failing_vars <- vars_of_type[base::sapply(vars_of_type, function(var) {
                                    var_value <- get_secret(var)

                                    # Check the appropriate type
                                    !switch(type_name,
                                            "character" = is.character(var_value),
                                            "vector" = is.vector(var_value),
                                            FALSE  # Default case for unknown types
                                    )
                                  })]

                                  # Report if any variables fail this type check
                                  if (length(failing_vars) > 0) {
                                    if (type_name == "character") {
                                      all_errors <- c(all_errors, paste("Type error:",
                                                                        base::paste(failing_vars, collapse=", "),
                                                                        "must be defined as character strings using quotes."))
                                    } else if (type_name == "vector") {
                                      all_errors <- c(all_errors, paste("Type error:",
                                                                        base::paste(failing_vars, collapse=", "),
                                                                        "must be defined as vectors using c() function."))
                                    }
                                  }

                                  # Only check emptiness for variables of the correct type
                                  correct_type_vars <- vars_of_type[!vars_of_type %in% failing_vars]

                                  # For character variables, check for empty strings
                                  if (type_name == "character" && length(correct_type_vars) > 0) {
                                    empty_vars <- correct_type_vars[base::sapply(correct_type_vars, function(var) {
                                      var_value <- get_secret(var)
                                      nchar(var_value) == 0 || all(trimws(var_value) == "")
                                    })]

                                    if (length(empty_vars) > 0) {
                                      all_errors <- c(all_errors, paste("Empty value error:",
                                                                        base::paste(empty_vars, collapse=", "),
                                                                        "cannot be empty strings."))
                                    }
                                  }

                                  # For vector variables, check if they're empty
                                  if (type_name == "vector" && length(correct_type_vars) > 0) {
                                    empty_vars <- correct_type_vars[base::sapply(correct_type_vars, function(var) {
                                      var_value <- get_secret(var)
                                      length(var_value) == 0
                                    })]

                                    if (length(empty_vars) > 0) {
                                      all_errors <- c(all_errors, paste("Empty vector error:",
                                                                        base::paste(empty_vars, collapse=", "),
                                                                        "cannot be empty vectors."))
                                    }
                                  }

                                  # Special checks for specific API types
                                  if (api_type == "redcap" && "uri" %in% correct_type_vars) {
                                    uri_value <- get_secret("uri")

                                    if (grepl("api/$", uri_value)) {
                                      # Format is correct, do nothing
                                    }
                                    # Check if it ends with "api" (missing trailing slash)
                                    else if (grepl("api$", uri_value)) {
                                      # Add trailing slash
                                      fixed_uri <- paste0(uri_value, "/")

                                      # Update the variable in the secrets environment
                                      assign_secret("uri", fixed_uri)

                                      # Update the secrets.R file
                                      if (file.exists(self$secrets_file)) {
                                        # Read and update file content
                                        file_content <- readLines(self$secrets_file)
                                        uri_pattern <- "^\\s*uri\\s*<-\\s*[\"\'](.*)[\"\']\\s*$"
                                        uri_line_index <- grep(uri_pattern, file_content)

                                        if (length(uri_line_index) > 0) {
                                          file_content[uri_line_index] <- gsub(uri_pattern,
                                                                               paste0("uri <- \"", fixed_uri, "\""),
                                                                               file_content[uri_line_index])
                                          writeLines(file_content, self$secrets_file)
                                          message("Note: Added trailing slash to uri in ", self$secrets_file,
                                                  " (", uri_value, " -> ", fixed_uri, ")")
                                        } else {
                                          message("Note: Added trailing slash to uri in memory, but couldn't update ",
                                                  self$secrets_file, " automatically.")
                                        }
                                      }
                                    }
                                    # Not ending with "api" at all
                                    else {
                                      # Don't add a slash, add an error instead
                                      all_errors <- c(all_errors, paste("URI format error:",
                                                                        "Please verify REDCap API endpoint in the API Playground"))
                                    }
                                  }

                                  # Check REDCap token length
                                  if (api_type == "redcap" && "token" %in% correct_type_vars) {
                                    token_value <- get_secret("token")
                                    if (nchar(token_value) < 32) {
                                      all_errors <- c(all_errors, paste("Token length error:",
                                                                        "token must be at least 32 characters long for REDCap API."))
                                    }
                                  }

                                  # SQL validation - ensure connection parameters are valid
                                  if (api_type == "sql") {
                                    # Can add specific SQL validation logic here if needed
                                    # For example, checking connection string format
                                  }
                                }
                              }

                              # If we found any errors, report them all at once
                              if (length(all_errors) > 0) {
                                stop(api_type, " API configuration errors in secrets.R:\n- ",
                                     paste(all_errors, collapse="\n- "), call. = FALSE)
                              } else {
                                # message("All ", api_type, " API credentials in secrets.R are valid.")
                              }

                              return(TRUE)
                            }
                          )
)

#' Create a wrapper function to make validation easier
#'
#' @param api_type API type to validate
#' @param secrets_file Path to secrets file
#' @return TRUE if validation passes
#' @export
validate_secrets <- function(api_type = NULL, secrets_file = "secrets.R") {
  secrets <- SecretsEnv$new(secrets_file)

  # If api_type is provided, validate just that API
  if (!is.null(api_type)) {
    # Check if the API is configured before validating
    configured_apis <- secrets$get_configured_apis()

    if (api_type %in% configured_apis) {
      return(secrets$validate_config(api_type))
    } else {
      # If this API isn't configured, return TRUE without validating
      message("API type '", api_type, "' is not configured in config.yml, skipping validation.")
      return(TRUE)
    }
  }

  # Otherwise, validation was already done during initialization
  return(TRUE)
}
