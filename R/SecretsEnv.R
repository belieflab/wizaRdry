# First, install R6 if you don't have it
if (!require(R6)) install.packages("R6"); library(R6)

#' Configuration Environment Class
#' 
#' @importFrom R6 R6Class
#' @noRd
SecretsEnv <- R6::R6Class("SecretsEnv",
                          public = list(
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
                                required = c("conn"),
                                types = c(conn = "character")
                              )
                            ),
                            
                            # Store the secrets file path
                            secrets_file = NULL,
                            
                            initialize = function(secrets_file = "secrets.R") {
                              # Check if secrets file exists
                              if (!file.exists(secrets_file)) {
                                stop(secrets_file, " file not found. Please create this file and define the required API variables.")
                              }
                              
                              # Store the secrets file path
                              self$secrets_file <- secrets_file
                              
                              # Source the secrets file
                              base::source(secrets_file)
                            },
                            
                            validate_config = function(api_type) {
                              if (!api_type %in% names(self$config_specs)) {
                                stop("Unknown API type: '", api_type, "'. Valid types are: ",
                                     base::paste(names(self$config_specs), collapse=", "))
                              }
                              
                              specs <- self$config_specs[[api_type]]
                              all_errors <- c()
                              
                              # Check that required variables exist
                              missing_vars <- specs$required[!base::sapply(specs$required, exists)]
                              if (length(missing_vars) > 0) {
                                all_errors <- c(all_errors, paste("Missing variables:",
                                                                  base::paste(missing_vars, collapse=", ")))
                              }
                              
                              # Only check existing variables for type and emptiness
                              existing_vars <- specs$required[base::sapply(specs$required, exists)]
                              
                              # Check variable types and emptiness for variables that exist
                              for (type_name in unique(specs$types)) {
                                # Get variables expected to be of this type that exist
                                vars_of_type_names <- names(specs$types[specs$types == type_name])
                                vars_of_type <- vars_of_type_names[vars_of_type_names %in% existing_vars]
                                
                                if (length(vars_of_type) > 0) {
                                  # Check which ones fail the type check
                                  failing_vars <- vars_of_type[base::sapply(vars_of_type, function(var) {
                                    var_value <- base::get(var)
                                    
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
                                      var_value <- base::get(var)
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
                                      var_value <- base::get(var)
                                      length(var_value) == 0
                                    })]
                                    
                                    if (length(empty_vars) > 0) {
                                      all_errors <- c(all_errors, paste("Empty vector error:",
                                                                        base::paste(empty_vars, collapse=", "),
                                                                        "cannot be empty vectors."))
                                    }
                                  }
                                  
                                  # For REDCap URI, check trailing slash and 'api' endpoint
                                  if (api_type == "redcap" && "uri" %in% correct_type_vars) {
                                    uri_value <- base::get("uri")
                                    
                                    # Check if it ends with "api/" (correct format)
                                    if (grepl("api/$", uri_value)) {
                                      # Format is correct, do nothing
                                    } 
                                    # Check if it ends with "api" (missing trailing slash)
                                    else if (grepl("api$", uri_value)) {
                                      # Add trailing slash
                                      fixed_uri <- paste0(uri_value, "/")
                                      
                                      # Update the variable in memory
                                      # assign("uri", fixed_uri, envir = .GlobalEnv)
                                      assign("uri", fixed_uri, envir = .wizaRdry_env)
                                      
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
                                                                        "uri must end with 'api/' for REDCap API access.",
                                                                        "Please verify REDCap API endpoint in the API Playground"))
                                    }
                                  }
                                  
                                  # Check REDCap token length
                                  if (api_type == "redcap" && "token" %in% correct_type_vars) {
                                    token_value <- base::get("token")
                                    if (nchar(token_value) < 32) {
                                      all_errors <- c(all_errors, paste("Token length error:",
                                                                        "token must be at least 32 characters long for REDCap API."))
                                    }
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

# Create a wrapper function to make validation easier
validate_secrets <- function(api_type, secrets_file = "secrets.R") {
  secrets <- SecretsEnv$new(secrets_file)
  return(secrets$validate_config(api_type))
}