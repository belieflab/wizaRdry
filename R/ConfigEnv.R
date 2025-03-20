# First, install R6 if you don't have it
if (!require(R6)) install.packages("R6"); library(R6)
# Install config package if needed
if (!require(config)) install.packages("config"); library(config)

#' Configuration Environment Class
#' 
#' @importFrom R6 R6Class
#' @noRd
ConfigEnv <- R6::R6Class("ConfigEnv",
                         public = list(
                           # Store configuration
                           config = NULL,
                           config_file = NULL,
                           
                           # Define validation specs for each API
                           api_specs = list(
                             mongo = list(
                               required = c("collection")
                             ),
                             qualtrics = list(
                               required = c("survey_ids")
                             ),
                             redcap = list(
                               required = c("super_keys")
                             ),
                             sql = list(
                               required = c()  # Add required fields for SQL as needed
                             )
                           ),
                           
                           initialize = function(config_file = "config.yml") {
                             # Check if config file exists
                             if (!file.exists(config_file)) {
                               stop(config_file, " not found. Please create this file with the required API configurations.")
                             }
                             
                             # Store the config file path
                             self$config_file <- config_file
                             
                             # Load configuration
                             self$config <- config::get(file = config_file)
                             
                             # Process variable substitutions
                             self$process_substitutions()
                           },
                           
                           # Method to handle variable substitutions like ${study_alias}
                           process_substitutions = function() {
                             # Process mongo collection name if it references study_alias
                             if (!is.null(self$config$mongo) &&
                                 !is.null(self$config$mongo$collection) &&
                                 self$config$mongo$collection == "${study_alias}") {
                               
                               if (!is.null(self$config$study_alias)) {
                                 self$config$mongo$collection <- self$config$study_alias
                               } else {
                                 warning("Cannot substitute ${study_alias} in mongo.collection: study_alias is not defined in config")
                               }
                             }
                             
                             # Add more substitution rules as needed
                           },
                           
                           # Get a specific configuration value
                           get_value = function(path) {
                             # Split the path by dots
                             parts <- strsplit(path, "\\.")[[1]]
                             
                             # Start with the root config
                             result <- self$config
                             
                             # Navigate through the path
                             for (part in parts) {
                               if (is.null(result) || !part %in% names(result)) {
                                 return(NULL)
                               }
                               result <- result[[part]]
                             }
                             
                             return(result)
                           },
                           
                           # Check if a configuration value exists
                           has_value = function(path) {
                             !is.null(self$get_value(path))
                           },
                           
                           # Validate specific API configuration
                           validate_config = function(api_type = NULL) {
                             # If no API type specified, validate core config
                             if (is.null(api_type)) {
                               return(self$validate_core_config())
                             }
                             
                             # Check if the API type is supported
                             if (!api_type %in% names(self$api_specs)) {
                               stop("Unknown API type: '", api_type, "'. Valid options are: ",
                                    paste(names(self$api_specs), collapse=", "))
                             }
                             
                             all_errors <- c()
                             
                             # Check if API section exists
                             if (!self$has_value(api_type)) {
                               stop("The '", api_type, "' section is missing in ", self$config_file,
                                    ". Please add a ", api_type, " section with the necessary configuration.")
                             }
                             
                             # Get API specs
                             specs <- self$api_specs[[api_type]]
                             
                             # Check required fields
                             for (field in specs$required) {
                               field_path <- paste0(api_type, ".", field)
                               if (!self$has_value(field_path)) {
                                 all_errors <- c(all_errors, paste("Missing '", field, "' setting in the ", api_type, " section"))
                               }
                             }
                             
                             # API-specific additional validations
                             if (api_type == "mongo") {
                               # Check if collection is empty after substitution
                               if (self$has_value("mongo.collection") && nchar(self$get_value("mongo.collection")) == 0) {
                                 all_errors <- c(all_errors, "The 'collection' setting cannot be empty")
                               }
                             } else if (api_type == "qualtrics") {
                               # Check if survey_ids exists and is a list
                               if (self$has_value("qualtrics.survey_ids")) {
                                 survey_ids <- self$get_value("qualtrics.survey_ids")
                                 if (!is.list(survey_ids)) {
                                   all_errors <- c(all_errors, "The 'survey_ids' setting must be a nested structure")
                                 } else {
                                   # Check if there are any institutions defined
                                   if (length(names(survey_ids)) == 0) {
                                     all_errors <- c(all_errors, "No institutions defined in 'survey_ids'")
                                   }
                                 }
                               }
                             } else if (api_type == "redcap") {
                               # Any redcap-specific validations
                             } else if (api_type == "sql") {
                               # Any sql-specific validations
                             }
                             
                             # If we found any errors, report them all at once
                             if (length(all_errors) > 0) {
                               stop(api_type, " configuration errors in ", self$config_file, ":\n- ",
                                    paste(all_errors, collapse="\n- "), call. = FALSE)
                             } else {
                               message("The ", api_type, " configuration in ", self$config_file, " is valid.")
                             }
                             
                             return(TRUE)
                           },
                           
                           # Validate core configuration
                           validate_core_config = function() {
                             all_errors <- c()
                             
                             # Check required global fields
                             required_fields <- c("study_alias", "identifier")
                             
                             for (field in required_fields) {
                               if (!self$has_value(field)) {
                                 all_errors <- c(all_errors, paste("Missing required '", field, "' setting in the root configuration"))
                               }
                             }
                             
                             # If we found any errors, report them all at once
                             if (length(all_errors) > 0) {
                               stop("Core configuration errors in ", self$config_file, ":\n- ",
                                    paste(all_errors, collapse="\n- "), call. = FALSE)
                             } else {
                               # message("The core configuration in ", self$config_file, " is valid.")
                             }
                             
                             return(TRUE)
                           }
                         )
)

# Create a function to validate configuration and return the config
validate_config <- function(api_type = NULL, config_file = "config.yml") {
  config_env <- ConfigEnv$new(config_file)
  
  # Validate the configuration
  validation_result <- config_env$validate_config(api_type)
  
  # If validation passes, return the config
  if (validation_result) {
    return(config_env$config)
  } else {
    return(NULL)  # Or handle failure appropriately
  }
}