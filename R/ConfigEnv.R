# Modified ConfigEnv class with more flexible missing value categories and SQL support
ConfigEnv <- R6::R6Class("ConfigEnv",
                         public = list(
                           # Store configuration
                           config = NULL,
                           config_file = NULL,

                           # Define validation specs for each API
                           api_specs = list(
                             mongo = list(
                               required = c("database")
                             ),
                             qualtrics = list(
                               required = c("survey_ids")
                             ),
                             redcap = list(
                               required = c("superkey", "primary_key")
                             ),
                             sql = list(
                               required = c("superkey", "primary_key", "schemas", "pii_fields")
                             ),
                             missing_data_codes = list(
                               required = c(),  # No required fields - all are optional
                               types = c("skipped", "refused", "unknown", "missing"),  # Add "missing" as valid category type
                               aliases = list(                                         # Add support for aliases
                                 "missing" = c("undefined", "na", "null"),
                                 "unknown" = c("undefined", "na", "null"),
                                 "skipped" = c("not_applicable", "na", "skip"),
                                 "refused" = c("declined", "no_answer")
                               ),
                               allow_custom = TRUE  # Flag to allow custom categories beyond the predefined ones
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
                             # Process mongo database name if it references study_alias
                             if (!is.null(self$config$mongo) &&
                                 !is.null(self$config$mongo$database) &&
                                 self$config$mongo$database == "${study_alias}") {
                               if (!is.null(self$config$study_alias)) {
                                 self$config$mongo$database <- self$config$study_alias
                               } else {
                                 warning("Cannot substitute ${study_alias} in mongo.database: study_alias is not defined in config")
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

                           # Check which APIs are configured
                           get_configured_apis = function() {
                             configured_apis <- character(0)
                             for (api_type in names(self$api_specs)) {
                               if (self$has_value(api_type)) {
                                 configured_apis <- c(configured_apis, api_type)
                               }
                             }
                             return(configured_apis)
                           },

                           # Normalize a missing value category name (handle aliases)
                           normalize_missing_category = function(category) {
                             # If the category is already a valid type, return it
                             if (category %in% self$api_specs$missing_data_codes$types) {
                               return(category)
                             }

                             # Check if it's an alias for a known category
                             for (valid_type in names(self$api_specs$missing_data_codes$aliases)) {
                               aliases <- self$api_specs$missing_data_codes$aliases[[valid_type]]
                               if (category %in% aliases) {
                                 return(valid_type)
                               }
                             }

                             # If allow_custom is TRUE, return the original category
                             if (self$api_specs$missing_data_codes$allow_custom) {
                               return(category)
                             }

                             # Otherwise return NULL (invalid category)
                             return(NULL)
                           },

                           # Validate specific API configuration - STRICT VERSION
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
                             # STRICT: Check if this API is actually configured - now REQUIRED
                             if (!self$has_value(api_type)) {
                               stop(api_type, " configuration errors in ", self$config_file, ":\n- ",
                                    "Missing '", api_type, "' section in configuration", call. = FALSE)
                             }

                             all_errors <- c()
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
                               # Check if database is empty after substitution
                               if (self$has_value("mongo.database") && nchar(self$get_value("mongo.database")) == 0) {
                                 all_errors <- c(all_errors, "The 'database' setting cannot be empty")
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
                               if (self$has_value("redcap.superkey") && nchar(self$get_value("redcap.superkey")) == 0) {
                                 all_errors <- c(all_errors, "The 'superkey' setting cannot be empty")
                               }
                               if (self$has_value("redcap.primary_key") && nchar(self$get_value("redcap.primary_key")) == 0) {
                                 all_errors <- c(all_errors, "The 'primary_key' setting cannot be empty")
                               }
                             } else if (api_type == "sql") {
                               # SQL-specific validations
                               # Check for primary_key setting if specified
                               if (self$has_value("sql.primary_key") && nchar(self$get_value("sql.primary_key")) == 0) {
                                 all_errors <- c(all_errors, "The 'primary_key' setting cannot be empty")
                               }

                               # Check for superkey table if specified
                               if (self$has_value("sql.superkey") && nchar(self$get_value("sql.superkey")) == 0) {
                                 all_errors <- c(all_errors, "The 'superkey' setting cannot be empty")
                               }

                               # Check for pii_fields if specified
                               if (self$has_value("sql.pii_fields")) {
                                 pii_fields <- self$get_value("sql.pii_fields")
                                 if (!is.vector(pii_fields) || !is.character(pii_fields)) {
                                   all_errors <- c(all_errors, "The 'pii_fields' setting must be a character vector")
                                 }
                               }
                             } else if (api_type == "missing_data_codes") {
                               # Get the current missing_data_codes configuration
                               missing_value_config <- self$get_value("missing_data_codes")

                               # If any categories are defined, validate them
                               if (!is.null(missing_value_config)) {
                                 config_categories <- names(missing_value_config)

                                 # More lenient validation when allow_custom is TRUE
                                 if (!specs$allow_custom) {
                                   # Check that all provided categories are valid or aliases of valid categories
                                   valid_categories <- c()
                                   unexpected_categories <- c()

                                   for (category in config_categories) {
                                     normalized_category <- self$normalize_missing_category(category)
                                     if (!is.null(normalized_category)) {
                                       valid_categories <- c(valid_categories, category)
                                     } else {
                                       unexpected_categories <- c(unexpected_categories, category)
                                     }
                                   }

                                   if (length(unexpected_categories) > 0) {
                                     known_aliases <- unlist(specs$aliases)
                                     all_errors <- c(all_errors, paste0("Unexpected categories in missing_data_codes: ",
                                                                        paste(unexpected_categories, collapse=", "),
                                                                        ". Allowed categories are: ",
                                                                        paste(c(specs$types, known_aliases), collapse=", ")))
                                   }
                                 }

                                 # For each provided category, check its values
                                 for (category in config_categories) {
                                   category_values <- self$get_value(paste0("missing_data_codes.", category))
                                   # Check if the category is a list or vector
                                   if (!is.vector(category_values) && !is.list(category_values)) {
                                     all_errors <- c(all_errors, paste0("The '", category, "' category must be a list or vector of values"))
                                   }
                                   # Check if the list has at least one value
                                   if (length(category_values) == 0) {
                                     all_errors <- c(all_errors, paste0("The '", category, "' category must contain at least one value"))
                                   }
                                 }

                                 # Missing categories are ok - we'll use defaults
                                 missing_categories <- setdiff(specs$types, config_categories)
                                 if (length(missing_categories) > 0) {
                                   message("Note: The following missing value categories are not defined and will use R's default NA: ",
                                           paste(missing_categories, collapse=", "))
                                 }
                               } else {
                                 # If missing_data_codes section is completely empty, just inform the user
                                 message("Note: No missing value categories defined. Default R NA values will be used for all missing value types.")
                               }
                             }

                             # If we found any errors, report them all at once
                             if (length(all_errors) > 0) {
                               stop(api_type, " configuration errors in ", self$config_file, ":\n- ",
                                    paste(all_errors, collapse="\n- "), call. = FALSE)
                             }
                             # else {
                             #   message("The ", api_type, " configuration in ", self$config_file, " is valid.")
                             # }
                             return(TRUE)
                           }

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
                           },

                           # Get missing value codes for a specific category
                           get_missing_data_codes = function(category = NULL) {
                             # If no category specified, return all missing values
                             if (is.null(category)) {
                               return(self$get_value("missing_data_codes"))
                             }

                             # If we have an exact match for the category, use it
                             if (self$has_value(paste0("missing_data_codes.", category))) {
                               values <- self$get_value(paste0("missing_data_codes.", category))
                               if (!is.null(values)) {
                                 return(values)
                               }
                             }

                             # Try to normalize the category name (handle aliases)
                             normalized_category <- self$normalize_missing_category(category)
                             if (!is.null(normalized_category) && normalized_category != category) {
                               # Check if the normalized category exists in config
                               if (self$has_value(paste0("missing_data_codes.", normalized_category))) {
                                 values <- self$get_value(paste0("missing_data_codes.", normalized_category))
                                 if (!is.null(values)) {
                                   return(values)
                                 }
                               }
                             }

                             # If no values are defined for any variant, return NULL to indicate default NA should be used
                             return(NULL)
                           }
                         )
)

# Create a function to validate configuration and return the config
validate_config <- function(api_type = NULL, config_file = "config.yml") {
  config_env <- ConfigEnv$new(config_file)
  # If no specific API type is provided, just validate core config
  if (is.null(api_type)) {
    validation_result <- config_env$validate_core_config()
  } else {
    # If a specific API type is requested, validate just that one
    validation_result <- config_env$validate_config(api_type)
  }
  # If validation passes, return the config
  if (validation_result) {
    return(config_env$config)
  } else {
    return(NULL)  # Or handle failure appropriately
  }
}

# Helper function to get missing value codes
get_missing_data_codes <- function(category = NULL, config_file = "config.yml") {
  config_env <- ConfigEnv$new(config_file)
  return(config_env$get_missing_data_codes(category))
}
