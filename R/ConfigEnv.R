# Simplified and refactored ConfigEnv class
ConfigEnv <- R6::R6Class("ConfigEnv",
                         public = list(
                           config = NULL,
                           config_file = NULL,

                           # API validation specs
                           api_specs = list(
                             mongo = list(
                               required = "database",
                               validator = function(self) {
                                 if (self$has_value("mongo.database") && nchar(self$get_value("mongo.database")) == 0) {
                                   return("The 'database' setting cannot be empty")
                                 }
                                 character(0)
                               }
                             ),
                             qualtrics = list(
                               required = "survey_ids",
                               validator = function(self) {
                                 if (!self$has_value("qualtrics.survey_ids")) return(character(0))

                                 survey_ids <- self$get_value("qualtrics.survey_ids")
                                 if (!is.list(survey_ids)) {
                                   return("The 'survey_ids' setting must be a nested structure")
                                 }
                                 if (length(names(survey_ids)) == 0) {
                                   return("No institutions defined in 'survey_ids'")
                                 }
                                 character(0)
                               }
                             ),
                             redcap = list(
                               required = c("superkey", "primary_key"),
                               validator = function(self) {
                                 errors <- character(0)
                                 for (field in c("superkey", "primary_key")) {
                                   path <- paste0("redcap.", field)
                                   if (self$has_value(path) && nchar(self$get_value(path)) == 0) {
                                     errors <- c(errors, paste("The", sQuote(field), "setting cannot be empty"))
                                   }
                                 }
                                 errors
                               }
                             ),
                             sql = list(
                               required = c("superkey", "primary_key", "schemas", "pii_fields"),
                               validator = function(self) {
                                 errors <- character(0)

                                 # Check string fields
                                 for (field in c("primary_key", "superkey")) {
                                   path <- paste0("sql.", field)
                                   if (self$has_value(path) && nchar(self$get_value(path)) == 0) {
                                     errors <- c(errors, paste("The", sQuote(field), "setting cannot be empty"))
                                   }
                                 }

                                 # Check pii_fields
                                 if (self$has_value("sql.pii_fields")) {
                                   pii_fields <- self$get_value("sql.pii_fields")
                                   if (!is.vector(pii_fields) || !is.character(pii_fields)) {
                                     errors <- c(errors, "The 'pii_fields' setting must be a character vector")
                                   }
                                 }

                                 errors
                               }
                             )
                           ),

                           # Missing data specs
                           missing_data_specs = list(
                             types = c("skipped", "refused", "unknown", "missing"),
                             aliases = list(
                               missing = c("undefined", "na", "null"),
                               unknown = c("undefined", "na", "null"),
                               skipped = c("not_applicable", "na", "skip"),
                               refused = c("declined", "no_answer")
                             ),
                             allow_custom = TRUE
                           ),

                           initialize = function(config_file = "config.yml") {
                             if (!file.exists(config_file)) {
                               stop(paste(config_file, "not found. Please create this file with the required API configurations."))
                             }

                             self$config_file <- config_file
                             self$config <- config::get(file = config_file)
                             self$process_substitutions()
                           },

                           # Simplified path navigation
                           get_value = function(path) {
                             parts <- strsplit(path, "\\.")[[1]]
                             Reduce(function(obj, key) {
                               if (is.null(obj) || !key %in% names(obj)) NULL else obj[[key]]
                             }, parts, init = self$config)
                           },

                           has_value = function(path) {
                             !is.null(self$get_value(path))
                           },

                           get_configured_apis = function() {
                             Filter(self$has_value, names(self$api_specs))
                           },

                           # Streamlined substitutions
                           process_substitutions = function() {
                             if (self$get_value("mongo.database") == "${study_alias}") {
                               study_alias <- self$get_value("study_alias")
                               if (!is.null(study_alias)) {
                                 self$config$mongo$database <- study_alias
                               } else {
                                 warning("Cannot substitute ${study_alias}: study_alias not defined")
                               }
                             }
                           },

                           # Generic validation with custom validators
                           validate_config = function(api_type = NULL) {
                             if (is.null(api_type)) return(self$validate_core_config())
                             if (api_type == "missing_data_codes") return(self$validate_missing_data_codes())

                             # Check if API type exists
                             if (!api_type %in% names(self$api_specs)) {
                               valid_options <- paste(names(self$api_specs), collapse = ", ")
                               stop(paste("Unknown API type:", sQuote(api_type), ". Valid options are:", valid_options))
                             }

                             if (!self$has_value(api_type)) return(TRUE)

                             spec <- self$api_specs[[api_type]]
                             errors <- character(0)

                             # Check required fields
                             errors <- c(errors, self$check_required_fields(api_type, spec$required))

                             # Run custom validator if it exists
                             if (!is.null(spec$validator)) {
                               errors <- c(errors, spec$validator(self))
                             }

                             if (length(errors) > 0) {
                               self$throw_error(api_type, errors)
                             }

                             TRUE
                           },

                           validate_core_config = function() {
                             required <- c("study_alias", "identifier")
                             missing <- Filter(function(field) !self$has_value(field), required)

                             if (length(missing) > 0) {
                               errors <- paste("Missing required", sQuote(missing), "setting in the root configuration")
                               self$throw_error("Core", errors)
                             }

                             TRUE
                           },

                           validate_missing_data_codes = function() {
                             config <- self$get_value("missing_data_codes")

                             if (is.null(config)) {
                               message("Note: No missing value categories defined. Default R NA values will be used.")
                               return(TRUE)
                             }

                             errors <- character(0)
                             categories <- names(config)

                             # Validate categories
                             if (!self$missing_data_specs$allow_custom) {
                               invalid <- Filter(function(cat) is.null(self$normalize_missing_category(cat)), categories)
                               if (length(invalid) > 0) {
                                 allowed <- paste(c(self$missing_data_specs$types, unlist(self$missing_data_specs$aliases)), collapse = ", ")
                                 errors <- c(errors, paste("Invalid categories:", paste(invalid, collapse = ", "), ". Allowed:", allowed))
                               }
                             }

                             # Validate category values
                             for (cat in categories) {
                               values <- self$get_value(paste0("missing_data_codes.", cat))
                               if (!is.vector(values) && !is.list(values)) {
                                 errors <- c(errors, paste("Category", sQuote(cat), "must be a vector or list"))
                               }
                               if (length(values) == 0) {
                                 errors <- c(errors, paste("Category", sQuote(cat), "cannot be empty"))
                               }
                             }

                             if (length(errors) > 0) {
                               self$throw_error("missing_data_codes", errors)
                             }

                             TRUE
                           },

                           normalize_missing_category = function(category) {
                             specs <- self$missing_data_specs

                             if (category %in% specs$types) return(category)

                             # Check aliases
                             for (type in names(specs$aliases)) {
                               if (category %in% specs$aliases[[type]]) return(type)
                             }

                             if (specs$allow_custom) category else NULL
                           },

                           get_missing_data_codes = function(category = NULL) {
                             if (is.null(category)) return(self$get_value("missing_data_codes"))

                             # Try direct path
                             direct <- self$get_value(paste0("missing_data_codes.", category))
                             if (!is.null(direct)) return(direct)

                             # Try normalized
                             normalized <- self$normalize_missing_category(category)
                             if (!is.null(normalized) && normalized != category) {
                               return(self$get_value(paste0("missing_data_codes.", normalized)))
                             }

                             NULL
                           },

                           # Helper methods
                           check_required_fields = function(api_type, required_fields) {
                             missing <- Filter(function(field) {
                               !self$has_value(paste0(api_type, ".", field))
                             }, required_fields)

                             if (length(missing) > 0) {
                               paste("Missing", sQuote(missing), "setting in the", api_type, "section")
                             } else {
                               character(0)
                             }
                           },

                           throw_error = function(context, errors) {
                             error_text <- paste("-", errors, collapse = "\n")
                             stop(paste(context, "configuration errors in", self$config_file, ":\n", error_text), call. = FALSE)
                           }
                         )
)

# Simplified helper functions
validate_config <- function(api_type = NULL, config_file = "config.yml") {
  config_env <- ConfigEnv$new(config_file)
  config_env$validate_config(api_type)
  config_env$config
}

get_missing_data_codes <- function(category = NULL, config_file = "config.yml") {
  ConfigEnv$new(config_file)$get_missing_data_codes(category)
}
