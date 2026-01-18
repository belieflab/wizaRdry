#' ValidationState R6 Class
#'
#' @description
#' Manages NDA validation state and tracks modifications to data structures.
#' Central object for tracking validation results, modifications, and determining
#' whether data definition files need to be created.
#'
#' @details
#' This class replaces the fragile attribute-passing pattern used previously.
#' It provides a structured way to track:
#' - Validation status (valid/invalid, new/existing structure)
#' - Value range violations
#' - New fields added
#' - Required field status
#' - Metadata from ndar_subject01
#'
#' The key method `needs_data_definition()` determines whether a data definition
#' file should be created based on whether the structure is new or modified.
#'
#' @keywords internal
ValidationState <- R6::R6Class("ValidationState",
  public = list(
    #' @field measure_name Character - name of the measure/structure
    measure_name = NULL,
    
    #' @field api Character - API type (redcap, qualtrics, mongo, etc.)
    api = NULL,
    
    #' @field data_env DataEnvironment - manages dataframe across environments
    data_env = NULL,
    
    #' @field nda_structure List - NDA structure definition from API
    nda_structure = NULL,
    
    #' @field is_valid Logical - whether validation passed
    is_valid = TRUE,
    
    #' @field is_new_structure Logical - whether structure is new (not in NDA)
    is_new_structure = FALSE,
    
    #' @field is_modified_structure Logical - whether existing structure has modifications
    is_modified_structure = FALSE,
    
    #' @field bypassed_validation Logical - whether validation was bypassed (new structures)
    bypassed_validation = FALSE,
    
    #' @field value_range_violations List - fields with value range violations
    value_range_violations = list(),
    
    #' @field new_fields Character vector - fields in data not in NDA structure
    new_fields = character(),
    
    #' @field renamed_fields Character vector - fields that were renamed
    renamed_fields = character(),
    
    #' @field dropped_fields Character vector - fields that were dropped
    dropped_fields = character(),
    
    #' @field missing_required Character vector - required fields with missing data
    missing_required = character(),
    
    #' @field required_metadata Data frame - ndar_subject01 required field metadata
    required_metadata = NULL,
    
    #' @field recommended_metadata Data frame - ndar_subject01 recommended field metadata
    recommended_metadata = NULL,
    
    #' @field warnings Character vector - warning messages
    warnings = character(),
    
    #' @field errors Character vector - error messages
    errors = character(),
    
    #' @description
    #' Create a new ValidationState instance
    #' @param measure_name Name of the measure/structure
    #' @param api API type (redcap, qualtrics, mongo, csv, oracle, sql)
    #' @param df Initial dataframe
    #' @param nda_structure NDA structure definition (NULL for new structures)
    #' @return A new ValidationState object
    initialize = function(measure_name, api, df, nda_structure = NULL) {
      if (missing(measure_name) || is.null(measure_name) || measure_name == "") {
        stop("measure_name is required and cannot be empty")
      }
      if (missing(api) || is.null(api) || api == "") {
        stop("api is required and cannot be empty")
      }
      if (missing(df) || is.null(df) || !is.data.frame(df)) {
        stop("df must be a valid data.frame")
      }
      
      self$measure_name <- measure_name
      self$api <- api
      self$data_env <- DataEnvironment$new(measure_name, df)
      self$nda_structure <- nda_structure
      
      # Determine if this is a new structure
      self$is_new_structure <- is.null(nda_structure) || 
                               is.null(nda_structure$dataElements) ||
                               nrow(nda_structure$dataElements) == 0
    },
    
    #' @description
    #' Get current dataframe
    #' @return Data frame
    get_df = function() {
      self$data_env$get_df()
    },
    
    #' @description
    #' Update dataframe in all environments
    #' @param df New dataframe
    #' @return Self (invisibly) for method chaining
    set_df = function(df) {
      self$data_env$set_df(df)
      invisible(self)
    },
    
    #' @description
    #' Add a value range violation
    #' @param field Field name
    #' @param expected Expected value range (NULL if no range defined)
    #' @param actual Vector of violating values
    #' @return Self (invisibly) for method chaining
    add_value_range_violation = function(field, expected, actual) {
      self$value_range_violations[[field]] <- list(
        expected = expected,
        actual = actual,
        count = length(actual)
      )
      self$is_modified_structure <- TRUE
      
      # Log the violation for debugging
      message(sprintf("[VIOLATION] Field '%s': expected=%s, found %d violating value(s)",
                     field, 
                     if(is.null(expected)) "NULL (no range defined)" else expected,
                     length(actual)))
      
      invisible(self)
    },
    
    #' @description
    #' Check if structure has modifications requiring data definition
    #' @return Logical
    has_modifications = function() {
      length(self$new_fields) > 0 || 
      length(self$value_range_violations) > 0
    },
    
    #' @description
    #' Determine if data definition file is needed
    #' @return Logical - TRUE if data definition should be created
    needs_data_definition = function() {
      self$is_new_structure || self$has_modifications()
    },
    
    #' @description
    #' Get human-readable modification reason
    #' @return Character string describing why structure is modified
    get_modification_reason = function() {
      if (self$is_new_structure) {
        return("new structure")
      }
      
      reasons <- character()
      
      if (length(self$new_fields) > 0) {
        reasons <- c(reasons, sprintf("%d new field%s", 
                                     length(self$new_fields),
                                     if(length(self$new_fields) > 1) "s" else ""))
      }
      
      if (length(self$value_range_violations) > 0) {
        reasons <- c(reasons, sprintf("%d value range difference%s", 
                                     length(self$value_range_violations),
                                     if(length(self$value_range_violations) > 1) "s" else ""))
      }
      
      if (length(reasons) == 0) {
        return("unmodified")
      }
      
      paste(reasons, collapse = " and ")
    },
    
    #' @description
    #' Convert to list for backward compatibility with old validation_results
    #' @return List with validation results
    to_list = function() {
      list(
        valid = self$is_valid,
        df = self$get_df(),
        message = self$get_modification_reason(),
        bypassed_validation = self$bypassed_validation,
        value_range_violations = self$value_range_violations,
        missing_required = self$missing_required,
        warnings = self$warnings,
        errors = self$errors
      )
    },
    
    #' @description
    #' Print method for ValidationState
    #' @return Self (invisibly)
    print = function() {
      cat("ValidationState:\n")
      cat(sprintf("  Measure: %s\n", self$measure_name))
      cat(sprintf("  API: %s\n", self$api))
      cat(sprintf("  Status: %s\n", if(self$is_valid) "VALID" else "INVALID"))
      cat(sprintf("  Structure Type: %s\n", 
                  if(self$is_new_structure) "NEW" else "EXISTING"))
      
      if (!self$is_new_structure) {
        cat(sprintf("  Modified: %s\n", 
                    if(self$is_modified_structure) "YES" else "NO"))
      }
      
      if (length(self$value_range_violations) > 0) {
        cat(sprintf("  Value Range Violations: %d field(s)\n", 
                    length(self$value_range_violations)))
      }
      
      if (length(self$new_fields) > 0) {
        cat(sprintf("  New Fields: %d\n", length(self$new_fields)))
      }
      
      if (length(self$missing_required) > 0) {
        cat(sprintf("  Missing Required: %d field(s)\n", 
                    length(self$missing_required)))
      }
      
      cat(sprintf("  Needs Data Definition: %s\n", 
                  if(self$needs_data_definition()) "YES" else "NO"))
      
      cat(sprintf("  Modification Reason: %s\n", self$get_modification_reason()))
      
      invisible(self)
    }
  )
)
