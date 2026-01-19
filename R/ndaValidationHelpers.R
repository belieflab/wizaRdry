#' NDA Validation Helper Functions
#'
#' @description
#' Core validation logic for NDA data structure compliance.
#' Includes value range checking, GUID validation, and violation tracking.
#'

#' Determine if validation messages should be shown
#'
#' @description
#' Helper function to centralize message suppression logic.
#' Messages are shown in strict mode OR when verbose is enabled.
#' In lenient non-verbose mode, individual messages are suppressed
#' to avoid duplication with summary warnings.
#'
#' @param strict Logical - if TRUE, enforce strict validation
#' @param verbose Logical - if TRUE, show detailed messages
#' @return Logical - TRUE if messages should be shown
#' @noRd
should_show_validation_message <- function(strict, verbose) {
  strict || verbose
}

#' Check for value range violations and update ValidationState
#'
#' @description
#' THE KEY FUNCTION for fixing value range violation tracking.
#' Scans all fields for violations and updates the ValidationState object.
#' This replaces the scattered violation checking in the old validate_structure().
#'
#' @param state ValidationState object to update
#' @param elements NDA structure dataElements
#' @param verbose Logical - print detailed output
#' @return List of violations detected (for logging/debugging)
#' @noRd
check_value_range_violations <- function(state, elements, verbose = FALSE) {
  if (verbose) cat("\n\nChecking value ranges...")
  
  df <- state$get_df()
  violations_detected <- list()
  df_cols <- names(df)
  
  for (col in intersect(df_cols, elements$name)) {
    element <- elements[elements$name == col, ]
    
    if (nrow(element) == 0) next
    
    value_range <- element$valueRange
    data_type <- element$type
    
    # Case 1: Field has no valueRange - check if this is an unbounded type
    if (is.null(value_range) || is.na(value_range) || value_range == "") {
      # String, GUID, Date, Integer, Float without valueRange are VALID (unbounded)
      # These types don't require a defined range to be compliant
      unbounded_types <- c("String", "GUID", "Date", "Integer", "Float")
      
      if (data_type %in% unbounded_types) {
        if (verbose) {
          cat(sprintf("\n\nField: %s", col))
          cat(sprintf("\n  Type: %s (no valueRange defined)", data_type))
          cat(sprintf("\n  Status: VALID - Unbounded %s field", data_type))
        }
        next  # NOT a violation - skip to next field
      }
      
      # For unknown/other types without valueRange, flag for investigation
      # This is a rare edge case
      if (has_data_values(df[[col]])) {
        unique_vals <- get_unique_values(df[[col]])
        
        state$add_value_range_violation(
          field = col,
          expected = NULL,
          actual = unique_vals
        )
        
        violations_detected[[col]] <- list(
          type = "unknown_type_no_range",
          field = col,
          data_type = data_type,
          expected = NULL,
          actual = unique_vals,
          count = length(unique_vals)
        )
        
        if (verbose) {
          cat(sprintf("\n\nField: %s (no valueRange defined)", col))
          cat(sprintf("\n  Type: %s (unknown/unhandled type)", data_type))
          cat(sprintf("\n  Data contains %d unique values", length(unique_vals)))
          cat(sprintf("\n  Sample values: %s", 
                     paste(head(unique_vals, 5), collapse = ", ")))
          if (length(unique_vals) > 5) {
            cat(sprintf(" (and %d more)", length(unique_vals) - 5))
          }
        }
      }
      next
    }
    
    # Case 2: Field has valueRange - check for violations
    if (verbose) {
      cat(sprintf("\n\nField: %s", col))
      cat(sprintf("\n  Type: %s", element$type))
      cat(sprintf("\n  Expected range: %s", value_range))
    }
    
    violating_values <- get_violations(df[[col]], value_range)
    
    if (length(violating_values) > 0) {
      state$add_value_range_violation(
        field = col,
        expected = value_range,
        actual = violating_values
      )
      
      state$is_valid <- FALSE
      
      violations_detected[[col]] <- list(
        type = "range_violation",
        field = col,
        expected_range = value_range,
        violating_values = violating_values,
        count = length(violating_values)
      )
      
      if (verbose) {
        cat("\n  ERROR: Value range violations found:")
        cat(sprintf("\n    Invalid values: %s",
                   paste(head(violating_values, 5), collapse = ", ")))
        if (length(violating_values) > 5) {
          cat(sprintf(" (and %d more...)", length(violating_values) - 5))
        }
      }
    } else if (verbose) {
      cat("\n  All values within expected range")
    }
  }
  
  if (verbose && length(violations_detected) > 0) {
    cat(sprintf("\n\nSummary: %d field(s) with value range issues", 
               length(violations_detected)))
  }
  
  return(violations_detected)
}

#' Check if field has actual data values (not just NAs)
#' @param field_values Vector of field values
#' @return Logical
#' @noRd
has_data_values <- function(field_values) {
  !all(is.na(field_values)) && length(unique(field_values[!is.na(field_values)])) > 0
}

#' Get unique non-NA values as sorted character vector
#' @param field_values Vector of field values
#' @return Character vector of unique values
#' @noRd
get_unique_values <- function(field_values) {
  unique_vals <- unique(field_values[!is.na(field_values)])
  as.character(sort(unique_vals))
}

#' Detect fields in dataframe not in NDA structure
#' @param df Data frame
#' @param elements NDA structure dataElements
#' @return Character vector of new field names
#' @noRd
detect_new_fields <- function(df, elements) {
  df_cols <- names(df)
  structure_cols <- elements$name
  
  # Find new fields
  new_fields <- setdiff(df_cols, structure_cols)
  
  # Exclude special API-specific fields
  exclude_patterns <- c("_complete$", "^record_id$", "^redcap_event_name$")
  for (pattern in exclude_patterns) {
    new_fields <- new_fields[!grepl(pattern, new_fields)]
  }
  
  # Exclude super-required fields (added by addNdarSubjectElements)
  super_required <- SUPER_REQUIRED_FIELDS
  new_fields <- setdiff(new_fields, super_required)
  
  return(new_fields)
}

#' Get violations for a specific field value against its range
#'
#' @description
#' Core function that checks if values violate the specified range.
#' Handles numeric ranges (::), categorical values (;), and mixed ranges.
#' Extracted from original ndaValidator.R lines 2040-2127.
#'
#' @param value Vector of values to check
#' @param range_str Value range string from NDA structure (e.g., "1::10" or "A;B;C")
#' @return Character vector of violating values
#' @noRd
get_violations <- function(value, range_str) {
  if (is.null(range_str) || is.na(range_str) || range_str == "") return(character(0))
  
  # First check if there are non-numeric values (when expected numeric)
  if (grepl("::", range_str)) {
    # Numeric range expected - check for non-numeric values
    if (!is.numeric(value)) {
      # Try to convert to numeric to find which values can't be converted
      value_numeric <- suppressWarnings(as.numeric(as.character(value)))
      non_numeric_mask <- !is.na(value) & is.na(value_numeric)
      
      if (any(non_numeric_mask)) {
        non_numeric_values <- unique(value[non_numeric_mask])
        if (length(non_numeric_values) > 0) {
          return(sort(as.character(non_numeric_values)))
        }
      }
    }
  }
  
  # Special case for mixed ranges like "1::26;77"
  if (grepl("::", range_str) && grepl(";", range_str)) {
    # Split by semicolon first
    parts <- strsplit(range_str, ";")[[1]]
    # Get the numeric range from the first part
    range_part <- parts[grepl("::", parts)][1]
    range <- as.numeric(strsplit(range_part, "::")[[1]])
    # Get individual values from other parts
    individual_values <- as.numeric(parts[!grepl("::", parts)])
    # Combine valid values: numbers in range plus individual values
    valid_values <- c(seq(from = range[1], to = range[2]), individual_values)
    
    # Check for violations - handle non-numeric values appropriately
    if (is.numeric(value)) {
      invalid_mask <- !value %in% valid_values
      invalid_mask[is.na(invalid_mask)] <- FALSE
      return(sort(unique(value[invalid_mask])))
    } else {
      # For non-numeric values, we need to convert to numeric first
      value_numeric <- suppressWarnings(as.numeric(as.character(value)))
      # Identify values that can be converted but are outside range
      convertible_mask <- !is.na(value_numeric)
      invalid_mask <- convertible_mask & !value_numeric %in% valid_values
      invalid_mask[is.na(invalid_mask)] <- FALSE
      
      result <- sort(unique(value[invalid_mask]))
      # If no numeric violations, return empty
      if (length(result) == 0) return(character(0))
      return(result)
    }
  }
  
  # Rest of the function for simple ranges
  if (grepl("::", range_str)) {
    # Numeric range check
    range <- as.numeric(strsplit(range_str, "::")[[1]])
    
    # Handle non-numeric values appropriately
    if (is.numeric(value)) {
      invalid_mask <- value < range[1] | value > range[2]
      invalid_mask[is.na(invalid_mask)] <- FALSE
      return(sort(unique(value[invalid_mask])))
    } else {
      # For non-numeric values, we need to convert to numeric first
      value_numeric <- suppressWarnings(as.numeric(as.character(value)))
      # Identify values that can be converted but are outside range
      convertible_mask <- !is.na(value_numeric)
      invalid_mask <- convertible_mask & (value_numeric < range[1] | value_numeric > range[2])
      invalid_mask[is.na(invalid_mask)] <- FALSE
      
      result <- sort(unique(value[invalid_mask]))
      # If no numeric violations, return empty
      if (length(result) == 0) return(character(0))
      return(result)
    }
  } else if (grepl(";", range_str)) {
    # Categorical values
    valid_values <- trimws(strsplit(range_str, ";")[[1]])
    
    # Convert to character for comparison
    value_char <- as.character(value)
    invalid_mask <- !value_char %in% valid_values
    invalid_mask[is.na(invalid_mask)] <- FALSE
    return(sort(unique(value[invalid_mask])))
  }
  
  return(character(0))
}

#' Check required and recommended fields for missing data
#'
#' @description
#' Validates that fields contain sufficient non-NA data according to NDA requirements.
#' In strict mode, ANY NA in required fields or ALL NA in recommended fields = failure.
#' In lenient mode, these trigger warnings only.
#'
#' IMPORTANT: Only checks the 5 super required fields from ndar_subject01.
#' Structure-level required fields (like phq9_1, phq9_2, etc.) are NOT validated.
#'
#' @param state ValidationState object
#' @param elements Data elements from NDA structure
#' @param super_required_fields Character vector - the 5 mandatory NDA fields to check
#' @param strict Logical - if TRUE, enforce strict validation
#' @param verbose Logical - print detailed output
#' @return List with two elements: required_violations and recommended_violations
#' @keywords internal
#' @noRd
check_field_data_completeness <- function(state, elements, super_required_fields = SUPER_REQUIRED_FIELDS, strict = TRUE, verbose = FALSE) {
  df <- state$get_df()
  
  # Force elements to be base data.frame to avoid tibble/data.table evaluation issues
  # When dplyr is loaded, jsonlite::fromJSON returns tibbles which have different
  # scoping behavior during subsetting operations (e.g., tibble[condition, ])
  if (!is.data.frame(elements) || inherits(elements, c("tbl_df", "tbl", "data.table"))) {
    elements <- as.data.frame(elements, stringsAsFactors = FALSE)
  }
  
  required_violations <- list()
  recommended_violations <- list()
  
  # Get required elements - ONLY check super required fields
  # Structure-level required fields (like phq9_*) are NOT validated
  # This prevents false positives for fields that are required in the structure
  # but not part of the core 5 super required fields from ndar_subject01
  required_elements <- elements[
    elements$required == "Required" & 
    elements$name %in% super_required_fields,
  ]
  
  # Get recommended elements (check all of them)
  recommended_elements <- elements[elements$required == "Recommended", ]
  
  # Check REQUIRED fields for ANY NA
  if (nrow(required_elements) > 0) {
    for (i in 1:nrow(required_elements)) {
      field_name <- required_elements$name[i]
      
      if (field_name %in% names(df)) {
        field_values <- df[[field_name]]
        na_count <- sum(is.na(field_values))
        total_count <- length(field_values)
        
        # In strict mode: ANY NA is a violation
        # In lenient mode: ANY NA is also a violation (but handled as warning)
        if (na_count > 0) {
          required_violations[[field_name]] <- list(
            field = field_name,
            total_rows = total_count,
            na_count = na_count,
            percent_missing = round((na_count / total_count) * 100, 1),
            issue = if (na_count == total_count) {
              "All values are NA"
            } else {
              sprintf("%d of %d values are NA (%.1f%%)", na_count, total_count, (na_count / total_count) * 100)
            }
          )
          
          # Show individual messages only in strict mode OR verbose mode
          # In lenient non-verbose mode, suppress to avoid duplication with summary
          if (should_show_validation_message(strict, verbose)) {
            message(sprintf("[REQUIRED FIELD ISSUE] %s: %s", field_name, required_violations[[field_name]]$issue))
          }
        }
      } else {
        # Field doesn't exist at all
        required_violations[[field_name]] <- list(
          field = field_name,
          total_rows = nrow(df),
          na_count = nrow(df),
          percent_missing = 100,
          issue = "Field missing from dataframe"
        )
        
        # Show individual messages only in strict mode OR verbose mode
        if (should_show_validation_message(strict, verbose)) {
          message(sprintf("[REQUIRED FIELD ISSUE] %s: Field missing from dataframe", field_name))
        }
      }
    }
  }
  
  # Check RECOMMENDED fields for ALL NA (only matters in strict mode)
  if (strict && nrow(recommended_elements) > 0) {
    for (i in 1:nrow(recommended_elements)) {
      field_name <- recommended_elements$name[i]
      
      if (field_name %in% names(df)) {
        field_values <- df[[field_name]]
        
        # Only flag if ALL values are NA
        if (all(is.na(field_values))) {
          recommended_violations[[field_name]] <- list(
            field = field_name,
            total_rows = length(field_values),
            na_count = length(field_values),
            percent_missing = 100,
            issue = "All values are NA"
          )
          
          # Show individual messages only in strict mode OR verbose mode
          if (should_show_validation_message(strict, verbose)) {
            message(sprintf("[RECOMMENDED FIELD ISSUE] %s: All values are NA", field_name))
          }
        }
      }
    }
  }
  
  return(list(
    required = required_violations,
    recommended = recommended_violations
  ))
}

#' Check DCC Fields for Missing Data
#'
#' @description
#' Validates that DCC (Data Coordinating Center) fields contain sufficient
#' non-NA data when dcc = TRUE. Checks both required and recommended DCC fields.
#'
#' @param state ValidationState object
#' @param elements Data elements from NDA structure
#' @param dcc_required_fields Character vector - DCC required fields to check
#' @param dcc_recommended_fields Character vector - DCC recommended fields to check
#' @param strict Logical - if TRUE, enforce strict validation
#' @param verbose Logical - print detailed output
#' @return List with two elements: required_violations and recommended_violations
#' @keywords internal
#' @noRd
check_dcc_fields <- function(state, elements, 
                             dcc_required_fields = DCC_REQUIRED_FIELDS,
                             dcc_recommended_fields = DCC_RECOMMENDED_FIELDS,
                             strict = TRUE, verbose = FALSE) {
  df <- state$get_df()
  
  # Force elements to be base data.frame
  if (!is.data.frame(elements) || inherits(elements, c("tbl_df", "tbl", "data.table"))) {
    elements <- as.data.frame(elements, stringsAsFactors = FALSE)
  }
  
  required_violations <- list()
  recommended_violations <- list()
  
  # Get DCC required elements that are in the structure
  dcc_required_in_structure <- elements[
    elements$required == "Required" & 
    elements$name %in% dcc_required_fields,
  ]
  
  # Get DCC recommended elements that are in the structure
  dcc_recommended_in_structure <- elements[
    elements$required == "Recommended" &
    elements$name %in% dcc_recommended_fields,
  ]
  
  # Check DCC required fields - ANY NA = violation
  if (nrow(dcc_required_in_structure) > 0) {
    for (i in seq_len(nrow(dcc_required_in_structure))) {
      field_name <- dcc_required_in_structure$name[i]
      
      if (field_name %in% names(df)) {
        field_data <- df[[field_name]]
        na_count <- sum(is.na(field_data))
        
        if (na_count > 0) {
          issue <- sprintf("DCC required field has %d/%d missing values",
                          na_count, length(field_data))
          required_violations[[field_name]] <- list(
            field = field_name,
            issue = issue,
            na_count = na_count,
            total_count = length(field_data)
          )
          
          if (should_show_validation_message(strict, verbose)) {
            message(sprintf("  [%s] %s: %s",
                           if (strict) "ERROR" else "WARN",
                           field_name,
                           issue))
          }
        }
      }
    }
  }
  
  # Check DCC recommended fields - ALL NA = violation (strict mode only)
  if (strict && nrow(dcc_recommended_in_structure) > 0) {
    for (i in seq_len(nrow(dcc_recommended_in_structure))) {
      field_name <- dcc_recommended_in_structure$name[i]
      
      if (field_name %in% names(df)) {
        field_data <- df[[field_name]]
        
        if (all(is.na(field_data))) {
          issue <- "DCC recommended field is entirely missing (all NA)"
          recommended_violations[[field_name]] <- list(
            field = field_name,
            issue = issue,
            na_count = length(field_data),
            total_count = length(field_data)
          )
          
          if (should_show_validation_message(strict, verbose)) {
            message(sprintf("  [%s] %s: %s",
                           if (strict) "ERROR" else "WARN",
                           field_name,
                           issue))
          }
        }
      }
    }
  }
  
  return(list(
    required_violations = required_violations,
    recommended_violations = recommended_violations
  ))
}

#' Print validation summary
#' @param state ValidationState object
#' @noRd
print_validation_summary <- function(state) {
  message("\n\nValidation Summary:")
  message(sprintf("- Status: %s", if(state$is_valid) "PASSED" else "FAILED"))
  message(sprintf("- Structure Type: %s", 
                 if(state$is_new_structure) "NEW" else "EXISTING"))
  
  if (!state$is_new_structure) {
    message(sprintf("- Modified: %s", 
                   if(state$is_modified_structure) "YES" else "NO"))
  }
  
  if (length(state$value_range_violations) > 0) {
    message(sprintf("- Value Range Violations: %d field(s) (%s)",
                   length(state$value_range_violations),
                   paste(names(state$value_range_violations), collapse = ", ")))
  }
  
  if (length(state$new_fields) > 0) {
    message(sprintf("- New Fields: %d (%s)",
                   length(state$new_fields),
                   paste(state$new_fields, collapse = ", ")))
  }
  
  if (length(state$missing_required) > 0) {
    message(sprintf("- Missing Required: %d field(s) (%s)",
                   length(state$missing_required),
                   paste(state$missing_required, collapse = ", ")))
  }
  
  message(sprintf("- Needs Data Definition: %s (reason: %s)",
                 if(state$needs_data_definition()) "YES" else "NO",
                 state$get_modification_reason()))
}
