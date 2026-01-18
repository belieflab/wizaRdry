#' NDA Validation Helper Functions
#'
#' @description
#' Core validation logic for NDA data structure compliance.
#' Includes value range checking, GUID validation, and violation tracking.
#'
#' @name ndaValidationHelpers
NULL

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
#' @export
check_value_range_violations <- function(state, elements, verbose = FALSE) {
  if (verbose) cat("\n\nChecking value ranges...")
  
  df <- state$get_df()
  violations_detected <- list()
  df_cols <- names(df)
  
  for (col in intersect(df_cols, elements$name)) {
    element <- elements[elements$name == col, ]
    
    if (nrow(element) == 0) next
    
    value_range <- element$valueRange
    
    # Case 1: Field has no valueRange but contains data
    # This should trigger data definition creation
    if (is.null(value_range) || is.na(value_range) || value_range == "") {
      if (has_data_values(df[[col]])) {
        unique_vals <- get_unique_values(df[[col]])
        
        state$add_value_range_violation(
          field = col,
          expected = NULL,
          actual = unique_vals
        )
        
        violations_detected[[col]] <- list(
          type = "missing_range",
          field = col,
          expected = NULL,
          actual = unique_vals,
          count = length(unique_vals)
        )
        
        if (verbose) {
          cat(sprintf("\n\nField: %s (no valueRange defined in NDA structure)", col))
          cat(sprintf("\n  Data contains %d unique values needing documentation", 
                     length(unique_vals)))
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
#' @export
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
  super_required <- c("subjectkey", "src_subject_id", "sex", 
                     "interview_age", "interview_date")
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
#' @export
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
