#' NDA Data Structure Validator
#'
#' @description
#' Main validation function for NDA data structure compliance.
#' This is the refactored version using modular components.
#' 
#' This is an internal function called by nda(). Users should not call this directly.
#'
#' @param measure_name Name of measure/structure
#' @param api API type (redcap, qualtrics, mongo, csv, oracle, sql)
#' @param limited_dataset Logical - if TRUE, skip de-identification (date-shifting, age-capping)
#' @param nda_base_url NDA API base URL
#' @param verbose Logical - print detailed output
#' @param debug Logical - print debug information
#' @param auto_drop_unknown Logical - automatically drop unknown fields
#' @param interactive_mode Logical - allow user prompts
#' @param modified_structure Pre-enhanced NDA structure (from ndaRequest.R)
#' @return ValidationState object with validation results
#' @keywords internal
#' @noRd
ndaValidator <- function(measure_name,
                         api,
                         limited_dataset = FALSE,
                             nda_base_url = "https://nda.nih.gov/api/datadictionary/v2",
                             verbose = FALSE,
                             debug = FALSE,
                             auto_drop_unknown = FALSE,
                             interactive_mode = TRUE,
                             modified_structure = NULL) {
  
  tryCatch({
    # Initialize environment
    # NOTE: This assignment to globalenv is intentional for cross-function data sharing
    # in the NDA validation workflow and maintains backward compatibility with legacy code
    if (!exists(".wizaRdry_env", envir = globalenv())) {
      .wizaRdry_env <- new.env(parent = parent.frame())
      assign(".wizaRdry_env", .wizaRdry_env, envir = globalenv())
    }
    
    # Get dataframe
    df <- base::get(measure_name, envir = .wizaRdry_env)
    if (is.null(df) || !is.data.frame(df)) {
      stop(sprintf("Dataframe '%s' not found or is not a data.frame", measure_name))
    }
    
    if (verbose) {
      message(sprintf("\n\n=== Starting NDA Validation for '%s' ===", measure_name))
      message(sprintf("API: %s | Rows: %d | Columns: %d", api, nrow(df), ncol(df)))
    }
    
    # Get or fetch NDA structure
    if (!is.null(modified_structure)) {
      if (verbose) message("\nUsing enhanced NDA structure with ndar_subject01 definitions...")
      elements <- modified_structure$dataElements
      nda_structure <- modified_structure
    } else {
      if (verbose) message(sprintf("\nFetching '%s' from NDA API...", measure_name))
      nda_structure <- fetch_nda_structure(measure_name, nda_base_url)
      elements <- nda_structure$dataElements
    }
    
    if (is.null(elements) || nrow(elements) == 0) {
      stop("No dataElements found in structure definition")
    }
    
    # Create ValidationState object
    state <- ValidationState$new(measure_name, api, df, nda_structure)
    
    # Add STEP 2 header (only in non-verbose mode)
    if (!verbose) {
      if (state$is_new_structure) {
        message("\n=== STEP 2: Preparing New Structure ===")
      } else {
        message("\n=== STEP 2: Validating Data Structure ===")
        message(sprintf("Validating against NDA structure '%s'...", measure_name))
      }
    }
    
    if (verbose) {
      message(sprintf("Structure type: %s", 
                     if(state$is_new_structure) "NEW (not in NDA)" else "EXISTING"))
      message(sprintf("Structure has %d field definitions", nrow(elements)))
    }
    
    # ============================================================================
    # PHASE 1: Data Cleaning
    # ============================================================================
    if (verbose) message("\n--- PHASE 1: Data Cleaning ---")
    
    # Convert problematic column types
    df <- convert_problematic_column_types(df, measure_name, verbose)
    state$set_df(df)
    
    # Convert logical columns to character
    df <- convert_logical_to_character(df, verbose)
    state$set_df(df)
    
    # ============================================================================
    # PHASE 2: Field Standardization
    # ============================================================================
    if (verbose) message("\n--- PHASE 2: Field Standardization ---")
    
    # Standardize column names
    df <- standardize_column_names(df, measure_name, verbose)
    df <- standardize_field_names(df, measure_name, verbose)
    state$set_df(df)
    
    # Handle missing required fields
    required_fields <- elements$name[elements$required == "Required"]
    missing_required <- required_fields[!required_fields %in% names(df)]
    if (length(missing_required) > 0) {
      df <- handle_missing_fields(df, elements, missing_required, verbose)
      state$set_df(df)
    }
    
    # ============================================================================
    # PHASE 3: De-identification
    # ============================================================================
    if (!verbose) {
      message("\n=== STEP 3: De-identifying Data ===")
    } else {
      message("\n--- PHASE 3: De-identification ---")
    }
    
    # Date-shifting with HIPAA reference (always show message in non-verbose)
    if ("interview_date" %in% names(df)) {
      if (!verbose && !limited_dataset) {
        message("Applying date-shifting to interview_date (per HIPAA Safe Harbor de-identification standard)...")
      }
      df <- standardize_dates(df, verbose = verbose, limited_dataset = limited_dataset)
      if (!verbose && !limited_dataset) {
        message("[OK] Date-shifting complete")
      }
    }
    
    # Age-capping with HIPAA reference (always show message in non-verbose)
    if ("interview_age" %in% names(df)) {
      if (!verbose && !limited_dataset) {
        message("Applying age-capping to interview_age (threshold: 89 years per HIPAA Safe Harbor de-identification standard)...")
      }
      
      age_result <- standardize_age(df, verbose = verbose, limited_dataset = limited_dataset)
      
      # Defensive check: ensure age_result is a list with df and stats
      if (is.list(age_result) && "df" %in% names(age_result) && "stats" %in% names(age_result)) {
        df <- age_result$df
        age_stats <- age_result$stats
        
        # Show result based on statistics (always show in non-verbose, not just verbose)
        if (!verbose && !limited_dataset) {
          if (isTRUE(age_stats$all_na)) {
            message("[OK] All values are NA - no age-capping needed")
          } else if (!is.null(age_stats$values_capped) && age_stats$values_capped > 0) {
            message(sprintf("[WARN] %d value%s exceeded threshold and %s capped to 1068 months (89 years)",
                           age_stats$values_capped,
                           if (age_stats$values_capped > 1) "s" else "",
                           if (age_stats$values_capped > 1) "were" else "was"))
          } else if (!is.null(age_stats$non_na_count) && age_stats$non_na_count > 0) {
            message(sprintf("[OK] All %d values below threshold - no capping needed", age_stats$non_na_count))
          } else {
            message("[OK] No age-capping needed")
          }
        }
      } else {
        # Fallback: age_result might be just a dataframe (old format)
        if (is.data.frame(age_result)) {
          df <- age_result
        }
        if (!verbose && !limited_dataset) {
          message("[OK] Age-capping complete")
        }
      }
    }
    
    state$set_df(df)
    
    if (limited_dataset == FALSE && verbose) {
      message("\nDataset has been de-identified using date-shifting and age-capping.")
    }
    
    # ============================================================================
    # PHASE 4: Value Range Validation (THE KEY FIX)
    # ============================================================================
    if (verbose) message("\n--- PHASE 4: Value Range Validation ---")
    
    # This is the critical function that properly tracks violations
    violations <- check_value_range_violations(state, elements, verbose)
    
    # Show value range summary (violations only, unless verbose)
    if (!verbose) {
      total_fields <- length(intersect(names(state$get_df()), elements$name))
      
      if (length(violations) == 0) {
        message(sprintf("Checking %d fields for NDA compliance...", total_fields))
        message("[OK] All fields validated successfully (0 violations)")
      } else {
        message(sprintf("Checking %d fields for NDA compliance...", total_fields))
        message("[WARN] Value range violations found:\n")
        
        for (field_name in names(violations)) {
          violation <- violations[[field_name]]
          expected_str <- if (is.null(violation$expected_range)) {
            "no range defined"
          } else {
            violation$expected_range
          }
          
          violating_vals <- violation$violating_values
          message(sprintf("  Field: %s", field_name))
          message(sprintf("  Expected range: %s", expected_str))
          message(sprintf("  Violating values found: %s (%d occurrence%s)", 
                         paste(violating_vals, collapse = ", "),
                         length(violating_vals),
                         if (length(violating_vals) > 1) "s" else ""))
          message("")
        }
        
        message(sprintf("[WARN] Validation completed with %d violation%s",
                       length(violations),
                       if (length(violations) > 1) "s" else ""))
      }
    }
    
    if (length(violations) > 0 && verbose) {
      message(sprintf("\nDetected %d field(s) with value range issues", length(violations)))
      message("These will trigger data definition file creation")
    }
    
    # ============================================================================
    # PHASE 5: New Field Detection
    # ============================================================================
    if (verbose) message("\n--- PHASE 5: New Field Detection ---")
    
    state$new_fields <- detect_new_fields(state$get_df(), elements)
    
    if (length(state$new_fields) > 0) {
      state$is_modified_structure <- TRUE
      if (verbose) {
        message(sprintf("\nDetected %d new field(s) not in NDA structure:", 
                       length(state$new_fields)))
        message(sprintf("  %s", paste(state$new_fields, collapse = ", ")))
      }
    }
    
    # ============================================================================
    # PHASE 6: Final Validation Summary
    # ============================================================================
    if (verbose) {
      message("\n--- PHASE 6: Validation Summary ---")
      print_validation_summary(state)
    }
    
    # Return ValidationState object (not a plain list)
    return(state)
    
  }, error = function(e) {
    error_msg <- sprintf("Error in ndaValidator for '%s': %s", measure_name, e$message)
    message(error_msg)
    
    if (debug) {
      message("\nTraceback:")
      message(paste(capture.output(traceback()), collapse="\n"))
    }
    
    # Try to get the dataframe for error state
    error_df <- tryCatch(base::get(measure_name, envir = .wizaRdry_env), error = function(e2) data.frame())
    
    # Use the modified_structure if provided (existing structures), otherwise NULL (new structures)
    error_nda_structure <- if (!is.null(modified_structure)) modified_structure else NULL
    
    # Return error state for graceful failure
    error_state <- ValidationState$new(measure_name, api, 
                                       error_df, error_nda_structure)
    error_state$is_valid <- FALSE
    error_state$errors <- c(error_msg)
    return(error_state)
  })
}
