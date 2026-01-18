#' NDA File Creation Helper Functions
#'
#' @description
#' Functions for creating NDA submission templates and data definition files
#' based on ValidationState results.
#'
#' @name ndaFileCreation
NULL

#' Create NDA files based on ValidationState
#'
#' @description
#' Determines which NDA files to create (submission template and/or data definition)
#' based on whether the structure is new, existing and unmodified, or existing and modified.
#'
#' Decision logic:
#' - NEW structure: Create data definition only (can't submit to non-existent structure)
#' - EXISTING unmodified: Create submission template only
#' - EXISTING modified: Create both submission template and data definition
#'
#' @param validation_state ValidationState object OR legacy list (for backward compatibility)
#' @param measure Measure name (used for legacy list format)
#' @param verbose Logical - print detailed messages
#' @return Invisible NULL
#' @noRd
create_nda_files <- function(validation_state, measure = NULL, verbose = TRUE) {
  
  # Handle both ValidationState and legacy list formats
  if (inherits(validation_state, "ValidationState")) {
    # NEW path: Use ValidationState
    is_new <- validation_state$is_new_structure
    needs_definition <- validation_state$needs_data_definition()
    reason <- validation_state$get_modification_reason()
    measure_name <- validation_state$measure_name
    nda_structure <- validation_state$nda_structure
    
    if (verbose) {
      message(sprintf("\n=== Creating NDA Files for '%s' ===", measure_name))
      message(sprintf("Structure type: %s", if(is_new) "NEW" else "EXISTING"))
      message(sprintf("Modification status: %s", reason))
    }
    
    # Use centralized field selection (user prompted ONCE)
    field_selection <- select_nda_fields(
      validation_state = validation_state,
      nda_structure = nda_structure,
      verbose = verbose
    )
    
    if (is_new) {
      # NEW structure: Only create data definition
      if (verbose) {
        message("\n[NEW STRUCTURE]")
        message("  [OK] Creating data definition (for structure registration)")
        message("  [SKIP] Skipping submission template (structure doesn't exist in NDA yet)")
      }
      
      tryCatch({
        createNdaDataDefinition(
          validation_state,
          selected_fields = field_selection$selected_fields,
          skip_prompts = TRUE
        )
        if (verbose) message("  [OK] Data definition created successfully")
      }, error = function(e) {
        warning(sprintf("Error creating data definition: %s", e$message))
      })
      
    } else {
      # EXISTING structure: Always create submission template
      if (verbose) {
        message("\n[EXISTING STRUCTURE]")
        message("  [OK] Creating submission template (for data upload)")
      }
      
      tryCatch({
        createNdaSubmissionTemplate(
          measure_name,
          skip_prompt = TRUE,  # Skip initial confirmation prompt
          selected_fields = field_selection$selected_fields,
          skip_prompts = TRUE   # Skip field selection prompts
        )
        if (verbose) message("  [OK] Submission template created successfully")
      }, error = function(e) {
        warning(sprintf("Error creating submission template: %s", e$message))
      })
      
      # Create data definition ONLY if modified
      if (needs_definition) {
        if (verbose) {
          message(sprintf("  [OK] Creating data definition (reason: %s)", reason))
        }
        
        tryCatch({
          createNdaDataDefinition(
            validation_state,
            selected_fields = field_selection$selected_fields,
            skip_prompts = TRUE
          )
          if (verbose) message("  [OK] Data definition created successfully")
        }, error = function(e) {
          warning(sprintf("Error creating data definition: %s", e$message))
        })
      } else {
        if (verbose) {
          message("  [SKIP] Skipping data definition (structure unmodified)")
        }
      }
    }
    
  } else {
    # LEGACY path: validation_state is actually a plain list
    if (verbose) {
      message("\n[LEGACY MODE] Using old validation_results format")
    }
    
    # Extract information from legacy format
    is_new <- isTRUE(validation_state$bypassed_validation)
    nda_structure <- attr(validation_state, "nda_structure")
    
    if (is.null(measure)) {
      stop("measure parameter required when using legacy validation_results format")
    }
    
    # Check for modifications in legacy format
    has_violations <- !is.null(validation_state$value_range_violations) && 
                     length(validation_state$value_range_violations) > 0
    
    # Check for new fields
    has_new_fields <- FALSE
    if (!is.null(nda_structure) && "dataElements" %in% names(nda_structure)) {
      df <- validation_state$df
      if (!is.null(df) && is.data.frame(df)) {
        structure_fields <- nda_structure$dataElements$name
        df_fields <- names(df)
        new_fields <- setdiff(df_fields, structure_fields)
        # Exclude special fields
        new_fields <- new_fields[!grepl("_complete$", new_fields)]
        super_required <- c("subjectkey", "src_subject_id", "sex", "interview_age", "interview_date")
        new_fields <- setdiff(new_fields, super_required)
        has_new_fields <- length(new_fields) > 0
      }
    }
    
    needs_definition <- is_new || has_violations || has_new_fields
    
    # Apply same logic as ValidationState path
    if (is_new) {
      if (verbose) message("Creating data definition for new structure (legacy mode)")
      tryCatch({
        submission_template <- list(columns = names(validation_state$df))
        createNdaDataDefinition(submission_template, nda_structure, measure)
      }, error = function(e) {
        warning(sprintf("Error creating data definition: %s", e$message))
      })
    } else {
      if (verbose) message("Creating submission template for existing structure (legacy mode)")
      tryCatch({
        createNdaSubmissionTemplate(measure)
      }, error = function(e) {
        warning(sprintf("Error creating submission template: %s", e$message))
      })
      
      if (needs_definition) {
        if (verbose) message("Creating data definition for modified structure (legacy mode)")
        tryCatch({
          submission_template <- list(columns = names(validation_state$df))
          createNdaDataDefinition(submission_template, nda_structure, measure)
        }, error = function(e) {
          warning(sprintf("Error creating data definition: %s", e$message))
        })
      } else {
        if (verbose) message("Skipping data definition - unmodified structure (legacy mode)")
      }
    }
  }
  
  invisible(NULL)
}
