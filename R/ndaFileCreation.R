#' NDA File Creation Helper Functions
#'
#' @description
#' Functions for creating NDA submission templates and data definition files
#' based on ValidationState results.
#'

#' Get educational description for structure type
#'
#' @description
#' Returns a user-friendly explanation of what each structure type means
#' and what actions the researcher needs to take.
#'
#' @param structure_type Character - one of "NEW", "MODIFIED", or "EXISTING"
#' @return Character string with educational description
#' @noRd
get_structure_description <- function(structure_type) {
  descriptions <- list(
    NEW = paste0(
      "This data structure does not yet exist in NDA. The draft data definitions file\n",
      "must be submitted to DCC and approved by NDA. When approved, you can create your\n",
      "submission file."
    ),
    
    EXISTING = paste0(
      "Your data structure already exists in NDA and requires no modifications. You can\n",
      "submit your submission file at any time."
    ),
    
    MODIFIED = paste0(
      "While your data structure already exists in NDA, you have made modifications to\n",
      "value ranges and/or added new elements. Please submit the draft definitions file\n",
      "to DCC. When approved by NDA, you can create your submission file."
    )
  )
  
  return(descriptions[[structure_type]])
}

#' Check if files should be created based on validation state
#'
#' @description
#' Validation gate that determines whether file creation should proceed.
#' In strict mode: validation failure → no files created
#' In lenient mode: validation failure → files created with warnings
#'
#' @param validation_state ValidationState object
#' @param strict Logical - if TRUE, enforce strict validation (no files on failure)
#' @param verbose Logical - print detailed messages
#' @return Logical - TRUE if files should be created, FALSE if validation failed
#' @noRd
should_create_nda_files <- function(validation_state, strict = TRUE, verbose = TRUE) {
  # Only check ValidationState objects (not legacy lists)
  if (!inherits(validation_state, "ValidationState")) {
    return(TRUE)  # Legacy format always proceeds
  }
  
  # LENIENT MODE with validation failures: Create files anyway (with warnings)
  if (!validation_state$is_valid && !strict) {
    if (verbose) {
      message("[WARN] Creating files despite validation failure (lenient mode)")
    }
    return(TRUE)  # Proceed to file creation
  }
  
  # STRICT MODE with validation failures: Skip file creation
  if (!validation_state$is_valid) {
    if (verbose) {
      message("[VALIDATION FAILED] Skipping file creation due to validation errors")
      if (length(validation_state$errors) > 0) {
        message("Validation errors:")
        for (err in validation_state$errors) {
          message(sprintf("  - %s", err))
        }
      }
    } else {
      message("[SKIPPED - Validation failed]")
    }
    return(FALSE)  # Do not create files
  }
  
  return(TRUE)  # Validation passed, create files
}

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
#' @param strict Logical - if TRUE, enforce strict validation (no files on failure)
#' @param verbose Logical - print detailed messages
#' @return Invisible NULL
#' @noRd
create_nda_files <- function(validation_state, measure = NULL, strict = TRUE, verbose = TRUE) {
  
  # Validation gate: Check if files should be created
  if (!should_create_nda_files(validation_state, strict, verbose)) {
    return(invisible(NULL))
  }
  
  # Handle both ValidationState and legacy list formats
  if (inherits(validation_state, "ValidationState")) {
    # NEW path: Use ValidationState
    is_new <- validation_state$is_new_structure
    needs_definition <- validation_state$needs_data_definition()
    reason <- validation_state$get_modification_reason()
    measure_name <- validation_state$measure_name
    nda_structure <- validation_state$nda_structure
    
    # Determine modification type
    modification_type <- if (is_new) {
      "NEW"
    } else if (needs_definition) {
      "MODIFIED"
    } else {
      "EXISTING"
    }
    
    # Print STEP 4 header with structure type
    if (verbose) {
      message(sprintf("\n=== Creating NDA Files for '%s' ===", measure_name))
      message(sprintf("Structure type: %s", if(is_new) "NEW" else "EXISTING"))
      message(sprintf("Modification status: %s", reason))
    } else {
      # Context-aware header based on structure type
      step4_header <- if (is_new) {
        "=== STEP 4: New Data Structure ==="
      } else if (needs_definition) {
        "=== STEP 4: Modified Data Structure ==="
      } else {
        "=== STEP 4: Existing Data Structure ==="
      }
      message(sprintf("\n%s", step4_header))
    }
    
    # Print educational description (non-verbose only)
    if (!verbose) {
      description <- get_structure_description(modification_type)
      message(description)
      message("")  # Blank line
      
      # Add lenient mode warning if validation failed
      if (!strict && !validation_state$is_valid) {
        message("[WARN] Running in lenient mode - creating DRAFT files despite validation errors")
        message("")  # Blank line
      }
    }
    
    # Show modification details for MODIFIED structures (non-verbose only)
    if (!verbose && needs_definition && !is_new) {
      message("")  # Blank line before changes
      message("  Changes detected:")
      
      # Show new fields
      if (length(validation_state$new_fields) > 0) {
        for (field in validation_state$new_fields) {
          message(sprintf("    - Added new element: %s", field))
        }
      }
      
      # Show DCC additions
      if (length(validation_state$ndar_subject_additions) > 0) {
        for (field in validation_state$ndar_subject_additions) {
          message(sprintf("    - Added DCC field: %s", field))
        }
      }
      
      # Show value range changes
      if (length(validation_state$value_range_violations) > 0) {
        for (field in names(validation_state$value_range_violations)) {
          violation <- validation_state$value_range_violations[[field]]
          if (!is.null(violation$expected)) {
            # Extract just the unique violating values for cleaner display
            violating_vals <- unique(violation$actual)
            message(sprintf("    - Updated value range: %s (extended from %s to include: %s)",
                           field,
                           violation$expected,
                           paste(violating_vals, collapse = ", ")))
          } else {
            message(sprintf("    - Added value range for new element: %s", field))
          }
        }
      }
      message("")
    }
    
    # Use centralized field selection (user prompted ONCE)
    field_selection <- select_nda_fields(
      validation_state = validation_state,
      nda_structure = nda_structure,
      verbose = verbose
    )
    
    if (is_new) {
      # NEW structure: Handle submission file based on strict mode
      
      # STEP 4A: Submission Template
      if (!verbose) {
        message("\n=== Submission Template ===")
      }
      
      if (!strict) {
        # LENIENT MODE: Create DRAFT submission file
        tryCatch({
          # Create file
          createNdaSubmissionTemplate(
            measure_name,
            skip_prompt = TRUE,
            selected_fields = field_selection$selected_fields,
            skip_prompts = TRUE,
            verbose = verbose
          )
          
          # Rename to _draft suffix
          original_path <- sprintf("./tmp/%s_submission.csv", measure_name)
          draft_path <- sprintf("./tmp/%s_submission_draft.csv", measure_name)
          if (file.exists(original_path)) {
            file.rename(original_path, draft_path)
          }
          
          if (!verbose) {
            message(sprintf("[DRAFT] ./tmp/%s_submission_draft.csv", measure_name))
          } else {
            message("[DRAFT] Draft submission file created for testing")
          }
        }, error = function(e) {
          warning(sprintf("Error creating draft submission file: %s", e$message))
        })
      } else {
        # STRICT MODE: Skip submission file
        if (!verbose) {
          message("[SKIP] Structure must be registered with NDA before creating submission file")
        } else {
          message("[SKIP] Skipping submission file (structure doesn't exist in NDA yet)")
        }
      }
      
      # STEP 4B: Data Definition (always created)
      if (!verbose) {
        message("")
        message("=== Data Definition ===")
      } else {
        message("\n[NEW STRUCTURE]")
        message("[OK] Creating data definition (for structure registration)")
      }
      
      tryCatch({
        createNdaDataDefinition(
          validation_state,
          selected_fields = field_selection$selected_fields,
          skip_prompts = TRUE,
          verbose = verbose
        )
      }, error = function(e) {
        warning(sprintf("Error creating data definition: %s", e$message))
      })
      
    } else {
      # EXISTING structure: Create submission file (and data definition if modified)
      
      # STEP 4A: Submission Template
      if (!verbose) {
        message("\n=== Submission Template ===")
      } else {
        message("\n[EXISTING STRUCTURE]")
      }
      
      # Determine prefix and filename based on structure type and mode
      if (!strict && needs_definition) {
        # DRAFT mode: MODIFIED in lenient mode
        submission_prefix <- "[DRAFT]"
        submission_filename <- sprintf("%s_submission_draft.csv", measure_name)
        should_rename_to_draft <- TRUE
      } else if (!validation_state$is_valid) {
        # WARN mode: EXISTING with validation errors
        submission_prefix <- "[WARN]"
        submission_filename <- sprintf("%s_submission.csv", measure_name)
        should_rename_to_draft <- FALSE
      } else {
        # OK mode: Validation passed
        submission_prefix <- "[OK]"
        submission_filename <- sprintf("%s_submission.csv", measure_name)
        should_rename_to_draft <- FALSE
      }
      
      tryCatch({
        createNdaSubmissionTemplate(
          measure_name,
          skip_prompt = TRUE,  # Skip initial confirmation prompt
          selected_fields = field_selection$selected_fields,
          skip_prompts = TRUE,   # Skip field selection prompts
          verbose = verbose
        )
        
        # Rename to draft if needed
        if (should_rename_to_draft) {
          original_path <- sprintf("./tmp/%s_submission.csv", measure_name)
          draft_path <- sprintf("./tmp/%s_submission_draft.csv", measure_name)
          if (file.exists(original_path)) {
            file.rename(original_path, draft_path)
          }
        }
        
        if (!verbose) {
          message(sprintf("%s ./tmp/%s", submission_prefix, submission_filename))
        } else {
          message("[OK] Submission file created successfully")
        }
      }, error = function(e) {
        warning(sprintf("Error creating submission file: %s", e$message))
      })
      
      # STEP 4B: Data Definition (only if modified)
      if (needs_definition) {
        if (!verbose) {
          message("")  # Blank line
          message("=== Data Definition ===")
        } else {
          message("")  # Blank line
          message(sprintf("[OK] Creating data definition (reason: %s)", reason))
        }
        
        tryCatch({
          createNdaDataDefinition(
            validation_state,
            selected_fields = field_selection$selected_fields,
            skip_prompts = TRUE,
          verbose = verbose
        )
      }, error = function(e) {
        warning(sprintf("Error creating data definition: %s", e$message))
      })
      } else {
        if (!verbose) {
          message("")  # Blank line
          message("=== Data Definition ===")
          message("[SKIP] Structure unmodified (no data definition needed)")
        } else {
          message("")  # Blank line
          message("[SKIP] Skipping data definition (structure unmodified)")
        }
      }
    }
    
    # Add final file creation summary (non-verbose only)
    if (!verbose) {
      message("")  # Blank line
      files_created <- character()
      
      if (!is_new) {
        files_created <- c(files_created, "submission template")
      }
      
      if (needs_definition) {
        files_created <- c(files_created, "data definition")
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
        super_required <- SUPER_REQUIRED_FIELDS
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
        createNdaDataDefinition(submission_template, nda_structure, measure, verbose = verbose)
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
          createNdaDataDefinition(submission_template, nda_structure, measure, verbose = verbose)
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
