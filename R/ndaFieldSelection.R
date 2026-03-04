#' NDA Field Selection Helper Functions
#'
#' @description
#' Centralized field selection logic to eliminate redundant prompts when creating
#' both submission templates and data definition files.
#'

#' Select NDA Fields Interactively
#'
#' @description
#' Prompts user ONCE for field selection and returns choices.
#' Eliminates redundant prompts between submission template and data definition.
#' This function consolidates all field selection logic that was previously
#' duplicated across createNdaSubmissionTemplate() and createNdaDataDefinition().
#'
#' @param validation_state ValidationState object containing the measure data
#' @param nda_structure NDA structure definition (list with dataElements)
#' @param verbose Logical - print messages (default: TRUE)
#' @param interactive_mode Logical - allow user prompts (default: interactive())
#' @return List with components:
#'   \itemize{
#'     \item selected_fields - Character vector of field names to include
#'     \item user_choices - List of user decisions for reproducibility
#'     \item missing_required - Required fields not in data

#'     \item timepoint_fields - Timepoint fields that were added
#'   }
#' @noRd
select_nda_fields <- function(validation_state, 
                               nda_structure, 
                               verbose = TRUE,
                               interactive_mode = interactive()) {
  
  df <- validation_state$get_df()
  elements <- nda_structure$dataElements
  
  # Start with fields present in the data
  selected_fields <- names(df)
  
  # STEP 1: Remove DCC fields if dcc=FALSE
  # DCC fields should only be included when dcc=TRUE
  if (!validation_state$dcc && length(validation_state$ndar_subject_additions) > 0) {
    dcc_fields_in_selection <- intersect(selected_fields, validation_state$ndar_subject_additions)
    if (length(dcc_fields_in_selection) > 0) {
      selected_fields <- setdiff(selected_fields, validation_state$ndar_subject_additions)
      if (verbose) {
        message(sprintf("Excluding %d DCC field(s) from submission file (dcc=FALSE): %s", 
                       length(dcc_fields_in_selection),
                       paste(dcc_fields_in_selection, collapse=", ")))
      }
    }
  }
  
  # Define super-required ndar_subject01 fields (always needed)
  ndar_required <- c("subjectkey", "src_subject_id", "interview_date", 
                     "interview_age", "sex")
  
  # Get all fields defined in the NDA structure
  structure_fields <- elements$name
  
  # Find required fields that exist in structure definition
  required_in_structure <- elements$name[elements$required == "Required"]
  
  # Find required fields that are NOT in the data and NOT ndar_subject01 fields
  missing_required <- setdiff(
    required_in_structure, 
    c(ndar_required, selected_fields)
  )
  
  # Initialize user choices tracking
  user_choices <- list(
    include_required = FALSE
  )
  
  # Auto-add required fields — required means required, no prompt needed
  if (length(missing_required) > 0) {
    selected_fields <- unique(c(selected_fields, missing_required))
    user_choices$include_required <- TRUE

    # Safe readline with error handling (defined locally for this scope)
    safe_readline_local <- function(prompt, default = "") {
      result <- tryCatch({
        readline(prompt = prompt)
      }, error = function(e) {
        return(default)
      }, interrupt = function(e) {
        message("\nInput cancelled")
        return(default)
      })
      if (is.null(result) || trimws(result) == "") default else result
    }

    # Collect value assignments for remediation script
    assignments <- c()

    for (field in missing_required) {
      # Look up metadata from nda_structure$dataElements
      field_row   <- elements[elements$name == field, ]
      value_range <- if (nrow(field_row) > 0 && !is.na(field_row$valueRange[1])) field_row$valueRange[1] else ""
      notes       <- if (nrow(field_row) > 0 && !is.na(field_row$notes[1]))      field_row$notes[1]      else ""
      desc        <- if (nrow(field_row) > 0 && !is.na(field_row$description[1])) field_row$description[1] else ""

      message(sprintf("\nRequired field auto-added: %s", field))
      if (nchar(desc) > 0)        message(sprintf("  Description: %s", desc))
      if (nchar(value_range) > 0) message(sprintf("  Value range: %s", value_range))
      if (nchar(notes) > 0)       message(sprintf("  Notes:       %s", notes))

      if (interactive_mode) {
        val <- safe_readline_local(
          prompt = sprintf("  Enter value for %s$%s (Enter to leave as NA): ",
                           validation_state$measure_name, field),
          default = ""
        )
        if (nchar(trimws(val)) > 0) {
          assignments[field] <- trimws(val)
        }
      }
    }

    # Write assignments to remediation script
    if (length(assignments) > 0) {
      script_path <- sprintf("./nda/%s/%s.R",
                             validation_state$api,
                             validation_state$measure_name)
      if (file.exists(script_path)) {
        update_cleaning_script(script_path, validation_state$measure_name,
                               renames = c(), drops = c(), assignments = assignments,
                               verbose = verbose)
        message(sprintf("  Written to remediation script: %s", script_path))
      } else {
        if (verbose) message(sprintf("  Script not found, skipping write: %s", script_path))
      }
    }
  }
  
  # STEP 2: Ensure all super required fields are included
  # These are mandatory for NDA submission and must ALWAYS be present
  missing_super_required <- setdiff(ndar_required, selected_fields)
  if (length(missing_super_required) > 0) {
    selected_fields <- c(selected_fields, missing_super_required)
    if (verbose) {
      message(sprintf("Adding %d missing super required field(s): %s", 
                     length(missing_super_required),
                     paste(missing_super_required, collapse=", ")))
    }
  }
  
  # STEP 3: Reorder fields - super required first (in order), then other fields
  # This ensures consistent field ordering across all NDA submissions
  super_required_present <- ndar_required[ndar_required %in% selected_fields]
  other_fields <- setdiff(selected_fields, ndar_required)
  selected_fields <- c(super_required_present, other_fields)
  
  if (verbose) {
    message(sprintf("Field ordering: %d super required first, then %d other fields",
                   length(super_required_present), length(other_fields)))
  }
  
  return(list(
    selected_fields = selected_fields,
    user_choices = user_choices,
    missing_required = missing_required
  ))
}
