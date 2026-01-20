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
  
  # Interactive prompt for other required fields (only if in interactive mode)
  if (interactive_mode && length(missing_required) > 0) {
    if (verbose) {
      message("\nThe following NDA required fields exist in this structure but are not currently selected:")
      message(paste("  ", paste(missing_required, collapse = ", ")))
    }
    
    # Safe readline with error handling
    safe_readline <- function(prompt, default = "n") {
      result <- tryCatch({
        readline(prompt = prompt)
      }, error = function(e) {
        return(default)
      })
      if (is.null(result) || result == "") default else result
    }
    
    user_input <- safe_readline(
      prompt = sprintf("Would you like to include these %d required field(s)? (y/n): ", 
                       length(missing_required)),
      default = "n"
    )
    
    # Validate input
    attempts <- 0
    while (!tolower(user_input) %in% c("y", "n", "yes", "no") && attempts < 3) {
      user_input <- safe_readline(
        prompt = "Please enter 'y' for yes or 'n' for no: ",
        default = "n"
      )
      attempts <- attempts + 1
    }
    
    # Default to "n" if still invalid after 3 attempts
    if (!tolower(user_input) %in% c("y", "n", "yes", "no")) {
      user_input <- "n"
      if (verbose) message("Invalid input - defaulting to 'n' (skip fields)")
    }
    
    user_choices$include_required <- tolower(user_input) %in% c("y", "yes")
    
    if (user_choices$include_required) {
      selected_fields <- unique(c(selected_fields, missing_required))
      if (verbose) {
        message(sprintf("Added %d required field(s) to selection.", 
                       length(missing_required)))
      }
    } else {
      if (verbose) {
        message("Skipping required fields. Note: These fields may be needed for NDA submission.")
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
