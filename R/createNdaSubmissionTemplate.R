#' Create NDA Submission Template
#'
#' This function creates a CSV template file for National Data Archive (NDA) submissions.
#' It extracts the data from a specified data frame and formats it according to NDA requirements,
#' with the structure name split into base name and suffix in the first line.
#' The function will prompt for confirmation before creating the file,
#' with an option to remember the user's preference for future calls.
#'
#' @param df Data frame to be used as template or character string naming a data frame
#'        in the global environment.
#' @param path Character string specifying the directory path where the "tmp" folder
#'        and template file should be created. Defaults to the current working directory.
#' @param skip_prompt Logical. If TRUE (default), skips the confirmation prompt. If FALSE,
#'        will prompt for confirmation unless the user has previously chosen to remember their preference.
#' @param selected_fields Character vector of field names to include in template. If NULL (default),
#'        uses all fields from data frame. Used by create_nda_files() for centralized field selection.
#' @param skip_prompts Logical. If TRUE, skip ALL interactive prompts (used when called from
#'        create_nda_files() with pre-selected fields). Default: FALSE.
#'
#' @return Invisible TRUE if successful. Creates a CSV file at the specified path
#'         and prints a message with the file location.
#'
#' @details
#' The function will:
#' 1. Create a 'tmp' directory if it doesn't exist
#' 2. Parse the structure name into base and suffix components (e.g., "eefrt01" → "eefrt" and "01")
#' 3. Write the structure name components as the first line
#' 4. Write column headers as the second line
#' 5. Write the data rows below
#'
#' @examples
#' \dontrun{
#'   # First create some sample data
#'   eefrt01 <- data.frame(
#'     src_subject_id = c("SUB001", "SUB002"),
#'     interview_age = c(240, 360),
#'     interview_date = c("01/01/2023", "02/15/2023"),
#'     response_time = c(450, 520)
#'   )
#'
#'   # Create the NDA template using the data frame directly
#'   to.nda(eefrt01)
#'
#'   # Or using the name as a string
#'   to.nda("eefrt01")
#'
#'   # Skip the confirmation prompt
#'   to.nda(eefrt01, skip_prompt = TRUE)
#' }
#'
#' @export
to.nda <- function(df, path = ".", skip_prompt = TRUE, selected_fields = NULL, skip_prompts = FALSE) { #set skip_prompt to TRUE so users need to specify to see prompt after prefs are set
  # Check for user preferences file
  user_prefs_file <- file.path(path, ".wizaRdry_prefs")
  user_prefs <- list(shown_tree = FALSE, auto_create = FALSE, auto_clean = FALSE, auto_nda = FALSE, auto_nda_template = FALSE)

  if (file.exists(user_prefs_file)) {
    tryCatch({
      user_prefs <- readRDS(user_prefs_file)
      # Add the auto_nda_template field if it doesn't exist
      if (is.null(user_prefs$auto_nda_template)) {
        user_prefs$auto_nda_template <- FALSE
      }
    }, error = function(e) {
      # If file exists but can't be read, create a new one
      user_prefs <- list(shown_tree = FALSE, auto_create = FALSE, auto_clean = FALSE, auto_nda = FALSE, auto_nda_template = FALSE)
    })
  }

  # Determine whether df is a data frame or a string
  if (is.character(df)) {
    df_name <- df
    if (!exists(df_name, envir = .GlobalEnv)) {
      stop(paste("Data frame", df_name, "not found in global environment"))
    }
  } else {
    df_name <- deparse(substitute(df))
  }

  # If skip_prompt is TRUE or user has previously set auto_nda_template to TRUE, bypass the prompt
  if (!skip_prompt | !user_prefs$auto_nda_template) {
    response <- readline(prompt = sprintf("Would you like to create the NDA submission template for %s now? y/n ",
                                          df_name))

    while (!tolower(response) %in% c("y", "n")) {
      response <- readline(prompt = "Please enter either y or n: ")
    }

    # If user selects y, save their preference automatically and set auto_nda_template to TRUE.
    if (tolower(response) == "y") {
      user_prefs$auto_nda_template <- TRUE
      saveRDS(user_prefs, user_prefs_file)
    }

    if (tolower(response) == "n") {
      message("NDA submission template creation cancelled.")
      invokeRestart("abort")  # This exits without the "Error:" prefix
    }
  }

  # Create directory structure if it doesn't exist
  tmp_path <- file.path(path, "tmp")
  if (!dir.exists(tmp_path)) {
    dir.create(tmp_path)
  }

  # Define structure_name
  structure_name <- df_name

  # Create the file path
  file_path <- file.path(tmp_path, paste0(structure_name, '_template.csv'))

  # Get the data frame
  if (is.character(df)) {
    template <- base::get(df, envir = .GlobalEnv)
  } else {
    template <- df
  }

  # Drop old pre-transformed fields (ending with .1, .2, etc.) that correspond to renamed fields
  # These are old versions of fields that were renamed during validation
  template_cols <- names(template)
  old_transformed_fields <- template_cols[grepl("\\.\\d+$", template_cols)]
  
  if (length(old_transformed_fields) > 0) {
    fields_to_drop <- character(0)
    for (old_field in old_transformed_fields) {
      # Extract base name by removing .1, .2, etc. suffix
      base_name <- sub("\\.\\d+$", "", old_field)
      # Check if the base name (without suffix) exists in the template
      if (base_name %in% template_cols) {
        # This is an old version of a renamed field - drop it
        fields_to_drop <- c(fields_to_drop, old_field)
      }
    }
    
    if (length(fields_to_drop) > 0) {
      template <- template[, !names(template) %in% fields_to_drop, drop = FALSE]
      message(sprintf("Dropped %d old pre-transformed field(s): %s", 
                     length(fields_to_drop), paste(fields_to_drop, collapse = ", ")))
      template_cols <- names(template)  # Update after dropping
    }
  }

  # Define fields to exclude from submission templates
  excluded_from_template <- c("state", "lost_to_followup", "lost_to_follow-up")
  
  # Super-required fields (always included): subjectkey, src_subject_id, interview_date, interview_age, sex
  super_required_fields <- SUPER_REQUIRED_FIELDS
  
  # Get structure field names to check which required fields exist in this structure
  structure_field_names <- character(0)
  ndar_required_in_structure <- character(0)
  
  # Try to fetch the structure to see which fields it contains
  tryCatch({
    nda_base_url <- "https://nda.nih.gov/api/datadictionary/v2"
    structure_url <- sprintf("%s/datastructure/%s", nda_base_url, structure_name)
    structure_response <- httr::GET(structure_url, httr::timeout(10))
    if (httr::status_code(structure_response) == 200) {
      raw_content <- rawToChar(structure_response$content)
      if (nchar(raw_content) > 0) {
        structure_data <- jsonlite::fromJSON(raw_content)
        if ("dataElements" %in% names(structure_data)) {
          structure_field_names <- structure_data$dataElements$name
        }
      }
    }
  }, error = function(e) {
    # Silently fail if structure fetch fails
  })
  
  # Get required fields from ndar_subject01 that exist in the current structure
  if (length(structure_field_names) > 0) {
    tryCatch({
      nda_base_url <- "https://nda.nih.gov/api/datadictionary/v2"
      url <- sprintf("%s/datastructure/ndar_subject01", nda_base_url)
      
      response <- httr::GET(url, httr::timeout(10))
      if (httr::status_code(response) == 200) {
        raw_content <- rawToChar(response$content)
        if (nchar(raw_content) > 0) {
          subject_structure <- jsonlite::fromJSON(raw_content)
          if ("dataElements" %in% names(subject_structure)) {
            ndar_required <- subject_structure$dataElements[subject_structure$dataElements$required == "Required", ]
            if (nrow(ndar_required) > 0) {
              ndar_required_names <- ndar_required$name
              # Get required fields that exist in structure but are not super-required
              ndar_required_in_structure <- setdiff(
                intersect(ndar_required_names, structure_field_names),
                super_required_fields
              )
            }
          }
        }
      }
    }, error = function(e) {
      # Silently fail if API call fails
    })
  }
  
  # Automatically include super-required fields if they exist in structure but not in template
  missing_super_required <- setdiff(
    intersect(super_required_fields, structure_field_names),
    template_cols
  )
  
  if (length(missing_super_required) > 0) {
    # Add missing super-required fields with NA values
    for (field in missing_super_required) {
      template[[field]] <- NA
    }
    message(sprintf("\nAutomatically including super-required fields: %s", 
                   paste(missing_super_required, collapse = ", ")))
    template_cols <- names(template)  # Update after adding fields
  }
  
  # Note: DCC required fields and timepoint fields removed - users should include them in their NDA script if needed
  
  # Use selected_fields if provided (from centralized field selection)
  # Otherwise, prompt user in interactive mode
  if (!is.null(selected_fields) && skip_prompts) {
    # Use pre-selected fields from create_nda_files()
    fields_to_add <- setdiff(selected_fields, template_cols)
    if (length(fields_to_add) > 0) {
      for (field in fields_to_add) {
        # Only add if field exists in structure
        if (field %in% structure_field_names) {
          template[[field]] <- NA
        }
      }
      template_cols <- names(template)
    }
  } else if (interactive() && length(ndar_required_in_structure) > 0 && !skip_prompts) {
    # Original interactive prompt logic (only when NOT using pre-selected fields)
    missing_required_in_structure <- setdiff(ndar_required_in_structure, template_cols)
    
    if (length(missing_required_in_structure) > 0) {
      message("\nThe following NDA required fields exist in this structure but are not in the template:")
      message(paste("  ", paste(missing_required_in_structure, collapse = ", ")))
      
      user_input <- readline(
        prompt = sprintf("Would you like to include these %d required field(s)? (y/n): ", 
                         length(missing_required_in_structure))
      )
      
      # Validate input
      while (!tolower(user_input) %in% c("y", "n", "yes", "no")) {
        user_input <- readline(
          prompt = "Please enter 'y' for yes or 'n' for no: "
        )
      }
      
      if (tolower(user_input) %in% c("y", "yes")) {
        # Add missing required fields with NA values
        for (field in missing_required_in_structure) {
          template[[field]] <- NA
        }
        message(sprintf("Added %d required field(s) to template.", 
                       length(missing_required_in_structure)))
        template_cols <- names(template)  # Update after adding fields
      } else {
        message("Skipping required fields. Note: These fields may be needed for NDA submission.")
      }
    }
  }
  
  # Get essential NDA fields dynamically from ndar_subject01 API
  # This ensures we always have the current required elements
  essential_nda_fields <- tryCatch({
    message("Fetching current ndar_subject01 required elements from NDA API...")
    nda_base_url <- "https://nda.nih.gov/api/datadictionary/v2"
    url <- sprintf("%s/datastructure/ndar_subject01", nda_base_url)
    
    response <- httr::GET(url, httr::timeout(10))
    if (httr::status_code(response) == 200) {
      raw_content <- rawToChar(response$content)
      if (nchar(raw_content) > 0) {
        subject_structure <- jsonlite::fromJSON(raw_content)
        if ("dataElements" %in% names(subject_structure)) {
          required_metadata <- subject_structure$dataElements[subject_structure$dataElements$required == "Required", ]
          if (nrow(required_metadata) > 0) {
            field_names <- required_metadata$name
            message(sprintf("Found %d required ndar_subject01 elements for template", length(field_names)))
            field_names
          } else {
            stop("No required elements found")
          }
        } else {
          stop("No dataElements in API response")
        }
      } else {
        stop("Empty API response")
      }
    } else {
      stop(sprintf("API request failed with status %d", httr::status_code(response)))
    }
  }, error = function(e) {
    message("Error fetching ndar_subject01 elements: ", e$message)
    message("Using fallback essential fields list")
    c("src_subject_id", "subjectkey", "sex", "interview_age", "interview_date", 
      "phenotype", "site", "race", "handedness", "visit", "week", "redcap_event_name")
  })
  
  # Filter template columns:
  # Just remove explicitly excluded fields
  # The dataframe has already been cleaned up by the nda script and DCC removal logic,
  # so we trust that all remaining fields should be in the template
  excluded_cols <- template_cols[template_cols %in% excluded_from_template]
  filtered_cols <- template_cols[!template_cols %in% excluded_from_template]
  
  template <- template[, filtered_cols, drop = FALSE]
  
  # Report what was filtered
  if (length(excluded_cols) > 0) {
    message(sprintf("Excluded %d fields from submission template: %s", 
                    length(excluded_cols), paste(excluded_cols, collapse = ", ")))
  }

  # Open a connection to overwrite the file
  con <- file(file_path, "w")

  # Split structure name into base name and suffix
  # Check if the name ends with 2 digits
  if (grepl("\\d{2}$", structure_name)) {
    structure_short_name <- substr(structure_name, 1, nchar(structure_name) - 2)
    structure_suffix <- substr(structure_name, nchar(structure_name) - 1, nchar(structure_name))
  } else {
    # If no suffix, use whole name and "01" as default
    structure_short_name <- structure_name
    structure_suffix <- "01"
  }

  # Remove columns with NA names BEFORE writing anything (prevents parser warnings)
  valid_cols <- !is.na(names(template))
  if (!all(valid_cols)) {
    invalid_count <- sum(!valid_cols)
    if (invalid_count > 0) {
      message(sprintf("Note: Removing %d column(s) with invalid (NA) names", invalid_count))
    }
    template <- template[, valid_cols, drop = FALSE]
  }

  # Write the line with separated components
  writeLines(paste0(structure_short_name, ",", structure_suffix), con)

  # Write column headers manually (now with NA columns already removed)
  writeLines(paste(names(template), collapse = ","), con)

  # Close the connection to save changes
  close(con)

  # Append the data without column headers
  write.table(template, file_path, row.names = FALSE, col.names = FALSE, append = TRUE,
              quote = TRUE, sep = ",", na = "")

  message(sprintf("\nSubmission Template created at: %s \n", file_path))

  return(invisible(TRUE))
}

#' Alias for 'to.nda'
#'
#' This is a legacy alias for the 'to.nda' function to maintain compatibility with older code.
#'
#' @inheritParams to.nda
#' @inherit to.nda return
#' @noRd
#' @examples
#' \dontrun{
#' createNda(prl01)
#' }
createNdaSubmissionTemplate <- to.nda
