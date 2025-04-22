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
#' @param skip_prompt Logical. If TRUE, skips the confirmation prompt. If FALSE (default),
#'        will prompt for confirmation unless the user has previously chosen to remember their preference.
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
to.nda <- function(df, path = ".", skip_prompt = FALSE) {
  # Check for user preferences file
  user_prefs_file <- file.path(path, "..wizaRdry_prefs")
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
  if (!skip_prompt && !user_prefs$auto_nda_template) {
    response <- readline(prompt = sprintf("Would you like to create the NDA submission template for %s now? y/n ",
                                          df_name))
    
    while (!tolower(response) %in% c("y", "n")) {
      response <- readline(prompt = "Please enter either y or n: ")
    }
    
    # Ask if they want to remember this choice
    if (tolower(response) == "y") {
      remember <- readline(prompt = "Would you like to remember this choice and skip this prompt in the future? y/n ")
      
      while (!tolower(remember) %in% c("y", "n")) {
        remember <- readline(prompt = "Please enter either y or n: ")
      }
      
      if (tolower(remember) == "y") {
        user_prefs$auto_nda_template <- TRUE
        saveRDS(user_prefs, user_prefs_file)
        message("Your preference has been saved. Use to.nda(skip_prompt = FALSE) to show this prompt again.")
      }
    }
    
    if (tolower(response) == "n") {
      # Instead of stopping with an error, return invisibly
      return(invisible(NULL))
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
  
  # Write the line with separated components
  writeLines(paste0(structure_short_name, ",", structure_suffix), con)
  
  # Write column headers manually
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
createNda <- to.nda
