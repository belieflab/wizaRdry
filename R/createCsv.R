#' Create .csv file from a data frame
#'
#' This function exports a given R data frame to a CSV file format.
#' The resulting file is saved in the "tmp" directory. If a filename
#' is not specified, the function uses the name of the data frame variable.
#' The ".csv" extension is appended automatically to the filename.
#' The function will prompt for confirmation before creating the file,
#' with an option to remember the user's preference for future calls.
#'
#' @param df Data frame to be exported to CSV format.
#' @param df_name Optional; a custom file name for the saved CSV file.
#'   If not provided, the name of the data frame variable is used.
#'   The function adds the ".csv" extension automatically.
#' @param path Character string specifying the directory path where the "tmp" folder
#'   and CSV file should be created. Defaults to the current working directory.
#' @param skip_prompt Logical. If TRUE, skips the confirmation prompt. If FALSE (default),
#'   will prompt for confirmation unless the user has previously chosen to remember their preference.
#'
#' @return Invisible TRUE if successful. The function writes a CSV file to the specified path
#'   and prints a message indicating the file's location.
#'
#' @examples
#' \dontrun{
#' # Create a sample data frame
#' sample_df <- data.frame(
#'   id = 1:3,
#'   name = c("Alice", "Bob", "Charlie")
#' )
#'
#' # Basic usage with prompt
#' to.csv(sample_df)
#'
#' # Custom filename
#' to.csv(sample_df, "participants_data")
#'
#' # Skip the confirmation prompt
#' to.csv(sample_df, skip_prompt = TRUE)
#'
#' # Save in a different directory
#' to.csv(sample_df, path = "path/to/project")
#' }
#'
#' @export
#' @author Joshua Kenney <joshua.kenney@yale.edu>
to.csv <- function(df, df_name = NULL, path = ".", skip_prompt = FALSE) {
  # Get the name of the dataframe for display
  df_display_name <- if (!is.null(df_name)) {
    df_name
  } else {
    deparse(substitute(df))
  }
  
  # Check for user preferences file
  user_prefs_file <- file.path(path, "..wizaRdry_prefs")
  user_prefs <- list(shown_tree = FALSE, auto_create = FALSE, auto_clean = FALSE, auto_nda = FALSE, auto_csv = FALSE)
  
  if (file.exists(user_prefs_file)) {
    tryCatch({
      user_prefs <- readRDS(user_prefs_file)
      # Add the auto_csv field if it doesn't exist
      if (is.null(user_prefs$auto_csv)) {
        user_prefs$auto_csv <- FALSE
      }
    }, error = function(e) {
      # If file exists but can't be read, create a new one
      user_prefs <- list(shown_tree = FALSE, auto_create = FALSE, auto_clean = FALSE, auto_nda = FALSE, auto_csv = FALSE)
    })
  }
  
  # If skip_prompt is TRUE or user has previously set auto_csv to TRUE, bypass the prompt
  if (!skip_prompt && !user_prefs$auto_csv) {
    response <- readline(prompt = sprintf("Would you like to create the csv for %s now? y/n ",
                                          df_display_name))
    
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
        user_prefs$auto_csv <- TRUE
        saveRDS(user_prefs, user_prefs_file)
        message("Your preference has been saved. Use to.csv(skip_prompt = FALSE) to show this prompt again.")
      }
    }
    
    if (tolower(response) == "n") {
      # Instead of stopping with an error, return invisibly
      return(invisible(NULL))
    }
  }
  
  # Use df_name if provided, otherwise derive from df variable name
  filename <- if (!is.null(df_name)) {
    df_name
  } else {
    deparse(substitute(df))
  }
  
  # Create tmp directory if it doesn't exist
  tmp_path <- file.path(path, "tmp")
  if (!dir.exists(tmp_path)) {
    dir.create(tmp_path)
  }
  
  # Construct the file path
  file_path <- file.path(tmp_path, paste0(filename, '.csv'))
  
  # Write the DataFrame to a CSV file
  write.csv(df, file_path, row.names = FALSE, quote = TRUE)
  
  # Notify user of file creation
  message(paste0("Extract created at ", file_path, "\n"))
  
  return(invisible(TRUE))
}

#' Alias for 'to.csv'
#'
#' This is a legacy alias for the 'to.csv' function to maintain compatibility with older code.
#'
#' @inheritParams to.csv
#' @inherit to.csv return
#' @export
#' @examples
#' \dontrun{
#' createCsv(prl01)
#' }
createCsv <- to.csv

