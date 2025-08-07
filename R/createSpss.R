#' Create .sav SPSS file from a data frame
#'
#' This function takes a R data frame and writes it to an SPSS file
#' using the Haven package. The resulting file will be stored in the
#' "tmp" directory with a default name derived from the data frame variable name,
#' but can be customized if desired. The function will prompt for confirmation
#' before creating the file, with an option to remember the user's preference for future calls.
#'
#' @param df Data frame to be exported to SPSS format.
#' @param df_name Optional; custom file name for the saved SPSS file. If not provided,
#'   the name of the data frame variable will be used. The ".sav" extension will
#'   be appended automatically.
#' @param path Character string specifying the directory path where the "tmp" folder
#'   and SPSS file should be created. Defaults to the current working directory.
#' @param skip_prompt Logical. If TRUE (default), skips the confirmation prompt. If FALSE,
#'   will prompt for confirmation unless the user has previously chosen to remember their preference.
#'
#' @return Invisible TRUE if successful. Writes an SPSS file to the designated path and prints a message indicating
#'   the file's location.
#'
#' @examples
#' \dontrun{
#' # Create a sample data frame
#' sample_df <- data.frame(
#'   id = 1:3,
#'   score = c(85, 92, 78),
#'   group = c("A", "B", "A")
#' )
#'
#' # Basic usage with prompt
#' to.sav(sample_df)
#'
#' # Custom filename
#' to.sav(sample_df, "participants_data")
#'
#' # Skip the confirmation prompt
#' to.sav(sample_df, skip_prompt = TRUE)
#'
#' # Save in a different directory
#' to.sav(sample_df, path = "path/to/project")
#' }
#'
#' @import haven
#' @export
to.sav <- function(df, df_name = NULL, path = ".", skip_prompt = TRUE) { # skip_prompt set to TRUE so user can flag FALSE if needed
  # Get the name of the data frame for display
  df_display_name <- if (!is.null(df_name)) {
    df_name
  } else {
    deparse(substitute(df))
  }

  # Check for user preferences file
  user_prefs_file <- file.path(path, ".wizaRdry_prefs")
  user_prefs <- list(shown_tree = FALSE, auto_create = FALSE, auto_clean = FALSE, auto_nda = FALSE,
                     auto_nda_template = FALSE, auto_csv = FALSE, auto_rds = FALSE, auto_sav = FALSE)

  if (file.exists(user_prefs_file)) {
    tryCatch({
      user_prefs <- readRDS(user_prefs_file)
      # Add the auto_sav field if it doesn't exist
      if (is.null(user_prefs$auto_sav)) {
        user_prefs$auto_sav <- FALSE
      }
    }, error = function(e) {
      # If file exists but can't be read, create a new one
      user_prefs <- list(shown_tree = FALSE, auto_create = FALSE, auto_clean = FALSE, auto_nda = FALSE,
                         auto_nda_template = FALSE, auto_csv = FALSE, auto_rds = FALSE, auto_sav = FALSE)
    })
  }

  # If skip_prompt is TRUE or user has previously set auto_sav to TRUE, bypass the prompt
  if (!skip_prompt | !user_prefs$auto_sav) {
    response <- readline(prompt = sprintf("Would you like to create the SPSS data file for %s now? y/n ",
                                          df_display_name))

    while (!tolower(response) %in% c("y", "n")) {
      response <- readline(prompt = "Please enter either y or n: ")
    }

    # If response is 'y', save their preference as yes for later and update auto_sav to TRUE
    if (tolower(response) == "y") {
      user_prefs$auto_sav <- TRUE
      saveRDS(user_prefs, user_prefs_file)
    }

    if (tolower(response) == "n") {
      message(".sav creation cancelled.")
      invokeRestart("abort")  # This exits without the "Error:" prefix
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
  file_path <- file.path(tmp_path, paste0(filename, '.sav'))

  # Write the data frame to an SPSS file
  write_sav(df, file_path)

  # Notify user of file creation
  message(paste0("Extract created at ", file_path, "\n"))

  return(invisible(TRUE))
}

#' Alias for 'to.sav'
#'
#' This is a legacy alias for the 'to.sav' function to maintain compatibility with older code.
#'
#' @inheritParams to.sav
#' @inherit to.sav return
#' @export
#' @examples
#' \dontrun{
#' createSpss(prl01)
#' }
createSpss <- to.sav


