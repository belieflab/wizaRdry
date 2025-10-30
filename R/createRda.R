#' Create .rds file from a data frame
#'
#' This function exports a given R data frame to an RDS file format.
#' The resulting file is saved in the "tmp" directory. If a filename
#' is not specified, the function uses the name of the data frame variable.
#' The ".rds" extension is appended automatically to the filename.
#' The function will prompt for confirmation before creating the file,
#' with an option to remember the user's preference for future calls.
#'
#' @param df Data frame to be exported to RDS format.
#' @param df_name Optional; a custom file name for the saved RDS file.
#'   If not provided, the name of the data frame variable is used.
#'   The function adds the ".rds" extension automatically.
#' @param path Character string specifying the directory path where the "tmp" folder
#'   and RDS file should be created. Defaults to the current working directory.
#' @param skip_prompt Logical. If TRUE (default), skips the confirmation prompt. If FALSE,
#'   will prompt for confirmation unless the user has previously chosen to remember their preference.
#'
#' @return Invisible TRUE if successful. The function writes an RDS file to the specified path
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
#' to.rds(sample_df)
#'
#' # Custom filename
#' to.rds(sample_df, "participants_data")
#'
#' # Skip the confirmation prompt
#' to.rds(sample_df, skip_prompt = TRUE)
#'
#' # Save in a different directory
#' to.rds(sample_df, path = "path/to/project")
#' }
#'
#' @export
to.rds <- function(df, df_name = NULL, path = ".", skip_prompt = TRUE) { #default skip_prompt set to true so user can flag FALSE if needed
  # Get the name of the data frame for display
  df_display_name <- if (!is.null(df_name)) {
    df_name
  } else {
    deparse(substitute(df))
  }

  # Check for user preferences file
  user_prefs_file <- file.path(path, ".wizaRdry_prefs")
  user_prefs <- list(shown_tree = FALSE, auto_create = FALSE, auto_clean = FALSE, auto_nda = FALSE,
                     auto_nda_template = FALSE, auto_csv = FALSE, auto_rds = FALSE)

  if (file.exists(user_prefs_file)) {
    tryCatch({
      user_prefs <- readRDS(user_prefs_file)
      # Add the auto_rds field if it doesn't exist
      if (is.null(user_prefs$auto_rds)) {
        user_prefs$auto_rds <- FALSE
      }
    }, error = function(e) {
      # If file exists but can't be read, create a new one
      user_prefs <- list(shown_tree = FALSE, auto_create = FALSE, auto_clean = FALSE, auto_nda = FALSE,
                         auto_nda_template = FALSE, auto_csv = FALSE, auto_rds = FALSE)
    })
  }

  # If skip_prompt is TRUE or user has previously set auto_rds to TRUE, bypass the prompt
  if (!skip_prompt | !user_prefs$auto_rds) {
    response <- readline(prompt = sprintf("Would you like to create the R data file for %s now? y/n ",
                                          df_display_name))

    while (!tolower(response) %in% c("y", "n")) {
      response <- readline(prompt = "Please enter either y or n: ")
    }

    # Save preference to yes/TRUE in auto_rds if response is 'y'
    if (tolower(response) == "y") {
      user_prefs$auto_rds <- TRUE
      saveRDS(user_prefs, user_prefs_file)
    }

    if (tolower(response) == "n") {
      message(".rds creation cancelled.")
      invokeRestart("abort")  # This exits without the "Error:" prefix
    }
  }

  # Validate the data frame
  if(is.null(df) || nrow(df) == 0) {
    stop("Data frame is empty or NULL. Cannot save to RDS.")
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
  file_path <- file.path(tmp_path, paste0(filename, '.rds'))

  # Save the data frame to an RDS file
  saveRDS(df, file = file_path)

  # Notify user of file creation
  message(paste0("Extract created at ", file_path, "\n"))

  return(invisible(TRUE))
}

#' Alias for 'to.rds' (DEPRECATED)
#'
#' This function is deprecated. Please use 'to.rds' instead.
#' This is a legacy alias for the 'to.rds' function to maintain compatibility with older code.
#'
#' @param ... Additional arguments passed through to \code{to.rds()}.
#' @inherit to.rds return
#' @export
#' @examples
#' \dontrun{
#' # DEPRECATED - use to.rds() instead
#' createRds(prl01)
#' }
createRds <- function(...) {
  .Deprecated("to.rds", package = "wizaRdry")
  to.rds(...)
}
