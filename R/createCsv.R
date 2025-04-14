#' Create CSV File from a DataFrame
#'
#' This function exports a given R DataFrame to a CSV file format. 
#' The resulting file is saved in the "./tmp/" directory. If a filename 
#' is not specified, the function uses the name of the DataFrame variable. 
#' The ".csv" extension is appended automatically to the filename.
#'
#' @param df DataFrame to be exported to CSV format.
#' @param df_name Optional; a custom file name for the saved CSV file. 
#'   If not provided, the name of the DataFrame variable is used. 
#'   The function adds the ".csv" extension automatically.
#' @return The function writes a CSV file to the specified path and prints a message 
#'   indicating the file's location. This function does not return a value.
#' @examples
#' \dontrun{
#' # Create a sample dataframe
#' sample_df <- data.frame(
#'   id = 1:3,
#'   name = c("Alice", "Bob", "Charlie")
#' )
#' to.csv(sample_df)
#' }
#' @export
#' @author Joshua Kenney <joshua.kenney@yale.edu>
to.csv <- function(df, df_name = NULL) {
  # Use df_name if provided, otherwise derive from df variable name
  filename <- if (!is.null(df_name)) {
    df_name
  } else {
    deparse(substitute(df))
  }
  if (!dir.exists("tmp")) {
    dir.create("tmp")
  }
  # Construct the file path
  path <- paste0('./tmp/', filename, '.csv')
  
  # Write the DataFrame to a CSV file
  write.csv(df, path, row.names = FALSE, quote = TRUE)
  
  # Notify user of file creation
  message(paste0("Extract created at ", path, "\n"))
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

