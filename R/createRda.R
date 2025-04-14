#' Create RDS File from a DataFrame
#'
#' This function exports a given R DataFrame to an RDS file format.
#' The resulting file is saved in the "./tmp/" directory. If a filename
#' is not specified, the function uses the name of the DataFrame variable.
#' The ".Rda" extension is appended automatically to the filename.
#'
#' @param df DataFrame to be exported to RDS format.
#' @param df_name Optional; a custom file name for the saved RDS file.
#'   If not provided, the name of the DataFrame variable is used.
#'   The function adds the ".Rda" extension automatically.
#' @return The function writes an RDS file to the specified path and prints a message
#'   indicating the file's location. This function does not return a value.
#' @examples
#' \dontrun{
#' # Create a sample dataframe
#' sample_df <- data.frame(
#'   id = 1:3,
#'   name = c("Alice", "Bob", "Charlie")
#' )
#' to.rda(sample_df)
#' }
#' @export
#' @author Joshua Kenney <joshua.kenney@yale.edu>
to.rda <- function(df, df_name = NULL) {
  if(is.null(df) || nrow(df) == 0) {
    stop("DataFrame is empty or NULL. Cannot save to RDS.")
  }
  
  filename <- if (!is.null(df_name)) {
    df_name
  } else {
    deparse(substitute(df))
  }
  if (!dir.exists("tmp")) {
    dir.create("tmp")
  }
  path <- paste0("./tmp/", filename, '.Rds')
  
  saveRDS(df, file = path)
  
  message(paste0("Extract created at ", path, "\n"))
}

#' Alias for 'to.rda'
#'
#' This is a legacy alias for the 'to.rda' function to maintain compatibility with older code.
#'
#' @inheritParams to.rda
#' @inherit to.rda return
#' @export
#' @examples
#' \dontrun{
#' createRda(prl01)
#' }
createRda <- to.rda

