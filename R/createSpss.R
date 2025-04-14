#' Create SPSS file from a DataFrame
#'
#' This function takes a R DataFrame and writes it to an SPSS file
#' using the Haven package. The resulting file will be stored in the
#' "./tmp/" directory with a default name derived from the DataFrame variable name,
#' but can be customized if desired.
#'
#' @param df DataFrame to be exported to SPSS format.
#' @param df_name Optional; custom file name for the saved SPSS file. If not provided,
#'   the name of the DataFrame variable will be used. The ".sav" extension will
#'   be appended automatically.
#' @return Writes an SPSS file to the designated path and prints a message indicating
#'   the file's location. This function does not return any value.
#' @examples
#' \dontrun{
#' # Create a sample dataframe
#' sample_df <- data.frame(
#'   id = 1:3,
#'   score = c(85, 92, 78),
#'   group = c("A", "B", "A")
#' )
#' to.sav(sample_df)
#' }
#' @import haven
#' @export
#' @author Joshua Kenney <joshua.kenney@yale.edu>
to.sav <- function(df, df_name = NULL) {
  
  
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
  path <- paste0("./tmp/", filename, '.sav')
  
  # Write the DataFrame to an SPSS file
  write_sav(df, path)
  
  # Notify user of file creation
  message(paste0("Extract created at ", path, "\n"))
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


