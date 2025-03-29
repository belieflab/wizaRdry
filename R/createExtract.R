#' Create Data Extracts in Various Formats
#'
#' This function creates data extracts from a given dataframe in one or more of 
#' the following formats: CSV, RDS, and SPSS.
#' It sources the necessary functions for creating each file type from their 
#' respective scripts and executes them based on the arguments passed. The user 
#' specifies the desired output formats using boolean flags for each file type.
#'
#' @param df The dataframe to be exported.
#' @param df_name The base name for the output file(s) without extension.
#' @param csv Logical; if TRUE, a CSV file is created using the `createCsv` function.
#' @param rdata Logical; if TRUE, an RDS file is created using the `createRda` function.
#' @param spss Logical; if TRUE, an SPSS file is created using the `createSpss` function.
#' @examples
#' createExtract(mtcars, "mtcars_export", csv = TRUE, rds = TRUE, spss = FALSE)
#' @noRd
#' @author Joshua Kenney <joshua.kenney@yale.edu>

createExtract <- function(df, df_name, csv=NULL, rdata=NULL, spss=NULL) {
  
  
  if (!is.null(csv) && csv) {
    createCsv(df, df_name)
  }
  if (!is.null(rdata) && rdata) {
    createRda(df, df_name)
  }
  if (!is.null(spss) && spss) {
    createSpss(df, df_name)
  }
}
