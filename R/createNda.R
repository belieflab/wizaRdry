#' Create NDA Submission Template
#'
#' This function creates a CSV template file for National Data Archive (NDA) submissions.
#' It extracts the data from a specified dataframe and formats it according to NDA requirements,
#' with the structure name split into base name and suffix in the first line.
#'
#' @param df Character string specifying the name of the dataframe to be used as template.
#'        This dataframe should exist in the global environment.
#'
#' @return No return value, called for side effects. Creates a CSV file at the specified path
#'         and prints a message with the file location.
#'
#' @details 
#' The function will:
#' 1. Create an 'nda/tmp' directory structure if it doesn't exist
#' 2. Parse the structure name into base and suffix components (e.g., "eefrt01" → "eefrt" and "01")
#' 3. Write the structure name components as the first line
#' 4. Write column headers as the second line
#' 5. Write the data rows below
#'
#' @note This function expects the dataframe to already exist in the global environment.
#'       It will not perform any data validation or transformation before creating the template.
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
#'   # Create the NDA template
#'   createNda("eefrt01")
#' }
#'
#' @export
createNda <- function(df) {
  if (!dir.exists("nda")) {
    dir.create("nda")
  }
  
  # Define structure_name explicitly
  structure_name <- df  # Assuming '01' is static
  
  # Create the file path
  path <- file.path('nda/tmp', paste0(df, '_template.csv'))
  
  # Get the dataframe
  template <- base::get(df, envir = .GlobalEnv)
  
  # Open a connection to overwrite the file
  con <- file(path, "w")
  
  # Split structure name into base name and suffix
  structure_short_name <- substr(structure_name, 1, nchar(structure_name) - 2)  # gets "eefrt"
  structure_suffix <- substr(structure_name, nchar(structure_name) - 1, nchar(structure_name))  # gets "01"
  
  # Write the line with separated components
  writeLines(paste0(structure_short_name, ",", structure_suffix), con)
  
  # Write column headers manually
  writeLines(paste(names(template), collapse = ","), con)
  
  # Close the connection to save changes
  close(con)
  
  # Append the data without column headers
  write.table(template, path, row.names = FALSE, col.names = FALSE, append = TRUE, 
              quote = TRUE, sep = ",", na = "")
  
  message(paste0("\nSubmission Template created at: ", path, "\n"))
}
