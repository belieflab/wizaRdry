#' Check for Presence of Key Variables in a Data Frame
#' 
#' This function checks whether specified variables exist as column names in a given data frame.
#' It reports which variables are present and which are missing, and returns the list of 
#' present variables for further processing.
#' 
#' @param df_name Character string. The name of the data frame to check (must exist in the global environment)
#' @param variables Character vector. Names of the variables (columns) to check for in the data frame
#' @param type Character string. A descriptor of the data frame type, used in output messages (default: "data")
#' 
#' @return Character vector containing the names of the variables that are present in the data frame
#' 
#' @details 
#' The function retrieves the data frame from the global environment using \code{base::get()}.
#' It then compares the specified variables with the column names of the data frame.
#' The function prints messages about which variables are present and which are missing.
#' Unlike earlier versions that would stop execution if variables were missing, this version 
#' continues execution and simply reports the missing variables.
#' 
#' @examples
#' # Create a sample data frame
#' test_df <- data.frame(id = 1:5, name = letters[1:5], value = rnorm(5))
#' 
#' # Check for the presence of specific keys
#' present_vars <- checkKeys("test_df", c("id", "name", "missing_var"), type = "test")
#' 
#' # Use the returned present variables for further operations
#' if("id" %in% present_vars) {
#'   # Proceed with operations requiring the 'id' column
#' }
#' 
#' @noRd
checkKeys <- function(df_name, variables, type = "data") {
  # Attempt to retrieve the dataframe from the global environment
  df <- base::get(df_name)
  
  # Find any missing variables
  missing_vars <- setdiff(variables, names(df))
  # Find present variables
  present_vars <- intersect(variables, names(df))
  
  # Print present variables
  if (length(present_vars) > 0) {
    message(paste("Candidate keys present in ", type, " data of ", df_name, ": ", paste(present_vars, collapse=", "), "."))
  }
  
  # Handle missing variables with tryCatch
  tryCatch({
    if (length(missing_vars) > 0) {
      # Instead of stopping, just print the message about missing variables
      message(paste("Missing variables in", type, "data of", df_name, ":", paste(missing_vars, collapse=", "), ". Merging on present variables:", paste(present_vars, collapse=", "), "."))
    } else {
      # Only print this message if there are no missing variables
      message(paste("All required variables are present in ", type, " data of ", df_name, "."))
    }
  }, error = function(e) {
    message("An unexpected error occurred: ", e$message)
  })
  # Return the list of present variables for further checks
  return(present_vars)
}
