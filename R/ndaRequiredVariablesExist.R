#' Check for Presence of NDA Required Variables in a Data Frame
#'
#' This function checks if a cleaned data frame contains all the variables required by the National Data Archive (NDA).
#' The set of required variables can be adjusted based on the specific requirements of the study and the presence
#' of certain variables like 'visit' or 'week'. The function is useful for ensuring data compliance before submission.
#'
#' @param measure_alias A string representing the alias name of the dataset to be checked.
#' @param measure_type A string indicating the type of measure, used to adjust the list of required variables if necessary.
#' @param nda_required_variables A vector of strings representing the initial set of NDA required variables to check for.
#'        This parameter is overwritten inside the function but can be used to extend the functionality in the future.
#' @return This function does not return a value but uses the `testthat` package to assert the presence of all NDA required variables and provides detailed feedback if any are missing.
#' @examples
#' \donttest{
#' ndaRequiredVariablesExist("your_dataset_alias", "qualtrics", c("src_subject_id", "phenotype"))
#' }
#' @importFrom testthat test_that expect_true
#' @importFrom dplyr setdiff
#' @note While currently the function overwrites the 'nda_required_variables' parameter internally, future versions may allow for dynamic adjustment based on 'measure_type'.
#'       It assumes that the dataset has been cleaned and is named according to a standard naming convention with a '_clean' suffix.
#' @noRd
ndaRequiredVariablesExist <- function(measure_alias, measure_type, nda_required_variables) {
  
#   if (!require(testthat)) {install.packages("testthat")}; library(testthat)
  
  # append _clean to the measure in question
  output_df_name <- paste0(measure_alias, "_clean")
  
  # store clean dataframe in df_clean
  df_clean <- base::get(output_df_name)
  
  # Initial list of NDA required variables
  nda_required_variables <- c("src_subject_id", "phenotype", "site", 
                              "subjectkey", "sex", "interview_date", "interview_age")
  
  # Adjust NDA required variables based on presence of 'visit' or 'week'
  adjusted_nda_required <- nda_required_variables
  
  # alter required variables for redcap measures (no interview_date or interview_age)
  if (measure_type=="redcap") {
    adjusted_nda_required <- nda_required_variables <- c("src_subject_id", "phenotype", "site", 
                                                         "subjectkey", "sex")
  }
  
  # If 'visit' and 'week' are not both required, adjust the list accordingly:
  if (!("visit" %in% colnames(df_clean)) && ("week" %in% colnames(df_clean))) {
    adjusted_nda_required <- dplyr::setdiff(adjusted_nda_required, "visit")  # Remove 'visit' if it's not there but 'week' is
  } else if (("visit" %in% colnames(df_clean)) && !("week" %in% colnames(df_clean))) {
    adjusted_nda_required <- dplyr::setdiff(adjusted_nda_required, "week")  # Remove 'week' if it's not there but 'visit' is
  } # If neither or both are present, no changes needed to adjusted_nda_required
  
  
  # Now check if the output dataframe contains all adjusted NDA required variables
  missing_vars <- dplyr::setdiff(adjusted_nda_required, colnames(df_clean))
  
  tryCatch({
    test_that("Check for missing NDA required variables", {
      testthat::expect_true(length(missing_vars) == 0, 
                            info = paste("SCRIPT ERROR: All NDA required variables are not present in '", measure_alias, " please make sure the following variable is present in the clean df: '", missing_vars, "."))
    })
  }, error = function(e) {
    message("All NDA required variables are not present in '", measure_alias, " please make sure the following variable is present in the clean df: '", missing_vars, ".", e$message)
  })
  
}
