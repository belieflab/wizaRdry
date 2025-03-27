#' Check Column Naming Conventions Against NDA Requirements
#'
#' This function checks that all non-NDA columns in a dataset follow a specified naming convention,
#' prefixed with the measure alias. It is designed to support different types of datasets like Qualtrics and REDCap.
#'
#' @param measure_alias A string representing the prefix expected for non-NDA columns.
#' @param measure_type A string specifying the type of dataset, supports 'qualtrics' or 'redcap'.
#' @param nda_required_variables A vector of strings listing the NDA required variables which are excluded from the naming convention check.
#' @return The function does not return a value but outputs an error message if any non-NDA columns do not follow the naming convention.
#' @examples
#' \dontrun{
#' checkColumnPrefix("dataset_name", "qualtrics", c("nda_var1", "nda_var2"))
#' }
#' @importFrom testthat test_that expect_true
#' @importFrom dplyr setdiff
#' @note This function assumes that the dataset follows a specific naming convention where non-NDA columns should be prefixed with the measure alias followed by an underscore.
#'       The actual dataset name is expected to be the measure alias suffixed with '_clean'.
#' @noRd
checkColumnPrefix <- function(measure_alias, measure_type, nda_required_variables) {
  
#   if (!require(testthat)) {install.packages("testthat")}; library(testthat)
#   if (!require(dplyr)) {install.packages("dplyr")}; library(dplyr)
  
  # Construct dataframe name based on measure_alias
  df_name <- paste0(measure_alias, "_clean")
  
  # Fetch the dataframe
  df <- base::get(df_name)
  
  # Determine columns to check based on measure_type
  if (measure_type == "qualtrics") {
    non_nda_cols <- dplyr::setdiff(colnames(df), c(nda_required_variables, "ResponseId"))
  } else if (measure_type=="redcap") {
    non_nda_cols <- dplyr::setdiff(colnames(df), c(nda_required_variables, "int_start", 
                                            "int_end"))
  } else {
    non_nda_cols <- dplyr::setdiff(colnames(df), nda_required_variables)
  }
  
  tryCatch({
    # Check naming convention for non-NDA columns
    test_that("Check column naming convention", {
      actual_non_conform <- non_nda_cols[!grepl(paste0("^", measure_alias, "_"), non_nda_cols)]
      is_conforming <- length(actual_non_conform) == 0
      expect_true(
        is_conforming,
        info = paste0("SCRIPT ERROR: The following non-NDA columns in '", df_name, 
                     "' do not follow the correct naming convention starting with '", measure_alias, "_':\n",
                     paste(actual_non_conform, collapse = ", "))
      )
    })
  }, error = function(e) {
    message(e$message)
  })
}
