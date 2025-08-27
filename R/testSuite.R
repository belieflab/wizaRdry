#' Run a Suite of Tests on Dataset
#'
#' This function performs a series of validation tests on a dataset to ensure it meets
#' quality standards and contains required variables for NDA submission.
#'
#' @param measure_alias Character string, the name of the measure to be tested
#' @param measure_type Character string, the type of measure (e.g., "redcap", "qualtrics", "mongo")
#' @param script_path Character string, the path to the script that processes the measure
#' @param super_key Character vector, one or more identifier variables to check for in the dataset
#'
#' @return No return value, called for side effects
#'
#' @details
#' The function performs the following checks:
#' \itemize{
#'   \item Checks for duplicate records in Qualtrics data
#'   \item Verifies that a cleaned data frame exists
#'   \item Checks that all NDA required variables are present
#'   \item Ensures column naming is consistent with measure prefix standards
#'   \item Validates that interview age is within acceptable range
#'   \item Verifies that super key variables are present in both raw and cleaned data
#' }
#'
#' @examples
#' \donttest{
#'   testSuite("rgpts", "qualtrics", "./clean/qualtrics/rgpts.R", "src_subject_id")
#' }
#'
#' @importFrom utils head
#' @noRd
testSuite <- function(measure_alias, measure_type, script_path, super_key) {
  # Get identifier from config
  config <- validate_config()
  identifier <- config$identifier

  # List of NDA required variables
  nda_required_variables <- c("src_subject_id", "phenotype", "site", "arm", "visit", "week",
                              "subjectkey", "sex", "interview_date", "interview_age", "state")

  # Perform generic tests
  checkQualtricsDuplicates(measure_alias, measure_type) # and give allow to View them in a table
  cleanDataFrameExists(measure_alias, measure_type) #checkin_clean x
  checkColumnPrefix(measure_alias, measure_type, nda_required_variables) # checkin_distress


  # perform nda-specific tests
  if (identifier == "src_subject_id") {
    ndaRequiredVariablesExist(measure_alias, measure_type, nda_required_variables) # do Nda req variables exist
    checkInterviewAge(measure_alias) # <240 >860
  }

  if (exists(measure_alias)) {
    message("Raw data found. Looking for super keys...")
    # Check for presence of super_key variables in the raw data
    candidate_keys <- checkKeys(measure_alias, super_key, "raw")
    # Assuming you still want to verify these keys are present in the cleaned data
    if (exists(paste0(measure_alias, "_clean"))) {
      message("Clean data found. Looking for candidate keys... ")
      candidate_keys <- checkKeys(paste0(measure_alias, "_clean"), candidate_keys, "clean")
    }
  }


  # ...add additional functions here, making sure they pass in measure_alias and measure_type
}
