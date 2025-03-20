#' Check Interview Age Validity
#'
#' This function checks whether the 'interview_age' for all records in a specified dataset
#' falls within the acceptable age range (12 to 70 years, converted into months).
#' If any record falls outside this range, the function will list the subject IDs that do not meet this condition.
#'
#' @param measure_alias A string representing the name of the dataset to check.
#'        The dataset should have a column 'interview_age' and 'src_subject_id'.
#' @return The function itself does not return a value but will output a message listing the subject IDs
#'         with 'interview_age' outside the 12 to 70 years range if such cases exist.
#' @export
#' @examples
#' \dontrun{
#' checkInterviewAge("your_dataset_alias")
#' }
#' @importFrom testthat test_that expect_true
#' @note This function requires the 'testthat' package for unit testing.
#'       It is designed to operate on datasets that follow a specific naming convention,
#'       appending '_clean' to the measure_alias to construct the dataframe name.
#'       The function throws an error if the 'interview_age' falls outside the specified range.
#' @noRd
checkInterviewAge <- function(measure_alias) {
  
#   if (!require(testthat)) {install.packages("testthat")}; library(testthat)
  
  months_in_a_year <- 12
  # define age range
  min_age <- 12 * months_in_a_year  # 144 months
  max_age <- 70 * months_in_a_year  # 840 months
  
  # Construct the expected dataframe name
  output_df_name <- paste0(measure_alias, "_clean")
  
  # Retrieve the dataframe based on constructed name
  df_clean <- base::get(output_df_name)  # specify the environment if needed
  
  # age checker
  rows_not_meeting_condition <- df_clean$src_subject_id[df_clean$interview_age < min_age | df_clean$interview_age > max_age]
  
  tryCatch({
    # Perform tests
    test_that(paste0("Check interview_age is between ", min_age, " and ", max_age), {
      testthat::expect_true(
        all(df_clean$interview_age >= min_age & df_clean$interview_age <= max_age),
        info = paste("DATA ERROR: All values in 'interview_age' should be greater than ", min_age, " and less than ", max_age, ". src_subject_id not meeting condition:", paste(rows_not_meeting_condition, collapse = ", "))
      )
    })
  }, error = function(e) {
    message("The following subjects have out of range ages:",rows_not_meeting_condition, e$message)
  })
}
