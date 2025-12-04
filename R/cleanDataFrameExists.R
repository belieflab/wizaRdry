#' Check if a Clean Data Frame Exists
#'
#' This function checks if a cleaned data frame, specified by the measure alias, exists in the global environment.
#' The function is designed to verify the existence of data frames intended to have been cleaned and prepared
#' under a specific naming convention (suffix '_clean').
#'
#' @param measure_alias A string representing the alias name of the dataset to be checked.
#' @param measure_type A string indicating the type of measure, currently not utilized in the function but reserved for future use.
#' @return This function does not return a value but outputs a message indicating whether the specified clean data frame exists.
#' @examples
#' \donttest{
#' cleanDataFrameExists("your_dataset_alias", "qualtrics")
#' }
#' @importFrom testthat test_that expect_true
#' @note This function assumes that the dataset, if cleaned and prepared correctly, has been named according to a standard
#'       naming convention with a '_clean' suffix. The measure_type parameter is included for potential future functionality
#'       but is not currently used.
#' @noRd
cleanDataFrameExists <- function(measure_alias, measure_type) {


  # append _clean to the measure in question
  output_df_name <- paste0(measure_alias, "_clean")

  if (exists(output_df_name)){
    base::cat("Clean data frame exists: ")
    return(invisible(TRUE))
  }
  
  error_msg <- paste("SCRIPT ERROR: The script did not create '", output_df_name, "' dataframe.")
  
  tryCatch({
    test_that("Clean df exists", {
      testthat::expect_true(FALSE, info = error_msg)
    })
  }, error = function(e) {
    message(error_msg)
  })

}
