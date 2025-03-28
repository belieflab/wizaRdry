#' Check for Duplicates in Qualtrics Data
#'
#' This function checks for duplicate records in a specified Qualtrics dataset
#' based on certain identifiers and time points. If duplicates are found,
#' they are exported to a CSV file for review and additionally displayed in the viewer.
#'
#' @param measure_alias A string representing the name of the dataset to check for duplicates.
#' @param measure_type A string specifying the type of measure, currently supports 'qualtrics' only.
#' @return The function does not return a value but will generate a CSV file if duplicates are found
#'         and display those duplicates in the RStudio viewer.
#' @examples
#' \donttest{
#' checkDuplicates("your_dataset_alias", "qualtrics")
#' }
#' @importFrom dplyr filter %>% 
#' @importFrom testthat test_that expect_true
#' @note This function requires the dplyr and testthat packages. It is specifically designed for
#'       Qualtrics data and expects the data frame to be named with a '_clean' suffix.
#'       It checks for duplicates based on 'src_subject_id' combined with 'visit' or 'week' columns.
#'       The function will stop and throw an error if the necessary columns are not present.
#' @noRd
ndaCheckQualtricsDuplicates <- function(measure_alias, measure_type) {
  
  # Ensure required packages are loaded
#   if (!require(dplyr)) { install.packages("dplyr") }; library(dplyr)
#   if (!require(testthat)) { install.packages("testthat") }; library(testthat)
  
  identifier <- "src_subject_id"
  
  df <- base::get0(measure_alias)
  
  if (measure_type == "qualtrics") {
    if (!(identifier %in% colnames(df))) {
      stop("Please provide a valid identifier: src_subject_id, workerId, PROLIFIC_PID")
    }
    
    for (col in c("visit", "week")) {
      if (col %in% base::colnames(df)) {
        df$duplicates <- base::duplicated(df[c(identifier, col)], first = TRUE)
        df_dup_ids <- base::subset(df, duplicates == TRUE)[, c(identifier, col)]
        
        if (base::nrow(df_dup_ids) > 0) {
          # Using inner_join to filter duplicates correctly
          df_duplicates <- df %>% 
            dplyr::inner_join(df_dup_ids, by = c(identifier, col))
          
          if (base::nrow(df_duplicates) > 0) {
            # Export and create a CSV file if duplicates found
            duplicate_extract <- paste0("duplicates_", measure_alias)
            createCsv(df_duplicates, paste0("duplicates_", measure_alias))
            
            tryCatch({
              testthat::test_that("Check for Qualtrics duplicates", {
                testthat::expect_true(base::nrow(df_duplicates) == 0, 
                                      info = paste("DATA ERROR: Duplicates detected in '", measure_alias, "': ", 
                                                   "Offending IDs: ", toString(base::unique(df_duplicates[[identifier]]))))
              })
            }, error = function(e) {
              duplicates_summary <- toString(base::unique(df_duplicates[[identifier]]))
              message(paste("Error in testing for duplicates in '", measure_alias, "': ", e$message, 
                            "\nOffending IDs: ", duplicates_summary,
                            "\nDetails exported to ", paste0("./tmp/",duplicate_extract,".csv")))
            })
            
            # Optionally view the offending records in RStudio's data viewer
            View(df_duplicates)
          }
        }
      }
    }
  }
}
