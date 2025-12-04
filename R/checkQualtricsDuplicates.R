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

checkQualtricsDuplicates <- function(measure_alias, measure_type, verbose = TRUE) {

  # Ensure required packages are loaded

  # Generate the name of the dataframe and get it
  output_df_name <- paste0(measure_alias, "_clean")
  df <- base::get(output_df_name)
  
  # possible identifiers
  possible_identifiers <- c("src_subject_id", "workerId", "PROLIFIC_PID", "participantId")

  if (measure_type == "qualtrics") {
    # Find which identifiers exist in the dataframe
    existing_identifiers <- possible_identifiers[possible_identifiers %in% colnames(df)]
    
    if (length(existing_identifiers) == 0) {
      stop("Please provide valid identifiers: src_subject_id, workerId, PROLIFIC_PID, participantId")
    }
    
    # Find the first identifier with non-NA values
    identifier <- NA
    for (key in existing_identifiers) {
      non_na_count <- sum(!is.na(df[[key]]))
      if (non_na_count > 0) {
        identifier <- key
        break
      }
    }
    
    if (is.na(identifier)) {
      stop(paste("No identifier found with non-NA values. Available identifiers:", paste(existing_identifiers, collapse = ", ")))
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

            duplicates_summary <- toString(base::unique(df_duplicates[[identifier]]))
            error_msg <- paste("DATA ERROR: Duplicates detected in '", measure_alias, "': ",
                              "Offending IDs: ", duplicates_summary,
                              "\nDetails exported to ", paste0("./tmp/",duplicate_extract,".csv"))
            
            tryCatch({
              testthat::test_that("Check for Qualtrics duplicates", {
                testthat::expect_true(FALSE, info = error_msg)
              })
            }, error = function(e) {
              message(error_msg)
            })

            # Optionally view the offending records in RStudio's data viewer
            if (verbose) {
              View(df_duplicates, title = paste0("Duplicates: ", measure_alias))
            }
          }
        }
      }
    }
  }
}
