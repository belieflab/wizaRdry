#' Add Prefix to Column Names Based on DataFrame Name
#'
#' This function adds a prefix to column names derived from the dataframe name (excluding "_clean").
#' It preserves standard identifier and metadata columns by excluding them from the prefixing operation.
#' This is useful for maintaining clear variable origins when merging multiple datasets.
#'
#' @param df A data frame whose columns need to be prefixed
#' @param excluded_cols Optional character vector of column names that should not be prefixed.
#'        Defaults to standard NDA and identifier columns.
#'
#' @return A data frame with prefixed column names
#'
#' @details
#' The function:
#' 1. Extracts a prefix from the data frame name (removing "_clean" suffix if present)
#' 2. Only adds the prefix to columns that don't already have it
#' 3. Preserves standard identifier/metadata columns untouched
#'
#' @examples
#' \donttest{
#' # Create sample data
#' social_prl_clean <- data.frame(
#'   src_subject_id = c("S001", "S002"),
#'   trial = c(1, 2),
#'   rt = c(450, 520),
#'   accuracy = c(1, 0)
#' )
#'
#' # Add prefix to column names
#' social_prl_clean <- addColumnPrefix(social_prl_clean)
#'
#' # Result will have columns:
#' # "src_subject_id", "social_prl_trial", "social_prl_rt", "social_prl_accuracy"
#' }
#'
#' @noRd
addColumnPrefix <- function(df) {
  # Determine the prefix from the dataframe name, excluding "_clean"
  prefix <- sub("_clean$", "", deparse(substitute(df)))
  
  # Specify columns that should not have the prefix added
  excluded_cols <- c("src_subject_id", "visit", "phenotype", "interview_age", "interview_date", "site", "sex", "subjectkey", "arm")
  
  # Identify columns that are not in the excluded list and don't already start with the prefix
  cols_to_prefix <- names(df)[!names(df) %in% excluded_cols & !startsWith(names(df), prefix)]
  
  # Loop through each column name that needs the prefix added
  new_names <- setNames(names(df), names(df))
  for (col in cols_to_prefix) {
    new_names[col] <- paste0(prefix, "_", col)
  }
  
  # Rename the columns in the dataframe
  names(df) <- new_names
  
  return(df)
}

# Example of usage:
# Assuming `social_prl_clean` is your dataframe
# social_prl_clean <- addColumnPrefix(social_prl_clean)
