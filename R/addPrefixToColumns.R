#' Add prefix to columns and handle text normalization
#'
#' @param df A data frame
#' @param collection_name The prefix to add
#' @param excluded_cols Columns to exclude from prefixing
#' @param similarity_threshold Threshold for considering a prefix similar (0-1)
#' @param normalize_case Whether to normalize column case (lowercase, etc)
#' @param normalize_type Type of case normalization ("lower", "upper", "title", etc)
#' @return A data frame with prefixed and normalized column names
#' @importFrom stringdist stringdist
#' @noRd
add_prefix_to_columns <- function(df, collection_name, 
                                  excluded_cols = c("src_subject_id", "visit", "phenotype", 
                                                    "interview_age", "interview_date", "site", 
                                                    "sex", "subjectkey", "arm", "handedness", 
                                                    "state", "status"),
                                  similarity_threshold = 0.8,
                                  normalize_case = FALSE,
                                  normalize_type = "lower") {
  
  # Ensure stringdist package is available
  if (!requireNamespace("stringdist", quietly = TRUE)) {
    stop("The stringdist package is required. Please install it with install.packages('stringdist')")
  }
  
  # Helper function to check if a column has a similar prefix
  has_similar_prefix <- function(col_name, prefix) {
    # If the column is too short, it can't have a meaningful prefix
    if (nchar(col_name) < nchar(prefix)) {
      return(FALSE)
    }
    
    # Extract potential prefix (same length as collection_name)
    potential_prefix <- substring(col_name, 1, nchar(prefix))
    
    # Calculate similarity
    similarity <- 1 - stringdist::stringdist(tolower(potential_prefix), 
                                             tolower(prefix), 
                                             method = "jw") / nchar(prefix)
    
    return(similarity >= similarity_threshold)
  }
  
  # Get the current column names
  col_names <- names(df)
  
  # Replace dots with underscores
  col_names <- gsub("\\.", "_", col_names)
  
  # Create a new set of column names
  new_col_names <- sapply(col_names, function(col) {
    # Check if column should be excluded from prefixing
    if (col %in% excluded_cols) {
      return(col)
    }
    
    # Check if the column already has the exact prefix
    if (startsWith(col, paste0(collection_name, "_"))) {
      return(col)
    }
    
    # Check if the column has a similar prefix using string distance
    if (has_similar_prefix(col, collection_name)) {
      return(col)  # Keep as is if it has a similar prefix
    }
    
    # Add the prefix if none of the above conditions are met
    return(paste0(collection_name, "_", col))
  })
  
  # Normalize case if requested
  if (normalize_case) {
    if (normalize_type == "lower") {
      new_col_names <- tolower(new_col_names)
    } else if (normalize_type == "upper") {
      new_col_names <- toupper(new_col_names)
    } else if (normalize_type == "title") {
      # Title case (capitalize first letter of each word)
      new_col_names <- gsub("(^|_)([a-z])", "\\1\\U\\2", tolower(new_col_names), perl = TRUE)
    }
  }
  
  # Rename the columns
  names(df) <- new_col_names
  
  return(df)
}