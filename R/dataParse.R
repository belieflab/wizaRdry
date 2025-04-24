#' Parse composite Qualtrics survey into component data frames by variable prefix
#'
#' This function fetches a Qualtrics data frame containing multiple surveys and
#' separates it into individual data frames for each survey detected in the data.
#' It identifies the appropriate identifier column (e.g., participantId, workerId)
#' and splits the data based on column name prefixes.
#'
#' @param qualtrics_alias Character string specifying the Qualtrics survey alias to retrieve.
#' @param label Logical; default TRUE, returns coded values as labels instead of raw values.
#' @param lower default TRUE convert prefixes to lower case
#'
#' @return Creates multiple dataframes in the global environment, one for each survey
#'   detected in the data. Each dataframe is named after its survey prefix.
#'   
#' @details
#' The function performs the following steps:
#' \itemize{
#'   \item Retrieves the raw Qualtrics data using the getSurvey() function
#'   \item Identifies which identifier column to use (participantId, workerId, PROLIFIC_PID, or src_subject_id)
#'   \item Determines survey prefixes by analyzing column names
#'   \item Creates separate dataframes for each survey prefix found
#'   \item Assigns each dataframe to the global environment with names matching the survey prefixes
#' }
#'
#' @examples
#' \dontrun{
#' # Parse a Qualtrics export containing multiple surveys
#' qualtrics.rune("combined_surveys", label = FALSE)
#' 
#' # After running, access individual survey dataframes directly:
#' head(pss)  # Access the PSS survey dataframe
#' head(cesd) # Access the CESD survey dataframe
#' }
#'
#' @importFrom dplyr filter select
#' @export
qualtrics.rune <- function(qualtrics_alias, label = FALSE, lower = TRUE){
  
  df <- qualtrics(qualtrics_alias, label)
  
  # Define potential identifiers
  identifiers <- c("participantId", "workerId", "PROLIFIC_PID", "src_subject_id")
  
  # Filter to keep only existing keys in the dataframe
  existing_keys <- identifiers[identifiers %in% names(df)]
  # Check if any identifiers exist in the dataframe
  if (length(existing_keys) == 0) {
    stop("No valid identifiers found in the dataframe.")
  }
  
  # Print existing identifiers for debugging
  #print("Existing identifiers:")
  #print(existing_keys)
  
  # Find the first identifier with non-NA values
  identifier <- NA
  for (key in existing_keys) {
    non_na_count <- sum(!is.na(df[[key]]))
    
    # Debug print to check how many non-NA values exist in each column
    #print(paste("Checking identifier:", key, "with", non_na_count, "non-NA values"))
    
    if (non_na_count > 0) {  # As long as there's at least 1 non-NA value
      identifier <- key
      break
    }
  }
  
  # If no column has any non-NA values, issue a warning or stop
  if (is.na(identifier)) {
    stop(paste("No identifier found without NA values or multiple identifiers exist:", existing_keys))
  }
  
  # Print the detected identifier for debugging
  message(paste("Detected identifier:", identifier))
  
  # Define common columns to include if they exist
  common_columns <- c(
    "record_id", "subjectkey", "site", "phenotype", "visit", "week",
    "state", "status", "lost_to_followup", "lost_to_follow-up", 
    "interview_age", "interview_date"
  )
  
  # Filter to only include common columns that exist in the original dataframe
  common_columns_exist <- common_columns[common_columns %in% names(df)]
  
  # Exclude non-survey and specific columns from survey prefix detection
  non_survey_columns <- c(existing_keys, common_columns)
  survey_columns <- names(df)[!names(df) %in% non_survey_columns & grepl("_", names(df))]
  
  # Extract unique survey prefixes from survey-specific column names
  extract_first_part <- function(string) {
    parts <- strsplit(string, "_")[[1]]
    return(parts[1])
  }
  survey_prefixes <- unique(sapply(survey_columns, extract_first_part))
  
  # Exclude prefixes that might still be problematic
  excluded_prefixes <- c("PROLIFIC", "interview")
  survey_prefixes <- survey_prefixes[!survey_prefixes %in% excluded_prefixes]
  
  # Create a list of dataframes, one for each survey
  output = list()
  for (prefix in survey_prefixes) {
    survey_specific_columns <- grep(paste0("^", prefix, "_"), names(df), value = TRUE)
    if (length(survey_specific_columns) > 0) {
      # Create subset dataframe with identifier, common columns, and survey-specific columns
      all_columns <- c(identifier, common_columns_exist, survey_specific_columns)
      subset_df <- df[, all_columns, drop = FALSE]
      
      # Apply lowercase transformation if requested
      if (lower) {
        # Always preserve the identifier column name and common columns
        cols_to_transform <- !names(subset_df) %in% c(identifier, common_columns_exist)
        names(subset_df)[cols_to_transform] <- tolower(names(subset_df)[cols_to_transform])
      }
      
      # Add to output list with lowercase prefix as key
      output[[tolower(prefix)]] <- subset_df
    }
  }
  
  names(output) <- tolower(survey_prefixes)
  
  # Use parent.frame() instead of globalenv() for CRAN compliance
  list2env(output, parent.frame())
}

#' Parse composite MongoDB collection into component data frames by variable prefix
#'
#' This function fetches a MongoDB collection containing multiple collections and
#' separates it into individual data frames for each collection detected in the data.
#' It identifies the appropriate identifier column (e.g., participantId, workerId)
#' and splits the data based on column name prefixes.
#'
#' @param collection Character string specifying the Mongo collection
#' @param db_name Character string specifying the Mongo database
#' @param lower default TRUE convert prefixes to lower case
#'
#' @return Creates multiple dataframes in the global environment, one for each survey
#'   detected in the data. Each dataframe is named after its survey prefix.
#'   
#' @details
#' The function performs the following steps:
#' \itemize{
#'   \item Retrieves the raw Qualtrics data using the getSurvey() function
#'   \item Identifies which identifier column to use (participantId, workerId, PROLIFIC_PID, or src_subject_id)
#'   \item Determines survey prefixes by analyzing column names
#'   \item Creates separate dataframes for each survey prefix found
#'   \item Assigns each dataframe to the global environment with names matching the survey prefixes
#' }
#'
#' @examples
#' \dontrun{
#' # Parse a Qualtrics export containing multiple surveys
#' mongo.rune("combined_surveys", label = FALSE)
#' 
#' # After running, access individual survey dataframes directly:
#' head(pss)  # Access the PSS survey dataframe
#' head(cesd) # Access the CESD survey dataframe
#' }
#'
#' @importFrom dplyr filter select
#' @export
mongo.rune <- function(collection, db_name = NULL, lower = TRUE ){
  
  df <- mongo(collection, db_name)
  
  # Define potential identifiers
  identifiers <- c("participantId", "workerId", "PROLIFIC_PID", "src_subject_id")
  
  # Filter to keep only existing keys in the dataframe
  existing_keys <- identifiers[identifiers %in% names(df)]
  
  # Check if any identifiers exist in the dataframe
  if (length(existing_keys) == 0) {
    stop("No valid identifiers found in the dataframe.")
  }
  
  # Print existing identifiers for debugging
  #print("Existing identifiers:")
  #print(existing_keys)
  
  # Find the first identifier with non-NA values
  identifier <- NA
  for (key in existing_keys) {
    non_na_count <- sum(!is.na(df[[key]]))
    
    # Debug print to check how many non-NA values exist in each column
    #print(paste("Checking identifier:", key, "with", non_na_count, "non-NA values"))
    
    if (non_na_count > 0) {  # As long as there's at least 1 non-NA value
      identifier <- key
      break
    }
  }
  
  # If no column has any non-NA values, issue a warning or stop
  if (is.na(identifier)) {
    stop(paste("No identifier found without NA values or multiple identifiers exist:", existing_keys))
  }
  
  # Print the detected identifier for debugging
  message(paste("Detected identifier:", identifier))
  
  # Define common columns to include if they exist
  common_columns <- c(
    "record_id", "subjectkey", "site", "phenotype", "visit", "week",
    "state", "status", "lost_to_followup", "lost_to_follow-up", 
    "interview_age", "interview_date"
  )
  
  # Filter to only include common columns that exist in the original dataframe
  common_columns_exist <- common_columns[common_columns %in% names(df)]
  
  # Exclude non-survey and specific columns from survey prefix detection
  non_survey_columns <- c(existing_keys, common_columns)
  survey_columns <- names(df)[!names(df) %in% non_survey_columns & grepl("_", names(df))]
  
  # Extract unique survey prefixes from survey-specific column names
  extract_first_part <- function(string) {
    parts <- strsplit(string, "_")[[1]]
    return(parts[1])
  }
  survey_prefixes <- unique(sapply(survey_columns, extract_first_part))
  
  # Exclude prefixes that might still be problematic
  excluded_prefixes <- c("PROLIFIC", "interview")
  survey_prefixes <- survey_prefixes[!survey_prefixes %in% excluded_prefixes]
  
  # Create a list of dataframes, one for each survey
  output = list()
  for (prefix in survey_prefixes) {
    survey_specific_columns <- grep(paste0("^", prefix, "_"), names(df), value = TRUE)
    if (length(survey_specific_columns) > 0) {
      # Create subset dataframe with identifier, common columns, and survey-specific columns
      all_columns <- c(identifier, common_columns_exist, survey_specific_columns)
      subset_df <- df[, all_columns, drop = FALSE]
      
      # Apply lowercase transformation if requested
      if (lower) {
        # Always preserve the identifier column name and common columns
        cols_to_transform <- !names(subset_df) %in% c(identifier, common_columns_exist)
        names(subset_df)[cols_to_transform] <- tolower(names(subset_df)[cols_to_transform])
      }
      
      # Add to output list with lowercase prefix as key
      output[[tolower(prefix)]] <- subset_df
    }
  }
  
  names(output) <- tolower(survey_prefixes)
  
  # Use parent.frame() instead of globalenv() for CRAN compliance
  list2env(output, parent.frame())
}

#' Parse composite data frame into component data frames by variable prefix
#'
#' This function takes a data frame containing multiple measures and
#' separates it into individual data frames for each measure detected in the data.
#' It identifies the appropriate identifier column (e.g., participantId, workerId)
#' and splits the data based on column name prefixes.
#'
#' @param df a dataframe containing multiple, prefixed measures
#' @param lower default TRUE convert prefixes to lower case
#' 
#' @return Creates multiple dataframes in the global environment, one for each survey
#'   detected in the data. Each dataframe is named after its survey prefix.
#'   
#' @details
#' The function performs the following steps:
#' \itemize{
#'   \item Identifies which identifier column to use (participantId, workerId, PROLIFIC_PID, or src_subject_id)
#'   \item Determines survey prefixes by analyzing column names
#'   \item Creates separate dataframes for each survey prefix found
#'   \item Assigns each dataframe to the global environment with names matching the survey prefixes
#' }
#'
#' @examples
#' # Parse a data frame containing multiple surveys
#' combined_df <- data.frame(
#'   record_id = c("REC001", "REC002", "REC003", "REC004"),
#'   src_subject_id = c("SUB001", "SUB002", "SUB003", "SUB004"),
#'   subjectkey = c("KEY001", "KEY002", "KEY003", "KEY004"),
#'   site = c("Yale", "NU", "Yale", "NU"),
#'   phenotype = c("A", "B", "A", "C"),
#'   visit = c(1, 2, 2, 1),
#'   state = c("complete", "completed baseline", "in progress", NA),
#'   status = c(NA, NA, NA, "complete"),
#'   lost_to_followup = c(FALSE, FALSE, TRUE, NA),
#'   interview_date = c("2023-01-15", "2023/02/20", NA, "2023-03-10"),
#'   foo_1 = c(1, 3, 5, 7),
#'   foo_2 = c("a", "b", "c", "d"),
#'   bar_1 = c(2, 4, 6, 8),
#'   bar_2 = c("w", "x", "y", "z")
#' )
#' rune(combined_df)
#' 
#' # After running, access individual survey dataframes directly:
#' head(foo)  # Access the foo dataframe
#' head(bar)  # Access the bar dataframe
#'
#' @importFrom dplyr filter select
#' @export
rune <- function(df, lower = TRUE){
  
  # Define potential identifiers
  identifiers <- c("participantId", "workerId", "PROLIFIC_PID", "src_subject_id")
  
  # Filter to keep only existing keys in the dataframe
  existing_keys <- identifiers[identifiers %in% names(df)]
  
  # Check if any identifiers exist in the dataframe
  if (length(existing_keys) == 0) {
    stop("No valid identifiers found in the dataframe.")
  }
  
  # Print existing identifiers for debugging
  #print("Existing identifiers:")
  #print(existing_keys)
  
  # Find the first identifier with non-NA values
  identifier <- NA
  for (key in existing_keys) {
    non_na_count <- sum(!is.na(df[[key]]))
    
    # Debug print to check how many non-NA values exist in each column
    #print(paste("Checking identifier:", key, "with", non_na_count, "non-NA values"))
    
    if (non_na_count > 0) {  # As long as there's at least 1 non-NA value
      identifier <- key
      break
    }
  }
  
  # If no column has any non-NA values, issue a warning or stop
  if (is.na(identifier)) {
    stop(paste("No identifier found without NA values or multiple identifiers exist:", existing_keys))
  }
  
  # Print the detected identifier for debugging
  message(paste("Detected identifier:", identifier))
  
  # Define common columns to include if they exist
  common_columns <- c(
    "record_id", "subjectkey", "site", "phenotype", "visit", "week",
    "state", "status", "lost_to_followup", "lost_to_follow-up", 
    "interview_age", "interview_date"
  )
  
  # Filter to only include common columns that exist in the original dataframe
  common_columns_exist <- common_columns[common_columns %in% names(df)]
  
  # Exclude non-survey and specific columns from survey prefix detection
  non_survey_columns <- c(existing_keys, common_columns)
  survey_columns <- names(df)[!names(df) %in% non_survey_columns & grepl("_", names(df))]
  
  # Extract unique survey prefixes from survey-specific column names
  extract_first_part <- function(string) {
    parts <- strsplit(string, "_")[[1]]
    return(parts[1])
  }
  survey_prefixes <- unique(sapply(survey_columns, extract_first_part))
  
  # Exclude prefixes that might still be problematic
  excluded_prefixes <- c("PROLIFIC", "interview")
  survey_prefixes <- survey_prefixes[!survey_prefixes %in% excluded_prefixes]
  
  # Create a list of dataframes, one for each survey
  output = list()
  for (prefix in survey_prefixes) {
    survey_specific_columns <- grep(paste0("^", prefix, "_"), names(df), value = TRUE)
    if (length(survey_specific_columns) > 0) {
      # Create subset dataframe with identifier, common columns, and survey-specific columns
      all_columns <- c(identifier, common_columns_exist, survey_specific_columns)
      subset_df <- df[, all_columns, drop = FALSE]
      
      # Apply lowercase transformation if requested
      if (lower) {
        # Always preserve the identifier column name and common columns
        cols_to_transform <- !names(subset_df) %in% c(identifier, common_columns_exist)
        names(subset_df)[cols_to_transform] <- tolower(names(subset_df)[cols_to_transform])
      }
      
      # Add to output list with lowercase prefix as key
      output[[tolower(prefix)]] <- subset_df
    }
  }
  
  names(output) <- tolower(survey_prefixes)
  
  # Use parent.frame() instead of globalenv() for CRAN compliance
  list2env(output, parent.frame())
}
