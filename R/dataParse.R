#' Parse Qualtrics Data into Separate Survey Dataframes
#'
#' This function takes a raw Qualtrics dataframe containing multiple surveys and
#' separates it into individual dataframes for each survey detected in the data.
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
  print("Existing identifiers:")
  print(existing_keys)
  
  # Find the first identifier with non-NA values
  identifier <- NA
  for (key in existing_keys) {
    non_na_count <- sum(!is.na(df[[key]]))
    
    # Debug print to check how many non-NA values exist in each column
    print(paste("Checking identifier:", key, "with", non_na_count, "non-NA values"))
    
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
  print(paste("Detected identifier:", identifier))
  
  # Exclude non-survey and specific columns
  non_survey_columns <- c(existing_keys, "interview_date", "PROLIFIC_PID")
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
      # Get the subset dataframe
      subset_df <- df[, c(identifier, survey_specific_columns)]
      
      # Apply lowercase transformation if requested
      if (lower) {
        # Always preserve the identifier column name
        cols_to_transform <- names(subset_df) != identifier
        names(subset_df)[cols_to_transform] <- tolower(names(subset_df)[cols_to_transform])
      }
      
      # Add to output list with lowercase prefix as key
      output[[tolower(prefix)]] <- subset_df
    }
  }
  
  names(output) <- tolower(survey_prefixes)
  
  return(list2env(output, globalenv()))
}


#' Parse Qualtrics Data into Separate Survey Dataframes
#'
#' This function takes a raw Qualtrics dataframe containing multiple surveys and
#' separates it into individual dataframes for each survey detected in the data.
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
  print("Existing identifiers:")
  print(existing_keys)
  
  # Find the first identifier with non-NA values
  identifier <- NA
  for (key in existing_keys) {
    non_na_count <- sum(!is.na(df[[key]]))
    
    # Debug print to check how many non-NA values exist in each column
    print(paste("Checking identifier:", key, "with", non_na_count, "non-NA values"))
    
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
  print(paste("Detected identifier:", identifier))
  
  # Exclude non-survey and specific columns
  non_survey_columns <- c(existing_keys, "interview_date", "PROLIFIC_PID")
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
      # Get the subset dataframe
      subset_df <- df[, c(identifier, survey_specific_columns)]
      
      # Apply lowercase transformation if requested
      if (lower) {
        # Always preserve the identifier column name
        cols_to_transform <- names(subset_df) != identifier
        names(subset_df)[cols_to_transform] <- tolower(names(subset_df)[cols_to_transform])
      }
      
      # Add to output list with lowercase prefix as key
      output[[tolower(prefix)]] <- subset_df
    }
  }
  
  names(output) <- tolower(survey_prefixes)
  
  return(list2env(output, globalenv()))
}

#' Parse Qualtrics Data into Separate Survey Dataframes
#'
#' This function takes a raw Qualtrics dataframe containing multiple surveys and
#' separates it into individual dataframes for each survey detected in the data.
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
#' rune(combined_df)
#' 
#' # After running, access individual survey dataframes directly:
#' head(pss)  # Access the PSS survey dataframe
#' head(cesd) # Access the CESD survey dataframe
#' }
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
  print("Existing identifiers:")
  print(existing_keys)
  
  # Find the first identifier with non-NA values
  identifier <- NA
  for (key in existing_keys) {
    non_na_count <- sum(!is.na(df[[key]]))
    
    # Debug print to check how many non-NA values exist in each column
    print(paste("Checking identifier:", key, "with", non_na_count, "non-NA values"))
    
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
  print(paste("Detected identifier:", identifier))
  
  # Exclude non-survey and specific columns
  non_survey_columns <- c(existing_keys, "interview_date", "PROLIFIC_PID")
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
      # Get the subset dataframe
      subset_df <- df[, c(identifier, survey_specific_columns)]
      
      # Apply lowercase transformation if requested
      if (lower) {
        # Always preserve the identifier column name
        cols_to_transform <- names(subset_df) != identifier
        names(subset_df)[cols_to_transform] <- tolower(names(subset_df)[cols_to_transform])
      }
      
      # Add to output list with lowercase prefix as key
      output[[tolower(prefix)]] <- subset_df
    }
  }
  
  names(output) <- tolower(survey_prefixes)
  
  return(list2env(output, globalenv()))
}

