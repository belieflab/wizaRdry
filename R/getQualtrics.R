#' Retrieve Survey Data from Qualtrics
#'
#' @param qualtrics_alias The alias for the Qualtrics survey to be retrieved.
#' @param institution Optional. The institution name (e.g., "temple" or "nu"). If NULL, all institutions will be searched.
#' @param label Logical indicating whether to return coded values or their associated labels (default is FALSE).
#' @return A cleaned and harmonized data frame containing the survey data with superkeys first.
#' @importFrom dplyr %>% select mutate
#' @export
#' @examples
#' \dontrun{
#' # Get survey by alias (will search all institutions)
#' survey_data <- qualtrics("rgpts")
#' }
qualtrics <- function(qualtrics_alias, institution = NULL, label = FALSE) {
  # Load necessary source files
  
  # Validate config
  cfg <- validate_config("qualtrics")
  
  # Get secrets using get_secret() to keep it secret, keep it safe
  baseUrls <- get_secret("baseUrls")
  apiKeys <- get_secret("apiKeys")
  
  # Get survey ID
  survey_id <- NULL
  
  if (!is.null(institution)) {
    # Check if institution exists
    if (!(institution %in% names(cfg$qualtrics$survey_ids))) {
      stop(paste("Institution", institution, "not found in ./config.yml configuration."))
    }
    
    # Check if survey exists in specified institution
    if (!(qualtrics_alias %in% names(cfg$qualtrics$survey_ids[[institution]]))) {
      stop(paste("Survey", qualtrics_alias, "not found in institution", institution))
    }
    
    survey_id <- cfg$qualtrics$survey_ids[[institution]][[qualtrics_alias]]
  } else {
    # Search all institutions
    found <- FALSE
    for (inst in names(cfg$qualtrics$survey_ids)) {
      if (qualtrics_alias %in% names(cfg$qualtrics$survey_ids[[inst]])) {
        survey_id <- cfg$qualtrics$survey_ids[[inst]][[qualtrics_alias]]
        institution <- inst
        found <- TRUE
        break
      }
    }
    
    if (!found) {
      stop(sprintf("Qualtrics survey '%s' not found in any institution.", qualtrics_alias))
    }
  }
  
  message(sprintf("Retrieving '%s' survey from %s Qualtrics...", qualtrics_alias, institution))
  
  # Connect to Qualtrics
  connectQualtrics()
  
  # Show loading animation (if implemented)
  if (exists("show_loading_animation")) {
    show_loading_animation()
  }
  
  # Fetch the data
  df <- qualtRics::fetch_survey(
    surveyID = survey_id,
    verbose = FALSE,
    label = label,
    convert = label,
    add_column_map = TRUE
  )
  
  if (!is.data.frame(df)) {
    stop(paste("fetch_survey did not return a data frame for", qualtrics_alias))
  }
  
  # Get identifier from config
  identifier <- cfg$identifier
  
  # Harmonize the data
  clean_df <- qualtricsHarmonization(df, identifier, qualtrics_alias)
  
  # List of allowed superkey columns to prioritize
  allowed_superkey_cols <- c(
    "record_id",
    "src_subject_id",
    "subjectkey",
    "site",
    "subsiteid",
    "sex",
    "race",
    "ethnic_group",
    "phenotype",
    "phenotype_description",
    "state",
    "status",
    "lost_to_followup",
    "lost_to_follow-up",
    "twins_study",
    "sibling_study",
    "family_study",
    "sample_taken",
    "interview_date",
    "interview_age",
    "visit",
    "week"
  )
  
  # Reorder columns to have superkeys first
  if (is.data.frame(clean_df) && ncol(clean_df) > 0) {
    # Identify which superkey columns are actually in the data
    present_superkeys <- intersect(allowed_superkey_cols, names(clean_df))
    
    # Get all other columns (non-superkeys)
    other_cols <- setdiff(names(clean_df), present_superkeys)
    
    # If there are matching superkeys, reorder the columns
    if (length(present_superkeys) > 0) {
      # Create new column order with superkeys first, then other columns
      new_order <- c(present_superkeys, other_cols)
      
      # Reorder the dataframe
      clean_df <- clean_df[, new_order, drop = FALSE]
    }
  }
  
  return(clean_df)
}

#################
## Helper Functions
#################

#' Connect to Qualtrics API
#'
#' This helper function sets up the connection to the Qualtrics API using credentials stored in a file or environment variables.
#' It is called internally by the 'qualtrics' function.
#'
#' @importFrom config get
#' @import qualtRics
#' @noRd
connectQualtrics <- function() {
  # Validate secrets
  validate_secrets("qualtrics")
  
  # Get secrets using get_secret() to keep it secret, keep it safe
  baseUrls <- get_secret("baseUrls")
  apiKeys <- get_secret("apiKeys")
  
  
  if (!exists("apiKeys") || !exists("baseUrls")) {
    stop("apiKeys and/or baseUrls arrays not found in secrets.R")
  }
  
  if (length(apiKeys) != length(baseUrls)) {
    stop("apiKeys and baseUrls arrays must have the same length.")
  }
  
  # Suppress messages about .Renviron
  suppressMessages({
    for (i in seq_along(apiKeys)) {
      tryCatch({
        # Set credentials and load environment manually to avoid restart message
        result <- qualtRics::qualtrics_api_credentials(
          api_key = apiKeys[i],
          base_url = baseUrls[i],
          install = TRUE,
          overwrite = TRUE
        )
        
        # If credentials were set successfully, also read them into current session
        if (file.exists("~/.Renviron")) {
          readRenviron("~/.Renviron")
        }
        
        return(TRUE)
      }, error = function(e) {
        if (i == length(apiKeys)) {
          stop("Failed to connect with any credentials provided in ./secrets.R")
        }
      })
    }
  })
}

#' Harmonize Data
#'
#' Performs data cleaning and harmonization on the fetched Qualtrics survey data.
#'
#' @param df Data frame containing Qualtrics survey data.
#' @param identifier The unique identifier for survey respondents.
#' @param qualtrics_alias The alias for the Qualtrics survey.
#' @return Harmonized data frame.
#' @importFrom dplyr mutate
#' @noRd
qualtricsHarmonization <- function(df, identifier, qualtrics_alias) {
  if (!is.data.frame(df)) {
    stop("Input to qualtricsHarmonization is not a data frame.")
  }
  
  # Validate config
  cfg <- validate_config("qualtrics")
  
  # Check for visit variable, if not add baseline
  `%!in%` <- Negate(`%in%`)
  if ("visit" %!in% colnames(df) && cfg$study_alias == 'capr') {
    df$visit <- "bl"
  }
  
  # If visit variable exists, standardize values
  if ("visit" %in% colnames(df) && cfg$study_alias == 'capr') {
    df$visit <- ifelse(is.na(df$visit), "bl", 
                       ifelse(df$visit == "0", "bl", 
                              ifelse(df$visit == "12", "12m", 
                                     ifelse(df$visit == "24", "24m", df$visit))))
  }
  
  # Additional processing can be uncommented and modified as needed
  # df$src_subject_id <- as.numeric(df$src_subject_id)
  # df$interview_date <- as.Date(df$interview_date, "%m/%d/%Y")
  # df$measure <- qualtrics_alias
  
  # convert dates (from string ("m/d/Y") to iso date format)
  if ("interview_date" %in% colnames(df)) {
    df$interview_date <- parse_dates_to_iso(df$interview_date, "interview_date")
  }
  
  suppressWarnings(return(df))
}

#' Display table of available Qualtrics surveys 
#'
#' Retrieves a list of all available surveys in the configured Qualtrics account.
#'
#' @param institution Optional; the institution identifier to use. If NULL, uses all
#'   institutions specified in the configuration file.
#'
#' @return A data frame containing the IDs and names of all available surveys
#'   in the configured Qualtrics account. Can be used to identify surveys for
#'   further data retrieval.
#'   
#' @export
qualtrics.index <- function(institution = NULL) {
  # Temporarily suppress warnings
  old_warn <- options("warn")
  options(warn = -1)
  
  tryCatch({
    # Load necessary source files for helper functions
    
    # Load required secrets and configuration
    validate_secrets("qualtrics")
    
    # Get secrets using get_secret() to keep it secret, keep it safe
    baseUrls <- get_secret("baseUrls")
    apiKeys <- get_secret("apiKeys")
    
    cfg <- validate_config("qualtrics")
    
    # Connect to Qualtrics using the existing helper function
    connectQualtrics()
    
    # Get all surveys
    message("Fetching available Qualtrics surveys...")
    surveys <- qualtRics::all_surveys()
    
    # Filter by institution if specified
    if (!is.null(institution) && !is.null(cfg$qualtrics$survey_ids)) {
      if (institution %in% names(cfg$qualtrics$survey_ids)) {
        # Extract the survey IDs for the specified institution
        inst_surveys <- cfg$qualtrics$survey_ids[[institution]]
        
        # Create a mapping of configured surveys for this institution
        configured_surveys <- data.frame(
          id = unlist(inst_surveys),
          alias = names(inst_surveys),
          stringsAsFactors = FALSE
        )
        
        # Filter and merge with alias information
        surveys <- merge(surveys, configured_surveys, by = "id", all.y = TRUE)
        message(paste0("Filtered to ", nrow(surveys), " surveys from institution '", institution, "'"))
      } else {
        warning(paste0("Institution '", institution, "' not found in configuration. Returning all surveys."))
      }
    } else if (!is.null(cfg$qualtrics$survey_ids)) {
      # Create a complete mapping of all configured surveys across institutions
      all_mapped_surveys <- data.frame(
        id = character(0),
        alias = character(0),
        institution = character(0),
        stringsAsFactors = FALSE
      )
      
      for (inst in names(cfg$qualtrics$survey_ids)) {
        inst_surveys <- cfg$qualtrics$survey_ids[[inst]]
        if (length(inst_surveys) > 0) {
          inst_df <- data.frame(
            id = unlist(inst_surveys),
            alias = names(inst_surveys),
            institution = rep(inst, length(inst_surveys)),
            stringsAsFactors = FALSE
          )
          all_mapped_surveys <- rbind(all_mapped_surveys, inst_df)
        }
      }
      
      # Merge with the surveys data
      if (nrow(all_mapped_surveys) > 0) {
        surveys <- merge(surveys, all_mapped_surveys, by = "id", all = TRUE)
      }
    }
    
    # Format the output
    if (nrow(surveys) > 0) {
      # Sort by name for easier reading
      surveys <- surveys[order(surveys$name), ]
      
      # Create output table
      if ("alias" %in% colnames(surveys)) {
        if ("institution" %in% colnames(surveys)) {
          # Full output with institution information
          result <- data.frame(
            ID = surveys$id,
            Alias = surveys$alias,
            Institution = surveys$institution,
            Name = surveys$name,
            Last_Modified = surveys$lastModified,
            stringsAsFactors = FALSE
          )
        } else {
          # Output for specific institution
          result <- data.frame(
            ID = surveys$id,
            Alias = surveys$alias,
            Name = surveys$name,
            Last_Modified = surveys$lastModified,
            stringsAsFactors = FALSE
          )
        }
      } else {
        # Basic output for unconfigured surveys
        result <- data.frame(
          ID = surveys$id,
          Name = surveys$name,
          Last_Modified = surveys$lastModified,
          stringsAsFactors = FALSE
        )
      }
      
      # Print the results in a user-friendly format
      message(paste0("\nFound ", nrow(result), " Qualtrics surveys:"))
      print(result, row.names = TRUE)
      
      # Restore previous warning setting
      options(old_warn)
      
      return(invisible(surveys))
    } else {
      message("No surveys found.")
      
      # Restore previous warning setting
      options(old_warn)
      
      return(invisible(NULL))
    }
  }, error = function(e) {
    # Restore previous warning setting before stopping
    options(old_warn)
    
    stop(paste("Error connecting to Qualtrics:", e$message))
  })
}

#' Fetch Qualtrics survey metadata to be stored in data frame
#'
#' This function extracts column mappings from the metadata of a Qualtrics survey data frame.
#' It can accept either a data frame containing Qualtrics data, a variable name as string,
#' or a survey alias string.
#'
#' @param survey_alias Can either be an existing dataframe, variable name as string, or survey alias string
#' @param exclude_embedded Only select QIDs
#' @return A list containing the mappings of column names to survey questions.
#' @export
qualtrics.dict <- function(survey_alias, exclude_embedded = TRUE) {
  # First handle the case of a non-existent variable being passed without quotes
  var_name <- NULL
  
  # Only try to get the name if survey_alias is missing
  if (missing(survey_alias)) {
    stop("Survey alias is required")
  }
  
  # Capture the actual call
  call_expr <- substitute(survey_alias)
  
  # Check if it's a symbol (variable name) that doesn't exist
  if (is.symbol(call_expr) && !exists(as.character(call_expr))) {
    var_name <- as.character(call_expr)
    message(sprintf("Object '%s' not found, using as survey alias instead.", var_name))
    survey_alias <- var_name
  }
  
  # Now proceed with normal function logic
  
  # Check if input is a data frame
  if (is.data.frame(survey_alias)) {
    # Input is already a data frame, use it directly
    colmap <- qualtRics::extract_colmap(respdata = survey_alias)
    
    # Filter to include only QID fields
    if (exclude_embedded && !is.null(colmap)) {
      if ("ImportId" %in% names(colmap)) {
        colmap <- colmap[!is.na(colmap$ImportId) & grepl("^QID", colmap$ImportId), ]
      } else if ("qid" %in% names(colmap)) {
        colmap <- colmap[!is.na(colmap$qid) & grepl("^QID", colmap$qid), ]
      }
    }
    
    return(colmap)
  }
  
  # Input is a string
  if (is.character(survey_alias)) {
    # First, check if it's a variable name in the global environment
    if (exists(survey_alias)) {
      var_data <- base::get(survey_alias)
      
      # Check if the variable is a data frame
      if (is.data.frame(var_data)) {
        message(sprintf("Using existing data frame '%s' from environment.", survey_alias))
        colmap <- qualtRics::extract_colmap(respdata = var_data)
        
        # Filter to include only QID fields
        if (exclude_embedded && !is.null(colmap)) {
          if ("ImportId" %in% names(colmap)) {
            colmap <- colmap[!is.na(colmap$ImportId) & grepl("^QID", colmap$ImportId), ]
          } else if ("qid" %in% names(colmap)) {
            colmap <- colmap[!is.na(colmap$qid) & grepl("^QID", colmap$qid), ]
          }
        }
        
        return(colmap)
      }
    }
    
    # Not a variable or not a data frame, treat as survey alias
    message(sprintf("Fetching dictionary for alias '%s' from Qualtrics.", survey_alias))
    
    # Temporarily suppress warnings and disable progress bars
    old_warn <- options("warn")
    old_opt <- options(qualtRics.progress = FALSE)
    on.exit({options(old_warn); options(old_opt)}, add = TRUE)
    options(warn = -1)
    
    # Get survey data with suppressed output
    survey_data <- suppressMessages(wizaRdry::qualtrics(survey_alias))
    colmap <- qualtRics::extract_colmap(respdata = survey_data)
    
    # Filter to include only QID fields
    if (exclude_embedded && !is.null(colmap)) {
      if ("ImportId" %in% names(colmap)) {
        colmap <- colmap[!is.na(colmap$ImportId) & grepl("^QID", colmap$ImportId), ]
      } else if ("qid" %in% names(colmap)) {
        colmap <- colmap[!is.na(colmap$qid) & grepl("^QID", colmap$qid), ]
      }
    }
    
    return(colmap)
  }
  
  # Invalid input type
  stop("Input must be either a data frame or a string (survey alias or variable name).")
}

#' Convert dates to ISO format robustly
#'
#' This function attempts to intelligently parse dates in various formats
#' and convert them to ISO format (YYYY-MM-DD).
#'
#' @param date_vector A vector of date strings to be parsed
#' @param column_name The name of the column being parsed (for error messages)
#' @return A Date vector in ISO format (YYYY-MM-DD)
#' @importFrom lubridate parse_date_time
#' @noRd
parse_dates_to_iso <- function(date_vector, column_name = "date") {
  if (is.null(date_vector) || length(date_vector) == 0) {
    return(date_vector)
  }
  
  # Skip if already in Date format
  if (inherits(date_vector, "Date")) {
    return(date_vector)
  }
  
  # If already a POSIXct or POSIXlt, convert to Date
  if (inherits(date_vector, "POSIXt")) {
    return(as.Date(date_vector))
  }
  
  # Convert to character if not already
  date_vector <- as.character(date_vector)
  
  # Remove any NA values for analysis
  non_na_dates <- date_vector[!is.na(date_vector) & date_vector != ""]
  
  if (length(non_na_dates) == 0) {
    # All NA or empty, just return a vector of NAs
    return(as.Date(date_vector))
  }
  
  # Define a set of possible date formats to try
  possible_formats <- c(
    # American formats
    "mdy", "mdY", "m/d/y", "m/d/Y", "m-d-y", "m-d-Y",
    # European/ISO formats
    "ymd", "Ymd", "y/m/d", "Y/m/d", "y-m-d", "Y-m-d",
    # Other common formats
    "dmy", "dmY", "d/m/y", "d/m/Y", "d-m-y", "d-m-Y",
    # Month name formats
    "mdy_b", "mdY_b", "b_d_y", "b_d_Y", 
    "dmy_b", "dmY_b", "d_b_y", "d_b_Y", 
    "ymd_b", "Ymd_b", "y_b_d", "Y_b_d"
  )
  
  # Try to detect the date format
  tryCatch({
    # Sample the first few non-NA dates to guess format
    sample_size <- min(100, length(non_na_dates))
    sample_dates <- non_na_dates[1:sample_size]
    
    # Try parsing with each format and keep track of success rate
    format_success <- numeric(length(possible_formats))
    
    for (i in seq_along(possible_formats)) {
      parsed_dates <- suppressWarnings(
        lubridate::parse_date_time(sample_dates, possible_formats[i], quiet = TRUE)
      )
      format_success[i] <- sum(!is.na(parsed_dates)) / length(sample_dates)
    }
    
    # Find the format with the highest success rate
    best_format_idx <- which.max(format_success)
    best_format <- possible_formats[best_format_idx]
    
    # If the best format doesn't parse at least 50% of dates, try combo of top formats
    if (format_success[best_format_idx] < 0.5) {
      # Get top 3 formats
      top_formats <- possible_formats[order(format_success, decreasing = TRUE)[1:3]]
      
      # Try parsing with these formats
      parsed_dates <- suppressWarnings(
        lubridate::parse_date_time(date_vector, top_formats, quiet = TRUE)
      )
    } else {
      # Parse all dates with the best format
      parsed_dates <- suppressWarnings(
        lubridate::parse_date_time(date_vector, best_format, quiet = TRUE)
      )
    }
    
    # Convert to Date class
    result <- as.Date(parsed_dates)
    
    # Basic validation: check for impossibly old dates (before 1900) or future dates
    result[result < as.Date("1900-01-01") | result > Sys.Date() + 30] <- NA
    
    # Log stats about parsing
    success_rate <- sum(!is.na(result)) / length(date_vector) * 100
    message(sprintf("Parsed %s: %.1f%% successful using %s format", 
                    column_name, success_rate, 
                    ifelse(format_success[best_format_idx] < 0.5, 
                           paste(top_formats, collapse=", "), best_format)))
    
    return(result)
  }, error = function(e) {
    # Fallback: try base R's as.Date with common formats
    warning(sprintf("Advanced date parsing failed for %s: %s. Falling back to basic parsing.",
                    column_name, e$message))
    
    fallback_formats <- c("%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y", "%Y/%m/%d")
    for (fmt in fallback_formats) {
      parsed <- suppressWarnings(as.Date(date_vector, format = fmt))
      if (sum(!is.na(parsed)) / length(parsed) > 0.5) {
        message(sprintf("Basic parsing of %s succeeded with format: %s", column_name, fmt))
        return(parsed)
      }
    }
    
    # If all else fails, return NA
    warning(sprintf("All date parsing methods failed for %s", column_name))
    return(as.Date(rep(NA, length(date_vector))))
  })
}

#' Alias for 'qualtrics'
#'
#' This is a legacy alias for the 'qualtrics' function to maintain compatibility with older code.
#'
#' @inheritParams qualtrics
#' @inherit qualtrics return
#' @export
#' @examples
#' \dontrun{
#' survey_data <- getSurvey("your_survey_alias")
#' }
getSurvey <- qualtrics
