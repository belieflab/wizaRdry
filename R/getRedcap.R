#
# function: redcap(instrument_name)
# input: instrument_name from table below
#

# Get full file paths of all R files in the api directory
# base::source all files using lapply()

# Initialize functions needed for the progress bar
#' @noRd
initializeLoadingAnimation <- function(steps) {
  # Get console width
  width <- tryCatch({
    if (requireNamespace("cli", quietly = TRUE)) {
      cli::console_width() - 10  # Leave some margin
    } else {
      getOption("width", 80) - 10  # Fallback to R's width setting
    }
  }, error = function(e) 80)  # Default if all else fails

  list(
    steps = steps,
    current = 0,
    width = width,
    start_time = Sys.time()
  )
}

#' @noRd
updateLoadingAnimation <- function(pb, current) {
  pb$current <- current
  percentage <- round(current / pb$steps * 100)
  filled <- round(pb$width * current / pb$steps)
  bar <- paste0(
    strrep("=", filled),
    strrep(" ", pb$width - filled)
  )
  cat(sprintf("\r|%s| %3d%%", bar, percentage))  # Removed extra spaces before bar
  utils::flush.console()
}

#' @noRd
completeLoadingAnimation <- function(pb) {
  updateLoadingAnimation(pb, pb$steps)
  cat("\n")
}

#' Format a time duration in a human-readable way
#'
#' @name formatDuration
#' @param duration The duration to format in seconds or minutes
#' @return A formatted string representing the duration
#' @noRd
formatDuration <- function(duration) {
  secs <- as.numeric(duration, units = "secs")
  if (secs < 60) {
    return(sprintf("%.1f seconds", secs))
  } else {
    mins <- floor(secs / 60)
    remaining_secs <- round(secs %% 60, 1)
    if (remaining_secs > 0) {
      return(sprintf("%d minutes and %.1f seconds", mins, remaining_secs))
    } else {
      return(sprintf("%d minutes", mins))
    }
  }
}

#' Fetch data from REDCap to be stored in a data frame
#'
#' Retrieves data from a REDCap instrument and ensures subject identifiers
#' are propagated across all events
#'
#' @param instrument_name Name of the REDCap instrument
#' @param ... Optional column names to filter for. Only rows with non-missing values
#'        in ALL specified columns will be returned. This is useful for filtering
#'        data to only include complete cases for specific variables of interest.
#' @param raw_or_label Whether to return raw or labeled values
#' @param redcap_event_name Optional event name filter. Can be a single string
#'        or a vector of event names (e.g., \code{c("event1", "event2")})
#' @param batch_size Number of records to retrieve per batch
#' @param records Optional vector of specific record IDs
#' @param fields Optional vector of specific fields
#' @param pii Logical; if FALSE (default), remove fields marked as PII. TRUE keeps PII.
#' @param interview_date Optional; can be either:
#'        - A date string in various formats (ISO, US, etc.) to filter data up to that date
#'        - A boolean TRUE to return only rows with non-NA interview_date values
#' @param date_format Default ymd define date format for interview_date
#' @param complete Option boolean TRUE will return only forms marked as complete in REDCap
#'
#' @return A data frame containing the requested REDCap data
#' @export
#' @examples
#' \dontrun{
#' # Get data from a specific instrument
#' data <- redcap("demographics")
#' }
redcap <- function(instrument_name = NULL, ..., raw_or_label = "raw",
                   redcap_event_name = NULL, batch_size = 1000,
                   records = NULL, fields = NULL, pii = FALSE,
                   interview_date = NULL, date_format = "ymd", complete = NULL) {
  start_time <- Sys.time()

  # Validate secrets and config - MUST COME FIRST
  validate_secrets("redcap")
  config <- validate_config("redcap")

  # Validate that the primary key is properly configured
  if (is.null(config$redcap$primary_key) || !is.character(config$redcap$primary_key)) {
    message("Warning: Primary key not properly configured. Defaulting to 'record_id'")
    config$redcap$primary_key <- "record_id"
  }

  # Log what primary key we're using
  message(sprintf("Using '%s' as the primary key", config$redcap$primary_key))

  # Define the allowed superkey columns explicitly
  allowed_superkey_cols <- c(
    config$redcap$primary_key,
    "src_subject_id",
    "subjectkey",
    "site",
    "subsiteid",
    "src_institution_id",
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
    "visit",
    "week",
    "interview_age",
    "group_status",
    "int_start",
    "int_end"
  )

  # Validate date_format parameter
  if (!date_format %in% c("mdy", "dmy", "ymd")) {
    stop("date_format must be one of 'mdy', 'dmy', or 'ymd'")
  }

  # Get secrets using get_secret() to keep it secret, keep it safe
  uri <- get_secret("uri")
  token <- get_secret("token")

  # Test to see if API connection is working
  message("Validating REDCap API connection and user permissions...")
  api_test <- tryCatch({
    # Use a call to redcap_instruments to see if API connnection is working
    forms_test <- REDCapR::redcap_instruments(redcap_uri = uri, token = token, verbose = FALSE)

    # Add conditions to see if API connection is working, if not, throw an error
    if (!is.null(forms_test$success) && !forms_test$success) {
      stop("API connection failed - check your user permissions")
    }

    if (is.null(forms_test$data) || nrow(forms_test$data) == 0) {
      stop("API connected but no instruments accessible - check your export permissions")
    }

    # If permissions are correct and everything is working, notify the user
    message("\nREDCap API connection and permissions validated successfully\n")
    TRUE

    # If permissions are not correct, throw an error with message
  }, error = function(e) {
    stop(sprintf(paste(
      "REDCap API validation failed: \n",
      "Please verify:\n",
      "1. Your REDCap user has 'API Export' and 'API Import/Update' rights\n",
      "2. The API module is enabled for your REDCap project\n",
      "3. Your API token and URI are correct in your secrets configuration"
    )), call. = FALSE)
  })

  # Input validation
  if (is.null(instrument_name)) {
    forms_data <- REDCapR::redcap_instruments(redcap_uri = uri, token = token, verbose = FALSE)$data
    forms_filtered <- forms_data[!grepl("nda", forms_data$instrument_name), ]
    random_instrument <- sample(forms_filtered$instrument_name, 1)
    forms_table <- paste(capture.output(message(redcap.index())), collapse = "\n")
    example_text <- sprintf("\n\nExample:\n%s <- getRedcap(\"%s\")", random_instrument, random_instrument)
    stop(sprintf("No REDCap Instrument Name provided!\n%s%s",
                 forms_table, example_text),
         call. = FALSE)
  }

  # Check if the config$redcap$superkey exists
  if (is.null(config$redcap$superkey)) {
    stop("No superkey form defined in ./config.yml. Please check your REDCap configuration.")
  }

  # Check if the instrument exists before trying to retrieve data
  tryCatch({
    forms_data <- REDCapR::redcap_instruments(redcap_uri = uri, token = token, verbose = FALSE)$data
    # Ensure instrument_name is properly trimmed
    instrument_name <- trimws(instrument_name)
    if (instrument_name %in% forms_data$instrument_name) {
      # Instrument exists, continue
    } else {
      # Format available instruments in a readable way
      available_forms <- paste(sort(forms_data$instrument_name), collapse = "\n- ")
      stop(sprintf("\nInstrument '%s' not found in REDCap for %s.\n\nAvailable instruments:\n- %s",
                   instrument_name, toupper(config$study_alias), available_forms))
    }
  }, error = function(e) {
    if (grepl("not found in REDCap", e$message)) {
      # This is our custom error, pass it through
      stop(e$message, call. = FALSE)
    } else {
      # This is an unexpected error
      stop(sprintf("Error connecting to REDCap: %s", e$message), call. = FALSE)
    }
  })

  # Determine if the requested instrument is the configured superkey form
  is_superkey_request <- FALSE
  if (!is.null(config$redcap$superkey)) {
    is_superkey_request <- identical(trimws(instrument_name), trimws(config$redcap$superkey))
  }

  # First get metadata to identify PII fields
  metadata <- NULL
  pii_fields <- c()
  # Get the full metadata to identify date fields
  metadata <- tryCatch({
    REDCapR::redcap_metadata_read(
      redcap_uri = uri,
      token = token,
      verbose = FALSE
    )$data
  }, error = function(e) {
    message("Warning: Could not retrieve metadata.")
    return(NULL)
  })

  # Find date fields in the metadata
  date_fields <- c()
  if (!is.null(metadata) && "field_name" %in% names(metadata) && "text_validation_type_or_show_slider_number" %in% names(metadata)) {
    # Identify all date fields
    date_validation_types <- c("date_mdy", "date_dmy", "date_ymd", "datetime_mdy", "datetime_dmy", "datetime_ymd", "datetime_seconds_mdy", "datetime_seconds_dmy", "datetime_seconds_ymd")
    date_fields <- metadata$field_name[metadata$text_validation_type_or_show_slider_number %in% date_validation_types]
    date_fields <- c(date_fields, "interview_date") # Add interview_date explicitly
    date_fields <- date_fields[!is.na(date_fields)]
    if (length(date_fields) > 0) {
      message(sprintf("Found %d date fields that will be properly formatted: %s",
                      length(date_fields),
                      paste(date_fields, collapse = ", ")))
    }
  }

  if (!pii && !is.null(metadata) && "field_name" %in% names(metadata) && "identifier" %in% names(metadata)) {
    pii_fields <- metadata$field_name[metadata$identifier == "y"]
    # Filter out NA values and print only the non-NA field names
    pii_fields <- pii_fields[!is.na(pii_fields)]
    if (length(pii_fields) > 0) {
      message(sprintf("Found %d PII fields that will be excluded: %s",
                      length(pii_fields),
                      paste(pii_fields, collapse = ", ")))
    }
  }

  # Now decide which fields to request based on the PII exclusion
  selected_fields <- NULL
  if (!pii && length(pii_fields) > 0) {
    # If fields parameter is provided, exclude PII fields from it
    if (!is.null(fields)) {
      selected_fields <- setdiff(fields, pii_fields)
      if (length(selected_fields) == 0) {
        message("Warning: All requested fields are marked as PII. Results may be empty.")
      }
    } else {
      # If no fields specified, we'll exclude PII fields after retrieving data
      # This is because we can't know all fields in advance
      selected_fields <- NULL
    }
  } else {
    # Use the fields parameter as-is if not excluding PII
    selected_fields <- fields
  }

  # Progress bar
  pb <- initializeLoadingAnimation(20)
  event_name_display <- if (!is.null(redcap_event_name)) {
    if (length(redcap_event_name) == 1) {
      sprintf(" %s", redcap_event_name)
    } else {
      sprintf(" [%s]", paste(redcap_event_name, collapse = ", "))
    }
  } else {
    ""
  }
  message(sprintf("\nImporting records from REDCap form: %s%s%s",
                  instrument_name,
                  event_name_display,
                 ifelse(!pii && length(pii_fields) > 0, " (excluding PII)", "")))
  for (i in 1:20) {
    updateLoadingAnimation(pb, i)
    Sys.sleep(0.1)
  }
  completeLoadingAnimation(pb)
  message("")

  # 1. First, get the superkey data (always using "label")
  superkey_response <- REDCapR::redcap_read(
    redcap_uri = uri,
    token = token,
    forms = config$redcap$superkey,
    batch_size = batch_size,
    records = records,
    raw_or_label = "label",  # Always use label for superkey
    raw_or_label_headers = "raw",
    verbose = FALSE
  )

  # 2. Then, get the instrument data with user's raw_or_label preference
  instrument_response <- REDCapR::redcap_read(
    redcap_uri = uri,
    token = token,
    forms = instrument_name,
    batch_size = batch_size,
    records = records,
    fields = selected_fields,
    raw_or_label = raw_or_label,  # Use user's preference
    raw_or_label_headers = "raw",
    verbose = FALSE
  )

  # Filter superkey columns to only allowed columns
  super_key_cols <- intersect(names(superkey_response$data), allowed_superkey_cols)

  # Add redcap_event_name to super_key_cols if it exists in either dataset
  if ("redcap_event_name" %in% names(superkey_response$data) ||
      "redcap_event_name" %in% names(instrument_response$data)) {
    super_key_cols <- c(super_key_cols, "redcap_event_name")
  }

  # If excluding PII, remove PII fields from superkey columns
  if (!pii && length(pii_fields) > 0) {
    super_key_cols <- setdiff(super_key_cols, pii_fields)
  }

  # If the requested instrument IS the superkey, return it directly without joins/propagation
  if (is_superkey_request) {
    message("Requested instrument matches configured superkey; returning without joins or propagation.")
    df <- instrument_response$data
  } else if ("redcap_event_name" %in% names(superkey_response$data)) {
    # First, ensure the primary key exists in the superkey data
    if (!(config$redcap$primary_key %in% names(superkey_response$data))) {
      stop(sprintf("Primary key '%s' not found in superkey data", config$redcap$primary_key))
    }

    # For each subject, collect all non-NA values across events
    subjects <- unique(superkey_response$data[[config$redcap$primary_key]])

    if (length(subjects) == 0) {
      message("No subjects found in superkey data. Creating empty consolidated superkey.")
      # Create an empty data frame with the primary key as the first column
      consolidated_superkey <- data.frame(matrix(ncol = 0, nrow = 0))
    } else {
      # Create a data frame with the primary key column
      primary_key_col <- config$redcap$primary_key
      consolidated_superkey <- data.frame(matrix(ncol = 1, nrow = length(subjects)))
      colnames(consolidated_superkey) <- primary_key_col
      consolidated_superkey[[primary_key_col]] <- subjects

      # Add the rest of the columns
      for (col in super_key_cols) {
        if (col != primary_key_col && col != "redcap_event_name") {
          # Only try to process columns that exist in the source data
          if (col %in% names(superkey_response$data)) {
            consolidated_superkey[[col]] <- NA
            for (subject_id in subjects) {
              # Get all values for this subject across all events
              subject_rows <- superkey_response$data[superkey_response$data[[primary_key_col]] == subject_id, ]
              non_na_values <- subject_rows[[col]][!is.na(subject_rows[[col]])]
              if (length(non_na_values) > 0) {
                consolidated_superkey[consolidated_superkey[[primary_key_col]] == subject_id, col] <- non_na_values[1]
              }
            }
          }
        }
      }
    }
  } else if (!is_superkey_request) {
    # If no redcap_event_name, just use the superkey data as is, but only the allowed columns
    allowed_cols <- intersect(names(superkey_response$data), super_key_cols)

    if (length(allowed_cols) == 0) {
      message("No matching columns found in superkey data. Creating empty consolidated superkey.")
      consolidated_superkey <- data.frame()
    } else {
      consolidated_superkey <- superkey_response$data[, allowed_cols, drop = FALSE]
    }
  }

  # FIXED: Merge the consolidated superkey with the instrument data
  # If superkey requested, we already set df above; otherwise proceed with merge logic
  if (!exists("df", inherits = FALSE) && nrow(consolidated_superkey) == 0) {
    message("Warning: Consolidated superkey is empty. Using instrument data as is.")
    df <- instrument_response$data
  } else if (!exists("df", inherits = FALSE) && nrow(instrument_response$data) == 0) {
    message("Warning: Instrument data is empty. Using consolidated superkey as is.")
    df <- consolidated_superkey
  } else if (!exists("df", inherits = FALSE) && "redcap_event_name" %in% names(instrument_response$data)) {
    # Keep the original event names from the instrument data
    df <- instrument_response$data

    # Check if the primary key exists in the instrument data
    if (!(config$redcap$primary_key %in% names(df))) {
      message(sprintf("Warning: Primary key '%s' not found in instrument data. Cannot merge superkey information.", config$redcap$primary_key))
    } else {
      # Merge in the consolidated superkey data (excluding event_name)
      for (subject_id in unique(df[[config$redcap$primary_key]])) {
        # Check if this subject exists in the superkey data
        if (config$redcap$primary_key %in% names(consolidated_superkey)) {
          superkey_data <- consolidated_superkey[consolidated_superkey[[config$redcap$primary_key]] == subject_id,
                                                 !(names(consolidated_superkey) %in% c("redcap_event_name")),
                                                 drop = FALSE]

          if (nrow(superkey_data) > 0) {
            # Apply the superkey data to all rows for this subject
            for (col in names(superkey_data)) {
              if (col != config$redcap$primary_key) {
                if (col %in% names(df)) {
                  df[df[[config$redcap$primary_key]] == subject_id, col] <- superkey_data[[col]][1]
                } else {
                  # Add the column if it doesn't exist in the instrument data
                  df[[col]] <- NA
                  df[df[[config$redcap$primary_key]] == subject_id, col] <- superkey_data[[col]][1]
                }
              }
            }
          }
        }
      }
    }
  } else if (!exists("df", inherits = FALSE)) {
    # Standard merge if there's no event name in the instrument data
    message("Performing standard merge of superkey and instrument data")
    # Make sure at least the primary key is in both data frames
    if (!(config$redcap$primary_key %in% names(consolidated_superkey)) ||
        !(config$redcap$primary_key %in% names(instrument_response$data))) {
      message(sprintf("Warning: Primary key '%s' missing from one of the data frames. Concatenating data instead of merging.", config$redcap$primary_key))
      df <- rbind(consolidated_superkey, instrument_response$data)
    } else {
      df <- merge(
        consolidated_superkey,
        instrument_response$data,
        by = config$redcap$primary_key,
        all.y = TRUE
      )
    }
  }

  # Continue with the existing propagation logic
  if (config$redcap$primary_key %in% names(df) && "redcap_event_name" %in% names(df)) {
    # Keep only fields that exist in our dataframe
    super_key_cols <- super_key_cols[super_key_cols %in% names(df)]
    if (length(super_key_cols) > 0) {
      message("\nPropagating superkey across all events for each subject...")
      # For each subject
      for (subject_id in unique(df[[config$redcap$primary_key]])) {
        # Get all rows for this subject
        subject_rows <- which(df[[config$redcap$primary_key]] == subject_id)
        # For each super key field, find a non-NA value across all events
        for (key_field in super_key_cols) {
          # Skip redcap_event_name to preserve the original values
          if (key_field == "redcap_event_name") {
            next
          }
          key_values <- df[subject_rows, key_field]
          non_na_values <- key_values[!is.na(key_values)]
          if (length(non_na_values) > 0) {
            # Propagate the first non-NA value to all events for this subject
            df[subject_rows, key_field] <- non_na_values[1]
          }
        }
      }
    }
  }

  # Fix the date fields - Convert numeric dates to proper date format
  # Process date fields - this includes interview_date
  if (length(date_fields) > 0) {
    message("\nConverting date fields to proper date format...")
    for (date_field in date_fields) {
      if (date_field %in% names(df)) {
        # Only process if the column exists and has some non-NA values
        if (any(!is.na(df[[date_field]]))) {
          # Convert to character first to handle both numeric and character inputs
          dates_char <- as.character(df[[date_field]])
          # Check if dates are numeric (SPSS/SAS format) - typically represented as days since 1960-01-01
          if (all(grepl("^\\d+(\\.\\d+)?$", dates_char[!is.na(dates_char)]))) {
            # Convert from numeric to date
            # For SPSS/SAS dates (days since 1960-01-01)
            numeric_dates <- as.numeric(dates_char)
            date_values <- as.Date(numeric_dates, origin = "1960-01-01")
            # Now format the date according to the requested format
            if (date_format == "mdy") {
              df[[date_field]] <- format(date_values, "%m/%d/%Y")
            } else if (date_format == "dmy") {
              df[[date_field]] <- format(date_values, "%d/%m/%Y")
            } else if (date_format == "ymd") {
              df[[date_field]] <- format(date_values, "%Y-%m-%d")
            }
            message(sprintf("  Converted %s from numeric to %s format", date_field, date_format))
          }
          # Check if they are already in a date-like format
          else if (any(grepl("-|/", dates_char[!is.na(dates_char)]))) {
            # Try to parse existing dates and reformat them
            parsed_dates <- tryCatch({
              # Try different parsing approaches
              if (all(grepl("/", dates_char[!is.na(dates_char)]))) {
                # Likely MM/DD/YYYY or DD/MM/YYYY
                parts <- strsplit(dates_char[!is.na(dates_char)][1], "/")[[1]]
                if (length(parts) == 3) {
                  if (as.numeric(parts[1]) > 12) { # DD/MM/YYYY
                    as.Date(dates_char, format = "%d/%m/%Y")
                  } else { # Default to MM/DD/YYYY
                    as.Date(dates_char, format = "%m/%d/%Y")
                  }
                }
              } else if (all(grepl("-", dates_char[!is.na(dates_char)]))) {
                # Likely YYYY-MM-DD
                as.Date(dates_char)
              } else {
                # Try generic parsing with lubridate
                if (date_format == "mdy") {
                  lubridate::mdy(dates_char)
                } else if (date_format == "dmy") {
                  lubridate::dmy(dates_char)
                } else {
                  lubridate::ymd(dates_char)
                }
              }
            }, error = function(e) {
              # If parsing fails, return NA
              message(sprintf("  Warning: Could not parse dates in %s, leaving as is", date_field))
              return(NULL)
            })
            if (!is.null(parsed_dates)) {
              # Format according to preference
              if (date_format == "mdy") {
                df[[date_field]] <- format(parsed_dates, "%m/%d/%Y")
              } else if (date_format == "dmy") {
                df[[date_field]] <- format(parsed_dates, "%d/%m/%Y")
              } else if (date_format == "ymd") {
                df[[date_field]] <- format(parsed_dates, "%Y-%m-%d")
              }
              message(sprintf("  Reformatted %s to %s format", date_field, date_format))
            }
          }
        }
      }
    }
  }

  # For interview_age columns
  age_cols <- grep("_interview_age$", base::names(df))
  if (length(age_cols) > 0) {
    base::names(df)[age_cols] <- "interview_age"
  }

  # For interview_date columns - more robust handling
  date_patterns <- c("_interview_date$", "interview_date")
  date_cols <- NULL
  for (pattern in date_patterns) {
    found_cols <- grep(pattern, base::names(df), ignore.case = TRUE)
    if (length(found_cols) > 0) {
      date_cols <- found_cols
      break  # Stop at first pattern that finds matches
    }
  }
  if (!is.null(date_cols) && length(date_cols) > 0) {
    base::names(df)[date_cols] <- "interview_date"
  }

  # Apply redcap_event_name filter if specified
  if (!is.null(redcap_event_name)) {
    if (!"redcap_event_name" %in% names(df)) {
      stop("Cannot filter by redcap_event_name: column not found in data")
    }
    # Use %in% to support both single strings and vectors
    df <- df[df$redcap_event_name %in% redcap_event_name, ]
  }

  # Study-specific processing (legacy - errors should be fixed in redcap...)
  if (config$study_alias == "capr") {
    df <- processCaprData(df, instrument_name)
  }

  # Make a final pass to remove any PII fields that might have been included
  if (!pii && length(pii_fields) > 0) {
    pii_cols_present <- intersect(names(df), pii_fields)
    pii_cols_present <- pii_cols_present[!is.na(pii_cols_present)]
    if (length(pii_cols_present) > 0) {
      message(sprintf("\nRemoving %d PII fields from final dataset: %s",
                      length(pii_cols_present),
                      paste(pii_cols_present, collapse = ", ")))
      df <- df[, !names(df) %in% pii_fields, drop = FALSE]
    }
  }

  # Create a copy of the original dataframe to preserve original values
  original_df <- df

  # Advanced date parsing function that handles multiple formats
  parseAnyDate <- function(date_string) {
    if (is.na(date_string) || is.null(date_string)) {
      return(NA)
    }
    # Try multiple date formats sequentially
    date <- NULL
    # Try ISO format (YYYY-MM-DD)
    if (grepl("^\\d{4}-\\d{1,2}-\\d{1,2}$", date_string)) {
      date <- tryCatch(lubridate::ymd(date_string), error = function(e) NULL)
    }
    # Try US format (MM/DD/YYYY)
    else if (grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", date_string)) {
      date <- tryCatch(lubridate::mdy(date_string), error = function(e) NULL)
    }
    # Try European format (DD.MM.YYYY)
    else if (grepl("^\\d{1,2}\\.\\d{1,2}\\.\\d{4}$", date_string)) {
      date <- tryCatch(lubridate::dmy(date_string), error = function(e) NULL)
    }
    # Try Canadian format (YYYY/MM/DD)
    else if (grepl("^\\d{4}/\\d{1,2}/\\d{1,2}$", date_string)) {
      date <- tryCatch(lubridate::ymd(date_string), error = function(e) NULL)
    }
    # Try other format (DD-MM-YYYY)
    else if (grepl("^\\d{1,2}-\\d{1,2}-\\d{4}$", date_string)) {
      date <- tryCatch(lubridate::dmy(date_string), error = function(e) NULL)
    }
    # Try abbreviated month name (15-Jan-2023 or Jan 15, 2023)
    else if (grepl("[A-Za-z]", date_string)) {
      date <- tryCatch(lubridate::parse_date_time(date_string, c("dmy", "mdy")), error = function(e) NULL)
    }
    # If all attempts fail, return NA
    if (is.null(date) || all(is.na(date))) {
      warning("Failed to parse date: ", date_string, ". Treating as NA.")
      return(NA)
    }
    return(as.Date(date))
  }

  # Handle interview_date filtering
  if ("interview_date" %in% names(df)) {
    # Create a temporary date column for filtering but don't modify the original
    df$temp_date <- sapply(df$interview_date, parseAnyDate)
    # Handle the interview_date parameter
    if (!is.null(interview_date)) {
      if (is.logical(interview_date) && interview_date == TRUE) {
        # Keep only rows with non-NA interview_date values
        rows_to_keep <- !is.na(df$temp_date)
        df <- df[rows_to_keep, ]
        original_df <- original_df[rows_to_keep, ]
      } else if (is.character(interview_date) || inherits(interview_date, "Date")) {
        # Filter by specific date
        input_date <- tryCatch({
          if (inherits(interview_date, "Date")) {
            interview_date
          } else {
            parseAnyDate(interview_date)
          }
        }, error = function(e) {
          stop("Failed to parse interview_date parameter: ", interview_date)
        })
        if (is.na(input_date)) {
          stop("Failed to parse interview_date parameter: ", interview_date)
        }
        # If we're here with a valid date, keep rows with dates before or equal to input_date
        rows_to_keep <- !is.na(df$temp_date) & df$temp_date <= input_date
        df <- df[rows_to_keep, ]
        original_df <- original_df[rows_to_keep, ]
      } else {
        stop("interview_date must be either a date string or TRUE")
      }
    }
    # Remove temporary column after filtering
    df$temp_date <- NULL
  }

  # Reorder columns to put superkey columns first
  if (ncol(df) > 0) {
    # Get names of all superkey columns that are in the dataset
    superkey_cols_in_data <- intersect(
      c(config$redcap$primary_key, "redcap_event_name", super_key_cols),
      names(df)
    )
    # Get names of all instrument columns
    instrument_cols <- setdiff(names(df), superkey_cols_in_data)
    # Create the new column order
    new_col_order <- c(superkey_cols_in_data, instrument_cols)
    # Reorder the dataframe
    df <- df[, new_col_order, drop = FALSE]
  }

  # Attach the instrument name as an attribute without an extra parameter
  attr(df, "redcap_instrument") <- instrument_name

  # allow sort by complete
  if (!is.null(instrument_name) && !is.null(complete)) {
    # Construct the complete variable name based on instrument_name
    complete_var <- paste0(instrument_name, "_complete")
    # Check if the complete variable exists in the dataframe
    if (complete_var %in% names(df)) {
      # Check for NA values in the complete variable
      na_counts <- sum(is.na(df[[complete_var]]))
      if (na_counts > 0) {
        message(sprintf("Note: Found %d rows with NA values in %s", na_counts, complete_var))
      }
      # If complete is TRUE, keep only complete records
      # If complete is FALSE, keep only incomplete records
      if (is.logical(complete)) {
        if (complete) {
          message("Filtering for complete records only...")
          # Handle both raw (1) and label ("Complete") formats
          if (raw_or_label == "raw") {
            df <- df[df[[complete_var]] == 2, ]
          } else {
            df <- df[df[[complete_var]] == "Complete", ]
          }
        } else {
          message("Filtering for incomplete records only...")
          # Handle both raw (0) and label ("Incomplete") formats
          if (raw_or_label == "raw") {
            df <- df[df[[complete_var]] == 0, ]
          } else {
            df <- df[df[[complete_var]] == "Incomplete", ]
          }
        }
      } else {
        warning("The 'complete' parameter should be TRUE or FALSE. Ignoring filter.")
      }
    } else {
      warning(paste0("Complete variable '", complete_var, "' not found in the dataset. Ignoring filter."))
    }
  }

  # Check if any column requests were passed via ...
  dots_args <- list(...)
  if (length(dots_args) > 0) {
    # Convert the dots arguments to a character vector
    requested_cols <- as.character(unlist(dots_args))
    # Find which of the requested columns actually exist in the data
    existing_cols <- intersect(requested_cols, names(df))
    if (length(existing_cols) > 0) {
      # Display the names of the columns that were found
      message(sprintf("Found %d of %d requested columns: %s",
                      length(existing_cols),
                      length(requested_cols),
                      paste(existing_cols, collapse = ", ")))
      # Create a filter to keep only rows where ALL requested columns have data
      rows_to_keep <- rep(TRUE, nrow(df))
      for (col in existing_cols) {
        # Check if column values are not NA
        not_na <- !is.na(df[[col]])
        # For non-NA values, check if they're not empty strings (if character type)
        not_empty <- rep(TRUE, nrow(df))
        if (is.character(df[[col]])) {
          not_empty <- df[[col]] != ""
        }
        # Combine the conditions - both not NA and not empty (if applicable)
        has_data <- not_na & not_empty
        # Update the rows_to_keep vector
        rows_to_keep <- rows_to_keep & has_data
      }
      # Apply the filter to keep only rows with data in all requested columns
      original_rows <- nrow(df)
      df <- df[rows_to_keep, ]
      kept_rows <- nrow(df)
      message(sprintf("Kept %d of %d rows where all requested columns have values.",
                      kept_rows, original_rows))
    } else {
      if (length(requested_cols) > 0) {
        warning("None of the requested columns were found in the dataset.")
      }
    }
  }

  # Show duration
  end_time <- Sys.time()
  duration <- difftime(end_time, start_time, units = "secs")
  message(sprintf("\nData frame '%s' retrieved in %s.", instrument_name, formatDuration(duration)))
  return(df)
}


#' Display table of available REDCap instruments and their labels
#'
#' Retrieves a list of all available REDCap forms as a formatted table
#'
#' @return A formatted table (kable) of available REDCap instruments/forms
#' @importFrom REDCapR redcap_instruments
#' @importFrom knitr kable
#' @export
redcap.index <- function() {
  # Load required packages

  # Validate secrets
  tryCatch({
    validate_secrets("redcap")

    # Get secrets using get_secret() to keep it secret, keep it safe
    uri <- get_secret("uri")
    token <- get_secret("token")

  }, error = function(e) {
    message("Error loading or validating REDCap secrets: ", e$message)
    return(NULL)
  })

  # Attempt to fetch instruments from REDCap
  tryCatch({
    forms_result <- REDCapR::redcap_instruments(redcap_uri = uri, token = token, verbose = FALSE)

    # Check if the operation was successful
    if (forms_result$success) {
      return(knitr::kable(forms_result$data, format = "simple"))
    } else {
      message("REDCap API returned an error: ", forms_result$status_message)
      return(NULL)
    }
  }, error = function(e) {
    message("Error connecting to REDCap: ", e$message)
    return(NULL)
  }, warning = function(w) {
    message("Warning during REDCap connection: ", w$message)
  })
}

#' Fetch REDCap data dictionary to be stored in data frame
#'
#' This function extracts metadata/dictionary information from REDCap. It can accept
#' either an instrument name to fetch new data, an existing data frame with instrument
#' attributes, or a variable name as string.
#'
#' @param instrument_name Can either be an instrument name to fetch new data, a data frame
#'   returned by redcap(), or a variable name as string
#' @return A data frame containing the data dictionary/metadata for the specified instrument
#' @importFrom REDCapR redcap_metadata_read
#' @export
redcap.dict <- function(instrument_name) {
  # First handle the case of a non-existent variable being passed without quotes
  var_name <- NULL

  # Only try to get the name if instrument_name is missing
  if (missing(instrument_name)) {
    stop("Instrument name is required")
  }

  # Capture the actual call
  call_expr <- substitute(instrument_name)

  # Check if it's a symbol (variable name) that doesn't exist
  if (is.symbol(call_expr) && !exists(as.character(call_expr))) {
    var_name <- as.character(call_expr)
    message(sprintf("Object '%s' not found, using as instrument name instead.", var_name))
    instrument_name <- var_name
  }

  # Now proceed with normal function logic

  # Check if input is a data frame with redcap_instrument attribute
  if (is.data.frame(instrument_name) && !is.null(attr(instrument_name, "redcap_instrument"))) {
    inst <- attr(instrument_name, "redcap_instrument")
    message(sprintf("Retrieving metadata for instrument '%s' from data frame attributes.", inst))

    # Fetch metadata using the instrument name
    # Validate secrets
    validate_secrets("redcap")

    # Get secrets using get_secret() to keep it secret, keep it safe
    uri <- get_secret("uri")
    token <- get_secret("token")

    metadata <- REDCapR::redcap_metadata_read(
      redcap_uri = uri,
      token = token,
      forms = inst,
      verbose = FALSE
    )$data

    return(metadata)
  }

  # Check if input is a regular data frame
  if (is.data.frame(instrument_name)) {
    message("Using provided REDCap metadata data frame.")
    return(instrument_name)
  }

  # Input is a string
  if (is.character(instrument_name)) {
    # Check if it's a variable name in the global environment
    if (exists(instrument_name)) {
      var_data <- base::get(instrument_name)

      # Check if the variable is a data frame with instrument attribute
      if (is.data.frame(var_data) && !is.null(attr(var_data, "redcap_instrument"))) {
        inst <- attr(var_data, "redcap_instrument")
        message(sprintf("Retrieving metadata for instrument '%s' from variable '%s'.",
                        inst, instrument_name))

        # Fetch metadata using the instrument name
        # Validate secrets
        validate_secrets("redcap")

        # Get secrets using get_secret() to keep it secret, keep it safe
        uri <- get_secret("uri")
        token <- get_secret("token")

        metadata <- REDCapR::redcap_metadata_read(
          redcap_uri = uri,
          token = token,
          forms = inst,
          verbose = FALSE
        )$data

        return(metadata)
      }

      # Check if the variable is just a data frame
      if (is.data.frame(var_data)) {
        message(sprintf("Using existing metadata data frame '%s' from environment.", instrument_name))
        return(var_data)
      }
    }

    # Not a variable or not a data frame, treat as instrument name
    message(sprintf("Fetching metadata for instrument '%s' from REDCap.", instrument_name))
    # Validate secrets
    validate_secrets("redcap")

    # Get secrets using get_secret() to keep it secret, keep it safe
    uri <- get_secret("uri")
    token <- get_secret("token")

    # Fetch metadata from REDCap
    metadata <- REDCapR::redcap_metadata_read(
      redcap_uri = uri,
      token = token,
      verbose = FALSE
    )$data

    dictionary <- metadata[metadata$form_name == instrument_name, ]

    message(sprintf("Retrieved metadata for instrument '%s' from REDCap.", instrument_name))
    return(dictionary)
  }

  # Invalid input type
  stop("Input must be either a data frame, a string variable name, or an instrument name string.")
}

#' Alias for 'redcap' (DEPRECATED)
#'
#' This function is deprecated. Please use 'redcap' instead.
#' This is a legacy alias for the 'redcap' function to maintain compatibility with older code.
#'
#' @inheritParams redcap
#' @inherit redcap return
#' @export
#' @examples
#' \dontrun{
#' # DEPRECATED - use redcap() instead
#' survey_data <- getRedcap("demographics")
#' }
getRedcap <- function(...) {
  .Deprecated("redcap", package = "wizaRdry")
  redcap(...)
}
