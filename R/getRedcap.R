#
# function: getRedcap(instrument_name)
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

#' Get Data from REDCap
#'
#' Retrieves data from a REDCap instrument and ensures subject identifiers
#' are propagated across all events
#'
#' @param instrument_name Name of the REDCap instrument
#' @param raw_or_label Whether to return raw or labeled values
#' @param redcap_event_name Optional event name filter
#' @param batch_size Number of records to retrieve per batch
#' @param records Optional vector of specific record IDs
#' @param fields Optional vector of specific fields
#'
#' @return A data frame containing the requested REDCap data
#' @export
#' @examples
#' \dontrun{
#' # Get data from a specific instrument
#' data <- getRedcap("demographics")
#' }
getRedcap <- function(instrument_name = NULL, raw_or_label = "raw",
                      redcap_event_name = NULL, batch_size = 1000,
                      records = NULL, fields = NULL) {
  
  start_time <- Sys.time()
  
  
  # Validate secrets and config
  validate_secrets("redcap")
  
  config <- validate_config("redcap")
  
  # Input validation
  if (is.null(instrument_name)) {
    forms_data <- REDCapR::redcap_instruments(redcap_uri = uri, token = token, verbose = FALSE)$data
    forms_filtered <- forms_data[!grepl("nda", forms_data$instrument_name), ]
    random_instrument <- sample(forms_filtered$instrument_name, 1)
    forms_table <- paste(capture.output(message(getRedcapForms())), collapse = "\n")
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
  
  # Progress bar
  pb <- initializeLoadingAnimation(20)
  message(sprintf("\nImporting records from REDCap form: %s%s", 
                  instrument_name, 
                  ifelse(!is.null(redcap_event_name), 
                         sprintf(" %s", redcap_event_name), 
                         "")))
  for (i in 1:20) {
    updateLoadingAnimation(pb, i)
    Sys.sleep(0.1)
  }
  completeLoadingAnimation(pb)
  message("")
  
  # Try the one-call approach
  response <- REDCapR::redcap_read(
    redcap_uri = uri,
    token = token,
    forms = c(config$redcap$superkey, instrument_name),
    batch_size = batch_size,
    records = records,
    fields = fields,
    raw_or_label = raw_or_label,
    raw_or_label_headers = "raw",
    verbose = FALSE
  )
  
  df <- response$data
  
  # Still propagate super key values across events
  if ("record_id" %in% names(df) && "redcap_event_name" %in% names(df)) {
    # Get metadata for the super_keys form to identify super key fields
    super_key_dict <- tryCatch({
      REDCapR::redcap_metadata_read(
        redcap_uri = uri, 
        token = token,
        forms = config$redcap$superkey,
        verbose = FALSE
      )$data
    }, error = function(e) {
      return(NULL)
    })
    
    # Extract field names from dictionary or use common fields as fallback
    if (!is.null(super_key_dict)) {
      super_key_cols <- super_key_dict$field_name
    }
    
    # Keep only fields that exist in our dataframe
    super_key_cols <- super_key_cols[super_key_cols %in% names(df)]
    
    if (length(super_key_cols) > 0) {
      message("Propagating superkey across all events for each subject...")
      
      # For each subject
      for (subject_id in unique(df$record_id)) {
        # Get all rows for this subject
        subject_rows <- which(df$record_id == subject_id)
        
        # For each super key field, find a non-NA value across all events
        for (key_field in super_key_cols) {
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
    df <- df[df$redcap_event_name == redcap_event_name, ]
  }
  
  # Study-specific processing
  if (config$study_alias == "impact-mh") {
    if ("dob" %in% colnames(df)) {
      df <- subset(df, select = -dob)
    }
  }
  
  if (config$study_alias == "capr") {
    df <- processCaprData(df, instrument_name)
  }
  
  # Attach the instrument name as an attribute without an extra parameter
  attr(df, "redcap_instrument") <- instrument_name
  
  # Show duration
  end_time <- Sys.time()
  duration <- difftime(end_time, start_time, units = "secs")
  message(sprintf("\nData frame '%s' retrieved in %s.", instrument_name, formatDuration(duration)))
  
  return(df)
  
}


#' Get Available REDCap Forms
#' 
#' Retrieves a list of all available REDCap forms as a formatted table
#' 
#' @return A formatted table (kable) of available REDCap instruments/forms
#' @importFrom REDCapR redcap_instruments
#' @importFrom knitr kable
#' @export
getRedcapForms <- function() {

  # Validate secrets
  validate_secrets("redcap")
  
  forms <- REDCapR::redcap_instruments(redcap_uri = uri, token = token, verbose = FALSE)$data
  
  # Option 1: Using knitr::kable for a clean table
  return(knitr::kable(forms, format = "simple"))
}

#' Extract Dictionary from REDCap Data
#'
#' This function extracts metadata/dictionary information from REDCap. It can accept
#' either an instrument name to fetch new data, an existing data frame with instrument
#' attributes, or a variable name as string.
#'
#' @param redcap_data Can either be an instrument name to fetch new data, a data frame 
#'   returned by getRedcap(), or a variable name as string
#' @return A data frame containing the data dictionary/metadata for the specified instrument
#' @importFrom REDCapR redcap_metadata_read
#' @export
getRedcapDictionary <- function(redcap_data) {
  # Check if input is a data frame with redcap_instrument attribute
  if (is.data.frame(redcap_data) && !is.null(attr(redcap_data, "redcap_instrument"))) {
    instrument_name <- attr(redcap_data, "redcap_instrument")
    message(sprintf("Retrieving metadata for instrument '%s' from data frame attributes.", instrument_name))
    
    # Fetch metadata using the instrument name
    # Validate secrets
    validate_secrets("redcap")
    
    metadata <- REDCapR::redcap_metadata_read(
      redcap_uri = uri, 
      token = token,
      forms = instrument_name,
      verbose = FALSE
    )$data
    
    return(metadata)
  }
  
  # Check if input is a regular data frame
  if (is.data.frame(redcap_data)) {
    message("Using provided REDCap metadata data frame.")
    return(redcap_data)
  }
  
  # Input is a string
  if (is.character(redcap_data)) {
    # Check if it's a variable name in the global environment
    if (exists(redcap_data)) {
      var_data <- base::get(redcap_data)
      
      # Check if the variable is a data frame with instrument attribute
      if (is.data.frame(var_data) && !is.null(attr(var_data, "redcap_instrument"))) {
        instrument_name <- attr(var_data, "redcap_instrument")
        message(sprintf("Retrieving metadata for instrument '%s' from variable '%s'.", 
                        instrument_name, redcap_data))
        
        # Fetch metadata using the instrument name
        # Validate secrets
        validate_secrets("redcap")
        
        metadata <- REDCapR::redcap_metadata_read(
          redcap_uri = uri, 
          token = token,
          forms = instrument_name,
          verbose = FALSE
        )$data
        
        return(metadata)
      }
      
      # Check if the variable is just a data frame
      if (is.data.frame(var_data)) {
        message(sprintf("Using existing metadata data frame '%s' from environment.", redcap_data))
        return(var_data)
      }
    }
    
    # Not a variable or not a data frame, treat as instrument name
    # Validate secrets
    validate_secrets("redcap")
    
    # Fetch metadata from REDCap
    metadata <- REDCapR::redcap_metadata_read(
      redcap_uri = uri, 
      token = token,
      verbose = FALSE
    )$data
    
    dictionary <- metadata[metadata$form_name == redcap_data, ]
    
    message(sprintf("Retrieved metadata for instrument '%s' from REDCap.", redcap_data))
    return(dictionary)
  }
  
  # Invalid input type
  stop("Input must be either a data frame, a string variable name, or an instrument name string.")
}


#' Alias for 'getRedcap'
#'
#' This is a legacy alias for the 'getRedcap' function to maintain compatibility with older code.
#'
#' @inheritParams getRedcap
#' @inherit getRedcap return
#' @export
#' @examples
#' \dontrun{
#' survey_data <- redcap("demographics")
#' }
redcap <- getRedcap

#' Alias for 'getRedcapForms'
#'
#' This is a legacy alias for the 'getRedcapForms' function to maintain compatibility with older code.
#'
#' @inherit getRedcapForms return
#' @export
#' @examples
#' \dontrun{
#' redcap.index()
#' }
redcap.index <- getRedcapForms

#' Alias for 'getRedcapDictionary'
#'
#' This is a legacy alias for the 'getRedcapDictionary' function to maintain compatibility with older code.
#'
#' @inheritParams getRedcapDictionary
#' @inherit getRedcapDictionary return
#' @export
#' @examples
#' \dontrun{
#' instrument_dict <- redcap.codex()
#' }
redcap.codex <- getRedcapDictionary
