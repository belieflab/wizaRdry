#' Process CAPR data
#' 
#' @param df Data frame containing CAPR data
#' @param instrument_name Name of the instrument
#' @return Processed data frame
#' @importFrom dplyr filter select mutate group_by first contains
#' @importFrom rlang .data

#' @noRd
processCaprData <- function(df, instrument_name) {
  # Debug: Print available columns
  message("Debug: Available columns in processCaprData:")
  message(paste("Columns:", paste(names(df), collapse = ", ")))
  message(paste("Number of rows:", nrow(df)))
  
  # Convert and filter src_subject_id
  if ("src_subject_id" %in% names(df)) {
    df$src_subject_id <- as.numeric(df$src_subject_id)
    # df <- filter(df, between(df$src_subject_id, 10000, 71110)) # between seems() to cause error
    # might be less flexible character to numeric
    # src_subject_id may download as character sometimes
    df <- dplyr::filter(df, .data$src_subject_id > 10000, .data$src_subject_id < 79110)
  } else {
    stop("src_subject_id column is required but not found in the data")
  }
  # include guard clauses for mesaures that require aditional filtering beyond form name
  if (instrument_name == "scid_scoresheet") {
    df <- df %>% dplyr::select(contains(c("src_subject_id", "redcap_event_name", "scid_", "scip_", "mdd_", "pdd_"))) # scid_p18a was misspelled in the dataframe, that is why there is a "scip" variable :)
  }
  df$src_subject_id <- as.character(df$src_subject_id)
  
  # create a visit variable based on redcap_event_name
  ## not over-writing with rename(), so that redcap_event_name can do a "soft retire"
  if ("redcap_event_name" %in% names(df)) {
    df <- df %>% dplyr::mutate(visit = .data$redcap_event_name)
  } else {
    message("Warning: redcap_event_name column not found. Creating placeholder visit column.")
    df <- df %>% dplyr::mutate(visit = NA_character_)
  }
  
  # align redcap_event_name-ing convention with more natural language
  df <- df %>% dplyr::mutate(visit = ifelse(.data$visit == "baseline_arm_1", "bl",
                                            ifelse(.data$visit == "12m_arm_1", "12m",
                                                   ifelse(.data$visit == "24m_arm_1", "24m", NA)
                                            )
  ))
  
  # Check phenotype data - CAPR uses labels by default, so no recoding needed
  if ("phenotype" %in% names(df)) {
    message("Debug: Phenotype column found. Sample values:")
    message(paste("Unique phenotype values:", paste(unique(df$phenotype), collapse = ", ")))
    message(paste("Number of non-NA phenotypes:", sum(!is.na(df$phenotype))))
    
    # No recoding needed since we're getting label data (e.g., "HC", "CHR", "HSC")
    # Just ensure it's a character column
    df$phenotype <- as.character(df$phenotype)
  } else {
    message("Warning: phenotype column not found in data. Creating placeholder phenotype column.")
    df <- df %>% dplyr::mutate(phenotype = NA_character_)
  }
  
  #make sure phenotype doesn't change after baseline visit
  if ("phenotype" %in% names(df) && any(!is.na(df$phenotype))) {
    df <- df %>% 
      dplyr::mutate(visit = factor(.data$visit, levels = c('bl','12m','24m')),
                    phenotype = factor(.data$phenotype)) %>%
      dplyr::group_by(.data$src_subject_id) %>%  
      dplyr::mutate(baseline_pheno = dplyr::first(.data$phenotype)) %>% 
      dplyr::mutate(phenotype = .data$baseline_pheno) %>% 
      dplyr::select(-.data$baseline_pheno) 
    
    # Remove rows where phenotype is NA
    # but first print warning and say how many folks are getting removed
    phenotype_nas <- df[is.na(df$phenotype),]
    if (nrow(phenotype_nas) > 0) {
      message(paste0('removing ', nrow(phenotype_nas),
                   ' subjects because they have NA for phenotype. This generally',
                   ' should not happen. Below are the subject IDs and visit dates ',
                   'for these people. They should be inspected and fixed in redcap'))
      message(paste0(phenotype_nas$src_subject_id, ' ', phenotype_nas$visit))
      df <- df[!is.na(df$phenotype), ]
    }
    
    # Remove rows where phenotype is 'ineligible' or 'withdrawn' (if these labels exist)
    # Note: CAPR might use different labels, so we check what's actually in the data
    ineligible_labels <- c("Ineligible", "Withdrew")
    existing_ineligible <- intersect(unique(df$phenotype), ineligible_labels)
    if (length(existing_ineligible) > 0) {
      message(paste("Removing phenotypes:", paste(existing_ineligible, collapse = ", ")))
      df <- df[!(df$phenotype %in% existing_ineligible), ]
    }
  } else {
    message("Warning: No valid phenotype data found. Skipping phenotype processing.")
  }
  
  # create a site variable based on src_institution_name
  ## not over-writing with rename(), so that redcap_event_name can do a "soft retire"
  if ("src_institution_id" %in% names(df)) {
    df <- df %>% dplyr::mutate(site = .data$src_institution_id)
    # get rid of deprecated variable names is good practice
    df <- subset(df, select = -src_institution_id)
  } else {
    # If src_institution_id doesn't exist, create a placeholder site column
    message("Warning: src_institution_id column not found in data. Creating placeholder site column.")
    df <- df %>% dplyr::mutate(site = NA_character_)
  }
  
  # convert dates - CAPR stores interview dates in superkey, not in individual instruments
  # Note: Date formatting is done in getRedcap before this function, so columns may already be Date objects
  if ("int_start" %in% names(df) && "int_end" %in% names(df)) {
    message("Debug: Interview date columns found:")
    message(paste("int_start sample values:", paste(head(df$int_start, 3), collapse = ", ")))
    message(paste("int_end sample values:", paste(head(df$int_end, 3), collapse = ", ")))
    message(paste("int_start class:", class(df$int_start)[1]))
    message(paste("int_end class:", class(df$int_end)[1]))
    
    # Handle both Date objects and character/numeric dates
    if (inherits(df$int_start, "Date") && inherits(df$int_end, "Date")) {
      # Dates are already properly formatted
      df$int_diff <- as.numeric(df$int_end - df$int_start)
      df$interview_date <- df$int_start
      message("Using pre-formatted Date objects for calculations")
    } else {
      # Convert to dates if they're not already
      df$int_start <- as.Date(df$int_start)
      df$int_end <- as.Date(df$int_end)
      df$int_diff <- as.numeric(df$int_end - df$int_start)
      df$interview_date <- df$int_start
      message("Converted columns to Date objects for calculations")
    }
  } else if ("interview_age" %in% names(df)) {
    message("Debug: CAPR structure detected - interview_age found")
    message(paste("interview_age sample values:", paste(head(df$interview_age, 3), collapse = ", ")))
    
    # Since interview_age is calculated from interview_date - subject_dob,
    # we should try to reconstruct interview_date if possible
    if ("subject_dob" %in% names(df)) {
      message("Attempting to reconstruct interview_date from interview_age and subject_dob")
      
      # Convert subject_dob to Date if it isn't already
      if (!inherits(df$subject_dob, "Date")) {
        df$subject_dob <- as.Date(df$subject_dob)
      }
      
      # Calculate interview_date: subject_dob + interview_age
      # Note: interview_age is in months with >=15 days rounded up
      # Use base R date arithmetic: add months by converting to days
      df$interview_date <- df$subject_dob + (df$interview_age * 30.44)
      df$int_diff <- NA_real_  # Can't calculate duration without start/end times
      
      message("Reconstructed interview_date from interview_age and subject_dob")
    } else {
      message("Warning: subject_dob not found, cannot reconstruct interview_date")
      df$int_diff <- NA_real_
      df$interview_date <- NA
    }
  } else {
    message("Warning: Neither interview date columns nor interview_age found. Skipping interview date processing.")
    message(paste("Available columns:", paste(names(df), collapse = ", ")))
    df$int_diff <- NA_real_
    df$interview_date <- NA
  }
  
  # Clean up CAPR-specific columns that are no longer needed
  columns_to_remove <- c("subject_dob", "group_status")
  existing_columns_to_remove <- intersect(names(df), columns_to_remove)
  
  if (length(existing_columns_to_remove) > 0) {
    message("Cleaning up CAPR-specific columns: ", paste(existing_columns_to_remove, collapse = ", "))
    df <- df[, !names(df) %in% existing_columns_to_remove, drop = FALSE]
  }
  
  # Standardize phenotype and sex values
  if ("phenotype" %in% names(df)) {
    df$phenotype <- tolower(df$phenotype)
    message("Converted phenotype values to lowercase")
  }
  
  if ("sex" %in% names(df)) {
    df$sex <- ifelse(df$sex == "Male", "M", 
                     ifelse(df$sex == "Female", "F", df$sex))
    message("Recoded sex values: Male -> M, Female -> F")
  }
  
  # Reorder columns to put calculated/important fields first
  calculated_fields <- c("phenotype", "site", "visit", "interview_date", "int_diff")
  existing_calculated_fields <- intersect(names(df), calculated_fields)
  other_fields <- setdiff(names(df), existing_calculated_fields)
  
  if (length(existing_calculated_fields) > 0) {
    message("Reordering columns to put calculated fields first")
    df <- df[, c(existing_calculated_fields, other_fields)]
  }
  
  return(df)
}
