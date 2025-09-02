#' Process CAPR data
#' 
#' @param df Data frame containing CAPR data
#' @param instrument_name Name of the instrument
#' @return Processed data frame
#' @importFrom dplyr filter select mutate group_by first contains
#' @importFrom rlang .data
#' @noRd
processCaprData <- function(df, instrument_name) {
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
  
  # recode phenotype (only need to recode phenotypes as 4 (ineligible) and 5 (withdrawn) have been removed in previous line)
  if ("phenotype" %in% names(df)) {
    df <- df %>% dplyr::mutate(phenotype = ifelse(is.na(.data$phenotype), NA,
                                                  ifelse(.data$phenotype == 1, "hc",
                                                         ifelse(.data$phenotype == 2, "chr",
                                                                ifelse(.data$phenotype == 3, "hsc", 
                                                                       ifelse(.data$phenotype == 4, "ineligible",
                                                                              ifelse(.data$phenotype == 5, "withdrawn", NA)))))))
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
    
    # Remove rows where phenotype is 'ineligible' or 'withdrawn'
    df <- df[!(df$phenotype %in% c("ineligible", "withdrawn")), ]
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
  
  # convert dates
  if ("int_start" %in% names(df) && "int_end" %in% names(df)) {
    df$int_diff <- as.numeric(df$int_end - df$int_start)
    df$interview_date <- df$int_start
  } else {
    message("Warning: int_start or int_end columns not found. Skipping interview date processing.")
    df$int_diff <- NA_real_
    df$interview_date <- NA
  }
  
  # remove dob
  if ("subject_dob" %in% names(df)) {
    df <- subset(df, select = -subject_dob)
  }
  
  return(df)
}
