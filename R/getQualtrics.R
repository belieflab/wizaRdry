#' Retrieve Survey Data from Qualtrics
#'
#' @param qualtrics_alias The alias for the Qualtrics survey to be retrieved.
#' @param institution Optional. The institution name (e.g., "temple" or "nu"). If NULL, all institutions will be searched.
#' @param label Logical indicating whether to return coded values or their associated labels (default is FALSE).
#' @return A cleaned and harmonized data frame containing the survey data.
#' @importFrom dplyr %>% select mutate
#' @export
#' @examples
#' \dontrun{
#' # Get survey by alias (will search all institutions)
#' survey_data <- getQualtrics("rgpts")
#' }
getQualtrics <- function(qualtrics_alias, institution = NULL, label = FALSE) {
  # Load necessary source files
  
  # Validate config
  cfg <- validate_config("qualtrics")
  
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
  
  return(clean_df)
}

#################
## Helper Functions
#################

#' Connect to Qualtrics API
#'
#' This helper function sets up the connection to the Qualtrics API using credentials stored in a file or environment variables.
#' It is called internally by the 'getQualtrics' function.
#'
#' @importFrom config get
#' @import qualtRics
#' @noRd
connectQualtrics <- function() {
  # Validate secrets
  validate_secrets("qualtrics")
  
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
  
  suppressWarnings(return(df))
}

#' Extract Column Mapping from Qualtrics Data Frame
#'
#' This function extracts column mappings from the metadata of a Qualtrics survey data frame.
#' It can accept either a data frame containing Qualtrics data, a variable name as string,
#' or a survey alias string.
#'
#' @param qualtrics_data Can either be an existing dataframe, variable name as string, or survey alias string
#' @return A list containing the mappings of column names to survey questions.
#' @export
getQualtricsDictionary <- function(qualtrics_data) {
  # Check if input is a data frame
  if (is.data.frame(qualtrics_data)) {
    # Input is already a data frame, use it directly
    return(qualtRics::extract_colmap(respdata = qualtrics_data))
  }
  
  # Input is a string
  if (is.character(qualtrics_data)) {
    # First, check if it's a variable name in the global environment
    if (exists(qualtrics_data)) {
      var_data <- base::get(qualtrics_data)
      
      # Check if the variable is a data frame
      if (is.data.frame(var_data)) {
        message(sprintf("Using existing data frame '%s' from environment.", qualtrics_data))
        return(qualtRics::extract_colmap(respdata = var_data))
      }
    }
    
    # Not a variable or not a data frame, treat as survey alias
    # When calling getQualtrics, pass NULL for institution and FALSE for label (default values)
    survey_data <- getQualtrics(qualtrics_data)
    return(qualtRics::extract_colmap(respdata = survey_data))
  }
  
  # Invalid input type
  stop("Input must be either a data frame or a string (survey alias or variable name).")
}

#' Alias for 'getQualtrics'
#'
#' This is a legacy alias for the 'getQualtrics' function to maintain compatibility with older code.
#'
#' @inheritParams getQualtrics
#' @inherit getQualtrics return
#' @export
#' @examples
#' \dontrun{
#' survey_data <- getSurvey("your_survey_alias")
#' }
getSurvey <- getQualtrics

#' Alias for 'getQualtrics'
#'
#' This is a legacy alias for the 'getQualtrics' function to maintain compatibility with older code.
#'
#' @inheritParams getQualtrics
#' @inherit getQualtrics return
#' @export
#' @examples
#' \dontrun{
#' survey_data <- qualtrics("your_survey_alias")
#' }
qualtrics <- getQualtrics

#' Alias for 'getQualtricsDictionary'
#'
#' This is a legacy alias for the 'getQualtricsDictionary' function to maintain compatibility with older code.
#'
#' @inheritParams getQualtricsDictionary
#' @inherit getQualtricsDictionary return
#' @export
#' @examples
#' \dontrun{
#' # Get dictionary from existing data frame
#' survey <- qualtrics("your_survey_alias")
#' survey_dict <- qualtrics_codex(my_survey)
#' # or:
#' # Get dictionary from variable name as string
#' survey_dict <- qualtrics.codex("my_survey")
#' }
qualtrics.codex <- getQualtricsDictionary
