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
  
#   lapply(list.files("api/src", pattern = "\\.R$", full.names = TRUE), base::source)
  
  # Get configuration
  cfg <- config::get()
  
  # Get survey ID
  survey_id <- NULL
  
  if (!is.null(institution)) {
    # Check if institution exists
    if (!(institution %in% names(cfg$qualtrics$survey_ids))) {
      stop(paste("Institution", institution, "not found in configuration"))
    }
    
    # Check if survey exists in specified institution
    if (!(qualtrics_alias %in% names(cfg$qualtrics$survey_ids[[institution]]))) {
      stop(paste("Survey", qualtrics_alias, "not found in institution", institution))
    }
    
    survey_id <- cfg$qualtrics$survey_ids[[institution]][[qualtrics_alias]]
  } else {
    # Search all institutions
    for (inst in names(cfg$qualtrics$survey_ids)) {
      if (qualtrics_alias %in% names(cfg$qualtrics$survey_ids[[inst]])) {
        survey_id <- cfg$qualtrics$survey_ids[[inst]][[qualtrics_alias]]
        institution <- inst
        break
      }
    }
  }
  
  if (is.null(survey_id)) {
    stop(paste("Survey", qualtrics_alias, "not found in any institution"))
  }
  
  message(sprintf("Retrieving %s survey from %s", qualtrics_alias, institution))
  
  # Connect to Qualtrics
  connectQualtrics(qualtrics_alias)
  
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

# ################ #
# Helper Functions #
# ################ #

#' Connect to Qualtrics API
#'
#' This helper function sets up the connection to the Qualtrics API using credentials stored in a file or environment variables.
#' It is called internally by the 'getSurvey' function.
#'
#' @param qualtrics_alias The alias for the Qualtrics survey to connect to.
#' @importFrom config get
#' @import qualtRics
#' @noRd
connectQualtrics <- function(qualtrics_alias) {
  
  # Validate secrets
#   base::source("api/SecretsEnv.R")
  validate_secrets("qualtrics")
  
  # Validate config
#   base::source("api/ConfigEnv.R")
  validate_config("qualtrics")
  
  #base::source(config$qualtrics$survey_ids)
  # NEW CODE
  cfg <- config::get()
  for (inst in names(cfg$qualtrics$survey_ids)) {
    if (qualtrics_alias %in% names(cfg$qualtrics$survey_ids[[inst]])) {
      survey_id <- cfg$qualtrics$survey_ids[[inst]][[qualtrics_alias]]
      break
    }
  }
  
  #if (!(qualtrics_alias %in% names(surveyIds))) {
  #  stop("Provided qualtrics_alias does not match any survey IDs.")
  #}
  
  if (!exists("apiKeys") || !exists("baseUrls")) {
    stop("apiKeys and/or baseUrls arrays not found in secrets.R")
  }
  if (length(apiKeys) != length(baseUrls)) {
    stop("apiKeys and baseUrls arrays must have the same length")
  }
  
  for (i in seq_along(apiKeys)) {
    tryCatch({
      qualtRics::qualtrics_api_credentials(
        api_key = apiKeys[i],
        base_url = baseUrls[i],
        install = TRUE,
        overwrite = TRUE
      )
      return(TRUE)
    }, error = function(e) {
      if (i == length(apiKeys)) {
        stop("Failed to connect with any credentials")
      }
    })
  }
}

#' Retrieve Data from Qualtrics
#'
#' Fetches survey data from Qualtrics based on the survey alias and label preference. 
#' It attempts to fetch survey data and handle any errors that occur.
#'
#' @param qualtrics_alias The alias for the Qualtrics survey whose data is to be fetched.
#' @param label Logical indicating whether to fetch choice labels instead of coded values.
#' @return Data frame containing survey data, or NULL in case of error.
#' @importFrom qualtRics fetch_survey
#' @noRd
getQualtricsData <- function(qualtrics_alias, label) {
  tryCatch({
    
    # Validate config
#     base::source("api/ConfigEnv.R")
    validate_config("qualtrics")
    
    df <- qualtRics::fetch_survey(
      surveyID = toString(surveyIds[qualtrics_alias]),
      verbose = FALSE,
      label = label,
      convert = label,
      add_column_map = TRUE
    )
    if (!is.data.frame(df)) {
      stop(paste("fetch_survey did not return a data frame for", qualtrics_alias))
    }
    return(df)
  }, error = function(e) {
    message("Error in getQualtricsData: ", e$message)
    return(NULL)
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
    stop("Input to qualtricsHarmonization is not a data frame")
  }
  
  # check for visit variable, if not add baseline
  if ("visit" %!in% colnames(df)) {
    df$visit <- "bl"
  }
  
  # if visit variable exists, make sure they are named according to convention
  if ("visit" %in% colnames(df)) {
    df$visit <- ifelse(is.na(df$visit), "bl", ifelse(df$visit == "0", "bl", ifelse(df$visit == "12", "12m", ifelse(df$visit == "24", "24m", df$visit))))
  }
  
  # df$src_subject_id <- as.numeric(df$src_subject_id)
  
  # convert dates
  # df$interview_date <- as.Date(df$interview_date, "%m/%d/%Y")
  
  # add measure column
  # df$measure <- qualtrics_alias
  
  # select visit
  # df <- df[df$visit==visit,]
  
  suppressWarnings(return(df))
  # comment into add prefixes (will break code)
  #suppressWarnings(return(addPrefixToColumnss(df,qualtrics_alias)))
}

#' Extract Column Mapping from Qualtrics Data Frame
#'
#' This function extracts column mappings from the metadata of a Qualtrics survey data frame.
#'
#' @param qualtrics_df Data frame obtained from Qualtrics.
#' @return A list containing the mappings of column names to survey questions.
#' @noRd
getDictionary <- function(qualtrics_df) {
  return(qualtRics::extract_colmap(respdata = qualtrics_df))
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
