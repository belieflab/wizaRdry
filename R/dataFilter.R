#' Data Filter
#'
#' Filters a dataframe based on specified 'visit', 'week', and 'state' parameters and 
#' selects columns based on 'columns_of_interest'. If 'columns_of_interest' is not provided,
#' retains all columns by default. Filters by 'visit' and 'week' if these columns exist
#' and values for them are provided.
#'
#' @param df Dataframe to be filtered and trimmed based on the provided parameters.
#' @param rows_of_interest Optional; a vector of row names to be retained in the final output. 
#'        If NULL or empty, all rows in the dataframe are retained.
#' @param columns_of_interest Optional; a vector of column names to be retained in the final output. 
#'        If NULL or empty, all columns in the dataframe are retained.
#' @param visit Optional; a specific visit value to filter the dataframe by. Only used if 
#'        'visit' column exists in the dataframe.
#' @param week Optional; a specific week value to filter the dataframe by. Only used if 
#'        'week' column exists in the dataframe.
#' @param states Optional; a vector of state conditions to filter the dataframe by. Only used 
#'        if 'state' column exists in the dataframe. Can include values like 'complete', 
#'        'completed baseline', 'completed 12m', 'completed 24m', etc.
#' @param arm Optional; a condition specified as part of a trial e.g. drug, placebo
#' @param site Optional; a site like Yale, NU 
#' @param sex Optional; a string of sex at birth 'M', 'F'
#' @param phenotype Optional; a string of phenotype to drill down
#' @param interview_date Optional; a string in MM/DD/YYYY format to lock data
#'
#' @return A filtered dataframe based on the provided 'visit', 'week', and 'state' parameters, 
#'         and containing only the columns specified in 'columns_of_interest'. If no columns 
#'         are specified, returns the entire dataframe with applied row filters.
#'
#' @examples
#' \dontrun{
#' filtered <- dataFilter(df, 
#'                             rows_of_interest = c("foo","bar"),
#'                             columns_of_interest = c("src_subject_id", "phenotype"), 
#'                             visit = 2, 
#'                             states = c("complete", "completed baseline"))
#' }
#' @import dplyr
#' @export
dataFilter <- function(df, rows_of_interest = NULL, columns_of_interest = NULL,
                       visit = NULL, week = NULL, states = NULL, arm = NULL, site = NULL,
                       sex = NULL, phenotype = NULL,
                       interview_date = NULL) {
  
#   if (!require(dplyr, quietly = TRUE)) {install.packages("dplyr")}; library(dplyr)
#   if (!require(lubridate, quietly = TRUE)) {install.packages("lubridate")}; library(lubridate)
  
  # print("Initial dataframe head:")
  # print(head(df))
  
  parseAnyDate <- function(date_string) {
    if (grepl("-", date_string)) {
      date <- tryCatch(ymd(date_string), error = function(e) NULL)
    } else if (grepl("/", date_string)) {
      date <- tryCatch(mdy(date_string), error = function(e) NULL)
    } else {
      date <- NULL
    }
    if (is.null(date) || any(is.na(date))) {
      stop("Failed to parse date. Please check the format: ", date_string)
    }
    return(date)
  }
  if ("interview_date" %in% names(df) && !is.null(interview_date)) {
    df$interview_date <- sapply(df$interview_date, parseAnyDate)
    input_date <- as.Date(parseAnyDate(interview_date))
    df <- df[df$interview_date <= input_date, ]
  }
  
  if ("state" %in% names(df) && !is.null(states) && length(states) > 0) {
    df <- df[df$state %in% states, ]
  }
  
  if ("visit" %in% names(df) && !is.null(visit)) {
    df <- df[df$visit == visit, ]
  }
  
  if ("week" %in% names(df) && !is.null(week)) {
    df <- df[df$week == week, ]
  }
  
  if ("arm" %in% names(df) && !is.null(arm)) {
    df <- df[df$arm %in% arm, ]
  }
  
  if ("site" %in% names(df) && !is.null(site)) {
    df <- df[df$site %in% site, ]
  }
  
  if ("sex" %in% names(df) && !is.null(sex)) {
    df <- df[df$sex %in% sex, ]
  }
  
  if ("phenotype" %in% names(df) && !is.null(phenotype)) {
    df <- df[df$phenotype %in% phenotype, ]
  }
  
  if (!is.null(columns_of_interest) && length(columns_of_interest) > 0) {
    df <- df[, columns_of_interest, drop = FALSE]
  } else {
    message("No columns of interest provided; all columns will be included.")
  }
  
  if (!is.null(rows_of_interest)) {
    df <- df[rows_of_interest, , drop = FALSE]
  }
  
  return(df)
}
