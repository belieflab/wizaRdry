#' Filters a data frame based on specified superkey parameters and well as row and column
#'
#' @param df Dataframe to be filtered and trimmed based on the provided parameters.
#' @param rows Optional; either a single row name or a vector of row names to be retained in the final output. 
#'        If NULL or empty, all rows in the dataframe are retained.
#' @param cols Optional; either a single column name or a vector of column names to be retained in the final output. 
#'        If NULL or empty, all columns in the dataframe are retained.#' Data Filter
#' @param record_id Optional; either a single record_id or a vector of record_ids to filter the dataframe by
#' @param src_subject_id Optional; either a single subject ID or a vector of subject IDs to filter the dataframe by
#' @param subjectkey Optional; either a single subjectkey or a vector of subjectkeys to filter the dataframe by
#' @param site Optional; either a single site value or a vector of site values to filter the dataframe by (e.g., Yale, NU)
#' @param subsiteid Optional; either a single subsiteid or a vector of subsiteids to filter the dataframe by
#' @param sex Optional; either a single sex value or a vector of sex values at birth to filter the dataframe by (e.g., 'M', 'F')
#' @param race Optional; either a single race value or a vector of race values to filter the dataframe by
#' @param ethnic_group Optional; either a single ethnic_group value or a vector of ethnic_group values to filter the dataframe by
#' @param phenotype Optional; either a single phenotype value or a vector of phenotype values to filter the dataframe by
#' @param phenotype_description Optional; either a single phenotype_description or a vector of phenotype_descriptions to filter the dataframe by
#' @param status Optional; either a single status string or a vector of status conditions to filter the dataframe by. Used 
#'        if either 'state' or 'status' column exists in the dataframe. Can include values like 'complete', 
#'        'completed baseline', 'completed 12m', 'completed 24m', etc.
#' @param lost_to_followup Optional; either a single value or a vector of values to filter the dataframe by (checks both 'lost_to_followup' and 'lost_to_follow-up' columns)
#' @param twins_study Optional; either a single twins_study value or a vector of twins_study values to filter the dataframe by
#' @param sibling_study Optional; either a single sibling_study value or a vector of sibling_study values to filter the dataframe by
#' @param family_study Optional; either a single family_study value or a vector of family_study values to filter the dataframe by
#' @param sample_taken Optional; either a single sample_taken value or a vector of sample_taken values to filter the dataframe by
#' @param visit Optional; either a single visit value or a vector of visit values to filter the dataframe by. 
#'        Only used if 'visit' column exists in the dataframe.
#' @param week Optional; either a single week value or a vector of week values to filter the dataframe by. 
#'        Only used if 'week' column exists in the dataframe.
#' @param arm Optional; either a single arm value or a vector of arm values to filter the dataframe by (e.g., drug, placebo)
#' @param interview_date Optional; can be either:
#'        - A date string in various formats (ISO, US, etc.) to filter data up to that date
#'        - A boolean TRUE to return only rows with non-NA interview_date values
#'
#' @return A filtered dataframe based on the provided parameters, 
#'         and containing only the columns specified in 'cols'. If no columns 
#'         are specified, returns the entire dataframe with applied row filters.
#'
#' @examples
#' \donttest{
#' # Create a sample dataframe
#' sample_df <- data.frame(
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
#'   stringsAsFactors = FALSE
#' )
#' 
#' # Set row names for demonstration
#' rownames(sample_df) <- c("foo", "bar", "baz", "qux")
#' 
#' # Filter by specific date
#' filtered1 <- sift(sample_df, 
#'                  cols = c("src_subject_id", "phenotype"), 
#'                  visit = 2, 
#'                  interview_date = "01/31/2023")
#'                  
#' # Filter to include only rows with non-NA interview dates
#' filtered2 <- sift(sample_df, 
#'                  interview_date = TRUE)
#'                  
#' # Filter by status (works with either state or status column)
#' filtered3 <- sift(sample_df,
#'                  status = c("complete", "completed baseline"))
#'                  
#' # Filter with specific row names
#' filtered4 <- sift(sample_df,
#'                  rows = c("foo", "qux"))
#'                  
#' # Filter with vector of visit values
#' filtered6 <- sift(sample_df,
#'                  visit = c(1, 2))
#'                  
#' # Filter by lost_to_followup
#' filtered10 <- sift(sample_df,
#'                  lost_to_followup = FALSE)
#'                  
#' # Filter by src_subject_id
#' filtered11 <- sift(sample_df,
#'                  src_subject_id = c("SUB001", "SUB004"))
#'                  
#' # Multiple filters combined
#' filtered12 <- sift(sample_df,
#'                  site = "Yale",
#'                  visit = 1,
#'                  cols = c("record_id", "src_subject_id", "site"))
#' }
#' @import dplyr
#' @import lubridate
#' @export
sift <- function(df, rows = NULL, cols = NULL,
                 record_id = NULL, src_subject_id = NULL, subjectkey = NULL, 
                 site = NULL, subsiteid = NULL, sex = NULL, race = NULL, 
                 ethnic_group = NULL, phenotype = NULL, phenotype_description = NULL, 
                 status = NULL, lost_to_followup = NULL, twins_study = NULL, 
                 sibling_study = NULL, family_study = NULL, sample_taken = NULL,
                 visit = NULL, week = NULL, arm = NULL, interview_date = NULL) {
  
  
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
      date <- tryCatch(ymd(date_string), error = function(e) NULL)
    } 
    # Try US format (MM/DD/YYYY)
    else if (grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", date_string)) {
      date <- tryCatch(mdy(date_string), error = function(e) NULL)
    } 
    # Try European format (DD.MM.YYYY)
    else if (grepl("^\\d{1,2}\\.\\d{1,2}\\.\\d{4}$", date_string)) {
      date <- tryCatch(dmy(date_string), error = function(e) NULL)
    }
    # Try Canadian format (YYYY/MM/DD)
    else if (grepl("^\\d{4}/\\d{1,2}/\\d{1,2}$", date_string)) {
      date <- tryCatch(ymd(date_string), error = function(e) NULL)
    }
    # Try other format (DD-MM-YYYY)
    else if (grepl("^\\d{1,2}-\\d{1,2}-\\d{4}$", date_string)) {
      date <- tryCatch(dmy(date_string), error = function(e) NULL)
    }
    # Try abbreviated month name (15-Jan-2023 or Jan 15, 2023)
    else if (grepl("[A-Za-z]", date_string)) {
      date <- tryCatch(parse_date_time(date_string, c("dmy", "mdy")), error = function(e) NULL)
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
        
        rows_to_keep <- df$temp_date <= input_date
        df <- df[rows_to_keep, ]
        original_df <- original_df[rows_to_keep, ]
      } else {
        stop("interview_date must be either a date string or TRUE")
      }
    }
    
    # Remove the temporary date column
    df$temp_date <- NULL
  }
  
  # Use the filtered dataframe for applying other filters, but keep the original values
  
  # Check for either state or status column and apply filtering
  if (!is.null(status)) {
    # Convert single string to vector if needed
    status_values <- if(is.character(status) && length(status) == 1) {
      c(status)
    } else {
      status
    }
    
    if ("state" %in% names(df)) {
      rows_to_keep <- df$state %in% status_values
      df <- df[rows_to_keep, ]
      original_df <- original_df[rows_to_keep, ]
    } else if ("status" %in% names(df)) {
      rows_to_keep <- df$status %in% status_values
      df <- df[rows_to_keep, ]
      original_df <- original_df[rows_to_keep, ]
    } else {
      warning("Neither 'state' nor 'status' column found in the dataframe. Status filtering skipped.")
    }
  }
  
  if ("visit" %in% names(df) && !is.null(visit)) {
    # Convert single value to vector if needed
    visit_values <- if(length(visit) == 1) {
      c(visit)
    } else {
      visit
    }
    rows_to_keep <- df$visit %in% visit_values
    df <- df[rows_to_keep, ]
    original_df <- original_df[rows_to_keep, ]
  }
  
  if ("week" %in% names(df) && !is.null(week)) {
    # Convert single value to vector if needed
    week_values <- if(length(week) == 1) {
      c(week)
    } else {
      week
    }
    rows_to_keep <- df$week %in% week_values
    df <- df[rows_to_keep, ]
    original_df <- original_df[rows_to_keep, ]
  }
  
  if ("arm" %in% names(df) && !is.null(arm)) {
    rows_to_keep <- df$arm %in% arm
    df <- df[rows_to_keep, ]
    original_df <- original_df[rows_to_keep, ]
  }
  
  if ("site" %in% names(df) && !is.null(site)) {
    rows_to_keep <- df$site %in% site
    df <- df[rows_to_keep, ]
    original_df <- original_df[rows_to_keep, ]
  }
  
  if ("sex" %in% names(df) && !is.null(sex)) {
    rows_to_keep <- df$sex %in% sex
    df <- df[rows_to_keep, ]
    original_df <- original_df[rows_to_keep, ]
  }
  
  if ("phenotype" %in% names(df) && !is.null(phenotype)) {
    rows_to_keep <- df$phenotype %in% phenotype
    df <- df[rows_to_keep, ]
    original_df <- original_df[rows_to_keep, ]
  }
  
  if ("record_id" %in% names(df) && !is.null(record_id)) {
    rows_to_keep <- df$record_id %in% record_id
    df <- df[rows_to_keep, ]
    original_df <- original_df[rows_to_keep, ]
  }
  
  if ("src_subject_id" %in% names(df) && !is.null(src_subject_id)) {
    rows_to_keep <- df$src_subject_id %in% src_subject_id
    df <- df[rows_to_keep, ]
    original_df <- original_df[rows_to_keep, ]
  }
  
  if ("subjectkey" %in% names(df) && !is.null(subjectkey)) {
    rows_to_keep <- df$subjectkey %in% subjectkey
    df <- df[rows_to_keep, ]
    original_df <- original_df[rows_to_keep, ]
  }
  
  if (!is.null(lost_to_followup)) {
    if ("lost_to_followup" %in% names(df)) {
      rows_to_keep <- df$lost_to_followup == lost_to_followup
      df <- df[rows_to_keep, ]
      original_df <- original_df[rows_to_keep, ]
    } else if ("lost_to_follow-up" %in% names(df)) {
      rows_to_keep <- df$`lost_to_follow-up` == lost_to_followup
      df <- df[rows_to_keep, ]
      original_df <- original_df[rows_to_keep, ]
    }
  }
  
  # Apply row and column filters to the original_df which has original values
  result_df <- original_df
  
  if (!is.null(cols)) {
    # Convert single column name to vector if needed
    col_values <- if(is.character(cols) && length(cols) == 1) {
      c(cols)
    } else {
      cols
    }
    result_df <- result_df[, col_values, drop = FALSE]
  } else {
    message("No columns provided; all columns will be included.")
  }
  
  if (!is.null(rows)) {
    # Convert single row name to vector if needed
    row_values <- if(is.character(rows) && length(rows) == 1) {
      c(rows)
    } else {
      rows
    }
    result_df <- result_df[row_values, , drop = FALSE]
  }
  
  return(result_df)
}

#' Alias for 'sift'
#'
#' This is a legacy alias for the 'sift' function to maintain compatibility with older code.
#'
#' @inheritParams sift
#' @inherit sift return
#' @export
#' @examples
#' \dontrun{
#' filtered <- dataFilter(df, sex="F")
#' }
dataFilter <- sift
