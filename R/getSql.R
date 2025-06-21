#' Handle missing values based on configuration
#'
#' @param df The data frame to process
#' @param config The configuration containing missing value definitions
#'
#' @return The processed data frame
#' @noRd
handle_missing_values <- function(df, config) {
  # If missing_data_codes is not configured, return the data frame as is
  if (is.null(config$missing_data_codes)) {
    return(df)
  }

  # Get missing value codes
  missing_codes <- get_missing_data_codes()
  if (is.null(missing_codes)) {
    return(df)
  }

  # Process each category of missing values
  for (category in names(missing_codes)) {
    values <- missing_codes[[category]]

    # Skip if no values defined
    if (length(values) == 0) {
      next
    }

    # Replace values with NA in the data frame
    for (col in names(df)) {
      if (is.character(df[[col]])) {
        # For character columns, convert missing value codes to NA
        for (value in values) {
          # Case insensitive comparison for string values
          if (is.character(value)) {
            mask <- tolower(df[[col]]) == tolower(value)
            if (any(mask, na.rm = TRUE)) {
              df[[col]][mask] <- NA
            }
          }
        }
      } else if (is.numeric(df[[col]])) {
        # For numeric columns, only match numeric missing codes
        for (value in values) {
          if (is.numeric(value)) {
            mask <- df[[col]] == value
            if (any(mask, na.rm = TRUE)) {
              df[[col]][mask] <- NA
            }
          }
        }
      }
    }
  }

  return(df)
}

#' Fetch data from SQL database to be stored in a data frame
#'
#' Retrieves data from a SQL table and optionally joins it with a primary keys table
#' as specified in the configuration.
#'
#' @param table_name Name of the SQL table or view to query
#' @param ... Optional column names to filter for. Only rows with non-missing values
#'        in ALL specified columns will be returned.
#' @param fields Optional vector of specific fields to select
#' @param where_clause Optional WHERE clause to filter results (without the "WHERE" keyword)
#' @param join_primary_keys Boolean, whether to join with the primary keys table (default: TRUE)
#' @param custom_query Optional custom SQL query to execute instead of building one
#' @param max_rows Optional limit on the number of rows to return
#' @param date_format Optional format for date fields (default uses ISO format)
#' @param batch_size Number of records to retrieve per batch for large datasets
#' @param exclude_pii Default TRUE to remove all fields marked as identifiable
#' @param interview_date Optional; can be either:
#'        - A date string in various formats (ISO, US, etc.) to filter data up to that date
#'        - A boolean TRUE to return only rows with non-NA interview_date values
#'
#' @return A data frame containing the requested SQL data
#' @importFrom RODBC odbcConnect odbcGetInfo sqlTables sqlQuery odbcClose
#' @export
#' @examples
#' \dontrun{
#' # Get data from a specific table
#' data <- sql("participants")
#'
#' # Get data with a where clause
#' survey_data <- sql("vw_surveyquestionresults",
#'                   where_clause = "resultidentifier = 'NRS'")
#' }
sql <- function(table_name = NULL, ..., fields = NULL, where_clause = NULL,
                join_primary_keys = TRUE, custom_query = NULL, max_rows = NULL,
                date_format = NULL, batch_size = 1000, exclude_pii = TRUE,
                interview_date = NULL) {

  start_time <- Sys.time()

  # Validate secrets and config - MUST COME FIRST
  validate_secrets("sql")
  config <- validate_config("sql")

  # Initialize loading animation
  pb <- initializeLoadingAnimation(20)

  # Validate parameters
  if (is.null(table_name) && is.null(custom_query)) {
    tables_info <- sql.tables()
    if (is.null(tables_info)) {
      stop("No table name or custom query provided, and could not retrieve table list")
    }
    stop("No table name or custom query provided. Use sql.tables() to see available tables.")
  }

  # Get connection parameters using get_secret instead of accessing .GlobalEnv
  conn_str <- get_secret("conn")
  user_id <- get_secret("uid")
  password <- get_secret("pwd")

  # Display loading message
  if (!is.null(custom_query)) {
    message(sprintf("\nExecuting custom SQL query..."))
  } else {
    message(sprintf("\nRetrieving data from SQL table: %s%s",
                    table_name,
                    ifelse(!is.null(where_clause), sprintf(" (with filters)"), "")))
  }

  # Update progress bar
  for (i in 1:10) {
    updateLoadingAnimation(pb, i)
    Sys.sleep(0.05)
  }

  # Establish database connection
  tryCatch({
    channel <- RODBC::odbcConnect(conn_str, uid = user_id, pwd = password, believeNRows = FALSE)
    if (channel == -1) {
      stop("Failed to connect to database. Please check your connection settings.")
    }
  }, error = function(e) {
    stop(paste("Error connecting to database:", e$message))
  })

  # Update progress bar
  for (i in 11:15) {
    updateLoadingAnimation(pb, i)
    Sys.sleep(0.05)
  }

  # Define the primary key information
  primary_key_column <- NULL
  superkey_table <- NULL

  if (join_primary_keys) {
    if (!is.null(config$sql$primary_key)) {
      primary_key_column <- config$sql$primary_key
    } else {
      message("Warning: Primary key not specified in config. Using default 'PARTICIPANTIDENTIFIER'")
      primary_key_column <- "PARTICIPANTIDENTIFIER"
    }

    if (!is.null(config$sql$superkey)) {
      superkey_table <- config$sql$superkey
    } else {
      message("Warning: Superkey table not specified in config. Join with primary keys disabled.")
      join_primary_keys <- FALSE
    }
  }

  # Determine fields to exclude based on PII settings
  pii_fields <- character(0)

  if (exclude_pii && !is.null(config$sql$pii_fields)) {
    pii_fields <- config$sql$pii_fields
    if (length(pii_fields) > 0) {
      message(sprintf("Will exclude %d PII fields: %s",
                      length(pii_fields),
                      paste(pii_fields, collapse = ", ")))
    }
  }

  # Build or execute SQL query
  result_data <- NULL

  tryCatch({
    if (!is.null(custom_query)) {
      # Execute custom query directly
      result_data <- RODBC::sqlQuery(channel, custom_query)
    } else {
      # Build query based on parameters
      query <- build_sql_query(
        table_name = table_name,
        fields = fields,
        where_clause = where_clause,
        superkey_table = superkey_table,
        primary_key_column = primary_key_column,
        max_rows = max_rows,
        pii_fields = if (exclude_pii) pii_fields else NULL
      )

      # Execute the query
      result_data <- RODBC::sqlQuery(channel, query)
    }

    if (is.character(result_data) && length(result_data) == 1) {
      # RODBC returns an error message as a character vector
      stop(paste("SQL error:", result_data))
    }
  }, error = function(e) {
    RODBC::odbcClose(channel)
    stop(paste("Error executing SQL query:", e$message))
  })

  # Update progress bar
  for (i in 16:20) {
    updateLoadingAnimation(pb, i)
    Sys.sleep(0.05)
  }
  completeLoadingAnimation(pb)

  # Close the database connection
  RODBC::odbcClose(channel)

  # Process and clean the data
  if (nrow(result_data) > 0) {
    # Process date fields
    result_data <- process_date_fields(result_data, date_format)

    # Handle interview_date filtering if specified
    if (!is.null(interview_date) && "interview_date" %in% names(result_data)) {
      result_data <- filter_by_interview_date(result_data, interview_date)
    }

    # Standardize column names for key fields (similar to redcap function)
    age_cols <- grep("_interview_age$", base::names(result_data))
    if (length(age_cols) > 0) {
      base::names(result_data)[age_cols] <- "interview_age"
    }

    date_patterns <- c("_interview_date$", "interview_date")
    date_cols <- NULL
    for (pattern in date_patterns) {
      found_cols <- grep(pattern, base::names(result_data), ignore.case = TRUE)
      if (length(found_cols) > 0) {
        date_cols <- found_cols
        break  # Stop at first pattern that finds matches
      }
    }
    if (!is.null(date_cols) && length(date_cols) > 0) {
      base::names(result_data)[date_cols] <- "interview_date"
    }

    # Process column filtering based on ... parameters
    dots_args <- list(...)
    if (length(dots_args) > 0) {
      result_data <- filter_by_column_values(result_data, dots_args)
    }
  }

  # Add metadata to the result
  attr(result_data, "sql_table") <- table_name
  if (!is.null(where_clause)) {
    attr(result_data, "where_clause") <- where_clause
  }

  # Show duration
  end_time <- Sys.time()
  duration <- difftime(end_time, start_time, units = "secs")
  message(sprintf("\nData frame retrieved from SQL in %s with %d rows and %d columns.",
                  formatDuration(duration),
                  nrow(result_data),
                  ncol(result_data)))

  return(result_data)
}

#' Build SQL query based on provided parameters
#'
#' @param table_name The main table to query
#' @param fields Optional specific fields to select
#' @param where_clause Optional WHERE clause
#' @param superkey_table Optional superkey table for joining
#' @param primary_key_column Primary key column name for joining
#' @param max_rows Maximum number of rows to return
#' @param pii_fields Fields to exclude as PII
#'
#' @return A string containing the SQL query
#' @noRd
build_sql_query <- function(table_name, fields = NULL, where_clause = NULL,
                            superkey_table = NULL, primary_key_column = NULL,
                            max_rows = NULL, pii_fields = NULL) {

  # Determine which fields to select
  if (is.null(fields)) {
    select_clause <- "SELECT DISTINCT"
  } else {
    # Filter out PII fields if specified
    if (!is.null(pii_fields)) {
      fields <- setdiff(fields, pii_fields)
    }
    select_clause <- paste0("SELECT DISTINCT ", paste(fields, collapse = ", "))
  }

  # Build the FROM clause
  if (!is.null(superkey_table) && !is.null(primary_key_column)) {
    # Join with superkey table
    from_clause <- sprintf("FROM %s t INNER JOIN %s pk ON t.%s = pk.%s",
                           table_name, superkey_table,
                           primary_key_column, primary_key_column)
  } else {
    from_clause <- paste("FROM", table_name)
  }

  # Add WHERE clause if provided
  where_part <- ""
  if (!is.null(where_clause) && nchar(where_clause) > 0) {
    where_part <- paste("WHERE", where_clause)
  }

  # Add LIMIT/TOP clause if max_rows is specified
  limit_part <- ""
  if (!is.null(max_rows) && is.numeric(max_rows) && max_rows > 0) {
    # Different syntax for different database systems
    # This assumes SQL Server, but can be modified based on config
    select_clause <- gsub("SELECT DISTINCT",
                          paste0("SELECT DISTINCT TOP ", as.integer(max_rows)),
                          select_clause)
  }

  # Combine all parts into the final query
  query <- paste(select_clause, from_clause, where_part)

  return(query)
}

#' Process date fields in the dataset
#'
#' @param df The data frame to process
#' @param date_format The desired output date format
#'
#' @return The processed data frame
#' @noRd
process_date_fields <- function(df, date_format = "ymd") {

  # Identify potential date columns by name pattern
  date_patterns <- c("date", "dt", "timestamp")
  potential_date_cols <- character(0)

  for (pattern in date_patterns) {
    found_cols <- grep(pattern, names(df), value = TRUE, ignore.case = TRUE)
    potential_date_cols <- c(potential_date_cols, found_cols)
  }

  # Remove duplicates
  potential_date_cols <- unique(potential_date_cols)

  # Process each potential date column
  for (col in potential_date_cols) {
    # Only process if the column exists and has data
    if (col %in% names(df) && !all(is.na(df[[col]]))) {
      # Check if it's already a Date or POSIXct object
      if (inherits(df[[col]], "Date") || inherits(df[[col]], "POSIXct")) {
        # Format according to preference
        if (date_format == "mdy") {
          df[[col]] <- format(df[[col]], "%m/%d/%Y")
        } else if (date_format == "dmy") {
          df[[col]] <- format(df[[col]], "%d/%m/%Y")
        } else if (date_format == "ymd") {
          df[[col]] <- format(df[[col]], "%Y-%m-%d")
        }
      }
      # Check if it's character data that looks like dates
      else if (is.character(df[[col]])) {
        # Try to convert string dates to consistent format
        if (any(grepl("-|/|\\.", df[[col]][!is.na(df[[col]])]))) {
          tryCatch({
            parsed_dates <- parseAnyDate(df[[col]])
            if (!all(is.na(parsed_dates))) {
              # Format according to preference
              if (date_format == "mdy") {
                df[[col]] <- format(parsed_dates, "%m/%d/%Y")
              } else if (date_format == "dmy") {
                df[[col]] <- format(parsed_dates, "%d/%m/%Y")
              } else if (date_format == "ymd") {
                df[[col]] <- format(parsed_dates, "%Y-%m-%d")
              }
            }
          }, error = function(e) {
            message(sprintf("Note: Could not parse dates in column %s", col))
          })
        }
      }
    }
  }

  return(df)
}

#' Parse dates in various formats
#'
#' @param date_strings Vector of date strings to parse
#'
#' @return Vector of Date objects
#' @noRd
parseAnyDate <- function(date_strings) {
  # Create a result vector of the same length as input
  result <- rep(as.Date(NA), length(date_strings))

  # Skip NA values
  mask_na <- !is.na(date_strings)

  if (!any(mask_na)) {
    return(result)
  }

  valid_strings <- date_strings[mask_na]

  # Try different date formats
  parsed <- tryCatch({
    # Try lubridate parsing functions in sequence
    if (requireNamespace("lubridate", quietly = TRUE)) {
      # Try ISO format (YYYY-MM-DD)
      parsed <- lubridate::ymd(valid_strings, quiet = TRUE)

      # If that fails, try US format (MM/DD/YYYY)
      if (all(is.na(parsed))) {
        parsed <- lubridate::mdy(valid_strings, quiet = TRUE)
      }

      # If that fails, try European format (DD/MM/YYYY)
      if (all(is.na(parsed))) {
        parsed <- lubridate::dmy(valid_strings, quiet = TRUE)
      }

      parsed
    } else {
      # Fallback if lubridate is not available
      as.Date(valid_strings)
    }
  }, error = function(e) {
    rep(as.Date(NA), length(valid_strings))
  })

  # Update the result with parsed dates
  result[mask_na] <- parsed

  return(result)
}

#' Filter data frame by interview_date
#'
#' @param df The data frame to filter
#' @param interview_date The interview date filter value
#'
#' @return The filtered data frame
#' @noRd
filter_by_interview_date <- function(df, interview_date) {
  if (is.null(interview_date) || !("interview_date" %in% names(df))) {
    return(df)
  }

  # Create a temporary date column for filtering
  df$temp_date <- parseAnyDate(df$interview_date)

  if (is.logical(interview_date) && interview_date == TRUE) {
    # Keep only rows with non-NA interview_date values
    df <- df[!is.na(df$temp_date), ]
  } else if (is.character(interview_date) || inherits(interview_date, "Date")) {
    # Filter by specific date
    target_date <- if (inherits(interview_date, "Date")) {
      interview_date
    } else {
      tryCatch({
        parseAnyDate(interview_date)
      }, error = function(e) {
        stop("Failed to parse interview_date parameter: ", interview_date)
      })
    }

    if (is.na(target_date)) {
      stop("Failed to parse interview_date parameter: ", interview_date)
    }

    # Keep rows with dates before or equal to target_date
    df <- df[!is.na(df$temp_date) & df$temp_date <= target_date, ]
  }

  # Remove temporary column
  df$temp_date <- NULL

  return(df)
}

#' Filter data frame by column values
#'
#' @param df The data frame to filter
#' @param cols_to_check List of column names to check for non-NA values
#'
#' @return The filtered data frame
#' @noRd
filter_by_column_values <- function(df, cols_to_check) {
  if (length(cols_to_check) == 0) {
    return(df)
  }

  # Convert to character vector
  requested_cols <- as.character(unlist(cols_to_check))

  # Find which columns exist in the data
  existing_cols <- intersect(requested_cols, names(df))

  if (length(existing_cols) > 0) {
    # Display columns found
    message(sprintf("Found %d of %d requested columns: %s",
                    length(existing_cols),
                    length(requested_cols),
                    paste(existing_cols, collapse = ", ")))

    # Create a filter to keep rows with data in all requested columns
    rows_to_keep <- rep(TRUE, nrow(df))

    for (col in existing_cols) {
      # Check for non-NA values
      not_na <- !is.na(df[[col]])

      # For character columns, also check for non-empty strings
      not_empty <- rep(TRUE, nrow(df))
      if (is.character(df[[col]])) {
        not_empty <- df[[col]] != ""
      }

      # Update filter
      rows_to_keep <- rows_to_keep & not_na & not_empty
    }

    # Apply the filter
    original_rows <- nrow(df)
    df <- df[rows_to_keep, ]
    kept_rows <- nrow(df)

    message(sprintf("Kept %d of %d rows where all requested columns have values.",
                    kept_rows, original_rows))
  } else {
    warning("None of the requested columns were found in the dataset.")
  }

  return(df)
}

#' Get a list of tables from the SQL database
#'
#' @param schema Optional schema name to filter tables
#' @return A data frame with table information
#' @importFrom RODBC odbcConnect sqlTables odbcClose
#' @export
sql.tables <- function(schema = NULL) {
  # Validate secrets
  validate_secrets("sql")

  # Get connection parameters using get_secret instead of accessing .GlobalEnv
  conn_str <- get_secret("conn")
  user_id <- get_secret("uid")
  password <- get_secret("pwd")

  # Connect to database
  tryCatch({
    channel <- RODBC::odbcConnect(conn_str, uid = user_id, pwd = password, believeNRows = FALSE)
    if (channel == -1) {
      stop("Failed to connect to database")
    }
  }, error = function(e) {
    message(paste("Error connecting to database:", e$message))
    return(NULL)
  })

  # Get table list
  tables <- NULL
  tryCatch({
    if (is.null(schema)) {
      tables <- RODBC::sqlTables(channel)
    } else {
      tables <- RODBC::sqlTables(channel, schema = schema)
    }
  }, error = function(e) {
    message(paste("Error retrieving tables:", e$message))
  }, finally = {
    RODBC::odbcClose(channel)
  })

  # Format the result
  if (!is.null(tables) && nrow(tables) > 0) {
    # Keep only relevant columns
    if (all(c("TABLE_SCHEM", "TABLE_NAME", "TABLE_TYPE") %in% names(tables))) {
      tables <- tables[, c("TABLE_SCHEM", "TABLE_NAME", "TABLE_TYPE")]
    }

    # Rename columns for clarity
    names(tables) <- c("Schema", "Table", "Type")

    # Sort by schema and table name
    tables <- tables[order(tables$Schema, tables$Table), ]

    return(knitr::kable(tables, format = "simple"))
  } else {
    message("No tables found")
    return(NULL)
  }
}

#' Get SQL table columns/metadata
#'
#' @param table_name Name of the table to get metadata for
#' @return A data frame with column information
#' @importFrom RODBC odbcConnect sqlColumns odbcClose
#' @export
sql.columns <- function(table_name) {
  if (is.null(table_name)) {
    stop("Table name is required")
  }

  # Validate secrets
  validate_secrets("sql")

  # Get connection parameters using get_secret instead of accessing .GlobalEnv
  conn_str <- get_secret("conn")
  user_id <- get_secret("uid")
  password <- get_secret("pwd")

  # Connect to database
  tryCatch({
    channel <- RODBC::odbcConnect(conn_str, uid = user_id, pwd = password, believeNRows = FALSE)
    if (channel == -1) {
      stop("Failed to connect to database")
    }
  }, error = function(e) {
    stop(paste("Error connecting to database:", e$message))
  })

  # Get column information
  columns <- NULL
  tryCatch({
    columns <- RODBC::sqlColumns(channel, table_name)
  }, error = function(e) {
    RODBC::odbcClose(channel)
    stop(paste("Error retrieving columns for table", table_name, ":", e$message))
  }, finally = {
    RODBC::odbcClose(channel)
  })

  # Format the result
  if (!is.null(columns) && nrow(columns) > 0) {
    # Keep only relevant columns
    if (all(c("COLUMN_NAME", "TYPE_NAME", "COLUMN_SIZE", "NULLABLE") %in% names(columns))) {
      columns <- columns[, c("COLUMN_NAME", "TYPE_NAME", "COLUMN_SIZE", "NULLABLE")]

      # Rename NULLABLE column for clarity
      columns$NULLABLE <- ifelse(columns$NULLABLE == 1, "Yes", "No")

      # Rename columns
      names(columns) <- c("Column", "Type", "Size", "Nullable")

      return(knitr::kable(columns, format = "simple"))
    } else {
      return(columns)
    }
  } else {
    message(paste("No columns found for table", table_name))
    return(NULL)
  }
}

#' Perform a direct SQL query with minimal processing
#'
#' @param query The SQL query to execute
#' @return A data frame with the query results
#' @importFrom RODBC odbcConnect sqlQuery odbcClose
#' @export
sql.query <- function(query) {
  if (is.null(query) || !is.character(query) || length(query) != 1) {
    stop("A valid SQL query string is required")
  }

  # Validate secrets
  validate_secrets("sql")

  # Get connection parameters using get_secret instead of accessing .GlobalEnv
  conn_str <- get_secret("conn")
  user_id <- get_secret("uid")
  password <- get_secret("pwd")

  # Connect to database
  tryCatch({
    channel <- RODBC::odbcConnect(conn_str, uid = user_id, pwd = password, believeNRows = FALSE)
    if (channel == -1) {
      stop("Failed to connect to database")
    }
  }, error = function(e) {
    stop(paste("Error connecting to database:", e$message))
  })

  # Execute query
  result <- NULL
  tryCatch({
    result <- RODBC::sqlQuery(channel, query)

    if (is.character(result) && length(result) == 1) {
      # RODBC returns an error message as a character vector
      stop(paste("SQL error:", result))
    }
  }, error = function(e) {
    RODBC::odbcClose(channel)
    stop(paste("Error executing query:", e$message))
  }, finally = {
    RODBC::odbcClose(channel)
  })

  return(result)
}

#' Format time duration in a human-readable way
#'
#' @param duration The duration to format
#' @return A formatted string
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

#' Initialize a loading animation
#'
#' @param steps Total number of steps
#' @return A list with animation state
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

#' Update the loading animation
#'
#' @param pb The progress bar object
#' @param current Current step
#' @return Nothing
#' @noRd
updateLoadingAnimation <- function(pb, current) {
  pb$current <- current
  percentage <- round(current / pb$steps * 100)
  filled <- round(pb$width * current / pb$steps)
  bar <- paste0(
    strrep("=", filled),
    strrep(" ", pb$width - filled)
  )
  cat(sprintf("\r|%s| %3d%%", bar, percentage))
  utils::flush.console()
}

#' Complete the loading animation
#'
#' @param pb The progress bar object
#' @return Nothing
#' @noRd
completeLoadingAnimation <- function(pb) {
  updateLoadingAnimation(pb, pb$steps)
  cat("\n")
}
