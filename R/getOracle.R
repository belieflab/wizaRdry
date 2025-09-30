#' Fetch data from Oracle database to be stored in a data frame
#'
#' Retrieves data from an Oracle table or view and optionally joins it with a primary keys table
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
#' @param pii Logical; if FALSE (default), remove fields marked as PII. TRUE keeps PII.
#' @param interview_date Optional; can be either:
#'        - A date string in various formats (ISO, US, etc.) to filter data up to that date
#'        - A boolean TRUE to return only rows with non-NA interview_date values
#' @param all Logical; if TRUE, use LEFT OUTER JOIN instead of INNER JOIN (default: FALSE),
#'        similar to the 'all' parameter in base R's merge() function
#' @param schema Optional schema name to use for table qualification
#'
#' @return A data frame containing the requested SQL data
#' @importFrom odbc dbConnect dbGetInfo dbListTables dbGetQuery dbDisconnect
#' @export
#' @examples
#' \dontrun{
#' # Get data from a specific table
#' data <- oracle("participants")
#'
#' # Get data with a where clause
#' survey_data <- oracle("vw_surveyquestionresults",
#'                   where_clause = "resultidentifier = 'NRS'")
#'
#' # Get all records, including those without matching primary key
#' all_data <- oracle("candidate", all = TRUE)
#'
#' # Specify schema explicitly
#' schema_data <- oracle("survey_results", schema = "STUDY_DATA")
#' }
oracle <- function(table_name = NULL, ..., fields = NULL, where_clause = NULL,
                   join_primary_keys = TRUE, custom_query = NULL, max_rows = NULL,
                   date_format = NULL, batch_size = 1000, pii = FALSE,
                   interview_date = NULL, all = FALSE, schema = NULL) {

  # Check if required packages are available
  if (!requireNamespace("odbc", quietly = TRUE)) {
    stop("Package 'odbc' is needed for this function to work. Please install it.",
         call. = FALSE)
  }

  start_time <- Sys.time()

  # Validate secrets and config - MUST COME FIRST
  validate_secrets("sql")
  config <- validate_config("sql")

  # If table_name includes schema (e.g., SCHEMA.TABLE) and no schema arg provided,
  # extract schema for consistent handling and checks
  if (is.null(schema) && !is.null(table_name) && grepl("\\.", table_name)) {
    parts <- strsplit(table_name, "\\.")[[1]]
    if (length(parts) >= 2) {
      schema <- parts[1]
      table_name <- parts[2]
      message(sprintf("Using schema '%s' from table name", schema))
    }
  }

  # Initialize loading animation
  pb <- initializeLoadingAnimation(20)

  # Validate parameters
  if (is.null(table_name) && is.null(custom_query)) {
    tables_info <- oracle.index(schema)
    if (is.null(tables_info)) {
      stop("No table name or custom query provided, and could not retrieve table list")
    }
    stop("No table name or custom query provided. Use oracle.index() to see available tables.")
  }

  # Get connection parameters using get_secret
  dsn <- get_secret("host")  # This should be the DSN name
  user_id <- get_secret("uid")
  password <- get_secret("pwd")

  # Display loading message
  if (!is.null(custom_query)) {
    message(sprintf("\nExecuting custom SQL query..."))
  } else {
    message(sprintf("\nRetrieving data from Oracle table: %s%s%s",
                    ifelse(!is.null(schema), paste0(schema, "."), ""),
                    table_name,
                    ifelse(!is.null(where_clause), sprintf(" (with filters)"), "")))
  }

  # Update progress bar
  for (i in 1:10) {
    updateLoadingAnimation(pb, i)
    Sys.sleep(0.05)
  }

  # Establish database connection
  channel <- NULL
  result_data <- NULL

  tryCatch({
    channel <- odbc::dbConnect(odbc::odbc(),
                              dsn = dsn,
                              UID = user_id,
                              PWD = password)
    if (is.null(channel)) {
      stop("Failed to connect to database. Please check your connection settings.")
    }

    # Get database type information for diagnostic purposes
    db_info <- tryCatch({
      odbc::dbGetInfo(channel)
    }, error = function(e) {
      list(dbms.name = "Oracle")  # Default to Oracle since we're using the oracle function
    })

    message(sprintf("Connected to %s database", db_info$dbms.name))

    # Update progress bar
    for (i in 11:15) {
      updateLoadingAnimation(pb, i)
      Sys.sleep(0.05)
    }

    # Define the primary key information with proper validation
    primary_key_column <- NULL
    superkey_table <- NULL

    if (join_primary_keys) {
      # Check config structure like your SQL functions do
      if (!is.null(config) && !is.null(config$sql) && !is.null(config$sql$primary_key)) {
        primary_key_column <- config$sql$primary_key
      } else {
        message("Warning: Primary key not specified in config. Using default 'PARTICIPANTIDENTIFIER'")
        primary_key_column <- "PARTICIPANTIDENTIFIER"
      }

      if (!is.null(config) && !is.null(config$sql) && !is.null(config$sql$superkey)) {
        superkey_table <- config$sql$superkey
      } else {
        message("Warning: Superkey table not specified in config. Join with primary keys disabled.")
        join_primary_keys <- FALSE
      }
    }

    # Determine if the requested table is the configured superkey table
    is_superkey_request <- FALSE
    if (!is.null(superkey_table) && !is.null(table_name)) {
      # Extract table name without schema for comparison
      table_name_only <- table_name
      
      # Extract superkey table name without schema for comparison
      superkey_name_only <- if (grepl("\\.", superkey_table)) {
        strsplit(superkey_table, "\\.")[[1]][2]
      } else {
        superkey_table
      }
      
      # Check if the requested table matches the superkey table
      is_superkey_request <- identical(trimws(toupper(table_name_only)), trimws(toupper(superkey_name_only)))
      
      if (is_superkey_request) {
        message("Requested table matches configured superkey; returning without joins.")
        join_primary_keys <- FALSE
      }
    }

    # Determine fields to exclude based on PII settings with proper validation
    pii_fields <- character(0)
    if (!pii && !is.null(config) && !is.null(config$sql) && !is.null(config$sql$pii_fields)) {
      pii_fields <- config$sql$pii_fields
      if (length(pii_fields) > 0) {
        message(sprintf("Will exclude %d PII fields: %s",
                        length(pii_fields),
                        paste(pii_fields, collapse = ", ")))
      }
    }

    # Check if table exists and get correct schema if needed
    table_exists <- FALSE
    if (!is.null(table_name)) {
      tables_list <- odbc::dbListTables(channel, schema_name = schema)
      # Convert table name to uppercase for case-insensitive comparison
      # (Oracle typically stores object names in uppercase)
      upper_table_name <- toupper(table_name)

      if (!is.null(schema)) {
        # Check if table exists in specified schema
        upper_schema <- toupper(schema)
        table_exists <- any(toupper(tables_list) == upper_table_name)
        if (!table_exists) {
          # Try with exact case
          table_exists <- any(tables_list == table_name)
        }
      } else {
        # Check if table exists in any schema
        table_exists <- any(toupper(tables_list) == upper_table_name)
        # If table exists but no schema specified, get the schema
        if (table_exists) {
          message(sprintf("Found table: %s", table_name))
        }
      }

      if (!table_exists && is.null(custom_query)) {
        warning(sprintf("Table '%s' not found in the database. Attempting query anyway...", table_name))
      }
    }

    # Build or execute SQL query
    query <- NULL
    if (!is.null(custom_query)) {
      # Execute custom query directly
      query <- custom_query
    } else {
      # For Oracle syntax
      is_oracle <- TRUE  # Always TRUE for oracle() function

      # Build query based on parameters
      query <- build_oracle_query(
        table_name = table_name,
        fields = fields,
        where_clause = where_clause,
        superkey_table = if (join_primary_keys) superkey_table else NULL,
        primary_key_column = if (join_primary_keys) primary_key_column else NULL,
        max_rows = max_rows,
        pii_fields = if (!pii) pii_fields else NULL,
        schema = schema,
        all = all,
        is_oracle = is_oracle
      )
    }

    message(sprintf("Executing query: %s", query))

    # Execute the query
    result_data <- odbc::dbGetQuery(channel, query)

    # Check if result is an error message
    if (is.character(result_data) && length(result_data) == 1) {
      stop(paste("SQL error:", result_data))
    }

    # Ensure we have a data frame, even if empty
    if (!is.data.frame(result_data)) {
      result_data <- data.frame()
    }

    # Update progress bar
    for (i in 16:20) {
      updateLoadingAnimation(pb, i)
      Sys.sleep(0.05)
    }

    completeLoadingAnimation(pb)

    # Process and clean the data
    if (is.data.frame(result_data) && nrow(result_data) > 0) {
      # Apply PII exclusion if needed
      if (!pii && !is.null(pii_fields) && length(pii_fields) > 0) {
        pii_cols_in_result <- intersect(names(result_data), pii_fields)
        pii_cols_missing <- setdiff(pii_fields, names(result_data))
        if (length(pii_cols_in_result) > 0) {
          message("Removing PII columns from result: ", paste(pii_cols_in_result, collapse = ", "))
          result_data <- result_data[, !(names(result_data) %in% pii_fields), drop = FALSE]
        }
        if (length(pii_cols_missing) > 0) {
          message("Configured PII columns not present in result (skipped): ", paste(pii_cols_missing, collapse = ", "))
        }
      }

      # Process date fields
      result_data <- process_date_fields(result_data, date_format)

      # Handle missing values using the configuration
      result_data <- handle_missing_values(result_data, config)

      # Handle interview_date filtering if specified
      if (!is.null(interview_date) && "interview_date" %in% names(result_data)) {
        result_data <- filter_by_interview_date(result_data, interview_date)
      }

      # Standardize column names for key fields
      age_cols <- grep("_interview_age$", base::names(result_data), ignore.case = TRUE)
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
    } else {
      # Create an empty data frame if no results
      if (!is.data.frame(result_data)) {
        result_data <- data.frame()
      }
      message("No data returned from query")
    }

  }, error = function(e) {
    stop(paste("Error:", e$message))
  }, finally = {
    # Close the database connection - fixed the issue here
    if (!is.null(channel)) {
      tryCatch({
        odbc::dbDisconnect(channel)
      }, error = function(e) {
        # Silently ignore close errors
      })
    }
  })

  # Add metadata to the result
  attr(result_data, "sql_table") <- table_name
  if (!is.null(schema)) {
    attr(result_data, "schema") <- schema
  }
  if (!is.null(where_clause)) {
    attr(result_data, "where_clause") <- where_clause
  }

  # Show duration
  end_time <- Sys.time()
  duration <- difftime(end_time, start_time, units = "secs")
  message(sprintf("\nData frame retrieved from Oracle in %s with %d rows and %d columns.",
                  formatDuration(duration),
                  nrow(result_data),
                  ncol(result_data)))

  return(result_data)
}

#' Get a list of tables from the Oracle database
#'
#' @param schema Optional schema name to filter tables
#' @return A data frame with table information
#' @importFrom odbc dbConnect dbListTables dbDisconnect
#' @export
oracle.index <- function(schema = NULL) {

  # Validate secrets
  validate_secrets("sql")
  config <- validate_config("sql")

  # Get connection parameters using get_secret
  dsn <- get_secret("host")  # This should be the DSN name
  user_id <- get_secret("uid")
  password <- get_secret("pwd")

  # Connect to database
  channel <- NULL
  tables <- NULL

  tryCatch({
    channel <- odbc::dbConnect(odbc::odbc(),
                              dsn = dsn,
                              UID = user_id,
                              PWD = password)
    if (is.null(channel)) {
      stop("Failed to connect to database")
    }

    # Get database type for diagnostics
    db_info <- tryCatch({
      odbc::dbGetInfo(channel)
    }, error = function(e) {
      list(dbms.name = "Oracle")  # Default to Oracle since we're using oracle.index
    })

    message(sprintf("Connected to %s database", db_info$dbms.name))

    # Oracle-specific handling
    is_oracle <- TRUE  # Always TRUE for oracle.index

    # Get table list
    if (is.null(schema)) {
      # Get all tables
      tables <- odbc::dbListTables(channel)
      message("Retrieving tables from all schemas")
    } else {
      # Get tables from specific schema
      # Note: Oracle treats schema name as case-sensitive
      tables <- odbc::dbListTables(channel, schema_name = schema)
      message(sprintf("Retrieving tables from schema: %s", schema))

      # If no tables found, try uppercase (Oracle often stores schemas in uppercase)
      if (is_oracle && (is.null(tables) || length(tables) == 0)) {
        upper_schema <- toupper(schema)
        tables <- odbc::dbListTables(channel, schema_name = upper_schema)
        message(sprintf("Trying uppercase schema: %s", upper_schema))
      }
    }

    # If still no tables, try direct query for Oracle
    if (is_oracle && (is.null(tables) || length(tables) == 0)) {
      message("Trying direct Oracle query to list tables...")
      if (is.null(schema)) {
        # Query all accessible tables in Oracle
        query <- "SELECT owner AS TABLE_SCHEM, table_name AS TABLE_NAME,
                 'TABLE' AS TABLE_TYPE FROM all_tables
                 UNION
                 SELECT owner AS TABLE_SCHEM, view_name AS TABLE_NAME,
                 'VIEW' AS TABLE_TYPE FROM all_views
                 ORDER BY 1, 2"
      } else {
        # Query tables from specific schema in Oracle
        query <- sprintf(
          "SELECT owner AS TABLE_SCHEM, table_name AS TABLE_NAME,
           'TABLE' AS TABLE_TYPE FROM all_tables WHERE owner = '%s'
           UNION
           SELECT owner AS TABLE_SCHEM, view_name AS TABLE_NAME,
           'VIEW' AS TABLE_TYPE FROM all_views WHERE owner = '%s'
           ORDER BY 1, 2",
          schema, schema
        )
      }
      tables <- odbc::dbGetQuery(channel, query)
    }

  }, error = function(e) {
    message(paste("Error retrieving tables:", e$message))
  }, finally = {
    # Fixed connection close handling
    if (!is.null(channel)) {
      tryCatch({
        odbc::dbDisconnect(channel)
      }, error = function(e) {
        # Silently ignore close errors
      })
    }
  })

  # Format the result
  if (!is.null(tables) && length(tables) > 0) {
    # If tables is a character vector (from dbListTables), convert to data frame
    if (is.character(tables)) {
      tables_df <- data.frame(
        Schema = rep(ifelse(is.null(schema), "Unknown", schema), length(tables)),
        Table = tables,
        Type = rep("TABLE", length(tables)),
        stringsAsFactors = FALSE
      )
    } else if (is.data.frame(tables)) {
      # Keep only relevant columns
      if (all(c("TABLE_SCHEM", "TABLE_NAME", "TABLE_TYPE") %in% names(tables))) {
        tables_df <- tables[, c("TABLE_SCHEM", "TABLE_NAME", "TABLE_TYPE")]
        names(tables_df) <- c("Schema", "Table", "Type")
      } else if (ncol(tables) >= 3) {
        # Handle case where column names might be different
        tables_df <- tables[, 1:3]
        names(tables_df) <- c("Schema", "Table", "Type")
      } else {
        tables_df <- tables
      }
    } else {
      tables_df <- data.frame()
    }

    # Sort by schema and table name
    if (nrow(tables_df) > 0) {
      tables_df <- tables_df[order(tables_df$Schema, tables_df$Table), ]
      return(knitr::kable(tables_df, format = "simple"))
    }
  }
  
  message("No tables found")
  return(NULL)
}

#' Get Oracle table columns/metadata
#'
#' @param table_name Name of the table to get metadata for
#' @param schema Optional schema name
#' @return A data frame with column information
#' @importFrom odbc dbConnect dbListFields dbGetQuery dbDisconnect
#' @export
oracle.desc <- function(table_name, schema = NULL) {

  if (is.null(table_name)) {
    stop("Table name is required")
  }

  # Validate secrets
  validate_secrets("sql")
  config <- validate_config("sql")

  # Parse table_name to extract schema if not provided separately
  if (is.null(schema) && grepl("\\.", table_name)) {
    parts <- strsplit(table_name, "\\.")[[1]]
    if (length(parts) > 1) {
      schema <- parts[1]
      table_name <- parts[2]
      message(sprintf("Using schema '%s' from table name", schema))
    }
  }

  # Get connection parameters using get_secret
  dsn <- get_secret("host")  # This should be the DSN name
  user_id <- get_secret("uid")
  password <- get_secret("pwd")

  # Connect to database
  channel <- NULL
  columns <- NULL

  tryCatch({
    channel <- odbc::dbConnect(odbc::odbc(),
                              dsn = dsn,
                              UID = user_id,
                              PWD = password)
    if (is.null(channel)) {
      stop("Failed to connect to database")
    }

    # Get database type for diagnostics
    db_info <- tryCatch({
      odbc::dbGetInfo(channel)
    }, error = function(e) {
      list(dbms.name = "Oracle")  # Default to Oracle for oracle.desc
    })

    message(sprintf("Connected to %s database", db_info$dbms.name))

    is_oracle <- TRUE  # Always TRUE for oracle.desc

    # Try to get column information
    tryCatch({
      if (!is.null(schema)) {
        # With schema
        columns <- odbc::dbListFields(channel, name = table_name, schema_name = schema)
      } else {
        # Without schema
        columns <- odbc::dbListFields(channel, name = table_name)
      }
      
      # Convert to data frame format
      if (!is.null(columns) && length(columns) > 0) {
        columns_df <- data.frame(
          Column = columns,
          Type = rep("Unknown", length(columns)),
          Size = rep(NA, length(columns)),
          Nullable = rep("Unknown", length(columns)),
          stringsAsFactors = FALSE
        )
        columns <- columns_df
      }
    }, error = function(e) {
      message(paste("Error using dbListFields:", e$message))
      # For Oracle, fall back to direct query
      if (is_oracle) {
        message("Trying direct Oracle query for column information...")
        # Try with uppercase table name for Oracle
        upper_table <- toupper(table_name)
        oracle_query <- if (!is.null(schema)) {
          # With schema
          upper_schema <- toupper(schema)
          sprintf(
            "SELECT column_name, data_type, data_length,
             DECODE(nullable, 'Y', 'Yes', 'No') as nullable
             FROM all_tab_columns
             WHERE owner = '%s' AND table_name = '%s'
             ORDER BY column_id",
            upper_schema, upper_table
          )
        } else {
          # Without schema, look in all accessible schemas
          sprintf(
            "SELECT owner as schema_name, column_name, data_type, data_length,
             DECODE(nullable, 'Y', 'Yes', 'No') as nullable
             FROM all_tab_columns
             WHERE table_name = '%s'
             ORDER BY owner, column_id",
            upper_table
          )
        }
        columns <- odbc::dbGetQuery(channel, oracle_query)
        if (is.character(columns) && length(columns) == 1) {
          # Error message
          message(paste("Oracle query error:", columns))
          columns <- NULL
        } else if (!is.null(columns) && nrow(columns) > 0) {
          # Rename columns to match expected format
          if ("schema_name" %in% names(columns)) {
            names(columns) <- c("Schema", "Column", "Type", "Size", "Nullable")
          } else {
            names(columns) <- c("Column", "Type", "Size", "Nullable")
          }
        }
      }
    })

  }, error = function(e) {
    message(paste("Error retrieving columns for table", table_name, ":", e$message))
  }, finally = {
    # Fixed connection close handling
    if (!is.null(channel)) {
      tryCatch({
        odbc::dbDisconnect(channel)
      }, error = function(e) {
        # Silently ignore close errors
      })
    }
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
    } else if (!all(c("Column", "Type", "Size", "Nullable") %in% names(columns))) {
      # Try to use whatever columns we have
      message("Warning: Column metadata format is non-standard")
      return(columns)
    }
    return(knitr::kable(columns, format = "simple"))
  } else {
    message(paste("No columns found for table", table_name))
    return(NULL)
  }
}

#' Perform a direct Oracle query with minimal processing
#'
#' @param query The SQL query to execute
#' @param pii Logical; if FALSE (default), remove fields marked as PII. TRUE keeps PII.
#' @param schema Optional schema name to qualify table names in the query
#' @return A data frame with the query results
#' @importFrom odbc dbConnect dbGetQuery dbDisconnect
#' @export
oracle.query <- function(query, exclude_pii = FALSE, schema = NULL) {

  if (is.null(query) || !is.character(query) || length(query) != 1) {
    stop("A valid SQL query string is required")
  }

  # Validate secrets
  validate_secrets("sql")
  config <- validate_config("sql")

  # Get connection parameters using get_secret
  dsn <- get_secret("host")  # This should be the DSN name
  user_id <- get_secret("uid")
  password <- get_secret("pwd")

  # Get PII fields configuration if needed with proper validation
  pii_fields <- NULL
  if (exclude_pii && !is.null(config) && !is.null(config$sql) && !is.null(config$sql$pii_fields)) {
    pii_fields <- config$sql$pii_fields
    if (length(pii_fields) > 0) {
      message(sprintf("Will exclude %d PII fields if present in results: %s",
                      length(pii_fields),
                      paste(pii_fields, collapse=", ")))
    }
  }

  # Modify query to include schema if provided
  if (!is.null(schema) && !grepl("\\.", query)) {
    # Simple schema qualification for table references
    # This is a basic approach - a proper SQL parser would be better
    query <- gsub("\\b(FROM|JOIN)\\s+(\\w+)",
                  paste0("\\1 ", schema, ".\\2"),
                  query,
                  ignore.case = TRUE)
    message(sprintf("Using schema: %s", schema))
    message(sprintf("Modified query: %s", query))
  }

  # Connect to database
  channel <- NULL
  result <- NULL

  tryCatch({
    channel <- odbc::dbConnect(odbc::odbc(),
                              dsn = dsn,
                              UID = user_id,
                              PWD = password)
    if (is.null(channel)) {
      stop("Failed to connect to database")
    }

    # Get database info for diagnostics
    db_info <- tryCatch({
      odbc::dbGetInfo(channel)
    }, error = function(e) {
      list(dbms.name = "Oracle")  # Default to Oracle for oracle.query
    })

    message(sprintf("Connected to %s database", db_info$dbms.name))

    # Execute query
    result <- odbc::dbGetQuery(channel, query)

    # Check if result is an error message
    if (is.character(result) && length(result) == 1) {
      stop(paste("SQL error:", result))
    }

    # Make sure we have a data frame
    if (!is.data.frame(result)) {
      result <- data.frame()
    }

    # Apply PII exclusion if enabled
    if (exclude_pii && !is.null(pii_fields) && length(pii_fields) > 0 && nrow(result) > 0) {
      pii_cols_present <- intersect(names(result), pii_fields)
      if (length(pii_cols_present) > 0) {
        message(sprintf("Removing %d PII fields from results: %s",
                        length(pii_cols_present),
                        paste(pii_cols_present, collapse=", ")))
        result <- result[, !names(result) %in% pii_fields, drop = FALSE]
      }
    }

    # Apply missing value handling if configured with proper validation
    if (!is.null(config) && !is.null(config$missing_data_codes) && nrow(result) > 0) {
      result <- handle_missing_values(result, config)
    }

  }, error = function(e) {
    message(paste("Error executing query:", e$message))
    stop(paste("Oracle Error:", e$message))
  }, finally = {
    # Fixed connection close handling
    if (!is.null(channel)) {
      tryCatch({
        odbc::dbDisconnect(channel)
      }, error = function(e) {
        # Silently ignore close errors
      })
    }
  })

  return(result)
}

build_oracle_query <- function(table_name, fields = NULL, where_clause = NULL,
                               superkey_table = NULL, primary_key_column = NULL,
                               max_rows = NULL, pii_fields = NULL, all = FALSE,
                               schema = NULL, is_oracle = TRUE) {

  # Add schema prefix to table names if specified
  table_with_schema <- table_name
  superkey_with_schema <- superkey_table

  if (!is.null(schema)) {
    if (!grepl("\\.", table_name)) {
      table_with_schema <- paste0(schema, ".", table_name)
    }
    if (!is.null(superkey_table) && !grepl("\\.", superkey_table)) {
      superkey_with_schema <- paste0(schema, ".", superkey_table)
    }
  }

  # Define table aliases
  main_table_alias <- "t"
  pk_table_alias <- "pk"

  # Determine which fields to select
  if (is.null(fields)) {
    # No specific fields requested
    if (!is.null(superkey_table) && !is.null(primary_key_column)) {
      # When joining, include fields from both tables
      if (!is.null(pii_fields) && length(pii_fields) > 0) {
        # PII exclusion needed - this will be handled in the main function
        message("Note: PII exclusion with * requires listing all non-PII columns explicitly")
        select_clause <- paste0("SELECT ", main_table_alias, ".*, ", pk_table_alias, ".*")
      } else {
        # No PII exclusion needed - include all fields from both tables
        select_clause <- paste0("SELECT ", main_table_alias, ".*, ", pk_table_alias, ".*")
      }
    } else {
      # No join - just select from main table
      if (!is.null(pii_fields) && length(pii_fields) > 0) {
        message("Note: PII exclusion with * requires listing all non-PII columns explicitly")
        select_clause <- "SELECT *"
      } else {
        select_clause <- "SELECT *"
      }
    }
  } else {
    # Specific fields requested
    # Filter out PII fields if specified
    if (!is.null(pii_fields)) {
      fields <- setdiff(fields, pii_fields)
    }

    # If fields array is not empty after filtering
    if (length(fields) > 0) {
      # For joins, qualify field names with table alias if they don't already have one
      if (!is.null(superkey_table) && !is.null(primary_key_column)) {
        # Check if fields specify table aliases or need them
        qualified_fields <- character(0)
        for (field in fields) {
          if (grepl("\\.", field)) {
            # Field already has table qualifier (e.g., "t.sub_id" or "pk.phi_field")
            qualified_fields <- c(qualified_fields, field)
          } else {
            # Add main table alias to field names
            qualified_fields <- c(qualified_fields, paste0(main_table_alias, ".", field))
          }
        }
        select_clause <- paste0("SELECT ", paste(qualified_fields, collapse = ", "))
      } else {
        # No join, use fields as-is
        select_clause <- paste0("SELECT ", paste(fields, collapse = ", "))
      }
    } else {
      # All requested fields were PII, so select a dummy field
      select_clause <- "SELECT 1 AS dummy_column"
    }
  }

  # Build the FROM clause
  if (!is.null(superkey_with_schema) && !is.null(primary_key_column)) {
    # Determine join type based on 'all' parameter
    join_type <- ifelse(all, "LEFT OUTER JOIN", "INNER JOIN")

    # Join with superkey table - using Oracle syntax format
    # Ensure primary_key_column doesn't have schema prefix
    clean_primary_key <- if (grepl("\\.", primary_key_column)) {
      strsplit(primary_key_column, "\\.")[[1]][2]
    } else {
      primary_key_column
    }
    
    from_clause <- sprintf(
      "FROM %s %s\n%s %s %s\n    ON %s.%s = %s.%s",
      table_with_schema, main_table_alias,
      join_type,
      superkey_with_schema, pk_table_alias,
      main_table_alias, clean_primary_key,
      pk_table_alias, clean_primary_key
    )
  } else {
    from_clause <- sprintf("FROM %s %s", table_with_schema, main_table_alias)
  }

  # Add WHERE clause if provided
  where_part <- ""
  if (!is.null(where_clause) && nchar(where_clause) > 0) {
    where_part <- paste("WHERE", where_clause)
  }

  # Add LIMIT/TOP clause if max_rows is specified
  limit_part <- ""
  if (!is.null(max_rows) && is.numeric(max_rows) && max_rows > 0) {
    if (is_oracle) {
      # Oracle uses ROWNUM
      if (nchar(where_part) > 0) {
        where_part <- paste0(where_part, " AND ROWNUM <= ", as.integer(max_rows))
      } else {
        where_part <- paste0("WHERE ROWNUM <= ", as.integer(max_rows))
      }
    } else {
      # Many databases use TOP in the SELECT clause
      select_clause <- gsub(
        "SELECT DISTINCT",
        paste0("SELECT DISTINCT TOP ", as.integer(max_rows)),
        select_clause
      )
    }
  }

  # Combine all parts into the final query
  query <- paste(select_clause, from_clause, where_part)

  # For debugging
  message("Generated SQL query: ", query)

  return(query)
}

#' Process date fields in the dataset
#'
#' @param df The data frame to process
#' @param date_format The desired output date format
#'
#' @return The processed data frame
#' @noRd
process_date_fields <- function(df, date_format = NULL) {
  # If no format specified, leave dates in database format
  if (is.null(date_format)) {
    return(df)
  }

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

#' Handle missing values based on configuration
#'
#' @param df The data frame to process
#' @param config The configuration containing missing value definitions
#' @return The processed data frame
#' @noRd
handle_missing_values <- function(df, config) {
  # If missing_data_codes is not configured, return the data frame as is
  if (is.null(config) || !is.list(config) || !("missing_data_codes" %in% names(config))) {
    return(df)
  }

  # Get missing value codes
  missing_codes <- config$missing_data_codes

  # Additional check that missing_codes is a list
  if (!is.list(missing_codes)) {
    return(df)
  }

  # Process each category of missing values
  transformed_count <- 0
  transformed_cols <- character(0)

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
              matches <- sum(mask, na.rm = TRUE)
              df[[col]][mask] <- NA
              transformed_count <- transformed_count + matches
              # Track which columns were affected
              if (!(col %in% transformed_cols)) {
                transformed_cols <- c(transformed_cols, col)
              }
            }
          }
        }
      } else if (is.numeric(df[[col]])) {
        # For numeric columns, only match numeric missing codes
        for (value in values) {
          if (is.numeric(value)) {
            mask <- df[[col]] == value
            if (any(mask, na.rm = TRUE)) {
              matches <- sum(mask, na.rm = TRUE)
              df[[col]][mask] <- NA
              transformed_count <- transformed_count + matches
              # Track which columns were affected
              if (!(col %in% transformed_cols)) {
                transformed_cols <- c(transformed_cols, col)
              }
            }
          }
        }
      }
    }
  }

  # Report changes if any were made
  if (transformed_count > 0) {
    message(sprintf("Replaced %d missing value codes with NA in %d column(s): %s",
                    transformed_count,
                    length(transformed_cols),
                    paste(transformed_cols, collapse=", ")))
  }

  return(df)
}

#' Get missing data codes from configuration
#'
#' @return A list of missing data codes by category
#' @noRd
get_missing_data_codes <- function() {
  # Get configuration
  config <- validate_config("sql")

  # Return missing data codes if configured with proper validation
  if (!is.null(config) && is.list(config) && "missing_data_codes" %in% names(config)) {
    return(config$missing_data_codes)
  }

  return(NULL)
}

#' Test Oracle database connection
#'
#' Tests the connection to the Oracle database using the configured DSN and credentials.
#' This is a simple connectivity test that doesn't perform any data operations.
#'
#' @return A logical value indicating whether the connection was successful
#' @importFrom odbc dbConnect dbDisconnect
#' @export
#' @examples
#' \dontrun{
#' # Test the Oracle connection
#' if (oracle.test()) {
#'   message("Oracle connection successful!")
#' } else {
#'   message("Oracle connection failed!")
#' }
#' }
oracle.test <- function() {
  
  # Check if required packages are available
  if (!requireNamespace("odbc", quietly = TRUE)) {
    message("Package 'odbc' is needed for this function to work. Please install it.")
    return(FALSE)
  }
  
  # Validate secrets
  tryCatch({
    validate_secrets("sql")
  }, error = function(e) {
    message("Error validating secrets: ", e$message)
    return(FALSE)
  })
  
  # Get connection parameters using get_secret
  dsn <- get_secret("host")  # This should be the DSN name
  user_id <- get_secret("uid")
  password <- get_secret("pwd")
  
  # Validate that we have the required connection parameters
  if (is.null(dsn) || is.null(user_id) || is.null(password)) {
    message("Missing connection parameters. Please check your secrets configuration.")
    return(FALSE)
  }
  
  # Test the connection
  channel <- NULL
  connection_successful <- FALSE
  
  tryCatch({
    message(sprintf("Testing connection to DSN: %s", dsn))
    
    # Attempt to connect
    channel <- odbc::dbConnect(odbc::odbc(),
                              dsn = dsn,
                              UID = user_id,
                              PWD = password)
    
    if (is.null(channel)) {
      message("Connection failed: channel is null")
      return(FALSE)
    }
    
    # Get database info to confirm connection is working
    db_info <- tryCatch({
      odbc::dbGetInfo(channel)
    }, error = function(e) {
      message("Warning: Could not retrieve database info: ", e$message)
      list(dbms.name = "Oracle")
    })
    
    message(sprintf("Successfully connected to %s database", db_info$dbms.name))
    connection_successful <- TRUE
    
  }, error = function(e) {
    message("Connection failed: ", e$message)
    connection_successful <- FALSE
  }, finally = {
    # Always try to close the connection
    if (!is.null(channel)) {
      tryCatch({
        odbc::dbDisconnect(channel)
        if (connection_successful) {
          message("Connection closed successfully")
        }
      }, error = function(e) {
        message("Warning: Error closing connection: ", e$message)
      })
    }
  })
  
  return(connection_successful)
}
