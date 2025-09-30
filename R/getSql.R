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
#' @param pii Logical; if FALSE (default), remove fields marked as PII. TRUE keeps PII.
#' @param interview_date Optional; can be either:
#'        - A date string in various formats (ISO, US, etc.) to filter data up to that date
#'        - A boolean TRUE to return only rows with non-NA interview_date values
#' @param all Logical; if TRUE, use LEFT OUTER JOIN instead of INNER JOIN (default: FALSE),
#'        similar to the 'all' parameter in base R's merge() function
#'
#' @return A data frame containing the requested SQL data
#' @export
#' @examples
#' \dontrun{
#' # Get data from a specific table
#' data <- sql("participants")
#'
#' # Get data with a where clause
#' survey_data <- sql("vw_surveyquestionresults",
#'                   where_clause = "resultidentifier = 'NRS'")
#'
#' # Get all records, including those without matching primary key
#' all_data <- sql("candidate", all = TRUE)
#' }
sql <- function(table_name = NULL, ..., fields = NULL, where_clause = NULL,
                join_primary_keys = TRUE, custom_query = NULL, max_rows = NULL,
                date_format = NULL, batch_size = 1000, pii = FALSE,
                interview_date = NULL, all = FALSE) {
  # Check if required packages are available
  if (!requireNamespace("RMariaDB", quietly = TRUE)) {
    stop("Package 'RMariaDB' is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("DBI", quietly = TRUE)) {
    stop("Package 'DBI' is needed for this function to work. Please install it.",
         call. = FALSE)
  }

  start_time <- Sys.time()

  # Validate secrets and config - MUST COME FIRST
  validate_secrets("sql")
  config <- validate_config("sql")

  # Initialize loading animation
  pb <- initializeLoadingAnimation(20)

  # Validate parameters
  if (is.null(table_name) && is.null(custom_query)) {
    tables_info <- sql.index()
    if (is.null(tables_info)) {
      stop("No table name or custom query provided, and could not retrieve table list")
    }
    stop("No table name or custom query provided. Use sql.index() to see available tables.")
  }

  # Get connection parameters using get_secret
  host_value <- get_secret("host")
  user_id <- get_secret("uid")
  password <- get_secret("pwd")

  # Parse the host value to extract database name and port if present
  # Default values
  db_name <- if (!is.null(config) && !is.null(config$sql$schema)) {
    config$sql$schema  # Use first configured schema
  } else {
    "mysql"  # Fallback default database
  }
  port_number <- 3306  # Default MySQL/MariaDB port

  # Extract host, port, and database from conn if it's a connection string
  if (grepl("Database=|dbname=", host_value)) {
    # It's a connection string, extract components
    if (grepl("Database=", host_value)) {
      db_name <- gsub(".*Database=([^;]+).*", "\\1", host_value)
    } else if (grepl("dbname=", host_value)) {
      db_name <- gsub(".*dbname=([^;]+).*", "\\1", host_value)
    }
    if (grepl("Port=", host_value)) {
      port_number <- as.integer(gsub(".*Port=([^;]+).*", "\\1", host_value))
    }
    if (grepl("Server=", host_value)) {
      host_value <- gsub(".*Server=([^;]+).*", "\\1", host_value)
    } else if (grepl("host=", host_value)) {
      host_value <- gsub(".*host=([^;]+).*", "\\1", host_value)
    }
  }

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

  # Determine if the requested table is the configured superkey table
  is_superkey_request <- FALSE
  if (!is.null(superkey_table) && !is.null(table_name)) {
    # Extract table name without schema for comparison
    table_name_only <- if (grepl("\\.", table_name)) {
      strsplit(table_name, "\\.")[[1]][2]
    } else {
      table_name
    }
    
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

  # Determine fields to exclude if PII exclusion is enabled
  pii_fields <- character(0)
  if (!pii && !is.null(config$sql$pii_fields)) {
    pii_fields <- config$sql$pii_fields
    if (length(pii_fields) > 0) {
      message(sprintf("Will exclude %d PII fields: %s",
                      length(pii_fields),
                      paste(pii_fields, collapse = ", ")))
    }
  }

  # Establish database connection using RMariaDB
  tryCatch({
    # Connect to MariaDB
    db_conn <- DBI::dbConnect(RMariaDB::MariaDB(),
                              host = host_value,
                              user = user_id,
                              password = password,
                              port = port_number,
                              dbname = db_name)
    message("Database connection established successfully.")
  }, error = function(e) {
    stop(paste("Error connecting to database:", e$message))
  })

  # Update progress bar
  for (i in 11:15) {
    updateLoadingAnimation(pb, i)
    Sys.sleep(0.05)
  }

  # Check if we need to add a schema to the table name
  if (!is.null(table_name) && !grepl("\\.", table_name) && !is.null(config$sql$schema)) {
    # Use the configured schema
    schema <- config$sql$schema
    
    # Build a query to check if the table exists in this schema
    check_query <- sprintf(
      "SELECT 1 FROM information_schema.tables WHERE table_schema = '%s' AND table_name = '%s' LIMIT 1",
      schema, table_name
    )

    # Execute the query
    check_result <- DBI::dbGetQuery(db_conn, check_query)

    # If table exists in this schema, use it
    if (nrow(check_result) > 0) {
      message("Found table '", table_name, "' in schema '", schema, "'")
      table_name <- paste0(schema, ".", table_name)
    } else {
      # If table not found in the configured schema, show an informative error
      stop(sprintf(
        "Table '%s' not found in configured schema '%s'. Please check your config.yml or specify the schema explicitly with '%s.%s'.",
        table_name, schema, schema, table_name
      ))
    }
  }

  # Build or execute SQL query
  result_data <- NULL
  tryCatch({
    if (!is.null(custom_query)) {
      # Execute custom query directly
      result_data <- DBI::dbGetQuery(db_conn, custom_query)

      # If PII exclusion is enabled, filter out PII columns from the result
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
    } else {
      # For PII exclusion with SELECT *, we need to get all column names first
      # with this updated version:

      # For PII exclusion with SELECT *, we need to get all column names first
      if (!pii && !is.null(pii_fields) && length(pii_fields) > 0 && is.null(fields)) {
        # Check if we're joining tables
        if (!is.null(superkey_table) && !is.null(primary_key_column)) {
          # Get column names from both tables
          table_parts <- strsplit(table_name, "\\.")[[1]]
          main_schema <- NULL
          main_table_only <- table_name
          if (length(table_parts) > 1) {
            main_schema <- table_parts[1]
            main_table_only <- table_parts[2]
          }

          # Extract schema and table name for superkey table
          superkey_parts <- strsplit(superkey_table, "\\.")[[1]]
          superkey_schema <- NULL
          superkey_table_only <- superkey_table
          if (length(superkey_parts) > 1) {
            superkey_schema <- superkey_parts[1]
            superkey_table_only <- superkey_parts[2]
          }

          # Get columns from main table
          main_col_query <- sprintf("SELECT column_name FROM information_schema.columns WHERE table_name = '%s'", main_table_only)
          if (!is.null(main_schema)) {
            main_col_query <- sprintf("%s AND table_schema = '%s'", main_col_query, main_schema)
          }

          # Get columns from superkey table
          superkey_col_query <- sprintf("SELECT column_name FROM information_schema.columns WHERE table_name = '%s'", superkey_table_only)
          if (!is.null(superkey_schema)) {
            superkey_col_query <- sprintf("%s AND table_schema = '%s'", superkey_col_query, superkey_schema)
          }

          message("Fetching column lists from both tables to exclude PII fields...")
          main_columns <- DBI::dbGetQuery(db_conn, main_col_query)
          superkey_columns <- DBI::dbGetQuery(db_conn, superkey_col_query)

          if (nrow(main_columns) > 0 && nrow(superkey_columns) > 0) {
            # Filter out PII fields from both tables
            main_valid_columns <- setdiff(main_columns$column_name, pii_fields)
            superkey_valid_columns <- setdiff(superkey_columns$column_name, pii_fields)

            # Build column list with table aliases
            main_column_list <- paste0("t.", main_valid_columns, collapse = ", ")
            superkey_column_list <- paste0("pk.", superkey_valid_columns, collapse = ", ")
            column_list <- paste(main_column_list, superkey_column_list, sep = ", ")

            # Parse table name parts for schema handling
            table_parts <- strsplit(table_name, "\\.")[[1]]
            if (length(table_parts) > 1) {
              schema <- table_parts[1]
              # Apply schema to superkey_table if needed
              if (!grepl("\\.", superkey_table)) {
                superkey_table <- paste0(schema, ".", superkey_table)
              }
            }

            # Determine join type based on 'all' parameter
            join_type <- ifelse(all, "LEFT OUTER JOIN", "INNER JOIN")
            query <- sprintf(
              "SELECT DISTINCT %s FROM %s t %s %s pk ON t.%s = pk.%s %s %s",
              column_list,
              table_name,
              join_type,
              superkey_table,
              primary_key_column, primary_key_column,
              ifelse(!is.null(where_clause) && nchar(where_clause) > 0, paste("WHERE", where_clause), ""),
              ifelse(!is.null(max_rows) && is.numeric(max_rows) && max_rows > 0, paste("LIMIT", as.integer(max_rows)), "")
            )
            message("Generated SQL query with PII exclusion for joined tables: ", query)
          } else {
            # Fall back to standard query building if column fetch fails
            query <- build_sql_query(
              table_name = table_name,
              fields = fields,
              where_clause = where_clause,
              superkey_table = if (join_primary_keys) superkey_table else NULL,
              primary_key_column = if (join_primary_keys) primary_key_column else NULL,
              max_rows = max_rows,
              pii_fields = NULL,  # Don't filter in query, will filter result after
              all = all
            )
          }
        } else {
          # Single table case (existing code)
          table_parts <- strsplit(table_name, "\\.")[[1]]
          schema_name <- NULL
          table_only <- table_name
          if (length(table_parts) > 1) {
            schema_name <- table_parts[1]
            table_only <- table_parts[2]
          }

          # Get all column names from the table
          col_query <- sprintf("SELECT column_name FROM information_schema.columns WHERE table_name = '%s'", table_only)
          # If table_name includes schema, add schema filter
          if (!is.null(schema_name)) {
            col_query <- sprintf("%s AND table_schema = '%s'", col_query, schema_name)
          }
          message("Fetching column list to exclude PII fields...")
          all_columns <- DBI::dbGetQuery(db_conn, col_query)
          if (nrow(all_columns) > 0) {
            # Filter out PII fields
            valid_columns <- setdiff(all_columns$column_name, pii_fields)
            column_list <- paste(valid_columns, collapse = ", ")
            query <- sprintf(
              "SELECT DISTINCT %s FROM %s %s %s",
              column_list,
              table_name,
              ifelse(!is.null(where_clause) && nchar(where_clause) > 0, paste("WHERE", where_clause), ""),
              ifelse(!is.null(max_rows) && is.numeric(max_rows) && max_rows > 0, paste("LIMIT", as.integer(max_rows)), "")
            )
            message("Generated SQL query with PII exclusion: ", query)
          } else {
            # Fall back to standard query building if column fetch fails
            query <- build_sql_query(
              table_name = table_name,
              fields = fields,
              where_clause = where_clause,
              superkey_table = if (join_primary_keys) superkey_table else NULL,
              primary_key_column = if (join_primary_keys) primary_key_column else NULL,
              max_rows = max_rows,
              pii_fields = NULL,  # Don't filter in query, will filter result after
              all = all
            )
          }
        }
      } else {
        # Standard query building
        query <- build_sql_query(
          table_name = table_name,
          fields = fields,
          where_clause = where_clause,
          superkey_table = if (join_primary_keys) superkey_table else NULL,
          primary_key_column = if (join_primary_keys) primary_key_column else NULL,
          max_rows = max_rows,
          pii_fields = if (!pii) pii_fields else NULL,
          all = all
        )
      }

      # Execute the query
      result_data <- DBI::dbGetQuery(db_conn, query)

      # Double-check PII exclusion on the result
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
    }
  }, error = function(e) {
    DBI::dbDisconnect(db_conn)
    stop(paste("Error executing SQL query:", e$message))
  })

  # Update progress bar
  for (i in 16:20) {
    updateLoadingAnimation(pb, i)
    Sys.sleep(0.05)
  }

  completeLoadingAnimation(pb)

  # Close the database connection
  DBI::dbDisconnect(db_conn)

  # Process and clean the data
  if (nrow(result_data) > 0) {
    # Process date fields
    if (!is.null(date_format)) {
      result_data <- process_date_fields(result_data, date_format)
    }

    # Handle interview_date filtering if specified
    if (!is.null(interview_date) && "interview_date" %in% names(result_data)) {
      result_data <- filter_by_interview_date(result_data, interview_date)
    }

    # Handle missing values using the configuration
    result_data <- handle_missing_values(result_data, config)

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

#' Get a list of tables from the SQL database
#'
#' @param schema Optional schema name to filter tables
#' @return A data frame with table information
#' @export
sql.index <- function(schema = NULL) {
  # Check if required packages are available
  if (!requireNamespace("RMariaDB", quietly = TRUE)) {
    stop("Package 'RMariaDB' is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("DBI", quietly = TRUE)) {
    stop("Package 'DBI' is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop("Package 'knitr' is needed for this function to work. Please install it.",
         call. = FALSE)
  }

  # Validate secrets and config
  validate_secrets("sql")
  config <- validate_config("sql")

  # Get connection parameters using get_secret
  host_value <- get_secret("host")
  user_id <- get_secret("uid")
  password <- get_secret("pwd")

  # Parse the host value to extract database name and port if present
  # Default values
  db_name <- if (!is.null(config) && !is.null(config$sql$schema)) {
    config$sql$schema  # Use first configured schema
  } else {
    "mysql"  # Fallback default database
  }
  port_number <- 3306  # Default MySQL/MariaDB port

  # Extract host, port, and database from conn if it's a connection string
  if (grepl("Database=|dbname=", host_value)) {
    # It's a connection string, extract components
    if (grepl("Database=", host_value)) {
      db_name <- gsub(".*Database=([^;]+).*", "\\1", host_value)
    } else if (grepl("dbname=", host_value)) {
      db_name <- gsub(".*dbname=([^;]+).*", "\\1", host_value)
    }

    if (grepl("Port=", host_value)) {
      port_number <- as.integer(gsub(".*Port=([^;]+).*", "\\1", host_value))
    }

    if (grepl("Server=", host_value)) {
      host_value <- gsub(".*Server=([^;]+).*", "\\1", host_value)
    } else if (grepl("host=", host_value)) {
      host_value <- gsub(".*host=([^;]+).*", "\\1", host_value)
    }
  }

  # Connect to database
  db_conn <- NULL
  tables <- NULL

  tryCatch({
    # Connect to MariaDB
    db_conn <- DBI::dbConnect(RMariaDB::MariaDB(),
                              host = host_value,
                              user = user_id,
                              password = password,
                              port = port_number,
                              dbname = db_name)

    # Get tables
    if (is.null(schema)) {
      # Query for all tables
      tables <- DBI::dbGetQuery(db_conn,
                                "SELECT table_schema AS 'Schema',
                               table_name AS 'Table',
                               table_type AS 'Type'
                               FROM information_schema.tables
                               ORDER BY table_schema, table_name")
    } else {
      # Query for tables in the specified schema
      tables <- DBI::dbGetQuery(db_conn,
                                sprintf("SELECT table_schema AS 'Schema',
                                      table_name AS 'Table',
                                      table_type AS 'Type'
                                      FROM information_schema.tables
                                      WHERE table_schema = '%s'
                                      ORDER BY table_schema, table_name",
                                        schema))
    }
  }, error = function(e) {
    message(paste("Error retrieving tables:", e$message))
    return(NULL)
  }, finally = {
    # Close connection if it was opened
    if (!is.null(db_conn)) {
      DBI::dbDisconnect(db_conn)
    }
  })

  # Format the result
  if (!is.null(tables) && nrow(tables) > 0) {
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
#' @export
sql.desc <- function(table_name) {
  # Check if required packages are available
  if (!requireNamespace("RMariaDB", quietly = TRUE)) {
    stop("Package 'RMariaDB' is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("DBI", quietly = TRUE)) {
    stop("Package 'DBI' is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop("Package 'knitr' is needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (is.null(table_name)) {
    stop("Table name is required")
  }

  # Validate secrets
  validate_secrets("sql")
  config <- validate_config("sql")

  # Get connection parameters using get_secret
  host_value <- get_secret("host")
  user_id <- get_secret("uid")
  password <- get_secret("pwd")

  # Parse table_name to extract schema and table
  parts <- strsplit(table_name, "\\.")[[1]]
  schema_name <- NULL
  table_only <- table_name

  if (length(parts) > 1) {
    # Explicit schema.table notation
    schema_name <- parts[1]
    table_only <- parts[2]
  } else {
    # No schema specified, use the first configured schema
    if (!is.null(config) && !is.null(config$sql$schema)) {
      schema_name <- config$sql$schema
      message(sprintf("No schema specified. Using default schema: %s", schema_name))
    }
  }

  # Parse the host value to extract database name and port if present
  # Default values
  db_name <- if (!is.null(config) && !is.null(config$sql$schema)) {
    config$sql$schema  # Use first configured schema
  } else {
    "mysql"  # Fallback default database
  }
  port_number <- 3306  # Default MySQL/MariaDB port

  # Extract host, port, and database from conn if it's a connection string
  if (grepl("Database=|dbname=", host_value)) {
    # It's a connection string, extract components
    if (grepl("Database=", host_value)) {
      db_name <- gsub(".*Database=([^;]+).*", "\\1", host_value)
    } else if (grepl("dbname=", host_value)) {
      db_name <- gsub(".*dbname=([^;]+).*", "\\1", host_value)
    }
    if (grepl("Port=", host_value)) {
      port_number <- as.integer(gsub(".*Port=([^;]+).*", "\\1", host_value))
    }
    if (grepl("Server=", host_value)) {
      host_value <- gsub(".*Server=([^;]+).*", "\\1", host_value)
    } else if (grepl("host=", host_value)) {
      host_value <- gsub(".*host=([^;]+).*", "\\1", host_value)
    }
  }

  # Connect to database
  db_conn <- NULL
  columns <- NULL

  tryCatch({
    # Connect to MariaDB
    db_conn <- DBI::dbConnect(RMariaDB::MariaDB(),
                              host = host_value,
                              user = user_id,
                              password = password,
                              port = port_number,
                              dbname = db_name)

    # Query for column information
    if (!is.null(schema_name)) {
      # Use the specified schema
      columns <- DBI::dbGetQuery(db_conn,
                                 sprintf("SELECT column_name AS 'Column',
                                       data_type AS 'Type',
                                       character_maximum_length AS 'Size',
                                       is_nullable AS 'Nullable',
                                       column_comment AS 'Description'
                                       FROM information_schema.columns
                                       WHERE table_schema = '%s' AND table_name = '%s'
                                       ORDER BY ordinal_position",
                                         schema_name, table_only))

      # If no columns found with the specified schema, try to find the table in any schema
      if (nrow(columns) == 0) {
        message(sprintf("No columns found for table '%s' in schema '%s'. Checking all schemas...",
                        table_only, schema_name))

        columns <- DBI::dbGetQuery(db_conn,
                                   sprintf("SELECT table_schema AS 'Schema',
                                          column_name AS 'Column',
                                          data_type AS 'Type',
                                          character_maximum_length AS 'Size',
                                          is_nullable AS 'Nullable',
                                          column_comment AS 'Description'
                                          FROM information_schema.columns
                                          WHERE table_name = '%s'
                                          ORDER BY table_schema, ordinal_position",
                                           table_only))
      }
    } else {
      # Try to find the table in any schema
      columns <- DBI::dbGetQuery(db_conn,
                                 sprintf("SELECT table_schema AS 'Schema',
                                      column_name AS 'Column',
                                      data_type AS 'Type',
                                      character_maximum_length AS 'Size',
                                      is_nullable AS 'Nullable',
                                      column_comment AS 'Description'
                                      FROM information_schema.columns
                                      WHERE table_name = '%s'
                                      ORDER BY table_schema, ordinal_position",
                                         table_only))
    }
  }, error = function(e) {
    if (!is.null(db_conn)) {
      DBI::dbDisconnect(db_conn)
    }
    stop(paste("Error retrieving columns for table", table_name, ":", e$message))
  }, finally = {
    # Close connection if it was opened
    if (!is.null(db_conn)) {
      DBI::dbDisconnect(db_conn)
    }
  })

  # Format the result
  if (!is.null(columns) && nrow(columns) > 0) {
    return(knitr::kable(columns, format = "simple"))
  } else {
    message(paste("No columns found for table", table_name))
    return(NULL)
  }
}

#' Perform a direct SQL query with minimal processing
#'
#' @param query The SQL query to execute
#' @param exclude_pii Default TRUE to remove all fields marked as identifiable
#' @return A data frame with the query results
#' @export
sql.query <- function(query, exclude_pii = FALSE) {
  # Check if required packages are available
  if (!requireNamespace("RMariaDB", quietly = TRUE)) {
    stop("Package 'RMariaDB' is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("DBI", quietly = TRUE)) {
    stop("Package 'DBI' is needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (is.null(query) || !is.character(query) || length(query) != 1) {
    stop("A valid SQL query string is required")
  }

  # Validate secrets and config
  validate_secrets("sql")
  config <- validate_config("sql")

  # Get connection parameters using get_secret
  host_value <- get_secret("host")
  user_id <- get_secret("uid")
  password <- get_secret("pwd")

  # Parse the host value to extract database name and port if present
  # Default values
  db_name <- if (!is.null(config) && !is.null(config$sql$schema)) {
    config$sql$schema  # Use first configured schema
  } else {
    "mysql"  # Fallback default database
  }
  port_number <- 3306  # Default MySQL/MariaDB port

  # Extract host, port, and database from conn if it's a connection string
  if (grepl("Database=|dbname=", host_value)) {
    # It's a connection string, extract components
    if (grepl("Database=", host_value)) {
      db_name <- gsub(".*Database=([^;]+).*", "\\1", host_value)
    } else if (grepl("dbname=", host_value)) {
      db_name <- gsub(".*dbname=([^;]+).*", "\\1", host_value)
    }

    if (grepl("Port=", host_value)) {
      port_number <- as.integer(gsub(".*Port=([^;]+).*", "\\1", host_value))
    }

    if (grepl("Server=", host_value)) {
      host_value <- gsub(".*Server=([^;]+).*", "\\1", host_value)
    } else if (grepl("host=", host_value)) {
      host_value <- gsub(".*host=([^;]+).*", "\\1", host_value)
    }
  }

  # Get config for PII exclusion if needed
  config <- NULL
  pii_fields <- NULL
  if (exclude_pii) {
    tryCatch({
      config <- validate_config("sql")
      if (!is.null(config$sql$pii_fields)) {
        pii_fields <- config$sql$pii_fields
        if (length(pii_fields) > 0) {
          message(sprintf("Will exclude %d PII fields if present in results: %s",
                          length(pii_fields),
                          paste(pii_fields, collapse=", ")))
        }
      }
    }, error = function(e) {
      warning("Could not load PII field configuration: ", e$message)
    })
  }

  # Connect to database
  db_conn <- NULL
  result <- NULL

  tryCatch({
    # Connect to MariaDB
    db_conn <- DBI::dbConnect(RMariaDB::MariaDB(),
                              host = host_value,
                              user = user_id,
                              password = password,
                              port = port_number,
                              dbname = db_name)

    # Execute query
    result <- DBI::dbGetQuery(db_conn, query)

    # Apply PII exclusion if enabled
    if (exclude_pii && !is.null(pii_fields) && length(pii_fields) > 0) {
      pii_cols_present <- intersect(names(result), pii_fields)
      if (length(pii_cols_present) > 0) {
        message(sprintf("Removing %d PII fields from results: %s",
                        length(pii_cols_present),
                        paste(pii_cols_present, collapse=", ")))
        result <- result[, !names(result) %in% pii_fields, drop = FALSE]
      }
    }

    # Apply missing value handling if configured
    if (!is.null(config) && !is.null(config$missing_data_codes) && nrow(result) > 0) {
      result <- handle_missing_values(result, config)
    }

  }, error = function(e) {
    if (!is.null(db_conn)) {
      DBI::dbDisconnect(db_conn)
    }
    stop(paste("Error executing query:", e$message))
  }, finally = {
    # Close connection if it was opened
    if (!is.null(db_conn)) {
      DBI::dbDisconnect(db_conn)
    }
  })

  return(result)
}

build_sql_query <- function(table_name, fields = NULL, where_clause = NULL,
                            superkey_table = NULL, primary_key_column = NULL,
                            max_rows = NULL, pii_fields = NULL, all = FALSE) {

  # Define table aliases
  main_table_alias <- "t"
  pk_table_alias <- "pk"

  # Determine which fields to select
  if (is.null(fields)) {
    # No specific fields requested
    if (!is.null(superkey_table) && !is.null(primary_key_column)) {
      # When joining, always include fields from both tables
      if (!is.null(pii_fields) && length(pii_fields) > 0) {
        # PII exclusion needed but no specific fields requested
        # This requires getting table metadata first, which we can't do in this function
        message("Note: PII exclusion with * requires listing all non-PII columns explicitly")
        select_clause <- "SELECT DISTINCT t.*, pk.*" # Will be replaced in main function
      } else {
        # No PII exclusion needed - include all fields from both tables
        select_clause <- "SELECT DISTINCT t.*, pk.*"
      }
    } else {
      # No join - just select from main table
      if (!is.null(pii_fields) && length(pii_fields) > 0) {
        message("Note: PII exclusion with * requires listing all non-PII columns explicitly")
        select_clause <- "SELECT DISTINCT *" # Will be replaced in main function
      } else {
        select_clause <- "SELECT DISTINCT *"
      }
    }
  } else {
    # Specific fields requested, exclude PII fields
    if (!is.null(pii_fields)) {
      fields <- setdiff(fields, pii_fields)
    }
    # If fields array is not empty after filtering
    if (length(fields) > 0) {
      # For joins, qualify field names with table alias
      if (!is.null(superkey_table) && !is.null(primary_key_column)) {
        # Check if fields already have table qualifiers
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
        select_clause <- paste0("SELECT DISTINCT ", paste(qualified_fields, collapse = ", "))
      } else {
        select_clause <- paste0("SELECT DISTINCT ", paste(fields, collapse = ", "))
      }
    } else {
      # All requested fields were PII, so select a dummy field
      select_clause <- "SELECT DISTINCT 1 AS dummy_column" # Fallback
    }
  }

  # Build the FROM clause
  # Handle schema in table name
  table_parts <- strsplit(table_name, "\\.")[[1]]
  if (!is.null(superkey_table) && !is.null(primary_key_column)) {
    # If table_name includes a schema, apply it to superkey_table if needed
    if (length(table_parts) > 1) {
      schema <- table_parts[1]
      # Apply schema to superkey_table if it doesn't already have one
      if (!grepl("\\.", superkey_table)) {
        superkey_table <- paste0(schema, ".", superkey_table)
      }
    }
    # Determine join type based on 'all' parameter
    join_type <- ifelse(all, "LEFT OUTER JOIN", "INNER JOIN")
    # Create JOIN clause
    from_clause <- sprintf(
      "FROM %s %s %s %s %s ON %s.%s = %s.%s",
      table_name, main_table_alias,
      join_type,
      superkey_table, pk_table_alias,
      main_table_alias, primary_key_column,
      pk_table_alias, primary_key_column
    )
  } else {
    # No join needed
    from_clause <- sprintf("FROM %s %s", table_name, main_table_alias)
  }

  # Add WHERE clause if provided
  where_part <- ""
  if (!is.null(where_clause) && nchar(where_clause) > 0) {
    where_part <- paste("WHERE", where_clause)
  }

  # Add LIMIT clause if max_rows is specified
  limit_part <- ""
  if (!is.null(max_rows) && is.numeric(max_rows) && max_rows > 0) {
    limit_part <- paste("LIMIT", as.integer(max_rows))
  }

  # Combine all parts into the final query
  query <- paste(select_clause, from_clause, where_part, limit_part)

  # For debugging
  message("Generated SQL query: ", query)

  # Add an attribute to indicate if column list expansion is needed for PII exclusion
  if (!is.null(pii_fields) && length(pii_fields) > 0 && is.null(fields)) {
    attr(query, "needs_column_list") <- TRUE
  }

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

#' Handle missing values in SQL data based on configuration
#'
#' This function processes a data frame and replaces values that match the missing data codes
#' defined in the configuration with NA. It handles different types of missing data
#' (skipped, refused, unknown, missing) according to the configuration.
#'
#' @param df Data frame containing SQL query results
#' @param config Configuration object loaded from config.yml
#' @return Data frame with missing values handled according to configuration
#' @noRd
handle_missing_values <- function(df, config) {
  # Return the dataframe unchanged if no missing_data_codes configuration
  if (is.null(config) || is.null(config$missing_data_codes)) {
    return(df)
  }

  # Get missing data codes from configuration
  missing_codes <- config$missing_data_codes
  transformed_count <- 0
  transformed_cols <- character(0)

  # Process each category of missing codes
  for (category in names(missing_codes)) {
    codes <- missing_codes[[category]]

    # Skip if no codes defined for this category
    if (length(codes) == 0) next

    # Apply to all columns in the dataframe
    for (col in names(df)) {
      # Only process numeric or character columns
      if (is.numeric(df[[col]]) || is.character(df[[col]])) {
        # For each code in this category
        for (code in codes) {
          # Convert to the appropriate type
          typed_code <- if (is.numeric(df[[col]])) {
            suppressWarnings(as.numeric(code))
          } else {
            as.character(code)
          }

          # Skip if conversion failed (e.g., non-numeric code for numeric column)
          if (is.na(typed_code) && !is.character(df[[col]])) next

          # Count matches before replacement
          matches <- sum(df[[col]] == typed_code, na.rm = TRUE)

          # Replace the code with NA if matches found
          if (matches > 0) {
            df[[col]][df[[col]] == typed_code] <- NA
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

  # Report changes if any were made
  if (transformed_count > 0) {
    message(sprintf("Replaced %d missing value codes with NA in %d column(s): %s",
                    transformed_count,
                    length(transformed_cols),
                    paste(transformed_cols, collapse=", ")))
  }

  return(df)
}
