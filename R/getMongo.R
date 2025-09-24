#' Cross-platform memory check function
#' @return List containing total and available memory in GB
#' @noRd
getAvailableMemory <- function() {
  tryCatch({
    if (.Platform$OS.type == "windows") {
      # Windows-specific memory detection with better error handling
      total_mem <- tryCatch({
        mem_info <- system('wmic ComputerSystem get TotalPhysicalMemory /Value', intern = TRUE)
        mem_line <- grep("TotalPhysicalMemory=", mem_info, value = TRUE)
        if (length(mem_line) == 0) return(NULL)
        total <- as.numeric(sub("TotalPhysicalMemory=", "", mem_line))
        if (is.na(total)) return(NULL)
        total / (1024^3)  # Convert to GB
      }, error = function(e) NULL)

      avail_mem <- tryCatch({
        # Get multiple memory metrics for better available memory calculation
        mem_info <- system('wmic OS get FreePhysicalMemory,TotalVisibleMemorySize /Value', intern = TRUE)
        # Extract free physical memory
        free_line <- grep("FreePhysicalMemory=", mem_info, value = TRUE)
        if (length(free_line) == 0) return(NULL)
        free_mem <- as.numeric(sub("FreePhysicalMemory=", "", free_line))
        if (is.na(free_mem)) return(NULL)
        # Get total visible memory for percentage calculation
        total_line <- grep("TotalVisibleMemorySize=", mem_info, value = TRUE)
        if (length(total_line) == 0) return(NULL)
        total_visible <- as.numeric(sub("TotalVisibleMemorySize=", "", total_line))
        if (is.na(total_visible)) return(NULL)
        # Convert KB to GB and add 20% buffer for cached memory
        available <- (free_mem / (1024^2)) * 1.2
        # Sanity check - don't return more than 90% of total memory
        max_available <- (total_visible / (1024^2)) * 0.9
        min(available, max_available)
      }, error = function(e) NULL)

      # Return both metrics, with NULL handling
      return(list(
        total = if (is.null(total_mem)) NULL else round(total_mem, 1),
        available = if (is.null(avail_mem)) NULL else round(avail_mem, 1)
      ))

    } else if (Sys.info()["sysname"] == "Darwin") {
      # MacOS
      total_mem <- tryCatch({
        mem_info <- system("sysctl hw.memsize", intern = TRUE)
        as.numeric(strsplit(mem_info, " ")[[1]][2]) / (1024^3)
      }, error = function(e) NULL)

      # More accurate available memory detection for Mac
      avail_mem <- tryCatch({
        vm_stat <- system("vm_stat", intern = TRUE)
        page_size <- 4096  # Default page size for Mac
        # Extract different memory stats
        get_pages <- function(pattern) {
          line <- grep(pattern, vm_stat, value = TRUE)
          as.numeric(sub(".*: *(\\d+).*", "\\1", line))
        }
        free_pages <- get_pages("Pages free:")
        inactive_pages <- get_pages("Pages inactive:")
        purgeable_pages <- get_pages("Pages purgeable:")
        cached_pages <- get_pages("File-backed pages:")
        # Calculate available memory including cache and purgeable
        total_available_pages <- free_pages + inactive_pages + purgeable_pages + cached_pages
        (total_available_pages * page_size) / (1024^3)  # Convert to GB
      }, error = function(e) NULL)

      return(list(
        total = total_mem,
        available = avail_mem
      ))

    } else {
      # Linux
      if (file.exists("/proc/meminfo")) {
        mem_info <- readLines("/proc/meminfo")
        # Helper function to extract memory values
        get_mem_value <- function(pattern) {
          line <- grep(pattern, mem_info, value = TRUE)
          value <- as.numeric(strsplit(line, "\\s+")[[1]][2])  # Get the number
          value / (1024^2)  # Convert KB to GB
        }
        # Get all relevant memory metrics
        total_mem <- get_mem_value("MemTotal:")
        free_mem <- get_mem_value("MemFree:")
        available_mem <- get_mem_value("MemAvailable:")  # Modern Linux kernels provide this
        cached_mem <- get_mem_value("Cached:")
        buffers_mem <- get_mem_value("Buffers:")
        slab_mem <- get_mem_value("SReclaimable:")  # Reclaimable kernel memory

        # Calculate true available memory
        # MemAvailable is already calculated by kernel using a sophisticated algorithm
        # But we can fall back to our own calculation if needed
        if (!is.na(available_mem)) {
          avail_mem <- available_mem
        } else {
          # Similar to how the kernel calculates it:
          # free + ((cached + buffers + slab) * 0.8)
          avail_mem <- free_mem + ((cached_mem + buffers_mem + slab_mem) * 0.8)
        }

        return(list(
          total = total_mem,
          available = avail_mem
        ))
      }
    }
  }, error = function(e) {
    return(list(total = NULL, available = NULL))
  })

  return(list(total = NULL, available = NULL))
}

#' Calculate optimal resource parameters
#' @param total_records Total number of records to process
#' @param mem_info Memory information structure
#' @param num_cores Number of CPU cores
#' @return List containing optimal chunk size and number of workers
#' @noRd
calculateResourceParams <- function(total_records, mem_info, num_cores) {
  # Default values
  params <- list(
    chunk_size = 1000,
    workers = min(4, num_cores)  # OPTIMIZATION: Fewer workers for consistency
  )

  # Adjust chunk size based on available memory and total records
  if (!is.null(mem_info$available)) {
    if (mem_info$available < 4) {
      params$chunk_size <- 500
    } else if (mem_info$available < 8) {
      params$chunk_size <- 1000
    } else if (mem_info$available < 16) {
      params$chunk_size <- 2000
    } else {
      params$chunk_size <- 5000
    }
  }

  # OPTIMIZATION: Larger chunks for bigger datasets
  if (total_records > 50000) {
    params$chunk_size <- max(params$chunk_size, 10000)
  }

  # Adjust for very small datasets
  if (total_records < params$chunk_size * 2) {
    params$chunk_size <- max(500, floor(total_records / 2))
  }

  # Calculate resulting chunks
  params$num_chunks <- ceiling(total_records / params$chunk_size)

  return(params)
}

#' Initialize a clean loading animation
#' @param steps Number of steps in the process
#' @return Loading animation object
#' @noRd
initializeLoadingAnimation <- function(steps) {
  list(
    steps = steps,
    current = 0,
    width = 50,
    start_time = Sys.time()
  )
}

#' Update the loading animation
#' @param pb Loading animation object
#' @param current Current step
#' @noRd
updateLoadingAnimation <- function(pb, current) {
  pb$current <- current
  percentage <- round(current / pb$steps * 100)
  filled <- round(pb$width * current / pb$steps)
  bar <- paste0(
    strrep("=", filled),
    strrep(" ", pb$width - filled)
  )
  cat(sprintf("\r  |%s| %3d%%", bar, percentage))
  utils::flush.console()
}

#' Complete the loading animation
#' @param pb Loading animation object
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

#' Create consistent chunks using unique IDs instead of skip/limit
#' @param Mongo MongoDB connection object
#' @param identifier Field to use as identifier
#' @param chunk_size Number of records per chunk
#' @return List containing chunks with specific IDs and total record count
#' @noRd
createConsistentChunks <- function(Mongo, identifier, chunk_size) {
  message("Getting unique identifiers for consistent chunking...")

  # Get ALL unique identifiers first (eliminates skip/limit issues)
  all_ids <- Mongo$distinct(identifier,
                            query = sprintf('{"%s": {"$exists": true, "$ne": ""}}', identifier))

  total_records <- length(all_ids)
  num_chunks <- ceiling(total_records / chunk_size)

  chunks <- vector("list", num_chunks)
  for (i in seq_len(num_chunks)) {
    start_idx <- (i - 1) * chunk_size + 1
    end_idx <- min(i * chunk_size, total_records)

    chunks[[i]] <- list(
      ids = all_ids[start_idx:end_idx],  # Specific IDs, not skip/limit
      size = length(all_ids[start_idx:end_idx])
    )
  }

  return(list(chunks = chunks, total_records = total_records))
}

#' Retrieve MongoDB data using $in queries for consistency
#' @param Mongo MongoDB connection object
#' @param identifier Field to use as identifier
#' @param chunk_info Chunk information with specific IDs
#' @param verbose Logical for verbose output
#' @return Data frame with consistent results
#' @noRd
getMongoDataConsistent <- function(Mongo, identifier, chunk_info, verbose = FALSE) {
  if (length(chunk_info$ids) == 0) {
    return(data.frame())
  }

  # Use $in query with specific IDs (eliminates skip/limit race conditions)
  query_json <- sprintf('{"%s": {"$in": %s}}',
                        identifier,
                        jsonlite::toJSON(chunk_info$ids, auto_unbox = FALSE))

  if(verbose) message(paste("Using $in query for", length(chunk_info$ids), "specific IDs"))

  df <- Mongo$find(query = query_json)

  if(verbose) message(paste("Retrieved", nrow(df), "rows"))

  return(df)
}

#' Validate results for duplicates and consistency
#' @param df Data frame to validate
#' @param identifier Field to check for duplicates
#' @return Logical indicating if validation passed
#' @noRd
validateResults <- function(df, identifier) {
  if (nrow(df) == 0) return(TRUE)

  duplicates <- sum(duplicated(df[[identifier]]))
  if (duplicates > 0) {
    warning(sprintf("Found %d duplicate records for identifier %s", duplicates, identifier))
    return(FALSE)
  }

  message(sprintf("Validation passed: %d unique records", nrow(df)))
  return(TRUE)
}

#' Fetch data from MongoDB to be stored in a data frame - UPDATED VERSION
#'
#' @param collection The name of the MongoDB collection
#' @param ... Optional column names to filter for. Only rows with non-missing values
#'        in ALL specified columns will be returned. This is useful for filtering
#'        data to only include complete cases for specific variables of interest.
#' @param database The database name (optional)
#' @param identifier Field to use as identifier (optional)
#' @param chunk_size Number of records per chunk (optional)
#' @param verbose Logical; if TRUE, displays detailed progress messages. Default is FALSE.
#' @param interview_date Optional; can be either:
#'        - A date string in various formats (ISO, US, etc.) to filter data up to that date
#'        - A boolean TRUE to return only rows with non-NA interview_date values
#'
#' @importFrom mongolite mongo ssl_options
#' @importFrom parallel detectCores
#' @importFrom future plan multisession
#' @importFrom future future
#' @importFrom future.apply future_lapply
#' @importFrom dplyr bind_rows
#' @importFrom utils flush.console
#' @importFrom stats setNames
#' @importFrom jsonlite toJSON
#'
#' @return A data frame containing the MongoDB data with superkeys first
#' @export
#' @examples
#' \dontrun{
#' # Get data from MongoDB collection
#' data <- mongo("collection")
#' }
mongo <- function(collection, ..., database = NULL, identifier = NULL, chunk_size = NULL, verbose = FALSE, interview_date = NULL) {
  start_time <- Sys.time()
  Mongo <- NULL  # Initialize to NULL for cleanup in on.exit

  # Setup cleanup on exit
  on.exit({
    disconnectMongo(Mongo)
  })

  # Suppress MongoDB messages globally
  options(mongolite.quiet = TRUE)

  # Get configuration
  cfg <- validate_config("mongo")
  if (is.null(database)) {
    database <- cfg$mongo$collection
  }

  # Validate identifier
  if (is.null(identifier)) {
    identifier <- cfg$identifier
  }
  if (is.null(identifier) || any(identifier == "")) {
    stop("No identifier specified in the config file.")
  }

  # Try connecting - will now throw explicit error if collection doesn't exist
  Mongo <- ConnectMongo(collection, database)

  # Find valid identifier
  if (is.null(identifier)) {
    for (key in trimws(strsplit(identifier, ",")[[1]])) {
      count <- Mongo$count(sprintf('{"$s": {"$exists": true, "$ne": ""}}', key))
      if (count > 0) {
        identifier <- key
        break
      }
    }
  }

  if (is.null(identifier)) {
    stop("No valid identifier found in the collection.")
  }

  # Get and display system resources
  mem_info <- getAvailableMemory()
  num_cores <- parallel::detectCores(logical = TRUE)

  # Display system info
  if (!is.null(mem_info$total)) {
    message(sprintf("System resources: %.0fGB RAM, %d-core CPU",
                    mem_info$total, num_cores))
  } else {
    message(sprintf("System resources: %d-core CPU.", num_cores))
  }

  if (!is.null(mem_info$available)) {
    message(sprintf("Memory available: %.0fGB RAM", mem_info$available))
  }

  # NEW: Use consistent chunking instead of skip/limit
  chunk_result <- createConsistentChunks(Mongo, identifier,
                                         if(is.null(chunk_size)) 1000 else chunk_size)
  chunks <- chunk_result$chunks
  total_records <- chunk_result$total_records  # Use actual count from distinct()

  # Calculate optimal parameters with the new optimizations
  params <- calculateResourceParams(total_records, mem_info, num_cores)

  # Use optimized chunk size if not manually specified
  if (is.null(chunk_size)) {
    # Recreate chunks with optimized size
    chunk_result <- createConsistentChunks(Mongo, identifier, params$chunk_size)
    chunks <- chunk_result$chunks
  }

  message(sprintf("Processing: %d chunks x ~%d records in parallel (%d workers)",
                  length(chunks), params$chunk_size, params$workers))

  # Setup parallel processing with optimized worker count
  plan(future::multisession, workers = params$workers)

  # Progress message
  message(sprintf("\nImporting %s records from %s/%s into dataframe...",
                  formatC(total_records, format = "d", big.mark = ","),
                  database, collection))

  # Initialize custom progress bar
  pb <- initializeLoadingAnimation(length(chunks))

  # Process chunks with consistent retrieval
  future_results <- vector("list", length(chunks))
  for (i in seq_along(chunks)) {
    future_results[[i]] <- future({
      temp <- tempfile()
      sink(temp)
      chunk_mongo <- NULL
      on.exit({
        sink()
        unlink(temp)
        disconnectMongo(chunk_mongo)
      })

      tryCatch({
        chunk_mongo <- ConnectMongo(collection, database)
        # Use NEW consistent retrieval method
        data_chunk <- getMongoDataConsistent(chunk_mongo, identifier, chunks[[i]], verbose)
        data_chunk
      }, error = function(e) {
        warning(sprintf("Error processing chunk %d: %s", i, e$message))
        NULL
      })
    })
    updateLoadingAnimation(pb, i)
  }

  # Collect results
  results <- lapply(future_results, future::value)

  # Combine results
  df <- dplyr::bind_rows(results)
  completeLoadingAnimation(pb)

  # NEW: Validate results for consistency
  validation_passed <- validateResults(df, identifier)
  if (!validation_passed) {
    warning("Data consistency validation failed. Results may contain duplicates.")
  }

  # Handle interview_date filtering if needed
  if (!is.null(interview_date) && "interview_date" %in% names(df)) {
    message("Filtering by interview date...", appendLF = FALSE)
    # Convert dates only once for the whole dataframe - much more efficient
    if (!inherits(df$interview_date, "Date")) {
      df$interview_date <- parse_dates_to_iso(df$interview_date, "interview_date")
      # The parse_dates_to_iso function already outputs a message with success percentage
    } else {
      message(" using existing Date format.")
    }

    # Apply the filter based on the parameter type
    if (is.logical(interview_date) && interview_date) {
      # Keep only non-NA interview dates
      df <- df[!is.na(df$interview_date), ]
    } else if (is.character(interview_date) || inherits(interview_date, "Date")) {
      # Parse the cutoff date once
      if (is.character(interview_date)) {
        cutoff_date <- parse_dates_to_iso(interview_date, "filter_date")
        if (all(is.na(cutoff_date))) {
          stop("Failed to parse interview_date parameter: ", interview_date)
        }
      } else {
        cutoff_date <- interview_date
      }
      # Keep only rows with dates up to the cutoff
      # Make sure we only keep non-NA dates before or equal to the cutoff
      df <- df[!is.na(df$interview_date) & df$interview_date <= cutoff_date, ]
    }
  }

  # Harmonize data
  message(sprintf("Harmonizing data on %s...", identifier), appendLF = FALSE)
  clean_df <- taskHarmonization(df, identifier, collection)
  message(sprintf("\rHarmonizing data on %s...done.", identifier))

  # List of allowed superkey columns to prioritize
  allowed_superkey_cols <- c(
    "record_id",
    "src_subject_id",
    "subjectkey",
    "site",
    "subsiteid",
    "sex",
    "race",
    "ethnic_group",
    "phenotype",
    "phenotype_description",
    "state",
    "status",
    "lost_to_followup",
    "lost_to_follow-up",
    "twins_study",
    "sibling_study",
    "family_study",
    "sample_taken",
    "interview_date",
    "interview_age",
    "visit",
    "week"
  )

  # Reorder columns to have superkeys first
  if (is.data.frame(clean_df) && ncol(clean_df) > 0) {
    # Identify which superkey columns are actually in the data
    present_superkeys <- intersect(allowed_superkey_cols, names(clean_df))
    # Get all other columns (non-superkeys)
    other_cols <- setdiff(names(clean_df), present_superkeys)
    # If there are matching superkeys, reorder the columns
    if (length(present_superkeys) > 0) {
      # Create new column order with superkeys first, then other columns
      new_order <- c(present_superkeys, other_cols)
      # Reorder the dataframe
      clean_df <- clean_df[, new_order, drop = FALSE]
    }
  }

  # Check if any column requests were passed via ...
  dots_args <- list(...)
  if (length(dots_args) > 0) {
    # Convert the dots arguments to a character vector
    requested_cols <- as.character(unlist(dots_args))
    # Find which of the requested columns actually exist in the data
    existing_cols <- intersect(requested_cols, names(clean_df))
    if (length(existing_cols) > 0) {
      # Display the names of the columns that were found
      message(sprintf("Found %d of %d requested columns: %s",
                      length(existing_cols),
                      length(requested_cols),
                      paste(existing_cols, collapse = ", ")))
      # Create a filter to keep only rows where ALL requested columns have data
      rows_to_keep <- rep(TRUE, nrow(clean_df))
      for (col in existing_cols) {
        # Check if column values are not NA
        not_na <- !is.na(df[[col]])
        # For non-NA values, check if they're not empty strings (if character type)
        not_empty <- rep(TRUE, nrow(clean_df))
        if (is.character(clean_df[[col]])) {
          not_empty <- clean_df[[col]] != ""
        }
        # Combine the conditions - both not NA and not empty (if applicable)
        has_data <- not_na & not_empty
        # Update the rows_to_keep vector
        rows_to_keep <- rows_to_keep & has_data
      }
      # Apply the filter to keep only rows with data in all requested columns
      original_rows <- nrow(clean_df)
      clean_df <- clean_df[rows_to_keep, ]
      kept_rows <- nrow(clean_df)
      message(sprintf("Kept %d of %d rows where all requested columns have values.",
                      kept_rows, original_rows))
    } else {
      if (length(requested_cols) > 0) {
        warning("None of the requested columns were found in the dataset.")
      }
    }
  }

  # Report execution time
  end_time <- Sys.time()
  duration <- difftime(end_time, start_time, units = "secs")
  message(sprintf("\nData frame '%s' retrieved in %s.", collection, formatDuration(duration)))

  return(clean_df)
}

# ################
## Helper Functions
## ################

#' Setup MongoDB connection with suppressed messages
#' @param collection The name of the collection you want to connect to.
#' @param database The name of the database you cant to connect to.
#' @return A mongolite::mongo object representing the connection to the MongoDB collection.
#' @noRd
ConnectMongo <- function(collection, database) {
  # Validate secrets
  validate_secrets("mongo")
  config <- validate_config("mongo")

  # Get secrets using get_secret() to keep it secret, keep it safe
  connectionString <- get_secret("connectionString")

  if (is.null(database)) {
    database = config$mongo$database
  }

  options <- ssl_options(weak_cert_validation = TRUE, key = "rds-combined-ca-bundle.pem")

  # The key is to use sink() to capture and discard the messages
  temp <- tempfile()
  sink(temp)

  # Create connection without specifying collection first
  base_connection <- mongolite::mongo(
    collection = collection, # This is a system collection that always exists
    db = database,
    url = connectionString,
    verbose = FALSE,
    options = options
  )

  # Check if the collection exists
  collections_list <- getCollectionsFromConnection(base_connection)

  # Close the base connection
  base_connection$disconnect()
  sink()
  unlink(temp)

  # Validate that collection exists
  if (!collection %in% collections_list) {
    stop(sprintf("Collection '%s' does not exist in database '%s'. Available collections: %s",
                 collection, database, paste(collections_list, collapse=", ")))
  }

  # If we get here, the collection exists - create normal connection
  sink(temp)
  on.exit({
    sink()
    unlink(temp)
  })

  Mongo <- mongolite::mongo(
    collection = collection,
    db = database,
    url = connectionString,
    verbose = FALSE,
    options = options
  )

  return(Mongo)
}

#' Safely close MongoDB connection
#' @param mongo A mongolite::mongo connection object
#' @noRd
disconnectMongo <- function(mongo_conn) {
  if (!is.null(mongo_conn)) {
    # Create a temporary sink to capture all output during disconnect
    temp <- tempfile()
    sink(file = temp, type = "output")
    sink(file = temp, type = "message")
    # Try disconnect with warning suppression
    tryCatch({
      suppressWarnings({
        mongo_conn$disconnect()
      })
    }, error = function(e) {
      # Do nothing, we'll handle errors after restoring output
    }, finally = {
      # Restore output streams
      sink(type = "message")
      sink(type = "output")
      unlink(temp)
    })
  }
}

getCollectionsFromConnection <- function(mongo_connection) {
  collections <- mongo_connection$run('{"listCollections":1,"nameOnly":true}')
  return(collections$cursor$firstBatch$name)
}

#' Task Data Harmonization Function
#'
#' This function performs data cleaning and preparation tasks, including handling missing values,
#' converting date formats, and adding necessary columns. It is tailored for a specific dataset
#' structure used in psychological or medical research.
#'
#' @param df A data frame containing the data to be harmonized.
#' @param identifier A string that specifies the unique identifier for the dataset;
#' it influences how date conversions and subsetting are handled.
#' @param collection A string representing the specific collection that needs harmonization.
#'
#' @return A data frame with the harmonized data, including standardized 'visit' column entries,
#' converted interview dates, and added 'measure' column based on the task.
#'
#' @examples
#' \donttest{
#' # Create a sample dataset
#' df <- data.frame(
#'   src_subject_id = 1:3,
#'   visit = c("bl", "6m", "12m"),
#'   score = c(10, 20, 30)
#' )
#' harmonized_data <- taskHarmonization(df, 'src_subject_id', 'task1')
#' }
#'
#' @importFrom stats setNames
#' @noRd
taskHarmonization <- function(df, identifier, collection) {
  # Ensure 'visit' column exists and update it as necessary
  if (!("visit" %in% colnames(df))) {
    df$visit <- "bl"  # Add 'visit' column with all values as "bl" if it doesn't exist
  } else {
    df$visit <- ifelse(is.na(df$visit) | df$visit == "", "bl", df$visit)  # Replace empty or NA 'visit' values with "bl"
  }

  # convert dates (from string ("m/d/Y") to iso date format)
  if ("interview_date" %in% colnames(df)) {
    df$interview_date <- parse_dates_to_iso(df$interview_date, "interview_date")
  }

  return(df)
}

#' Display table of available MongoDB collections
#'
#'
#' Retrieves a list of all available collections in the configured MongoDB database.
#' @param database Optional; the name of the database to connect to. If NULL, uses the database
#'   specified in the configuration file.
#' @return A character vector containing the names of all available collections
#'   in the configured MongoDB database.
#' @export
mongo.index <- function(database = NULL) {
  # Temporarily suppress warnings
  old_warn <- options("warn")

  # Function to suppress specific warnings by pattern
  suppressSpecificWarning <- function(expr, pattern) {
    withCallingHandlers(
      expr,
      warning = function(w) {
        # Check for multiple patterns that might indicate endSessions warning
        if (grepl(pattern, w$message, fixed = TRUE) ||
            grepl("endSessions", w$message, fixed = TRUE) ||
            grepl("Feature not supported", w$message, fixed = TRUE)) {
          invokeRestart("muffleWarning")
        }
      }
    )
  }

  # Function to detect if we're connecting to DocumentDB vs MongoDB
  detectDatabaseType <- function(connectionString) {
    # DocumentDB typically uses different host patterns or connection strings
    # Check for common DocumentDB indicators
    if (grepl("docdb|documentdb", connectionString, ignore.case = TRUE)) {
      return("documentdb")
    }
    # Check for AWS DocumentDB cluster endpoints (they often contain 'docdb')
    if (grepl("amazonaws\\.com.*docdb", connectionString, ignore.case = TRUE)) {
      return("documentdb")
    }
    return("mongodb")
  }

  validate_secrets("mongo")
  config <- validate_config("mongo")

  # Get secrets using get_secret() to keep it secret, keep it safe
  connectionString <- get_secret("connectionString")

  if (is.null(database)) {
    database = config$mongo$database
  }

  options <- ssl_options(weak_cert_validation = TRUE, key = "rds-combined-ca-bundle.pem")

  # Create a temporary sink to capture MongoDB connection messages
  temp <- tempfile()
  sink(temp)
  result <- NULL

  # Detect database type to handle warnings appropriately
  db_type <- detectDatabaseType(connectionString)
  message(sprintf("Detected database type: %s", db_type))

  # Create a direct connection to the database without specifying a collection
  tryCatch({
    # Use suppressSpecificWarning to handle the endSessions warning
    suppressSpecificWarning({
      # Connect directly to the database, not a specific collection
      base_connection <- mongolite::mongo(
        collection = "system.namespaces", # This is a system collection that always exists
        db = database,
        url = connectionString,
        verbose = FALSE,
        options = options
      )

      # Get the list of collections
      collections <- base_connection$run('{"listCollections":1,"nameOnly":true}')
      result <- collections$cursor$firstBatch$name

      # Try to disconnect with warning suppression
      suppressSpecificWarning({
        base_connection$disconnect()
      }, "endSessions")

      # Force garbage collection to clean up any lingering connections
      rm(base_connection)
      invisible(gc(verbose = FALSE))
    }, "endSessions")

    # Additional suppression for any remaining warnings
    suppressWarnings({
      # This should catch any warnings that slip through
    })

    sink()
    unlink(temp)

    # Display collections in a nice format
    if (length(result) > 0) {
      message(sprintf("Available %s Collections:", toupper(database)))
      message("===================================")

      # Calculate adaptive padding based on longest collection name
      max_name_length <- max(nchar(result))
      padding_width <- max(max_name_length + 2, 15)  # At least 15 chars, or longest name + 2

      # Display collections in columns (4 per row)
      for (i in seq(1, length(result), by = 4)) {
        row_collections <- result[i:min(i + 3, length(result))]
        # Pad shorter names to align columns with adaptive width
        padded_names <- sprintf(paste0("%-", padding_width, "s"), row_collections)
        message(paste(padded_names, collapse = " "))
      }

      message("")
      message(sprintf("Total: %d collections", length(result)))
    } else {
      message("No collections found in the database.")
    }

    # Restore previous warning setting
    options(old_warn)
    invisible(result)  # Use invisible() to prevent console printing

  }, error = function(e) {
    sink()
    unlink(temp)
    # Restore previous warning setting before stopping
    options(old_warn)
    stop(paste("Error connecting to MongoDB:", e$message))
  })
}

#' Convert dates to ISO format robustly
#'
#' This function attempts to intelligently parse dates in various formats
#' and convert them to ISO format (YYYY-MM-DD).
#'
#' @param date_vector A vector of date strings to be parsed
#' @param column_name The name of the column being parsed (for error messages)
#' @return A Date vector in ISO format (YYYY-MM-DD)
#' @importFrom lubridate parse_date_time
#' @noRd
parse_dates_to_iso <- function(date_vector, column_name = "date") {
  if (is.null(date_vector) || length(date_vector) == 0) {
    return(date_vector)
  }

  # Skip if already in Date format
  if (inherits(date_vector, "Date")) {
    return(date_vector)
  }

  # If already a POSIXct or POSIXlt, convert to Date
  if (inherits(date_vector, "POSIXt")) {
    return(as.Date(date_vector))
  }

  # Convert to character if not already
  date_vector <- as.character(date_vector)

  # Remove any NA values for analysis
  non_na_dates <- date_vector[!is.na(date_vector) & date_vector != ""]
  if (length(non_na_dates) == 0) {
    # All NA or empty, just return a vector of NAs
    return(as.Date(date_vector))
  }

  # Define a set of possible date formats to try
  possible_formats <- c(
    # American formats
    "mdy", "mdY", "m/d/y", "m/d/Y", "m-d-y", "m-d-Y",
    # European/ISO formats
    "ymd", "Ymd", "y/m/d", "Y/m/d", "y-m-d", "Y-m-d",
    # Other common formats
    "dmy", "dmY", "d/m/y", "d/m/Y", "d-m-y", "d-m-Y",
    # Month name formats
    "mdy_b", "mdY_b", "b_d_y", "b_d_Y",
    "dmy_b", "dmY_b", "d_b_y", "d_b_Y",
    "ymd_b", "Ymd_b", "y_b_d", "Y_b_d"
  )

  # Try to detect the date format
  tryCatch({
    # Sample the first few non-NA dates to guess format
    sample_size <- min(100, length(non_na_dates))
    sample_dates <- non_na_dates[1:sample_size]

    # Try parsing with each format and keep track of success rate
    format_success <- numeric(length(possible_formats))
    for (i in seq_along(possible_formats)) {
      parsed_dates <- suppressWarnings(
        lubridate::parse_date_time(sample_dates, possible_formats[i], quiet = TRUE)
      )
      format_success[i] <- sum(!is.na(parsed_dates)) / length(sample_dates)
    }

    # Find the format with the highest success rate
    best_format_idx <- which.max(format_success)
    best_format <- possible_formats[best_format_idx]

    # If the best format doesn't parse at least 50% of dates, try combo of top formats
    if (format_success[best_format_idx] < 0.5) {
      # Get top 3 formats
      top_formats <- possible_formats[order(format_success, decreasing = TRUE)[1:3]]
      # Try parsing with these formats
      parsed_dates <- suppressWarnings(
        lubridate::parse_date_time(date_vector, top_formats, quiet = TRUE)
      )
    } else {
      # Parse all dates with the best format
      parsed_dates <- suppressWarnings(
        lubridate::parse_date_time(date_vector, best_format, quiet = TRUE)
      )
    }

    # Convert to Date class
    result <- as.Date(parsed_dates)

    # Basic validation: check for impossibly old dates (before 1900) or future dates
    result[result < as.Date("1900-01-01") | result > Sys.Date() + 30] <- NA

    # Log stats about parsing
    success_rate <- sum(!is.na(result)) / length(date_vector) * 100
    message(sprintf("Parsed %s: %.1f%% successful using %s format",
                    column_name, success_rate,
                    ifelse(format_success[best_format_idx] < 0.5,
                           paste(top_formats, collapse=", "), best_format)))

    return(result)

  }, error = function(e) {
    # Fallback: try base R's as.Date with common formats
    warning(sprintf("Advanced date parsing failed for %s: %s. Falling back to basic parsing.",
                    column_name, e$message))

    fallback_formats <- c("%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y", "%Y/%m/%d")
    for (fmt in fallback_formats) {
      parsed <- suppressWarnings(as.Date(date_vector, format = fmt))
      if (sum(!is.na(parsed)) / length(parsed) > 0.5) {
        message(sprintf("Basic parsing of %s succeeded with format: %s", column_name, fmt))
        return(parsed)
      }
    }

    # If all else fails, return NA
    warning(sprintf("All date parsing methods failed for %s", column_name))
    return(as.Date(rep(NA, length(date_vector))))
  })
}

#' Alias for 'mongo' (DEPRECATED)
#'
#' This function is deprecated. Please use 'mongo' instead.
#' This is a legacy alias for the 'mongo' function to maintain compatibility with older code.
#'
#' @inheritParams mongo
#' @inherit mongo return
#' @export
#' @examples
#' \dontrun{
#' # DEPRECATED - use mongo() instead
#' survey_data <- getTask("task_alias")
#' }
getTask <- function(...) {
  .Deprecated("mongo", package = "wizaRdry")
  mongo(...)
}
