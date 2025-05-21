
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
    workers = num_cores  # Use all cores by default
  )

  # Adjust chunk size based on available memory
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

#' Fetch data from MongoDB to be stored in a data frame
#'
#' @param collection_name The name of the MongoDB collection
#' @param ... Optional column names to filter for. Only rows with non-missing values
#'        in ALL specified columns will be returned. This is useful for filtering
#'        data to only include complete cases for specific variables of interest.
#' @param db_name The database name (optional)
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
#'
#' @return A data frame containing the MongoDB data with superkeys first
#' @export
#' @examples
#' \dontrun{
#' # Get data from MongoDB collection
#' data <- mongo("collection_name")
#' }
mongo <- function(collection_name, ..., db_name = NULL, identifier = NULL, chunk_size = NULL, verbose = FALSE, interview_date = NULL) {
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

  if (is.null(db_name)) {
    db_name <- cfg$mongo$collection
  }

  # Validate identifier
  if (is.null(identifier)) {
    identifier <- cfg$identifier
  }

  if (is.null(identifier) || any(identifier == "")) {
    stop("No identifier specified in the config file.")
  }

  # Try connecting - will now throw explicit error if collection doesn't exist
  Mongo <- ConnectMongo(collection_name, db_name)

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

  # Get total records
  query_json <- sprintf('{"%s": {"$ne": ""}}', identifier)
  total_records <- Mongo$count(query_json)

  # Get and display system resources
  mem_info <- getAvailableMemory()
  num_cores <- parallel::detectCores(logical = TRUE)
  workers <- num_cores

  # Display system info
  if (!is.null(mem_info$total)) {
    message(sprintf("System resources: %.0fGB RAM, %d-core CPU",
                    mem_info$total, num_cores))
  } else {
    message(sprintf("System resources: %d-core CPU.", num_cores))
  }

  # Calculate parameters once
  params <- calculateResourceParams(total_records, mem_info, num_cores)

  if (!is.null(mem_info$available)) {
    message(sprintf("Memory available: %.0fGB RAM", mem_info$available))
  }

  # Adjust chunk size based on memory
  if (is.null(chunk_size)) {  # Only if not manually specified
    if (!is.null(mem_info$available)) {
      if (mem_info$available < 4) {
        chunk_size <- 500
      } else if (mem_info$available < 8) {
        chunk_size <- 1000
      } else if (mem_info$available < 16) {
        chunk_size <- 2000
      } else {
        chunk_size <- 5000
      }
    } else {
      chunk_size <- 1000  # Conservative default
    }
  }

  message(sprintf("Processing: %d chunks x %d records in parallel (%d workers)",
                  params$num_chunks, params$chunk_size, params$workers))

  # Setup chunks
  num_chunks <- ceiling(total_records / chunk_size)
  chunks <- createChunks(total_records, chunk_size)

  # Setup parallel processing with quiet connections
  plan(future::multisession, workers = workers)

  # Progress message
  message(sprintf("\nImporting %s records from %s/%s into dataframe...",
                  formatC(total_records, format = "d", big.mark = ","),
                  db_name, collection_name))

  # Initialize custom progress bar
  pb <- initializeLoadingAnimation(num_chunks)

  # Process chunks
  future_results <- vector("list", length(chunks))
  for (i in seq_along(chunks)) {
    future_results[[i]] <- future({
      temp <- tempfile()
      sink(temp)
      chunk_mongo <- NULL  # Initialize connection variable

      on.exit({
        sink()
        unlink(temp)
        disconnectMongo(chunk_mongo)  # Cleanup connection in worker
      })

      tryCatch({
        chunk_mongo <- ConnectMongo(collection_name, db_name)
        batch_info <- chunks[[i]]
        if (!is.null(batch_info) && !is.null(batch_info$start) && !is.null(batch_info$size)) {
          data_chunk <- getMongoData(chunk_mongo, identifier, batch_info, verbose)
        } else {
          warning("Invalid batch info, skipping chunk")
          return(NULL)
        }
        data_chunk
      }, error = function(e) {
        warning(sprintf("Error processing chunk %d: %s", i, e$message))
        NULL
      })
    })
    updateLoadingAnimation(pb, i)
  }

  # Collect results using efficient future_lapply
  # doesn't work with sing for some reason
  # results <- future.apply::future_lapply(future_results, future::value)

  # OLD version (works with sing/ch):
  results <- lapply(future_results, future::value)

  # Combine results
  df <- dplyr::bind_rows(results)
  completeLoadingAnimation(pb)

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
  message(sprintf("Harmonizing data on %s...", identifier), appendLF = FALSE)  # Prevents line feed
  clean_df <- taskHarmonization(df, identifier, collection_name)
  message(sprintf("\rHarmonizing data on %s...done.", identifier))  # Overwrites the line with 'done'

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
  message(sprintf("\nData frame '%s' retrieved in %s.", collection_name, formatDuration(duration)))

  return(clean_df)
}

# ################ #
# Helper Functions #
# ################ #

createChunks <- function(total_records, chunk_size) {
  tryCatch({
    num_chunks <- ceiling(total_records / chunk_size)
    chunks <- vector("list", num_chunks)
    for (i in seq_len(num_chunks)) {
      chunks[[i]] <- list(
        start = (i - 1) * chunk_size,
        size = if (i == num_chunks) {
          min(chunk_size, total_records - ((i - 1) * chunk_size))
        } else {
          chunk_size
        }
      )
    }
    return(chunks)
  }, error = function(e) {
    warning("Error creating chunks, falling back to single chunk")
    return(list(list(start = 0, size = total_records)))
  })
}

#' Setup MongoDB connection with suppressed messages
#' @param collection_name The name of the collection you want to connect to.
#' @param db_name The name of the database you cant to connect to.
#' @return A mongolite::mongo object representing the connection to the MongoDB collection.
#' @noRd
ConnectMongo <- function(collection_name, db_name) {
  # Validate secrets
  validate_secrets("mongo")

  config <- validate_config("mongo")

  # Get secrets using get_secret() to keep it secret, keep it safe
  connectionString <- get_secret("connectionString")

  if (is.null(db_name)) {
    db_name = config$mongo$collection
  }
  options <- ssl_options(weak_cert_validation = TRUE, key = "rds-combined-ca-bundle.pem")

  # The key is to use sink() to capture and discard the messages
  temp <- tempfile()
  sink(temp)

  # Create connection without specifying collection first
  base_connection <- mongolite::mongo(
    collection = collection_name, # This is a system collection that always exists
    db = db_name,
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
  if (!collection_name %in% collections_list) {
    stop(sprintf("Collection '%s' does not exist in database '%s'. Available collections: %s",
                 collection_name, db_name, paste(collections_list, collapse=", ")))
  }

  # If we get here, the collection exists - create normal connection
  sink(temp)
  on.exit({
    sink()
    unlink(temp)
  })

  Mongo <- mongolite::mongo(
    collection = collection_name,
    db = db_name,
    url = connectionString,
    verbose = FALSE,
    options = options
  )

  return(Mongo)
}

#' Safely close MongoDB connection
#' @param mongo A mongolite::mongo connection object
#' @noRd
# Update the disconnectMongo function to use stronger warning suppression
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

#' Retrieve Mongo Data
#'
#' Retrieves data from MongoDB based on the specified batch information and query criteria.
#' It filters out entries where the specified identifier doesn't exist or is empty.
#'
#' @param Mongo The MongoDB connection object.
#' @param identifier The document field to check for existence and non-emptiness.
#' @param batch_info List containing 'start' and 'size' defining the batch to fetch.
#' @param verbose Logical; if TRUE, displays detailed progress messages. Default is FALSE.
#' @return A data.frame with the filtered data or NULL if no valid data is found or in case of error.
#' @examples
#' # This example assumes 'Mongo' is a MongoDB connection
#' # batch_info <- list(start = 0, size = 100)
#' # df <- getMongoData(Mongo, "src_subject_id", batch_info)
#' @noRd
getMongoData <- function(Mongo, identifier, batch_info, verbose = FALSE) {
  # Check for both exists AND non-empty
  query_json <- sprintf('{"%s": {"$exists": true, "$ne": ""}}', identifier)
  if(verbose) message(paste("Using query:", query_json))

  # Get initial data
  df <- Mongo$find(query = query_json, skip = batch_info$start, limit = batch_info$size)
  if(verbose) message(paste("Initial rows:", nrow(df)))

  # Only proceed with filtering if we have data
  if (!is.null(df) && nrow(df) > 0) {
    # Print sample of data before filtering
    if(verbose) {
      message("Sample before filtering:")
      message(head(df[[identifier]]))
    }

    # Apply both NA and empty string filtering
    df <- df[!is.na(df[[identifier]]) & df[[identifier]] != "", ]
    if(verbose) message(paste("Rows after complete filtering:", nrow(df)))

    # Print sample after filtering
    if(verbose) {
      message("Sample after filtering:")
      message(head(df[[identifier]]))
    }
  } else {
    if(verbose) message("No data found in initial query")
  }

  return(df)
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
#' @param collection_name A string representing the specific collection that needs harmonization.
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
taskHarmonization <- function(df, identifier, collection_name) {

  # Ensure 'visit' column exists and update it as necessary
  if (!("visit" %in% colnames(df))) {
    df$visit <- "bl"  # Add 'visit' column with all values as "bl" if it doesn't exist
  } else {
    df$visit <- ifelse(is.na(df$visit) | df$visit == "", "bl", df$visit)  # Replace empty or NA 'visit' values with "bl"
  }

  # capr wants as.numeric
  # if (config$mongo$collection === "capr") {
  #   df$src_subject_id <- as.numeric(df$src_subject_id)
  # }

  # convert dates (from string ("m/d/Y") to iso date format)
  if ("interview_date" %in% colnames(df)) {
    df$interview_date <- parse_dates_to_iso(df$interview_date, "interview_date")
  }

  # add measure column
  # df$measure <- collection_name

  return(df)
  # comment into add prefixes (will break code)
  #return(addPrefixToColumnss(df,collection_name))

}

getCollectionsFromConnection <- function(mongo_connection) {
  collections <- mongo_connection$run('{"listCollections":1,"nameOnly":true}')
  return(collections$cursor$firstBatch$name)
}

#' Display table of available MongoDB collections
#'
#'
#' Retrieves a list of all available collections in the configured MongoDB database.
#'
#' @param db_name Optional; the name of the database to connect to. If NULL, uses the database
#'   specified in the configuration file.
#'
#' @return A character vector containing the names of all available collections
#'   in the configured MongoDB database.
#'
#' @export
mongo.index <- function(db_name = NULL) {
  # Temporarily suppress warnings
  old_warn <- options("warn")

  # Function to suppress specific warnings by pattern
  suppressSpecificWarning <- function(expr, pattern) {
    withCallingHandlers(
      expr,
      warning = function(w) {
        if (grepl(pattern, w$message, fixed = TRUE)) {
          invokeRestart("muffleWarning")
        }
      }
    )
  }

  validate_secrets("mongo")

  config <- validate_config("mongo")

  # Get secrets using get_secret() to keep it secret, keep it safe
  connectionString <- get_secret("connectionString")

  if (is.null(db_name)) {
    db_name = config$mongo$collection
  }

  options <- ssl_options(weak_cert_validation = TRUE, key = "rds-combined-ca-bundle.pem")

  # Create a temporary sink to capture MongoDB connection messages
  temp <- tempfile()
  sink(temp)

  result <- NULL

  # Create a direct connection to the database without specifying a collection
  tryCatch({
    # Use suppressSpecificWarning to handle the endSessions warning
    suppressSpecificWarning({
      # Connect directly to the database, not a specific collection
      base_connection <- mongolite::mongo(
        collection = "system.namespaces", # This is a system collection that always exists
        db = db_name,
        url = connectionString,
        verbose = FALSE,
        options = options
      )

      # Get the list of collections
      collections <- base_connection$run('{"listCollections":1,"nameOnly":true}')
      result <- collections$cursor$firstBatch$name

      # Try to disconnect with warning suppression
      suppressWarnings(base_connection$disconnect())

      # Force garbage collection to clean up any lingering connections
      rm(base_connection)
      invisible(gc(verbose = FALSE))
    }, "endSessions")

    sink()
    unlink(temp)

    # Restore previous warning setting
    options(old_warn)

    return(result)
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

#' Alias for 'mongo'
#'
#' This is a legacy alias for the 'mongo' function to maintain compatibility with older code.
#'
#' @inheritParams mongo
#' @inherit mongo return
#' @export
#' @examples
#' \dontrun{
#' survey_data <- getTask("task_alias")
#' }
getTask <- mongo
