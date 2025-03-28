# if (!require(mongolite)) { install.packages("mongolite") }; library(mongolite)
# if (!require(future)) { install.packages("future") }; library(future)
# if (!require(future.apply)) { install.packages("future.apply") }; library(future.apply)
# if (!require(config)) { install.packages("config") }; library(config)
# if (!require(dplyr)) { install.packages("dplyr") }; library(dplyr)

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

#' Retrieve data from MongoDB
#'
#' @param collection_name The name of the MongoDB collection
#' @param db_name The database name (optional)
#' @param identifier Field to use as identifier (optional)
#' @param chunk_size Number of records per chunk (optional)
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
#' @return A data frame containing the MongoDB data
#' @export
#' @examples
#' \dontrun{
#' # Get data from MongoDB collection
#' data <- getMongo("collection_name")
#' }
getMongo <- function(collection_name, db_name = NULL, identifier = NULL, chunk_size = NULL) {
  start_time <- Sys.time()
  Mongo <- NULL  # Initialize to NULL for cleanup in on.exit
  
  # Setup cleanup on exit
  on.exit({
    disconnectMongo(Mongo)
  })
  
  # Suppress MongoDB messages globally
  options(mongolite.quiet = TRUE)
  
  # Get configuration
#   base::source("api/ConfigEnv.R")
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
      count <- Mongo$count(sprintf('{"%s": {"$exists": true, "$ne": ""}}', key))
      if (count > 0) {
        identifier <- key
        break
      }
    }
  }
  
  if (is.null(identifier)) {
    stop("No valid identifier found in the collection.")
  }
  
  # message(sprintf("Using identifier: %s", identifier))
  
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
  #message("Retrieving data:")
  #message(sprintf("Found %d records in %s/%s", total_records, db_name, collection_name))
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
          data_chunk <- getMongoData(chunk_mongo, identifier, batch_info)
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
  
  # Collect results
  results <- lapply(future_results, future::value)
  
  # Combine results
  # message("\nCombining data chunks...")
  df <- dplyr::bind_rows(results)
  completeLoadingAnimation(pb)
  
  # Harmonize data
  message(sprintf("Harmonizing data on %s...", identifier), appendLF = FALSE)  # Prevents line feed
  clean_df <- taskHarmonization(df, identifier, collection_name)
  Sys.sleep(0.5)  # Optional: small pause for visual effect
  message(sprintf("\rHarmonizing data on %s...done.", identifier))  # Overwrites the line with 'done'
  # "\u2713"
  
  # Report execution time
  end_time <- Sys.time()
  duration <- difftime(end_time, start_time, units = "secs")
  Sys.sleep(0.5)  # Optional: small pause for visual effect
  message(sprintf("\nData frame '%s' retrieved in %s.", collection_name, formatDuration(duration-1)))# minus 1 to account for sleep
  
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
#   base::source("api/SecretsEnv.R")
  validate_secrets("mongo")
  
#   base::source("api/ConfigEnv.R")
  config <- validate_config("mongo")
  
  if (is.null(db_name)) {
    db_name = config$mongo$collection
  }
  options <- ssl_options(weak_cert_validation = TRUE, key = "rds-combined-ca-bundle.pem")
  
  # The key is to use sink() to capture and discard the messages
  temp <- tempfile()
  sink(temp)
  
  # Create connection without specifying collection first
  base_connection <- mongolite::mongo(
    collection = "system.namespaces", # This is a system collection that always exists
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
disconnectMongo <- function(mongo) {
  if (!is.null(mongo)) {
    tryCatch({
      mongo$disconnect()
    }, error = function(e) {
      warning(sprintf("Error disconnecting from MongoDB: %s", e$message))
    })
  }
}

#' Retrieve Task Data
#'
#' Retrieves data from MongoDB based on the specified batch information and query criteria. 
#' It filters out entries where the specified identifier doesn't exist or is empty.
#'
#' @param Mongo The MongoDB connection object.
#' @param identifier The document field to check for existence and non-emptiness.
#' @param batch_info List containing 'start' and 'size' defining the batch to fetch.
#' @return A data.frame with the filtered data or NULL if no valid data is found or in case of error.
#' @examples
#' # This example assumes 'Mongo' is a MongoDB connection
#' # batch_info <- list(start = 0, size = 100)
#' # df <- getMongoData(Mongo, "src_subject_id", batch_info)
#' @noRd
getMongoData <- function(Mongo, identifier, batch_info) {
  # Check for both exists AND non-empty
  query_json <- sprintf('{"%s": {"$exists": true, "$ne": ""}}', identifier)
  message(paste("Using query:", query_json))
  
  # Get initial data
  df <- Mongo$find(query = query_json, skip = batch_info$start, limit = batch_info$size)
  message(paste("Initial rows:", nrow(df)))
  
  # Only proceed with filtering if we have data
  if (!is.null(df) && nrow(df) > 0) {
    # Print sample of data before filtering
    message("Sample before filtering:")
    message(head(df[[identifier]]))
    
    # Apply both NA and empty string filtering
    df <- df[!is.na(df[[identifier]]) & df[[identifier]] != "", ]
    message(paste("Rows after complete filtering:", nrow(df)))
    
    # Print sample after filtering
    message("Sample after filtering:")
    message(head(df[[identifier]]))
  } else {
    message("No data found in initial query")
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
  
  # convert dates (from string ("m/d/Y") to date format)
  interview_date_exists <- "interview_date" %in% colnames(df)
  
  if (interview_date_exists) {
    df$interview_date <- as.Date(df$interview_date, "%m/%d/%Y")
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

# Maintain original getCollections function for backward compatibility
getCollections <- function() {
  Mongo <- NULL
  on.exit({
    disconnectMongo(Mongo)
  })
  
  # Connect to any default collection just to get connection
  # Mongo <- ConnectMongo("system.namespaces", silent_validation = TRUE)
  Mongo <- ConnectMongo("system.namespaces")
  collections <- getCollectionsFromConnection(Mongo)
  return(collections)
}


#' Alias for 'getMongo'
#'
#' This is a legacy alias for the 'getMongo' function to maintain compatibility with older code.
#'
#' @inheritParams getMongo
#' @inherit getMongo return
#' @export
#' @examples
#' \dontrun{
#' survey_data <- getTask("task_alias")
#' }
getTask <- getMongo

#' Alias for 'getMongo'
#'
#' This is a legacy alias for the 'getMongo' function to maintain compatibility with older code.
#'
#' @inheritParams getMongo
#' @inherit getMongo return
#' @export
#' @examples
#' \dontrun{
#' survey_data <- getTask("task_alias")
#' }
mongo <- getMongo
