#' NDA Request
#'
#' This function processes requests for clean data sequentially for specified measures.
#' It makes a request to the NIH NDA API for the named data structures
#' and runs the associated data remediation routines. It then runs a series of
#' unit tests to verify that the data quality standards are met.
#'
#' @param ... Strings, specifying the measures to process, which can be a Mongo collection, REDCap instrument, or Qualtrics survey.
#' @param csv Optional; Boolean, if TRUE creates a .csv extract in ./tmp.
#' @param rdata Optional; Boolean, if TRUE creates an .rdata extract in ./tmp.
#' @param spss Optional; Boolean, if TRUE creates a .sav extract in ./tmp.
#' @param limited_dataset Optional; Boolean, if TRUE does not perform date-shifting of interview_date or age-capping of interview_age
#' @return Prints the time taken for the data request process.
#' @export
#' @examples
#' \dontrun{
#'   ndaRequest("prl", csv=TRUE)
#'   ndaRequest("rgpts", "kamin", rdata=TRUE)
#' }
#' 
#' @author Joshua Kenney <joshua.kenney@yale.edu>
#' 
ndaRequest <- function(..., csv = FALSE, rdata = FALSE, spss = FALSE, limited_dataset = FALSE) {
  
  start_time <- Sys.time()
  
#   base::source("api/getRedcap.R")
#   base::source("api/getSurvey.R")
#   base::source("api/getTask.R")
  
  # Set up cleanup for any MongoDB connections that might persist
  on.exit({
    # Find and cleanup any mongo connections in the global environment
    mongo_objects <- ls(envir = .GlobalEnv, pattern = "^Mongo|_mongo$|^mongo", all.names = TRUE)
    for (obj in mongo_objects) {
      if (exists(obj, envir = .GlobalEnv)) {
        conn <- base::get(obj, envir = .GlobalEnv)
        if (is.environment(conn) && exists("disconnect", envir = conn)) {
          tryCatch({
            conn$disconnect()
          }, error = function(e) NULL)
        }
      }
    }
    gc()  # Force garbage collection
  })
  
#   base::source("api/ndaValidator.R")
  
  # Required Libraries Setup
#   if (!require("tidyverse")) {install.packages("tidyverse")}; library(tidyverse)
#   if (!require("dplyr")) {install.packages("dplyr")}; library(dplyr)
#   if (!require("config")) {install.packages("config")}; library(config)
#   if (!require("beepr")) {install.packages("beepr")}; library(beepr)
  
  # Prepare lists for REDCap, Qualtrics, and tasks
  redcap_list <- tools::file_path_sans_ext(list.files("./nda/redcap"))
  qualtrics_list <- tools::file_path_sans_ext(list.files("./nda/qualtrics"))
  task_list <- tools::file_path_sans_ext(list.files("./nda/mongo"))
  
  # Get identifier from config
  config <- config::get()
  identifier <- config$identifier
  if (is.null(identifier) || identifier == "") {
    stop("No identifier specified in the config file.")
  }
  
  # Split identifier if it's a comma-separated string
  if (is.character(identifier)) {
    identifier <- strsplit(identifier, ",")[[1]]
  }
  
  # Source necessary R scripts from the 'api' directory
#   lapply(list.files("api/src", pattern = "\\.R$", full.names = TRUE), base::source)
#   lapply(list.files("api/fn", pattern = "\\.R$", full.names = TRUE), base::source)
  
  # Validate Measures Function
  validateMeasures <- function(data_list) {
    # Check if input is a dataframe
    if (is.data.frame(data_list)) {
      # Get the name of the dataframe as a string
      data_list <- deparse(substitute(data_list))
    }
    
    # Ensure data_list is a character vector (in case it's a single string)
    if (!is.character(data_list)) {
      data_list <- as.character(data_list)
    }
    
    # Validate measures against predefined lists
    invalid_list <- Filter(function(measure) !measure %in% c(redcap_list, qualtrics_list, task_list), data_list)
    
    if (length(invalid_list) > 0) {
      stop(paste(invalid_list, collapse = ", "), " does not have a cleaning script, please create one in nda/.\n")
    }
  }
  
  # Compile data list and validate measures
  data_list <- list(...)
  
  #this is so the function doesn't break if user enters a variable storing a character vector 
  #or a list of strings 
  #in other words it let's you do this:
  #vars_i_want <- c('demo','sps','sips_p')
  #dataRequest(vars_i_want)
  if (length(data_list) == 1){
    data_list = data_list[[1]]
  }
  validateMeasures(data_list)
  
  # Process each measure using processNda function
  for (measure in data_list) {
    api <- ifelse(measure %in% redcap_list, "redcap", ifelse(measure %in% qualtrics_list, "qualtrics", "mongo"))
    processNda(measure, api, csv, rdata, spss, identifier, start_time, limited_dataset)
  }
  
  # Clean up and record processing time
  # performCleanup()
  # print(Sys.time() - start_time)  # Print time taken for processing
}

processNda <- function(measure, api, csv, rdata, spss, identifier, start_time, limited_dataset = FALSE) {
  # Check if input is a dataframe
  if (is.data.frame(measure)) {
    # Get the name of the dataframe as a string
    measure_name <- deparse(substitute(measure))
  } else {
    measure_name <- measure
  }
  
  # Ensure data_list is a character vector (in case it's a single string)
  if (!is.character(measure)) {
    measure <- as.character(measure)
  }
  
  # Construct the path to the measure's cleaning script
  file_path <- sprintf("./nda/%s/%s.R", api, measure)
  message("\nFetching ", measure, " with nda/", api, "/", measure,".R\n")
  
  # Setup cleanup on exit
  on.exit({
    if (exists("mongo_conn") && !is.null(mongo_conn)) {
      tryCatch({
        mongo_conn$disconnect()
      }, error = function(e) {
        warning(sprintf("Error disconnecting from MongoDB: %s", e$message))
      })
    }
    # Clear the mongo connection from memory
    if (exists("mongo_conn")) {
      rm(mongo_conn)
    }
    gc()  # Force garbage collection
  })
  
  result <- tryCatch({
    base::source(file_path)  # Execute the cleaning script for the measure
    # Apply date format preservation after processing
    # Get the data frame from global environment
    df <- base::get0(measure, envir = .GlobalEnv)
    
    # Only process if df exists and is a data frame
    if (!is.null(df) && is.data.frame(df)) {
      # Reassign the processed data frame
      base::assign(measure, df, envir = .GlobalEnv)
    }
    
    if (api == "qualtrics") {
      # Remove specified qualtrics columns
      cols_to_remove <- c("StartDate", "EndDate", "Status", "Progress", "Duration (in seconds)", 
                          "Finished", "RecordedDate", "ResponseId", "DistributionChannel", 
                          "UserLanguage", "candidateId", "studyId", "measure", "ATTN", "ATTN_1", "SC0")
      df <- df[!names(df) %in% cols_to_remove]
      
      # Reassign the filtered dataframe to the global environment
      base::assign(measure, df, envir = .GlobalEnv)
      
#       source("api/test/ndaCheckQualtricsDuplicates.R")
      ndaCheckQualtricsDuplicates(measure,"qualtrics")
      
      # show missing data that needs filled
      missing_data <- df[is.na(df$src_subject_id) | is.na(df$subjectkey) | is.na(df$interview_age) | is.na(df$interview_date) | is.na(df$sex), ]
      if (nrow(missing_data) > 0) {
        View(missing_data)
      }
     
    }
    
    # Run validation
#     base::source("api/ndaValidator.R")
    validation_results <- ndaValidator(measure, api, limited_dataset)
    
    # Now apply date format preservation AFTER validation
    df <- base::get0(measure, envir = .GlobalEnv)
    # if (!is.null(df) && is.data.frame(df)) {
    #   df <- preserveDateFormat(df, limited_dataset)
    #   base::assign(measure, df, envir = .GlobalEnv)
    # }
    
    # Add limited de-identification summary
    if (limited_dataset == FALSE) {
      message("Dataset has been de-identified using date-shifting and age-capping.")
    }
    
    # audio alert of validation
    ifelse(validation_results$valid, "mario", "wilhelm") |> beepr::beep()
    
#     base::source("api/src/createNda.R")
    # Create data upload template regardless of if test passes
    createNda(measure)
    formatElapsedTime(start_time)
    
  }, error = function(e) {
    # Check if identifier is valid (you can modify this logic based on your criteria)
    if (length(identifier) == 0 || all(is.na(identifier))) {
      message("An error occurred: ", e$message)  # General error message
    } else {
      message("Error with ", measure, ": ", e$message)  # Specific error message
    }
    NULL  # Return NULL on error
  })
  
  # Flush environment
#   base::source("api/env/cleanup.R")
  
  return(result)  # Return the result of the processing
}




# Add helper function for MongoDB cleanup
disconnectMongo <- function(mongo) {
  if (!is.null(mongo)) {
    tryCatch({
      mongo$disconnect()
      rm(list = deparse(substitute(mongo)), envir = parent.frame())
    }, error = function(e) {
      warning(sprintf("Error disconnecting from MongoDB: %s", e$message))
    })
  }
}


# Cleanup Function
performCleanup <- function() {
  # Placeholder for cleanup operations, like disconnecting from databases
#   suppressWarnings(source("api/env/cleanup.R"))
}

# Helper function to preserve MM/DD/YYYY format
# preserveDateFormat <- function(df, limited_dataset = limited_dataset) {
#   if ("interview_date" %in% names(df)) {
#     # Convert to Date first to ensure consistent handling
#     dates <- as.Date(df$interview_date, format = "%m/%d/%Y")
#     
#     # Apply format based on limited_dataset flag
#     df$interview_date <- format(dates, 
#                                 ifelse(limited_dataset, "%m/%d/%Y", "%m/01/%Y"))
#     
#     # Add debug message
#     message("Applying date format with limited_dataset = ", limited_dataset)
#     message("Sample dates after formatting: ", 
#             paste(head(df$interview_date), collapse=", "))
#   }
#   return(df)
# }

# Helper function to display time savings.
formatElapsedTime <- function(start_time) {
  time_diff <- Sys.time() - start_time
  units <- attr(time_diff, "units")
  
  formatted_time <- switch(units,
                           "secs" = sprintf("%.1f seconds", time_diff),
                           "mins" = sprintf("%.1f minutes", time_diff),
                           sprintf("%.1f %s", time_diff, units)
  )
  
  message("Formatted for NDA in ", formatted_time, ".")
}
