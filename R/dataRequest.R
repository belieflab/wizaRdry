#' Data Request
#'
#' This function processes requests for clean data sequentially for specified measures.
#' It makes a request to the appropriate API for the named measure or measures
#' and runs the associated data cleaning routines. It then runs a series of
#' unit tests to verify that the data quality standards are met.
#'
#' @param ... Strings, specifying the measures to process, which can be a Mongo collection, REDCap instrument, or Qualtrics survey.
#' @param csv Optional; Boolean, if TRUE creates a .csv extract in ./tmp.
#' @param rdata Optional; Boolean, if TRUE creates an .rdata extract in ./tmp.
#' @param spss Optional; Boolean, if TRUE creates a .sav extract in ./tmp.
#' @return Prints the time taken for the data request process.
#' @export
#' @examples
#' \dontrun{
#'   dataRequest("prl", csv=TRUE)
#'   dataRequest("rgpts", "kamin", rdata=TRUE)
#' }
#' 
#' @author Joshua Kenney <joshua.kenney@yale.edu>
#' 
dataRequest <- function(..., csv = FALSE, rdata = FALSE, spss = FALSE) {
  
#   base::source("api/testSuite.R")
  
  # Required Libraries Setup
#   if (!require("tidyverse")) {install.packages("tidyverse")}; library(tidyverse)
#   if (!require("dplyr")) {install.packages("dplyr")}; library(dplyr)
#   if (!require("config")) {install.packages("config")}; library(config)
  
  # Prepare lists for REDCap, Qualtrics, and MongoDB
  redcap_list <- tools::file_path_sans_ext(list.files("./clean/redcap"))
  qualtrics_list <- tools::file_path_sans_ext(list.files("./clean/qualtrics"))
  mongo_list <- tools::file_path_sans_ext(list.files("./clean/mongo"))
  
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
  
  start_time <- Sys.time()
  
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
    invalid_list <- Filter(function(measure) !measure %in% c(redcap_list, qualtrics_list, mongo_list), data_list)
    
    if (length(invalid_list) > 0) {
      stop(paste(invalid_list, collapse = ", "), " does not have a cleaning script, please create one in clean/.\n")
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
  
  # Process each measure using processData function
  for (measure in data_list) {
    sourceCategory <- ifelse(measure %in% redcap_list, "redcap", ifelse(measure %in% qualtrics_list, "qualtrics", "mongo"))
#     base::source("api/dataRequest.R")
    processData(measure, sourceCategory, csv, rdata, spss, identifier)
  }
  
  # Clean up and record processing time
  print(Sys.time() - start_time)  # Print time taken for processing
  
  # Flush environment
#   base::source("api/env/cleanup.R")
  
  return(invisible(NULL))
  
}

processData <- function(measure, source, csv, rdata, spss, identifier) {
  # Check if input is a dataframe
  if (is.data.frame(measure)) {
    # Get the name of the dataframe as a string
    measure <- deparse(substitute(measure))
  }
  
  # Ensure data_list is a character vector (in case it's a single string)
  if (!is.character(measure)) {
    measure <- as.character(measure)
  }
  # Construct the path to the measure's cleaning script
  file_path <- sprintf("./clean/%s/%s.R", source, measure)
  message("\nProcessing ", measure, " from ./clean/", source, "/", measure, ".R")
  
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
    # Ensure testSuite is sourced and then called
#     base::source("api/testSuite.R")
    # Call testSuite with identifier
    testSuite(measure, source, file_path, identifier)
    
    df_name <- paste0(measure, "_clean")  # Construct the name of the cleaned data frame
    
    # Assuming createExtract is a function to create data extracts
    createExtract(base::get(df_name), df_name, csv, rdata, spss)  # Create data extracts
  }, error = function(e) {
    # Check if identifier is valid (you can modify this logic based on your criteria)
    if (length(identifier) == 0 || all(is.na(identifier))) {
      message("An error occurred: ", e$message)  # General error message
    } else {
      message("Error with ./clean/", source, "/", measure, ".R: ", e$message)  # Specific error message
    }
    NULL  # Return NULL on error
  })
  
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
