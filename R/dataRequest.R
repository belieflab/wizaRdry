#' Generate clean data frames from cleaning scripts created in the ./clean directory
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
#' @param skip_prompt Logical. If TRUE (default), skips confirmation prompts. If FALSE,
#'   prompts for confirmation unless the user has previously chosen to remember their preference.
#' @return Prints the time taken for the data request process.
#' @export
#' @examples
#' \dontrun{
#'   clean("prl", csv=TRUE)
#'   clean("rgpts", "kamin", rdata=TRUE)
#'
#'   # Skip confirmation prompts
#'   clean("prl", csv=TRUE, skip_prompt=TRUE)
#' }
#'
#' @author Joshua Kenney <joshua.kenney@yale.edu>
clean <- function(..., csv = FALSE, rdata = FALSE, spss = FALSE, skip_prompt = TRUE) { #skip_prompt to true so preferences can be set on first run

  # Define base path
  path <- "." # Or whatever directory you're working from

  # Required Libraries Setup

  # Prepare lists for REDCap, Qualtrics, and MongoDB
  redcap_list <- tools::file_path_sans_ext(list.files("./clean/redcap"))
  qualtrics_list <- tools::file_path_sans_ext(list.files("./clean/qualtrics"))
  mongo_list <- tools::file_path_sans_ext(list.files("./clean/mongo"))

  # Get identifier from config
  config <- validate_config()
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

  # Create a mapping to store the source type for newly created scripts
  new_script_sources <- list()

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


    # Check for user preferences file
    user_prefs_file <- file.path(path, "..wizaRdry_prefs")
    user_prefs <- list(shown_tree = FALSE, auto_create = FALSE, auto_clean = FALSE)

    if (file.exists(user_prefs_file)) {
      tryCatch({
        user_prefs <- readRDS(user_prefs_file)
        # Add the auto_clean field if it doesn't exist
        if (is.null(user_prefs$auto_clean)) {
          user_prefs$auto_clean <- FALSE
        }
      }, error = function(e) {
        # If file exists but can't be read, create a new one
        user_prefs <- list(shown_tree = FALSE, auto_create = FALSE, auto_clean = FALSE)
      })
    }

    # Validate measures against predefined lists
    invalid_scripts <- Filter(function(measure) !measure %in% c(redcap_list, qualtrics_list, mongo_list), data_list)

    # If we have invalid scripts to create and need to prompt
    if (length(invalid_scripts) > 0) {
      # If skip_prompt is TRUE or user has previously set auto_clean to TRUE, bypass the prompt
      if (!skip_prompt | !user_prefs$auto_clean) {
        script_word <- ifelse(length(invalid_scripts) > 1, "scripts", "script")
        response <- readline(prompt = sprintf("Would you like to create cleaning %s for %s now? y/n ",
                                              script_word,
                                              paste(invalid_scripts, collapse = ", ")))

        while (!tolower(response) %in% c("y", "n")) {
          response <- readline(prompt = "Please enter either y or n: ")
        }

        # If user selects y, set auto_clean to TRUE and update preferences
        if (tolower(response) == "y") {
          user_prefs$auto_clean <- TRUE
          saveRDS(user_prefs, user_prefs_file)
        }

        if (tolower(response) == "n") {
          message("Cleaning script creation cancelled.")
          invokeRestart("abort")  # This exits without the "Error:" prefix
        }
      }

      # If response is "y", allow user to select api:
      api_selection <- function() {
        options <- c("mongo", "qualtrics", "redcap")

        cat("Select script type (choose only one):\n")

        for (i in 1:length(options)) {
          cat(i, ":", options[i], "\n")
        }

        # Get user choice
        choice <- as.numeric(readline("Enter number to select option: "))

        while(is.na(choice) || choice < 1 || choice > length(options)) {
          cat("Please enter a valid number between 1 and", length(options), "\n")
          choice <- as.numeric(readline("Enter number to select option: "))
        }

        # Return the selected API
        return(options[choice])
      }

      # Use the function - select API once for all scripts
      selected_api <- api_selection()

      # Process each invalid script
      for (script_name in invalid_scripts) {
        message(sprintf("\nProcessing script: %s", script_name))

        # Store the selected API for this script in our mapping
        new_script_sources[[script_name]] <<- selected_api

        clean_templates <- list(
          mongo = list(
            path = sprintf(file.path(path, "clean", "mongo", "%s.R"), script_name),  # Added .R extension
            content = paste(
              "#",
              sprintf("# clean/mongo/%s.R", script_name),
              "#",
              '# config:  database name is defined in config.yml',
              '# secrets: connectionString is defined in secrets.R',
              '# encrypt: the *.pem file must be placed in the root of this repository',
              "#",
              "# return a list of the collection(s) from MongoDB",
              "# mongo.index()",
              "#",
              "# get collection from MongoDB",
              "# IMPORTANT: both variable name and script filename must match",
              sprintf("%s <- mongo(\"%s\")", script_name, script_name),
              "",
              "# cleaning script code...",
              "",
              "# final df must be named like the R script and appended with _clean",
              sprintf("%s_clean <- %s", script_name, script_name),
              sep = "\n"
            )
          ),
          qualtrics = list(
            path = sprintf(file.path(path, "clean", "qualtrics", "%s.R"), script_name),  # Added .R extension
            content = paste(
              "#",
              sprintf("# clean/qualtrics/%s.R", script_name),
              "#",
              "# get survey from Qualtrics database",
              "# config:  surveys are defined in config.yml as key-value pairs",
              "# secrets: baseUrls and apiKeys are defined in secrets.R",
              "#",
              "# return a list of the survey(s) from Qualtrics",
              "# qualtrics.index()",
              "#",
              "# get collection from Qualtrics",
              "# IMPORTANT: both variable name and script filename must match",
              sprintf("%s <- qualtrics(\"%s\")", script_name, script_name),
              "",
              "# cleaning script code...",
              "",
              "# IMPORTANT: final df must be appended with _clean",
              sprintf("%s_clean <- %s", script_name, script_name),
              sep = "\n"
            )
          ),
          redcap = list(
            path = sprintf(file.path(path, "clean", "redcap", "%s.R"), script_name),  # Added .R extension
            content = paste(
              "#",
              sprintf("# clean/redcap/%s.R", script_name),
              "#",
              "# config:  superkey instrument is defined in config.yml",
              "# secrets: uri and token are defined in secrets.R",
              "#",
              "# return a list of the instrument_name(s) from REDCap",
              "# redcap.index()",
              "#",
              "# get the instrument_name from REDCap",
              "# IMPORTANT: both variable name and script filename must match",
              sprintf("%s <- redcap(\"%s\")", script_name, script_name),
              "",
              "# cleaning script code...",
              "",
              "# IMPORTANT: final df must be appended with _clean",
              sprintf("%s_clean <- %s", script_name, script_name),
              sep = "\n"
            )
          )
        )

        template <- clean_templates[[selected_api]]

        # Create directory if it doesn't exist
        dir_path <- dirname(template$path)
        if (!dir.exists(dir_path)) {
          dir.create(dir_path, recursive = TRUE)
        }

        if (!file.exists(template$path)) {
          writeLines(template$content, template$path)
          message(sprintf("Created file: %s", template$path))
          # open script for editing
          rstudioapi::navigateToFile(sprintf("%s",template$path))
        } else {
          message(sprintf("File already exists: %s (skipped)", template$path))
        }
      }
    }

    # After creating new scripts in validateMeasures, update the lists
    redcap_list <<- tools::file_path_sans_ext(list.files("./clean/redcap"))
    qualtrics_list <<- tools::file_path_sans_ext(list.files("./clean/qualtrics"))
    mongo_list <<- tools::file_path_sans_ext(list.files("./clean/mongo"))

    # Return the data_list invisibly instead of stopping execution
    return(invisible(data_list))
  }

  # Compile data list and validate measures
  data_list <- list(...)

  # This is so the function doesn't break if user enters a variable storing a character vector
  # or a list of strings
  # in other words it let's you do this:
  # vars_i_want <- c('demo','sps','sips_p')
  # clean(vars_i_want)
  if (length(data_list) == 1) {
    data_list = data_list[[1]]
  }

  # Validate measures and potentially create new scripts
  validateMeasures(data_list)

  # Process each measure using processData function
  for (measure in data_list) {
    # Check if this is a newly created script with a known source
    if (measure %in% names(new_script_sources)) {
      sourceCategory <- new_script_sources[[measure]]
    } else {
      # Otherwise determine the source from updated lists
      sourceCategory <- ifelse(measure %in% redcap_list, "redcap",
                               ifelse(measure %in% qualtrics_list, "qualtrics", "mongo"))
    }

    processData(measure, sourceCategory, csv, rdata, spss, identifier)
  }

  # Clean up and record processing time
  print(Sys.time() - start_time)  # Print time taken for processing

  # Flush environment

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

#' Alias for 'clean'
#'
#' This is a legacy alias for the 'clean' function to maintain compatibility with older code.
#'
#' @inheritParams clean
#' @inherit clean return
#' @export
#' @examples
#' \dontrun{
#' prl <- clean("prl")
#' }
dataRequest <- clean
