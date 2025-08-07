#' Generate validated NDA submission templates created in the ./nda directory
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
#' @param skip_prompt Logical. If TRUE (default), skips confirmation prompts unless preferences aren't set yet. If FALSE,
#'   prompts for confirmation unless the user has previously chosen to remember their preference.
#' @return Prints the time taken for the data request process.
#' @export
#' @examples
#' \dontrun{
#'   nda("prl", csv=TRUE)
#'   nda("rgpts", "kamin", rdata=TRUE)
#'
#'   # Skip confirmation prompts
#'   nda("prl", csv=TRUE, skip_prompt=TRUE)
#' }
#'
#' @author Joshua Kenney <joshua.kenney@yale.edu>
nda <- function(..., csv = FALSE, rdata = FALSE, spss = FALSE, limited_dataset = FALSE, skip_prompt = TRUE) {
  start_time <- Sys.time()

  # Define base path
  path <- "."


  # Required Libraries Setup

  # Prepare lists for REDCap, Qualtrics, and tasks
  csv_list <- tools::file_path_sans_ext(list.files("./nda/csv"))
  redcap_list <- tools::file_path_sans_ext(list.files("./nda/redcap"))
  qualtrics_list <- tools::file_path_sans_ext(list.files("./nda/qualtrics"))
  mongo_list <- tools::file_path_sans_ext(list.files("./nda/mongo"))
  oracle_list <- tools::file_path_sans_ext(list.files("./nda/oracle"))
  sql_list <- tools::file_path_sans_ext(list.files("./nda/sql"))

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

  # Source necessary R scripts from the 'api' directory

  # Create a mapping to store the API type for newly created scripts
  new_script_apis <- list()

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

    user_prefs_file <- file.path(path, "..wizaRdry_prefs")
    user_prefs <- list(shown_tree = FALSE, auto_create = FALSE, auto_clean = FALSE, auto_nda = FALSE)

    if (file.exists(user_prefs_file)) {
      tryCatch({
        user_prefs <- readRDS(user_prefs_file)
        # Add the auto_nda field if it doesn't exist
        if (is.null(user_prefs$auto_nda)) {
          user_prefs$auto_nda <- FALSE
        }
      }, error = function(e) {
        # If file exists but can't be read, create a new one
        user_prefs <- list(shown_tree = FALSE, auto_create = FALSE, auto_clean = FALSE, auto_nda = FALSE)
      })
    }

    # Create a list name_changes to store updated script_names
    name_changes <- list()

    # Create a character vector for failed validation tracking
    failed_validations <- character(0)

    # Your existing code to determine structures to create
    # For example:
    invalid_structures <- Filter(function(measure) !measure %in% c(csv_list, redcap_list, qualtrics_list, mongo_list, oracle_list, sql_list), data_list)

    # If we have structures to create and need to prompt
    if (length(invalid_structures) > 0) {
      # If skip_prompt is TRUE or user has previously set auto_nda to TRUE, bypass the prompt
      if (!skip_prompt | !user_prefs$auto_nda) {

        template_word <- ifelse(length(invalid_structures) > 1, "templates", "template")
        response <- readline(prompt = sprintf("Would you like to create NDA submission %s for %s now? y/n ",
                                              template_word,
                                              paste(invalid_structures, collapse = ", ")))


        while (!tolower(response) %in% c("y", "n")) {
          response <- readline(prompt = "Please enter either y or n: ")
        }

        # If response is 'y', update auto_nda to TRUE in user preferences
        if (tolower(response) == "y") {
          user_prefs$auto_nda <- TRUE
          saveRDS(user_prefs, user_prefs_file)
        }

        if (tolower(response) == "n") {
          message("NDA submission template script creation cancelled.")
          invokeRestart("abort")  # This exits without the "Error:" prefix
        }
      }

      # Process each invalid script
      for (script_name in invalid_structures) {
        message(sprintf("Processing script: %s\n", script_name))

        original_name <- script_name  # Store the original name for tracking changes

        # Improved validation function for NDA data structure names
        validate_script_name <- function(script_name, nda_base_url = "https://nda.nih.gov/api/datadictionary/v2") {

          # First, check if it's a valid structure name directly
          url <- sprintf("%s/datastructure/%s", nda_base_url, script_name)

          # Add proper error handling for the API request
          response <- tryCatch({
            httr::GET(url, httr::timeout(10))
          }, error = function(e) {
            message("Network error when connecting to NDA API: ", e$message)
            message("Check your internet connection or try again later.")
            return(NULL)
          })

          # If we got a response, check if it's valid
          if (!is.null(response)) {
            status_code <- httr::status_code(response)

            if (status_code == 200) {
              # Try to parse the JSON with handling for errors and empty string responses
              content <- tryCatch({
                raw_content <- rawToChar(response$content) # store raw_content
                if (nchar(raw_content) > 0) { # if there is content
                  jsonlite::fromJSON(raw_content)
                } else { # response is valid but content is empty
                  NULL # set content to NULL and go to similarity search
                }
              }, error = function(e) { # throw an error if something is wrong
                message("Error parsing API response: ", e$message)
                return(NULL)
              })

              if (!is.null(content) && "dataElements" %in% names(content)) {
                message(sprintf("Successfully validated '%s' - found in NDA data dictionary", script_name))
                return(TRUE)
              } else if (!is.null(content)) {
                message(sprintf("API returned content but no dataElements for '%s'", script_name))
              }
            } else if (status_code == 404) {
              message(sprintf("Data structure '%s' not found in NDA data dictionary", script_name))
            } else {
              message(sprintf("API returned status code %d for '%s'", status_code, script_name))
            }
          } else {
            message("Couldn't connect to NDA API")
            return(FALSE)
          }

          # If we get here, the direct lookup failed, so search for similar structures
          search_url <- sprintf("%s/datastructure", nda_base_url) # API endpoint to search and get all NDA data structures

          search_response <- tryCatch({
            httr::GET(search_url, httr::timeout(10)) # Get and store all NDA data structures
          }, error = function(e) {
            message("Network error when searching NDA API: ", e$message)
            return(NULL)
          })

          if (!is.null(search_response) && httr::status_code(search_response) == 200) {
            # Try to parse the JSON with error handling
            all_structures <- tryCatch({
              raw_content <- rawToChar(search_response$content)
              if (nchar(raw_content) > 0) {
                content <- jsonlite::fromJSON(raw_content)
                if (is.data.frame(content)) {
                  content
                } else {
                  message("Unexpected API response format: not a data frame")
                  return(data.frame())
                }
              } else {
                data.frame() # Pass in an empty dataframe if api response is empty
              }
            }, error = function(e) { # Throw an error if something else is wrong.
              message("Error parsing API response: ", e$message)
              return(data.frame())
            })

            if (nrow(all_structures) > 0) {
              # Look for exact or similar matches
              exact_match <- all_structures[which(all_structures$shortName == script_name), ]

              if (nrow(exact_match) > 0) {
                message(sprintf("Found exact match for '%s' in NDA data dictionary", script_name))
                return(TRUE)
              } else {
                # Show closest matches
                # Safely calculate similarities
                similarities <- tryCatch({
                  sapply(all_structures$shortName, function(name) {
                    calculate_similarity(script_name, name)
                  })
                }, error = function(e) {
                  message("Error calculating similarities: ", e$message)
                  return(numeric(0))
                })

                if (length(similarities) > 0) {
                  # Get top matches but handle possible errors
                  top_matches <- tryCatch({
                    sorted_similarities <- sort(similarities, decreasing = TRUE)
                    head(sorted_similarities, 5)
                  }, error = function(e) {
                    message("Error sorting similarities: ", e$message)
                    return(numeric(0))
                  })

                  if (length(top_matches) > 0 && top_matches[1] > 0.7) {
                    message("Data structure name not found in NDA dictionary. Did you mean one of these?")
                    for (i in seq_along(top_matches)) {
                      match_name <- names(top_matches)[i]
                      match_score <- top_matches[i]
                      message(sprintf("%d. %s (%.1f%% match)", i, match_name, match_score * 100))
                    }

                    use_suggested <- readline(prompt = "Use one of these instead? (select number or press Enter to keep original): ")
                    while (use_suggested != "" && (!grepl("^[0-9]+$", use_suggested) ||
                                                   as.numeric(use_suggested) < 1 || as.numeric(use_suggested) > length(top_matches))) {
                      use_suggested <- readline(prompt = sprintf("Please enter a number between 1 and %d, or press Enter to keep original: ", length(top_matches)))
                    }

                    if (grepl("^[0-9]+$", use_suggested)) {
                      selection <- as.numeric(use_suggested)
                      if (selection >= 1 && selection <= length(top_matches)) {
                        script_name <- names(top_matches)[selection]
                        message(sprintf("Using '%s' instead", script_name))
                        return(script_name)
                      }
                    }
                  } else {
                    message(sprintf("Warning: '%s' not found in NDA data dictionary and no close matches found", script_name))
                    proceed <- readline(prompt = "Proceed anyway? (y/n): ")
                    # Check that proceed was given a valid y or n response
                    while (!tolower(proceed) %in% c("y", "n")){
                      proceed <- readline(prompt = "Please enter either y or n: ")
                    }
                    if (tolower(proceed) == "y") {
                      return(script_name)  # Return the original name
                    } else {
                      return(FALSE)  # Indicate validation failed
                    }
                  }
                } else {
                  message(sprintf("No similarity data available for '%s'", script_name))
                }
              }
            } else {
              message("No data structures returned from NDA API")
            }
          } else {
            message("Failed to search NDA data dictionary")
          }

          message(sprintf("Warning: Unable to validate '%s' against NDA data dictionary", script_name))
          proceed <- readline(prompt = "Proceed anyway? (y/n): ")
          while (!tolower(proceed) %in% c("y", "n")){
            proceed <- readline(prompt = "Please enter either y or n: ")
          }
          if (tolower(proceed) == "y") {
            return(script_name)  # Return the original name
          } else {
            return(FALSE)  # Indicate validation failed
          }
        }

        # Validate the script name
        validated_name <- validate_script_name(script_name)

        if (is.logical(validated_name) && !validated_name) { # Only runs if validation fails
          message("Script creation cancelled due to validation failure.")
          # Store current original (script) name in failed_validations
          failed_validations <- c(failed_validations, original_name)
          next  # Skip this script and continue with the next one
        } else if (is.character(validated_name)) {
          # A different name was selected
          script_name <- validated_name
        }

        # Track name changes after validated name is stored in script_name
        name_changes[[original_name]] <- script_name

        # If script passes validation, allow user to select api:
        api_selection <- function() {
          options <- c("csv", "mongo", "qualtrics", "redcap", "oracle", "sql")

          cat("\nSelect script type (choose only one):\n")

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
          cat("\n") # Improve spacing and readability
          return(options[choice])
        }

        # Use the function
        selected_api <- api_selection()

        # Store the selected API for this script in our mapping
        new_script_apis[[script_name]] <<- selected_api

        clean_templates <- list(
          mongo = list(
            path = sprintf(file.path(path, "nda", "mongo", "%s.R"), script_name),  # Added .R extension
            content = paste(
              "#",
              sprintf("# nda/mongo/%s.R", script_name),
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
              sprintf("%s <- mongo(\"%s\")", script_name, original_name),
              "",
              "# nda remediation code...",
              "",
              "# IMPORTANT: final df name must still match the NDA data structure alias",
              "",
              sep = "\n"
            )
          ),
          qualtrics = list(
            path = sprintf(file.path(path, "nda", "qualtrics", "%s.R"), script_name),  # Added .R extension
            content = paste(
              "#",
              sprintf("# nda/qualtrics/%s.R", script_name),
              "#",
              "# get survey from Qualtrics database",
              "# config:  surveys are defined in config.yml as key-value pairs",
              "# secrets: baseUrls and apiKeys are defined in secrets.R",
              "#",
              "# return a list of the survey(s) from Qualtrics",
              "# qualtrics.index()",
              "#",
              "# get survey from Qualtrics",
              "# IMPORTANT: both variable name and script filename must match",
              sprintf("%s <- qualtrics(\"%s\")", script_name, original_name),
              "",
              "# nda remediation code...",
              "",
              "# IMPORTANT: final df name must still match the NDA data structure alias",
              "",
              sep = "\n"
            )
          ),
          redcap = list(
            path = sprintf(file.path(path, "nda", "redcap", "%s.R"), script_name),  # Added .R extension
            content = paste(
              "#",
              sprintf("# nda/redcap/%s.R", script_name),
              "#",
              "# config:  superkey instrument is defined in config.yml",
              "# secrets: uri and token are defined in secrets.R",
              "#",
              "# return a list of the instrument_name(s) from REDCap",
              "# redcap.index()",
              "#",
              "# get the instrument_name from REDCap",
              "# IMPORTANT: both variable name and script filename must match the NDA data structure alias",
              sprintf("%s <- redcap(\"%s\")", script_name, original_name),
              "",
              "# nda remediation code...",
              "",
              "# IMPORTANT: final df name must still match the NDA data structure alias",
              "",
              sep = "\n"
            )
          ),
          oracle = list(
            path = sprintf(file.path(path, "nda", "oracle", "%s.R"), script_name),  # Added .R extension
            content = paste(
              "#",
              sprintf("# nda/oracle/%s.R", script_name),
              "#",
              "# config:  coming soon...",
              "# secrets: coming soon...",
              "#",
              "# return a list of the table(s) from ORACLE",
              "# oracle.index()",
              "#",
              "# get the table data from ORACLE",
              "# IMPORTANT: both variable name and script filename must match the NDA data structure alias",
              sprintf("%s <- oracle(\"%s\")", script_name, original_name),
              "",
              "# nda remediation code...",
              "",
              "# IMPORTANT: final df name must still match the NDA data structure alias",
              "",
              sep = "\n"
            )
          ),
          sql = list(
            path = sprintf(file.path(path, "nda", "sql", "%s.R"), script_name),  # Added .R extension
            content = paste(
              "#",
              sprintf("# nda/sql/%s.R", script_name),
              "#",
              "# config:  coming soon...",
              "# secrets: coming soon...",
              "#",
              "# return a list of the table(s) from SQL",
              "# sql.index()",
              "#",
              "# get the table data from SQL",
              "# IMPORTANT: both variable name and script filename must match the NDA data structure alias",
              sprintf("%s <- sql(\"%s\")", script_name, original_name),
              "",
              "# nda remediation code...",
              "",
              "# IMPORTANT: final df name must still match the NDA data structure alias",
              "",
              sep = "\n"
            )
          ),
          csv = list(
            path = sprintf(file.path(path, "nda", "csv", "%s.R"), script_name),  # Added .R extension
            content = paste(
              "#",
              sprintf("# nda/csv/%s.R", script_name),
              "#",
              "# get the data from CSV file",
              "# IMPORTANT: both variable name and script filename must match the NDA data structure alias",
              sprintf("%s <- read.csv(\"%s\".csv)", script_name, original_name),
              "",
              "# nda remediation code...",
              "",
              "# IMPORTANT: final df name must still match the NDA data structure alias",
              "",
              sep = "\n"
            )
          )
        )

        # Create directory if it doesn't exist
        template <- clean_templates[[selected_api]]
        dir_path <- dirname(template$path)
        if (!dir.exists(dir_path)) {
          dir.create(dir_path, recursive = TRUE)
        }

        # Write the file if it doesn't exist
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
    csv_list <<- tools::file_path_sans_ext(list.files("./nda/csv"))
    redcap_list <<- tools::file_path_sans_ext(list.files("./nda/redcap"))
    qualtrics_list <<- tools::file_path_sans_ext(list.files("./nda/qualtrics"))
    mongo_list <<- tools::file_path_sans_ext(list.files("./nda/mongo"))
    oracle_list <<- tools::file_path_sans_ext(list.files("./nda/oracle"))
    sql_list <<- tools::file_path_sans_ext(list.files("./nda/sql"))

    # Update data_list with validated names if necessary (only if name_changes is not empty)
    if (length(name_changes) > 0) {
      for (original_name in names(name_changes)) {
        new_name <- name_changes[[original_name]]
        if (original_name != new_name) {
          # Replace the original name with the new name in data_list
          data_list[data_list == original_name] <- new_name
        }
      }
    }

    # Remove failed validations (scripts) from data_list if they exist
    # This will make sure unvalidated scripts are not processed later on
    if (length(failed_validations) > 0) {
      data_list <- data_list[!data_list %in% failed_validations]
    }

    # Return the updated data_list and name_changes invisibly instead of stopping execution
    return(invisible(data_list))
  }

  # Compile data list and validate measures
  data_list <- list(...)

  # This is so the function doesn't break if user enters a variable storing a character vector
  # or a list of strings
  # in other words it let's you do this:
  # vars_i_want <- c('demo','sps','sips_p')
  # dataRequest(vars_i_want)
  if (length(data_list) == 1) {
    data_list = data_list[[1]]
  }

  # Validate measures and potentially create new scripts
  # Validation results contains data_list (updated if necessary)
  validation_results <- validateMeasures(data_list)

  # Update data_list with validated names in main function call (for invalid structures if neccessary)
  data_list <- validation_results # This where data_list is updated globally if necessary

  # Process each measure using processNda function
  for (measure in data_list) {
    # Check if this is a newly created script with a known API
    if (measure %in% names(new_script_apis)) {
      api <- new_script_apis[[measure]]
    } else {
      # Otherwise determine the API from updated lists
      api <- ifelse(measure %in% redcap_list, "redcap",
                    ifelse(measure %in% qualtrics_list, "qualtrics",
                           ifelse(measure %in% mongo_list, "mongo",
                                  ifelse(measure %in% oracle_list, "oracle",
                                         ifelse(measure %in% sql_list, "sql", "csv")))))
    }

    processNda(measure, api, csv, rdata, spss, identifier, start_time, limited_dataset)
  }

  # Clean up and record processing time
  # performCleanup()
  # message(Sys.time() - start_time)  # Print time taken for processing
}

processNda <- function(measure, api, csv, rdata, spss, identifier, start_time, limited_dataset = FALSE) {
  # Store the origin environment (the one that called this function)
  origin_env <- parent.frame()

  # Add message to user to signify which data structure is being processed
  # This will help when iterating through multiple data structures

  # Create dynamic border length based on the length of the data structure name (measure)
  # 24 is the length of the preceeding message "Processing NDA Structure:"
  border_length <- 27 + nchar(measure)
  border <- paste(rep("=", border_length), collapse = "")

  # Format api name for processing message
  api_formatted <- switch(api,
                          redcap = "REDCap",
                          qualtrics = "Qualtrics",
                          mongo = "MongoDB",
                          oracle = "Oracle",
                          sql = "SQL",
                          csv = "CSV",
                          api)

  # Use dynamic border and formatted api name in message
  message(sprintf("\n%s\n Processing NDA Structure: %s \n Data source: %s\n%s\n", border, measure, api_formatted, border))

  # Add debugging flag
  DEBUG <- FALSE

  if (DEBUG) message("\n[DEBUG] Starting processNda with measure: ", measure, ", api: ", api)

  # Check if input is a dataframe
  if (is.data.frame(measure)) {
    # Get the name of the dataframe as a string
    measure_name <- deparse(substitute(measure))
    if (DEBUG) message("[DEBUG] Input is a dataframe, assigned name: ", measure_name)
  } else {
    measure_name <- measure
    if (DEBUG) message("[DEBUG] Input is not a dataframe, using as name: ", measure_name)
  }

  # Ensure data_list is a character vector (in case it's a single string)
  if (!is.character(measure)) {
    measure <- as.character(measure)
    if (DEBUG) message("[DEBUG] Converted measure to character: ", measure)
  }

  # Construct the path to the measure's cleaning script
  file_path <- sprintf("./nda/%s/%s.R", api, measure)
  message("Fetching ", measure, " with ./nda/", api, "/", measure, ".R\n")

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
    if (DEBUG) message("[DEBUG] Sourcing script file: ", file_path)
    base::source(file_path)

    # Initialize environments...
    if (!exists(".wizaRdry_env")) {
      if (DEBUG) message("[DEBUG] Creating .wizaRdry_env")
      .wizaRdry_env <- new.env(parent = globalenv())
    }

    # Get the data frame
    if (DEBUG) message("[DEBUG] Attempting to get dataframe: ", measure)
    df <- base::get0(measure)

    # Store required metadata for later use
    required_field_metadata <- NULL

    if (!is.null(df) && is.data.frame(df)) {
      if (DEBUG) {
        message("[DEBUG] Found dataframe, dims: ", nrow(df), "x", ncol(df))
      }

      # ADD REQUIRED AND RECOMMENDED ELEMENTS
      enhancement_result <- addNdarSubjectElements(df, measure)
      df <- enhancement_result$df
      required_field_metadata <- enhancement_result$required_metadata
      recommended_field_metadata <- enhancement_result$recommended_metadata  # NEW

      # Update environments
      base::assign(measure, df, envir = globalenv())
      base::assign(measure, df, envir = .wizaRdry_env)

      if (DEBUG) message("[DEBUG] Enhanced dataframe with required and recommended elements")
    }

    if (api == "qualtrics") {
      if (DEBUG) message("[DEBUG] Processing as Qualtrics data")

      # Re-get the data to ensure we have the latest version
      if (DEBUG) message("[DEBUG] Re-getting dataframe from environment")
      if (exists(measure, envir = globalenv())) {
        df <- base::get(measure, envir = globalenv())
        if (DEBUG) message("[DEBUG] Got from globalenv()")
      } else if (exists(measure, envir = .wizaRdry_env)) {
        df <- base::get(measure, envir = .wizaRdry_env)
        if (DEBUG) message("[DEBUG] Got from .wizaRdry_env")
      } else if (exists(measure, envir = origin_env)) {
        df <- base::get(measure, envir = origin_env)
        if (DEBUG) message("[DEBUG] Got from origin_env")
      } else {
        if (DEBUG) message("[DEBUG] ERROR: Can't find dataframe in any environment")
        stop(paste("Object", measure, "not found in any environment"))
      }

      # Remove specified qualtrics columns
      cols_to_remove <- c("StartDate", "EndDate", "Status", "Progress", "Duration (in seconds)",
                          "Finished", "RecordedDate", "ResponseId", "DistributionChannel",
                          "UserLanguage", "candidateId", "studyId", "measure", "ATTN", "ATTN_1", "SC0",
                          "IPAddress", "RecipientLastName", "RecipientFirstName", "RecipientEmail",
                          "ExternalReference", "LocationLatitude", "LocationLongitude")

      if (DEBUG) {
        message("[DEBUG] Current columns: ", paste(names(df), collapse=", "))
        intersection <- intersect(names(df), cols_to_remove)
        message("[DEBUG] Columns to remove (", length(intersection), "): ", paste(intersection, collapse=", "))
      }

      # Remove the columns - different approach
      cols_to_keep <- setdiff(names(df), cols_to_remove)
      if (DEBUG) message("[DEBUG] Columns to keep (", length(cols_to_keep), "): ", paste(head(cols_to_keep, 10), collapse=", "), "...")

      # Create a new dataframe with only the columns to keep
      df_new <- df[, cols_to_keep, drop = FALSE]

      if (DEBUG) {
        message("[DEBUG] After removal: ", ncol(df_new), " columns remain")
        message("[DEBUG] New columns: ", paste(names(df_new), collapse=", "))
      }

      # Reassign the filtered dataframe to BOTH environments
      if (DEBUG) message("[DEBUG] Assigning filtered dataframe back to environments")

      # Update in globalenv
      base::assign(measure, df_new, envir = globalenv())
      if (DEBUG) message("[DEBUG] Assigned to globalenv()")

      # Update in origin_env
      base::assign(measure, df_new, envir = origin_env)
      if (DEBUG) message("[DEBUG] Assigned to origin_env")

      # Update in .wizaRdry_env if it exists
      if (exists(".wizaRdry_env")) {
        base::assign(measure, df_new, envir = .wizaRdry_env)
        if (DEBUG) message("[DEBUG] Assigned to .wizaRdry_env")
      }

      # Update our local df variable for continuing the function
      df <- df_new

      # Verify the changes took effect
      if (DEBUG) {
        if (exists(measure, envir = globalenv())) {
          df_check <- base::get(measure, envir = globalenv())
          message("[DEBUG] Verification - globalenv columns: ", paste(head(names(df_check), 5), collapse=", "), "...")
        }
      }

      if (DEBUG) message("[DEBUG] Calling ndaCheckQualtricsDuplicates")
      ndaCheckQualtricsDuplicates(measure, "qualtrics")

    }

    if (api == "redcap") {
      if (DEBUG) message("[DEBUG] Processing as REDCap data")

      # Re-get the data to ensure we have the latest version
      if (DEBUG) message("[DEBUG] Re-getting dataframe from environment")
      if (exists(measure, envir = globalenv())) {
        df <- base::get(measure, envir = globalenv())
        if (DEBUG) message("[DEBUG] Got from globalenv()")
      } else if (exists(measure, envir = .wizaRdry_env)) {
        df <- base::get(measure, envir = .wizaRdry_env)
        if (DEBUG) message("[DEBUG] Got from .wizaRdry_env")
      } else if (exists(measure, envir = origin_env)) {
        df <- base::get(measure, envir = origin_env)
        if (DEBUG) message("[DEBUG] Got from origin_env")
      } else {
        if (DEBUG) message("[DEBUG] ERROR: Can't find dataframe in any environment")
        stop(paste("Object", measure, "not found in any environment"))
      }


      # Define config so you can access primary key
      config <- validate_config()

      # Remove specified REDCap columns, including configured primary key
      cols_to_remove <- c(config$redcap$primary_key, "redcap_event_name")

      if (DEBUG) {
        message("[DEBUG] Current columns: ", paste(names(df), collapse=", "))
        intersection <- intersect(names(df), cols_to_remove)
        message("[DEBUG] Columns to remove (", length(intersection), "): ", paste(intersection, collapse=", "))
      }

      # Remove the columns - different approach
      cols_to_keep <- setdiff(names(df), cols_to_remove)
      if (DEBUG) message("[DEBUG] Columns to keep (", length(cols_to_keep), "): ", paste(head(cols_to_keep, 10), collapse=", "), "...")

      # Create a new dataframe with only the columns to keep
      df_new <- df[, cols_to_keep, drop = FALSE]

      if (DEBUG) {
        message("[DEBUG] After removal: ", ncol(df_new), " columns remain")
        message("[DEBUG] New columns: ", paste(names(df_new), collapse=", "))
      }

      # Reassign the filtered dataframe to BOTH environments
      if (DEBUG) message("[DEBUG] Assigning filtered dataframe back to environments")

      # Update in globalenv
      base::assign(measure, df_new, envir = globalenv())
      if (DEBUG) message("[DEBUG] Assigned to globalenv()")

      # Update in origin_env
      base::assign(measure, df_new, envir = origin_env)
      if (DEBUG) message("[DEBUG] Assigned to origin_env")

      # Update in .wizaRdry_env if it exists
      if (exists(".wizaRdry_env")) {
        base::assign(measure, df_new, envir = .wizaRdry_env)
        if (DEBUG) message("[DEBUG] Assigned to .wizaRdry_env")
      }

      # Update our local df variable for continuing the function
      df <- df_new

      # Verify the changes took effect
      if (DEBUG) {
        if (exists(measure, envir = globalenv())) {
          df_check <- base::get(measure, envir = globalenv())
          message("[DEBUG] Verification - globalenv columns: ", paste(head(names(df_check), 5), collapse=", "), "...")
        }
      }
    }

    if (api == "mongo") {
      if (DEBUG) message("[DEBUG] Processing as Mongo data")

      # Re-get the data to ensure we have the latest version
      if (DEBUG) message("[DEBUG] Re-getting dataframe from environment")
      if (exists(measure, envir = globalenv())) {
        df <- base::get(measure, envir = globalenv())
        if (DEBUG) message("[DEBUG] Got from globalenv()")
      } else if (exists(measure, envir = .wizaRdry_env)) {
        df <- base::get(measure, envir = .wizaRdry_env)
        if (DEBUG) message("[DEBUG] Got from .wizaRdry_env")
      } else if (exists(measure, envir = origin_env)) {
        df <- base::get(measure, envir = origin_env)
        if (DEBUG) message("[DEBUG] Got from origin_env")
      } else {
        if (DEBUG) message("[DEBUG] ERROR: Can't find dataframe in any environment")
        stop(paste("Object", measure, "not found in any environment"))
      }

      # Remove specified qualtrics columns
      cols_to_remove <- c("internal_node_id", "trial_type", "trial_index", "stimulus", "time_elapsed")

      if (DEBUG) {
        message("[DEBUG] Current columns: ", paste(names(df), collapse=", "))
        intersection <- intersect(names(df), cols_to_remove)
        message("[DEBUG] Columns to remove (", length(intersection), "): ", paste(intersection, collapse=", "))
      }

      # Remove the columns - different approach
      cols_to_keep <- setdiff(names(df), cols_to_remove)
      if (DEBUG) message("[DEBUG] Columns to keep (", length(cols_to_keep), "): ", paste(head(cols_to_keep, 10), collapse=", "), "...")

      # Create a new dataframe with only the columns to keep
      df_new <- df[, cols_to_keep, drop = FALSE]

      if (DEBUG) {
        message("[DEBUG] After removal: ", ncol(df_new), " columns remain")
        message("[DEBUG] New columns: ", paste(names(df_new), collapse=", "))
      }

      # Reassign the filtered dataframe to BOTH environments
      if (DEBUG) message("[DEBUG] Assigning filtered dataframe back to environments")

      # Update in globalenv
      base::assign(measure, df_new, envir = globalenv())
      if (DEBUG) message("[DEBUG] Assigned to globalenv()")

      # Update in origin_env
      base::assign(measure, df_new, envir = origin_env)
      if (DEBUG) message("[DEBUG] Assigned to origin_env")

      # Update in .wizaRdry_env if it exists
      if (exists(".wizaRdry_env")) {
        base::assign(measure, df_new, envir = .wizaRdry_env)
        if (DEBUG) message("[DEBUG] Assigned to .wizaRdry_env")
      }

      # Update our local df variable for continuing the function
      df <- df_new

      # Verify the changes took effect
      if (DEBUG) {
        if (exists(measure, envir = globalenv())) {
          df_check <- base::get(measure, envir = globalenv())
          message("[DEBUG] Verification - globalenv columns: ", paste(head(names(df_check), 5), collapse=", "), "...")
        }
      }

    }

    # show missing data that needs filled
    if (DEBUG) message("[DEBUG] Checking for missing data in required fields")
    missing_data <- df[is.na(df$src_subject_id) | is.na(df$subjectkey) | is.na(df$interview_age) | is.na(df$interview_date) | is.na(df$sex), ]
    if (nrow(missing_data) > 0) {
      if (DEBUG) message("[DEBUG] Found ", nrow(missing_data), " rows with missing required data")
      View(missing_data)
    }

    # Re-integrate ndaValidator with proper environment management
    if (DEBUG) message("[DEBUG] Running validation")

    # First check if NDA structure exists
    nda_structure_exists <- TRUE
    nda_structure <- NULL

    tryCatch({
      nda_base_url <- "https://nda.nih.gov/api/datadictionary/v2"
      url <- sprintf("%s/datastructure/%s", nda_base_url, measure)

      response <- httr::GET(url, httr::timeout(10))

      if (httr::status_code(response) == 200) {
        raw_content <- rawToChar(response$content)
        if (nchar(raw_content) > 0) {
          nda_structure <- jsonlite::fromJSON(raw_content)
          if (!"dataElements" %in% names(nda_structure)) {
            nda_structure_exists <- FALSE
          }
        } else {
          nda_structure_exists <- FALSE
        }
      } else if (httr::status_code(response) == 404) {
        nda_structure_exists <- FALSE
        message(sprintf("\nNDA structure '%s' not found in data dictionary - treating as new structure", measure))
      } else {
        nda_structure_exists <- FALSE
        message(sprintf("Could not validate NDA structure '%s' (status: %d) - treating as new structure",
                        measure, httr::status_code(response)))
      }
    }, error = function(e) {
      nda_structure_exists <- FALSE
      message(sprintf("Network error checking NDA structure '%s' - treating as new structure: %s",
                      measure, e$message))
    })

    if (nda_structure_exists && !is.null(nda_structure)) {

      # *** ADD THIS NEW CODE HERE ***
      # Enhance existing structure with ndar_subject01 metadata BEFORE validation
      if (!is.null(required_field_metadata) || !is.null(recommended_field_metadata)) {
        message("Enhancing existing NDA structure with ndar_subject01 metadata...")
        nda_structure <- mergeNdarSubjectIntoExisting(nda_structure, required_field_metadata, recommended_field_metadata)
      }
      # *** END NEW CODE ***

      # EXISTING NDA STRUCTURE - Run full validation
      validation_results <- ndaValidator(measure, api, limited_dataset, modified_structure = nda_structure)

      # ADD BOTH METADATA ATTRIBUTES
      if (!is.null(required_field_metadata)) {
        attr(validation_results, "required_metadata") <- required_field_metadata
      }
      if (!is.null(recommended_field_metadata)) {
        attr(validation_results, "recommended_metadata") <- recommended_field_metadata
      }

      # CRITICAL: Update all environments with validated dataframe
      if (is.list(validation_results) && !is.null(validation_results$df)) {
        if (DEBUG) message("[DEBUG] Updating environments with validated dataframe")
        # Update globalenv
        base::assign(measure, validation_results$df, envir = globalenv())
        if (DEBUG) message("[DEBUG] Updated globalenv()")
        # Update origin_env
        base::assign(measure, validation_results$df, envir = origin_env)
        if (DEBUG) message("[DEBUG] Updated origin_env")
        # Update wizaRdry_env
        if (exists(".wizaRdry_env")) {
          base::assign(measure, validation_results$df, envir = .wizaRdry_env)
          if (DEBUG) message("[DEBUG] Updated .wizaRdry_env")
        }
        # Update our local df variable
        df <- validation_results$df
        if (DEBUG) message("[DEBUG] Updated local df variable")
      }

      # Play validation sound
      if (exists("validation_results") && is.list(validation_results)) {
        if (DEBUG) message("[DEBUG] Playing validation sound")
        ifelse(validation_results$valid, "mario", "wilhelm") |> beepr::beep()
      }

      # Create data upload template ONLY for existing structures
      if (DEBUG) message("[DEBUG] Creating NDA template")
      createNdaSubmissionTemplate(measure)

    } else {
      # NEW STRUCTURE - Bypass validation, create minimal results
      message(sprintf("\nSkipping NDA validation for new structure '%s'", measure))
      message("Proceeding with data definition creation using computed metadata only")

      # Create minimal validation results for compatibility
      validation_results <- list(
        valid = TRUE,  # Always valid for new structures
        df = base::get0(measure),
        message = paste("New data structure:", measure, "- no NDA validation performed"),
        bypassed_validation = TRUE
      )

      # Create a mock NDA structure for data definition creation
      df <- base::get0(measure)
      if (!is.null(df) && is.data.frame(df)) {
        # Create minimal structure that will force all fields to be computed
        nda_structure <- list(
          shortName = measure,
          dataElements = data.frame(
            name = character(0),
            type = character(0),
            description = character(0),
            required = character(0),
            stringsAsFactors = FALSE
          )
        )
        # Store as attribute for data definition creation
        attr(validation_results, "nda_structure") <- nda_structure
      }

      if (!is.null(required_field_metadata)) {
        # Enhance with both required and recommended metadata
        enhanced_structure <- mergeRequiredMetadata(nda_structure, required_field_metadata, recommended_field_metadata)
        attr(validation_results, "nda_structure") <- enhanced_structure
        message("Enhanced new structure with complete ndar_subject01 required and recommended field metadata")
      }

      # Play success sound for new structure
      if (DEBUG) message("[DEBUG] Playing new structure sound")
      beepr::beep("treasure")

      # DO NOT CREATE SUBMISSION TEMPLATE FOR NEW STRUCTURES
      message("Skipping submission template creation for new structure")
    }

    # Now apply date format preservation AFTER validation
    if (DEBUG) message("[DEBUG] Get latest dataframe version")
    df <- base::get0(measure)

    # Add limited de-identification summary
    if (limited_dataset == FALSE) {
      message("\nDataset has been de-identified using date-shifting and age-capping.")
    }

    # Create data upload template regardless of if test passes
    # if (DEBUG) message("[DEBUG] Creating NDA template")
    # createNdaSubmissionTemplate(measure)

    # Create data definition
    if (DEBUG) message("[DEBUG] Creating NDA data definition")
    tryCatch({
      df <- base::get0(measure)
      if (!is.null(df) && is.data.frame(df) &&
          exists("validation_results") && is.list(validation_results)) {

        # Get NDA structure from validation results attribute
        nda_structure <- attr(validation_results, "nda_structure")

        if (!is.null(nda_structure)) {
          submission_template <- list(columns = names(df))
          createNdaDataDefinition(submission_template, nda_structure, measure)
        } else {
          message("NDA structure not available from validation results")
        }
      }
    }, error = function(e) {
      message("Error creating data definition: ", e$message)
    })

    formatElapsedTime(start_time)
  }, error = function(e) {
    if (DEBUG) message("[DEBUG] Error caught: ", e$message)
    # Check if identifier is valid
    if (length(identifier) == 0 || all(is.na(identifier))) {
      message("An error occurred: ", e$message)  # General error message
    } else {
      message("Error with ", measure, ": ", e$message)  # Specific error message
    }
    NULL  # Return NULL on error
  })

  # Final verification
  if (DEBUG && exists(measure)) {
    final_df <- base::get0(measure)
    if (!is.null(final_df) && is.data.frame(final_df)) {
      message("[DEBUG] FINAL CHECK - Columns in ", measure, ":", paste(head(names(final_df), 10), collapse=", "), "...")
      qualtrics_cols <- intersect(names(final_df), c("StartDate", "EndDate", "Status", "Progress", "ResponseId"))
      if (length(qualtrics_cols) > 0) {
        message("[DEBUG] WARNING: Qualtrics columns still present: ", paste(qualtrics_cols, collapse=", "))
      } else {
        message("[DEBUG] SUCCESS: No Qualtrics columns found in final dataset")
      }
    }
  }

  return(result)  # Return the result of the processing
}

addNdarSubjectElements <- function(df, measure) {
  # Initialize return structure
  result <- list(
    df = df,
    required_metadata = NULL,
    recommended_metadata = NULL
  )

  tryCatch({
    message("Fetching required and recommended elements from ndar_subject01 API...")

    nda_base_url <- "https://nda.nih.gov/api/datadictionary/v2"
    url <- sprintf("%s/datastructure/ndar_subject01", nda_base_url)

    response <- httr::GET(url, httr::timeout(10))

    if (httr::status_code(response) == 200) {
      raw_content <- rawToChar(response$content)
      if (nchar(raw_content) > 0) {
        subject_structure <- jsonlite::fromJSON(raw_content)

        if ("dataElements" %in% names(subject_structure)) {
          # Get REQUIRED elements (ALL of them)
          required_elements <- subject_structure$dataElements[
            subject_structure$dataElements$required == "Required",
          ]

          # Get RECOMMENDED elements (ALL of them initially)
          all_recommended_elements <- subject_structure$dataElements[
            subject_structure$dataElements$required == "Recommended",
          ]

          # Filter RECOMMENDED to only those that exist in BOTH ndar_subject01 AND the dataframe
          common_recommended_names <- intersect(all_recommended_elements$name, names(df))
          recommended_elements <- all_recommended_elements[
            all_recommended_elements$name %in% common_recommended_names,
          ]

          message(sprintf("Found %d required elements from ndar_subject01 (will process ALL)", nrow(required_elements)))
          message(sprintf("Found %d recommended elements in ndar_subject01, %d are common with dataframe (will process COMMON only)",
                          nrow(all_recommended_elements), nrow(recommended_elements)))

          if (nrow(recommended_elements) > 0) {
            message(sprintf("Common recommended fields: %s",
                            paste(recommended_elements$name, collapse = ", ")))
          } else {
            message("No common recommended fields found between ndar_subject01 and dataframe")
          }

          # PRESERVE ALL METADATA for required and COMMON recommended only
          result$required_metadata <- required_elements
          result$recommended_metadata <- recommended_elements  # Only common ones

          # Process ALL REQUIRED elements (add missing, ensure correct types)
          message("\n--- Processing ALL REQUIRED fields ---")
          for (i in 1:nrow(required_elements)) {
            col_name <- required_elements$name[i]
            col_type <- required_elements$type[i]

            message(sprintf("Processing required field: %s (%s)", col_name, col_type))

            # Determine R data type
            if (grepl("String|GUID", col_type, ignore.case = TRUE)) {
              default_value <- NA_character_
              conversion_func <- as.character
            } else if (grepl("Integer", col_type, ignore.case = TRUE)) {
              default_value <- NA_integer_
              conversion_func <- as.integer
            } else if (grepl("Float", col_type, ignore.case = TRUE)) {
              default_value <- NA_real_
              conversion_func <- as.numeric
            } else if (grepl("Date", col_type, ignore.case = TRUE)) {
              default_value <- NA_character_
              conversion_func <- as.character
            } else {
              default_value <- NA_character_
              conversion_func <- as.character
            }

            if (col_name %in% names(df)) {
              # Column exists - preserve existing values but ensure correct type
              existing_data <- df[[col_name]]
              non_na_count <- sum(!is.na(existing_data))

              if (non_na_count > 0) {
                message(sprintf("  - Field exists with %d values, ensuring %s type",
                                non_na_count, col_type))
                df[[col_name]] <- tryCatch({
                  conversion_func(existing_data)
                }, error = function(e) {
                  message(sprintf("    Warning: Type conversion failed for %s", col_name))
                  existing_data
                })
              } else {
                message(sprintf("  - Field exists but empty, setting to %s type", col_type))
                df[[col_name]] <- rep(default_value, nrow(df))
              }
            } else {
              # Column doesn't exist - add it (REQUIRED fields are always added)
              message(sprintf("  - Adding new required field as %s type", col_type))
              df[[col_name]] <- rep(default_value, nrow(df))
            }
          }

          # Process ONLY COMMON RECOMMENDED elements
          if (nrow(recommended_elements) > 0) {
            message("\n--- Processing COMMON RECOMMENDED fields ---")
            for (i in 1:nrow(recommended_elements)) {
              col_name <- recommended_elements$name[i]
              col_type <- recommended_elements$type[i]

              message(sprintf("Processing common recommended field: %s (%s)", col_name, col_type))

              # Determine R data type (same logic as required)
              if (grepl("String|GUID", col_type, ignore.case = TRUE)) {
                default_value <- NA_character_
                conversion_func <- as.character
              } else if (grepl("Integer", col_type, ignore.case = TRUE)) {
                default_value <- NA_integer_
                conversion_func <- as.integer
              } else if (grepl("Float", col_type, ignore.case = TRUE)) {
                default_value <- NA_real_
                conversion_func <- as.numeric
              } else if (grepl("Date", col_type, ignore.case = TRUE)) {
                default_value <- NA_character_
                conversion_func <- as.character
              } else {
                default_value <- NA_character_
                conversion_func <- as.character
              }

              # Since we filtered for common fields, we know the column exists
              # Just ensure correct type and preserve existing values
              existing_data <- df[[col_name]]
              non_na_count <- sum(!is.na(existing_data))

              if (non_na_count > 0) {
                message(sprintf("  - Field exists with %d values, ensuring %s type",
                                non_na_count, col_type))
                df[[col_name]] <- tryCatch({
                  conversion_func(existing_data)
                }, error = function(e) {
                  message(sprintf("    Warning: Type conversion failed for %s", col_name))
                  existing_data
                })
              } else {
                message(sprintf("  - Field exists but empty, setting to %s type", col_type))
                df[[col_name]] <- rep(default_value, nrow(df))
              }
            }
          } else {
            message("\n--- No common recommended fields to process ---")
          }

          # Reorder columns: REQUIRED first, then COMMON RECOMMENDED, then others
          required_field_names <- required_elements$name
          recommended_field_names <- recommended_elements$name  # Only common ones
          other_field_names <- setdiff(names(df), c(required_field_names, recommended_field_names))

          present_required <- intersect(required_field_names, names(df))
          present_recommended <- intersect(recommended_field_names, names(df))

          df <- df[, c(present_required, present_recommended, other_field_names)]
          result$df <- df

          message("\nSuccessfully processed ndar_subject01 elements")
          message(sprintf("Required fields (%d): %s",
                          length(present_required), paste(present_required, collapse = ", ")))
          if (length(present_recommended) > 0) {
            message(sprintf("Common recommended fields (%d): %s",
                            length(present_recommended), paste(present_recommended, collapse = ", ")))
          } else {
            message("Common recommended fields (0): none")
          }

        } else {
          message("No dataElements found in ndar_subject01 API response")
        }
      } else {
        message("Empty response from ndar_subject01 API")
      }
    } else {
      message(sprintf("Failed to fetch ndar_subject01 from API (status: %d)", httr::status_code(response)))
    }

  }, error = function(e) {
    message("Error fetching ndar_subject01 from API: ", e$message)
    message("Continuing without required/recommended field updates...")
  })

  return(result)
}

# Function to merge required metadata into new structure definitions
mergeRequiredMetadata <- function(new_structure, required_metadata, recommended_metadata = NULL) {
  if (is.null(required_metadata) || nrow(required_metadata) == 0) {
    return(new_structure)
  }

  tryCatch({
    message("Merging required and recommended ndar_subject01 metadata into new structure...")

    # Combine required and recommended metadata
    all_metadata <- required_metadata
    if (!is.null(recommended_metadata) && nrow(recommended_metadata) > 0) {
      all_metadata <- rbind(required_metadata, recommended_metadata)
      message(sprintf("Combined %d required + %d recommended = %d total elements",
                      nrow(required_metadata), nrow(recommended_metadata), nrow(all_metadata)))
    }

    # For new structures, prepend the combined metadata with their full definitions
    if (is.null(new_structure$dataElements) || nrow(new_structure$dataElements) == 0) {
      # No existing elements, use combined metadata as base
      new_structure$dataElements <- all_metadata
      message(sprintf("Added %d elements as base structure", nrow(all_metadata)))
    } else {
      # Existing elements - merge without overwriting
      existing_names <- new_structure$dataElements$name
      combined_names <- all_metadata$name

      # Find elements not already in the structure
      missing_elements <- all_metadata[!combined_names %in% existing_names, ]

      if (nrow(missing_elements) > 0) {
        # Prepend missing elements
        new_structure$dataElements <- rbind(missing_elements, new_structure$dataElements)
        message(sprintf("Added %d missing elements", nrow(missing_elements)))
      }

      message("Required and recommended elements preserved with complete metadata")
    }

  }, error = function(e) {
    message("Error merging metadata: ", e$message)
  })

  return(new_structure)
}

# Enhanced mergeNdarSubjectIntoExisting function to properly override field definitions

mergeNdarSubjectIntoExisting <- function(existing_structure, required_metadata, recommended_metadata) {
  if (is.null(existing_structure) || is.null(existing_structure$dataElements)) {
    return(existing_structure)
  }

  tryCatch({
    message("Overriding existing structure fields with authoritative ndar_subject01 definitions...")

    # Combine required and recommended metadata from ndar_subject01
    ndar_metadata <- data.frame()
    if (!is.null(required_metadata) && nrow(required_metadata) > 0) {
      ndar_metadata <- rbind(ndar_metadata, required_metadata)
    }
    if (!is.null(recommended_metadata) && nrow(recommended_metadata) > 0) {
      ndar_metadata <- rbind(ndar_metadata, recommended_metadata)
    }

    if (nrow(ndar_metadata) == 0) {
      return(existing_structure)
    }

    # Get existing and ndar field names
    existing_names <- existing_structure$dataElements$name
    ndar_names <- ndar_metadata$name

    # Find fields that should be REPLACED with ndar_subject01 definitions
    fields_to_replace <- intersect(existing_names, ndar_names)

    # Find fields that are completely new from ndar_subject01
    new_fields <- setdiff(ndar_names, existing_names)

    # Find fields that remain unchanged from original structure
    unchanged_fields <- setdiff(existing_names, ndar_names)

    message(sprintf("Replacing %d fields with ndar_subject01 definitions: %s",
                    length(fields_to_replace), paste(head(fields_to_replace, 8), collapse = ", ")))

    if (length(new_fields) > 0) {
      message(sprintf("Adding %d new ndar_subject01 fields: %s",
                      length(new_fields), paste(head(new_fields, 5), collapse = ", ")))
    }

    message(sprintf("Keeping %d original structure fields unchanged: %s",
                    length(unchanged_fields), paste(head(unchanged_fields, 5), collapse = ", ")))

    # Build the new dataElements in priority order:
    # 1. REQUIRED fields from ndar_subject01 (first priority)
    # 2. RECOMMENDED fields from ndar_subject01 (second priority)
    # 3. Original structure fields that weren't replaced (last)

    new_data_elements <- data.frame()

    # Add REQUIRED fields from ndar_subject01 first
    if (!is.null(required_metadata) && nrow(required_metadata) > 0) {
      required_in_structure <- required_metadata[required_metadata$name %in% c(fields_to_replace, new_fields), ]
      if (nrow(required_in_structure) > 0) {
        new_data_elements <- rbind(new_data_elements, required_in_structure)
        message(sprintf("Added %d required fields from ndar_subject01", nrow(required_in_structure)))
      }
    }

    # Add RECOMMENDED fields from ndar_subject01 second
    if (!is.null(recommended_metadata) && nrow(recommended_metadata) > 0) {
      recommended_in_structure <- recommended_metadata[recommended_metadata$name %in% c(fields_to_replace, new_fields), ]
      if (nrow(recommended_in_structure) > 0) {
        new_data_elements <- rbind(new_data_elements, recommended_in_structure)
        message(sprintf("Added %d recommended fields from ndar_subject01", nrow(recommended_in_structure)))
      }
    }

    # Add unchanged original structure fields last
    unchanged_elements <- existing_structure$dataElements[
      existing_structure$dataElements$name %in% unchanged_fields,
    ]
    if (nrow(unchanged_elements) > 0) {
      new_data_elements <- rbind(new_data_elements, unchanged_elements)
      message(sprintf("Preserved %d original structure fields", nrow(unchanged_elements)))
    }

    # Replace the dataElements with the new prioritized version
    existing_structure$dataElements <- new_data_elements

    message(sprintf("Enhanced structure now has %d total fields with ndar_subject01 definitions taking priority",
                    nrow(existing_structure$dataElements)))

    # Show what the key fields now look like
    key_fields <- c("race", "phenotype", "phenotype_description", "twins_study", "sibling_study")
    present_key_fields <- intersect(key_fields, existing_structure$dataElements$name)
    if (length(present_key_fields) > 0) {
      message("Key ndar_subject01 field definitions now active:")
      for (field in present_key_fields) {
        field_def <- existing_structure$dataElements[existing_structure$dataElements$name == field, ]
        if (nrow(field_def) > 0) {
          message(sprintf("  - %s: %s (%s) - %s",
                          field, field_def$type[1], field_def$required[1],
                          substr(field_def$description[1], 1, 50)))
        }
      }
    }

  }, error = function(e) {
    message("Error merging ndar_subject01 into existing structure: ", e$message)
  })

  return(existing_structure)
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

#' Alias for 'nda'
#'
#' This is a legacy alias for the 'nda' function to maintain compatibility with older code.
#'
#' @inheritParams nda
#' @inherit nda return
#' @export
#' @examples
#' \dontrun{
#' prl01 <- ndaRequest("prl01")
#' }
ndaRequest <- nda

