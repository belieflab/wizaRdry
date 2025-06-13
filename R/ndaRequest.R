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
  redcap_list <- tools::file_path_sans_ext(list.files("./nda/redcap"))
  qualtrics_list <- tools::file_path_sans_ext(list.files("./nda/qualtrics"))
  mongo_list <- tools::file_path_sans_ext(list.files("./nda/mongo"))
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

    # Your existing code to determine structures to create
    # For example:
    invalid_structures <- Filter(function(measure) !measure %in% c(redcap_list, qualtrics_list, mongo_list, sql_list), data_list)

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
        message(sprintf("\nProcessing script: %s", script_name))

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
              # Try to parse the JSON with error handling
              content <- tryCatch({
                jsonlite::fromJSON(rawToChar(response$content))
              }, error = function(e) {
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
          search_url <- sprintf("%s/datastructure", nda_base_url)

          search_response <- tryCatch({
            httr::GET(search_url, httr::timeout(10))
          }, error = function(e) {
            message("Network error when searching NDA API: ", e$message)
            return(NULL)
          })

          if (!is.null(search_response) && httr::status_code(search_response) == 200) {
            # Try to parse the JSON with error handling
            all_structures <- tryCatch({
              content <- jsonlite::fromJSON(rawToChar(search_response$content))
              if (is.data.frame(content)) {
                content
              } else {
                message("Unexpected API response format: not a data frame")
                return(data.frame())
              }
            }, error = function(e) {
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
                    message("\nData structure name not found in NDA dictionary. Did you mean one of these?")
                    for (i in seq_along(top_matches)) {
                      match_name <- names(top_matches)[i]
                      match_score <- top_matches[i]
                      message(sprintf("%d. %s (%.1f%% match)", i, match_name, match_score * 100))
                    }

                    use_suggested <- readline(prompt = "Use one of these instead? (enter number or 'n' to keep original): ")

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
          if (tolower(proceed) == "y") {
            return(script_name)  # Return the original name
          } else {
            return(FALSE)  # Indicate validation failed
          }
        }

        # Validate the script name
        validated_name <- validate_script_name(script_name)

        if (is.logical(validated_name) && !validated_name) {
          message("Script creation cancelled due to validation failure.")
          next  # Skip this script and continue with the next one
        } else if (is.character(validated_name)) {
          # A different name was selected
          script_name <- validated_name
        }

        # If script passes validation, allow user to select api:
        api_selection <- function() {
          options <- c("mongo", "qualtrics", "redcap", "sql")

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
              sprintf("%s <- mongo(\"%s\")", script_name, script_name),
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
              "# get collection from Qualtrics",
              "# IMPORTANT: both variable name and script filename must match",
              sprintf("%s <- qualtrics(\"%s\")", script_name, script_name),
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
              sprintf("%s <- redcap(\"%s\")", script_name, script_name),
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
              "# config:  not needed yet",
              "# secrets: not needed yet",
              "#",
              "# return a list of the instrument_name(s) from SQL",
              "# sql.index()",
              "#",
              "# get the instrument_name from REDCap",
              "# IMPORTANT: both variable name and script filename must match the NDA data structure alias",
              #sprintf("%s <- sql(\"%s\")", script_name, script_name),
              sprintf("%s)", script_name),
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
    redcap_list <<- tools::file_path_sans_ext(list.files("./nda/redcap"))
    qualtrics_list <<- tools::file_path_sans_ext(list.files("./nda/qualtrics"))
    mongo_list <<- tools::file_path_sans_ext(list.files("./nda/mongo"))
    sql_list <<- tools::file_path_sans_ext(list.files("./nda/sql"))

    # Return the data_list invisibly instead of stopping execution
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
  validateMeasures(data_list)

  # Process each measure using processNda function
  for (measure in data_list) {
    # Check if this is a newly created script with a known API
    if (measure %in% names(new_script_apis)) {
      api <- new_script_apis[[measure]]
    } else {
      # Otherwise determine the API from updated lists
      api <- ifelse(measure %in% redcap_list, "redcap",
                    ifelse(measure %in% qualtrics_list, "qualtrics", "mongo"))
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
  message("\nFetching ", measure, " with ./nda/", api, "/", measure,".R\n")

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
    base::source(file_path)  # Execute the cleaning script for the measure

    # Initialize the wizaRdry environment if it doesn't exist
    if (!exists(".wizaRdry_env")) {
      if (DEBUG) message("[DEBUG] Creating .wizaRdry_env")
      .wizaRdry_env <- new.env(parent = globalenv())
    }

    # Get the data frame from global environment
    if (DEBUG) message("[DEBUG] Attempting to get dataframe: ", measure)
    df <- base::get0(measure)

    # Only process if df exists and is a data frame
    if (!is.null(df) && is.data.frame(df)) {
      if (DEBUG) {
        message("[DEBUG] Found dataframe, dims: ", nrow(df), "x", ncol(df))
        message("[DEBUG] First 10 column names: ", paste(names(df)[1:min(10, ncol(df))], collapse=", "))
      }

      # Reassign the processed data frame to both environments
      base::assign(measure, df, envir = .wizaRdry_env)
      if (DEBUG) message("[DEBUG] Assigned to .wizaRdry_env")
    } else {
      if (DEBUG) message("[DEBUG] Warning: dataframe not found or not a data.frame")
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
    validation_results <- ndaValidator(measure, api, limited_dataset)

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

    # Also re-enable the validation sound alert
    if (exists("validation_results") && is.list(validation_results)) {
      if (DEBUG) message("[DEBUG] Playing validation sound")
      ifelse(validation_results$valid, "mario", "wilhelm") |> beepr::beep()
    }

    # Now apply date format preservation AFTER validation
    if (DEBUG) message("[DEBUG] Get latest dataframe version")
    df <- base::get0(measure)

    # Add limited de-identification summary
    if (limited_dataset == FALSE) {
      message("\nDataset has been de-identified using date-shifting and age-capping.")
    }

    # Create data upload template regardless of if test passes
    if (DEBUG) message("[DEBUG] Creating NDA template")
    createNda(measure)
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

