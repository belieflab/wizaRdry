# A safer version of readline that handles errors gracefully
safe_readline <- function(prompt, default = "", allow_exit = TRUE) {
  # Force reset R's internal error state at the beginning of the function
  options(show.error.messages = FALSE)
  options(show.error.messages = TRUE)

  # Just reset warnings directly without touching .Last.warning
  suppressWarnings(warning("Dummy warning to reset warning system"))

  result <- tryCatch({
    readline(prompt)
  }, interrupt = function(i) { # Interrupt handling. This part only gets called if Ctrl + C is pressed
    if (allow_exit) {
      message("\nOperation canceled by user. Exiting...")
      invokeRestart("abort") # At this point, the abort should cause nda() to exit at the highest level
    } else {
      message("\nInterrupt received. Using default value.")
      return(default)
    }
  }, error = function(e) {
    message(sprintf("Error in readline. Using default: %s", default))
    return(default)
  }, warning = function(w) {
    message(sprintf("Warning in readline. Using default: %s", default))
    return(default)
  })

  # If empty, return default
  if (is.null(result) || result == "") {
    return(default)
  }

  return(result)
}
# STEP 1: Add these 3 functions to your code (before ndaValidator function)

# Enhanced parse_array_string function to handle your specific array formats
parse_array_string <- function(value) {
  if (is.null(value) || is.na(value)) return(NULL)

  # Handle string arrays with brackets
  if (is.character(value) && length(value) == 1) {
    # Remove brackets and unicode prefixes
    clean_str <- gsub("^\\[|\\]$", "", value)  # Remove outer brackets
    clean_str <- gsub("u'|'|\"", "", clean_str)  # Remove unicode prefixes and quotes

    # Split by comma and clean up
    values <- strsplit(clean_str, ",\\s*")[[1]]
    values <- trimws(values)

    # Remove empty values
    values <- values[values != ""]

    return(values)
  }

  # Handle numeric arrays
  if (is.numeric(value) && length(value) > 1) {
    return(sprintf("%.1f", value))
  }

  return(as.character(value))
}

# Enhanced function to detect and handle array-like fields
detect_array_fields <- function(df, elements, verbose = TRUE) {
  array_fields <- list()

  for (i in 1:nrow(elements)) {
    field_name <- elements$name[i]
    type <- elements$type[i]
    value_range <- elements$valueRange[i]
    notes <- elements$notes[i]

    # Skip if field doesn't exist
    if (!field_name %in% names(df)) next

    field_values <- df[[field_name]]

    # Check if values look like arrays
    if (is.character(field_values)) {
      # Look for bracket notation
      has_brackets <- any(grepl("^\\[.*\\]$", field_values), na.rm = TRUE)

      if (has_brackets) {
        # Parse a sample to understand the data structure
        sample_values <- field_values[!is.na(field_values) & grepl("^\\[.*\\]$", field_values)]
        if (length(sample_values) > 0) {
          parsed_sample <- parse_array_string(sample_values[1])

          array_fields[[field_name]] <- list(
            type = type,
            value_range = value_range,
            notes = notes,
            sample_parsed = parsed_sample,
            array_type = if (all(suppressWarnings(!is.na(as.numeric(parsed_sample))))) "numeric" else "text"
          )

          if (verbose) {
            cat(sprintf("\nDetected array field: %s", field_name))
            cat(sprintf("\n  Expected type: %s", type))
            cat(sprintf("\n  Array type: %s", array_fields[[field_name]]$array_type))
            cat(sprintf("\n  Sample parsed: %s", paste(parsed_sample, collapse=", ")))
            cat(sprintf("\n  Expected range: %s", value_range))
          }
        }
      }
    }
  }

  return(array_fields)
}

# Enhanced function to convert array fields to appropriate numeric codes
convert_array_fields <- function(df, elements, verbose = TRUE) {
  if (verbose) cat("\n\n--- Converting Array Fields ---\n")

  # Detect array fields
  array_fields <- detect_array_fields(df, elements, verbose = verbose)

  if (length(array_fields) == 0) {
    if (verbose) cat("No array fields detected.\n")
    return(df)
  }

  conversions_made <- list()

  for (field_name in names(array_fields)) {
    field_info <- array_fields[[field_name]]
    field_values <- df[[field_name]]

    if (verbose) {
      cat(sprintf("\n\nProcessing array field: %s", field_name))
      cat(sprintf("\n  Target type: %s", field_info$type))
      cat(sprintf("\n  Expected range: %s", field_info$value_range))
    }

    # Strategy 1: Try to extract mappings from notes
    mapping <- NULL
    if (!is.null(field_info$notes) && !is.na(field_info$notes)) {
      # Look for array notation in notes like "1=[0.9, 0.5, 0.1]" or "1=(Left, Middle, Right)"
      array_pattern <- "(\\d+)\\s*=\\s*[\\(\\[]([^\\)\\]]+)[\\)\\]]"
      matches <- gregexpr(array_pattern, field_info$notes, perl = TRUE)

      if (matches[[1]][1] != -1) {
        mapping <- list()
        all_matches <- regmatches(field_info$notes, matches)[[1]]

        for (match in all_matches) {
          parts <- regexec(array_pattern, match, perl = TRUE)
          extracted <- regmatches(match, parts)[[1]]

          if (length(extracted) >= 3) {
            code <- extracted[2]
            array_content <- extracted[3]

            # Clean up the array content and normalize case
            clean_content <- gsub("'|\"", "", array_content)
            array_values <- trimws(strsplit(clean_content, ",")[[1]])

            # Create a signature for this array (normalize to lowercase and sort)
            array_signature <- paste(sort(tolower(array_values)), collapse=",")
            mapping[[array_signature]] <- code
          }
        }

        if (verbose && length(mapping) > 0) {
          cat("\n  Found array mappings in notes:")
          for (sig in names(mapping)) {
            cat(sprintf("\n    [%s] -> %s", sig, mapping[[sig]]))
          }
        }
      }
    }

    # Strategy 2: If no mapping found, create position-based mapping
    if (is.null(mapping) || length(mapping) == 0) {
      # Get unique arrays and assign codes based on value range
      unique_arrays <- unique(field_values[!is.na(field_values)])

      if (field_info$type %in% c("Integer", "Float")) {
        # Get valid codes from value range
        valid_codes <- NULL

        if (!is.null(field_info$value_range) && !is.na(field_info$value_range)) {
          if (grepl("::", field_info$value_range)) {
            # Range notation
            range_parts <- strsplit(field_info$value_range, "::")[[1]]
            valid_codes <- seq(from = as.numeric(range_parts[1]),
                               to = as.numeric(range_parts[2]))
          } else if (grepl(";", field_info$value_range)) {
            # Discrete values
            valid_codes <- as.numeric(strsplit(field_info$value_range, ";")[[1]])
          }
        }

        # Special handling for single value ranges with multi-element arrays
        if (!is.null(valid_codes) && length(valid_codes) == 1 && length(unique_arrays) > 0) {
          # If range is just "1" but we have arrays, expand the range based on array content
          sample_array <- parse_array_string(unique_arrays[1])
          if (!is.null(sample_array) && length(sample_array) > 1) {
            # Create a range from 1 to length of array elements
            valid_codes <- seq(1, length(sample_array))
            if (verbose) {
              cat(sprintf("\n  Expanded range from %s to 1::%d based on array length",
                          field_info$value_range, length(sample_array)))
            }
          }
        }

        if (!is.null(valid_codes) && length(unique_arrays) <= length(valid_codes)) {
          mapping <- list()
          for (i in 1:length(unique_arrays)) {
            array_val <- unique_arrays[i]
            # Parse the array to create a signature
            parsed_array <- parse_array_string(array_val)
            if (!is.null(parsed_array)) {
              # Normalize case and sort for consistent signature
              array_signature <- paste(sort(tolower(parsed_array)), collapse=",")
              mapping[[array_signature]] <- as.character(valid_codes[i])
            }
          }

          if (verbose && length(mapping) > 0) {
            cat("\n  Created position-based mapping:")
            for (sig in names(mapping)) {
              cat(sprintf("\n    [%s] -> %s", sig, mapping[[sig]]))
            }
          }
        }
      }
    }

    # Apply the mapping
    if (!is.null(mapping) && length(mapping) > 0) {
      new_values <- field_values
      conversion_count <- 0

      for (i in 1:length(field_values)) {
        if (!is.na(field_values[i])) {
          parsed_array <- parse_array_string(field_values[i])
          if (!is.null(parsed_array)) {
            # Normalize case and sort for consistent signature matching
            array_signature <- paste(sort(tolower(parsed_array)), collapse=",")

            if (array_signature %in% names(mapping)) {
              new_values[i] <- mapping[[array_signature]]
              conversion_count <- conversion_count + 1
            }
          }
        }
      }

      if (conversion_count > 0) {
        # Apply type conversion
        if (field_info$type == "Integer") {
          df[[field_name]] <- as.integer(new_values)
        } else if (field_info$type == "Float") {
          df[[field_name]] <- as.numeric(new_values)
        } else {
          df[[field_name]] <- new_values
        }

        conversions_made[[field_name]] <- list(
          converted_count = conversion_count,
          mapping = mapping
        )

        if (verbose) {
          cat(sprintf("\n  SUCCESS: Converted %d array values to numeric codes", conversion_count))
        }
      } else {
        if (verbose) {
          cat("\n  FAILED: Could not convert array values")
        }
      }
    } else {
      if (verbose) {
        cat("\n  FAILED: No mapping strategy worked")
      }
    }
  }

  # Summary
  if (verbose && length(conversions_made) > 0) {
    cat("\n\nArray Conversion Summary:")
    for (field in names(conversions_made)) {
      cat(sprintf("\n- %s: %d values converted",
                  field, conversions_made[[field]]$converted_count))
    }
    cat("\n")
  }

  return(df)
}

# Function to handle missing required fields
handle_missing_fields <- function(df, elements, missing_required, verbose = FALSE) {
  if(verbose) {
    message("\nAuto-adding missing required fields with missing data codes:")
  }

  for (field in missing_required) {
    element <- elements[elements$name == field, ]

    if(verbose) {
      cat("\nNotes for", field, ":", element$notes)
    }

    # Extract missing value code from notes (e.g. -999)
    missing_code <- NULL
    if (!is.na(element$notes)) {
      # Look for pattern like "999 = Missing" or "999 = Missing/NA"
      if (grepl("\\d+\\s*=\\s*Missing(?:/NA)?", element$notes, perl = TRUE)) {
        value <- gsub(".*?(\\d+)\\s*=\\s*Missing(?:/NA)?.*", "\\1", element$notes, perl = TRUE)
        missing_code <- paste0("-", value)  # Make it negative
      }
    }

    if (!is.null(missing_code)) {
      # Convert to proper type and fill entire column
      if (element$type == "Float") {
        df[[field]] <- as.numeric(missing_code)
      } else if (element$type == "Integer") {
        df[[field]] <- as.integer(missing_code)
      } else {
        df[[field]] <- missing_code
      }

      if(verbose) {
        cat(sprintf("\n- Added %s with missing code %s as type %s",
                    field, missing_code, element$type))
      }
    }
  }

  if(verbose) cat("\n")
  return(df)
}

# Generic function for field standardization
standardize_field_names <- function(df, measure_name, verbose = FALSE) {
  if(verbose) cat("\nStandardizing common field names...")

  # Track all transformations for summary
  transformations <- list()

  # Handle index -> trial conversion
  if ("index" %in% names(df)) {
    if(verbose) cat("\n\nProcessing 'index' to 'trial' conversion...")

    # Store original state for summary
    orig_values <- head(df$index, 3)

    # Convert to numeric if not already
    df$index <- as.numeric(df$index)

    # Create trial column
    df$trial <- df$index

    # Set non-positive values to NA
    df$trial[df$index <= 0] <- NA

    # Count transformations
    total_rows <- length(df$index)
    positive_rows <- sum(df$index > 0, na.rm = TRUE)

    # Store transformation summary
    transformations[["index_to_trial"]] <- list(
      from = "index",
      to = "trial",
      total = total_rows,
      valid = positive_rows,
      sample_before = orig_values,
      sample_after = head(df$trial, 3)
    )

    if(verbose) {
      cat(sprintf("\n  Total rows: %d", total_rows))
      cat(sprintf("\n  Valid rows: %d", positive_rows))
      cat("\n  Sample values:")
      cat(sprintf("\n    Before: %s", paste(orig_values, collapse=", ")))
      cat(sprintf("\n    After:  %s", paste(head(df$trial, 3), collapse=", ")))
    }

    # Remove original column
    df$index <- NULL
  }

  # Print summary if any transformations occurred
  if(verbose && length(transformations) > 0) {
    cat("\n\nField standardization summary:")
    for(transform_name in names(transformations)) {
      transform <- transformations[[transform_name]]
      cat(sprintf("\n- %s -> %s", transform$from, transform$to))
      cat(sprintf("\n  Processed %d rows (%d valid)",
                  transform$total, transform$valid))
    }
    cat("\n")
  }

  return(df)
}

# Extract mapping rules from Notes field
# Modified get_mapping_rules function with better error handling
# Properly fixed get_mapping_rules function
# Enhanced get_mapping_rules function to better extract value meanings
# The get_mapping_rules function remains mostly the same, but with improved handling
get_mapping_rules <- function(notes) {
  if (is.null(notes) || is.na(notes) || notes == "") return(NULL)

  rules <- list()

  tryCatch({
    # Handle more elaborate mapping formats like "-999=Missing" or "999=Not Applicable"
    pattern1 <- "(-?\\d+)\\s*=\\s*([^;,]+)"
    matches1 <- gregexpr(pattern1, notes, perl = TRUE)
    if (matches1[[1]][1] != -1) {
      all_matches <- regmatches(notes, matches1)[[1]]
      for (match in all_matches) {
        parts <- regexec(pattern1, match, perl = TRUE)
        extracted <- regmatches(match, parts)[[1]]
        if (length(extracted) >= 3) {
          code <- trimws(extracted[2])
          meaning <- trimws(extracted[3])
          rules[[code]] <- meaning
        }
      }
    }

    # Handle simple mappings like "0=Never" or "0 = Never"
    if (grepl("[^=]+=[^;]+", notes)) {
      patterns <- strsplit(notes, ";\\s*")[[1]]
      for (pattern in patterns) {
        if (grepl("=", pattern)) {
          parts <- strsplit(pattern, "=")[[1]]
          if (length(parts) >= 2) {  # Check if we have both parts
            code <- trimws(parts[1])
            value <- trimws(parts[2])
            if (!(code %in% names(rules))) {  # Don't overwrite if already captured
              rules[[code]] <- value  # Code is the key (e.g., "-99" -> "N/A")
            }
          }
        }
      }
    }

    # Handle array notation like "1=(0.9, 0.5, 0.1)"
    if (grepl("=\\(.*\\)", notes)) {
      pattern_matches <- gregexpr("(\\d+)=\\(([^)]+)\\)", notes)
      if (pattern_matches[[1]][1] != -1) {
        matches <- regmatches(notes, pattern_matches)[[1]]
        for (match in matches) {
          code_match <- regexec("(\\d+)=\\(([^)]+)\\)", match)
          parts <- regmatches(match, code_match)[[1]]
          if (length(parts) >= 3) {  # Check if we have enough parts
            code <- parts[2]
            values <- sprintf("[%s]", parts[3])
            rules[[code]] <- values  # Code is the key
          }
        }
      }
    }
  }, error = function(e) {
    warning(sprintf("Error parsing mapping rules: %s\nNotes: %s", e$message, notes))
    return(list())  # Return empty list on error instead of NULL
  })

  return(rules)
}


# This is the corrected fix_na_values function that translates existing missing codes
# rather than inserting them for NA values
# This is the enhanced fix_na_values function that handles more flexible missing value categories
fix_na_values <- function(df, elements, verbose = FALSE, config_file = "./config.yml") {
  if(verbose) cat("\n\nTranslating missing data codes to NDA standards...")

  # Initialize change tracking
  changes_made <- list()

  # Load missing data codes from config if available
  custom_missing_codes <- NULL
  if (!is.null(config_file) && file.exists(config_file)) {
    tryCatch({
      config_env <- ConfigEnv$new(config_file)
      custom_missing_codes <- config_env$get_missing_data_codes()
      if(verbose && !is.null(custom_missing_codes)) {
        message("Found custom missing data codes in ", config_file)
        print(custom_missing_codes)
      }
    }, error = function(e) {
      if(verbose) {
        message("Note: Could not load missing data codes from config: ", e$message)
      }
    })
  } else if(verbose) {
    message("Note: Config file not found. No custom missing data codes to translate.")
  }

  # If no custom codes are defined, nothing to translate
  if (is.null(custom_missing_codes) || length(custom_missing_codes) == 0) {
    if(verbose) message("No custom missing data codes defined for translation.")
    return(df)
  }

  # Define standard missing value categories and their mapping to NDA codes
  standard_categories <- c("skipped", "refused", "unknown", "missing")

  # Process each column in the dataframe
  for (i in 1:nrow(elements)) {
    field_name <- elements$name[i]
    type <- elements$type[i]
    value_range <- elements$valueRange[i]
    notes <- elements$notes[i]

    # Skip if field doesn't exist in the dataframe
    if (!field_name %in% names(df)) next

    # Only process numeric fields
    if (!type %in% c("Integer", "Float")) next

    if(verbose) {
      cat(sprintf("\n\nField: %s", field_name))
      cat(sprintf("\n  Type: %s", type))
      if(!is.null(value_range) && !is.na(value_range)) {
        cat(sprintf("\n  Value Range: %s", value_range))
      }
    }

    # Extract NDA missing data codes from value range and notes
    nda_codes <- list(
      skipped = NULL,
      refused = NULL,
      unknown = NULL,
      missing = NULL  # Added 'missing' as a standard category
    )

    # Parse value range to find NDA missing data codes
    if (!is.null(value_range) && !is.na(value_range) && value_range != "") {
      range_parts <- trimws(strsplit(value_range, ";")[[1]])
      # Get rules for interpreting codes
      rules <- get_mapping_rules(notes)

      # Check each special code in the value range
      for (code in range_parts) {
        # Skip range notation
        if (grepl("::", code)) next

        code_value <- as.numeric(code)

        # If we have a rule for this code, check what it means
        if (!is.null(rules) && code %in% names(rules)) {
          meaning <- rules[[code]]

          # Determine which category this code belongs to
          if (grepl("skip|not applicable|not required|condition|exclude", meaning, ignore.case = TRUE)) {
            nda_codes$skipped <- code_value
            if(verbose) cat(sprintf("\n  NDA skip code: %d (%s)", code_value, meaning))
          }
          else if (grepl("refuse|decline|wish|777|999[^0-9]", meaning, ignore.case = TRUE)) {
            nda_codes$refused <- code_value
            if(verbose) cat(sprintf("\n  NDA refused code: %d (%s)", code_value, meaning))
          }
          else if (grepl("missing|undefined|unknown|not collect|n/?a|null|-999|-99|-9", meaning, ignore.case = TRUE)) {
            # Assign to both 'unknown' and 'missing' categories for compatibility
            nda_codes$unknown <- code_value
            nda_codes$missing <- code_value
            if(verbose) cat(sprintf("\n  NDA unknown/missing code: %d (%s)", code_value, meaning))
          }
        }
        # If no explicit meaning but looks like a special code, make an educated guess
        else if (code_value < 0 || code_value > 990) {
          # Typical missing data codes are negative or large numbers (999, 9999)
          nda_codes$unknown <- code_value
          nda_codes$missing <- code_value  # Also assign to 'missing' for compatibility
          if(verbose) cat(sprintf("\n  Guessing NDA unknown/missing code: %d (based on value)", code_value))
        }
      }
    }

    # If we couldn't find explicit meanings, try common patterns
    if ((is.null(nda_codes$unknown) || is.null(nda_codes$missing)) &&
        !is.null(value_range) && !is.na(value_range)) {
      range_parts <- trimws(strsplit(value_range, ";")[[1]])
      # Look for typical codes in value range
      for (code in range_parts) {
        code_num <- suppressWarnings(as.numeric(code))
        if (!is.na(code_num)) {
          if (code_num %in% c(-99, -999, 999, 9999) || code_num < -50) {
            nda_codes$unknown <- code_num
            nda_codes$missing <- code_num  # Also assign to 'missing' for compatibility
            if(verbose) cat(sprintf("\n  Detected likely NDA unknown/missing code: %d", code_num))
            break
          }
        }
      }
    }

    # Process the field to translate custom missing codes to NDA codes
    if (!is.null(custom_missing_codes)) {
      field_column <- df[[field_name]]
      changes <- list()

      # Process all categories in the custom_missing_codes
      for (category_name in names(custom_missing_codes)) {
        # Get corresponding NDA code based on category
        nda_code <- NULL

        # Map the category to a standard category if possible
        std_category <- category_name
        if (!category_name %in% standard_categories) {
          # Try to normalize using aliases
          if (category_name %in% c("undefined", "na", "null")) {
            std_category <- "missing"
          } else if (category_name %in% c("not_applicable", "na", "skip")) {
            std_category <- "skipped"
          } else if (category_name %in% c("declined", "no_answer")) {
            std_category <- "refused"
          }
        }

        # Get the NDA code based on the standard category
        if (std_category == "skipped" && !is.null(nda_codes$skipped)) {
          nda_code <- nda_codes$skipped
        } else if (std_category == "refused" && !is.null(nda_codes$refused)) {
          nda_code <- nda_codes$refused
        } else if (std_category == "unknown" && !is.null(nda_codes$unknown)) {
          nda_code <- nda_codes$unknown
        } else if (std_category == "missing" && !is.null(nda_codes$missing)) {
          nda_code <- nda_codes$missing
        } else if (!is.null(nda_codes$unknown)) {
          # Fallback: if category is not recognized, use unknown code
          nda_code <- nda_codes$unknown
          if(verbose) cat(sprintf("\n  Using unknown code %d for custom category '%s'",
                                  nda_codes$unknown, category_name))
        }

        # If we found an NDA code, process the values
        if (!is.null(nda_code)) {
          category_values <- custom_missing_codes[[category_name]]

          for (custom_code in category_values) {
            # Convert to numeric for comparison
            custom_code_num <- as.numeric(custom_code)

            # Skip if conversion failed (non-numeric values)
            if (is.na(custom_code_num)) {
              if(verbose) cat(sprintf("\n  Warning: Skipping non-numeric code '%s' in category '%s'",
                                      custom_code, category_name))
              next
            }

            # Find values in this column that match our custom code
            matches <- which(field_column == custom_code_num)
            if (length(matches) > 0) {
              df[[field_name]][matches] <- nda_code

              # Track changes
              if (!category_name %in% names(changes)) {
                changes[[category_name]] <- list(
                  from = c(),
                  to = nda_code,
                  count = 0
                )
              }

              changes[[category_name]]$from <- c(changes[[category_name]]$from, custom_code_num)
              changes[[category_name]]$count <- changes[[category_name]]$count + length(matches)

              if(verbose) cat(sprintf("\n  Translated %d values from %d to NDA %s code %d",
                                      length(matches), custom_code_num, std_category, nda_code))
            }
          }
        }
      }

      # If changes were made, store them
      if (length(changes) > 0) {
        changes_made[[field_name]] <- changes
      }
    }
  }

  # Report summary of changes if any were made
  if (length(changes_made) > 0 && verbose) {
    cat("\n\nMissing value code translation summary:")
    for (field in names(changes_made)) {
      cat(sprintf("\n- %s:", field))
      field_changes <- changes_made[[field]]

      for (category in names(field_changes)) {
        cat(sprintf("\n  %s: %d values from %s to %d",
                    category,
                    field_changes[[category]]$count,
                    paste(field_changes[[category]]$from, collapse=", "),
                    field_changes[[category]]$to))
      }
    }
    cat("\n")
  } else if (verbose) {
    cat("\nNo missing data codes needed translation.\n")
  }

  return(df)
}

# Improved conversion that strictly uses ValueRange from the NDA API
# Improved conversion that preserves original values and only converts actual missing data
apply_type_conversions <- function(df, elements, verbose = FALSE) {
  if(verbose) cat("\nApplying type conversions...")

  # First, convert all empty strings to NA in character columns
  for (col in names(df)) {
    if (is.character(df[[col]])) {
      empty_count <- sum(df[[col]] == "", na.rm = TRUE)
      if (empty_count > 0 && verbose) {
        cat(sprintf("\n\nField: %s", col))
        cat(sprintf("\n  Converting %d empty strings to NA", empty_count))
      }
      df[[col]][df[[col]] == ""] <- NA
    }
  }

  conversion_summary <- list()

  for (i in 1:nrow(elements)) {
    field_name <- elements$name[i]
    type <- elements$type[i]
    value_range <- elements$valueRange[i]  # This is from the NDA API
    notes <- elements$notes[i]  # This contains code meanings

    if (field_name %in% names(df) && !is.null(type)) {
      tryCatch({
        if (type %in% c("Integer", "Float")) {
          if(verbose) cat(sprintf("\n\nField: %s", field_name))
          if(verbose) cat(sprintf("\n  Target type: %s", type))

          # Store original values
          orig_values <- df[[field_name]]

          # First, identify which special code should be used for NAs
          na_code <- NULL

          # Extract all special codes from valueRange
          if (!is.null(value_range) && !is.na(value_range) && value_range != "") {
            range_parts <- trimws(strsplit(value_range, ";")[[1]])
            special_codes <- range_parts[!grepl("::", range_parts)]

            # Get meanings of codes from notes
            rules <- get_mapping_rules(notes)

            # Find the NA code based on NDA definitions
            for (code in special_codes) {
              if (code %in% names(rules)) {
                meaning <- rules[[code]]
                if (grepl("N/?A", meaning, ignore.case = TRUE)) {
                  na_code <- as.numeric(code)
                  if(verbose) cat(sprintf("\n  Using NA code %s from ValueRange", code))
                  break
                }
              }
            }

            # If no NA code found but -99 is in the value range, use that
            if (is.null(na_code) && "-99" %in% special_codes) {
              na_code <- -99
              if(verbose) cat("\n  Using default -99 code from ValueRange")
            }
          }

          # Special handling for PSSP fields
          if (grepl("^pssp", field_name) && is.null(na_code)) {
            na_code <- -99
            if(verbose) cat("\n  Using -99 code for PSSP field")
          }

          # IMPORTANT: Detect original missing values BEFORE conversion
          originally_missing <- is.na(orig_values)
          originally_missing_count <- sum(originally_missing)

          if(verbose && originally_missing_count > 0) {
            cat(sprintf("\n  Found %d originally missing values", originally_missing_count))
          }

          # Check for non-numeric values before conversion (for warning only)
          non_numeric_values <- c()
          if (!is.numeric(orig_values)) {
            # Try to identify which values won't convert cleanly
            non_numeric_test <- suppressWarnings(as.numeric(as.character(orig_values)))
            non_numeric_mask <- !is.na(orig_values) & is.na(non_numeric_test)

            if (any(non_numeric_mask)) {
              non_numeric_values <- unique(orig_values[non_numeric_mask])
              non_numeric_count <- sum(non_numeric_mask)

              if(verbose) {
                cat(sprintf("\n  WARNING: Found %d non-numeric values that will be preserved: %s",
                            non_numeric_count,
                            paste(head(non_numeric_values, 5), collapse=", ")))

                if(length(non_numeric_values) > 5) {
                  cat(sprintf(" (and %d more)", length(non_numeric_values) - 5))
                }
              }
            }
          }

          # Create a new column with converted values, preserving the original
          # Convert to character first (this keeps special values intact)
          char_values <- as.character(orig_values)

          # Apply type conversion with warnings suppressed
          if (type == "Integer") {
            converted_values <- suppressWarnings(as.integer(char_values))
          } else if (type == "Float") {
            converted_values <- suppressWarnings(as.numeric(char_values))
          }

          # Now we have a parallel converted vector with possible NAs from coercion
          # Only replace original NAs with NA codes, not NAs from conversion
          if (!is.null(na_code) && originally_missing_count > 0) {
            converted_values[originally_missing] <- na_code
            if(verbose) cat(sprintf("\n  Replaced %d originally missing values with %d",
                                    originally_missing_count, na_code))
          }

          # Now identify which values would be lost in conversion (non-numeric strings)
          conversion_mask <- rep(TRUE, length(orig_values))

          # For non-numeric originals that aren't NA, preserve the original
          if (!is.numeric(orig_values)) {
            # Find positions where:
            # 1. Original value is not NA
            # 2. Converted value is NA (conversion failed)
            # These are values we want to preserve
            preserve_mask <- !is.na(orig_values) & is.na(converted_values)

            if (any(preserve_mask)) {
              # Don't convert these positions
              conversion_mask[preserve_mask] <- FALSE

              if(verbose) {
                cat(sprintf("\n  Preserving %d non-numeric original values", sum(preserve_mask)))
              }
            }
          }

          # Apply the converted values only where conversion_mask is TRUE
          # Otherwise keep the original value
          df[[field_name]][conversion_mask] <- converted_values[conversion_mask]

          # Store conversion summary
          conversion_summary[[field_name]] <- list(
            type = type,
            originally_missing = originally_missing_count,
            preserved_count = sum(!conversion_mask),
            non_numeric_values = non_numeric_values,
            sample_before = head(orig_values),
            sample_after = head(df[[field_name]])
          )
        }
      }, error = function(e) {
        if(verbose) {
          cat(sprintf("\n\nError converting %s to %s:", field_name, type))
          cat(sprintf("\n  %s", e$message))
        }
      })
    }
  }

  # Print summary if any conversions were done
  if(verbose && length(conversion_summary) > 0) {
    cat("\n\nType conversion summary:")
    for(field in names(conversion_summary)) {
      cat(sprintf("\n- %s -> %s", field, conversion_summary[[field]]$type))

      # Report original missing values
      if(conversion_summary[[field]]$originally_missing > 0) {
        cat(sprintf(" (%d originally missing values replaced with codes)",
                    conversion_summary[[field]]$originally_missing))
      }

      # Report preserved values
      if(conversion_summary[[field]]$preserved_count > 0) {
        cat(sprintf("\n  Preserved %d original non-numeric values",
                    conversion_summary[[field]]$preserved_count))
      }

      # Show sample values
      if(verbose > 1) {  # More detailed output for higher verbosity
        cat("\n  Sample values:")
        cat(sprintf("\n    Before: %s", paste(conversion_summary[[field]]$sample_before, collapse=", ")))
        cat(sprintf("\n    After:  %s", paste(conversion_summary[[field]]$sample_after, collapse=", ")))
      }
    }
    cat("\n")
  }

  return(df)
}

# Demonstrate with standardize_dates as well
standardize_dates <- function(df, date_cols = c("interview_date"), verbose = TRUE, limited_dataset = FALSE) {
  date_summary <- list()

  for (col in date_cols) {
    if (col %in% names(df)) {
      tryCatch({
        if(verbose) cat(sprintf("\n\nField: %s", col))

        # Store original values
        orig_dates <- head(df[[col]], 3)

        # Convert to character first to ensure consistency
        dates <- as.character(df[[col]])

        # Remove timezone information
        dates <- gsub("\\s+\\d{2}:\\d{2}:\\d{2}.*$", "", dates)

        # Try different date formats with better error handling
        date_formats <- c(
          "%Y-%m-%d",    # 2023-12-31
          "%m/%d/%Y",    # 12/31/2023
          "%Y/%m/%d",    # 2023/12/31
          "%d-%m-%Y",    # 31-12-2023
          "%m-%d-%Y",    # 12-31-2023
          "%Y%m%d"       # 20231231 (no separators)
        )

        success <- FALSE
        parsed_dates <- NULL

        for (format in date_formats) {
          parsed_dates <- tryCatch({
            temp_dates <- as.Date(dates, format = format)
            if(!all(is.na(temp_dates))) {
              temp_dates
            } else {
              NULL
            }
          }, error = function(e) NULL)

          if (!is.null(parsed_dates)) {
            if(verbose) cat(sprintf("\n  Detected format: %s", format))

            # Choose output format based on limited_dataset flag
            output_format <- ifelse(limited_dataset, "%m/%d/%Y", "%m/01/%Y")

            # For dates that couldn't be parsed, keep the original value
            new_dates <- rep(NA, length(dates))
            valid_mask <- !is.na(parsed_dates)

            # Format only the successful dates
            new_dates[valid_mask] <- format(parsed_dates[valid_mask], output_format)

            # Keep original values for failures (but note them)
            if(any(!valid_mask) && verbose) {
              cat(sprintf("\n  Warning: %d dates could not be parsed", sum(!valid_mask)))
              cat(sprintf("\n  Sample problematic values: %s",
                          paste(head(dates[!valid_mask]), collapse=", ")))
            }

            # Update only the successful conversions
            df[[col]] <- new_dates

            success <- TRUE

            date_summary[[col]] <- list(
              original_format = format,
              sample_before = orig_dates,
              sample_after = head(df[[col]], 3),
              failed_count = sum(!valid_mask)
            )
            break
          }
        }

        if(!success && verbose) {
          cat(sprintf("\n  Warning: Could not determine date format"))
          cat(sprintf("\n  Sample values: %s", paste(head(dates), collapse=", ")))
        }

      }, error = function(e) {
        if(verbose) {
          cat(sprintf("\n\nError processing dates in %s:", col))
          cat(sprintf("\n  %s", e$message))
        }
      })
    }
  }

  if(verbose && length(date_summary) > 0) {
    if(limited_dataset == FALSE) message("\n\nDe-identifying interview_date using date-shifting...")
    cat("Date standardization summary:")
    for(field in names(date_summary)) {
      cat(sprintf("\n- %s", field))
      cat(sprintf("\n  Before: %s", paste(date_summary[[field]]$sample_before, collapse=", ")))
      cat(sprintf("\n  After:  %s", paste(date_summary[[field]]$sample_after, collapse=", ")))
      if(date_summary[[field]]$failed_count > 0) {
        cat(sprintf("\n  Failed: %d dates could not be parsed", date_summary[[field]]$failed_count))
      }
    }
    cat("\n")
  }

  return(df)
}

standardize_age <- function(df, verbose = TRUE, limited_dataset = limited_dataset) {
  if ("interview_age" %in% names(df) && limited_dataset == FALSE) {
    if(verbose && limited_dataset == FALSE) message("\nDe-identifying interview_age using age-capping...")

    # Convert to numeric first
    df$interview_age <- as.numeric(df$interview_age)
    orig_age_stats <- summary(df$interview_age)

    # Count values that will be changed
    values_to_change <- sum(df$interview_age > 1068, na.rm = TRUE)

    # Apply the age standardization (cap at 1068 months = 89 years * 12)
    df$interview_age <- pmin(df$interview_age, 1068)

    if(verbose) {
      cat("Age standardization summary:")
      cat("\nBefore:", capture.output(orig_age_stats))
      cat("\nAfter:", capture.output(summary(df$interview_age)))
      if(values_to_change > 0) {
        cat(sprintf("\nNumber of values capped at 1068 months: %d", values_to_change))
      } else {
        cat("\nNo values needed capping (all were <= 1068 months)")
      }
    }
  }

  return(df)
}

# Calculate Jaro-Winkler similarity between two strings
calculate_jaro_winkler <- function(str1, str2) {
  # Convert to lowercase for comparison
  str1 <- tolower(str1)
  str2 <- tolower(str2)

  # Handle exact matches first
  if (str1 == str2) return(1.0)
  if (nchar(str1) == 0 || nchar(str2) == 0) return(0.0)

  # Parse structure names for scientific patterns
  parse_nda_name <- function(name) {
    # Remove version numbers (01, 02, etc.)
    base <- gsub("\\d+$", "", name)
    # Split on underscores
    parts <- unlist(strsplit(base, "[_-]"))

    list(
      base = base,
      parts = parts,
      has_suffix = grepl("\\d+$", name),
      suffix = if(grepl("\\d+$", name)) gsub(".*?(\\d+)$", "\\1", name) else ""
    )
  }

  comp1 <- parse_nda_name(str1)
  comp2 <- parse_nda_name(str2)

  # Jaro-Winkler similarity algorithm implementation
  jaro_winkler <- function(s1, s2, prefix_weight = 0.1) {
    if (s1 == s2) return(1.0)
    if (nchar(s1) == 0 || nchar(s2) == 0) return(0.0)

    len1 <- nchar(s1)
    len2 <- nchar(s2)

    # Calculate match window
    match_window <- floor(max(len1, len2) / 2) - 1
    if (match_window < 0) match_window <- 0

    # Track matches
    s1_matches <- rep(FALSE, len1)
    s2_matches <- rep(FALSE, len2)
    matches <- 0
    transpositions <- 0

    # Find matches within the window
    for (i in 1:len1) {
      start <- max(1, i - match_window)
      end <- min(len2, i + match_window)

      for (j in start:end) {
        if (s2_matches[j] || substr(s1, i, i) != substr(s2, j, j)) next
        s1_matches[i] <- TRUE
        s2_matches[j] <- TRUE
        matches <- matches + 1
        break
      }
    }

    if (matches == 0) return(0.0)

    # Count transpositions
    k <- 1
    for (i in 1:len1) {
      if (!s1_matches[i]) next
      while (!s2_matches[k]) k <- k + 1
      if (substr(s1, i, i) != substr(s2, k, k)) transpositions <- transpositions + 1
      k <- k + 1
    }

    # Calculate Jaro similarity
    jaro <- (matches/len1 + matches/len2 + (matches - transpositions/2)/matches) / 3

    # Add Winkler prefix bonus
    prefix_length <- 0
    max_prefix <- min(len1, len2, 4)
    for (i in 1:max_prefix) {
      if (substr(s1, i, i) == substr(s2, i, i)) {
        prefix_length <- prefix_length + 1
      } else {
        break
      }
    }

    return(jaro + (prefix_weight * prefix_length * (1 - jaro)))
  }

  # Start with Jaro-Winkler base similarity
  base_score <- jaro_winkler(str1, str2)

  # Scientific naming bonuses
  bonuses <- 0

  # 1. Substring containment bonus (e.g., "fomo" in "fomo01")
  if (nchar(comp1$base) > 0 && nchar(comp2$base) > 0) {
    if (grepl(comp1$base, comp2$base, fixed = TRUE) || grepl(comp2$base, comp1$base, fixed = TRUE)) {
      shorter_len <- min(nchar(comp1$base), nchar(comp2$base))
      longer_len <- max(nchar(comp1$base), nchar(comp2$base))
      bonuses <- bonuses + (shorter_len / longer_len) * 0.15  # Reduced from 0.25
    }
  }

  # 2. Version number bonus (both have numbers, or one is subset of other)
  if (comp1$has_suffix && comp2$has_suffix) {
    if (comp1$suffix == comp2$suffix) {
      bonuses <- bonuses + 0.08  # Reduced from 0.15
    }
  } else if (comp1$has_suffix || comp2$has_suffix) {
    # One has version, one doesn't - check if base names match
    base1 <- comp1$base
    base2 <- comp2$base
    if (base1 == base2 || base1 == str2 || base2 == str1) {
      bonuses <- bonuses + 0.12  # Reduced from 0.2
    }
  }

  # 3. Common scientific abbreviations
  scientific_abbrevs <- list(
    demo = c("demographic", "demographics"),
    behav = c("behavioral", "behaviour", "behavior"),
    cog = c("cognitive", "cognition"),
    psych = c("psychological", "psychology"),
    neuro = c("neurological", "neuroscience"),
    clin = c("clinical"),
    fam = c("family", "familial")
  )

  # Check if either string is an abbreviation of the other
  for (abbrev in names(scientific_abbrevs)) {
    full_terms <- scientific_abbrevs[[abbrev]]
    if ((str1 == abbrev && any(sapply(full_terms, function(x) grepl(x, str2)))) ||
        (str2 == abbrev && any(sapply(full_terms, function(x) grepl(x, str1))))) {
      bonuses <- bonuses + 0.2  # Reduced from 0.3
      break
    }
  }

  # Combine base score with bonuses, cap at 1.0
  final_score <- min(1.0, base_score + bonuses)

  return(final_score)
}

# Calculate Levenshtein distance similarity between two strings
calculate_levenshtein <- function(str1, str2) {
  # Convert to lowercase
  str1 <- tolower(str1)
  str2 <- tolower(str2)

  # Create matrix
  m <- nchar(str1)
  n <- nchar(str2)
  d <- matrix(0, nrow = m + 1, ncol = n + 1)
  d[1,] <- 0:n
  d[,1] <- 0:m

  # Fill matrix
  for (i in 2:(m + 1)) {
    for (j in 2:(n + 1)) {
      cost <- if (substr(str1, i-1, i-1) == substr(str2, j-1, j-1)) 0 else 1
      d[i,j] <- min(
        d[i-1,j] + 1,      # deletion
        d[i,j-1] + 1,      # insertion
        d[i-1,j-1] + cost  # substitution
      )
    }
  }

  # Return similarity score (1 - normalized distance)
  return(1 - d[m+1,n+1] / max(m,n))
}

# Helper function to standardize handedness values
standardize_handedness <- function(value) {
  # Create mapping for handedness terms
  handedness_map <- c(
    "left" = "L",
    "l" = "L",
    "right" = "R",
    "r" = "R",
    "both" = "B",
    "ambidextrous" = "B"
  )

  # Convert to lowercase for consistent matching
  value <- tolower(value)

  # Map values using the handedness_map
  mapped_values <- handedness_map[value]
  mapped_values[is.na(mapped_values)] <- value[is.na(mapped_values)]

  # Count and report transformations
  # n_transformed <- sum(value != mapped_values, na.rm = TRUE)
  # if (n_transformed > 0) {
  #   cat(sprintf("\nTransformed %d handedness values to NDA standard format\n", n_transformed))
  # }

  return(mapped_values)
}

# Helper function to standardize boolean to numeric values
standardize_binary <- function(value) {
  # Create mapping for boolean to numeric terms (including case variations)
  binary_map <- c(
    "true" = "1",
    "false" = "0",
    "t" = "1",
    "f" = "0",
    "TRUE" = "1",
    "FALSE" = "0",
    "True" = "1",
    "False" = "0",
    "yes" = "1",
    "no" = "0",
    "y" = "1",
    "n" = "0"
  )

  # Convert value to character without changing case
  value <- as.character(value)

  # Keep track of which values were NA originally
  was_na <- is.na(value)

  # Map values using the binary_map (exact match)
  mapped_values <- binary_map[value]

  # For any unmatched values, try lowercase matching
  still_na <- is.na(mapped_values)
  if(any(still_na) && !all(was_na[still_na])) {
    mapped_values[still_na] <- binary_map[tolower(value[still_na])]
  }

  # Keep original values for any remaining unmatched
  mapped_values[is.na(mapped_values) & !was_na] <- value[is.na(mapped_values) & !was_na]

  # Restore original NAs
  mapped_values[was_na] <- NA

  # Count and report transformations
  n_transformed <- sum(value != mapped_values, na.rm = TRUE)
  if (n_transformed > 0) {
    cat(sprintf("\nTransformed %d boolean values to 0/1 format\n", n_transformed))
  }

  return(mapped_values)
}

# Parse array-like strings to vectors
parse_array_string <- function(value) {
  if (is.null(value) || is.na(value)) return(NULL)

  # Handle string arrays
  if (is.character(value)) {
    # Remove unicode prefix, brackets, and quotes
    clean_str <- gsub("\\[|\\]|u'|'", "", value)
    values <- strsplit(clean_str, ",\\s*")[[1]]
    return(tolower(trimws(values)))
  }

  # Handle numeric arrays
  if (is.numeric(value) && length(value) > 1) {
    return(sprintf("%.1f", value))
  }

  return(tolower(trimws(value)))
}

# Helper function to fetch structure elements from API
fetch_structure_elements <- function(structure_name, nda_base_url) {

  # Save the URL parameter to a local variable with a different name to avoid conflicts
  api_url <- nda_base_url


  url <- sprintf("%s/datastructure/%s", api_url, structure_name)
  response <- httr::GET(url)

  if (httr::status_code(response) != 200) {
    stop("Failed to fetch structure elements: ", content(response, "text"))
  }

  content <- jsonlite::fromJSON(rawToChar(response$content))

  if (!"dataElements" %in% names(content)) {
    stop("Unexpected API response format - no dataElements found")
  }

  elements <- content$dataElements
  return(elements)
}

# Calculate similarity with more accurate prefix handling
# Calculate similarity with more accurate prefix handling
# Modified find_and_rename_fields function with automatic unknown field handling
# Modified find_and_rename_fields function - fixed to NOT drop renamed fields
find_and_rename_fields <- function(df, elements, structure_name, measure_name, api, verbose = TRUE,
                                   auto_drop_unknown = FALSE,
                                   interactive_mode = TRUE) {
  # Store the origin environment (as processNda does)
  origin_env <- parent.frame()
  renamed <- list(
    df = df,
    renames = character(),
    columns_to_drop = character(),
    renamed_fields = character(),
    similarity_scores = list(),
    script_updated_renames = FALSE,  # Track if we updated the script for renames
    script_updated_drops = FALSE     # Track if we updated the script for drops
  )

  # Get dataframe column names
  df_cols <- names(df)
  valid_fields <- elements$name

  # Get structure short name
  structure_prefix <- substr(structure_name, nchar(structure_name) - 1, nchar(structure_name))

  # Find unknown fields
  unknown_fields <- setdiff(df_cols, valid_fields)
  if (length(unknown_fields) > 0) {
    if(verbose) cat("\nAnalyzing field name similarities...\n")
    # Process each field
    for (field in unknown_fields) {
      # Check if this field has special characters
      has_special_chars <- grepl("[#\\$%&\\*\\+/:<=>\\?@\\[\\\\\\]\\^\\{\\|\\}~]", field)
      if (has_special_chars && verbose) {
        cat(sprintf("Note: Field '%s' contains special characters\n", field))
      }

      # Check if this is a hierarchical field (contains multiple underscores with numbers)
      parts <- strsplit(field, "_")[[1]]
      num_parts <- sum(grepl("^\\d+$", parts))

      # If field has more than 2 numeric parts, it's hierarchical - skip renaming
      if (num_parts > 2) {
        if(verbose) {
          cat(sprintf("\nField: %s\n", field))
          cat("Skipping rename - hierarchical field structure detected\n")
        }
        next
      }

      # For non-hierarchical fields, proceed with similarity matching
      base_field <- sub(paste0("^", structure_prefix, "_"), "", field)

      # Calculate similarity scores
      similarities <- sapply(valid_fields, function(name) {
        # Calculate direct similarity
        calculate_levenshtein(field, name)
      })

      # Store all similarity scores
      renamed$similarity_scores[[field]] <- sort(similarities, decreasing = TRUE)

      if(verbose) {
        cat(sprintf("\nField: %s\n", field))
        cat("Top matches:\n")
        top_matches <- head(sort(similarities, decreasing = TRUE), 5)
        for(i in seq_along(top_matches)) {
          cat(sprintf("%d. %s (%.2f%% match)\n",
                      i,
                      names(top_matches)[i],
                      top_matches[i] * 100))
        }
      }

      # Remove any NA values
      similarities <- similarities[!is.na(similarities)]
      if (length(similarities) > 0) {
        best_match <- names(similarities)[which.max(similarities)]
        best_score <- max(similarities)
        if (best_score > 0.9) {  # High confidence automatic match
          # Present option to rename in interactive mode
          rename_field <- TRUE
          if(interactive_mode) {
            rename_input <- safe_readline(
              prompt = sprintf("Rename '%s' to '%s' (similarity: %.2f%%)? (y/n): ",
                               field, best_match, best_score * 100),
              default = "y"
            )
            # Check to make sure the input is a valid response
            while (!tolower(rename_input) %in% c("y", "n")){
              rename_input <- readline(prompt = "Please enter either y or n: ")
            }
            rename_field <- tolower(rename_input) %in% c("y", "yes")
          }

          # Check value range compatibility before renaming
          if(rename_field) {
            # Get value ranges for both fields
            source_range <- get_field_value_range(field, elements)
            target_range <- get_field_value_range(best_match, elements)
            
            # Check if value ranges are compatible
            range_check <- check_value_range_compatibility(source_range, target_range, field, best_match, verbose)
            
            if(!range_check$compatible) {
              if(verbose) {
                cat(sprintf("\n⚠️  VALUE RANGE CONFLICT DETECTED:\n"))
                cat(sprintf("   Source field '%s' range: %s\n", field, source_range))
                cat(sprintf("   Target field '%s' range: %s\n", best_match, target_range))
                cat(sprintf("   %s\n", range_check$message))
                cat(sprintf("   Recommendation: %s\n", range_check$recommendation))
              }
              
              # Ask user if they want to proceed despite the conflict
              if(interactive_mode) {
                proceed_input <- safe_readline(
                  prompt = sprintf("Proceed with rename despite value range conflict? (y/n): "),
                  default = "n"
                )
                while (!tolower(proceed_input) %in% c("y", "n")){
                  proceed_input <- readline(prompt = "Please enter either y or n: ")
                }
                rename_field <- tolower(proceed_input) %in% c("y", "yes")
              } else {
                # In non-interactive mode, skip the rename if there's a conflict
                rename_field <- FALSE
                if(verbose) {
                  cat("Skipping rename due to value range conflict (non-interactive mode)\n")
                }
              }
            }
          }
          
          if(rename_field) {
            if(verbose) {
              message(sprintf("\nRENAMING: '%s' to '%s' (similarity: %.2f%%)\n",
                              field, best_match, best_score * 100))
            }
            # Use [[ ]] to safely handle special characters in field names
            field_data <- df[[field]]
            df[[best_match]] <- field_data
            # Track rename
            renamed$renamed_fields <- c(renamed$renamed_fields, field)
            renamed$renames <- c(renamed$renames,
                                 sprintf("%s -> %s (%.2f%%)",
                                         field, best_match, best_score * 100))
          } else {
            # If not renaming, check if we should drop
            drop_field <- auto_drop_unknown
            if(interactive_mode) {
              drop_input <- safe_readline(
                prompt = sprintf("Drop field '%s'? (y/n): ", field),
                default = if(auto_drop_unknown) "y" else "n"
              )
              # Check that the input is a valid response
              while (!tolower(drop_input) %in% c("y", "n")){
                drop_input <- readline(prompt = "Please enter either y or n: ")
              }
              drop_field <- tolower(drop_input) %in% c("y", "yes")
            }
            if(drop_field) {
              renamed$columns_to_drop <- c(renamed$columns_to_drop, field)
              if(verbose) cat(sprintf("Will drop field '%s'\n", field))
            } else if(verbose) {
              cat(sprintf("Keeping field '%s'\n", field))
            }
          }
        } else {
          # Lower confidence match - allow user to select from top matches
          if(verbose) {
            cat(sprintf("No automatic rename - best match below 90%% threshold\n"))
          }
          # Present top 5 matches as options
          top_matches <- head(sort(similarities, decreasing = TRUE), 5)
          selected_match <- NULL
          if(interactive_mode) {
            rename_input <- safe_readline(
              prompt = sprintf("Select match for '%s' (1-5 to select, or press Enter to skip): ", field),
              default = "0"
            )
            # First check that the input is valid
            while (!grepl("^[0-5]$",rename_input)){
              rename_input <- safe_readline(prompt = 'Please enter a number 1-5 or press Enter to skip: ', default='0')
            }
            # Check if input is a number between 1-5
            if(grepl("^[1-5]$", rename_input)) {
              match_idx <- as.integer(rename_input)
              if(match_idx <= length(top_matches)) {
                selected_match <- names(top_matches)[match_idx]
              }
            }

            # If user selected a match
            if(!is.null(selected_match)) {
              # Check value range compatibility before renaming
              source_range <- get_field_value_range(field, elements)
              target_range <- get_field_value_range(selected_match, elements)
              
              # Check if value ranges are compatible
              range_check <- check_value_range_compatibility(source_range, target_range, field, selected_match, verbose)
              
              if(!range_check$compatible) {
                if(verbose) {
                  cat(sprintf("\n⚠️  VALUE RANGE CONFLICT DETECTED:\n"))
                  cat(sprintf("   Source field '%s' range: %s\n", field, source_range))
                  cat(sprintf("   Target field '%s' range: %s\n", selected_match, target_range))
                  cat(sprintf("   %s\n", range_check$message))
                  cat(sprintf("   Recommendation: %s\n", range_check$recommendation))
                }
                
                # Ask user if they want to proceed despite the conflict
                if(interactive_mode) {
                  proceed_input <- safe_readline(
                    prompt = sprintf("Proceed with rename despite value range conflict? (y/n): "),
                    default = "n"
                  )
                  while (!tolower(proceed_input) %in% c("y", "n")){
                    proceed_input <- readline(prompt = "Please enter either y or n: ")
                  }
                  if(tolower(proceed_input) %in% c("y", "yes")) {
                    # Proceed with rename
                    if(verbose) {
                      message(sprintf("\nRENAMING: '%s' to '%s' (similarity: %.2f%%)\n",
                                      field, selected_match, top_matches[match_idx] * 100))
                    }
                    field_data <- df[[field]]
                    df[[selected_match]] <- field_data
                    # Track rename
                    renamed$renamed_fields <- c(renamed$renamed_fields, field)
                    renamed$renames <- c(renamed$renames,
                                         sprintf("%s -> %s (%.2f%%)",
                                                 field, selected_match, top_matches[match_idx] * 100))
                  } else {
                    # Skip rename due to conflict
                    if(verbose) {
                      cat("Skipping rename due to value range conflict\n")
                    }
                    selected_match <- NULL  # Reset to trigger drop/keep logic
                  }
                } else {
                  # In non-interactive mode, skip the rename if there's a conflict
                  if(verbose) {
                    cat("Skipping rename due to value range conflict (non-interactive mode)\n")
                  }
                  selected_match <- NULL  # Reset to trigger drop/keep logic
                }
              } else {
                # Value ranges are compatible, proceed with rename
                if(verbose) {
                  message(sprintf("\nRENAMING: '%s' to '%s' (similarity: %.2f%%)\n",
                                  field, selected_match, top_matches[match_idx] * 100))
                }
                # Use [[ ]] to safely handle special characters in field names
                field_data <- df[[field]]
                df[[selected_match]] <- field_data
                # Track rename
                renamed$renamed_fields <- c(renamed$renamed_fields, field)
                renamed$renames <- c(renamed$renames,
                                     sprintf("%s -> %s (%.2f%%)",
                                             field, selected_match, top_matches[match_idx] * 100))
              }
            } else {
              # Ask if we should drop the field
              drop_input <- safe_readline(
                prompt = sprintf("Drop field '%s'? (y/n): ", field),
                default = if(auto_drop_unknown) "y" else "n"
              )
              # Check that input is a valid response
              while (!tolower(drop_input) %in% c("y", "n")){
                drop_input <- readline(prompt = 'Please enter either y or n: ')
              }
              drop_field <- tolower(drop_input) %in% c("y", "yes")
              if(drop_field) {
                renamed$columns_to_drop <- c(renamed$columns_to_drop, field)
                if(verbose) cat(sprintf("Will drop field '%s'\n", field))
              } else if(verbose) {
                cat(sprintf("Keeping field '%s'\n", field))
              }
            }
          } else {
            # Non-interactive mode - use auto_drop_unknown setting
            drop_field <- auto_drop_unknown
            if(drop_field) {
              renamed$columns_to_drop <- c(renamed$columns_to_drop, field)
              if(verbose) cat(sprintf("Will drop field '%s'\n", field))
            } else if(verbose) {
              cat(sprintf("Keeping field '%s'\n", field))
            }
          }
        }
      } else {
        # No matches found
        drop_field <- auto_drop_unknown
        if(interactive_mode) {
          drop_input <- safe_readline(
            prompt = sprintf("No matches found for '%s'. Drop this field? (y/n): ", field),
            default = if(auto_drop_unknown) "y" else "n"
          )
          # Check that the input is a valid response
          while (!tolower(drop_input) %in% c("y", "n")){
            drop_input <- readline(prompt = 'Please enter either y or n: ')
          }
          drop_field <- tolower(drop_input) %in% c("y", "yes")
        }
        if(drop_field) {
          renamed$columns_to_drop <- c(renamed$columns_to_drop, field)
          if(verbose) cat(sprintf("Will drop field '%s'\n", field))
        } else if(verbose) {
          cat(sprintf("Keeping field '%s'\n", field))
        }
      }
    }

    # After all renames, we need to process the final dataframe
    # First, handle renamed fields - these should be kept in the new dataframe with their new names

    # Create new dataframe - first get the list of columns to keep
    all_cols <- names(df)
    cols_to_keep <- setdiff(all_cols, union(renamed$columns_to_drop, renamed$renamed_fields))

    # Add the new columns created from renames
    renamed_targets <- c()
    for (rename_str in renamed$renames) {
      match_result <- regexec("^([^ ]+) -> ([^ ]+)", rename_str)
      if (match_result[[1]][1] != -1) {
        parts <- regmatches(rename_str, match_result)[[1]]
        if (length(parts) >= 3) {
          new_name <- parts[3]
          new_name <- sub(" \\(.*\\)$", "", new_name) # Remove percentage part if present
          renamed_targets <- c(renamed_targets, new_name)
        }
      }
    }

    # Combine original kept columns and renamed columns
    cols_to_keep <- c(cols_to_keep, renamed_targets)

    # Create the new dataframe
    df_new <- df[, cols_to_keep, drop = FALSE]

    # Update all environments EXACTLY like processNda
    base::assign(measure_name, df_new, envir = globalenv())
    base::assign(measure_name, df_new, envir = origin_env)
    if (exists(".wizaRdry_env")) {
      base::assign(measure_name, df_new, envir = .wizaRdry_env)
    }

    # Update local variable
    df <- df_new
    renamed$df <- df_new

    # Now handle renaming operations display and script updates
    has_renames <- length(renamed$renames) > 0
    has_drops <- length(renamed$columns_to_drop) > 0

    # First display rename operations if any
    if (has_renames && verbose) {
      cat("\n\nRenaming columns:\n")
      renamed_fields_str <- paste(renamed$renamed_fields, collapse = ", ")
      cat(paste(" ", renamed_fields_str))
      cat("\n")

      # Ask if we should update the cleaning script with rename operations
      update_rename_script <- FALSE
      if (interactive_mode) {
        update_input <- safe_readline(
          prompt = sprintf("Update the cleaning script to include rename operations? (y/n): "),
          default = "y"
        )
        # Check that the input is a valid response
        while (!tolower(update_input) %in% c("y", "n")){
          update_input <- readline(prompt = 'Please enter either y or n: ')
        }
        update_rename_script <- tolower(update_input) %in% c("y", "yes")
      } else {
        update_rename_script <- TRUE
      }

      if (update_rename_script) {
        if(verbose) {
          cat("Will add renaming operations to cleaning script\n")
        }

        # Update the cleaning script with rename operations
        tryCatch({
          api_str <- as.character(api)
          measure_name_str <- as.character(measure_name)

          # First try the expected path
          cleaning_script_path <- file.path(".", "nda", api_str, paste0(measure_name_str, ".R"))

          # If not found, also check alternative location
          if (!file.exists(cleaning_script_path)) {
            alt_path <- file.path(".", "clean", api_str, paste0(measure_name_str, ".R"))
            if (file.exists(alt_path)) {
              cleaning_script_path <- alt_path
              if(verbose) cat("Found cleaning script in alternative location:", cleaning_script_path, "\n")
            }
          }

          if(verbose) cat("Attempting to update cleaning script at:", cleaning_script_path, "\n")

          if(file.exists(cleaning_script_path)) {
            # Read existing content
            existing_content <- readLines(cleaning_script_path)

            # Generate rename code
            rename_code <- c("")

            # Extract field pairs from renames
            rename_pairs <- c()
            for (rename_str in renamed$renames) {
              # Extract original and new field names from the rename string
              match_result <- regexec("^([^ ]+) -> ([^ ]+)", rename_str)
              if (match_result[[1]][1] != -1) {
                parts <- regmatches(rename_str, match_result)[[1]]
                if (length(parts) >= 3) {
                  old_name <- parts[2]
                  new_name <- parts[3]
                  new_name <- sub(" \\(.*\\)$", "", new_name) # Remove percentage part if present
                  rename_pairs <- c(rename_pairs, paste0('"', old_name, '" = "', new_name, '"'))
                }
              }
            }

            # Generate a single clean rename block
            if (length(rename_pairs) > 0) {
              rename_pairs_str <- paste(rename_pairs, collapse = ", ")
              rename_code <- c(
                rename_code,
                "# Auto-generated code to rename fields",
                paste0("rename_map <- c(", rename_pairs_str, ")"),
                paste0("for (old_name in names(rename_map)) {"),
                paste0("  if (old_name %in% names(", measure_name_str, ")) {"),
                paste0("    ", measure_name_str, "[[rename_map[old_name]]] <- ", measure_name_str, "[[old_name]]"),
                paste0("    # Remove original column after renaming"),
                paste0("    ", measure_name_str, " <- ", measure_name_str, "[, !names(", measure_name_str, ") %in% c(old_name)]"),
                paste0("  }"),
                paste0("}")
              )
            }

            # Write back the entire file with the new code appended
            writeLines(c(existing_content, rename_code), cleaning_script_path)
            if(verbose) cat("Successfully updated cleaning script with rename operations\n")

            # Mark that we've updated the script for renames
            renamed$script_updated_renames <- TRUE
          } else if(verbose) {
            cat("Could not find cleaning script at expected locations.\n")
            cat("\nSuggested code for cleaning script:\n")
            cat("# Auto-generated code to rename fields\n")
            rename_pairs_str <- paste(rename_pairs, collapse = ", ")
            cat(sprintf("rename_map <- c(%s)\n", rename_pairs_str))
            cat(sprintf("for (old_name in names(rename_map)) {\n"))
            cat(sprintf("  if (old_name %%in%% names(%s)) {\n", measure_name_str))
            cat(sprintf("    %s[[rename_map[old_name]]] <- %s[[old_name]]\n", measure_name_str, measure_name_str))
            cat(sprintf("    # Remove original column after renaming\n"))
            cat(sprintf("    %s <- %s[, !names(%s) %%in%% c(old_name)]\n", measure_name_str, measure_name_str, measure_name_str))
            cat(sprintf("  }\n"))
            cat(sprintf("}\n"))
          }
        }, error = function(e) {
          if(verbose) cat("Error updating cleaning script with rename operations:", e$message, "\n")
        })
      }
    }

    # Separately handle drop operations
    if (has_drops && verbose) {
      # Get columns that were only dropped (not renamed)
      non_renamed_drops <- setdiff(renamed$columns_to_drop, renamed$renamed_fields)

      if (length(non_renamed_drops) > 0) {
        cat("\nDropping columns:\n")
        drop_fields_str <- paste(non_renamed_drops, collapse = ", ")
        cat(paste(" ", drop_fields_str))
        cat("\n")

        # Ask if we should update the cleaning script with drop operations
        update_drop_script <- FALSE
        if (interactive_mode) {
          update_input <- safe_readline(
            prompt = sprintf("Update the cleaning script to include drop operations? (y/n): "),
            default = "y"
          )
          # Check that the input is a valid response
          while (!tolower(update_input) %in% c("y", "n")){
            update_input <- readline(prompt = 'Please enter either y or n: ')
          }
          update_drop_script <- tolower(update_input) %in% c("y", "yes")
        } else {
          update_drop_script <- TRUE
        }

        if (update_drop_script) {
          if(verbose) {
            cat("Will add drop operations to cleaning script\n")
          }

          # Update the cleaning script with drop operations
          tryCatch({
            api_str <- as.character(api)
            measure_name_str <- as.character(measure_name)

            # First try the expected path
            cleaning_script_path <- file.path(".", "nda", api_str, paste0(measure_name_str, ".R"))

            # If not found, also check alternative location
            if (!file.exists(cleaning_script_path)) {
              alt_path <- file.path(".", "clean", api_str, paste0(measure_name_str, ".R"))
              if (file.exists(alt_path)) {
                cleaning_script_path <- alt_path
                if(verbose) cat("Found cleaning script in alternative location:", cleaning_script_path, "\n")
              }
            }

            if(verbose) cat("Attempting to update cleaning script at:", cleaning_script_path, "\n")

            if(file.exists(cleaning_script_path)) {
              # Read existing content
              existing_content <- readLines(cleaning_script_path)

              # Generate drop code
              drop_fields_str <- paste(shQuote(unique(non_renamed_drops)), collapse=", ")
              drop_code <- c(
                "",
                "# Auto-generated code to remove additional fields",
                paste0(measure_name_str, " <- ", measure_name_str, "[, !names(",
                       measure_name_str, ") %in% c(",
                       drop_fields_str, ")]")
              )

              # Write back the entire file with the new code appended
              writeLines(c(existing_content, drop_code), cleaning_script_path)
              if(verbose) cat("Successfully updated cleaning script with drop operations\n")

              # Mark that we've updated the script for drops
              renamed$script_updated_drops <- TRUE
            } else if(verbose) {
              cat("Could not find cleaning script at expected locations.\n")
              cat("\nSuggested code for cleaning script:\n")
              cat("# Auto-generated code to remove additional fields\n")
              cat(sprintf("%s <- %s[, !names(%s) %%in%% c(%s)]\n",
                          measure_name_str, measure_name_str, measure_name_str, drop_fields_str))
            }
          }, error = function(e) {
            if(verbose) cat("Error updating cleaning script with drop operations:", e$message, "\n")
          })
        }
      }
    }

    # Set overall script updated flag if either operation happened
    renamed$script_updated <- renamed$script_updated_renames || renamed$script_updated_drops
  }

  return(renamed)
}

# Helper function to get violating values with type conversion
# Updated get_violations function with more robust categorical matching
# Updated get_violations function
# Updated get_violations function with special handling for ranges with both :: and ;
# Updated get_violations function with better handling of non-numeric values
get_violations <- function(value, range_str) {
  if (is.null(range_str) || is.na(range_str) || range_str == "") return(character(0))

  # First check if there are non-numeric values (when expected numeric)
  if (grepl("::", range_str)) {
    # Numeric range expected - check for non-numeric values
    if (!is.numeric(value)) {
      # Try to convert to numeric to find which values can't be converted
      value_numeric <- suppressWarnings(as.numeric(as.character(value)))
      non_numeric_mask <- !is.na(value) & is.na(value_numeric)

      if (any(non_numeric_mask)) {
        non_numeric_values <- unique(value[non_numeric_mask])
        if (length(non_numeric_values) > 0) {
          return(sort(as.character(non_numeric_values)))
        }
      }
    }
  }

  # Special case for mixed ranges like "1::26;77"
  if (grepl("::", range_str) && grepl(";", range_str)) {
    # Split by semicolon first
    parts <- strsplit(range_str, ";")[[1]]
    # Get the numeric range from the first part
    range_part <- parts[grepl("::", parts)][1]
    range <- as.numeric(strsplit(range_part, "::")[[1]])
    # Get individual values from other parts
    individual_values <- as.numeric(parts[!grepl("::", parts)])
    # Combine valid values: numbers in range plus individual values
    valid_values <- c(seq(from = range[1], to = range[2]), individual_values)

    # Check for violations - handle non-numeric values appropriately
    if (is.numeric(value)) {
      invalid_mask <- !value %in% valid_values
      invalid_mask[is.na(invalid_mask)] <- FALSE
      return(sort(unique(value[invalid_mask])))
    } else {
      # For non-numeric values, we need to convert to numeric first
      value_numeric <- suppressWarnings(as.numeric(as.character(value)))
      # Identify values that can be converted but are outside range
      convertible_mask <- !is.na(value_numeric)
      invalid_mask <- convertible_mask & !value_numeric %in% valid_values
      invalid_mask[is.na(invalid_mask)] <- FALSE

      result <- sort(unique(value[invalid_mask]))
      # If no numeric violations, return empty
      if (length(result) == 0) return(character(0))
      return(result)
    }
  }

  # Rest of the function for simple ranges
  if (grepl("::", range_str)) {
    # Numeric range check
    range <- as.numeric(strsplit(range_str, "::")[[1]])

    # Handle non-numeric values appropriately
    if (is.numeric(value)) {
      invalid_mask <- value < range[1] | value > range[2]
      invalid_mask[is.na(invalid_mask)] <- FALSE
      return(sort(unique(value[invalid_mask])))
    } else {
      # For non-numeric values, we need to convert to numeric first
      value_numeric <- suppressWarnings(as.numeric(as.character(value)))
      # Identify values that can be converted but are outside range
      convertible_mask <- !is.na(value_numeric)
      invalid_mask <- convertible_mask & (value_numeric < range[1] | value_numeric > range[2])
      invalid_mask[is.na(invalid_mask)] <- FALSE

      result <- sort(unique(value[invalid_mask]))
      # If no numeric violations, return empty
      if (length(result) == 0) return(character(0))
      return(result)
    }
  } else if (grepl(";", range_str)) {
    # Categorical values
    valid_values <- trimws(strsplit(range_str, ";")[[1]])

    # Convert to character for comparison
    value_char <- as.character(value)
    invalid_mask <- !value_char %in% valid_values
    invalid_mask[is.na(invalid_mask)] <- FALSE
    return(sort(unique(value[invalid_mask])))
  }

  return(character(0))
}

# Main validation logic function
# Modified validate_structure function with better error handling
# Modified validate_structure function to better handle non-numeric values
# Modified validate_structure function to automatically fail on non-numeric values
# Fully automated validate_structure function with no user prompts for violations
validate_structure <- function(df, elements, measure_name, api, verbose = FALSE,
                               auto_drop_unknown = FALSE,
                               missing_required_fields = character(0),
                               interactive_mode = TRUE,
                               collect_only = FALSE) {
  if(verbose) cat("\nValidating data structure...")

  results <- list(
    valid = TRUE,
    missing_required = character(0),
    value_range_violations = list(),
    non_numeric_in_numeric = list(),
    unknown_fields = character(0),
    unknown_fields_dropped = character(0),
    warnings = character(0)
  )

  # Check for missing required fields
  if(length(missing_required_fields) > 0) {
    results$valid <- FALSE
    results$missing_required <- c(results$missing_required, missing_required_fields)
    if(verbose) {
      cat("\n\nERROR: Required fields missing values:")
      cat(sprintf("\n  %s", paste(missing_required_fields, collapse=", ")))
      cat("\n  These must be fixed before data submission is possible.")
    }
  }

  tryCatch({
    # Get field lists
    required_fields <- elements$name[elements$required == "Required"]
    valid_fields <- elements$name
    df_cols <- names(df)

    # Check for unknown fields
    results$unknown_fields <- setdiff(df_cols, valid_fields)
    if(length(results$unknown_fields) > 0) {
      if(verbose) {
        cat("\n\nUnknown fields detected:")
        cat(sprintf("\n  %s", paste(results$unknown_fields, collapse=", ")))
      }

      drop_fields <- FALSE
      if(interactive_mode) {
        # Prompt user for confirmation
        user_input <- safe_readline(prompt = "Do you want to drop these unknown fields? (y/n): ",
                                    default = if(auto_drop_unknown) "y" else "n")
        # Check that the input is a valid response
        while (!tolower(user_input) %in% c("y", "n")){
          user_input <- readline(prompt = 'Please enter either y or n: ')
        }
        drop_fields <- tolower(user_input) %in% c("y", "yes")
      } else {
        # Use the auto_drop_unknown setting
        drop_fields <- auto_drop_unknown
      }

      if(drop_fields) {
        # Track which fields were dropped
        results$unknown_fields_dropped <- results$unknown_fields

        # Drop the unknown fields
        df <- df[, !names(df) %in% results$unknown_fields]

        if(verbose) {
          cat("\nDropping unknown fields")
        }

        # Determine which environment contains the original dataframe
        env_to_use <- NULL
        calling_env <- parent.frame(2) # The environment that called this function

        if(exists(measure_name, envir = calling_env)) {
          env_to_use <- calling_env
        } else if(exists(".wizaRdry_env") && exists(measure_name, envir = .wizaRdry_env)) {
          env_to_use <- .wizaRdry_env
        }

        # Update the dataframe in the appropriate environment
        if(!is.null(env_to_use)) {
          assign(measure_name, df, envir = env_to_use)

          # Only update the cleaning script if not in collect_only mode
          if(!collect_only) {
            # Ask about updating cleaning script
            update_cleaning_script <- FALSE
            if(interactive_mode) {
              update_input <- safe_readline(prompt = "Update cleaning script with code to drop these fields? (y/n): ",
                                            default = "y")
              # Check that the input is a valid response
              while (!tolower(update_input) %in% c("y", "n")){
                update_input <- readline(prompt = 'Please enter either y or n: ')
              }
              update_cleaning_script <- tolower(update_input) %in% c("y", "yes")
            } else {
              update_cleaning_script <- TRUE  # Always update in non-interactive mode
            }

            if(update_cleaning_script) {
              # Generate R code for cleanup script
              tryCatch({
                api_str <- as.character(api)
                measure_name_str <- as.character(measure_name)

                # First try the expected path
                cleaning_script_path <- file.path(".", "nda", api_str, paste0(measure_name_str, ".R"))

                # If not found, also check alternative location
                if (!file.exists(cleaning_script_path)) {
                  alt_path <- file.path(".", "clean", api_str, paste0(measure_name_str, ".R"))
                  if (file.exists(alt_path)) {
                    cleaning_script_path <- alt_path
                    if(verbose) cat("\nFound cleaning script in alternative location:", cleaning_script_path)
                  }
                }

                if(verbose) cat("\nAttempting to update cleaning script at:", cleaning_script_path)

                if(file.exists(cleaning_script_path)) {
                  # Read existing content
                  existing_content <- readLines(cleaning_script_path)

                  # Create the code to remove columns
                  unknown_fields_str <- paste(shQuote(results$unknown_fields), collapse=", ")

                  # ensure case-insensitive matching in the removal command
                  removal_code <- c(
                    "",
                    "# Auto-generated code to remove unknown fields",
                    paste0(measure_name_str, " <- ", measure_name_str, "[, !tolower(names(",
                           measure_name_str, ")) %in% tolower(c(",
                           unknown_fields_str, "))]")
                  )

                  # Write back the entire file with the new code appended
                  writeLines(c(existing_content, removal_code), cleaning_script_path)
                  if(verbose) cat("\nSuccessfully updated cleaning script with code to remove unknown fields")
                } else if(verbose) {
                  cat("\nCould not find cleaning script at expected locations.")
                  unknown_fields_str <- paste(shQuote(results$unknown_fields), collapse=", ")
                  removal_code <- paste0(measure_name_str, " <- ", measure_name_str, "[, !names(",
                                         measure_name_str, ") %in% c(",
                                         unknown_fields_str, ")]")
                  cat("\n\nSuggested code for cleaning script:")
                  cat("\n", removal_code)
                }
              }, error = function(e) {
                if(verbose) cat("\nError updating cleaning script:", e$message)
              })
            }
          }
        }

        # Reset unknown fields list
        results$unknown_fields <- character(0)

        # Update column names after dropping fields
        df_cols <- names(df)
      } else {
        if(verbose) cat("\nKeeping unknown fields in the dataset")

        # Automatically fail validation if unknown fields are kept
        results$valid <- FALSE
        if(verbose) cat("\nValidation will fail due to unknown fields")
      }
    }

    # Check for invalid GUID values
    if(verbose) message("\n\nChecking for invalid GUID values...")

    # First, create a list to store the specific issues
    guid_issues <- list(
      invalid_length = character(0),
      duplicate_guids = list(),
      valid = TRUE
    )

    # Check 1: GUID length validation (must be exactly 12 characters)
    subjectkeys <- as.character(df$subjectkey)
    invalid_length_mask <- nchar(subjectkeys) != 12 & !is.na(subjectkeys) # Create a boolean to see if there are any GUIDs that are not 12 characters long

    if(any(invalid_length_mask)) {
      guid_issues$invalid_length <- unique(subjectkeys[invalid_length_mask]) # Store the GUIDs that are not 12 characters long
      guid_issues$valid <- FALSE # Set valid flag for GUIDs to FALSE

      if(verbose) {
        cat(sprintf("\n  ERROR: %d GUIDs with invalid length (must be 12 characters):", length(guid_issues$invalid_length)))
        cat(sprintf("\n    %s", paste(head(guid_issues$invalid_length, 10), collapse=", ")))
        if(length(guid_issues$invalid_length) > 10) {
          cat(sprintf(" (and %d more)", length(guid_issues$invalid_length) - 10))
        }
      }
    }

    # Check 2: GUID uniqueness per src_subject_id
    # Use aggregate to get unique src_subject_ids per GUID in one pass
    valid_rows <- !is.na(df$subjectkey) & !is.na(df$src_subject_id)
    if(any(valid_rows)) {
      guid_src_unique <- stats::aggregate(df$src_subject_id[valid_rows],
                                  by = list(GUID = df$subjectkey[valid_rows]),
                                  FUN = function(x) length(unique(x)))

      # Find GUIDs with more than one unique src_subject_id
      problematic_guids <- guid_src_unique$GUID[guid_src_unique$x > 1]

      if(length(problematic_guids) > 0) {
        guid_issues$valid <- FALSE

        # Only for problematic GUIDs, get the actual src_subject_id values
        for(guid in problematic_guids) {
          src_ids <- unique(df$src_subject_id[df$subjectkey == guid & !is.na(df$subjectkey)])
          guid_issues$duplicate_guids[[guid]] <- src_ids
        }

        if(verbose) {
          cat(sprintf("\n  ERROR: %d GUIDs used with multiple src_subject_ids:",
                    length(problematic_guids))) # Show the user the # of GUIDs with multiple src_subject_ids
          for(guid in head(problematic_guids, 5)) {
            cat(sprintf("\n    '%s' -> %s", guid,
                      paste(guid_issues$duplicate_guids[[guid]], collapse=", "))) # Show the invalid GUID and associated src_subject_ids
          }
          if(length(problematic_guids) > 5) {
            cat(sprintf("\n    (and %d more problematic GUIDs)", length(problematic_guids) - 5))
          }
        }
      }
    }

    # Update the the valid flag for results based on GUID issues
    # Store the GUID issues list in results for later use in the summary
    if(guid_issues$valid == FALSE) {
      results$valid <- FALSE
      results$guid_validation <- guid_issues # Store length issue GUIDs and duplicate GUIDs in results
    }

    # Check required fields
    missing_required <- required_fields[!required_fields %in% df_cols]
    if(length(missing_required) > 0) {
      results$valid <- FALSE
      results$missing_required <- c(results$missing_required, missing_required)
      if(verbose) {
        cat("\n\nMissing required fields:")
        cat(sprintf("\n  %s", paste(missing_required, collapse=", ")))
      }
    } else if(verbose) {
      cat("\n\nAll required fields present")
    }

    # Check value ranges and type validity
    if(verbose) cat("\n\nChecking value ranges...")

    for(col in intersect(df_cols, valid_fields)) {
      element <- elements[elements$name == col, ]

      if(nrow(element) > 0 &&
         !is.null(element$valueRange) &&
         !is.na(element$valueRange) &&
         element$valueRange != "") {

        if(verbose) {
          cat(sprintf("\n\nField: %s", col))
          cat(sprintf("\n  Type: %s", element$type))
          cat(sprintf("\n  Expected range: %s", element$valueRange))
        }

        # Handle binary fields
        if(element$valueRange == "0;1") {
          values <- as.character(df[[col]])
          if(any(tolower(values) %in% c("true", "false"), na.rm = TRUE)) {
            if(verbose) cat("\n  Converting boolean values to 0/1")
            df[[col]] <- standardize_binary(values)

            # Update in environment
            env_to_use <- NULL
            calling_env <- parent.frame(2) # The environment that called this function

            if(exists(measure_name, envir = calling_env)) {
              env_to_use <- calling_env
            } else if(exists(".wizaRdry_env") && exists(measure_name, envir = .wizaRdry_env)) {
              env_to_use <- .wizaRdry_env
            }

            if(!is.null(env_to_use)) {
              assign(measure_name, df, envir = env_to_use)
            }
          }
        }

        # Check for non-numeric values in numeric fields
        if (element$type %in% c("Integer", "Float") && !is.numeric(df[[col]])) {
          # Try to convert to see which values can't be converted
          values_numeric <- suppressWarnings(as.numeric(as.character(df[[col]])))
          non_numeric_mask <- !is.na(df[[col]]) & is.na(values_numeric)

          if (any(non_numeric_mask)) {
            non_numeric_values <- unique(df[[col]][non_numeric_mask])

            if(verbose) {
              cat(sprintf("\n  ERROR: Non-numeric values found in numeric field:"))
              cat(sprintf("\n    %s", paste(head(non_numeric_values, 5), collapse=", ")))

              if(length(non_numeric_values) > 5) {
                cat(sprintf(" (and %d more)", length(non_numeric_values) - 5))
              }

              cat("\n  Validation will fail due to non-numeric values in numeric field")
            }

            # Add to results
            results$non_numeric_in_numeric[[col]] <- list(
              expected_type = element$type,
              values = non_numeric_values
            )

            # Automatically fail validation - no user prompt
            results$valid <- FALSE
          }
        }

        # Check for range violations
        violating_values <- tryCatch({
          get_violations(df[[col]], element$valueRange)
        }, error = function(e) {
          results$warnings <- c(
            results$warnings,
            sprintf("Error checking %s: %s", col, e$message)
          )
          character(0)
        })

        if(length(violating_values) > 0) {
          if(verbose) {
            cat("\n  ERROR: Value range violations found:")
            cat(sprintf("\n    Invalid values: %s",
                        paste(head(violating_values, 5), collapse=", ")))

            if(length(violating_values) > 5) {
              cat(sprintf(" (and %d more...)",
                          length(violating_values) - 5))
            }

            cat(sprintf("\n  Validation will fail due to violations in %s", col))
          }

          # Automatically fail validation - no user prompt
          results$valid <- FALSE
          results$value_range_violations[[col]] <- list(
            expected = element$valueRange,
            actual = violating_values
          )
        } else if(verbose) {
          cat("\n  All values within expected range")
        }
      }
    }

    # Final summary
    if(verbose) {
      message("\n\nValidation Summary:")
      message(sprintf("- Status: %s",
                      if(results$valid) "PASSED" else "FAILED"))

      if(length(results$unknown_fields) > 0) {
        message(sprintf("- Unknown fields: %d (%s)",
                        length(results$unknown_fields),
                        paste(head(results$unknown_fields, 10), collapse=", ")))

        if(length(results$unknown_fields) > 10) {
          message(sprintf("  ... and %d more", length(results$unknown_fields) - 10))
        }
      }

      # If GUID issues were found, report them
      if(length(results$guid_validation) > 0) {
        # First report the top 10 GUIDs with invalid length (if any)
        if(length(results$guid_validation$invalid_length) > 0) {
          message(sprintf("- GUIDs with invalid length found (must be 12 characters): %d (%s)",
                          length(results$guid_validation$invalid_length),
                          paste(head(results$guid_validation$invalid_length, 10), collapse=", ")))
          if(length(results$guid_validation$invalid_length) > 10) {
            message(sprintf("  ... and %d more", length(results$guid_validation$invalid_length) - 10))
          }
        }

        # Report the top 10 GUIDs with multiple src_subject_ids (if any)
        if(length(results$guid_validation$duplicate_guids) > 0) {
            message(sprintf("- Non-unique GUIDs found: %d (%s)",
                            length(results$guid_validation$duplicate_guids),
                            paste(head(names(results$guid_validation$duplicate_guids), 10), collapse=", ")))
          if(length(results$guid_validation$duplicate_guids) > 10) {
            message(sprintf("  ... and %d more", length(results$guid_validation$duplicate_guids) - 10))
          }
        }
      }

      if(length(results$missing_required) > 0) {
        message(sprintf("- Missing required fields: %d (%s)",
                        length(results$missing_required),
                        paste(results$missing_required, collapse=", ")))
      }

      if(length(results$value_range_violations) > 0) {
        message(sprintf("- Fields with range violations: %d (%s)",
                        length(results$value_range_violations),
                        paste(names(results$value_range_violations), collapse=", ")))
      }

      if(length(results$non_numeric_in_numeric) > 0) {
        message(sprintf("- Fields with non-numeric values in numeric fields: %d (%s)",
                        length(results$non_numeric_in_numeric),
                        paste(names(results$non_numeric_in_numeric), collapse=", ")))
      }

      if(length(results$warnings) > 0) {
        message("\n\nWarnings:")
        for(warning in results$warnings) {
          message(sprintf("\n- %s", warning))
        }
      }

      cat("\n")
    }
  }, error = function(e) {
    results$valid <- FALSE
    results$warnings <- c(
      results$warnings,
      sprintf("Critical validation error: %s", e$message)
    )

    if(verbose) {
      cat("\n\nCritical Validation Error:")
      cat(sprintf("\n  %s", e$message))
    }
  })

  return(results)
}

# Modify the main validation function to include date standardization
# Add enhanced debug logging
debug_print <- function(msg, df = NULL, sample_size = 5, debug = FALSE) {
  if(debug) {
    cat("\nDEBUG:", msg, "\n")
    if (!is.null(df)) {
      cat("Dataframe info:\n")
      cat("- Dimensions:", paste(dim(df), collapse=" x "), "\n")
      cat("- Column names:", paste(names(df), collapse=", "), "\n")
      cat("- First", sample_size, "rows of data:\n")
      message(head(df, sample_size))
    }
  }
}


# Modified ndaValidator with enhanced error handling
# Helper function to standardize column names
standardize_column_names <- function(df, structure_name, verbose = FALSE) {
  if(verbose) cat("\nStandardizing column names...")

  # Get structure short name by taking last 2 digits of structure_name
  prefix <- substr(structure_name, nchar(structure_name) - 1, nchar(structure_name))

  # Create name mapping function
  standardize_name <- function(name) {
    # Replace hyphens with underscores (but don't lowercase)
    name <- gsub("-", "_", name)
    # Handle prefix if present
    if (grepl(paste0("^", prefix, "[_-]?\\d+$"), name, ignore.case = TRUE)) {
      # Ensure consistent underscore between prefix and number
      name <- gsub(paste0("^(", prefix, ")[_-]?(\\d+)$"), "\\1_\\2", name, ignore.case = TRUE)
    }
    return(name)
  }


  # Standardize column names
  old_names <- names(df)
  new_names <- sapply(old_names, standardize_name)

  # Report changes
  changed <- old_names != new_names
  if (any(changed) && verbose) {
    cat("\n\nColumn name changes:")
    for (i in which(changed)) {
      cat(sprintf("\n  %s -> %s", old_names[i], new_names[i]))
    }

    # Add summary
    cat(sprintf("\n\nSummary: %d names standardized\n", sum(changed)))
  }

  # Apply new names
  names(df) <- new_names
  return(df)
}

parse_field_name <- function(name) {
  # Split name into components
  parts <- strsplit(name, "_")[[1]]

  # Extract prefix and numeric components
  prefix <- parts[1]  # e.g., "lec"

  # Get all numeric components
  numbers <- as.numeric(grep("^\\d+$", parts, value = TRUE))

  list(
    prefix = prefix,
    numbers = numbers,
    original = name
  )
}

# Helper function to compare numeric patterns
compare_numeric_patterns <- function(name1, name2) {
  # Parse both names
  parsed1 <- parse_field_name(name1)
  parsed2 <- parse_field_name(name2)

  # Must have same prefix
  if (parsed1$prefix != parsed2$prefix) {
    return(0)
  }

  # Compare number of numeric components
  n1 <- length(parsed1$numbers)
  n2 <- length(parsed2$numbers)

  # If one is hierarchical (has underscore numbers) and other isn't, they're different
  if ((grepl("_\\d+_\\d+", name1) && !grepl("_\\d+_\\d+", name2)) ||
      (!grepl("_\\d+_\\d+", name1) && grepl("_\\d+_\\d+", name2))) {
    return(0.3)  # Very low similarity for different patterns
  }

  # Compare the actual numbers
  max_nums <- max(n1, n2)
  matching_nums <- sum(parsed1$numbers[1:min(n1, n2)] == parsed2$numbers[1:min(n1, n2)])

  # Calculate similarity based on matching numbers
  similarity <- matching_nums / max_nums

  return(similarity)
}

# Modify transform_value_ranges to be more robust
# improve the transform_value_ranges function to handle date and logical values better

# Modified transform_value_ranges function to return info about required field issues
# Modified transform_value_ranges function
transform_value_ranges <- function(df, elements, verbose = FALSE) {
  if(verbose) cat("\nChecking and transforming value ranges...")

  # Initialize missing field tracking
  missing_required_fields <- character(0)

  # Initialize range_summary to store transformation details
  range_summary <- list()

  # Check which columns are required
  required_fields <- elements$name[elements$required == "Required"]

  # More robust check for missing/NA values in required fields
  missing_required <- FALSE
  missing_fields <- character(0)

  for(field in required_fields) {
    if(field %in% names(df)) {
      if(is.character(df[[field]])) {
        missing_data_codes <- is.na(df[[field]]) | df[[field]] == ""
      } else {
        missing_data_codes <- is.na(df[[field]])
      }

      if(any(missing_data_codes)) {
        missing_required <- TRUE
        missing_fields <- c(missing_fields, field)
      }
    } else {
      missing_required <- TRUE
      missing_fields <- c(missing_fields, field)
    }
  }

  if(missing_required) {
    if(verbose) {
      warning(sprintf('NDA required values contain NA or no data in fields: %s\nThese will need to be fixed before submission.',
                      paste(missing_fields, collapse=", ")))
    }
    # Store missing fields for later use
    missing_required_fields <- missing_fields
  }

  # Process binary fields safely
  binary_fields <- elements$name[!is.na(elements$valueRange) & elements$valueRange == "0;1"]
  if (length(binary_fields) > 0) {
    if(verbose) cat("\n\nProcessing binary fields (0;1)...")

    for (field in binary_fields) {
      if (field %in% names(df)) {
        # Safe conversion - always convert to character first
        values <- as.character(df[[field]])
        potential_booleans <- c("true", "false", "t", "f", "TRUE", "FALSE", "True", "False")

        # Check if any of the values match potential boolean values
        has_boolean <- any(tolower(values) %in% tolower(potential_booleans), na.rm = TRUE)

        if (has_boolean) {
          if(verbose) {
            cat(sprintf("\n\nField: %s", field))
            cat("\n  Converting boolean values to 0/1")
          }

          # Store original values
          orig_values <- unique(values)

          # Transform values more carefully
          result <- standardize_binary(values)
          # Make sure we don't introduce NAs where there weren't any before
          was_na <- is.na(values)
          df[[field]] <- result

          # Store summary
          range_summary[[field]] <- list(
            type = "binary",
            values_transformed = sum(values != df[[field]], na.rm = TRUE),
            orig_values = orig_values,
            new_values = unique(df[[field]])
          )

          if(verbose) {
            cat("\n  Value mapping:")
            cat(sprintf("\n    Before: %s", paste(head(orig_values), collapse=", ")))
            cat(sprintf("\n    After:  %s", paste(head(unique(df[[field]])), collapse=", ")))
          }
        }
      }
    }
  }

  # Process fields with value range rules
  for (i in 1:nrow(elements)) {
    field_name <- elements$name[i]
    value_range <- elements$valueRange[i]

    # Skip if field doesn't exist, has no range, or is binary (already processed)
    if (!field_name %in% names(df) ||
        is.na(value_range) ||
        value_range == "" ||
        value_range == "0;1" ||
        !grepl(";", value_range)) {
      next
    }

    if(verbose) {
      cat(sprintf("\n\nField: %s", field_name))
      cat(sprintf("\n  Expected values: %s", value_range))
    }

    # Store original values
    orig_values <- tryCatch({
      unique(df[[field_name]])
    }, error = function(e) {
      if(verbose) cat(sprintf("\n  Error getting unique values: %s", e$message))
      return(NA)
    })

    # Get expected values and standardize
    expected_values <- trimws(unlist(strsplit(value_range, ";")))
    current_values <- df[[field_name]]
    transformed_count <- 0

    # Special handling for handedness
    if(field_name == "handedness") {
      df[[field_name]] <- standardize_handedness(current_values)
      transformed_count <- sum(current_values != df[[field_name]], na.rm = TRUE)
    } else {
      # Convert field to character for safer comparison
      df[[field_name]] <- as.character(df[[field_name]])

      # Map case-insensitive matches for other fields
      for (exp_val in expected_values) {
        # Handle NA values specially
        matches <- !is.na(current_values) & tolower(current_values) == tolower(exp_val)
        if (any(matches, na.rm = TRUE)) {
          transformed_count <- transformed_count + sum(matches, na.rm = TRUE)
          df[[field_name]][matches] <- exp_val
        }
      }
    }

    # Store summary
    range_summary[[field_name]] <- list(
      type = "categorical",
      values_transformed = transformed_count,
      orig_values = orig_values,
      new_values = unique(df[[field_name]])
    )

    if(verbose && transformed_count > 0) {
      cat(sprintf("\n  Transformed %d values", transformed_count))
      cat("\n  Value comparison:")
      cat(sprintf("\n    Before: %s", paste(head(orig_values), collapse=", ")))
      cat(sprintf("\n    After:  %s", paste(head(unique(df[[field_name]])), collapse=", ")))
    }
  }

  # Print summary if needed
  if(verbose && length(range_summary) > 0) {
    cat("\n\nValue range transformation summary:")
    for(field in names(range_summary)) {
      cat(sprintf("\n- %s", field))
      if(range_summary[[field]]$values_transformed > 0) {
        cat(sprintf(" (%d values standardized)", range_summary[[field]]$values_transformed))
      }
    }
    cat("\n")
  }

  # Add the missing fields as an attribute to the dataframe
  attr(df, "missing_required_fields") <- missing_required_fields

  return(df)
}

# Robust bidirectional transformation function for field values
# Fixed bidirectional transformation function that properly handles empty cells
transform_field_values <- function(df, elements, verbose = TRUE) {
  if(verbose) cat("\n\n--- Transforming Field Values (Bidirectional) ---\n")

  # Track all transformations
  all_transformations <- list()
  field_statuses <- list()

  # Process each field in the structure
  for (i in 1:nrow(elements)) {
    field_name <- elements$name[i]
    type <- elements$type[i]
    value_range <- elements$valueRange[i]
    notes <- elements$notes[i]

    # Skip if field doesn't exist
    if (!field_name %in% names(df)) next

    # Get current field values
    field_values <- df[[field_name]]
    field_values_char <- as.character(field_values)

    # Determine if we need a transformation
    needs_transformation <- FALSE
    transformation_direction <- NULL
    invalid_values <- NULL

    # Check numeric fields for non-numeric values
    if (type %in% c("Integer", "Float")) {
      # Test if values can be converted to numeric
      numeric_test <- suppressWarnings(as.numeric(field_values_char))
      # Handle empty strings - treat them as NA
      empty_mask <- !is.na(field_values) & field_values_char == ""
      if (any(empty_mask)) {
        field_values_char[empty_mask] <- NA
        field_values[empty_mask] <- NA
      }

      # Check for non-numeric, non-empty values
      invalid_mask <- !is.na(field_values) & is.na(numeric_test) & field_values_char != ""

      if (any(invalid_mask)) {
        needs_transformation <- TRUE
        transformation_direction <- "string_to_numeric"
        invalid_values <- unique(field_values[invalid_mask])
      }
    }
    # Check string fields that contain numeric values but should be text
    else if (type == "String" && !is.null(value_range) && !is.na(value_range) && value_range != "") {
      # If value range specifies text values but we have numbers
      if (grepl(";", value_range) && !grepl("::", value_range)) {
        valid_values <- trimws(strsplit(value_range, ";")[[1]])
        # Check if valid values are non-numeric text
        if (any(!suppressWarnings(all(!is.na(as.numeric(valid_values)))))) {
          # Check if our current values are numeric and need conversion to text
          is_numeric_mask <- !is.na(field_values) & !is.na(suppressWarnings(as.numeric(field_values_char)))
          if (any(is_numeric_mask)) {
            needs_transformation <- TRUE
            transformation_direction <- "numeric_to_string"
            invalid_values <- unique(field_values[is_numeric_mask])
          }
        }
      }
    }

    # Skip if no transformation needed
    if (!needs_transformation || length(invalid_values) == 0) next

    if(verbose) {
      cat(sprintf("\nField: %s", field_name))
      cat(sprintf("\n  Type: %s", type))
      cat(sprintf("\n  Expected range: %s", value_range))
      cat(sprintf("\n  Direction: %s", transformation_direction))
      cat(sprintf("\n  Invalid values: %s", paste(invalid_values, collapse=", ")))
    }

    # Extract mapping rules
    mapping <- NULL

    # Try to extract mapping from notes field
    if (!is.null(notes) && !is.na(notes) && notes != "") {
      # Get mapping rules - handle bidirectional mapping
      mapping_result <- extract_bidirectional_mappings(notes, transformation_direction)
      mapping <- mapping_result$mapping

      if(verbose && !is.null(mapping) && length(mapping) > 0) {
        cat("\n  Extracted mapping rules from notes:")
        for (key in names(mapping)) {
          cat(sprintf("\n    '%s' -> '%s'", key, mapping[[key]]))
        }
      }
    }

    # If no mapping found, try to create one based on value range
    if ((is.null(mapping) || length(mapping) == 0) &&
        !is.null(value_range) && !is.na(value_range) && value_range != "") {

      if (transformation_direction == "string_to_numeric") {
        # Get valid numeric values from value range
        valid_values <- NULL
        if (grepl("::", value_range)) {
          # Numeric range (e.g., "1::3")
          range_parts <- strsplit(value_range, "::")[[1]]
          range_start <- as.numeric(range_parts[1])
          range_end <- as.numeric(range_parts[2])
          valid_values <- seq(from = range_start, to = range_end)
        } else if (grepl(";", value_range)) {
          # Discrete values (e.g., "1;2")
          valid_values <- as.numeric(strsplit(value_range, ";")[[1]])
        }

        # Create position-based mapping if we have valid values
        if (!is.null(valid_values) && length(invalid_values) <= length(valid_values)) {
          mapping <- list()
          for (j in 1:length(invalid_values)) {
            # Skip empty strings - they'll be handled as NAs
            if (invalid_values[j] != "") {
              mapping[[as.character(invalid_values[j])]] <- as.character(valid_values[j])
            }
          }

          if(verbose && length(mapping) > 0) {
            cat("\n  Created position-based mapping:")
            for (key in names(mapping)) {
              cat(sprintf("\n    '%s' -> '%s'", key, mapping[[key]]))
            }
          }
        }
      }
      else if (transformation_direction == "numeric_to_string") {
        # Get valid string values from value range
        if (grepl(";", value_range)) {
          valid_text_values <- trimws(strsplit(value_range, ";")[[1]])

          # Create position-based mapping if we have valid values
          if (length(invalid_values) <= length(valid_text_values)) {
            mapping <- list()
            for (j in 1:length(invalid_values)) {
              mapping[[as.character(invalid_values[j])]] <- valid_text_values[j]
            }

            if(verbose && length(mapping) > 0) {
              cat("\n  Created position-based mapping:")
              for (key in names(mapping)) {
                cat(sprintf("\n    '%s' -> '%s'", key, mapping[[key]]))
              }
            }
          }
        }
      }
    }

    # Apply the mapping if we have one
    if (!is.null(mapping) && length(mapping) > 0) {
      transformations <- list()
      transformed_count <- 0

      # Apply each mapping rule
      for (from_value in names(mapping)) {
        to_value <- mapping[[from_value]]

        # Case-insensitive matching for text values
        if (transformation_direction == "string_to_numeric") {
          matches <- !is.na(field_values) &
            tolower(field_values_char) == tolower(from_value)
        } else {
          # Exact matching for numeric values
          matches <- !is.na(field_values) &
            field_values_char == from_value
        }

        if (any(matches)) {
          field_values_char[matches] <- to_value
          transformed_count <- transformed_count + sum(matches)
          transformations[[from_value]] <- list(
            to = to_value,
            count = sum(matches)
          )
        }
      }

      # Update the dataframe with transformed values
      if (transformed_count > 0) {
        # Apply the appropriate type conversion
        if (transformation_direction == "string_to_numeric") {
          if (type == "Integer") {
            df[[field_name]] <- suppressWarnings(as.integer(field_values_char))
          } else {
            df[[field_name]] <- suppressWarnings(as.numeric(field_values_char))
          }
        } else {
          # Just use the character values for string conversion
          df[[field_name]] <- field_values_char
        }

        # Record successful transformation
        all_transformations[[field_name]] <- transformations
        field_statuses[[field_name]] <- "FIXED"

        if(verbose) {
          cat("\n  FIXED: Transformed values")
          for (value in names(transformations)) {
            cat(sprintf("\n    '%s' -> '%s' (%d occurrences)",
                        value, transformations[[value]]$to,
                        transformations[[value]]$count))
          }
        }
      } else {
        field_statuses[[field_name]] <- "FAILED"
        if(verbose) cat("\n  FAILED: Could not transform all values")
      }
    } else {
      field_statuses[[field_name]] <- "FAILED"
      if(verbose) cat("\n  FAILED: No mapping rules found")
    }

    # Always handle empty strings separately for numeric fields
    if (type %in% c("Integer", "Float")) {
      # Convert empty strings to NA
      empty_mask <- !is.na(df[[field_name]]) & as.character(df[[field_name]]) == ""
      if (any(empty_mask)) {
        if(verbose) {
          cat(sprintf("\n  Converting %d empty string values to NA", sum(empty_mask)))
        }
        df[[field_name]][empty_mask] <- NA
      }
    }
  }

  # Final summary report
  if(verbose && length(all_transformations) > 0) {
    cat("\n\nField Value Transformation Summary:")
    for (field in names(all_transformations)) {
      cat(sprintf("\n- %s: %s", field, field_statuses[[field]]))
      if (field_statuses[[field]] == "FIXED") {
        # Calculate total transformations for this field
        total_transformed <- sum(sapply(all_transformations[[field]],
                                        function(x) x$count))
        cat(sprintf(" (%d values transformed)", total_transformed))
      }
    }
    cat("\n")
  }

  return(df)
}
# Helper function to extract bidirectional mappings from notes
extract_bidirectional_mappings <- function(notes, direction = "string_to_numeric") {
  if (is.null(notes) || is.na(notes) || notes == "") {
    return(list(mapping = NULL, source = "none"))
  }

  mapping <- list()
  source <- "none"

  # Try multiple pattern matching approaches

  # 1. Look for explicit mappings with equals sign
  # Format: "text=number" or "number=text"
  if (grepl("=", notes)) {
    parts <- strsplit(notes, "[;,]")[[1]]
    for (part in parts) {
      if (grepl("=", part)) {
        key_value <- strsplit(part, "=")[[1]]
        if (length(key_value) >= 2) {
          key <- trimws(key_value[1])
          value <- trimws(key_value[2])

          # Determine which is the string and which is the number
          key_numeric <- suppressWarnings(!is.na(as.numeric(key)))
          value_numeric <- suppressWarnings(!is.na(as.numeric(value)))

          if (key_numeric && !value_numeric) {
            # number=text format
            if (direction == "numeric_to_string") {
              mapping[[key]] <- value
            } else {
              mapping[[value]] <- key
            }
          } else if (!key_numeric && value_numeric) {
            # text=number format
            if (direction == "string_to_numeric") {
              mapping[[key]] <- value
            } else {
              mapping[[value]] <- key
            }
          }
        }
      }
    }

    if (length(mapping) > 0) {
      source <- "equals_pattern"
      return(list(mapping = mapping, source = source))
    }
  }

  # 2. Look for "X means Y" or "X represents Y" patterns
  patterns <- c(
    "([^ ]+)\\s+means\\s+([^ ]+)",
    "([^ ]+)\\s+represents\\s+([^ ]+)",
    "([^ ]+)\\s+is\\s+([^ ]+)",
    "([^ ]+)\\s+for\\s+([^ ]+)"
  )

  for (pattern in patterns) {
    matches <- gregexpr(pattern, notes, perl = TRUE)
    if (matches[[1]][1] != -1) {
      result <- regmatches(notes, matches)[[1]]
      for (match in result) {
        parts <- regexec(pattern, match, perl = TRUE)
        extracted <- regmatches(match, parts)[[1]]
        if (length(extracted) >= 3) {
          key <- trimws(extracted[2])
          value <- trimws(extracted[3])

          # Determine which is the string and which is the number
          key_numeric <- suppressWarnings(!is.na(as.numeric(key)))
          value_numeric <- suppressWarnings(!is.na(as.numeric(value)))

          if (key_numeric && !value_numeric) {
            # number means/represents text format
            if (direction == "numeric_to_string") {
              mapping[[key]] <- value
            } else {
              mapping[[value]] <- key
            }
          } else if (!key_numeric && value_numeric) {
            # text means/represents number format
            if (direction == "string_to_numeric") {
              mapping[[key]] <- value
            } else {
              mapping[[value]] <- key
            }
          }
        }
      }
    }

    if (length(mapping) > 0) {
      source <- "means_pattern"
      return(list(mapping = mapping, source = source))
    }
  }

  # 3. Last resort - try to find any pattern with text and numbers in notes
  text_num_pattern <- "([a-zA-Z:_-]+)\\s*[=:]\\s*(\\d+)"
  num_text_pattern <- "(\\d+)\\s*[=:]\\s*([a-zA-Z:_-]+)"

  # Choose pattern based on transformation direction
  if (direction == "string_to_numeric") {
    pattern <- text_num_pattern
  } else {
    pattern <- num_text_pattern
  }

  matches <- gregexpr(pattern, notes, perl = TRUE)
  if (matches[[1]][1] != -1) {
    result <- regmatches(notes, matches)[[1]]
    for (match in result) {
      parts <- regexec(pattern, match, perl = TRUE)
      extracted <- regmatches(match, parts)[[1]]
      if (length(extracted) >= 3) {
        if (direction == "string_to_numeric") {
          text_val <- trimws(extracted[2])
          num_val <- trimws(extracted[3])
          mapping[[text_val]] <- num_val
        } else {
          num_val <- trimws(extracted[2])
          text_val <- trimws(extracted[3])
          mapping[[num_val]] <- text_val
        }
      }
    }

    if (length(mapping) > 0) {
      source <- "general_pattern"
      return(list(mapping = mapping, source = source))
    }
  }

  # If we get here, no mappings were found
  return(list(mapping = mapping, source = source))
}

# Updated ndaValidator to incorporate better missing value handling
# Updated ndaValidator to incorporate better missing value handling
ndaValidator <- function(measure_name,
                         api,
                         limited_dataset = FALSE,
                         nda_base_url = "https://nda.nih.gov/api/datadictionary/v2",
                         verbose = TRUE,
                         debug = FALSE,
                         auto_drop_unknown = FALSE,
                         interactive_mode = TRUE,
                         modified_structure = NULL) {
  tryCatch({
    # Initialize a list to track all columns to be removed
    all_columns_to_drop <- character(0)

    if (!exists(".wizaRdry_env")) {
      .wizaRdry_env <- new.env(parent = parent.frame())
    }

    # Get the dataframe from the environment
    df <- base::get(measure_name, envir = .wizaRdry_env)

    # Force all complex/problematic data types to character immediately
    for (col in names(df)) {
      tryCatch({
        # Convert POSIXct and other complex classes to character
        if (inherits(df[[col]], "POSIXt") ||
            inherits(df[[col]], "Date") ||
            length(class(df[[col]])) > 1) {

          if(verbose) message(sprintf("Column '%s' has a complex class structure. Converting to character.", col))
          df[[col]] <- as.character(df[[col]])
        }

        # Test if column is accessible
        dummy <- df[[col]][1]
      }, error = function(e) {
        # If any error, convert to character
        if(verbose) message(sprintf("Column '%s' has an unusable type. Converting to character.", col))

        # Try three different approaches to fix problematic columns
        tryCatch({
          df[[col]] <- as.character(df[[col]])
        }, error = function(e2) {
          tryCatch({
            df[[col]] <- as.character(unlist(df[[col]]))
          }, error = function(e3) {
            # Last resort - replace with NAs
            if(verbose) message(sprintf("Could not convert column '%s'. Replacing with NAs.", col))
            df[[col]] <- rep(NA, nrow(df))
          })
        })
      })
    }

    # Save the cleaned dataframe
    assign(measure_name, df, envir = .wizaRdry_env)

    # Get structure name and fetch/use elements
    structure_name <- measure_name

    if (!is.null(modified_structure)) {
      # Use the enhanced structure that was passed in
      message("\n\nUsing enhanced NDA structure with ndar_subject01 definitions...")
      elements <- modified_structure$dataElements

      if (is.null(elements) || nrow(elements) == 0) {
        stop("Enhanced structure contains no dataElements")
      }

      message(sprintf("Enhanced structure contains %d field definitions", nrow(elements)))

      # Show what key fields are now defined
      key_fields <- c("race", "phenotype", "phenotype_description", "twins_study", "sibling_study")
      enhanced_fields <- intersect(key_fields, elements$name)
      if (length(enhanced_fields) > 0) {
        message(sprintf("Enhanced with ndar_subject01 definitions for: %s",
                        paste(enhanced_fields, collapse = ", ")))
      }

    } else {
      # Original logic: fetch from API
      message("\n\nFetching ", structure_name, " Data Structure from NDA API...")
      elements <- fetch_structure_elements(structure_name, nda_base_url)
    }

    if (is.null(elements) || nrow(elements) == 0) {
      stop("No elements found in the structure definition")
    }

    # PHASE 1: NA Value Mapping
    if(verbose) message("\n\n--- PHASE 1: NA Value Mapping ---")

    # Handle missing required fields
    required_fields <- elements$name[elements$required == "Required"]
    missing_required <- required_fields[!required_fields %in% names(df)]
    if(length(missing_required) > 0) {
      df <- handle_missing_fields(df, elements, missing_required, verbose = verbose)
    }

    # PHASE 2: Column Standardization
    if(verbose) message("\n\n--- PHASE 2: Column Standardization ---")

    # Standardize column names
    df <- standardize_column_names(df, structure_name, verbose = verbose)
    df <- standardize_field_names(df, measure_name, verbose = verbose)

    # Rename fields with close matches - track columns rename
    renamed_results <- find_and_rename_fields(df, elements, structure_name, measure_name, api,
                                              verbose = verbose,
                                              auto_drop_unknown = auto_drop_unknown,
                                              interactive_mode = interactive_mode)

    df <- renamed_results$df

    # Get the list of fields that were renamed (not to be dropped at the end)
    renamed_fields <- renamed_results$renamed_fields

    # Collect columns that were dropped during renaming
    all_columns_to_drop <- c(all_columns_to_drop,
                             setdiff(renamed_results$columns_to_drop, renamed_fields))

    # PHASE 3: Value Transformation
    if(verbose) message("\n\n--- PHASE 3: Value Transformation ---")

    # Convert logical values to character
    for(col in names(df)) {
      if(is.logical(df[[col]])) {
        if(verbose) cat(sprintf("\nConverting logical column %s to character", col))
        df[[col]] <- as.character(df[[col]])
      }
    }

    # Transform value ranges
    df <- transform_value_ranges(df, elements, verbose = verbose)

    # Extract missing required fields from attributes
    missing_required_fields <- attr(df, "missing_required_fields")
    if(is.null(missing_required_fields)) missing_required_fields <- character(0)

    # Transform field values using bidirectional approach
    df <- transform_field_values(df, elements, verbose = verbose)

    # Convert array fields to numeric codes - ADD THIS LINE
    df <- convert_array_fields(df, elements, verbose = verbose)

    # Apply type conversions
    df <- apply_type_conversions(df, elements, verbose = verbose)

    # Apply missing value code standardization - THIS IS THE KEY ENHANCEMENT
    df <- fix_na_values(df, elements, verbose = verbose)

    # PHASE 4: Validation and De-Identification
    if(verbose) message("\n\n--- PHASE 4: Validation and De-Identification ---")

    # De-identification steps
    df <- standardize_dates(df, verbose = verbose, limited_dataset = limited_dataset)
    df <- standardize_age(df, verbose = verbose, limited_dataset = limited_dataset)

    # Save processed dataframe back to environment
    assign(measure_name, df, envir = .wizaRdry_env)

    # Final validation - also collects unknown fields to drop
    validation_results <- validate_structure(df, elements, measure_name, api,
                                             verbose = verbose,
                                             auto_drop_unknown = auto_drop_unknown,
                                             missing_required_fields = missing_required_fields,
                                             interactive_mode = interactive_mode,
                                             collect_only = TRUE)  # Don't update script yet

    # Add the unknown fields that were identified during validation
    # BUT exclude fields that were successfully renamed
    if(length(validation_results$unknown_fields_dropped) > 0) {
      fields_to_drop <- setdiff(validation_results$unknown_fields_dropped, renamed_fields)
      all_columns_to_drop <- c(all_columns_to_drop, fields_to_drop)
    }
    # Provide an attribute to the validation result with the processed dataframe
    # This allows the calling function to access the updated dataframe
    validation_results$df <- df

    # Store NDA structure as an attribute
    attr(validation_results, "nda_structure") <- list(
      shortName = structure_name,
      dataElements = elements
    )

    # Final check for missing required values
    if(!validation_results$valid && length(validation_results$missing_required) > 0) {
      if(verbose) message("\nValidation FAILED: Required fields are missing or contain NA values")
    }
    return(validation_results)
  }, error = function(e) {
    message("Error in ndaValidator: ", e$message)
    if(debug) {
      message("Traceback:")
      message(paste(capture.output(traceback()), collapse="\n"))
    }
    return(NULL)
  })
}

# Simple utility to check the actual dataset values directly
# Fixed check_special_values function without the environment argument error
check_special_values <- function(measure_name, verbose = TRUE) {
  # Check if the object exists first
  if (!exists(measure_name)) {
    if (verbose) message(sprintf("Dataset %s not found", measure_name))
    return(NULL)
  }

  # Get the dataset (without specifying environment)
  df <- base::get(measure_name)

  if (verbose) message(sprintf("\nChecking values in %s dataset...", measure_name))

  # Check for -99 values in integer columns
  special_value_cols <- c()

  for (col in names(df)) {
    # Skip non-numeric columns
    if (!is.numeric(df[[col]])) next

    # Check for special values
    has_minus_99 <- any(df[[col]] == -99, na.rm = TRUE)
    has_na <- any(is.na(df[[col]]))

    if (has_minus_99) {
      special_value_cols <- c(special_value_cols, col)
      if (verbose) {
        message(sprintf("Column %s contains %d values of -99 and %d NA values",
                        col, sum(df[[col]] == -99, na.rm = TRUE), sum(is.na(df[[col]]))))
      }
    } else if (has_na && verbose) {
      message(sprintf("Column %s contains %d NA values but no -99 values",
                      col, sum(is.na(df[[col]]))))
    }
  }

  if (length(special_value_cols) > 0 && verbose) {
    message(sprintf("\nFound -99 values in %d columns:", length(special_value_cols)))
    message(paste(special_value_cols, collapse=", "))
  } else if (verbose) {
    message("\nNo -99 values found in any columns. Check for NAs instead.")
  }

  return(invisible(special_value_cols))
}

#' Get field value range from NDA elements
#'
#' @param field_name Name of the field
#' @param elements NDA data elements
#' @return Value range string or "No range specified"
#' @noRd
get_field_value_range <- function(field_name, elements) {
  if (is.null(elements) || nrow(elements) == 0) {
    return("No range specified")
  }
  
  # Find the field in elements
  field_idx <- which(elements$name == field_name)
  if (length(field_idx) == 0) {
    return("No range specified")
  }
  
  # Get the value range
  value_range <- elements$valueRange[field_idx[1]]
  if (is.null(value_range) || is.na(value_range) || value_range == "") {
    return("No range specified")
  }
  
  return(as.character(value_range))
}

#' Check if two value ranges are compatible for renaming
#'
#' @param source_range Value range of source field
#' @param target_range Value range of target field
#' @param source_field Name of source field
#' @param target_field Name of target field
#' @param verbose Whether to print verbose output
#' @return List with compatibility check results
#' @noRd
check_value_range_compatibility <- function(source_range, target_range, source_field, target_field, verbose = FALSE) {
  
  # If either range is missing, they're compatible
  if (source_range == "No range specified" || target_range == "No range specified") {
    return(list(
      compatible = TRUE,
      message = "One or both fields have no value range specified",
      recommendation = "Proceed with caution - ranges cannot be validated"
    ))
  }
  
  # If ranges are identical, they're compatible
  if (source_range == target_range) {
    return(list(
      compatible = TRUE,
      message = "Value ranges are identical",
      recommendation = "Safe to rename"
    ))
  }
  
  # Parse the ranges to extract numeric ranges and special codes
  source_parts <- parse_value_range(source_range)
  target_parts <- parse_value_range(target_range)
  
  # Check if both have numeric ranges
  if (!is.null(source_parts$numeric_range) && !is.null(target_parts$numeric_range)) {
    source_min <- source_parts$numeric_range$min
    source_max <- source_parts$numeric_range$max
    target_min <- target_parts$numeric_range$min
    target_max <- target_parts$numeric_range$max
    
    # Check if ranges overlap significantly
    overlap_min <- max(source_min, target_min)
    overlap_max <- min(source_max, target_max)
    overlap_size <- max(0, overlap_max - overlap_min + 1)
    
    source_size <- source_max - source_min + 1
    target_size <- target_max - target_min + 1
    
    # Calculate overlap percentage
    overlap_percentage <- overlap_size / min(source_size, target_size) * 100
    
    if (overlap_percentage >= 80) {
      return(list(
        compatible = TRUE,
        message = sprintf("Numeric ranges overlap by %.1f%% - likely compatible", overlap_percentage),
        recommendation = "Proceed with rename"
      ))
    } else if (overlap_percentage >= 50) {
      return(list(
        compatible = FALSE,
        message = sprintf("Numeric ranges overlap by only %.1f%% - potential conflict", overlap_percentage),
        recommendation = "Consider creating a new field instead of renaming"
      ))
    } else {
      return(list(
        compatible = FALSE,
        message = sprintf("Numeric ranges have minimal overlap (%.1f%%) - likely incompatible", overlap_percentage),
        recommendation = "Create a new field instead of renaming"
      ))
    }
  }
  
  # Handle cases where we have only special codes (no numeric range)
  # Special codes (like 888, 999, -777) are placeholders and don't affect compatibility
  source_missing <- source_parts$missing_codes
  target_missing <- target_parts$missing_codes
  
  # If both have missing codes, they're compatible (special codes are placeholders)
  if (!is.null(source_missing) && !is.null(target_missing)) {
    return(list(
      compatible = TRUE,
      message = "Both fields have special codes (placeholders) - compatible",
      recommendation = "Proceed with rename"
    ))
  }
  
  # If one has missing codes and the other doesn't, that's compatible
  # Special codes are placeholders and don't restrict the valid data range
  if ((!is.null(source_missing) && is.null(target_missing)) || 
      (is.null(source_missing) && !is.null(target_missing))) {
    return(list(
      compatible = TRUE,
      message = "Special codes are placeholders and don't affect compatibility",
      recommendation = "Proceed with rename"
    ))
  }
  
  # If neither has numeric ranges nor special codes, they're compatible
  if (is.null(source_parts$numeric_range) && is.null(target_parts$numeric_range) &&
      is.null(source_missing) && is.null(target_missing)) {
    return(list(
      compatible = TRUE,
      message = "Neither field has defined ranges - compatible",
      recommendation = "Proceed with rename"
    ))
  }
  
  # If we get here, ranges are different and potentially incompatible
  return(list(
    compatible = FALSE,
    message = "Value ranges are significantly different",
    recommendation = "Create a new field instead of renaming to avoid data loss"
  ))
}

#' Parse a value range string to extract numeric ranges and missing codes
#'
#' @param value_range Value range string (e.g., "0::6;999=missing")
#' @return List with parsed components
#' @noRd
parse_value_range <- function(value_range) {
  if (is.null(value_range) || is.na(value_range) || value_range == "") {
    return(list(numeric_range = NULL, missing_codes = NULL))
  }
  
  # Split by semicolon
  parts <- trimws(strsplit(value_range, ";")[[1]])
  
  numeric_range <- NULL
  missing_codes <- NULL
  
  for (part in parts) {
    if (grepl("::", part)) {
      # This is a numeric range (e.g., "0::6")
      range_parts <- strsplit(part, "::")[[1]]
      if (length(range_parts) == 2) {
        min_val <- as.numeric(range_parts[1])
        max_val <- as.numeric(range_parts[2])
        if (!is.na(min_val) && !is.na(max_val)) {
          numeric_range <- list(min = min_val, max = max_val)
        }
      }
    } else if (grepl("=", part)) {
      # This is a missing code with meaning (e.g., "999=missing")
      code_parts <- strsplit(part, "=")[[1]]
      if (length(code_parts) == 2) {
        code <- trimws(code_parts[1])
        meaning <- trimws(code_parts[2])
        if (grepl("missing|na|not.*applicable", meaning, ignore.case = TRUE)) {
          if (is.null(missing_codes)) missing_codes <- character(0)
          missing_codes <- c(missing_codes, code)
        }
      }
    } else {
      # This is a standalone special code (e.g., "888", "-777", "999")
      # Check if it's a numeric value that could be a special code
      if (grepl("^-?\\d+$", part)) {
        if (is.null(missing_codes)) missing_codes <- character(0)
        missing_codes <- c(missing_codes, part)
      }
    }
  }
  
  return(list(numeric_range = numeric_range, missing_codes = missing_codes))
}


