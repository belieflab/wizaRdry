
# Safe readline function that works in non-interactive environments too
safe_readline <- function(prompt = "", default = "y") {
  tryCatch({
    if (interactive()) {
      result <- suppressWarnings(readline(prompt))
      if (result == "") return(default)
      return(result)
    } else {
      # In non-interactive mode, just return the default
      message(paste0(prompt, " (Using default: ", default, ")"))
      return(default)
    }
  }, error = function(e) {
    # If there's any error, return the default
    message(paste0("Error in readline: ", e$message, " (Using default: ", default, ")"))
    return(default)
  })
}

# Function to handle missing required fields
handle_missing_fields <- function(df, elements, missing_required, verbose = FALSE) {
  if(verbose) {
    message("\nAuto-adding missing required fields with missing value codes:")
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
get_mapping_rules <- function(notes) {
  if (is.null(notes) || is.na(notes) || notes == "") return(NULL)
  
  rules <- list()
  
  tryCatch({
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
            rules[[values]] <- code
          }
        }
      }
    }
    
    # Handle simple mappings like "1=Red" and "NaN=-1"
    if (grepl("[^=]+=[^;]+", notes)) {
      patterns <- strsplit(notes, ";\\s*")[[1]]
      for (pattern in patterns) {
        if (grepl("=", pattern)) {
          parts <- strsplit(pattern, "=")[[1]]
          if (length(parts) >= 2) {  # Check if we have both parts
            value <- trimws(parts[1])
            code <- trimws(parts[2])
            rules[[code]] <- value
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

# semi-generalizable e.g. handle mooney rt null
apply_null_transformations <- function(df, elements) {
  for (i in 1:nrow(elements)) {
    field_name <- elements$name[i]
    type <- elements$type[i]
    notes <- elements$notes[i]
    
    if (field_name %in% names(df) && !is.null(notes)) {
      # Extract transformation rules from Notes
      rules <- get_mapping_rules(notes)
      
      if (!is.null(rules) && length(rules) > 0) {
        cat(sprintf("\nRules for field '%s':\n", field_name))
        message(rules)
        
        null_placeholder <- as.numeric(rules[[1]])
        
        cat(sprintf("Using placeholder value: %s\n", null_placeholder))
        
        # Add debugging before conversion
        cat(sprintf("\nUnique values before conversion in %s:\n", field_name))
        message(unique(df[[field_name]]))
        
        message("applying type conversions")
        
        df[[field_name]] <- as.character(df[[field_name]])
        
        null_mask <- df[[field_name]] %in% c("null", "NaN", "") | is.na(df[[field_name]])
        df[[field_name]][null_mask] <- null_placeholder
        
        # Add debugging for type conversion
        if (type == "Integer" || type == "Float") {
          cat(sprintf("\nConverting %s to %s\n", field_name, type))
          # Check for problematic values before conversion
          non_numeric <- df[[field_name]][!grepl("^-?\\d*\\.?\\d+$", df[[field_name]])]
          if (length(non_numeric) > 0) {
            cat(sprintf("Warning: Non-numeric values found in %s:\n", field_name))
            message(unique(non_numeric))
          }
          
          if (type == "Integer") {
            df[[field_name]] <- as.integer(df[[field_name]])
          } else if (type == "Float") {
            df[[field_name]] <- as.numeric(df[[field_name]])
          }
          
          # Check for NAs after conversion
          new_nas <- is.na(df[[field_name]])
          if (any(new_nas)) {
            cat(sprintf("\nWarning: %d NAs introduced in %s\n", sum(new_nas), field_name))
            cat("Sample of values that became NA:\n")
            message(head(df[[field_name]][new_nas]))
          }
        }
        
        cat(sprintf("Values after transformation: %s\n", 
                    paste(unique(df[[field_name]]), collapse = ", ")))
      }
    }
  }
  return(df)
}



# Convert fields to their proper type based on NDA definition
# Modify the apply_type_conversions function to be more robust
# Convert fields to proper type based on NDA definition
apply_type_conversions <- function(df, elements, verbose = FALSE) {
  if(verbose) cat("\nApplying type conversions...")
  conversion_summary <- list()
  
  for (i in 1:nrow(elements)) {
    field_name <- elements$name[i]
    type <- elements$type[i]
    
    if (field_name %in% names(df) && !is.null(type)) {
      tryCatch({
        if (type %in% c("Integer", "Float")) {
          if(verbose) cat(sprintf("\n\nField: %s", field_name))
          if(verbose) cat(sprintf("\n  Target type: %s", type))
          
          # Store original values for comparison
          orig_values <- head(df[[field_name]], 3)
          
          # First convert to character
          df[[field_name]] <- as.character(df[[field_name]])
          
          # Remove currency symbols, commas, etc
          df[[field_name]] <- gsub("[^0-9.-]", "", df[[field_name]])
          
          if (type == "Integer") {
            # Convert to numeric first to handle decimals
            df[[field_name]] <- as.numeric(df[[field_name]])
            
            # Check for decimal values
            float_mask <- !is.na(df[[field_name]]) & 
              abs(df[[field_name]] - floor(df[[field_name]])) > 0
            
            if (any(float_mask)) {
              float_count <- sum(float_mask)
              if(verbose) {
                cat(sprintf("\n  Found %d decimal values to round", float_count))
                cat("\n  Sample conversions:")
                float_examples <- head(df[[field_name]][float_mask])
                rounded_examples <- round(float_examples)
                for(j in seq_along(float_examples)) {
                  cat(sprintf("\n    %.2f -> %d", 
                              float_examples[j], 
                              rounded_examples[j]))
                }
              }
              df[[field_name]] <- round(df[[field_name]])
            }
            
            df[[field_name]] <- as.integer(df[[field_name]])
            
          } else if (type == "Float") {
            df[[field_name]] <- as.numeric(df[[field_name]])
          }
          
          # Check for NAs after conversion
          na_count <- sum(is.na(df[[field_name]]))
          if (na_count > 0 && verbose) {
            cat(sprintf("\n  Warning: %d NA values introduced", na_count))
            cat("\n  Sample values that became NA:")
            na_mask <- is.na(df[[field_name]])
            cat(sprintf("\n    Original: %s", 
                        paste(head(orig_values[na_mask]), collapse=", ")))
          }
          
          # Store summary for this field
          conversion_summary[[field_name]] <- list(
            type = type,
            nas_introduced = na_count,
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
  
  if(verbose && length(conversion_summary) > 0) {
    cat("\n\nType conversion summary:")
    for(field in names(conversion_summary)) {
      cat(sprintf("\n- %s -> %s", field, conversion_summary[[field]]$type))
      if(conversion_summary[[field]]$nas_introduced > 0) {
        cat(sprintf(" (%d NAs)", conversion_summary[[field]]$nas_introduced))
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

# Calculate Levenshtein distance similarity between two strings
calculate_similarity <- function(str1, str2) {
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
find_and_rename_fields <- function(df, elements, structure_name, verbose = TRUE, 
                                   auto_drop_unknown = FALSE,
                                   interactive_mode = TRUE) {
  renamed <- list(
    df = df,
    renames = character(),
    columns_to_drop = character(),
    similarity_scores = list()
  )
  
  # Get dataframe column names
  df_cols <- names(df)
  valid_fields <- elements$name
  
  # Get structure short name by taking last 2 digits of structure_name
  structure_prefix <- substr(structure_name, nchar(structure_name) - 1, nchar(structure_name))
  
  # Find unknown fields
  unknown_fields <- setdiff(df_cols, valid_fields)
  
  if (length(unknown_fields) > 0) {
    if(verbose) cat("\nAnalyzing field name similarities...\n")
    
    for (field in unknown_fields) {
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
        # Remove prefix from target field if it exists
        target_base <- sub(paste0("^", structure_prefix, "_"), "", name)
        
        # Calculate direct similarity
        calculate_similarity(field, name)
      })
      
      # Store all similarity scores
      renamed$similarity_scores[[field]] <- sort(similarities, decreasing = TRUE)
      
      if(verbose) {
        cat(sprintf("\nField: %s\n", field))
        cat("Top matches:\n")
        top_matches <- head(sort(similarities, decreasing = TRUE), 3)
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
        
        if (best_score > 0.9) {  # Increased threshold for more conservative matching
          # Present option to rename in interactive mode
          rename_field <- TRUE
          
          if(interactive_mode) {
            rename_input <- safe_readline(prompt = sprintf("Rename '%s' to '%s' (similarity: %.2f%%)? (y/n): ", 
                                                           field, best_match, best_score * 100), default = "y")
            rename_field <- tolower(rename_input) %in% c("y", "yes")
          }
          
          if(rename_field) {
            if(verbose) {
              message(sprintf("\nRENAMING: '%s' to '%s' (similarity: %.2f%%)\n",
                              field, best_match, best_score * 100))
            }
            
            # Add the new column with renamed data
            renamed$df[[best_match]] <- df[[field]]
            
            # Mark original column for dropping
            renamed$columns_to_drop <- c(renamed$columns_to_drop, field)
            
            # Store the rename operation
            renamed$renames <- c(renamed$renames,
                                 sprintf("%s -> %s (%.2f%%)",
                                         field, best_match, best_score * 100))
          }
        } else {
          if(verbose) {
            cat(sprintf("No automatic rename - best match below 90%% threshold\n"))
          }
          
          # Mark this as a column to drop if auto_drop_unknown is TRUE
          drop_field <- auto_drop_unknown
          
          if(interactive_mode) {
            drop_input <- safe_readline(prompt = sprintf("Drop field '%s'? (y/n): ", field), default = if(auto_drop_unknown) "y" else "n")
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
        # No matches found
        drop_field <- auto_drop_unknown
        
        if(interactive_mode) {
          drop_input <- safe_readline(prompt = sprintf("No matches found for '%s'. Drop this field? (y/n): ", field), default = if(auto_drop_unknown) "y" else "n")
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
    
    # Drop original columns after all renames
    if(length(renamed$columns_to_drop) > 0) {
      if(verbose) {
        cat("\nDropping columns:")
        cat(sprintf("\n  %s", paste(renamed$columns_to_drop, collapse=", ")))
      }
      renamed$df <- renamed$df[, !names(renamed$df) %in% renamed$columns_to_drop]
    }
    
    if(verbose && length(renamed$renames) > 0) {
      cat("\n\nRename operations completed:")
      cat(paste("\n-", renamed$renames), sep = "")
      cat("\n")
    }
  }
  
  return(renamed)
}

# Helper function to get violating values with type conversion
# Updated get_violations function with more robust categorical matching
# Updated get_violations function
# Updated get_violations function with special handling for ranges with both :: and ;
get_violations <- function(value, range_str) {
  if (is.null(range_str) || is.na(range_str) || range_str == "") return(character(0))
  
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
    
    # Check for violations
    invalid_mask <- !value %in% valid_values
    invalid_mask[is.na(invalid_mask)] <- FALSE
    
    return(sort(unique(value[invalid_mask])))
  }
  
  # Rest of the function for simple ranges
  if (grepl("::", range_str)) {
    # Numeric range check
    range <- as.numeric(strsplit(range_str, "::")[[1]])
    
    # Convert value to numeric if it's character
    if (is.character(value)) {
      value <- as.numeric(value)
    }
    
    invalid_mask <- value < range[1] | value > range[2]
    invalid_mask[is.na(invalid_mask)] <- FALSE
    return(sort(unique(value[invalid_mask])))
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
    unknown_fields = character(0),
    unknown_fields_dropped = character(0),  # Track which fields were dropped
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
        if(exists(measure_name, envir = globalenv())) {
          env_to_use <- globalenv()
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
        
        # Ask if validation should fail due to unknown fields
        fail_validation <- FALSE
        
        if(interactive_mode) {
          fail_input <- safe_readline(prompt = "Should validation fail because of unknown fields? (y/n): ", 
                                      default = "n")
          fail_validation <- tolower(fail_input) %in% c("y", "yes")
        } else {
          fail_validation <- TRUE  # Always fail in non-interactive mode when fields aren't dropped
        }
        
        if(fail_validation) {
          results$valid <- FALSE
          if(verbose) cat("\nValidation will fail due to unknown fields")
        } else {
          if(verbose) cat("\nValidation will proceed despite unknown fields")
        }
      }
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
    
    # Check value ranges
    if(verbose) cat("\n\nChecking value ranges...")
    
    for(col in intersect(df_cols, valid_fields)) {
      element <- elements[elements$name == col, ]
      
      if(nrow(element) > 0 &&
         !is.null(element$valueRange) &&
         !is.na(element$valueRange) &&
         element$valueRange != "") {
        
        if(verbose) {
          cat(sprintf("\n\nField: %s", col))
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
            if(exists(measure_name, envir = globalenv())) {
              env_to_use <- globalenv()
            } else if(exists(".wizaRdry_env") && exists(measure_name, envir = .wizaRdry_env)) {
              env_to_use <- .wizaRdry_env
            }
            if(!is.null(env_to_use)) {
              assign(measure_name, df, envir = env_to_use)
            }
          }
        }
        
        # Check for violations
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
            cat("\n  Value range violations found:")
            cat(sprintf("\n    Invalid values: %s",
                        paste(head(violating_values, 5), collapse=", ")))
            if(length(violating_values) > 5) {
              cat(sprintf(" (and %d more...)",
                          length(violating_values) - 5))
            }
          }
          
          # Ask if these violations should cause validation to fail
          fail_due_to_violations <- TRUE
          
          if(interactive_mode) {
            violation_input <- safe_readline(prompt = sprintf("Should violations in field '%s' cause validation to fail? (y/n): ", col), default = "y")
            fail_due_to_violations <- tolower(violation_input) %in% c("y", "yes")
          }
          
          if(fail_due_to_violations) {
            results$valid <- FALSE
            results$value_range_violations[[col]] <- list(
              expected = element$valueRange,
              actual = violating_values
            )
            if(verbose) cat(sprintf("\n  Validation will fail due to violations in %s", col))
          } else {
            if(verbose) cat(sprintf("\n  Ignoring violations in %s for validation purposes", col))
          }
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
        missing_values <- is.na(df[[field]]) | df[[field]] == ""
      } else {
        missing_values <- is.na(df[[field]])
      }
      
      if(any(missing_values)) {
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

# Modified ndaValidator to include column name standardization
# new structure based on observations from Stanford
ndaValidator <- function(measure_name,
                         api,
                         limited_dataset = FALSE,
                         nda_base_url = "https://nda.nih.gov/api/datadictionary/v2",
                         verbose = TRUE,
                         debug = FALSE,
                         auto_drop_unknown = FALSE,
                         interactive_mode = TRUE) {
  
  tryCatch({
    # Initialize a list to track all columns to be removed
    all_columns_to_drop <- character(0)
    
    if (!exists(".wizaRdry_env")) {
      .wizaRdry_env <- new.env(parent = globalenv())
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
    
    # Get structure name and fetch elements
    structure_name <- measure_name
    message("\n\nFetching ", structure_name, " Data Structure from NDA API...")
    elements <- fetch_structure_elements(structure_name, nda_base_url)
    
    if (is.null(elements) || nrow(elements) == 0) {
      stop("No elements found in the structure definition")
    }
    
    # PHASE 1: NA Value Mapping
    if(verbose) message("\n\n--- PHASE 1: NA Value Mapping ---")
    
    # Apply null transformations first
    df <- apply_null_transformations(df, elements, verbose = verbose)
    
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
    
    # Rename fields with close matches - track columns to drop
    renamed_results <- find_and_rename_fields(df, elements, structure_name, 
                                              verbose = verbose, 
                                              auto_drop_unknown = auto_drop_unknown, 
                                              interactive_mode = interactive_mode)
    df <- renamed_results$df
    
    # Collect columns that were dropped during renaming
    all_columns_to_drop <- c(all_columns_to_drop, renamed_results$columns_to_drop)
    
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
    
    # Apply type conversions
    df <- apply_type_conversions(df, elements, verbose = verbose)
    
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
    if(length(validation_results$unknown_fields_dropped) > 0) {
      all_columns_to_drop <- c(all_columns_to_drop, validation_results$unknown_fields_dropped)
    }
    
    # Final check for missing required values
    if(!validation_results$valid && length(validation_results$missing_required) > 0) {
      if(verbose) message("\nValidation FAILED: Required fields are missing or contain NA values")
    }
    
    # Now update the cleaning script with ALL columns to drop in one operation
    if(length(all_columns_to_drop) > 0) {
      update_cleaning_script <- FALSE
      
      if(interactive_mode) {
        update_input <- safe_readline(
          prompt = sprintf("Update cleaning script with code to drop %d fields? (y/n): ", 
                           length(all_columns_to_drop)), 
          default = "y")
        update_cleaning_script <- tolower(update_input) %in% c("y", "yes")
      } else {
        update_cleaning_script <- TRUE
      }
      
      if(update_cleaning_script) {
        if(verbose) {
          cat("\nUpdating cleaning script with all fields to drop:")
          cat(sprintf("\n  %s", paste(all_columns_to_drop, collapse=", ")))
        }
        
        # Update the cleaning script
        update_result <- tryCatch({
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
            unknown_fields_str <- paste(shQuote(unique(all_columns_to_drop)), collapse=", ")
            
            removal_code <- c(
              "",
              "# Auto-generated code to remove unknown fields",
              paste0(measure_name_str, " <- ", measure_name_str, "[, !names(",
                     measure_name_str, ") %in% c(",
                     unknown_fields_str, ")]")
            )
            
            # Write back the entire file with the new code appended
            writeLines(c(existing_content, removal_code), cleaning_script_path)
            
            if(verbose) cat("\nSuccessfully updated cleaning script with code to remove unknown fields")
            TRUE
          } else {
            if(verbose) {
              cat("\nCould not find cleaning script at expected locations.")
              unknown_fields_str <- paste(shQuote(unique(all_columns_to_drop)), collapse=", ")
              removal_code <- paste0(measure_name_str, " <- ", measure_name_str, "[, !names(",
                                     measure_name_str, ") %in% c(",
                                     unknown_fields_str, ")]")
              cat("\n\nSuggested code for cleaning script:")
              cat("\n", removal_code)
            }
            FALSE
          }
        }, error = function(e) {
          if(verbose) cat("\nError updating cleaning script:", e$message)
          FALSE
        })
        
        if(verbose && update_result) {
          cat("\nCleaning script updated with all", length(all_columns_to_drop), "fields to drop")
        }
      }
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

# Modified apply_null_transformations with better error handling
apply_null_transformations <- function(df, elements, verbose = FALSE) {
  if(verbose) cat("\nApplying null value transformations...")
  transform_summary <- list()
  
  for (i in 1:nrow(elements)) {
    field_name <- elements$name[i]
    type <- elements$type[i]
    notes <- elements$notes[i]
    
    if (field_name %in% names(df) && !is.null(notes)) {
      tryCatch({
        # Extract transformation rules from Notes
        rules <- get_mapping_rules(notes)
        
        if (!is.null(rules) && length(rules) > 0) {
          if(verbose) {
            cat(sprintf("\n\nField: %s", field_name))
            cat("\n  Rules found:")
            for(rule_name in names(rules)) {
              cat(sprintf("\n    %s -> %s", rule_name, rules[[rule_name]]))
            }
          }
          
          # Get placeholder value safely
          null_placeholder <- tryCatch({
            as.numeric(rules[[1]])
          }, error = function(e) {
            if(verbose) {
              cat(sprintf("\n  Warning: Could not convert placeholder to numeric"))
              cat(sprintf("\n    Error: %s", e$message))
            }
            NA
          })
          
          if(verbose) {
            cat(sprintf("\n  Using placeholder: %s", 
                        if(is.na(null_placeholder)) "NA" else null_placeholder))
          }
          
          # Store original values
          orig_values <- unique(df[[field_name]])
          
          # Convert field to character first
          df[[field_name]] <- as.character(df[[field_name]])
          
          # Apply null transformations
          null_mask <- df[[field_name]] %in% c("null", "NaN", "") | is.na(df[[field_name]])
          null_count <- sum(null_mask)
          df[[field_name]][null_mask] <- null_placeholder
          
          # Apply type conversion if needed
          if (type %in% c("Integer", "Float")) {
            if(verbose) cat(sprintf("\n  Converting to %s", type))
            
            if (type == "Integer") {
              df[[field_name]] <- as.integer(df[[field_name]])
            } else {
              df[[field_name]] <- as.numeric(df[[field_name]])
            }
          }
          
          # Store transformation summary
          transform_summary[[field_name]] <- list(
            type = type,
            nulls_transformed = null_count,
            values_before = orig_values,
            values_after = unique(df[[field_name]])
          )
          
          if(verbose && null_count > 0) {
            cat(sprintf("\n  Transformed %d null values", null_count))
            cat("\n  Value comparison:")
            cat(sprintf("\n    Before: %s", 
                        paste(head(orig_values), collapse=", ")))
            cat(sprintf("\n    After:  %s", 
                        paste(head(unique(df[[field_name]])), collapse=", ")))
          }
        }
      }, error = function(e) {
        if(verbose) {
          cat(sprintf("\n\nError processing field %s:", field_name))
          cat(sprintf("\n  %s", e$message))
        }
      })
    }
  }
  
  if(verbose && length(transform_summary) > 0) {
    cat("\n\nNull transformation summary:")
    for(field in names(transform_summary)) {
      cat(sprintf("\n- %s", field))
      if(transform_summary[[field]]$nulls_transformed > 0) {
        cat(sprintf(" (%d nulls transformed)", 
                    transform_summary[[field]]$nulls_transformed))
      }
    }
    cat("\n")
  }
  
  return(df)
}
