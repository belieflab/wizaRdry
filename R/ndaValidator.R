
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
      cat(sprintf("\n- %s → %s", transform$from, transform$to))
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
        print(rules)
        
        null_placeholder <- as.numeric(rules[[1]])
        
        cat(sprintf("Using placeholder value: %s\n", null_placeholder))
        
        # Add debugging before conversion
        cat(sprintf("\nUnique values before conversion in %s:\n", field_name))
        print(unique(df[[field_name]]))
        
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
            print(unique(non_numeric))
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
            print(head(df[[field_name]][new_nas]))
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
                  cat(sprintf("\n    %.2f → %d", 
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
      cat(sprintf("\n- %s → %s", field, conversion_summary[[field]]$type))
      if(conversion_summary[[field]]$nas_introduced > 0) {
        cat(sprintf(" (%d NAs)", conversion_summary[[field]]$nas_introduced))
      }
    }
    cat("\n")
  }
  
  return(df)
}

# Demonstrate with standardize_dates as well
standardize_dates <- function(df, date_cols = c("interview_date"), verbose = TRUE, limited_dataset = limited_dataset) {
  date_summary <- list()
  
  for (col in date_cols) {
    if (col %in% names(df)) {
      tryCatch({
        if(verbose) cat(sprintf("\n\nField: %s", col))
        
        # Store original values
        orig_dates <- head(df[[col]], 3)
        
        # Remove timezone information
        dates <- gsub("\\s+\\d{2}:\\d{2}:\\d{2}.*$", "", df[[col]])
        
        # Try different date formats
        date_formats <- c(
          "%Y-%m-%d",    # 2023-12-31
          "%m/%d/%Y",    # 12/31/2023
          "%Y/%m/%d",    # 2023/12/31
          "%d-%m-%Y",    # 31-12-2023
          "%m-%d-%Y"     # 12-31-2023
        )
        
        success <- FALSE
        for (format in date_formats) {
          parsed_dates <- tryCatch({
            as.Date(dates, format = format)
          }, error = function(e) NULL)
          
          if (!is.null(parsed_dates) && !all(is.na(parsed_dates))) {
            if(verbose) cat(sprintf("\n  Detected format: %s", format))
            # df[[col]] <- format(parsed_dates, "%Y-%m-%d")
            #df[[col]] <- format(parsed_dates, "%m/%d/%Y")
            # Perform interview_date date shifting to created de-identified dataset
            df[[col]] <- format(parsed_dates, ifelse(limited_dataset, "%m/%d/%Y", "%m/01/%Y"))
            
            success <- TRUE
            
            date_summary[[col]] <- list(
              original_format = format,
              sample_before = orig_dates,
              sample_after = head(df[[col]], 3)
            )
            break
          }
        }
        
        if(!success && verbose) {
          cat(sprintf("\n  Warning: Could not determine date format"))
          cat(sprintf("\n  Sample values: %s", 
                      paste(head(dates), collapse=", ")))
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
      cat(sprintf("\n  Before: %s", 
                  paste(date_summary[[field]]$sample_before, collapse=", ")))
      cat(sprintf("\n  After:  %s", 
                  paste(date_summary[[field]]$sample_after, collapse=", ")))
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
    "False" = "0"
  )
  
  # Convert value to character without changing case
  value <- as.character(value)
  
  # Map values using the binary_map (exact match)
  mapped_values <- binary_map[value]
  
  # For any unmatched values, try lowercase matching
  still_na <- is.na(mapped_values)
  if(any(still_na)) {
    mapped_values[still_na] <- binary_map[tolower(value[still_na])]
  }
  
  # Keep original values for any remaining unmatched
  mapped_values[is.na(mapped_values)] <- value[is.na(mapped_values)]
  
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
fetch_structure_elements <- function(structure_name, api_base_url) {
  
  if (!require(httr)) {install.packages("httr")}; library(httr)
  if (!require(jsonlite)) {install.packages("jsonlite")}; library(jsonlite)
  
  url <- sprintf("%s/datastructure/%s", api_base_url, structure_name)
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
find_and_rename_fields <- function(df, elements, structure_name, verbose = TRUE) {
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
          if(verbose) {
            message(sprintf("\nRENAMING: '%s' to '%s' (similarity: %.2f%%)\n",
                          field, best_match, best_score * 100))
          }
          
          # Add the new column with renamed data
          renamed$df[[best_match]] <- renamed$df[[field]]
          
          # Mark original column for dropping
          renamed$columns_to_drop <- c(renamed$columns_to_drop, field)
          
          # Store the rename operation
          renamed$renames <- c(renamed$renames,
                             sprintf("%s -> %s (%.2f%%)",
                                   field, best_match, best_score * 100))
        } else if(verbose) {
          cat(sprintf("No automatic rename - best match below 90%% threshold\n"))
        }
      }
    }
    
    # Drop original columns after all renames
    if(length(renamed$columns_to_drop) > 0) {
      if(verbose) {
        cat("\nDropping original columns:")
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
validate_structure <- function(df, elements, measure_name, verbose = FALSE) {
  if(verbose) cat("\nValidating data structure...")
  
  results <- list(
    valid = TRUE,
    missing_required = character(0),
    value_range_violations = list(),
    unknown_fields = character(0),
    warnings = character(0)
  )
  
  tryCatch({
    # Get field lists
    required_fields <- elements$name[elements$required == "Required"]
    valid_fields <- elements$name
    df_cols <- names(df)
    
    # # Check for and DROP unknown fields    
    # results$unknown_fields <- setdiff(df_cols, valid_fields)
    # if(length(results$unknown_fields) > 0) {
    #   if(verbose) {
    #     cat("\n\nUnknown fields detected:")
    #     cat(sprintf("\n  %s", paste(results$unknown_fields, collapse=", ")))
    #   }
    #   df <- df[, !names(df) %in% results$unknown_fields]
    #   results$unknown_fields <- character(0)
    #   if(verbose) {
    #     cat("\nDropped unknown fields from dataframe")
    #   }
    #   assign(measure_name, df, envir = .GlobalEnv)
    # }
    
    # Check for unknown fields
    results$unknown_fields <- setdiff(df_cols, valid_fields)
    if(length(results$unknown_fields) > 0) {
      if(verbose) {
        cat("\n\nUnknown fields detected:")
        cat(sprintf("\n  %s", paste(results$unknown_fields, collapse=", ")))
      }
      # No longer dropping fields here
      results$valid <- FALSE  # Keep failing validation if unknown fields exist
    }
    
    # Update field lists after renaming
    df_cols <- names(df)  # Get updated column names
    required_fields <- setdiff(required_fields, df_cols)  # Only keep the ones that are still missing
    
    # Check required fields 
    if(length(required_fields) > 0) {
      missing_required <- required_fields  # These are already the missing ones now
      if(length(missing_required) > 0) {
        results$valid <- FALSE
        results$missing_required <- missing_required
        if(verbose) {
          cat("\n\nMissing required fields:")
          cat(sprintf("\n  %s", paste(missing_required, collapse=", ")))
        }
      } else if(verbose) {
        cat("\n\nAll required fields present")
      }
    }
    
    # Rest of validation code...
    
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
          if(any(tolower(values) %in% c("true", "false"))) {
            if(verbose) cat("\n  Converting boolean values to 0/1")
            df[[col]] <- standardize_binary(values)
            assign(measure_name, df, envir = .GlobalEnv)
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
          results$valid <- FALSE
          results$value_range_violations[[col]] <- list(
            expected = element$valueRange,
            actual = violating_values
          )
          
          if(verbose) {
            cat("\n  Value range violations found:")
            cat(sprintf("\n    Invalid values: %s", 
                        paste(head(violating_values, 5), collapse=", ")))
            if(length(violating_values) > 5) {
              cat(sprintf(" (and %d more...)", 
                          length(violating_values) - 5))
            }
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
        message(sprintf("- Unknown fields: %d", 
                        length(results$unknown_fields)))
      }
      
      if(length(results$missing_required) > 0) {
        message(sprintf("- Missing required fields: %d", 
                        length(results$missing_required)))
      }
      
      if(length(results$value_range_violations) > 0) {
        message(sprintf("- Fields with range violations: %d", 
                        length(results$value_range_violations)))
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
      print(head(df, sample_size))
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
    # Convert to lowercase
    name <- tolower(name)
    # Replace hyphens with underscores
    name <- gsub("-", "_", name)
    # Handle prefix if present
    if (grepl(paste0("^", prefix, "[_-]?\\d+$"), name)) {
      # Ensure consistent underscore between prefix and number
      name <- gsub(paste0("^(", prefix, ")[_-]?(\\d+)$"), "\\1_\\2", name)
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
      cat(sprintf("\n  %s → %s", old_names[i], new_names[i]))
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
transform_value_ranges <- function(df, elements, verbose = FALSE) {
  if(verbose) cat("\nChecking and transforming value ranges...")
  range_summary <- list()
  
  # Check which columns are required
  required_fields <- elements$name[elements$required == "Required"]
  
  # MODIFIED: More robust check for missing/NA values in required fields
  missing_required <- FALSE
  missing_fields <- character(0)
  
  for(field in required_fields) {
    if(field %in% names(df)) {
      if(any(is.na(df[[field]]) | df[[field]] == "")) {
        missing_required <- TRUE
        missing_fields <- c(missing_fields, field)
      }
    } else {
      missing_required <- TRUE
      missing_fields <- c(missing_fields, field)
    }
  }
  
  if(missing_required) {
    stop(sprintf('\nNDA required values contain NA or no data in fields: %s\nPlease update ndar_subject01 values to make sure no data is missing', 
                 paste(missing_fields, collapse=", ")))
  }
  
  # Rest of the function remains the same
  # Only check non-required columns for emptiness e.g."rt" on eefrt  
  empty_cols <- sapply(df[, !names(df) %in% required_fields], function(col) all(is.na(col) | col == ""))
  if (any(empty_cols)) {
    empty_col_names <- names(empty_cols)[empty_cols]
    if(verbose) {
      cat("\n\nEmpty columns detected:")
      cat(sprintf("\n  Dropping: %s", paste(empty_col_names, collapse=", ")))
    }
    df <- df[, !names(df) %in% empty_col_names, drop=FALSE]
  }
  
  # Process binary fields first
  binary_fields <- elements$name[!is.na(elements$valueRange) & elements$valueRange == "0;1"]
  if (length(binary_fields) > 0) {
    if(verbose) cat("\n\nProcessing binary fields (0;1)...")
    
    for (field in binary_fields) {
      if (field %in% names(df)) {
        values <- as.character(df[[field]])
        potential_booleans <- c("true", "false", "t", "f", "TRUE", "FALSE", "True", "False")
        if (any(values %in% potential_booleans, na.rm = TRUE)) {
          if(verbose) {
            cat(sprintf("\n\nField: %s", field))
            cat("\n  Converting boolean values to 0/1")
          }
          
          # Store original values
          orig_values <- unique(values)
          
          # Transform values 
          df[[field]] <- standardize_binary(values)
          
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
  
  # Process fields that have value range rules
  for (i in 1:nrow(elements)) {
    field_name <- elements$name[i]
    value_range <- elements$valueRange[i]
    if (field_name %in% names(df) && !is.na(value_range) && value_range != "" && 
        value_range != "0;1" && grepl(";", value_range)) {
      
      if(verbose) {
        cat(sprintf("\n\nField: %s", field_name))
        cat(sprintf("\n  Expected values: %s", value_range))
      }
      
      # Store original values
      orig_values <- unique(df[[field_name]])
      
      # Get expected values and standardize
      expected_values <- trimws(unlist(strsplit(value_range, ";")))
      current_values <- df[[field_name]]
      transformed_count <- 0
      
      # Special handling for handedness
      if(field_name == "handedness") {
        df[[field_name]] <- standardize_handedness(current_values)
        transformed_count <- sum(current_values != df[[field_name]], na.rm = TRUE)
      } else {
        # Map case-insensitive matches for other fields
        for (exp_val in expected_values) {
          matches <- tolower(current_values) == tolower(exp_val) 
          if (any(matches)) {
            transformed_count <- transformed_count + sum(matches)
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
  
  return(df)
}

# Modified ndaValidator to include column name standardization
ndaValidator <- function(measure_name,
                         source,
                         limited_dataset = FALSE,
                         api_base_url = "https://nda.nih.gov/api/datadictionary/v2",
                         verbose = TRUE,
                         debug = FALSE) {
  tryCatch({
    # Get the dataframe from the global environment
    df <- base::get(measure_name, envir = .GlobalEnv)
    debug_print("Initial dataframe loaded", df, debug = debug)
    
    # Get structure name
    structure_name <- measure_name
    
    # Add explicit date standardization step to make data de-identified
    df <- standardize_dates(df, verbose = verbose, limited_dataset = limited_dataset)
    
    # Add explicit age standardization step to make data de-identified
    df <- standardize_age(df, verbose = verbose, limited_dataset = limited_dataset)
    
    # Standardize column names based on structure
    df <- standardize_column_names(df, structure_name, verbose = verbose)
    
    # Add field name standardization
    df <- standardize_field_names(df, measure_name, verbose = verbose)
    
    # Save standardized dataframe back to global environment
    assign(measure_name, df, envir = .GlobalEnv)
    
    # Continue with structure fetching and validation...
    message("\n\nFetching ", structure_name, " Data Structure from NDA API...")
    elements <- fetch_structure_elements(structure_name, api_base_url)
    
    if (is.null(elements) || nrow(elements) == 0) {
      stop("No elements found in the structure definition")
    }
    
    # ADD THE BLOCK HERE - BEFORE ANY OTHER TRANSFORMATIONS
    # Check for required fields that are missing and add them
    required_fields <- elements$name[elements$required == "Required"]
    missing_required <- required_fields[!required_fields %in% names(df)]
    if(length(missing_required) > 0) {
      df <- handle_missing_fields(df, elements, missing_required, verbose = TRUE)
      assign(measure_name, df, envir = .GlobalEnv)
    }
    
    # Then continue with the rest of the transformations
    renamed_results <- find_and_rename_fields(df, elements, structure_name, verbose)
    df <- renamed_results$df
    
    # Process the dataframe with additional error handling
    df <- tryCatch({
      # Apply type conversions
      df <- apply_type_conversions(df, elements, verbose = verbose)
      
      # Apply null transformations
      df <- apply_null_transformations(df, elements, verbose = verbose)
      
      # Transform value ranges
      df <- transform_value_ranges(df, elements, verbose = verbose)
      
      df
    }, error = function(e) {
      stop(sprintf("Error processing dataframe: %s", e$message))
    })
    
    # Save processed dataframe back to global environment
    assign(measure_name, df, envir = .GlobalEnv)
    
    # Check and add any missing required fields BEFORE validation
    required_fields <- elements$name[elements$required == "Required"]
    missing_required <- required_fields[!required_fields %in% names(df)]
    if(length(missing_required) > 0) {
      df <- handle_missing_fields(df, elements, missing_required, verbose = TRUE)
      assign(measure_name, df, envir = .GlobalEnv)
    }
    
    # Now validate the complete dataset
    validation_results <- validate_structure(df, elements, measure_name, verbose = verbose)
    return(validation_results)
    
  }, error = function(e) {
    message("Error: ", e$message)
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
              cat(sprintf("\n    %s → %s", rule_name, rules[[rule_name]]))
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
