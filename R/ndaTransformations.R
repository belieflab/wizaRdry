#' NDA Data Transformation Functions
#'
#' @description
#' Functions for transforming data to meet NDA requirements.
#' Includes date/age standardization, type conversions, array handling,
#' and missing data code translation.
#'

#' Standardize interview_date for NDA submission
#'
#' @description
#' Converts dates to MM/DD/YYYY format. For de-identification (when limited_dataset=FALSE),
#' shifts dates to MM/01/YYYY (first of month). Handles multiple input date formats.
#'
#' @param df Data frame
#' @param date_cols Character vector of date column names (default: "interview_date")
#' @param verbose Logical - print detailed output
#' @param limited_dataset Logical - if FALSE, performs date-shifting for de-identification
#' @return Data frame with standardized dates
#' @noRd
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

#' Standardize interview_age for NDA submission
#'
#' @description
#' Caps age at 1068 months (89 years) for de-identification when limited_dataset=FALSE.
#' NDA requires age-capping for privacy protection.
#'
#' @param df Data frame
#' @param verbose Logical - print detailed output
#' @param limited_dataset Logical - if FALSE, performs age-capping
#' @return Data frame with standardized age
#' @noRd
standardize_age <- function(df, verbose = TRUE, limited_dataset = FALSE) {
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

#' Standardize handedness values to NDA format
#'
#' @description
#' Converts handedness values to standard codes: L (left), R (right), B (both/ambidextrous)
#'
#' @param value Character vector of handedness values
#' @return Character vector with standardized values
#' @noRd
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
  
  return(mapped_values)
}

#' Standardize boolean values to 0/1 format
#'
#' @description
#' Converts boolean/text values (TRUE/FALSE, yes/no, etc.) to numeric 0/1 format
#' required by NDA for binary fields.
#'
#' @param value Character or logical vector
#' @return Character vector with "0" or "1" values
#' @noRd
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

#' Convert logical columns to character
#'
#' @description
#' Helper function to convert all logical columns to character before validation.
#' Prevents type mismatch errors during NDA validation.
#'
#' @param df Data frame
#' @param verbose Logical - print detailed output
#' @return Data frame with logical columns converted to character
#' @noRd
convert_logical_to_character <- function(df, verbose = FALSE) {
  for(col in names(df)) {
    if(is.logical(df[[col]])) {
      if(verbose) cat(sprintf("\nConverting logical column %s to character", col))
      df[[col]] <- as.character(df[[col]])
    }
  }
  return(df)
}

#' Convert problematic column types to character
#'
#' @description
#' Force complex data types (POSIXct, Date, lists) to character to prevent
#' errors during validation. This is a safety function called early in validation.
#'
#' @param df Data frame
#' @param measure_name Name of measure (for error messages)
#' @param verbose Logical - print detailed output
#' @return Data frame with problematic columns converted
#' @noRd
convert_problematic_column_types <- function(df, measure_name, verbose = FALSE) {
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
  
  return(df)
}

#' Standardize column names
#'
#' @description
#' Standardizes column names by replacing hyphens with underscores and
#' ensuring consistent prefix_number format.
#'
#' @param df Data frame
#' @param structure_name NDA structure name
#' @param verbose Logical - print changes
#' @return Data frame with standardized column names
#' @noRd
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

#' Generic function for field standardization
#'
#' @description
#' Standardizes common field names (e.g., "index" -> "trial" for behavioral tasks)
#'
#' @param df Data frame
#' @param measure_name Name of measure
#' @param verbose Logical - print transformations
#' @return Data frame with standardized field names
#' @noRd
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
