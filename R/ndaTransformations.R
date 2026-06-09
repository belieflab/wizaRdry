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
#' NDA requires age-capping for privacy protection per HIPAA Safe Harbor standard.
#'
#' @param df Data frame
#' @param verbose Logical - print detailed output
#' @param limited_dataset Logical - if FALSE, performs age-capping
#' @return List with two elements: df (modified dataframe) and stats (age statistics)
#' @noRd
standardize_age <- function(df, verbose = TRUE, limited_dataset = FALSE) {
  # Initialize statistics
  age_stats <- list(
    values_capped = 0,
    non_na_count = 0,
    all_na = TRUE,
    max_age = NA
  )
  
  if ("interview_age" %in% names(df)) {
    # Convert to numeric
    df$interview_age <- as.numeric(df$interview_age)
    
    # Check if all values are NA
    age_stats$all_na <- all(is.na(df$interview_age))
    age_stats$non_na_count <- sum(!is.na(df$interview_age))
    
    if (!age_stats$all_na) {
      age_stats$max_age <- max(df$interview_age, na.rm = TRUE)
      
      if (!limited_dataset) {
        # Count values that will be capped
        age_stats$values_capped <- sum(df$interview_age > 1068, na.rm = TRUE)
        
        # Store original for verbose output
        if (verbose) {
          orig_age_stats <- summary(df$interview_age)
        }
        
        # Apply age-capping (cap at 1068 months = 89 years)
        df$interview_age <- pmin(df$interview_age, 1068)
        
        # Verbose output
        if (verbose) {
          cat("\nAge standardization summary:")
          cat("\nBefore:", capture.output(orig_age_stats))
          cat("\nAfter:", capture.output(summary(df$interview_age)))
          if (age_stats$values_capped > 0) {
            cat(sprintf("\nNumber of values capped at 1068 months: %d", age_stats$values_capped))
          } else {
            cat("\nNo values needed capping (all were <= 1068 months)")
          }
          cat("\n")
        }
      }
    }
  }
  
  # Return both dataframe and statistics
  return(list(
    df = df,
    stats = age_stats
  ))
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
convert_logical_to_integer <- function(df, verbose = FALSE) {
  bool_strings <- c("TRUE", "FALSE", "true", "false", "True", "False")
  for (col in names(df)) {
    x <- df[[col]]
    if (is.logical(x)) {
      if (verbose) cat(sprintf("\nConverting logical column %s to integer", col))
      df[[col]] <- as.integer(x)
    } else if (is.character(x)) {
      non_na <- x[!is.na(x) & nzchar(x)]
      if (length(non_na) > 0 && all(non_na %in% bool_strings)) {
        if (verbose) cat(sprintf("\nConverting boolean-string column %s to integer", col))
        df[[col]] <- as.integer(x %in% c("TRUE", "true", "True"))
      }
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
  
  # Handle index/trial_index -> trial conversion
  index_source <- if ("trial_index" %in% names(df)) "trial_index" else if ("index" %in% names(df)) "index" else NULL
  if (!is.null(index_source) && !"trial" %in% names(df)) {
    if(verbose) cat(sprintf("\n\nProcessing '%s' to 'trial' conversion...", index_source))

    orig_values <- head(df[[index_source]], 3)
    raw_index   <- suppressWarnings(as.numeric(df[[index_source]]))

    # Convert 0-based jsPsych indices to 1-based trial numbers
    df$trial <- raw_index + 1L
    df$trial[is.na(raw_index)] <- NA_integer_

    total_rows   <- length(raw_index)
    positive_rows <- sum(!is.na(df$trial))

    transformations[[paste0(index_source, "_to_trial")]] <- list(
      from          = index_source,
      to            = "trial",
      total         = total_rows,
      valid         = positive_rows,
      sample_before = orig_values,
      sample_after  = head(df$trial, 3)
    )

    if(verbose) {
      cat(sprintf("\n  Total rows: %d", total_rows))
      cat(sprintf("\n  Valid rows: %d", positive_rows))
      cat("\n  Sample values:")
      cat(sprintf("\n    Before: %s", paste(orig_values, collapse=", ")))
      cat(sprintf("\n    After:  %s", paste(head(df$trial, 3), collapse=", ")))
    }

    df[[index_source]] <- NULL
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

#' Recode user-defined missing data codes to the NDA-expected special codes
#'
#' @description
#' For each column in \code{df}, looks up the NDA structure's \code{valueRange}
#' for that field, extracts the special (non-range) codes the NDA expects, and
#' replaces any study-specific missing data codes (from config.yml) with those
#' NDA-defined codes. This runs per-field so the target code matches exactly
#' what the NDA structure defines for each measure (e.g., -9 from "1::4;-9").
#'
#' @param df Data frame
#' @param config List - config object from validate_config()
#' @param nda_structure List - NDA structure with a \code{dataElements} data frame
#' @param verbose Logical - print replacement summary
#' @return Data frame with user missing codes replaced by NDA special codes
#' @noRd
recode_missing_data_codes <- function(df, config, nda_structure, verbose = TRUE) {
  if (is.null(config) || is.null(config$missing_data_codes)) return(df)
  if (is.null(nda_structure) || !"dataElements" %in% names(nda_structure)) return(df)

  # Flatten all user-defined codes across all categories into a numeric vector
  all_user_codes <- unique(suppressWarnings(as.numeric(unlist(config$missing_data_codes))))
  all_user_codes <- all_user_codes[!is.na(all_user_codes)]
  if (length(all_user_codes) == 0) return(df)

  elements <- nda_structure$dataElements
  total_replacements <- 0L
  affected_cols <- character(0)

  for (col in names(df)) {
    # Only recode numeric/integer columns that actually contain user missing codes
    if (!is.numeric(df[[col]]) && !is.integer(df[[col]])) next
    user_hits <- !is.na(df[[col]]) & df[[col]] %in% all_user_codes
    if (!any(user_hits)) next

    # Find the NDA field definition for this column
    field_idx <- which(elements$name == col)
    if (length(field_idx) == 0) next
    range_str <- elements$valueRange[field_idx[1]]
    if (is.null(range_str) || is.na(range_str) || range_str == "") next

    # Extract NDA special codes: values listed after ';' that fall outside the
    # primary numeric range (e.g., from "1::4;-9" extract -9)
    nda_special_codes <- .extract_nda_special_codes(range_str)

    if (length(nda_special_codes) == 0) {
      # No NDA-defined special code for this field; blank it out.
      # NDA accepts NA for fields that don't define an explicit missing-value code.
      df[[col]][user_hits] <- NA_real_
    } else {
      # Use the most-negative special code as the replacement target
      # (most negative is typically the generic "missing / not known" code)
      target_code <- nda_special_codes[which.min(nda_special_codes)]
      df[[col]][user_hits] <- target_code
    }

    n <- sum(user_hits)
    total_replacements <- total_replacements + n
    affected_cols <- c(affected_cols, col)
  }

  if (verbose && total_replacements > 0) {
    message(sprintf(
      "Recoded %d missing value(s) in %d field(s) to NDA special code or NA: %s",
      total_replacements, length(affected_cols),
      paste(affected_cols, collapse = ", ")
    ))
  }

  return(df)
}

#' Extract NDA special (non-range) codes from a valueRange string
#'
#' @description
#' Given a valueRange like "1::4;-9" or "0::3;-8;-9", returns the numeric
#' codes that are listed explicitly (after ";") and fall outside the primary
#' contiguous range. These are the NDA-defined special/missing codes.
#'
#' @param range_str valueRange string from an NDA data element
#' @return Numeric vector of special codes outside the primary range
#' @noRd
.extract_nda_special_codes <- function(range_str) {
  if (!grepl(";", range_str)) return(numeric(0))
  if (!grepl("::", range_str)) return(numeric(0))  # categorical only (e.g. "1;2;3") — no special codes
  parts <- trimws(strsplit(range_str, ";")[[1]])
  range_parts   <- parts[grepl("::", parts)]
  special_parts <- suppressWarnings(as.numeric(parts[!grepl("::", parts)]))
  special_parts <- special_parts[!is.na(special_parts)]

  # Exclude specials that fall inside the primary numeric range
  if (length(range_parts) > 0) {
    bounds <- suppressWarnings(as.numeric(strsplit(range_parts[1], "::")[[1]]))
    if (length(bounds) == 2 && !any(is.na(bounds))) {
      special_parts <- special_parts[special_parts < bounds[1] | special_parts > bounds[2]]
    }
  }
  special_parts
}

#' Normalize an array-notation string for comparison
#'
#' @description
#' Converts R vector notation c(...) to bracket notation [...], strips all
#' whitespace, and lowercases so "[0.9, 0.7]" and "c(0.9,0.7)" compare equal.
#'
#' @param s Character scalar
#' @return Normalized character scalar
#' @noRd
.normalize_array_string <- function(s) {
  if (is.null(s) || length(s) == 0 || is.na(s)) return(NA_character_)
  s <- gsub("^c\\((.*)\\)$", "[\\1]", trimws(s))
  s <- gsub("\\s", "", s)
  tolower(s)
}

#' Detect and convert array-notation fields to numeric codes
#'
#' @description
#' Handles MongoDB list columns and character columns containing bracket/vector
#' notation (e.g., "[0.9, 0.7, 0.3, 0.1]" or "c(0.9, 0.7, 0.3, 0.1)").
#'
#' For each such field that appears in the NDA structure, the function:
#'   1. Looks up the NDA field notes for array-to-code mappings
#'      (notes format: "1=(0.9, 0.7, 0.3, 0.1); 2=(0.1, 0.3, 0.7, 0.9)")
#'   2. If mappings exist, converts every array value to its numeric code.
#'   3. If array values are found but NO mapping is defined, stops with a
#'      clear error explaining what format the notes must follow.
#'   4. If values cannot be matched to any defined pattern, stops with the
#'      unmatched samples and the patterns that were expected.
#'
#' @param df Data frame (may contain list columns from mongolite)
#' @param elements NDA structure dataElements data frame
#' @param verbose Logical - print conversion details
#' @return Data frame with array columns recoded to numeric codes (character)
#' @noRd
convert_array_fields <- function(df, elements, verbose = FALSE) {
  converted_fields <- character(0)

  for (col in intersect(names(df), elements$name)) {
    col_data <- df[[col]]

    is_list_col  <- is.list(col_data)
    is_char_array <- is.character(col_data) &&
      any(!is.na(col_data) & grepl("^\\[|^c\\(", col_data))

    if (!is_list_col && !is_char_array) next

    field_idx <- which(elements$name == col)
    if (length(field_idx) == 0) next
    element  <- elements[field_idx[1], ]
    notes    <- if ("notes" %in% names(element) && !is.na(element$notes[[1]])) element$notes[[1]] else ""

    rules       <- get_mapping_rules(notes)
    array_rules <- Filter(function(v) grepl("^\\[.*\\]$", v), rules)

    # Helper: represent one list element as "[v1, v2, ...]"
    list_to_bracket <- function(x) {
      if (is.null(x) || length(x) == 0 || all(is.na(x))) return(NA_character_)
      paste0("[", paste(x, collapse = ", "), "]")
    }

    if (length(array_rules) == 0) {
      sample_vals <- if (is_list_col) {
        head(vapply(col_data[!sapply(col_data, function(x) is.null(x) || length(x) == 0 || all(is.na(x)))],
                    list_to_bracket, character(1)), 3)
      } else {
        head(col_data[!is.na(col_data) & grepl("^\\[|^c\\(", col_data)], 3)
      }
      stop(sprintf(
        paste0(
          "Field '%s' contains array data that cannot be automatically recoded.\n",
          "  Sample values : %s\n",
          "  No array-to-code mapping was found in the NDA field notes.\n",
          "  Expected notes format: \"1=(val1, val2, ...); 2=(val3, val4, ...)\"\n",
          "  Recode this field manually in your NDA script before submission."
        ),
        col, paste(sample_vals, collapse = ", ")
      ))
    }

    # Build reverse map: normalized_array_string -> code
    reverse_map <- setNames(
      as.list(names(array_rules)),
      vapply(unlist(array_rules), .normalize_array_string, character(1))
    )

    # Convert each row
    if (is_list_col) {
      new_values <- vapply(col_data, function(x) {
        key <- .normalize_array_string(list_to_bracket(x))
        if (is.na(key)) return(NA_character_)
        code <- reverse_map[[key]]
        if (is.null(code)) NA_character_ else as.character(code)
      }, character(1))
    } else {
      new_values <- vapply(col_data, function(x) {
        if (is.na(x)) return(NA_character_)
        if (!grepl("^\\[|^c\\(", x)) return(x)
        key  <- .normalize_array_string(x)
        code <- reverse_map[[key]]
        if (is.null(code)) NA_character_ else as.character(code)
      }, character(1))
    }

    # Count values that had array notation but didn't match any pattern
    if (is_list_col) {
      had_array <- !sapply(col_data, function(x) is.null(x) || length(x) == 0 || all(is.na(x)))
    } else {
      had_array <- !is.na(col_data) & grepl("^\\[|^c\\(", col_data)
    }

    unmapped_idx <- which(had_array & is.na(new_values))

    if (length(unmapped_idx) > 0) {
      sample_unmapped <- if (is_list_col) {
        head(vapply(col_data[unmapped_idx], list_to_bracket, character(1)), 3)
      } else {
        head(col_data[unmapped_idx], 3)
      }
      stop(sprintf(
        paste0(
          "Field '%s': %d array value(s) could not be matched to any NDA-defined code.\n",
          "  Unmatched sample : %s\n",
          "  Defined patterns : %s\n",
          "  Verify the data matches the NDA structure definition."
        ),
        col,
        length(unmapped_idx),
        paste(unique(sample_unmapped), collapse = ", "),
        paste(unlist(array_rules), collapse = " | ")
      ))
    }

    df[[col]] <- new_values
    converted_fields <- c(converted_fields, col)

    if (verbose) {
      message(sprintf("  [ARRAY RECODE] '%s': %d value(s) converted to numeric codes",
                      col, sum(had_array)))
    }
  }

  if (length(converted_fields) > 0 && !verbose) {
    message(sprintf("[OK] Auto-recoded array fields: %s",
                    paste(converted_fields, collapse = ", ")))
  }

  return(df)
}
