#' NDA Field Mapping and Detection Functions
#'
#' @description
#' Functions for detecting, mapping, and renaming fields to match NDA structure requirements.
#' Includes similarity calculation, field matching, and interactive renaming.
#'

#' Calculate Jaro-Winkler similarity between two strings
#'
#' @description
#' Calculates string similarity using Jaro-Winkler algorithm.
#' Enhanced for NDA structure name matching with scientific abbreviations.
#'
#' @param str1 First string
#' @param str2 Second string
#' @return Numeric similarity score (0-1)
#' @noRd
calculate_jaro_winkler <- function(str1, str2) {
  # Convert to lowercase for comparison
  str1 <- tolower(str1)
  str2 <- tolower(str2)
  
  # Handle exact matches first
  if (str1 == str2) return(1.0)
  if (nchar(str1) == 0 || nchar(str2) == 0) return(0.0)
  
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
  
  return(base_score)
}

#' Calculate Levenshtein distance similarity between two strings
#'
#' @description
#' Alternative similarity metric using edit distance.
#'
#' @param str1 First string
#' @param str2 Second string
#' @return Numeric similarity score (0-1)
#' @noRd
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

#' Extract mapping rules from NDA field notes
#'
#' @description
#' Parses the "notes" field from NDA structure to extract value mappings
#' (e.g., "1=Never; 2=Sometimes; 999=Missing")
#'
#' @param notes Notes string from NDA dataElement
#' @return Named list of code -> meaning mappings, or NULL
#' @noRd
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
          if (length(parts) >= 2) {
            code <- trimws(parts[1])
            value <- trimws(parts[2])
            if (!(code %in% names(rules))) {
              rules[[code]] <- value
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
          if (length(parts) >= 3) {
            code <- parts[2]
            values <- sprintf("[%s]", parts[3])
            rules[[code]] <- values
          }
        }
      }
    }
  }, error = function(e) {
    warning(sprintf("Error parsing mapping rules: %s\nNotes: %s", e$message, notes))
    return(list())
  })
  
  return(rules)
}

#' Handle missing required fields
#'
#' @description
#' Auto-adds missing required fields with appropriate missing data codes
#' extracted from the NDA structure notes.
#'
#' @param df Data frame
#' @param elements NDA structure dataElements
#' @param missing_required Character vector of missing required field names
#' @param verbose Logical - print details
#' @return Data frame with missing fields added
#' @noRd
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

#' Parse a value range string to extract numeric ranges and missing codes
#'
#' @param value_range Value range string (e.g., "0::6;999=missing")
#' @return List with parsed components (numeric_range, missing_codes)
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
      if (grepl("^-?\\d+$", part)) {
        if (is.null(missing_codes)) missing_codes <- character(0)
        missing_codes <- c(missing_codes, part)
      }
    }
  }
  
  return(list(numeric_range = numeric_range, missing_codes = missing_codes))
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
  
  # Default: ranges are compatible if we can't determine otherwise
  return(list(
    compatible = TRUE,
    message = "Cannot determine compatibility - special codes or no numeric ranges",
    recommendation = "Proceed with caution"
  ))
}

#' Get Matrix Group Name for REDCap Field
#' @param field_name Character - field to check
#' @param api Character - API type
#' @param measure_name Character - for REDCap dictionary lookup
#' @return Character - matrix group name or empty string
#' @noRd
get_matrix_group_name <- function(field_name, api, measure_name) {
  if (tolower(api) != "redcap") return("")
  
  redcap_dict <- tryCatch({
    redcap.dict(measure_name)
  }, error = function(e) NULL)
  
  if (is.null(redcap_dict) || !field_name %in% redcap_dict$field_name) {
    return("")
  }
  
  matrix_group <- redcap_dict$matrix_group_name[redcap_dict$field_name == field_name]
  if (is.na(matrix_group)) return("")
  
  return(as.character(matrix_group))
}

#' Fetch NDA structure from API
#'
#' @param structure_name Structure short name
#' @param nda_base_url NDA API base URL
#' @return List with structure definition
#' @noRd
fetch_nda_structure <- function(structure_name, nda_base_url) {
  url <- sprintf("%s/datastructure/%s", nda_base_url, structure_name)
  response <- httr::GET(url, httr::timeout(10))
  
  if (httr::status_code(response) != 200) {
    stop(sprintf("Failed to fetch structure '%s' - HTTP %d", 
                structure_name, httr::status_code(response)))
  }
  
  content <- jsonlite::fromJSON(rawToChar(response$content))
  
  if (!"dataElements" %in% names(content)) {
    stop("Unexpected API response format - no dataElements found")
  }
  
  return(content)
}
