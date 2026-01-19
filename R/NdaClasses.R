#' NDA Type Safety Classes
#'
#' Complete R6 class system for type-safe NDA data structure management.
#' All Excel columns are represented by typed R6 classes with validation.
#'
#' @name NdaClasses
#' @keywords internal
NULL

# Get config environment for validation settings
get_validation_config <- function() {
  config_file <- Sys.getenv("WIZARDRY_CONFIG", "config.yml")
  if (file.exists(config_file)) {
    tryCatch({
      config <- yaml::read_yaml(config_file)
      return(list(
        strict_validation = config$validation$strict %||% FALSE,
        auto_fix = config$validation$auto_fix %||% TRUE
      ))
    }, error = function(e) {
      return(list(strict_validation = FALSE, auto_fix = TRUE))
    })
  }
  list(strict_validation = FALSE, auto_fix = TRUE)
}

# =============================================================================
# ELEMENT NAME CLASS
# =============================================================================

#' Element Name Management
#'
#' Validates and manages NDA element names
#' 
#' @field name Character - the element name
#' @field is_valid Logical - passes NDA naming rules
#' @field validation_errors Character vector - validation issues
#' 
#' @keywords internal
#' @noRd
ElementName <- R6::R6Class("ElementName",
  public = list(
    value = NULL,  # Alias for name (used by NdaDataStructure)
    name = NULL,
    is_valid = NULL,
    validation_errors = NULL,
    
    initialize = function(name = "") {
      self$name <- name
      self$value <- name  # Keep value in sync with name
      self$validation_errors <- character(0)
      self$validate()
    },
    
    validate = function() {
      config <- get_validation_config()
      self$validation_errors <- character(0)
      
      if (is.null(self$name) || !nzchar(self$name)) {
        self$validation_errors <- c(self$validation_errors, "Name cannot be empty")
        self$is_valid <- FALSE
        
        if (config$strict_validation) {
          stop(sprintf("Invalid element name: %s", paste(self$validation_errors, collapse = "; ")))
        }
        return(invisible(self))
      }
      
      # NDA naming rules
      if (!grepl("^[a-zA-Z]", self$name)) {
        self$validation_errors <- c(self$validation_errors, "Must start with a letter")
      }
      
      if (grepl("[^a-zA-Z0-9_]", self$name)) {
        self$validation_errors <- c(self$validation_errors, 
                                   "Can only contain letters, numbers, and underscores")
      }
      
      if (nchar(self$name) > 50) {
        self$validation_errors <- c(self$validation_errors, 
                                   "Name exceeds 50 character maximum")
      }
      
      self$is_valid <- length(self$validation_errors) == 0
      
      if (!self$is_valid && config$strict_validation) {
        stop(sprintf("Invalid element name '%s': %s", 
                    self$name, 
                    paste(self$validation_errors, collapse = "; ")))
      }
      
      invisible(self)
    },
    
    to_string = function() {
      self$name
    },
    
    matches = function(pattern) {
      grepl(pattern, self$name, ignore.case = TRUE)
    }
  )
)

# =============================================================================
# DATA TYPE CLASS
# =============================================================================

#' Data Type Management
#'
#' Validates and manages NDA data types
#' 
#' @field value Character - the data type value
#' @field valid_types Character vector - allowed NDA types
#' @field is_valid Logical - whether value is a valid NDA type
#' @field validation_errors Character vector - validation issues
#' 
#' @keywords internal
#' @noRd
DataType <- R6::R6Class("DataType",
  public = list(
    value = NULL,
    valid_types = c("String", "Integer", "Float", "Date", "GUID", "Boolean"),
    is_valid = NULL,
    validation_errors = NULL,
    
    initialize = function(type = "String") {
      self$value <- type
      self$validation_errors <- character(0)
      self$validate()
    },
    
    validate = function() {
      config <- get_validation_config()
      self$validation_errors <- character(0)
      
      if (is.null(self$value) || !nzchar(self$value)) {
        self$validation_errors <- c(self$validation_errors, "Type cannot be empty")
        self$is_valid <- FALSE
        
        if (config$strict_validation) {
          stop(sprintf("Invalid data type: %s", paste(self$validation_errors, collapse = "; ")))
        }
        return(invisible(self))
      }
      
      if (!self$value %in% self$valid_types) {
        self$validation_errors <- c(self$validation_errors, 
                                   sprintf("Type '%s' not in valid types: %s", 
                                          self$value, 
                                          paste(self$valid_types, collapse = ", ")))
        self$is_valid <- FALSE
        
        if (config$strict_validation) {
          stop(sprintf("Invalid data type: %s", paste(self$validation_errors, collapse = "; ")))
        }
      } else {
        self$is_valid <- TRUE
      }
      
      invisible(self)
    },
    
    to_string = function() {
      self$value
    },
    
    is_string = function() {
      self$value == "String"
    },
    
    is_numeric = function() {
      self$value %in% c("Integer", "Float")
    },
    
    is_date = function() {
      self$value == "Date"
    },
    
    is_guid = function() {
      self$value == "GUID"
    },
    
    is_boolean = function() {
      self$value == "Boolean"
    },
    
    requires_size = function() {
      self$value == "String"
    }
  )
)

# ==============================================================================
# SHIFT DETECTION & MERGE FUNCTIONS
# ==============================================================================

#' Detect Value Range Shift
#'
#' Detects if user's value range is a shifted version of NDA's range.
#' Example: NDA has 0=Not at all, 4=Very much (0::4)
#'          User has 1=Not at all, 5=Very much (1::5)
#'          → Shifted by +1
#'
#' @param nda_range ValueRange object or string for NDA range
#' @param user_range ValueRange object or string for user's range
#' @return List with:
#'   - is_shifted: TRUE/FALSE
#'   - offset: Integer offset (e.g., +1, -2)
#'   - confidence: "high", "medium", "low"
#'   - evidence: List of evidence supporting detection
#'
#' @keywords internal
#' @noRd
detect_value_range_shift <- function(nda_range, user_range) {
  
  # Convert to ValueRange objects if needed
  if (is.character(nda_range)) {
    nda_range <- ValueRange$new(nda_range)
  }
  if (is.character(user_range)) {
    user_range <- ValueRange$new(user_range)
  }
  
  # Initialize result
  result <- list(
    is_shifted = FALSE,
    offset = 0L,
    confidence = "none",
    evidence = list()
  )
  
  # Can't detect shift if either range is empty
  if (nda_range$is_empty() || user_range$is_empty()) {
    result$evidence$reason <- "One or both ranges are empty"
    return(result)
  }
  
  # Extract numeric codes
  nda_codes <- nda_range$get_numeric_codes()
  user_codes <- user_range$get_numeric_codes()
  
  # Need numeric codes from both ranges
  if (length(nda_codes) == 0 || length(user_codes) == 0) {
    result$evidence$reason <- "Non-numeric codes in one or both ranges"
    return(result)
  }
  
  # Must have same number of values to be a shift
  if (length(nda_codes) != length(user_codes)) {
    result$evidence$reason <- sprintf(
      "Different number of codes (NDA: %d, User: %d)",
      length(nda_codes), length(user_codes)
    )
    return(result)
  }
  
  # Sort codes for comparison
  nda_sorted <- sort(nda_codes)
  user_sorted <- sort(user_codes)
  
  # Calculate offsets between corresponding positions
  offsets <- user_sorted - nda_sorted
  
  # Check if offset is consistent (all the same)
  unique_offsets <- unique(offsets)
  
  if (length(unique_offsets) != 1) {
    result$evidence$reason <- "Inconsistent offsets between codes"
    result$evidence$offsets <- offsets
    return(result)
  }
  
  offset <- unique_offsets[1]
  
  # Zero offset means ranges are identical
  if (offset == 0) {
    result$evidence$reason <- "Ranges are identical (no shift)"
    return(result)
  }
  
  # We detected a consistent shift!
  result$is_shifted <- TRUE
  result$offset <- offset
  result$evidence$nda_codes <- nda_sorted
  result$evidence$user_codes <- user_sorted
  result$evidence$offset_pattern <- offsets
  
  # Determine confidence based on labels (if available)
  nda_labels <- nda_range$get_labels()
  user_labels <- user_range$get_labels()
  
  if (length(nda_labels) > 0 && length(user_labels) > 0) {
    # Compare labels for matching codes
    label_matches <- 0
    label_comparisons <- 0
    
    for (i in seq_along(nda_sorted)) {
      nda_code <- as.character(nda_sorted[i])
      user_code <- as.character(user_sorted[i])
      
      if (nda_code %in% names(nda_labels) && user_code %in% names(user_labels)) {
        label_comparisons <- label_comparisons + 1
        
        nda_label <- tolower(trimws(nda_labels[[nda_code]]))
        user_label <- tolower(trimws(user_labels[[user_code]]))
        
        # Check if labels match or are very similar
        if (nda_label == user_label) {
          label_matches <- label_matches + 1
        } else {
          # Calculate simple similarity (for partial credit)
          max_len <- max(nchar(nda_label), nchar(user_label))
          common_chars <- sum(utf8ToInt(nda_label) %in% utf8ToInt(user_label))
          similarity <- common_chars / max_len
          if (similarity > 0.7) {
            label_matches <- label_matches + 0.5
          }
        }
      }
    }
    
    if (label_comparisons > 0) {
      match_rate <- label_matches / label_comparisons
      result$evidence$label_match_rate <- match_rate
      
      if (match_rate >= 0.8) {
        result$confidence <- "high"
      } else if (match_rate >= 0.5) {
        result$confidence <- "medium"
      } else {
        result$confidence <- "low"
      }
    } else {
      result$confidence <- "medium"
      result$evidence$label_match_rate <- NA
    }
  } else {
    # No labels to compare, medium confidence based on numeric pattern alone
    result$confidence <- "medium"
    result$evidence$label_match_rate <- NA
  }
  
  return(result)
}

#' Merge Value Ranges
#'
#' Intelligently merges value ranges from NDA, REDCap, and data with conflict resolution.
#'
#' Priority order:
#' 1. No NDA range exists → use REDCap or data-inferred range
#' 2. NDA range exists, no user range → use NDA as-is
#' 3. Shift detected → normalize to NDA format, document shift
#' 4. New codes detected (e.g., 999=Undefined) → append to NDA range
#' 5. Conflict with no resolution → use NDA, warn user
#'
#' @param nda_range ValueRange object or string for NDA range (may be NULL/empty)
#' @param redcap_range ValueRange object or string for REDCap range (may be NULL/empty)
#' @param data_range ValueRange object or string for data-inferred range (may be NULL/empty)
#' @param missing_codes Character vector of codes to treat as missing data
#' @return List with:
#'   - merged_range: ValueRange object (final merged result)
#'   - strategy_used: Character describing which merge strategy was applied
#'   - modifications: List of modifications made
#'   - warnings: Character vector of warnings for user
#'
#' @keywords internal
#' @noRd
merge_value_ranges <- function(nda_range = NULL, 
                                redcap_range = NULL, 
                                data_range = NULL,
                                missing_codes = character(0)) {
  
  # Convert to ValueRange objects if needed
  if (!is.null(nda_range) && is.character(nda_range)) {
    nda_range <- ValueRange$new(nda_range)
  }
  if (!is.null(redcap_range) && is.character(redcap_range)) {
    redcap_range <- ValueRange$new(redcap_range)
  }
  if (!is.null(data_range) && is.character(data_range)) {
    data_range <- ValueRange$new(data_range)
  }
  
  # Initialize result
  result <- list(
    merged_range = NULL,
    strategy_used = "unknown",
    modifications = list(),
    warnings = character(0)
  )
  
  # CASE 1: No NDA range exists → use REDCap or data-inferred range
  if (is.null(nda_range) || nda_range$is_empty()) {
    if (!is.null(redcap_range) && !redcap_range$is_empty()) {
      result$merged_range <- redcap_range
      result$strategy_used <- "no_nda_use_redcap"
      result$modifications <- list("Used REDCap range (no NDA range available)")
    } else if (!is.null(data_range) && !data_range$is_empty()) {
      result$merged_range <- data_range
      result$strategy_used <- "no_nda_use_data"
      result$modifications <- list("Used data-inferred range (no NDA range available)")
    } else {
      # No range available from any source
      result$merged_range <- ValueRange$new("")
      result$strategy_used <- "no_range_available"
      result$warnings <- c(result$warnings, "No value range available from any source")
    }
    return(result)
  }
  
  # CASE 2: NDA range exists, no user range → use NDA as-is
  user_range <- if (!is.null(redcap_range) && !redcap_range$is_empty()) {
    redcap_range
  } else if (!is.null(data_range) && !data_range$is_empty()) {
    data_range
  } else {
    NULL
  }
  
  if (is.null(user_range)) {
    result$merged_range <- nda_range
    result$strategy_used <- "nda_only"
    return(result)
  }
  
  # CASE 3: Check for shift
  shift_detection <- detect_value_range_shift(nda_range, user_range)
  
  if (shift_detection$is_shifted) {
    # Normalize to NDA format
    result$merged_range <- nda_range$clone()
    result$strategy_used <- "shift_detected_normalized"
    result$modifications <- list(
      sprintf(
        "Shift detected (offset: %+d, confidence: %s). Normalized to NDA format.",
        shift_detection$offset,
        shift_detection$confidence
      )
    )
    result$warnings <- c(
      result$warnings,
      sprintf(
        "User data appears shifted by %+d from NDA range. Data will be normalized.",
        shift_detection$offset
      )
    )
    return(result)
  }
  
  # CASE 4: Check for new codes (e.g., missing data codes)
  nda_codes <- nda_range$get_numeric_codes()
  user_codes <- user_range$get_numeric_codes()
  
  new_codes <- setdiff(user_codes, nda_codes)
  
  if (length(new_codes) > 0) {
    # Check if new codes are missing data codes
    new_codes_char <- as.character(new_codes)
    missing_in_new <- new_codes_char %in% missing_codes
    
    if (any(missing_in_new)) {
      # Append missing codes to NDA range
      result$merged_range <- nda_range$merge_with(user_range)
      result$strategy_used <- "missing_codes_appended"
      result$modifications <- list(
        sprintf(
          "Appended missing data codes to NDA range: %s",
          paste(new_codes_char[missing_in_new], collapse = ", ")
        )
      )
    } else {
      # New codes exist but aren't recognized as missing codes
      result$merged_range <- nda_range$merge_with(user_range)
      result$strategy_used <- "new_codes_appended"
      result$modifications <- list(
        sprintf(
          "Appended new codes to NDA range: %s",
          paste(new_codes_char, collapse = ", ")
        )
      )
      result$warnings <- c(
        result$warnings,
        sprintf(
          "User data contains codes not in NDA range: %s. Appended to range.",
          paste(new_codes_char, collapse = ", ")
        )
      )
    }
    return(result)
  }
  
  # CASE 5: Ranges are compatible or identical
  if (all(user_codes %in% nda_codes)) {
    result$merged_range <- nda_range$clone()
    result$strategy_used <- "user_subset_of_nda"
    result$modifications <- list("User range is subset of NDA range. Using NDA range.")
    return(result)
  }
  
  # CASE 6: Conflict with no clear resolution → use NDA, warn user
  result$merged_range <- nda_range$clone()
  result$strategy_used <- "conflict_use_nda"
  result$warnings <- c(
    result$warnings,
    sprintf(
      "Value range conflict detected. Using NDA range. NDA: %s | User: %s",
      nda_range$to_nda_format(),
      user_range$to_nda_format()
    )
  )
  result$modifications <- list("Conflict detected. Defaulted to NDA range.")
  
  return(result)
}

# =============================================================================
# SIZE CLASS
# =============================================================================

#' Field Size Management
#'
#' Manages field size with type-specific validation
#' 
#' @field size Integer - field size
#' @field data_type DataType - associated data type
#' @field is_valid Logical - valid for this data type
#' 
#' @keywords internal
#' @noRd
Size <- R6::R6Class("Size",
  public = list(
    size = NULL,
    data_type = NULL,
    is_valid = NULL,
    
    initialize = function(size = NA, data_type = NULL) {
      self$data_type <- data_type
      self$set_size(size)
    },
    
    set_size = function(size) {
      if (is.null(size) || is.na(size) || !nzchar(as.character(size))) {
        self$size <- NA_integer_
        # Check if size is required for this data type
        if (!is.null(self$data_type)) {
          # Handle both DataType object and string
          if (inherits(self$data_type, "DataType")) {
            self$is_valid <- !self$data_type$requires_size()
          } else {
            # String data type
            self$is_valid <- (self$data_type != "String")
          }
        } else {
          self$is_valid <- TRUE
        }
        return(invisible(self))
      }
      
      size_num <- suppressWarnings(as.integer(size))
      
      if (is.na(size_num)) {
        self$size <- NA_integer_
        self$is_valid <- FALSE
      } else {
        self$size <- size_num
        self$is_valid <- self$validate_for_type()
      }
      
      invisible(self)
    },
    
    validate_for_type = function() {
      if (is.null(self$data_type)) return(TRUE)
      
      # Get type string (handle both DataType object and string)
      type_str <- if (inherits(self$data_type, "DataType")) {
        self$data_type$value
      } else {
        as.character(self$data_type)
      }
      
      if (type_str == "String") {
        return(!is.na(self$size) && self$size > 0 && self$size <= 4000)
      }
      
      if (type_str %in% c("Integer", "Float", "Date", "GUID", "Boolean")) {
        return(is.na(self$size))
      }
      
      TRUE
    },
    
    to_string = function() {
      if (is.na(self$size)) return("")
      as.character(self$size)
    },
    
    to_integer = function() {
      self$size
    }
  )
)

# =============================================================================
# REQUIREMENT LEVEL CLASS
# =============================================================================

#' Requirement Level Management
#'
#' Manages NDA requirement levels with validation
#' 
#' @field level Character - "Required", "Recommended", "Conditional", "No"
#' @field is_valid Logical - valid NDA requirement level
#' 
#' @keywords internal
#' @noRd
RequirementLevel <- R6::R6Class("RequirementLevel",
  public = list(
    level = NULL,
    is_valid = NULL,
    
    VALID_LEVELS = c("Required", "Recommended", "Conditional", "No"),
    
    initialize = function(level = "No") {
      self$set_level(level)
    },
    
    set_level = function(level) {
      if (is.null(level) || !nzchar(level)) {
        self$level <- "No"
        self$is_valid <- TRUE
      } else if (level %in% self$VALID_LEVELS) {
        self$level <- level
        self$is_valid <- TRUE
      } else {
        self$level <- level
        self$is_valid <- FALSE
        
        config <- get_validation_config()
        if (config$auto_fix) {
          self$level <- "No"
          self$is_valid <- TRUE
        }
      }
      invisible(self)
    },
    
    is_required = function() {
      self$level == "Required"
    },
    
    is_recommended = function() {
      self$level == "Recommended"
    },
    
    is_conditional = function() {
      self$level == "Conditional"
    },
    
    to_string = function() {
      self$level
    }
  )
)

# =============================================================================
# CONDITION CLASS
# =============================================================================

#' Conditional Logic Management
#'
#' Manages conditional requirements (e.g., "Required if sex=F")
#' 
#' @field condition Character - condition expression
#' @field is_parsed Logical - successfully parsed
#' @field dependent_fields Character vector - fields this depends on
#' 
#' @keywords internal
#' @noRd
Condition <- R6::R6Class("Condition",
  public = list(
    condition = NULL,
    is_parsed = NULL,
    dependent_fields = NULL,
    
    initialize = function(condition = "") {
      self$condition <- condition
      self$dependent_fields <- character(0)
      self$parse()
    },
    
    parse = function() {
      if (is.null(self$condition) || !nzchar(self$condition)) {
        self$is_parsed <- TRUE
        return(invisible(self))
      }
      
      field_pattern <- "\\b([a-zA-Z_][a-zA-Z0-9_]*)\\s*[=<>!]"
      matches <- gregexpr(field_pattern, self$condition, perl = TRUE)
      
      if (matches[[1]][1] != -1) {
        match_text <- regmatches(self$condition, matches)[[1]]
        self$dependent_fields <- unique(gsub("\\s*[=<>!].*$", "", match_text))
        self$is_parsed <- TRUE
      } else {
        self$is_parsed <- FALSE
      }
      
      invisible(self)
    },
    
    to_string = function() {
      self$condition %||% ""
    },
    
    is_empty = function() {
      is.null(self$condition) || !nzchar(self$condition)
    },
    
    get_dependencies = function() {
      self$dependent_fields
    }
  )
)

# =============================================================================
# DESCRIPTION CLASS
# =============================================================================

#' Element Description Management
#'
#' Manages field descriptions with length validation
#' 
#' @field text Character - description text
#' @field source Character - where description came from
#' @field is_valid Logical - meets NDA requirements
#' 
#' @keywords internal
#' @noRd
Description <- R6::R6Class("Description",
  public = list(
    text = NULL,
    source = NULL,
    is_valid = NULL,
    
    MAX_LENGTH = 4000,
    
    initialize = function(text = "", source = "unknown") {
      self$text <- text
      self$source <- source
      self$validate()
    },
    
    validate = function() {
      if (is.null(self$text)) {
        self$is_valid <- FALSE
        return(invisible(self))
      }
      
      cleaned <- gsub("<br\\s*/?>", " ", self$text, ignore.case = TRUE)
      cleaned <- gsub("<[^>]+>", "", cleaned)
      cleaned <- trimws(cleaned)
      
      self$text <- cleaned
      self$is_valid <- nzchar(self$text) && nchar(self$text) <= self$MAX_LENGTH
      
      invisible(self)
    },
    
    to_string = function() {
      self$text %||% ""
    },
    
    set_text = function(text, source = NULL) {
      self$text <- text
      if (!is.null(source)) self$source <- source
      self$validate()
      invisible(self)
    },
    
    append = function(additional) {
      if (nzchar(self$text) && nzchar(additional)) {
        self$text <- paste(self$text, additional, sep = "; ")
      } else if (nzchar(additional)) {
        self$text <- additional
      }
      self$validate()
      invisible(self)
    }
  )
)

# =============================================================================
# VALUE RANGE CLASS
# =============================================================================

#' Value Range Type System
#'
#' Represents a structured value range with parsing, comparison, and merging
#' 
#' @field type Character - "categorical", "range", "mixed", "empty"
#' @field codes Character vector - numeric codes: c("0", "1", "2")
#' @field labels Named character vector - c("0"="No", "1"="Yes")
#' @field ranges List of numeric ranges - list(c(0, 10), c(20, 30))
#' @field raw Character - original string
#' @field source Character - "nda", "redcap", "data", "computed"
#' @field is_shifted Logical - TRUE if detected as shifted version
#' @field shift_offset Integer - offset amount if shifted
#' 
#' @keywords internal
#' @noRd
ValueRange <- R6::R6Class("ValueRange",
  public = list(
    type = NULL,
    codes = NULL,
    labels = NULL,
    ranges = NULL,
    raw = NULL,
    source = NULL,
    is_shifted = FALSE,
    shift_offset = 0,
    
    initialize = function(raw_string = "", source = "unknown") {
      self$raw <- raw_string
      self$source <- source
      if (nzchar(raw_string)) {
        self$parse()
      } else {
        self$type <- "empty"
      }
    },
    
    parse = function() {
      if (is.null(self$raw) || !nzchar(self$raw)) {
        self$type <- "empty"
        return(invisible(self))
      }
      
      parts <- trimws(strsplit(self$raw, ";")[[1]])
      parts <- parts[nzchar(parts)]
      
      if (length(parts) == 0) {
        self$type <- "empty"
        return(invisible(self))
      }
      
      has_labels <- any(grepl("=", parts))
      has_ranges <- any(grepl("::", parts, fixed = TRUE))
      
      if (has_labels) {
        self$type <- if (has_ranges) "mixed" else "categorical"
        self$parse_categorical(parts)
      } else if (has_ranges) {
        self$type <- "range"
        self$parse_ranges(parts)
      } else {
        self$type <- "categorical"
        self$codes <- parts
      }
      
      invisible(self)
    },
    
    parse_categorical = function(parts) {
      self$codes <- character(0)
      self$labels <- character(0)
      
      for (part in parts) {
        if (grepl("=", part)) {
          split_part <- strsplit(part, "=")[[1]]
          if (length(split_part) >= 2) {
            code <- trimws(split_part[1])
            label <- trimws(paste(split_part[-1], collapse = "="))
            self$codes <- c(self$codes, code)
            self$labels <- c(self$labels, label)
            names(self$labels)[length(self$labels)] <- code
          }
        } else if (grepl("::", part, fixed = TRUE)) {
          self$parse_ranges(part)
        } else {
          self$codes <- c(self$codes, trimws(part))
        }
      }
    },
    
    parse_ranges = function(parts) {
      self$ranges <- list()
      
      for (part in parts) {
        if (grepl("::", part, fixed = TRUE)) {
          bounds <- strsplit(part, "::", fixed = TRUE)[[1]]
          if (length(bounds) == 2) {
            start <- suppressWarnings(as.numeric(trimws(bounds[1])))
            end <- suppressWarnings(as.numeric(trimws(bounds[2])))
            if (!is.na(start) && !is.na(end)) {
              self$ranges[[length(self$ranges) + 1]] <- c(start, end)
            }
          }
        }
      }
    },
    
    to_nda_format = function() {
      if (self$type == "empty") return("")
      
      if (self$type == "categorical" && !is.null(self$labels) && length(self$labels) > 0) {
        return(paste(self$codes, collapse = ";"))
      } else if (self$type == "range" && !is.null(self$ranges)) {
        range_strings <- sapply(self$ranges, function(r) {
          paste0(as.integer(r[1]), "::", as.integer(r[2]))
        })
        return(paste(range_strings, collapse = ";"))
      } else if (!is.null(self$codes)) {
        return(paste(self$codes, collapse = ";"))
      }
      
      return(self$raw)
    },
    
    to_notes_format = function() {
      if (is.null(self$labels) || length(self$labels) == 0) {
        return("")
      }
      
      pairs <- sapply(seq_along(self$labels), function(i) {
        code <- names(self$labels)[i]
        label <- self$labels[i]
        paste0(code, "=", label)
      })
      
      paste(pairs, collapse = ";")
    },
    
    get_numeric_codes = function() {
      numeric_codes <- suppressWarnings(as.numeric(self$codes))
      numeric_codes[!is.na(numeric_codes)]
    },
    
    contains_code = function(code) {
      code_str <- as.character(code)
      
      if (code_str %in% self$codes) return(TRUE)
      
      if (!is.null(self$ranges) && length(self$ranges) > 0) {
        code_num <- suppressWarnings(as.numeric(code_str))
        if (!is.na(code_num)) {
          for (range in self$ranges) {
            if (code_num >= range[1] && code_num <= range[2]) {
              return(TRUE)
            }
          }
        }
      }
      
      FALSE
    },
    
    merge_with = function(other, strategy = "append_new") {
      if (strategy == "append_new") {
        new_codes <- setdiff(other$codes, self$codes)
        
        if (length(new_codes) > 0) {
          self$codes <- c(self$codes, new_codes)
          
          if (!is.null(other$labels)) {
            for (code in new_codes) {
              if (code %in% names(other$labels)) {
                self$labels <- c(self$labels, other$labels[code])
                names(self$labels)[length(self$labels)] <- code
              }
            }
          }
        }
      } else if (strategy == "override") {
        self$codes <- other$codes
        self$labels <- other$labels
        self$ranges <- other$ranges
        self$type <- other$type
      }
      
      invisible(self)
    }
  )
)

# =============================================================================
# NOTES CLASS
# =============================================================================

#' Notes Management System
#'
#' Manages field notes including code mappings, descriptions, and metadata
#' 
#' @field text Character - main notes text
#' @field code_mappings Named character vector - code=label pairs
#' @field metadata_notes Character vector - additional metadata notes
#' @field source Character - where notes came from
#' 
#' @keywords internal
#' @noRd
Notes <- R6::R6Class("Notes",
  public = list(
    text = NULL,
    code_mappings = NULL,
    metadata_notes = NULL,
    source = NULL,
    
    initialize = function(text = "", source = "unknown") {
      self$text <- text
      self$source <- source
      self$code_mappings <- character(0)
      self$metadata_notes <- character(0)
      
      if (nzchar(text)) {
        self$parse()
      }
    },
    
    parse = function() {
      if (is.null(self$text) || !nzchar(self$text)) return(invisible(self))
      
      parts <- trimws(strsplit(self$text, "[|;]")[[1]])
      
      for (part in parts) {
        if (grepl("=", part)) {
          split_part <- strsplit(part, "=")[[1]]
          if (length(split_part) >= 2) {
            code <- trimws(split_part[1])
            label <- trimws(paste(split_part[-1], collapse = "="))
            self$code_mappings <- c(self$code_mappings, label)
            names(self$code_mappings)[length(self$code_mappings)] <- code
          }
        } else if (nzchar(part)) {
          self$metadata_notes <- c(self$metadata_notes, part)
        }
      }
      
      invisible(self)
    },
    
    add_mapping = function(code, label) {
      self$code_mappings <- c(self$code_mappings, label)
      names(self$code_mappings)[length(self$code_mappings)] <- code
      self$rebuild_text()
      invisible(self)
    },
    
    add_note = function(note) {
      if (nzchar(note) && !(note %in% self$metadata_notes)) {
        self$metadata_notes <- c(self$metadata_notes, note)
        self$rebuild_text()
      }
      invisible(self)
    },
    
    merge_with = function(other) {
      if (!is.null(other$code_mappings) && length(other$code_mappings) > 0) {
        for (code in names(other$code_mappings)) {
          self$code_mappings[code] <- other$code_mappings[code]
        }
      }
      
      if (!is.null(other$metadata_notes) && length(other$metadata_notes) > 0) {
        new_notes <- setdiff(other$metadata_notes, self$metadata_notes)
        self$metadata_notes <- c(self$metadata_notes, new_notes)
      }
      
      self$rebuild_text()
      invisible(self)
    },
    
    rebuild_text = function() {
      parts <- character(0)
      
      if (length(self$code_mappings) > 0) {
        mappings <- sapply(seq_along(self$code_mappings), function(i) {
          code <- names(self$code_mappings)[i]
          label <- self$code_mappings[i]
          paste0(code, "=", label)
        })
        parts <- c(parts, mappings)
      }
      
      if (length(self$metadata_notes) > 0) {
        parts <- c(parts, self$metadata_notes)
      }
      
      self$text <- paste(parts, collapse = ";")
      invisible(self)
    },
    
    to_string = function() {
      self$text %||% ""
    }
  )
)

# =============================================================================
# ALIASES CLASS
# =============================================================================

#' Field Aliases Management
#'
#' Manages field aliases with proper formatting (no c() wrapper)
#' 
#' @field aliases Character vector - list of aliases
#' @field source Character - where aliases came from
#' 
#' @keywords internal
#' @noRd
Aliases <- R6::R6Class("Aliases",
  public = list(
    aliases = NULL,
    source = NULL,
    
    initialize = function(aliases = NULL, source = "unknown") {
      self$source <- source
      self$set_aliases(aliases)
    },
    
    set_aliases = function(aliases) {
      if (is.null(aliases)) {
        self$aliases <- character(0)
        return(invisible(self))
      }
      
      # Handle list from NDA API (THIS FIXES THE c() WRAPPER BUG)
      if (is.list(aliases)) {
        aliases <- unlist(aliases, use.names = FALSE)
      }
      
      # Handle comma-separated string
      if (is.character(aliases) && length(aliases) == 1 && grepl(",", aliases)) {
        aliases <- trimws(strsplit(aliases, ",")[[1]])
      }
      
      # Clean up
      aliases <- as.character(aliases)
      aliases <- aliases[!is.na(aliases) & nzchar(aliases)]
      aliases <- unique(aliases)
      
      self$aliases <- aliases
      invisible(self)
    },
    
    add = function(alias) {
      if (!is.null(alias) && nzchar(alias) && !(alias %in% self$aliases)) {
        self$aliases <- c(self$aliases, alias)
      }
      invisible(self)
    },
    
    to_string = function() {
      if (length(self$aliases) == 0) return("")
      paste(self$aliases, collapse = ",")
    },
    
    to_vector = function() {
      self$aliases
    },
    
    has = function(alias) {
      alias %in% self$aliases
    },
    
    count = function() {
      length(self$aliases)
    }
  )
)

# =============================================================================
# VALIDATION RULES CLASS
# =============================================================================

#' Validation Rules Management
#'
#' Stores and validates field-level validation rules
#' 
#' @field allowed_values Character vector - permitted values
#' @field min_value Numeric - minimum allowed value
#' @field max_value Numeric - maximum allowed value
#' @field data_type Character - expected data type
#' @field custom_rules List - custom validation functions
#' 
#' @keywords internal
#' @noRd
ValidationRules <- R6::R6Class("ValidationRules",
  public = list(
    allowed_values = NULL,
    min_value = NULL,
    max_value = NULL,
    data_type = NULL,
    custom_rules = NULL,
    
    initialize = function() {
      self$allowed_values <- character(0)
      self$custom_rules <- list()
    },
    
    set_from_value_range = function(value_range) {
      if (is.null(value_range)) return(invisible(self))
      
      if (!is.null(value_range$codes)) {
        self$allowed_values <- value_range$codes
      }
      
      if (!is.null(value_range$ranges) && length(value_range$ranges) > 0) {
        all_bounds <- unlist(value_range$ranges)
        self$min_value <- min(all_bounds)
        self$max_value <- max(all_bounds)
      }
      
      invisible(self)
    },
    
    validate = function(value) {
      errors <- character(0)
      
      if (length(self$allowed_values) > 0) {
        if (!(as.character(value) %in% self$allowed_values)) {
          errors <- c(errors, sprintf("Value '%s' not in allowed values", value))
        }
      }
      
      if (!is.null(self$min_value) || !is.null(self$max_value)) {
        num_val <- suppressWarnings(as.numeric(value))
        if (!is.na(num_val)) {
          if (!is.null(self$min_value) && num_val < self$min_value) {
            errors <- c(errors, sprintf("Value %s below minimum %s", num_val, self$min_value))
          }
          if (!is.null(self$max_value) && num_val > self$max_value) {
            errors <- c(errors, sprintf("Value %s above maximum %s", num_val, self$max_value))
          }
        }
      }
      
      list(valid = length(errors) == 0, errors = errors)
    },
    
    to_list = function() {
      list(
        allowed_values = self$allowed_values,
        min_value = self$min_value,
        max_value = self$max_value,
        data_type = self$data_type
      )
    }
  )
)

# =============================================================================
# MISSING INFO CLASS
# =============================================================================

#' Missing Data Information
#'
#' Tracks missing data statistics for a field
#' 
#' @field missing_count Integer - number of missing values
#' @field total_count Integer - total values
#' @field missing_percentage Numeric - percentage missing
#' @field missing_codes Character vector - codes treated as missing
#' 
#' @keywords internal
#' @noRd
MissingInfo <- R6::R6Class("MissingInfo",
  public = list(
    missing_count = NULL,
    total_count = NULL,
    missing_percentage = NULL,
    missing_codes = NULL,
    
    initialize = function(missing_count = 0, total_count = 0, missing_codes = NULL) {
      self$missing_count <- missing_count
      self$total_count <- total_count
      self$missing_codes <- missing_codes
      self$calculate_percentage()
    },
    
    calculate_percentage = function() {
      if (self$total_count > 0) {
        self$missing_percentage <- round((self$missing_count / self$total_count) * 100, 1)
      } else {
        self$missing_percentage <- 0
      }
      invisible(self)
    },
    
    to_list = function() {
      list(
        missing_count = self$missing_count,
        missing_percentage = self$missing_percentage,
        total_count = self$total_count,
        missing_codes = self$missing_codes
      )
    }
  )
)

# =============================================================================
# SOURCE METADATA CLASS
# =============================================================================

#' Source Metadata Tracking
#'
#' Tracks where field metadata came from for provenance
#' 
#' @field primary_source Character - "nda", "redcap", "qualtrics", "data", "computed"
#' @field nda_matched Logical - found in NDA dictionary
#' @field redcap_matched Logical - found in REDCap dictionary
#' @field is_modified Logical - modified from original source
#' @field modifications Character vector - list of modifications made
#' 
#' @keywords internal
#' @noRd
SourceMetadata <- R6::R6Class("SourceMetadata",
  public = list(
    primary_source = NULL,
    nda_matched = NULL,
    redcap_matched = NULL,
    is_modified = NULL,
    modifications = NULL,
    
    initialize = function(primary_source = "unknown") {
      self$primary_source <- primary_source
      self$nda_matched <- FALSE
      self$redcap_matched <- FALSE
      self$is_modified <- FALSE
      self$modifications <- character(0)
    },
    
    mark_nda_match = function() {
      self$nda_matched <- TRUE
      if (self$primary_source == "unknown") {
        self$primary_source <- "nda"
      }
      invisible(self)
    },
    
    mark_redcap_match = function() {
      self$redcap_matched <- TRUE
      if (self$primary_source == "unknown") {
        self$primary_source <- "redcap"
      }
      invisible(self)
    },
    
    add_modification = function(modification) {
      self$is_modified <- TRUE
      self$modifications <- c(self$modifications, modification)
      invisible(self)
    },
    
    to_string = function() {
      parts <- c(self$primary_source)
      if (self$nda_matched) parts <- c(parts, "nda_matched")
      if (self$redcap_matched) parts <- c(parts, "redcap_matched")
      if (self$is_modified) {
        parts <- c(parts, sprintf("modified(%d)", length(self$modifications)))
      }
      paste(parts, collapse = "|")
    }
  )
)
