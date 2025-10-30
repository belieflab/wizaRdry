#' @noRd
createNdaDataDefinition <- function(submission_template, nda_structure, measure_name, data_frame = NULL) {

  # Try to get the data frame from the global environment if not provided
  if (is.null(data_frame)) {
    data_frame <- tryCatch({
      base::get0(measure_name)
    }, error = function(e) NULL)
  }

  # Load missing data codes from config if available
  missing_data_codes <- NULL
  tryCatch({
    config_env <- ConfigEnv$new("config.yml")
    missing_data_codes <- config_env$get_missing_data_codes()
  }, error = function(e) {
    # Silently fail if config can't be loaded
    missing_data_codes <- NULL
  })

  # Build optional description maps from external sources (REDCap/Qualtrics)
  redcap_label_map <- NULL
  redcap_choices_map <- NULL
  qualtrics_label_map <- NULL

  # Attempt to fetch REDCap metadata and map field_name -> field_label and field_name -> select_choices_or_calculations
  try({
    # Prefer using the provided data_frame if it carries instrument attribute
    if (is.data.frame(data_frame) && !is.null(attr(data_frame, "redcap_instrument"))) {
      rc_meta <- redcap.dict(data_frame)
    } else {
      rc_meta <- redcap.dict(measure_name)
    }
    if (is.data.frame(rc_meta) && all(c("field_name", "field_label") %in% names(rc_meta))) {
      # Create a named vector mapping field names to labels
      redcap_label_map <- rc_meta$field_label
      names(redcap_label_map) <- rc_meta$field_name

      # Create a named vector mapping field names to select_choices_or_calculations for ValueRange
      if ("select_choices_or_calculations" %in% names(rc_meta)) {
        redcap_choices_map <- rc_meta$select_choices_or_calculations
        names(redcap_choices_map) <- rc_meta$field_name
        # Remove NA values
        redcap_choices_map <- redcap_choices_map[!is.na(redcap_choices_map) & redcap_choices_map != ""]
      } else {
        redcap_choices_map <- NULL
      }
    } else {
      redcap_choices_map <- NULL
    }
  }, silent = TRUE)

  # Function to parse RedCap select_choices_or_calculations and convert to NDA ValueRange format
  parse_redcap_choices_to_value_range <- function(choices_string) {
    if (is.null(choices_string) || is.na(choices_string) || choices_string == "") {
      return("")
    }

    # Split by pipe (|) to get individual choices
    choices <- strsplit(choices_string, "\\|")[[1]]
    choices <- trimws(choices)

    # Parse each choice (format: "value, label" or "value, label")
    parsed_choices <- character(0)
    for (choice in choices) {
      if (choice != "") {
        # Split by comma to separate value and label
        parts <- strsplit(choice, ",")[[1]]
        if (length(parts) >= 2) {
          value <- trimws(parts[1])
          label <- trimws(paste(parts[-1], collapse = ","))  # In case label contains commas
          # Format as NDA ValueRange: "value; label"
          parsed_choices <- c(parsed_choices, paste0(value, "; ", label))
        } else if (length(parts) == 1) {
          # If no comma, treat the whole thing as a value
          parsed_choices <- c(parsed_choices, trimws(parts[1]))
        }
      }
    }

    # Join with semicolons for NDA format
    return(paste(parsed_choices, collapse = "; "))
  }

  # Function to infer value ranges from data tuples when choices are not available
  infer_value_range_from_data <- function(data_vector, data_type) {
    if (is.null(data_vector) || length(data_vector) == 0) {
      return("")
    }

    # Remove NAs for analysis
    clean_data <- data_vector[!is.na(data_vector)]
    if (length(clean_data) == 0) {
      return("")
    }

    unique_vals <- unique(clean_data)
    unique_count <- length(unique_vals)

    if (data_type == "Integer") {
      # For integers, check if values look like categorical codes
      if (unique_count <= 20 && all(clean_data >= 0 & clean_data <= 99)) {
        # Likely categorical data - list all values
        sorted_vals <- sort(as.numeric(unique_vals))
        return(paste(sorted_vals, collapse = ";"))
      } else {
        # Likely continuous data - use range notation
        min_val <- min(clean_data, na.rm = TRUE)
        max_val <- max(clean_data, na.rm = TRUE)
        return(paste0(as.integer(min_val), "::", as.integer(max_val)))
      }
    } else if (data_type == "String") {
      # For strings, list unique values if reasonable number
      if (unique_count <= 20) {
        sorted_vals <- sort(as.character(unique_vals))
        return(paste(sorted_vals, collapse = ";"))
      } else {
        return(paste0("Multiple categorical values (", unique_count, " unique)"))
      }
    } else if (data_type == "Float") {
      # For floats, don't specify value ranges (too restrictive)
      return("")
    }

    return("")
  }

  # Helper: clean HTML breaks from labels
  rc_clean_label <- function(x) {
    if (is.null(x) || is.na(x)) return("")
    gsub("<br>", " ", x, fixed = TRUE)
  }

  # Helper: enrich REDCap checkbox children (e.g., parent___1 or parent_other)
  # Using parent field's label and choices to derive description/valueRange/notes
  rc_enrich_checkbox_child <- function(column_name, rc_meta) {
    out <- list(description = NULL, valueRange = NULL, notes = NULL, data_type = NULL, size = NULL)
    if (is.null(rc_meta) || !is.data.frame(rc_meta) || !all(c("field_name", "field_type") %in% names(rc_meta))) return(out)

    parent <- NULL
    suffix <- NULL
    if (grepl("___", column_name, fixed = TRUE)) {
      parent <- sub("___.*$", "", column_name)
      suffix <- sub("^.*___", "", column_name)
    } else if (grepl("_other$", column_name)) {
      parent <- sub("_other$", "", column_name)
      suffix <- "other"
    }

    # Attempt to infer parent via branching_logic if name heuristic fails
    if ((is.null(parent) || !(parent %in% rc_meta$field_name)) && "branching_logic" %in% names(rc_meta)) {
      this_row <- rc_meta[rc_meta$field_name == column_name, , drop = FALSE]
      if (nrow(this_row) == 1) {
        bl <- as.character(this_row$branching_logic %||% "")
        m <- regmatches(bl, regexpr("\\[([A-Za-z0-9_]+)\\(", bl, perl = TRUE))
        if (length(m) == 1 && nzchar(m)) {
          parent_candidate <- sub("^\\[", "", m)
          parent_candidate <- sub("\\($", "", parent_candidate)
          if (parent_candidate %in% rc_meta$field_name) parent <- parent_candidate
        }
      }
    }

    if (is.null(parent) || parent == "") return(out)

    parent_row <- rc_meta[rc_meta$field_name == parent, , drop = FALSE]
    if (nrow(parent_row) != 1) return(out)
    if (!identical(as.character(parent_row$field_type), "checkbox")) return(out)

    parent_label <- as.character(parent_row$field_label %||% "")
    parent_label <- rc_clean_label(parent_label)
    choices <- as.character(parent_row$select_choices_or_calculations %||% "")

    # Build map from coded value -> label
    choice_map <- list()
    if (nzchar(choices)) {
      parts <- strsplit(choices, "\\|")[[1]]
      for (p in trimws(parts)) {
        kv <- strsplit(p, ",")[[1]]
        if (length(kv) >= 2) {
          key <- trimws(kv[1])
          val <- trimws(paste(kv[-1], collapse = ","))
          val <- rc_clean_label(val)
          choice_map[[key]] <- val
        }
      }
    }

    # Numeric child like parent___1
    if (!is.null(suffix) && grepl("^[0-9]+$", suffix)) {
      label <- choice_map[[suffix]] %||% parent_label
      # Compose description as single line: Parent label - choice label
      if (nzchar(parent_label) && nzchar(label) && parent_label != label) {
        out$description <- paste0(parent_label, " - ", label)
      } else {
        out$description <- label
      }
      out$valueRange <- "0;1"
      out$notes <- "0=No;1=Yes"
      # Per current expectation: keep as String with Size 255
      out$data_type <- "String"
      out$size <- 255
      return(out)
    }

    # Other text child
    if (!is.null(suffix) && identical(tolower(suffix), "other")) {
      out$description <- paste0(parent_label, ": If you selected 'other', please describe.")
      out$valueRange <- ""
      out$notes <- ""
      out$data_type <- "String"
      out$size <- 255
      return(out)
    }

    out
  }

  # Helper: parse REDCap choices -> numeric codes only for ValueRange + mapping text for Notes
  rc_parse_choices_codes_and_notes <- function(choices_string) {
    if (is.null(choices_string) || is.na(choices_string) || choices_string == "") {
      return(list(codes = "", notes = ""))
    }
    parts <- strsplit(choices_string, "\\|")[[1]]
    parts <- trimws(parts)
    codes <- character(0)
    note_pairs <- character(0)
    for (p in parts) {
      if (p == "") next
      kv <- strsplit(p, ",")[[1]]
      if (length(kv) >= 2) {
        code <- trimws(kv[1])
        label <- trimws(paste(kv[-1], collapse = ","))
        label <- rc_clean_label(label)
        codes <- c(codes, code)
        note_pairs <- c(note_pairs, paste0(code, "=", label))
      } else {
        # fallback: if no comma, treat as bare code
        code <- trimws(kv[1])
        codes <- c(codes, code)
      }
    }
    list(
      codes = if (length(codes)) paste(codes, collapse = "; ") else "",
      notes = if (length(note_pairs)) paste(note_pairs, collapse = "; ") else ""
    )
  }

  # Helper: use REDCap validation/type to derive ValueRange when appropriate
  rc_derive_value_range_from_validation <- function(meta_row) {
    if (is.null(meta_row) || nrow(meta_row) == 0) return("")
    vtype <- meta_row$text_validation_type_or_show_slider_number
    vmin <- suppressWarnings(as.numeric(meta_row$text_validation_min))
    vmax <- suppressWarnings(as.numeric(meta_row$text_validation_max))
    ftype <- meta_row$field_type

    # Integer-like
    if (!is.null(vtype) && !is.na(vtype) && grepl("integer", vtype, ignore.case = TRUE)) {
      if (!is.na(vmin) && !is.na(vmax)) return(paste0(as.integer(vmin), "::", as.integer(vmax)))
      return("")
    }

    # Slider (numeric)
    if (!is.null(vtype) && !is.na(vtype) && grepl("slider|number", vtype, ignore.case = TRUE)) {
      if (!is.na(vmin) && !is.na(vmax)) return(paste0(vmin, "::", vmax))
      return("")
    }

    # Field type specific
    if (!is.null(ftype) && ftype %in% c("radio", "checkbox", "dropdown")) {
      # Handled via choices, not validation
      return("")
    }

    # Date/time: let NDA handle as type; no numeric range
    if (!is.null(vtype) && grepl("date|datetime", vtype, ignore.case = TRUE)) {
      return("")
    }

    return("")
  }

  # Attempt to fetch Qualtrics column map and map variable -> question text
  try({
    # qualtrics.dict accepts either a data frame or an alias
    if (is.data.frame(data_frame)) {
      q_map <- qualtrics.dict(data_frame)
    } else {
      q_map <- qualtrics.dict(measure_name)
    }
    if (is.data.frame(q_map)) {
      # Heuristically identify variable and question-text columns
      var_cols <- c("qname", "VariableName", "variable", "Variable", "col_name", "column", "Column", "name", "Name")
      txt_cols <- c("description", "QuestionText", "question_text", "Question", "Label", "label", "Description")
      var_col <- var_cols[var_cols %in% names(q_map)]
      txt_col <- txt_cols[txt_cols %in% names(q_map)]
      if (length(var_col) > 0 && length(txt_col) > 0) {
        var_col <- var_col[1]
        txt_col <- txt_col[1]
        # Filter out NAs and create mapping
        valid_rows <- !is.na(q_map[[var_col]]) & !is.na(q_map[[txt_col]])
        if (any(valid_rows)) {
          qualtrics_label_map <- q_map[[txt_col]][valid_rows]
          names(qualtrics_label_map) <- q_map[[var_col]][valid_rows]
        }
      }
    }
  }, silent = TRUE)

  # Handle both character vector and list formats
  if (is.character(submission_template)) {
    submission_template <- list(columns = submission_template)
  }

  # Validate inputs
  if (!is.list(submission_template)) {
    stop("submission_template must be a list or character vector")
  }

  if (!is.list(nda_structure)) {
    stop("nda_structure must be a list")
  }

  if (missing(measure_name) || !is.character(measure_name)) {
    stop("measure_name must be provided as a character string")
  }

  # Extract selected columns from submission template
  # Handle different possible structures
  selected_columns <- NULL
  if ("columns" %in% names(submission_template)) {
    selected_columns <- submission_template$columns
  } else if ("selected_fields" %in% names(submission_template)) {
    selected_columns <- submission_template$selected_fields
  } else if (is.character(submission_template)) {
    selected_columns <- submission_template
  } else {
    stop("Could not extract selected columns from submission_template")
  }

  # Define fields that should never be included as new or modified structures
  # They can still be included only if part of essential ndar_subject01 and unchanged
  excluded_from_change <- c("state", "lost_to_followup", "lost_to_follow-up", "study_status")

  # Get essential NDA fields dynamically from ndar_subject01 API
  # This ensures we always have the current required elements with complete metadata
  essential_nda_fields <- tryCatch({
    # Try to get required field metadata from the global environment or nda_structure
    required_metadata <- NULL

    # First, try to get from nda_structure if it contains ndar_subject01 data
    if (!is.null(nda_structure) && "dataElements" %in% names(nda_structure)) {
      required_elements <- nda_structure$dataElements[nda_structure$dataElements$required == "Required", ]
      if (nrow(required_elements) > 0) {
        required_metadata <- required_elements
      }
    }

    # If not found in nda_structure, try to fetch from API directly
    if (is.null(required_metadata)) {
      message("Fetching current ndar_subject01 required elements from NDA API...")
      nda_base_url <- "https://nda.nih.gov/api/datadictionary/v2"
      url <- sprintf("%s/datastructure/ndar_subject01", nda_base_url)

      response <- httr::GET(url, httr::timeout(10))
      if (httr::status_code(response) == 200) {
        raw_content <- rawToChar(response$content)
        if (nchar(raw_content) > 0) {
          subject_structure <- jsonlite::fromJSON(raw_content)
          if ("dataElements" %in% names(subject_structure)) {
            required_metadata <- subject_structure$dataElements[subject_structure$dataElements$required == "Required", ]
          }
        }
      }
    }

    # Extract field names from metadata
    if (!is.null(required_metadata) && nrow(required_metadata) > 0) {
      field_names <- required_metadata$name
      # Exclude internal fields that should never appear
      field_names <- setdiff(field_names, excluded_from_change)
      message(sprintf("Found %d required ndar_subject01 elements: %s",
                      length(field_names), paste(head(field_names, 5), collapse = ", ")))
      field_names
    } else {
      # Fallback to essential fields if API fails
      message("Using fallback essential fields list")
      setdiff(c("src_subject_id", "subjectkey", "sex", "interview_age", "interview_date",
        "phenotype", "site", "race", "handedness", "visit", "week", "redcap_event_name"), excluded_from_change)
    }
  }, error = function(e) {
    message("Error fetching ndar_subject01 elements: ", e$message)
    message("Using fallback essential fields list")
    setdiff(c("src_subject_id", "subjectkey", "sex", "interview_age", "interview_date",
      "phenotype", "site", "race", "handedness", "visit", "week", "redcap_event_name"), excluded_from_change)
  })

  # Convert to character vector if needed
  if (is.data.frame(selected_columns)) {
    selected_columns <- names(selected_columns)
  } else if (is.list(selected_columns) && all(sapply(selected_columns, is.character))) {
    selected_columns <- unlist(selected_columns)
  }

  # Extract NDA data elements
  nda_elements <- NULL
  if ("dataElements" %in% names(nda_structure)) {
    nda_elements <- nda_structure$dataElements
  } else if ("fields" %in% names(nda_structure)) {
    nda_elements <- nda_structure$fields
  } else {
    warning("Could not find dataElements or fields in nda_structure")
    nda_elements <- list()
  }

  # Create lookup map for NDA elements
  nda_lookup <- list()
  if (is.data.frame(nda_elements)) {
    # If it's a dataframe, create lookup by name
    for (i in seq_len(nrow(nda_elements))) {
      element_name <- nda_elements$name[i]
      if (!is.na(element_name)) {
        nda_lookup[[element_name]] <- as.list(nda_elements[i, ])
      }
    }
  } else if (is.list(nda_elements)) {
    # If it's already a list, use as is or extract names
    for (element in nda_elements) {
      if (is.list(element) && "name" %in% names(element)) {
        nda_lookup[[element$name]] <- element
      }
    }
  }

  # Helper function to compute metadata from actual data
  compute_field_metadata <- function(field_name, data_vector) {
    if (is.null(data_vector) || length(data_vector) == 0) {
      return(list(
        data_type = "String",
        description = "",
        size = 255,
        valueRange = "",
        required = "Recommended",
        notes = "",
        aliases = ""
      ))
    }

    # Remove NAs for analysis
    clean_data <- data_vector[!is.na(data_vector)]
    total_count <- length(data_vector)
    valid_count <- length(clean_data)
    missing_count <- total_count - valid_count

    if (valid_count == 0) {
      return(list(
        data_type = "String",
        description = "",
        size = 255,
        valueRange = "",
        required = "Recommended",
        notes = "",
        aliases = "",
        missing_info = list(
          missing_count = missing_count,
          missing_percentage = 100.0,
          total_count = total_count
        )
      ))
    }

    # Determine data type based on R class and content
    data_type <- "String"  # default
    size <- 255  # default for strings

    # Check actual R class first
    if (is.integer(data_vector) || (is.numeric(data_vector) && all(clean_data == round(clean_data), na.rm = TRUE))) {
      data_type <- "Integer"
      # For integers, size is based on the maximum number of digits
      max_val <- max(abs(clean_data), na.rm = TRUE)
      size <- max(nchar(as.character(max_val)), 1)
    } else if (is.numeric(data_vector) || is.double(data_vector)) {
      data_type <- "Float"
      # For floats, size includes decimal places
      max_chars <- max(nchar(as.character(clean_data)), na.rm = TRUE)
      size <- min(max_chars, 50)  # Cap at reasonable size
    } else if (is.logical(data_vector)) {
      data_type <- "Integer"  # NDA uses 0/1 for booleans
      size <- 1
    } else if (is.character(data_vector) || is.factor(data_vector)) {
      data_type <- "String"
      # For strings, size is the maximum character length
      char_lengths <- nchar(as.character(clean_data))
      size <- max(char_lengths, na.rm = TRUE)
      if (is.na(size) || size < 1) size <- 255
    } else {
      # Fallback for other types
      data_type <- "String"
      size <- max(nchar(as.character(clean_data)), na.rm = TRUE)
      if (is.na(size) || size < 1) size <- 255
    }

    # Compute value range based on data type
    unique_vals <- unique(clean_data)
    unique_count <- length(unique_vals)
    value_range <- ""

    # Detect user-defined missing value codes from config and data
    user_defined_codes <- character(0)
    if (data_type %in% c("Integer", "Float") && !is.null(missing_data_codes)) {
      # Check for missing data codes defined in config
      for (category in names(missing_data_codes)) {
        if (category != "required" && category != "types" && category != "aliases" && category != "allow_custom") {
          category_values <- missing_data_codes[[category]]
          if (is.character(category_values) || is.numeric(category_values)) {
            for (code in category_values) {
              if (as.numeric(code) %in% data_vector) {
                user_defined_codes <- c(user_defined_codes, as.character(code))
              }
            }
          }
        }
      }
    }

    # Use the enhanced inference function to determine value range
    value_range <- infer_value_range_from_data(data_vector, data_type)

    # Append user-defined missing value codes if they exist
    if (length(user_defined_codes) > 0) {
      if (value_range == "") {
        value_range <- paste(user_defined_codes, collapse = ";")
      } else {
        value_range <- paste0(value_range, ";", paste(user_defined_codes, collapse = ";"))
      }
    }

    # Generate smart description based on field name patterns and data
    description <- generate_field_description(field_name, clean_data, data_type)

    # All computed fields are marked as "Recommended" (requirement 3)
    required <- "Recommended"

    # Keep notes minimal for all field types
    notes_parts <- c()

    # Calculate missing percentage for missing_info (still needed)
    missing_pct <- round((missing_count / total_count) * 100, 1)

    # Document user-defined missing value codes if they exist
    if (length(user_defined_codes) > 0 && !is.null(missing_data_codes)) {
      code_descriptions <- character(0)
      for (code in user_defined_codes) {
        # Find which category this code belongs to
        for (category in names(missing_data_codes)) {
          if (category != "required" && category != "types" && category != "aliases" && category != "allow_custom") {
            category_values <- missing_data_codes[[category]]
            if (as.numeric(code) %in% category_values) {
              code_descriptions <- c(code_descriptions, paste0(code, " = ", category))
              break
            }
          }
        }
      }
      if (length(code_descriptions) > 0) {
        notes_parts <- c(notes_parts, paste0("User-defined codes: ", paste(code_descriptions, collapse = ", ")))
      }
    }

    # Do not append Mean/SD or other statistical summaries to notes

    notes <- paste(notes_parts, collapse = "; ")

    return(list(
      data_type = data_type,
      description = description,
      size = size,
      valueRange = value_range,
      required = required,
      aliases = "",
      notes = notes,
      missing_info = list(
        missing_count = missing_count,
        missing_percentage = missing_pct,
        total_count = total_count
      )
    ))
  }

  # Enhanced helper function to generate smart descriptions
  generate_field_description <- function(field_name, data_vector, data_type) {
    # Pattern matching for common field types
    name_lower <- tolower(field_name)

    # Handle specific patterns first
    if (grepl("^(src_)?subject", name_lower)) {
      return("Subject identifier")
    } else if (grepl("subjectkey|guid", name_lower)) {
      return("Global unique identifier")
    } else if (grepl("interview_age|age", name_lower)) {
      return("Age at interview (in months)")
    } else if (grepl("interview_date|date", name_lower)) {
      return("Interview date")
    } else if (grepl("^sex$|gender", name_lower)) {
      return("Biological sex")
    } else if (grepl("handedness", name_lower)) {
      return("Handedness preference")
    } else if (grepl("arm|group|condition", name_lower)) {
      return("Study arm or condition assignment")
    }

    # Pattern matching for survey/task items
    if (grepl("attention|check", name_lower)) {
      return("Attention check or validation item")
    } else if (grepl("trial|item_\\d+", name_lower)) {
      return("Trial or item response")
    } else if (grepl("response|answer", name_lower)) {
      return("Response data")
    } else if (grepl("score|total", name_lower)) {
      if (data_type %in% c("Integer", "Float")) {
        min_val <- min(data_vector, na.rm = TRUE)
        max_val <- max(data_vector, na.rm = TRUE)
        return(paste0("Score ranging from ", min_val, " to ", max_val))
      } else {
        return("Score or total measurement")
      }
    } else if (grepl("time|duration|rt|latency", name_lower)) {
      if (data_type %in% c("Integer", "Float")) {
        return("Response time or duration (likely in milliseconds)")
      } else {
        return("Time measurement")
      }
    }

    # Task-specific patterns (based on your rgpts fields)
    if (grepl("rgpts|gpts", name_lower)) {
      if (grepl("_a#?\\d+", name_lower)) {
        return("RGPTS Scale A item response")
      } else if (grepl("_b#?\\d+", name_lower)) {
        return("RGPTS Scale B item response")
      } else {
        return("RGPTS questionnaire response")
      }
    }

    # Generic patterns based on data characteristics
    if (data_type == "Integer") {
      unique_count <- length(unique(data_vector[!is.na(data_vector)]))
      min_val <- min(data_vector, na.rm = TRUE)
      max_val <- max(data_vector, na.rm = TRUE)

      if (unique_count <= 10) {
        return(paste0("Numeric scale (", unique_count, " levels: ", min_val, "-", max_val, ")"))
      } else {
        return(paste0("Numeric measurement (range: ", min_val, "-", max_val, ")"))
      }
    } else if (data_type == "Float") {
      # For Float fields, don't generate generic "Numeric measurement" placeholder
      # Let other parts of the code handle meaningful descriptions
      return("")
    } else {
      unique_count <- length(unique(data_vector[!is.na(data_vector)]))
      if (unique_count <= 5) {
        return(paste0("Categorical variable (", unique_count, " categories)"))
      } else if (unique_count <= 20) {
        return(paste0("Categorical response (", unique_count, " possible values)"))
      } else {
        return(paste0("Text field (", unique_count, " unique values)"))
      }
    }
  }

  # Map REDCap field_type/validation to NDA data type
  rc_map_redcap_to_nda_type <- function(meta_row, current_type = NULL) {
    if (is.null(meta_row) || nrow(meta_row) == 0) return(current_type %||% "String")
    ftype <- as.character(meta_row$field_type %||% "")
    vtype <- as.character(meta_row$text_validation_type_or_show_slider_number %||% "")

    # Choice-based
    if (ftype %in% c("radio", "dropdown", "checkbox")) return("Integer")

    # Numeric validations
    if (nzchar(vtype)) {
      if (grepl("^integer$", vtype, ignore.case = TRUE)) return("Integer")
      if (grepl("^(number|float|decimal)$", vtype, ignore.case = TRUE)) return("Float")
      if (grepl("^slider|show_slider_number", vtype, ignore.case = TRUE)) return("Integer")
      if (grepl("date|datetime", vtype, ignore.case = TRUE)) return("Date")
      if (grepl("zipcode|email|phone", vtype, ignore.case = TRUE)) return("String")
    }

    # Calculated fields numeric
    if (ftype %in% c("calc")) return("Float")

    # Text default
    if (ftype %in% c("text", "notes")) return(current_type %||% "String")

    return(current_type %||% "String")
  }

  # For modified data definitions, focus ONLY on changes:
  # 1. New fields (not in NDA)
  # 2. Modified fields (in NDA but changed)
  # Exclude unchanged NDA fields
  # IMPORTANT: ndar_subject01 variables can NEVER be modified

  all_nda_fields <- names(nda_lookup)
  user_added_fields <- selected_columns[!selected_columns %in% all_nda_fields]

  # Identify ndar_subject01 variables that should never be modified
  ndar_subject01_fields <- all_nda_fields[grepl("^ndar_subject01", all_nda_fields)]

  # Initialize lists for different field categories
  new_fields <- character(0)
  modified_fields <- character(0)
  unchanged_fields <- character(0)

  if (!is.null(data_frame)) {
    # Categorize fields based on whether they exist in data and NDA
    for (field in selected_columns) {
      # Skip change categorization for excluded fields: force unchanged
      if (field %in% excluded_from_change) {
        if (field %in% names(data_frame) && field %in% all_nda_fields) {
          unchanged_fields <- c(unchanged_fields, field)
        }
        next
      }
      if (field %in% names(data_frame)) {
        if (field %in% all_nda_fields) {
          # Field exists in both data and NDA - check if it's modified
          if (field %in% ndar_subject01_fields) {
            # ndar_subject01 fields can never be modified
            unchanged_fields <- c(unchanged_fields, field)
          } else {
            # Check if this field has actual modifications
            is_modified <- FALSE

            # Check for missing value code modifications
            if (!is.null(missing_data_codes)) {
              field_data <- data_frame[[field]]
              field_type <- if ("type" %in% names(nda_lookup[[field]])) nda_lookup[[field]]$type else "String"

              if (field_type %in% c("Integer", "Float")) {
                # Check if field has missing value codes not in original NDA range
                original_value_range <- if ("valueRange" %in% names(nda_lookup[[field]])) nda_lookup[[field]]$valueRange else ""

                # Detect missing value codes in the data
                user_defined_codes <- character(0)
                for (category in names(missing_data_codes)) {
                  if (category != "required" && category != "types" && category != "aliases" && category != "allow_custom") {
                    category_values <- missing_data_codes[[category]]
                    if (is.character(category_values) || is.numeric(category_values)) {
                      for (code in category_values) {
                        if (as.numeric(code) %in% field_data) {
                          user_defined_codes <- c(user_defined_codes, as.character(code))
                        }
                      }
                    }
                  }
                }

                # Check if any of these codes are not in the original value range
                if (length(user_defined_codes) > 0) {
                  original_codes <- character(0)
                  if (original_value_range != "") {
                    # Parse original value range to extract individual codes
                    range_parts <- trimws(strsplit(original_value_range, ";")[[1]])
                    for (part in range_parts) {
                      if (!grepl("::", part)) {  # Skip range notation
                        original_codes <- c(original_codes, part)
                      }
                    }
                  }

                  # Find new codes that aren't in the original
                  new_codes <- setdiff(user_defined_codes, original_codes)
                  if (length(new_codes) > 0) {
                    is_modified <- TRUE
                  }
                }
              }
            }

            if (is_modified) {
              modified_fields <- c(modified_fields, field)
            } else {
              unchanged_fields <- c(unchanged_fields, field)
            }
          }
        } else {
          # Field exists in data but not in NDA - it's new
          if (!(field %in% excluded_from_change)) {
            new_fields <- c(new_fields, field)
          } else {
            unchanged_fields <- c(unchanged_fields, field)
          }
        }
      } else {
        # Field doesn't exist in data - skip it
        next
      }
    }
  } else {
    # If no data frame available, treat all selected columns as new
    new_fields <- setdiff(selected_columns, excluded_from_change)
  }

  # Include fields that are new, modified, OR essential NDA fields
  # Essential NDA fields are always included even if unchanged
  essential_fields <- intersect(selected_columns, essential_nda_fields)
  ordered_columns <- c(new_fields, modified_fields, essential_fields)

  # Ensure we don't drop any selected columns: append remaining ones and de-duplicate
  remaining_columns <- setdiff(selected_columns, ordered_columns)
  if (length(remaining_columns) > 0) {
    ordered_columns <- c(ordered_columns, remaining_columns)
  }
  ordered_columns <- unique(ordered_columns)

  # Report what's being included in the data definition
  if (length(new_fields) > 0) {
    message(sprintf("Including %d new fields in data definition: %s",
                    length(new_fields), paste(head(new_fields, 5), collapse = ", ")))
  }
  if (length(modified_fields) > 0) {
    message(sprintf("Including %d modified fields in data definition: %s",
                    length(modified_fields), paste(head(modified_fields, 5), collapse = ", ")))
  }
  if (length(essential_fields) > 0) {
    message(sprintf("Including %d essential NDA fields in data definition: %s",
                    length(essential_fields), paste(head(essential_fields, 5), collapse = ", ")))
  }
  if (length(unchanged_fields) > 0) {
    message(sprintf("Excluding %d unchanged NDA fields from data definition: %s",
                    length(unchanged_fields), paste(head(unchanged_fields, 5), collapse = ", ")))
  }

  # Initialize data definition structure
  data_definition <- list(
    measure_name = measure_name,
    created_at = Sys.time(),
    total_selected_fields = length(selected_columns),
    fields = list(),
    metadata = list(
      source_template = if ("metadata" %in% names(submission_template)) submission_template$metadata else NULL,
      nda_structure_info = list(
        total_elements = length(nda_lookup),
        structure_name = if ("shortName" %in% names(nda_structure)) nda_structure$shortName else measure_name
      ),
      validation_summary = list(
        matched_fields = 0,
        unmatched_fields = 0,
        computed_fields = 0,
        modified_fields = 0,
        warnings = character(0)
      )
    )
  )

  # Persist NDA element names for downstream export/formatting without relying on outer scope
  data_definition$metadata$nda_element_names <- names(nda_lookup)

  # Process each selected column in the new order
  for (i in seq_along(ordered_columns)) {
    column_name <- ordered_columns[i]
    # Skip internal/excluded fields unconditionally
    if (column_name %in% excluded_from_change) next

    # Check if column exists in NDA structure
    if (column_name %in% names(nda_lookup)) {
      # Field found in NDA structure
      nda_field <- nda_lookup[[column_name]]

      # Get missing value information for existing fields
      missing_info <- NULL
      field_data <- NULL
      field_exists_in_data <- FALSE

      if (!is.null(data_frame) && column_name %in% names(data_frame)) {
        field_data <- data_frame[[column_name]]
        field_exists_in_data <- TRUE
        total_count <- length(field_data)
        missing_count <- sum(is.na(field_data))
        missing_percentage <- round((missing_count / total_count) * 100, 1)

        missing_info <- list(
          missing_count = missing_count,
          missing_percentage = missing_percentage,
          total_count = total_count
        )
      }

      # Check if this existing field has missing value codes that aren't in the original NDA value range
      is_modified_structure <- FALSE
      updated_value_range <- NULL
      modification_notes <- NULL

      # IMPORTANT: ndar_subject01 variables can NEVER be modified
      if (column_name %in% ndar_subject01_fields) {
        # Force ndar_subject01 fields to remain unchanged
        is_modified_structure <- FALSE
      } else if (field_exists_in_data && !is.null(missing_data_codes)) {
        original_value_range <- if ("valueRange" %in% names(nda_field)) nda_field$valueRange else ""
        field_type <- if ("type" %in% names(nda_field)) nda_field$type else "String"

        # Only check numeric fields for missing value codes
        if (field_type %in% c("Integer", "Float")) {
          # Detect missing value codes in the data
          user_defined_codes <- character(0)
          for (category in names(missing_data_codes)) {
            if (category != "required" && category != "types" && category != "aliases" && category != "allow_custom") {
              category_values <- missing_data_codes[[category]]
              if (is.character(category_values) || is.numeric(category_values)) {
                for (code in category_values) {
                  if (as.numeric(code) %in% field_data) {
                    user_defined_codes <- c(user_defined_codes, as.character(code))
                  }
                }
              }
            }
          }

          # Check if any of these codes are not in the original value range
          if (length(user_defined_codes) > 0) {
            original_codes <- character(0)
            if (original_value_range != "") {
              # Parse original value range to extract individual codes
              range_parts <- trimws(strsplit(original_value_range, ";")[[1]])
              for (part in range_parts) {
                if (!grepl("::", part)) {  # Skip range notation
                  original_codes <- c(original_codes, part)
                }
              }
            }

            # Find new codes that aren't in the original
            new_codes <- setdiff(user_defined_codes, original_codes)
            if (length(new_codes) > 0) {
              is_modified_structure <- TRUE

              # Create updated value range
              if (original_value_range == "") {
                updated_value_range <- paste(new_codes, collapse = ";")
              } else {
                updated_value_range <- paste0(original_value_range, ";", paste(new_codes, collapse = ";"))
              }

              # Create modification notes
              code_descriptions <- character(0)
              for (code in new_codes) {
                for (category in names(missing_data_codes)) {
                  if (category != "required" && category != "types" && category != "aliases" && category != "allow_custom") {
                    category_values <- missing_data_codes[[category]]
                    if (as.numeric(code) %in% category_values) {
                      code_descriptions <- c(code_descriptions, paste0(code, " = ", category))
                      break
                    }
                  }
                }
              }
              modification_notes <- paste0("Modified structure: Added missing value codes: ", paste(code_descriptions, collapse = ", "))
            }
          }
        }
      }

      # Determine source and metadata based on whether structure was modified
      if (is_modified_structure) {
        source <- "nda_modified"
        # Use updated value range
        validation_rules <- list(
          min_value = if ("minimum" %in% names(nda_field)) nda_field$minimum else NULL,
          max_value = if ("maximum" %in% names(nda_field)) nda_field$maximum else NULL,
          allowed_values = updated_value_range,
          pattern = if ("pattern" %in% names(nda_field)) nda_field$pattern else NULL
        )

        # Update NDA metadata with modified value range
        modified_nda_metadata <- nda_field
        modified_nda_metadata$valueRange <- updated_value_range
              # Do not append internal modification notes to exportable metadata

        nda_metadata_to_use <- modified_nda_metadata
        data_definition$metadata$validation_summary$modified_fields <-
          (data_definition$metadata$validation_summary$modified_fields %||% 0) + 1
      } else {
        source <- "nda_validated"
        # Use original validation rules and metadata
        validation_rules <- list(
          min_value = if ("minimum" %in% names(nda_field)) nda_field$minimum else NULL,
          max_value = if ("maximum" %in% names(nda_field)) nda_field$maximum else NULL,
          allowed_values = if ("valueRange" %in% names(nda_field)) nda_field$valueRange else NULL,
          pattern = if ("pattern" %in% names(nda_field)) nda_field$pattern else NULL
        )
        nda_metadata_to_use <- nda_field
        data_definition$metadata$validation_summary$matched_fields <-
          data_definition$metadata$validation_summary$matched_fields + 1
      }

      # Determine final description with fallback hierarchy:
      # 1) NDA description (if present and non-empty)
      # 2) REDCap field_label (if available) - ONLY for non-ndar_subject01 fields
      # 3) Qualtrics question text (if available) - ONLY for non-ndar_subject01 fields
      # 4) Keep as empty (will be handled downstream)
      nda_desc <- if ("description" %in% names(nda_field)) nda_field$description else ""
      fallback_desc <- nda_desc
      if (is.null(fallback_desc) || identical(fallback_desc, "") || is.na(fallback_desc)) {
        # Only apply external fallbacks for fields NOT matched to NDA
        if (!(column_name %in% names(nda_lookup))) {
          if (!is.null(redcap_label_map) && column_name %in% names(redcap_label_map)) {
            fallback_desc <- as.character(redcap_label_map[[column_name]])
          } else if (!is.null(qualtrics_label_map) && column_name %in% names(qualtrics_label_map)) {
            fallback_desc <- as.character(qualtrics_label_map[[column_name]])
          }
          if (!is.null(fallback_desc) && fallback_desc != "") {
            fallback_desc <- rc_clean_label(fallback_desc)
          }
          # If we found a fallback, also enrich nda_metadata to carry it through export
          if (!identical(fallback_desc, "")) {
            nda_metadata_to_use$description <- fallback_desc
          }
        } else {
          fallback_desc <- ""
        }
      }

      # Enrich ValueRange/Notes from REDCap choices ONLY for non-NDA fields
      if (!(column_name %in% names(nda_lookup)) && !is.null(redcap_choices_map) && column_name %in% names(redcap_choices_map)) {
        redcap_choices <- redcap_choices_map[[column_name]]
        if (!is.null(redcap_choices) && !is.na(redcap_choices) && redcap_choices != "") {
          parsed <- rc_parse_choices_codes_and_notes(redcap_choices)
          if (parsed$codes != "") {
            current_value_range <- if ("valueRange" %in% names(nda_metadata_to_use)) nda_metadata_to_use$valueRange else ""
            if (is.null(current_value_range) || current_value_range == "" || is.na(current_value_range)) {
              nda_metadata_to_use$valueRange <- parsed$codes
              validation_rules$allowed_values <- parsed$codes
            }
          }
          # Append mapping into notes (preserve existing)
          if (parsed$notes != "") {
            existing_notes <- if ("notes" %in% names(nda_metadata_to_use)) (nda_metadata_to_use$notes %||% "") else ""
            nda_metadata_to_use$notes <- paste0(existing_notes, ifelse(existing_notes != "", " | ", ""), parsed$notes)
          }
        }
      }

      # If still no ValueRange, attempt to derive from validation/type where possible using REDCap metadata row
      if (!(column_name %in% names(nda_lookup)) && (is.null(nda_metadata_to_use$valueRange) || nda_metadata_to_use$valueRange == "" || is.na(nda_metadata_to_use$valueRange))) {
        if (exists("rc_meta") && is.data.frame(rc_meta) && "field_name" %in% names(rc_meta)) {
          rc_row <- rc_meta[rc_meta$field_name == column_name, , drop = FALSE]
          if (nrow(rc_row) == 1) {
            derived <- rc_derive_value_range_from_validation(rc_row)
            if (derived != "") {
              nda_metadata_to_use$valueRange <- derived
              validation_rules$allowed_values <- derived
            }
          }
        }
      }

      data_definition$fields[[column_name]] <- list(
        name = column_name,
        selection_order = i,
        selected_for_submission = TRUE,
        source = source,

        # Copy NDA metadata
        data_type = if ("type" %in% names(nda_field)) nda_field$type else "unknown",
        description = fallback_desc,
        required = if ("required" %in% names(nda_field)) as.logical(nda_field$required) else FALSE,

        # Validation rules
        validation_rules = validation_rules,

        # Additional NDA metadata
        nda_metadata = nda_metadata_to_use,

        # Missing value information
        missing_info = missing_info,

        # Modification information
        is_modified = is_modified_structure,
        modification_notes = modification_notes
      )

    } else {
      # Field not found in NDA structure - compute metadata from data

      # Get data for this column if data frame is available
      column_data <- NULL
      field_exists_in_data <- FALSE

      if (!is.null(data_frame) && column_name %in% names(data_frame)) {
        column_data <- data_frame[[column_name]]
        field_exists_in_data <- TRUE
      }

      # Compute metadata
      computed_metadata <- compute_field_metadata(column_name, column_data)

      # Apply external fallback descriptions to computed fields when available
      computed_desc <- computed_metadata$description
      rc_label <- NULL
      if (!is.null(redcap_label_map) && column_name %in% names(redcap_label_map)) {
        rc_label <- as.character(redcap_label_map[[column_name]])
        rc_label <- rc_clean_label(rc_label)
      }
      if (!is.null(rc_label) && nzchar(rc_label)) {
        computed_desc <- rc_label
      } else if (is.null(computed_desc) || identical(computed_desc, "") || is.na(computed_desc)) {
        if (!is.null(qualtrics_label_map) && column_name %in% names(qualtrics_label_map)) {
          computed_desc <- as.character(qualtrics_label_map[[column_name]])
        }
      }

      # REDCap checkbox child handling: derive from parent metadata first
      if (exists("rc_meta") && is.data.frame(rc_meta)) {
        cbx <- rc_enrich_checkbox_child(column_name, rc_meta)
        if (!is.null(cbx$description) && cbx$description != "") {
          computed_desc <- cbx$description
        }
        if (!is.null(cbx$valueRange)) {
          computed_metadata$valueRange <- cbx$valueRange
        }
        if (!is.null(cbx$notes) && cbx$notes != "") {
          existing_notes <- computed_metadata$notes %||% ""
          computed_metadata$notes <- paste0(existing_notes, ifelse(existing_notes != "", " | ", ""), cbx$notes)
        }
        if (!is.null(cbx$data_type) && cbx$data_type != "") {
          computed_metadata$data_type <- cbx$data_type
        }
        if (!is.null(cbx$size)) {
          computed_metadata$size <- cbx$size
        }
      }

      # Enhance computed ValueRange with RedCap choices if available
      computed_value_range <- computed_metadata$valueRange
      if ((is.null(computed_value_range) || computed_value_range == "" || is.na(computed_value_range)) &&
          !is.null(redcap_choices_map) && column_name %in% names(redcap_choices_map)) {
        redcap_choices <- redcap_choices_map[[column_name]]
        if (!is.null(redcap_choices) && !is.na(redcap_choices) && redcap_choices != "") {
          parsed <- rc_parse_choices_codes_and_notes(redcap_choices)
          if (parsed$codes != "") {
            computed_metadata$valueRange <- parsed$codes
          }
          # Add mapping to notes
          if (parsed$notes != "") {
            existing_notes <- computed_metadata$notes %||% ""
            computed_metadata$notes <- paste0(existing_notes, ifelse(existing_notes != "", " | ", ""), parsed$notes)
          }
        }
      }

      # If still no ValueRange, attempt derive from REDCap validation/type
      if ((is.null(computed_metadata$valueRange) || computed_metadata$valueRange == "" || is.na(computed_metadata$valueRange)) && exists("rc_meta") && is.data.frame(rc_meta) && "field_name" %in% names(rc_meta)) {
        rc_row <- rc_meta[rc_meta$field_name == column_name, , drop = FALSE]
        if (nrow(rc_row) == 1) {
          derived <- rc_derive_value_range_from_validation(rc_row)
          if (derived != "") {
            computed_metadata$valueRange <- derived
          }
        }
      }

      # Clean description for <br>
      if (!is.null(computed_desc) && computed_desc != "") {
        computed_desc <- rc_clean_label(computed_desc)
      }

      # Apply REDCap-to-NDA type mapping for computed fields if metadata available
      if (exists("rc_meta") && is.data.frame(rc_meta) && "field_name" %in% names(rc_meta)) {
        rc_row <- rc_meta[rc_meta$field_name == column_name, , drop = FALSE]
        if (nrow(rc_row) == 1) {
          computed_metadata$data_type <- rc_map_redcap_to_nda_type(rc_row, computed_metadata$data_type %||% "String")
        }
      }

      # Adjust warning message based on whether field exists in data
      if (field_exists_in_data) {
        # Determine enrichment source
        enrichment <- if (exists("rc_meta") && is.data.frame(rc_meta) && column_name %in% rc_meta$field_name) {
          "from REDCap"
        } else if (!is.null(qualtrics_label_map) && column_name %in% names(qualtrics_label_map)) {
          "from Qualtrics"
        } else {
          "from dataframe"
        }
        warning_msg <- paste("Column", column_name, "not found in NDA data structure - computing metadata", enrichment)
      } else {
        enrichment <- if (exists("rc_meta") && is.data.frame(rc_meta) && column_name %in% rc_meta$field_name) {
          "from REDCap"
        } else if (!is.null(qualtrics_label_map) && column_name %in% names(qualtrics_label_map)) {
          "from Qualtrics"
        } else {
          "from template"
        }
        warning_msg <- paste("Column", column_name, "not found in NDA data structure or data frame - using placeholder metadata", enrichment)
      }

      data_definition$metadata$validation_summary$warnings <-
        c(data_definition$metadata$validation_summary$warnings, warning_msg)

      data_definition$fields[[column_name]] <- list(
        name = column_name,
        selection_order = i,
        selected_for_submission = TRUE,
        source = if (field_exists_in_data) "computed_from_data" else "template_only",
        data_type = computed_metadata$data_type,
        description = computed_desc,
        required = computed_metadata$required == "Required",
        validation_rules = list(
          min_value = NULL,
          max_value = NULL,
          allowed_values = computed_metadata$valueRange,
          pattern = NULL
        ),
        # Create NDA-style metadata structure
        nda_metadata = list(
          name = column_name,
          type = computed_metadata$data_type,
          size = computed_metadata$size,
          required = computed_metadata$required,
          description = computed_desc,
          valueRange = computed_metadata$valueRange,
          notes = computed_metadata$notes,
          aliases = computed_metadata$aliases
        ),
        computed = TRUE,
        exists_in_data = field_exists_in_data,
        warning = warning_msg,
        missing_info = computed_metadata$missing_info
      )

      data_definition$metadata$validation_summary$unmatched_fields <-
        data_definition$metadata$validation_summary$unmatched_fields + 1
      data_definition$metadata$validation_summary$computed_fields <-
        data_definition$metadata$validation_summary$computed_fields + 1
    }
  }

  # Add summary statistics
  data_definition$summary <- list(
    total_fields = length(selected_columns),
    matched_fields = data_definition$metadata$validation_summary$matched_fields,
    unmatched_fields = data_definition$metadata$validation_summary$unmatched_fields,
    computed_fields = data_definition$metadata$validation_summary$computed_fields,
    modified_fields = data_definition$metadata$validation_summary$modified_fields,
    match_percentage = round(
      (data_definition$metadata$validation_summary$matched_fields / length(selected_columns)) * 100,
      2
    )
  )

  # Add missing value summary across all fields
  total_missing_count <- 0
  total_rows <- 0
  fields_with_missing <- 0

  for (field_name in names(data_definition$fields)) {
    field <- data_definition$fields[[field_name]]
    if (!is.null(field$missing_info)) {
      total_missing_count <- total_missing_count + field$missing_info$missing_count
      total_rows <- max(total_rows, field$missing_info$total_count)
      if (field$missing_info$missing_count > 0) {
        fields_with_missing <- fields_with_missing + 1
      }
    }
  }

  if (total_rows > 0) {
    data_definition$summary$missing_data_summary <- list(
      total_missing_values = total_missing_count,
      total_data_points = total_rows * length(selected_columns),
      overall_missing_percentage = round((total_missing_count / (total_rows * length(selected_columns))) * 100, 1),
      fields_with_missing = fields_with_missing,
      total_fields = length(selected_columns)
    )
  }

  # Print summary
  cat("\n=== Data Definition Summary ===\n")
  cat("Measure:", measure_name, "\n")
  cat("Selected fields:", length(selected_columns), "\n")
  cat("Matched with NDA:", data_definition$summary$matched_fields, "\n")
  cat("Modified NDA structures:", data_definition$summary$modified_fields, "\n")
  cat("Computed from data:", data_definition$summary$computed_fields, "\n")
  cat("Match percentage:", paste0(data_definition$summary$match_percentage, "%"), "\n")

  # Print missing value summary
  if (!is.null(data_definition$summary$missing_data_summary)) {
    missing_summary <- data_definition$summary$missing_data_summary
    cat("\n=== Missing Data Summary ===\n")
    cat("Total missing values:", missing_summary$total_missing_values, "\n")
    cat("Total data points:", missing_summary$total_data_points, "\n")
    cat("Overall missing percentage:", paste0(missing_summary$overall_missing_percentage, "%"), "\n")
    cat("Fields with missing data:", missing_summary$fields_with_missing, "/", missing_summary$total_fields, "\n")
  }

  if (length(data_definition$metadata$validation_summary$warnings) > 0) {
    cat("\nInfo:\n")
    for (warning in data_definition$metadata$validation_summary$warnings) {
      cat("  -", warning, "\n")
    }
  }

  # Auto-export to XLSX
  tryCatch({
    exportDataDefinition(data_definition, "xlsx")
  }, error = function(e) {
    warning("Could not export data definition: ", e$message)
  })

  return(data_definition)
}

#' @noRd
validateDataDefinition <- function(data_definition, strict_validation = FALSE) {

  validation_result <- list(
    is_valid = TRUE,
    errors = character(0),
    warnings = character(0),
    recommendations = character(0)
  )

  # Check required NDA fields
  required_nda_fields <- c("src_subject_id", "subjectkey", "interview_age", "interview_date", "sex")
  selected_field_names <- names(data_definition$fields)

  for (required_field in required_nda_fields) {
    if (!required_field %in% selected_field_names) {
      validation_result$errors <- c(
        validation_result$errors,
        paste("Missing required NDA field:", required_field)
      )
      validation_result$is_valid <- FALSE
    }
  }

  # Check for unmatched fields
  if (data_definition$summary$unmatched_fields > 0) {
    if (strict_validation) {
      validation_result$errors <- c(
        validation_result$errors,
        paste("Unmatched fields found:", data_definition$summary$unmatched_fields)
      )
      validation_result$is_valid <- FALSE
    } else {
      validation_result$warnings <- c(
        validation_result$warnings,
        paste("Unmatched fields found:", data_definition$summary$unmatched_fields)
      )
    }
  }

  # Check match percentage
  if (data_definition$summary$match_percentage < 80) {
    validation_result$recommendations <- c(
      validation_result$recommendations,
      paste0("Low match percentage (", data_definition$summary$match_percentage,
             "%). Consider reviewing field selections.")
    )
  }

  # Print validation results
  cat("\n=== Validation Results ===\n")
  cat("Status:", ifelse(validation_result$is_valid, "VALID", "INVALID"), "\n")

  if (length(validation_result$errors) > 0) {
    cat("\nErrors:\n")
    for (error in validation_result$errors) {
      cat("  \u2717", error, "\n")
    }
  }

  if (length(validation_result$warnings) > 0) {
    cat("\nWarnings:\n")
    for (warning in validation_result$warnings) {
      cat("  \u26a0", warning, "\n")
    }
  }

  if (length(validation_result$recommendations) > 0) {
    cat("\nRecommendations:\n")
    for (rec in validation_result$recommendations) {
      cat("  \u2139", rec, "\n")
    }
  }

  return(validation_result)
}

#' @noRd
exportDataDefinition <- function(data_definition, format = "csv") {
  # Create directory structure if it doesn't exist
  tmp_path <- file.path(".", "tmp")
  if (!dir.exists(tmp_path)) {
    dir.create(tmp_path, recursive = TRUE)
  }

  # Internal-only fields that must never be exported
  excluded_internal <- c("state", "lost_to_followup", "lost_to_follow-up", "study_status")

  # Create file path with appropriate extension
  file_path <- file.path(tmp_path, paste0(data_definition$measure_name, "_definitions.", format))

  switch(format,
         "json" = {
           if (requireNamespace("jsonlite", quietly = TRUE)) {
             jsonlite::write_json(data_definition, file_path, pretty = TRUE, auto_unbox = TRUE)
             cat("Data definition exported to:", file_path, "\n")
           } else {
             stop("jsonlite package required for JSON export")
           }
         },

         "csv" = {
           # Flatten the fields for CSV export with exact NDA column names and case
           field_names <- names(data_definition$fields)
           # Filter out excluded fields from final export
           field_names <- setdiff(field_names, excluded_internal)

           if (length(field_names) == 0) {
             warning("No fields to export")
             return(invisible(NULL))
           }

           # Build each column safely with error handling
           tryCatch({
             element_names <- field_names

             data_types <- sapply(field_names, function(fname) {
               tryCatch({
                 x <- data_definition$fields[[fname]]
                 if (!is.null(x$nda_metadata) && "type" %in% names(x$nda_metadata)) {
                   as.character(x$nda_metadata$type %||% "String")
                 } else {
                   as.character(x$data_type %||% "String")
                 }
               }, error = function(e) "String")
             })

             sizes <- sapply(field_names, function(fname) {
               tryCatch({
                 x <- data_definition$fields[[fname]]
                 if (!is.null(x$nda_metadata) && "size" %in% names(x$nda_metadata)) {
                   size_val <- x$nda_metadata$size
                   if (is.null(size_val) || is.na(size_val)) "" else as.numeric(size_val)
                 } else {
                   ""
                 }
               }, error = function(e) "")
             })

             required_vals <- sapply(field_names, function(fname) {
               tryCatch({
                 x <- data_definition$fields[[fname]]
                 if (!is.null(x$nda_metadata) && "required" %in% names(x$nda_metadata)) {
                   as.character(x$nda_metadata$required %||% "No")
                 } else if (!is.null(x$required)) {
                   ifelse(isTRUE(x$required), "Required", "No")
                 } else {
                   "No"
                 }
               }, error = function(e) "No")
             })

             descriptions <- sapply(field_names, function(fname) {
               tryCatch({
                 x <- data_definition$fields[[fname]]
                 if (!is.null(x$nda_metadata) && "description" %in% names(x$nda_metadata)) {
                  desc_val <- as.character(x$nda_metadata$description %||% "")
                  # Strip boilerplate
                  desc_val <- gsub("^Missing field: ", "", desc_val)
                  desc_val <- gsub("^Empty field: ", "", desc_val)
                  desc_val
                 } else {
                   as.character(x$description %||% "")
                 }
               }, error = function(e) "")
             })

             value_ranges <- sapply(field_names, function(fname) {
               tryCatch({
                 x <- data_definition$fields[[fname]]
                 if (!is.null(x$nda_metadata) && "valueRange" %in% names(x$nda_metadata)) {
                   val_range <- x$nda_metadata$valueRange %||% ""
                   # Clean up NA values and convert to empty string
                   if (is.na(val_range) || val_range == "NA") "" else as.character(val_range)
                 } else {
                   ""
                 }
               }, error = function(e) "")
             })
            # Compress numeric ranges
            value_ranges <- vapply(value_ranges, compress_value_range, character(1))

             notes <- sapply(field_names, function(fname) {
               tryCatch({
                 x <- data_definition$fields[[fname]]
                 if (!is.null(x$nda_metadata) && "notes" %in% names(x$nda_metadata)) {
                   notes_val <- x$nda_metadata$notes %||% ""
                   # Clean up NA values and convert to empty string
                   if (is.na(notes_val) || notes_val == "NA" || notes_val == "character(0)") "" else as.character(notes_val)
                 } else if (!is.null(x$notes)) {
                   as.character(x$notes %||% "")
                 } else {
                   ""
                 }
               }, error = function(e) "")
             })

             aliases <- sapply(field_names, function(fname) {
               tryCatch({
                 x <- data_definition$fields[[fname]]
                 if (!is.null(x$nda_metadata) && "aliases" %in% names(x$nda_metadata)) {
                   alias_val <- x$nda_metadata$aliases %||% ""
                   normalize_aliases_export(alias_val)
                 } else {
                   ""
                 }
               }, error = function(e) "")
             })

             # Extract missing value information
             missing_counts <- sapply(field_names, function(fname) {
               tryCatch({
                 x <- data_definition$fields[[fname]]
                 if (!is.null(x$missing_info) && "missing_count" %in% names(x$missing_info)) {
                   as.character(x$missing_info$missing_count %||% "0")
                 } else {
                   "0"
                 }
               }, error = function(e) "0")
             })

             missing_percentages <- sapply(field_names, function(fname) {
               tryCatch({
                 x <- data_definition$fields[[fname]]
                 if (!is.null(x$missing_info) && "missing_percentage" %in% names(x$missing_info)) {
                   paste0(as.character(x$missing_info$missing_percentage %||% "0"), "%")
                 } else {
                   "0%"
                 }
               }, error = function(e) "0%")
             })

             # Curator notes to summarize changes
             curator_notes <- sapply(field_names, function(fname) {
               x <- data_definition$fields[[fname]]
               parts <- character(0)
               # Source summary
               if (!is.null(x$source)) {
                 if (identical(x$source, "computed_from_data")) {
                   parts <- c(parts, "New field (not in NDA); metadata inferred from data")
                 } else if (identical(x$source, "nda_modified") || isTRUE(x$is_modified)) {
                   parts <- c(parts, "Modified NDA field (valueRange updated)")
                 } else if (identical(x$source, "template_only")) {
                   parts <- c(parts, "In template only; no data observed")
                 } else if (identical(x$source, "nda_validated")) {
                   parts <- c(parts, "Matched NDA field")
                 }
               }
               # Required status
               reqFlag <- NULL
               if (!is.null(x$nda_metadata) && "required" %in% names(x$nda_metadata)) {
                 reqFlag <- x$nda_metadata$required
               } else if (!is.null(x$required)) {
                 reqFlag <- ifelse(isTRUE(x$required), "Required", "No")
               }
               if (!is.null(reqFlag) && identical(as.character(reqFlag), "Required")) {
                 parts <- c(parts, "Required by NDA")
               }
               # Skip missing percentage in curator notes
               note <- paste(parts, collapse = " | ")
               if (identical(note, "")) {
                 note <- paste0("Requesting to add ", fname, " as is")
               }
               note
             })

             # Create data frame with explicit length checking
            fields_df <- data.frame(
               ElementName = element_names,
               DataType = data_types,
               Size = sizes,
               Required = required_vals,
               ElementDescription = descriptions,
               ValueRange = value_ranges,
               Notes = notes,
               Aliases = aliases,
              `Notes for Data Curator` = curator_notes,
              stringsAsFactors = FALSE,
              check.names = FALSE
             )

             # --- Safe export sanitization ---
             # Drop rows with empty ElementName
             fields_df$ElementName <- as.character(fields_df$ElementName)
             fields_df <- fields_df[!is.na(fields_df$ElementName) & fields_df$ElementName != "", , drop = FALSE]

             # NA -> ""
             for (nm in names(fields_df)) {
               fields_df[[nm]][is.na(fields_df[[nm]])] <- ""
             }

             # Normalize DataType and Required
             fields_df$DataType <- as.character(fields_df$DataType)
             fields_df$Required <- as.character(fields_df$Required)
             fields_df$DataType[!fields_df$DataType %in% c("String", "Integer", "Float", "Date", "GUID", "Boolean")] <- "String"
             fields_df$Required[!fields_df$Required %in% c("Required", "Recommended", "Conditional", "No")] <- "No"

             # Coerce Size to numeric-like text or empty
             suppressWarnings(sz <- as.numeric(fields_df$Size))
             sz[is.na(sz)] <- NA_real_
             fields_df$Size <- ifelse(is.na(sz), "", as.character(sz))

             # Only Strings have Size; clear Size for non-String types
             non_string_idx <- which(fields_df$DataType != "String")
             if (length(non_string_idx) > 0) {
               fields_df$Size[non_string_idx] <- ""
             }

             # Compress numeric ValueRange and normalize spacing
             fields_df$ValueRange <- vapply(fields_df$ValueRange, compress_value_range, character(1))

             # Normalize Aliases to comma-separated without quotes/c()
             fields_df$Aliases <- vapply(fields_df$Aliases, normalize_aliases_export, character(1))

             write.csv(fields_df, file_path, row.names = FALSE)
             cat("Data definition exported to:", file_path, "\n")

           }, error = function(e) {
             warning("Error creating CSV export: ", e$message)
             cat("Debug info - field count:", length(field_names), "\n")
             cat("Debug info - fields:", paste(head(field_names, 5), collapse = ", "), "\n")
           })
         },

         "xlsx" = {
           # openxlsx is a hard dependency (Imports); proceed directly

           # Reuse the same field assembly as CSV branch
           field_names <- names(data_definition$fields)
           field_names <- setdiff(field_names, excluded_internal)
           if (length(field_names) == 0) {
             warning("No fields to export")
             return(invisible(NULL))
           }

           element_names <- field_names

           data_types <- sapply(field_names, function(fname) {
             tryCatch({
               x <- data_definition$fields[[fname]]
               if (!is.null(x$nda_metadata) && "type" %in% names(x$nda_metadata)) {
                 as.character(x$nda_metadata$type %||% "String")
               } else {
                 as.character(x$data_type %||% "String")
               }
             }, error = function(e) "String")
           })

           sizes <- sapply(field_names, function(fname) {
             tryCatch({
               x <- data_definition$fields[[fname]]
               if (!is.null(x$nda_metadata) && "size" %in% names(x$nda_metadata)) {
                 size_val <- x$nda_metadata$size
                 if (is.null(size_val) || is.na(size_val)) "" else as.numeric(size_val)
               } else {
                 ""
               }
             }, error = function(e) "")
           })

           required_vals <- sapply(field_names, function(fname) {
             tryCatch({
               x <- data_definition$fields[[fname]]
               if (!is.null(x$nda_metadata) && "required" %in% names(x$nda_metadata)) {
                 as.character(x$nda_metadata$required %||% "No")
               } else if (!is.null(x$required)) {
                 ifelse(isTRUE(x$required), "Required", "No")
               } else {
                 "No"
               }
             }, error = function(e) "No")
           })

           descriptions <- sapply(field_names, function(fname) {
             tryCatch({
               x <- data_definition$fields[[fname]]
               if (!is.null(x$nda_metadata) && "description" %in% names(x$nda_metadata)) {
                desc_val <- as.character(x$nda_metadata$description %||% "")
                desc_val <- gsub("^Missing field: ", "", desc_val)
                desc_val <- gsub("^Empty field: ", "", desc_val)
                desc_val
               } else {
                 as.character(x$description %||% "")
               }
             }, error = function(e) "")
           })

           value_ranges <- sapply(field_names, function(fname) {
             tryCatch({
               x <- data_definition$fields[[fname]]
               if (!is.null(x$nda_metadata) && "valueRange" %in% names(x$nda_metadata)) {
                 val_range <- x$nda_metadata$valueRange %||% ""
                 if (is.na(val_range) || val_range == "NA") "" else as.character(val_range)
               } else {
                 ""
               }
             }, error = function(e) "")
           })
           value_ranges <- vapply(value_ranges, compress_value_range, character(1))

           notes <- sapply(field_names, function(fname) {
             tryCatch({
               x <- data_definition$fields[[fname]]
               if (!is.null(x$nda_metadata) && "notes" %in% names(x$nda_metadata)) {
                 notes_val <- x$nda_metadata$notes %||% ""
                 notes_val <- if (is.na(notes_val) || notes_val == "NA" || notes_val == "character(0)") "" else as.character(notes_val)
                 # Strip internal modification text if ever present
                 notes_val <- gsub("\\bModified structure: Added missing value codes:.*$", "", notes_val)
                 trimws(notes_val)
               } else if (!is.null(x$notes)) {
                 as.character(x$notes %||% "")
               } else {
                 ""
               }
             }, error = function(e) "")
           })

           aliases <- sapply(field_names, function(fname) {
             tryCatch({
               x <- data_definition$fields[[fname]]
               if (!is.null(x$nda_metadata) && "aliases" %in% names(x$nda_metadata)) {
                 alias_val <- x$nda_metadata$aliases %||% ""
                 normalize_aliases_export(alias_val)
               } else {
                 ""
               }
             }, error = function(e) "")
           })

           # Curator notes to summarize changes (mirror CSV branch)
           curator_notes <- sapply(field_names, function(fname) {
             x <- data_definition$fields[[fname]]
             parts <- character(0)
             if (!is.null(x$source)) {
               if (identical(x$source, "computed_from_data")) {
                 parts <- c(parts, "New field (not in NDA); metadata inferred from data")
               } else if (identical(x$source, "nda_modified") || isTRUE(x$is_modified)) {
                 parts <- c(parts, "Modified NDA field (valueRange updated)")
               } else if (identical(x$source, "template_only")) {
                 parts <- c(parts, "In template only; no data observed")
               } else if (identical(x$source, "nda_validated")) {
                 parts <- c(parts, "Matched NDA field")
               }
             }
             reqFlag <- NULL
             if (!is.null(x$nda_metadata) && "required" %in% names(x$nda_metadata)) {
               reqFlag <- x$nda_metadata$required
             } else if (!is.null(x$required)) {
               reqFlag <- ifelse(isTRUE(x$required), "Required", "No")
             }
             if (!is.null(reqFlag) && identical(as.character(reqFlag), "Required")) {
               parts <- c(parts, "Required by NDA")
             }
             # Skip missing percentage in curator notes
             note <- paste(parts, collapse = " | ")
             if (identical(note, "")) {
               note <- paste0("Requesting to add ", fname, " as is")
             }
             note
           })

          fields_df <- data.frame(
             ElementName = element_names,
             DataType = data_types,
             Size = sizes,
             Required = required_vals,
             ElementDescription = descriptions,
             ValueRange = value_ranges,
             Notes = notes,
             Aliases = aliases,
            `Notes for Data Curator` = curator_notes,
            stringsAsFactors = FALSE,
            check.names = FALSE
           )

           # Sanitize (same as CSV)
           fields_df$ElementName <- as.character(fields_df$ElementName)
           fields_df <- fields_df[!is.na(fields_df$ElementName) & fields_df$ElementName != "", , drop = FALSE]
           for (nm in names(fields_df)) {
             fields_df[[nm]][is.na(fields_df[[nm]])] <- ""
           }
           fields_df$DataType <- as.character(fields_df$DataType)
           fields_df$Required <- as.character(fields_df$Required)
           fields_df$DataType[!fields_df$DataType %in% c("String", "Integer", "Float", "Date", "GUID", "Boolean")] <- "String"
           fields_df$Required[!fields_df$Required %in% c("Required", "Recommended", "Conditional", "No")] <- "No"
           suppressWarnings(sz <- as.numeric(fields_df$Size))
           sz[is.na(sz)] <- NA_real_
           fields_df$Size <- ifelse(is.na(sz), "", as.character(sz))
           non_string_idx <- which(fields_df$DataType != "String")
           if (length(non_string_idx) > 0) {
             fields_df$Size[non_string_idx] <- ""
           }
           fields_df$ValueRange <- vapply(fields_df$ValueRange, compress_value_range, character(1))
           fields_df$Aliases <- vapply(fields_df$Aliases, normalize_aliases_export, character(1))

           # Build workbook with formatting
           wb <- openxlsx::createWorkbook()
           openxlsx::addWorksheet(wb, "Data Definitions")
           openxlsx::writeData(wb, "Data Definitions", fields_df, startRow = 1, startCol = 1, withFilter = TRUE)

           # Header style: yellow fill, bold
           headerStyle <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#FFF2CC", halign = "left", valign = "top", border = "Bottom")
           openxlsx::addStyle(wb, "Data Definitions", headerStyle, rows = 1, cols = seq_len(ncol(fields_df)), gridExpand = TRUE)

           # Wrap text for description/notes/valueRange
           wrapStyle <- openxlsx::createStyle(wrapText = TRUE, valign = "top")
           wrapCols <- which(colnames(fields_df) %in% c("ElementDescription", "ValueRange", "Notes", "Notes for Data Curator"))
           if (length(wrapCols) > 0 && nrow(fields_df) > 0) {
             openxlsx::addStyle(wb, "Data Definitions", wrapStyle, rows = 2:(nrow(fields_df) + 1), cols = wrapCols, gridExpand = TRUE)
           }

            # Red text for Required == "Required"
           if (nrow(fields_df) > 0) {
             reqRows <- which(fields_df$Required == "Required")
             if (length(reqRows) > 0) {
                redText <- openxlsx::createStyle(fontColour = "#FF0000")
               openxlsx::addStyle(wb, "Data Definitions", redText, rows = reqRows + 1, cols = which(colnames(fields_df) == "Required"), gridExpand = TRUE)
             }
           }

           # NDA Highlighting Rules
           if (nrow(fields_df) > 0) {
             elementCol <- which(colnames(fields_df) == "ElementName")
             valueCols <- which(colnames(fields_df) %in% c("ValueRange", "Notes"))

            blueFill <- openxlsx::createStyle(fgFill = "#00B0F0")
            yellowFill <- openxlsx::createStyle(fgFill = "#FFFF00")
            redFont <- openxlsx::createStyle(fontColour = "#FF0000")
            # Collect rich text edits to apply post-save via openxlsx2
            rich_text_edits <- list()

             # Determine whether each element exists in NDA by lookup retained in metadata,
             # falling back to NDA API element search when not found locally
             nda_element_names <- data_definition$metadata$nda_element_names %||% character(0)
             in_local_nda <- element_names %in% nda_element_names

             # Cached API checker to minimize network calls
             .nda_exists_cache <- new.env(parent = emptyenv())
             nda_element_exists_api <- function(el_name) {
               if (!nzchar(el_name)) return(FALSE)
               if (!is.null(.nda_exists_cache[[el_name]])) return(.nda_exists_cache[[el_name]])
               base_url <- "https://nda.nih.gov/api/datadictionary/v2/datastructure/dataElement/"
               url <- paste0(base_url, utils::URLencode(el_name, reserved = TRUE))
               exists_flag <- FALSE
               try({
                 resp <- httr::GET(url, httr::timeout(8))
                 if (httr::status_code(resp) == 200) {
                   raw <- rawToChar(resp$content)
                   if (nzchar(raw)) {
                     parsed <- tryCatch(jsonlite::fromJSON(raw), error = function(e) NULL)
                     # Endpoint returns an array of structures containing the element; non-empty => exists
                     if (!is.null(parsed) && length(parsed) > 0) exists_flag <- TRUE
                   }
                 }
               }, silent = TRUE)
               .nda_exists_cache[[el_name]] <- exists_flag
               exists_flag
             }

             api_check_needed <- which(!in_local_nda)
             api_exists <- rep(FALSE, length(element_names))
             if (length(api_check_needed) > 0) {
               for (idx in api_check_needed) {
                 api_exists[idx] <- nda_element_exists_api(element_names[idx])
               }
             }

             element_is_in_nda <- in_local_nda | api_exists

             # Fetch NDA element metadata for allowed range comparison (cached)
             .nda_meta_cache <- new.env(parent = emptyenv())
             nda_fetch_element <- function(el_name) {
               if (!nzchar(el_name)) return(NULL)
               if (!is.null(.nda_meta_cache[[el_name]])) return(.nda_meta_cache[[el_name]])
               base_url <- "https://nda.nih.gov/api/datadictionary/v2/dataelement/"
               url <- paste0(base_url, utils::URLencode(el_name, reserved = TRUE))
               out <- NULL
               try({
                 resp <- httr::GET(url, httr::timeout(8))
                 if (httr::status_code(resp) == 200) {
                   raw <- rawToChar(resp$content)
                   if (nzchar(raw)) {
                     out <- tryCatch(jsonlite::fromJSON(raw), error = function(e) NULL)
                   }
                 }
               }, silent = TRUE)
               .nda_meta_cache[[el_name]] <- out
               out
             }

             # Helper to expand numeric valueRange string into a set of allowed integer values
             expand_numeric_value_range <- function(val_range_str) {
               if (is.null(val_range_str) || identical(val_range_str, "") || is.na(val_range_str)) return(NULL)
               parts <- trimws(strsplit(as.character(val_range_str), ";")[[1]])
               parts <- parts[nzchar(parts)]
               if (length(parts) == 0) return(NULL)
               out_vals <- integer(0)
               for (p in parts) {
                 if (grepl("::", p, fixed = TRUE)) {
                   bounds <- suppressWarnings(as.numeric(strsplit(p, "::", fixed = TRUE)[[1]]))
                   if (length(bounds) == 2 && !any(is.na(bounds))) {
                     a <- floor(bounds[1]); b <- floor(bounds[2])
                     if (b >= a && (b - a) <= 10000) { # safety bound
                       out_vals <- c(out_vals, seq(a, b))
                     }
                   }
                 } else {
                   num <- suppressWarnings(as.numeric(p))
                   if (!is.na(num)) out_vals <- c(out_vals, floor(num))
                 }
               }
               unique(out_vals)
             }

             # Determine which rows are modified (our proposal changes)
             row_modified <- vapply(field_names, function(fname) {
               x <- data_definition$fields[[fname]]
               isTRUE(x$is_modified) || identical(as.character(x$source %||% ""), "nda_modified")
             }, logical(1))

             # Track diagnostics for a Routes sheet
             route_source <- vapply(field_names, function(fname) {
               x <- data_definition$fields[[fname]]
               as.character(x$source %||% "")
             }, character(1))
             applied_blue <- rep(FALSE, length(field_names))
             applied_yellow <- rep(FALSE, length(field_names))
             applied_red <- rep(FALSE, length(field_names))
             nda_range_mismatch <- rep(FALSE, length(field_names))

             # Blue: all rows whose ElementName exists in NDA
             idx_in_nda <- which(element_is_in_nda)
             if (length(idx_in_nda) > 0) {
               openxlsx::addStyle(wb, "Data Definitions", blueFill, rows = idx_in_nda + 1, cols = elementCol, gridExpand = TRUE)
               applied_blue[idx_in_nda] <- TRUE
             }

             # Yellow: modified NDA elements -> highlight changed definition columns (ValueRange/Notes here)
             idx_modified <- which(row_modified & element_is_in_nda)
             if (length(idx_modified) > 0 && length(valueCols) > 0) {
               openxlsx::addStyle(wb, "Data Definitions", yellowFill, rows = idx_modified + 1, cols = valueCols, gridExpand = TRUE)
               applied_yellow[idx_modified] <- TRUE
             }

             # Yellow + red text on ValueRange for NDA elements whose exported ValueRange extends beyond NDA-allowed values
             if (length(valueCols) > 0) {
               cat("Comparing NDA value ranges for mismatches...\n")
              for (i in which(element_is_in_nda)) {
                # Fast skip when our ValueRange is empty
                ours_str <- as.character(fields_df$ValueRange[i] %||% "")
                if (!nzchar(ours_str)) next
                # Fetch NDA metadata only when needed
                el <- element_names[i]
                nda_meta <- nda_fetch_element(el)
                if (is.null(nda_meta)) next
                nda_allowed <- expand_numeric_value_range(nda_meta$valueRange)
                if (is.null(nda_allowed) || length(nda_allowed) == 0) next
                ours_vals <- expand_numeric_value_range(ours_str)
                if (is.null(ours_vals)) next
                  if (length(setdiff(ours_vals, nda_allowed)) > 0) {
                  # Clear definition columns except ValueRange and Notes
                  clear_cols <- c("DataType", "Size", "Required", "ElementDescription")
                  clear_idx <- which(colnames(fields_df) %in% clear_cols)
                  if (length(clear_idx) > 0) {
                    for (cc in clear_idx) {
                      openxlsx::writeData(wb, "Data Definitions", "", startRow = i + 1, startCol = cc, colNames = FALSE)
                    }
                  }

                  # Apply yellow fill to ValueRange and Notes cells to indicate change
                  vr_col <- which(colnames(fields_df) == "ValueRange")
                  notes_col <- which(colnames(fields_df) == "Notes")
                  if (length(vr_col) == 1) {
                    openxlsx::addStyle(wb, "Data Definitions", yellowFill, rows = i + 1, cols = vr_col, gridExpand = TRUE)
                  }
                  if (length(notes_col) == 1) {
                    openxlsx::addStyle(wb, "Data Definitions", yellowFill, rows = i + 1, cols = notes_col, gridExpand = TRUE)
                  }

                  # Build rich-text instructions to apply later with openxlsx2
                  tokens <- trimws(strsplit(ours_str, ";")[[1]])
                  tokens <- tokens[nzchar(tokens)]
                  added_tokens <- character(0)
                  token_is_added <- logical(length(tokens))
                  for (ti in seq_along(tokens)) {
                    tok_vals <- expand_numeric_value_range(tokens[ti])
                    token_is_added[ti] <- length(setdiff(tok_vals %||% numeric(0), nda_allowed)) > 0
                    if (token_is_added[ti]) added_tokens <- c(added_tokens, tokens[ti])
                  }
                  rich_text_edits[[length(rich_text_edits) + 1]] <- list(
                    row = i + 1,
                    vr_col = if (length(vr_col) == 1) vr_col else NA_integer_,
                    notes_col = if (length(notes_col) == 1) notes_col else NA_integer_,
                    tokens = tokens,
                    token_is_added = token_is_added,
                    added_tokens = added_tokens
                  )
                  applied_yellow[i] <- TRUE
                  nda_range_mismatch[i] <- TRUE
                }
              }
              cat("Value range comparison complete.\n")
            }

            # Red text only (no yellow fill): elements not in NDA (new/proposed elements)
            idx_not_in_nda <- which(!element_is_in_nda)
            if (length(idx_not_in_nda) > 0) {
              openxlsx::addStyle(wb, "Data Definitions", redFont, rows = idx_not_in_nda + 1, cols = seq_len(ncol(fields_df)), gridExpand = TRUE)
              applied_red[idx_not_in_nda] <- TRUE
            }

             # Add diagnostics worksheet summarizing route and highlighting decisions
             routes_df <- data.frame(
               ElementName = element_names,
               InNDA = element_is_in_nda,
               Source = route_source,
               Modified = row_modified,
               BlueApplied = applied_blue,
               YellowApplied = applied_yellow,
               RedApplied = applied_red,
               NdaRangeMismatch = nda_range_mismatch,
               stringsAsFactors = FALSE,
               check.names = FALSE
             )
             openxlsx::addWorksheet(wb, "Routes")
             openxlsx::writeData(wb, "Routes", routes_df, startRow = 1, startCol = 1, withFilter = TRUE)
             openxlsx::setColWidths(wb, "Routes", cols = seq_len(ncol(routes_df)), widths = "auto")

             # Ensure Notes for Data Curator defaults for unchanged NDA elements
             idx_no_change <- which(element_is_in_nda & !row_modified & !nda_range_mismatch)
             if (length(idx_no_change) > 0) {
               notes_col <- which(colnames(fields_df) == "Notes for Data Curator")
               default_notes <- paste0("Requesting to add ", element_names[idx_no_change], " as is.")
               # Overwrite cells in worksheet to guarantee desired phrasing
               for (k in seq_along(idx_no_change)) {
                 openxlsx::writeData(wb, "Data Definitions", default_notes[k], startRow = idx_no_change[k] + 1, startCol = notes_col, colNames = FALSE)
               }

               # For unchanged NDA elements, clear all definition columns
               cols_to_clear <- c("DataType", "Size", "Required", "ElementDescription", "ValueRange", "Notes", "Aliases")
               clear_cols_idx <- which(colnames(fields_df) %in% cols_to_clear)
               if (length(clear_cols_idx) > 0) {
                 for (cc in clear_cols_idx) {
                   # write empty strings into each target row for this column
                   for (r in idx_no_change) {
                     openxlsx::writeData(wb, "Data Definitions", "", startRow = r + 1, startCol = cc, colNames = FALSE)
                   }
                 }
               }
             }
           }
          
          # Auto widths
          openxlsx::setColWidths(wb, "Data Definitions", cols = seq_len(ncol(fields_df)), widths = "auto")

          openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)

          # Post-process with openxlsx2 to apply per-token rich text if needed
          if (length(rich_text_edits) > 0) {
            # Helper to convert numeric column index to Excel column letters
            num_to_col <- function(n) {
              letters <- c()
              while (n > 0) {
                r <- (n - 1) %% 26
                letters <- c(LETTERS[r + 1], letters)
                n <- (n - 1) %/% 26
              }
              paste0(letters, collapse = "")
            }
            wb2 <- openxlsx2::wb_load(file_path)
            # Resolve exported functions dynamically to avoid R CMD check NOTE/WARNING
            create_rich_text_fn <- get("create_rich_text", asNamespace("openxlsx2"))
            wb_add_rich_text_fn <- get("wb_add_rich_text", asNamespace("openxlsx2"))
            for (edit in rich_text_edits) {
              # ValueRange rich text
              if (!is.na(edit$vr_col) && length(edit$tokens) > 0) {
                texts <- character(0)
                colors <- character(0)
                for (ti in seq_along(edit$tokens)) {
                  texts <- c(texts, edit$tokens[ti])
                  colors <- c(colors, if (edit$token_is_added[ti]) "FF0000" else "000000")
                  if (ti < length(edit$tokens)) {
                    texts <- c(texts, "; ")
                    colors <- c(colors, "000000")
                  }
                }
                dims <- paste0(num_to_col(edit$vr_col), edit$row)
                # Write rich text (added tokens red) into the yellow-filled cell
                rt <- create_rich_text_fn(text = texts, color = colors)
                wb_add_rich_text_fn(wb2, sheet = "Data Definitions", dims = dims, x = rt)
              }
              # Notes rich text
              if (!is.na(edit$notes_col) && length(edit$added_tokens) > 0) {
                texts2 <- c("Added: ")
                colors2 <- c("000000")
                for (j in seq_along(edit$added_tokens)) {
                  texts2 <- c(texts2, edit$added_tokens[j])
                  colors2 <- c(colors2, "FF0000")
                  if (j < length(edit$added_tokens)) {
                    texts2 <- c(texts2, "; ")
                    colors2 <- c(colors2, "000000")
                  }
                }
                dims2 <- paste0(num_to_col(edit$notes_col), edit$row)
                rt2 <- create_rich_text_fn(text = texts2, color = colors2)
                wb_add_rich_text_fn(wb2, sheet = "Data Definitions", dims = dims2, x = rt2)
              }
            }
            openxlsx2::wb_save(wb2, file = file_path, overwrite = TRUE)
          }
          cat("Data definition exported to:", file_path, "\n")
         },

         "yaml" = {
           if (requireNamespace("yaml", quietly = TRUE)) {
             write_yaml_func <- get("write_yaml", asNamespace("yaml"))
             write_yaml_func(data_definition, file_path)
             cat("Data definition exported to:", file_path, "\n")
           } else {
             warning("yaml package not available. Install with install.packages('yaml')")
             return(invisible(NULL))
           }
         },

         stop("Unsupported format. Use 'json', 'csv', or 'yaml'")
  )
}

# Helper function for null coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x

# Compress numeric ValueRange like "1; 2; 3; 4; 5; 9999" -> "1::5;9999"
compress_value_range <- function(val_range) {
  if (is.null(val_range) || identical(val_range, "") || is.na(val_range)) return("")
  parts <- trimws(strsplit(as.character(val_range), ";")[[1]])
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0) return("")
  suppressWarnings(nums <- as.numeric(parts))
  if (any(is.na(nums))) {
    # Not purely numeric; normalize spacing only
    return(paste(parts, collapse = "; "))
  }

  # Keep special outliers separate (e.g., 77, 88, 99, 777, 999, -7, -8, -9, 9999) if they are not contiguous with the main run
  # Strategy: sort ascending to find contiguous runs, then join. This also fixes descending inputs like 5;4;3;2;1;0.
  nums_sorted <- sort(unique(nums))

  runs <- list()
  run_start <- nums_sorted[1]
  run_prev <- nums_sorted[1]
  for (i in seq_along(nums_sorted)) {
    if (i == 1) next
    if (nums_sorted[i] == run_prev + 1) {
      run_prev <- nums_sorted[i]
    } else {
      runs[[length(runs) + 1]] <- c(run_start, run_prev)
      run_start <- nums_sorted[i]
      run_prev <- nums_sorted[i]
    }
  }
  # close last run
  runs[[length(runs) + 1]] <- c(run_start, run_prev)

  # Build tokens: ranges as a::b when length >= 3, otherwise list numbers individually
  tokens <- character(0)
  for (r in runs) {
    a <- r[1]; b <- r[2]
    if (b - a + 1 >= 3) {
      tokens <- c(tokens, paste0(a, "::", b))
    } else if (a == b) {
      tokens <- c(tokens, as.character(a))
    } else {
      tokens <- c(tokens, as.character(a), as.character(b))
    }
  }

  # Preserve any values from the original list that were not numeric (already handled above) or out-of-order duplicates (deduped)
  paste(tokens, collapse = ";")
}

# Normalize aliases to plain comma-separated (no quotes), stripping c() and character(0)
normalize_aliases_export <- function(alias_val) {
  if (is.null(alias_val)) return("")
  # If it's a list, flatten first
  if (is.list(alias_val)) {
    alias_val <- unlist(alias_val, use.names = FALSE)
    if (length(alias_val) == 0) return("")
  }
  # Vector case (after possible flatten)
  if (is.vector(alias_val) && !is.list(alias_val)) {
    alias_vec <- as.character(alias_val)
    alias_vec <- alias_vec[!is.na(alias_vec) & nzchar(alias_vec)]
    if (length(alias_vec) == 0) return("")
    # remove any embedded quotes in vector elements and suppress 'list()'
    alias_vec <- gsub('"', "", alias_vec, fixed = TRUE)
    alias_vec <- alias_vec[alias_vec != "list()"]
    if (length(alias_vec) == 0) return("")
    return(paste(alias_vec, collapse = ","))
  }
  # Character scalar that might contain c() or quotes
  if (is.character(alias_val) && length(alias_val) == 1) {
    s <- as.character(alias_val)
    if (is.na(s) || !nzchar(s)) return("")
    if (identical(trimws(s), "character(0)") || identical(trimws(s), "list()")) return("")
    # remove all embedded quotes first
    s <- gsub('"', "", s, fixed = TRUE)
    # handle character(0) anywhere
    if (grepl("character\\(0\\)", s)) return("")
    # strip c( ... ) wrapper, tolerate spaces using POSIX classes and proper escaping
    if (grepl("^c[[:space:]]*\\(.*\\)[[:space:]]*$", s, perl = TRUE)) {
      s <- sub("^c[[:space:]]*\\((.*)\\)[[:space:]]*$", "\\1", s, perl = TRUE)
    }
    # split by comma
    parts <- trimws(strsplit(s, ",")[[1]])
    parts <- parts[!is.na(parts) & nzchar(parts) & parts != "list()"]
    if (length(parts) == 0) return("")
    return(paste(parts, collapse = ","))
  }
  # Fallback: coerce to character, join, then strip like scalar path
  as_char <- as.character(alias_val)
  as_char <- gsub('"', "", as_char, fixed = TRUE)
  as_char <- as_char[!is.na(as_char) & nzchar(as_char) & !grepl("character\\(0\\)|^list\\(\\)$", as_char)]
  if (length(as_char) == 0) return("")
  s <- paste(as_char, collapse = ",")
  if (grepl("^c[[:space:]]*\\(.*\\)[[:space:]]*$", s, perl = TRUE)) {
    s <- sub("^c[[:space:]]*\\((.*)\\)[[:space:]]*$", "\\1", s, perl = TRUE)
  }
  parts <- trimws(strsplit(s, ",")[[1]])
  parts <- parts[!is.na(parts) & nzchar(parts) & parts != "list()"]
  if (length(parts) == 0) return("")
  paste(parts, collapse = ",")
}
