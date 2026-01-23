# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

#' Safe Readline with Error Handling
#' @param prompt Character - prompt to display
#' @param default Character - default value if error/empty
#' @return Character - user input or default
#' @noRd
safe_readline <- function(prompt, default = "") {
  result <- tryCatch({
    readline(prompt = prompt)
  }, error = function(e) {
    return(default)
  }, interrupt = function(e) {
    message("\nInput cancelled (Esc pressed)")
    return(default)
  })
  if (is.null(result) || trimws(result) == "") default else result
}

#' Detect Fields Not in NDA Structure
#' @param df Data frame
#' @param elements NDA structure dataElements
#' @param verbose Logical
#' @return Character vector of unexpected field names
#' @noRd
detect_unexpected_fields <- function(df, elements, verbose = TRUE) {
  df_cols <- names(df)
  valid_fields <- elements$name
  unexpected <- setdiff(df_cols, valid_fields)
  
  if (length(unexpected) > 0 && verbose) {
    message(sprintf("\n[FIELD MAPPING] Detected %d unexpected field(s) not in NDA structure",
                   length(unexpected)))
  }
  
  return(unexpected)
}

#' Check if Field is Part of REDCap Matrix
#' @description
#' REDCap matrices have a matrix_group_name in the data dictionary.
#' We should skip auto-renaming these as they follow a specific structure.
#' Falls back to heuristic if dictionary unavailable.
#' 
#' @param field_name Character - field to check
#' @param api Character - API type (only applies to "redcap")
#' @param measure_name Character - instrument name for REDCap dictionary lookup
#' @return Logical - TRUE if field is part of a matrix
#' @noRd
is_redcap_matrix_field <- function(field_name, api, measure_name) {
  # Only applies to REDCap
  if (tolower(api) != "redcap") {
    return(FALSE)
  }
  
  # Try to fetch REDCap dictionary
  redcap_dict <- tryCatch({
    redcap.dict(measure_name)
  }, error = function(e) {
    return(NULL)
  })
  
  if (is.null(redcap_dict)) {
    # Fallback heuristic: fieldname_N or fieldname_N_N pattern
    # More than 1 numeric part suggests matrix structure
    parts <- strsplit(field_name, "_")[[1]]
    num_parts <- sum(grepl("^\\d+$", parts))
    return(num_parts > 1)
  }
  
  # Check if field has matrix_group_name defined
  if (field_name %in% redcap_dict$field_name) {
    matrix_group <- redcap_dict$matrix_group_name[redcap_dict$field_name == field_name]
    return(!is.na(matrix_group) && nchar(matrix_group) > 0)
  }
  
  return(FALSE)
}

# ============================================================================
# SIMILARITY & COMPATIBILITY
# ============================================================================

#' Calculate Similarity Scores for a Field
#' @param field_name Character - field to match
#' @param valid_fields Character vector - NDA structure fields
#' @param verbose Logical
#' @return Named numeric vector of similarity scores (sorted descending)
#' @noRd
calculate_field_similarities <- function(field_name, valid_fields, verbose = TRUE) {
  # Calculate Levenshtein similarity for all valid fields
  similarities <- sapply(valid_fields, function(name) {
    calculate_levenshtein(field_name, name)
  })
  
  # Remove NA values
  similarities <- similarities[!is.na(similarities)]
  
  # Sort by similarity (descending)
  similarities <- sort(similarities, decreasing = TRUE)
  
  return(similarities)
}

#' Check Value Range Compatibility for Rename
#' @param source_field Character - source field name
#' @param target_field Character - target field name
#' @param elements NDA structure dataElements
#' @param interactive_mode Logical
#' @param verbose Logical
#' @return Logical - TRUE if rename should proceed, FALSE otherwise
#' @noRd
check_rename_compatibility <- function(source_field, target_field, elements,
                                      interactive_mode = TRUE, verbose = TRUE) {
  # Get value ranges
  source_range <- get_field_value_range(source_field, elements)
  target_range <- get_field_value_range(target_field, elements)
  
  # Check compatibility
  range_check <- check_value_range_compatibility(
    source_range, target_range, 
    source_field, target_field, 
    verbose
  )
  
  if (!range_check$compatible) {
    if (verbose) {
      message(sprintf("\n*** VALUE RANGE CONFLICT DETECTED:"))
      message(sprintf("   Source field '%s' range: %s", source_field, source_range))
      message(sprintf("   Target field '%s' range: %s", target_field, target_range))
      message(sprintf("   %s", range_check$message))
      message(sprintf("   Recommendation: %s", range_check$recommendation))
    }
    
    if (interactive_mode) {
      proceed_input <- safe_readline(
        prompt = "Proceed with rename despite conflict? (y/n): ",
        default = "n"
      )
      while (!tolower(proceed_input) %in% c("y", "n")) {
        proceed_input <- safe_readline(
          prompt = "Please enter 'y' or 'n': ",
          default = "n"
        )
      }
      return(tolower(proceed_input) == "y")
    } else {
      if (verbose) message("Skipping rename due to conflict (non-interactive mode)")
      return(FALSE)
    }
  }
  
  return(TRUE)
}

# ============================================================================
# USER PROMPTS
# ============================================================================

#' Prompt User for High Confidence Rename (>90% similarity)
#' @param field_name Character - source field
#' @param best_match Character - suggested target field
#' @param similarity Numeric - similarity score (0-1)
#' @param elements NDA structure dataElements
#' @param interactive_mode Logical
#' @param verbose Logical
#' @return List with: action ("rename", "keep", "drop"), target (field name if rename)
#' @noRd
prompt_high_confidence_rename <- function(field_name, best_match, similarity,
                                         elements, interactive_mode = TRUE,
                                         verbose = TRUE) {
  result <- list(action = "keep", target = NULL)
  
  if (!interactive_mode) {
    # Non-interactive: auto-rename high confidence matches if compatible
    if (check_rename_compatibility(field_name, best_match, elements, FALSE, verbose)) {
      result$action <- "rename"
      result$target <- best_match
    }
    return(result)
  }
  
  # Interactive: prompt user
  rename_input <- safe_readline(
    prompt = sprintf("Rename '%s' to '%s' (%.1f%% match)? (y/n/m for manual): ",
                    field_name, best_match, similarity * 100),
    default = "y"
  )
  
  # Validate input
  while (!tolower(rename_input) %in% c("y", "n", "m")) {
    rename_input <- safe_readline(
      prompt = "Please enter 'y', 'n', or 'm': ",
      default = "y"
    )
  }
  
  if (tolower(rename_input) == "m") {
    # Manual entry
    manual_input <- safe_readline(
      prompt = sprintf("Enter the NDA field name for '%s': ", field_name),
      default = ""
    )
    while (manual_input == "" || is.na(manual_input)) {
      manual_input <- safe_readline(
        prompt = sprintf("Please enter a valid field name for '%s': ", field_name),
        default = ""
      )
    }
    result$target <- manual_input
    result$action <- "rename"
    if (verbose) message(sprintf("Using manual field name: '%s'", manual_input))
    
  } else if (tolower(rename_input) == "y") {
    # Check compatibility
    if (check_rename_compatibility(field_name, best_match, elements, interactive_mode, verbose)) {
      result$action <- "rename"
      result$target <- best_match
    } else {
      # Compatibility check failed, ask to drop/keep
      result <- prompt_field_action(field_name, FALSE, interactive_mode, verbose)
    }
  } else {
    # User said no, ask what to do with field
    result <- prompt_field_action(field_name, FALSE, interactive_mode, verbose)
  }
  
  return(result)
}

#' Prompt User for Low Confidence Rename (<90% similarity) with Pagination
#' @param field_name Character - source field
#' @param similarities Named numeric vector - sorted similarity scores
#' @param elements NDA structure dataElements
#' @param interactive_mode Logical
#' @param auto_drop_unknown Logical
#' @param verbose Logical
#' @return List with: action ("rename", "keep", "drop"), target (field name if rename)
#' @noRd
prompt_low_confidence_rename <- function(field_name, similarities, elements,
                                        interactive_mode = TRUE,
                                        auto_drop_unknown = FALSE,
                                        verbose = TRUE) {
  result <- list(action = if(auto_drop_unknown) "drop" else "keep", target = NULL)
  
  if (!interactive_mode) {
    return(result)
  }
  
  # Show paginated options
  page_size <- 5
  total <- length(similarities)
  page <- 1
  
  repeat {
    start_idx <- (page - 1) * page_size + 1
    end_idx <- min(page * page_size, total)
    
    message(sprintf("\nField: %s", field_name))
    message(sprintf("Top matches %d-%d of %d:", start_idx, end_idx, total))
    
    for (i in seq(start_idx, end_idx)) {
      local_num <- i - start_idx + 1
      match_name <- names(similarities)[i]
      match_score <- similarities[i]
      message(sprintf("%d. %s (%.1f%% match)", local_num, match_name, match_score * 100))
    }
    
    prompt_msg <- "Select 1-5, 'n' next, 'p' prev, 'm' manual, Enter skip, Esc quit: "
    user_input <- safe_readline(prompt = prompt_msg, default = "")
    
    # Empty = skip
    if (is.null(user_input) || trimws(user_input) == "") {
      break
    }
    
    # Manual entry
    if (tolower(user_input) == "m") {
      manual_input <- safe_readline(
        prompt = sprintf("Enter the NDA field name for '%s': ", field_name),
        default = ""
      )
      while (manual_input == "" || is.na(manual_input)) {
        manual_input <- safe_readline(
          prompt = "Please enter a valid field name: ",
          default = ""
        )
      }
      result$target <- manual_input
      result$action <- "rename"
      if (verbose) message(sprintf("Using manual field name: '%s'", manual_input))
      break
    }
    
    # Pagination
    if (tolower(user_input) == "n") {
      if (page * page_size < total) {
        page <- page + 1
      } else {
        message("Already at last page.")
      }
      next
    }
    
    if (tolower(user_input) == "p") {
      if (page > 1) {
        page <- page - 1
      } else {
        message("Already at first page.")
      }
      next
    }
    
    # Number selection (1-5)
    if (grepl("^[1-5]$", user_input)) {
      match_idx <- as.integer(user_input)
      if (match_idx >= 1 && match_idx <= (end_idx - start_idx + 1)) {
        global_index <- start_idx + match_idx - 1
        selected_match <- names(similarities)[global_index]
        
        if (verbose) {
          message(sprintf("Selected: %s (%.1f%% match)", 
                     selected_match, similarities[global_index] * 100))
        }
        
        # Check compatibility
        if (check_rename_compatibility(field_name, selected_match, elements, interactive_mode, verbose)) {
          result$action <- "rename"
          result$target <- selected_match
        } else {
          # Compatibility failed, ask what to do
          result <- prompt_field_action(field_name, auto_drop_unknown, interactive_mode, verbose)
        }
        break
      } else {
        message(sprintf("Please enter a number between 1 and %d", (end_idx - start_idx + 1)))
        next
      }
    }
    
    message("Invalid input. Choose 1-5, 'n', 'p', 'm', or Enter.")
  }
  
  # If user skipped pagination, ask what to do with field
  if (result$action != "rename") {
    result <- prompt_field_action(field_name, auto_drop_unknown, interactive_mode, verbose)
  }
  
  return(result)
}

#' Prompt User for Field Action (Keep or Drop)
#' @param field_name Character - field name
#' @param auto_drop_unknown Logical - default to drop
#' @param interactive_mode Logical
#' @param verbose Logical
#' @return List with: action ("keep" or "drop"), target = NULL
#' @noRd
prompt_field_action <- function(field_name, auto_drop_unknown = FALSE,
                               interactive_mode = TRUE, verbose = TRUE) {
  result <- list(action = if(auto_drop_unknown) "drop" else "keep", target = NULL)
  
  if (!interactive_mode) {
    if (verbose && result$action == "drop") {
      message(sprintf("Dropping field '%s' (auto_drop_unknown=TRUE)", field_name))
    }
    return(result)
  }
  
  drop_input <- safe_readline(
    prompt = sprintf("Drop field '%s'? (y/n): ", field_name),
    default = if(auto_drop_unknown) "y" else "n"
  )
  
  while (!tolower(drop_input) %in% c("y", "n")) {
    drop_input <- safe_readline(
      prompt = "Please enter 'y' or 'n': ",
      default = if(auto_drop_unknown) "y" else "n"
    )
  }
  
  if (tolower(drop_input) == "y") {
    result$action <- "drop"
    if (verbose) message(sprintf("Will drop field '%s'", field_name))
  } else {
    result$action <- "keep"
    if (verbose) message(sprintf("Keeping field '%s'", field_name))
  }
  
  return(result)
}

# ============================================================================
# FIELD OPERATIONS
# ============================================================================

#' Apply Field Rename to Data Frame
#' @param df Data frame
#' @param source_field Character - old name
#' @param target_field Character - new name
#' @param verbose Logical
#' @return Data frame with renamed field
#' @noRd
apply_field_rename <- function(df, source_field, target_field, verbose = TRUE) {
  # Safely handle special characters in field names
  field_data <- df[[source_field]]
  df[[target_field]] <- field_data
  
  if (verbose) {
    message(sprintf("[RENAME] '%s' → '%s'", source_field, target_field))
  }
  
  return(df)
}

# ============================================================================
# MAIN ORCHESTRATOR
# ============================================================================

#' Process Unexpected Fields with Interactive Mapping
#' @description
#' Main orchestrator for handling fields in data that don't exist in NDA structure.
#' Prompts user to rename, keep, or drop each unexpected field.
#' 
#' @param df Data frame
#' @param elements NDA structure dataElements
#' @param structure_name Character - NDA structure name
#' @param measure_name Character - measure name
#' @param api Character - API type (redcap, qualtrics, etc.)
#' @param verbose Logical
#' @param auto_drop_unknown Logical
#' @param interactive_mode Logical
#' @return List with:
#'   - df: Updated data frame
#'   - renames: Named character vector (old_name = new_name)
#'   - renamed_fields: Character vector of original field names that were renamed
#'   - columns_to_drop: Character vector of fields to drop
#' @noRd
process_unexpected_fields <- function(df, elements, structure_name, measure_name, api,
                                     verbose = TRUE, auto_drop_unknown = FALSE,
                                     interactive_mode = TRUE) {
  result <- list(
    df = df,
    renames = character(),
    renamed_fields = character(),
    columns_to_drop = character()
  )
  
  # Detect unexpected fields
  unexpected_fields <- detect_unexpected_fields(df, elements, verbose)
  
  if (length(unexpected_fields) == 0) {
    if (verbose) message("[FIELD MAPPING] No unexpected fields detected")
    return(result)
  }
  
  if (verbose) {
    message(sprintf("\n=== Interactive Field Mapping ==="))
    message("Processing unexpected fields...")
  }
  
  # Process each unexpected field
  for (field in unexpected_fields) {
    if (verbose) message(sprintf("\n[%d/%d] Processing field: '%s'", 
                                which(unexpected_fields == field),
                                length(unexpected_fields), field))
    
    # Check for special characters
    has_special_chars <- grepl("[#\\$%&\\*\\+/:<=>\\?@\\[\\\\\\]\\^\\{\\|\\}~]", field)
    if (has_special_chars && verbose) {
      message(sprintf("  Note: Field contains special characters"))
    }
    
    # Check if REDCap matrix field (skip renaming)
    if (is_redcap_matrix_field(field, api, measure_name)) {
      if (verbose) {
        matrix_group <- get_matrix_group_name(field, api, measure_name)
        if (nchar(matrix_group) > 0) {
          message(sprintf("  Skipping - REDCap matrix field (group: %s)", matrix_group))
        } else {
          message(sprintf("  Skipping - REDCap matrix field detected"))
        }
      }
      next
    }
    
    # Calculate similarity scores
    similarities <- calculate_field_similarities(field, elements$name, verbose = FALSE)
    
    if (length(similarities) == 0) {
      # No matches found
      if (verbose) message("  No similarity matches found")
      action <- prompt_field_action(field, auto_drop_unknown, interactive_mode, verbose)
      
      if (action$action == "drop") {
        result$columns_to_drop <- c(result$columns_to_drop, field)
      }
      next
    }
    
    # Get best match
    best_match <- names(similarities)[1]
    best_score <- similarities[1]
    
    # High confidence match (>90%)
    if (best_score > 0.9) {
      if (verbose) {
        message(sprintf("  High confidence match: %s (%.1f%%)", best_match, best_score * 100))
      }
      
      action <- prompt_high_confidence_rename(
        field, best_match, best_score, elements,
        interactive_mode, verbose
      )
      
    } else {
      # Low confidence match (<90%)
      if (verbose) {
        message(sprintf("  Best match below 90%% threshold (%.1f%%)", best_score * 100))
      }
      
      action <- prompt_low_confidence_rename(
        field, similarities, elements,
        interactive_mode, auto_drop_unknown, verbose
      )
    }
    
    # Execute action
    if (action$action == "rename") {
      result$df <- apply_field_rename(result$df, field, action$target, verbose)
      result$renamed_fields <- c(result$renamed_fields, field)
      # Store as named vector: old_name = new_name
      names(result$renames)[length(result$renames) + 1] <- field
      result$renames[length(result$renames)] <- action$target
    } else if (action$action == "drop") {
      result$columns_to_drop <- c(result$columns_to_drop, field)
    }
    # action == "keep" means do nothing (field stays as-is, becomes new field in structure)
  }
  
  # Summary
  if (verbose && (length(result$renamed_fields) > 0 || length(result$columns_to_drop) > 0)) {
    message(sprintf("\n[FIELD MAPPING] Summary:"))
    if (length(result$renamed_fields) > 0) {
      message(sprintf("  - Renamed: %d field(s)", length(result$renamed_fields)))
      for (i in seq_along(result$renames)) {
        old_name <- names(result$renames)[i]
        new_name <- result$renames[i]
        message(sprintf("    • %s → %s", old_name, new_name))
      }
    }
    if (length(result$columns_to_drop) > 0) {
      message(sprintf("  - Dropped: %d field(s)", length(result$columns_to_drop)))
      message(sprintf("    • %s", paste(result$columns_to_drop, collapse = ", ")))
    }
  }
  
  return(result)
}

# ============================================================================
# SCRIPT UPDATE (AUTO-INJECTION)
# ============================================================================

#' Update Cleaning Script with Field Operations
#' @description
#' Injects or updates auto-generated section in cleaning script with
#' rename and drop operations from interactive field mapping.
#' 
#' @param script_path Character - path to cleaning script (e.g., "./nda/redcap/cde_phq901.R")
#' @param measure_name Character - dataframe name
#' @param renames Named character vector - old_name = new_name
#' @param drops Character vector - fields to drop
#' @param verbose Logical
#' @return Logical - TRUE if successful, FALSE otherwise
#' @noRd
update_cleaning_script <- function(script_path, measure_name, renames, drops, verbose = TRUE) {
  # Check if script exists
  if (!file.exists(script_path)) {
    if (verbose) message(sprintf("[SCRIPT UPDATE] Script not found: %s", script_path))
    return(FALSE)
  }
  
  # Read script
  script_lines <- readLines(script_path, warn = FALSE)
  
  # Generate new auto-generated section
  new_section <- generate_auto_generated_section(measure_name, renames, drops)
  
  # Find existing auto-generated section
  auto_gen_start <- grep("^# ========== AUTO-GENERATED: Field Operations", script_lines)
  auto_gen_end <- grep("^# =====================================================================", script_lines)
  
  if (length(auto_gen_start) > 0 && length(auto_gen_end) > 0) {
    # Replace existing section
    before_section <- if (auto_gen_start[1] > 1) script_lines[1:(auto_gen_start[1] - 1)] else character()
    after_section <- if (auto_gen_end[length(auto_gen_end)] < length(script_lines)) {
      script_lines[(auto_gen_end[length(auto_gen_end)] + 1):length(script_lines)]
    } else {
      character()
    }
    
    new_script <- c(before_section, "", new_section, "", after_section)
  } else {
    # Find insertion point (before final line, which is usually blank or comment)
    # Insert before the last non-empty line
    insert_point <- length(script_lines)
    while (insert_point > 1 && trimws(script_lines[insert_point]) == "") {
      insert_point <- insert_point - 1
    }
    
    before_section <- script_lines[1:insert_point]
    after_section <- if (insert_point < length(script_lines)) {
      script_lines[(insert_point + 1):length(script_lines)]
    } else {
      character()
    }
    
    new_script <- c(before_section, "", new_section, "", after_section)
  }
  
  # Write back to file
  tryCatch({
    writeLines(new_script, script_path)
    if (verbose) {
      message(sprintf("[SCRIPT UPDATE] Updated cleaning script: %s", script_path))
      message("[SCRIPT UPDATE] Changes will persist on next run")
    }
    return(TRUE)
  }, error = function(e) {
    if (verbose) message(sprintf("[SCRIPT UPDATE] Failed to write script: %s", e$message))
    return(FALSE)
  })
}

#' Generate Auto-Generated Section Code
#' @param measure_name Character - dataframe name
#' @param renames Named character vector - old_name = new_name
#' @param drops Character vector - fields to drop
#' @return Character vector - lines of code
#' @noRd
generate_auto_generated_section <- function(measure_name, renames, drops) {
  lines <- character()
  
  # Header
  lines <- c(lines, "# ========== AUTO-GENERATED: Field Operations (DO NOT EDIT) ==========")
  
  # Renames section
  if (length(renames) > 0) {
    lines <- c(lines, "# Renames (from interactive field mapping)")
    for (i in seq_along(renames)) {
      old_name <- names(renames)[i]
      new_name <- renames[i]
      lines <- c(lines, sprintf("%s$%s <- %s$%s  # Renamed: %s → %s", 
                               measure_name, new_name, measure_name, old_name, old_name, new_name))
      lines <- c(lines, sprintf("%s$%s <- NULL  # Remove original field", measure_name, old_name))
    }
    lines <- c(lines, "")
  }
  
  # Drops section
  if (length(drops) > 0) {
    lines <- c(lines, "# Drops (from interactive field mapping + standard cleanup)")
    drop_vector <- paste0("c('", paste(drops, collapse = "', '"), "')")
    lines <- c(lines, sprintf("%s <- %s[, !names(%s) %%in%% %s, drop = FALSE]", 
                             measure_name, measure_name, measure_name, drop_vector))
  } else {
    lines <- c(lines, "# No fields marked for drop")
  }
  
  # Footer
  lines <- c(lines, "# =====================================================================")
  
  return(lines)
}
