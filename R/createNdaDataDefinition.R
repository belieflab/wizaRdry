#' Create Data Definition from NDA Structure and Submission Template
#'
#' This function creates a data definition by intersecting the columns selected
#' for the final submission template against the data structure pulled from NDA.
#' It provides metadata about each field including validation rules, data types,
#' and submission-specific information.
#'
#' @param submission_template List containing the submission template with selected columns
#' @param nda_structure List containing the complete NDA data structure from API
#' @param measure_name Character string of the measure/data structure name
#' @return List containing the intersected data definition with metadata
#' @export
#' @examples
#' \dontrun{
#'   # After creating submission template
#'   template <- createTemplate(selected_columns, nda_data)
#'   data_def <- createDataDefinition(template, nda_data, "prl01")
#' }
createNdaDataDefinition <- function(submission_template, nda_structure, measure_name) {

  # Validate inputs
  if (!is.list(submission_template)) {
    stop("submission_template must be a list")
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
        warnings = character(0)
      )
    )
  )

  # Sort selected columns: existing fields first, then missing ones
  existing_fields <- character(0)
  missing_fields <- character(0)

  if (!is.null(data_frame)) {
    existing_fields <- selected_columns[selected_columns %in% names(data_frame)]
    missing_fields <- selected_columns[!selected_columns %in% names(data_frame)]
  } else {
    # If no data frame available, keep original order
    existing_fields <- selected_columns
  }

  # Reorder: existing fields first, then missing ones
  ordered_columns <- c(existing_fields, missing_fields)

  # Process each selected column in the new order
  for (i in seq_along(ordered_columns)) {
    column_name <- ordered_columns[i]

    # Check if column exists in NDA structure
    if (column_name %in% names(nda_lookup)) {
      # Field found in NDA structure
      nda_field <- nda_lookup[[column_name]]

      data_definition$fields[[column_name]] <- list(
        name = column_name,
        selection_order = i,
        selected_for_submission = TRUE,
        source = "nda_validated",

        # Copy NDA metadata
        data_type = if ("type" %in% names(nda_field)) nda_field$type else "unknown",
        description = if ("description" %in% names(nda_field)) nda_field$description else "",
        required = if ("required" %in% names(nda_field)) as.logical(nda_field$required) else FALSE,

        # Validation rules
        validation_rules = list(
          min_value = if ("minimum" %in% names(nda_field)) nda_field$minimum else NULL,
          max_value = if ("maximum" %in% names(nda_field)) nda_field$maximum else NULL,
          allowed_values = if ("valueRange" %in% names(nda_field)) nda_field$valueRange else NULL,
          pattern = if ("pattern" %in% names(nda_field)) nda_field$pattern else NULL
        ),

        # Additional NDA metadata
        nda_metadata = nda_field
      )

      data_definition$metadata$validation_summary$matched_fields <-
        data_definition$metadata$validation_summary$matched_fields + 1

    } else {
      # Field not found in NDA structure
      warning_msg <- paste("Column", column_name, "not found in NDA data structure")
      data_definition$metadata$validation_summary$warnings <-
        c(data_definition$metadata$validation_summary$warnings, warning_msg)

      data_definition$fields[[column_name]] <- list(
        name = column_name,
        selection_order = i,
        selected_for_submission = TRUE,
        source = "template_only",
        data_type = "unknown",
        description = "Field not found in NDA data structure",
        required = FALSE,
        validation_rules = list(),
        warning = warning_msg
      )

      data_definition$metadata$validation_summary$unmatched_fields <-
        data_definition$metadata$validation_summary$unmatched_fields + 1
    }
  }

  # Add summary statistics
  data_definition$summary <- list(
    total_fields = length(selected_columns),
    matched_fields = data_definition$metadata$validation_summary$matched_fields,
    unmatched_fields = data_definition$metadata$validation_summary$unmatched_fields,
    match_percentage = round(
      (data_definition$metadata$validation_summary$matched_fields / length(selected_columns)) * 100,
      2
    )
  )

  # Print summary
  cat("\n=== Data Definition Summary ===\n")
  cat("Measure:", measure_name, "\n")
  cat("Selected fields:", length(selected_columns), "\n")
  cat("Matched with NDA:", data_definition$summary$matched_fields, "\n")
  cat("Unmatched fields:", data_definition$summary$unmatched_fields, "\n")
  cat("Match percentage:", paste0(data_definition$summary$match_percentage, "%"), "\n")

  if (length(data_definition$metadata$validation_summary$warnings) > 0) {
    cat("\nWarnings:\n")
    for (warning in data_definition$metadata$validation_summary$warnings) {
      cat("  -", warning, "\n")
    }
  }

  # Auto-export to CSV
  tryCatch({
    exportDataDefinition(data_definition, "csv")
  }, error = function(e) {
    warning("Could not export data definition: ", e$message)
  })

  return(data_definition)
}

#' Validate Data Definition
#'
#' Validates the created data definition against submission requirements
#'
#' @param data_definition List containing the data definition
#' @param strict_validation Logical, whether to enforce strict validation rules
#' @return List with validation results
#' @export
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

#' Export Data Definition
#'
#' Exports the data definition to various formats
#'
#' @param data_definition List containing the data definition
#' @param format Character string specifying export format ("json", "csv", "yaml")
#' @export
exportDataDefinition <- function(data_definition, format = "csv") {
  # Create directory structure if it doesn't exist
  tmp_path <- file.path(".", "tmp")
  if (!dir.exists(tmp_path)) {
    dir.create(tmp_path, recursive = TRUE)
  }

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
                   if (is.null(size_val) || is.na(size_val)) NA else as.numeric(size_val)
                 } else {
                   NA
                 }
               }, error = function(e) NA)
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
                   as.character(x$nda_metadata$description %||% "")
                 } else {
                   as.character(x$description %||% "")
                 }
               }, error = function(e) "")
             })

             value_ranges <- sapply(field_names, function(fname) {
               tryCatch({
                 x <- data_definition$fields[[fname]]
                 if (!is.null(x$nda_metadata) && "valueRange" %in% names(x$nda_metadata)) {
                   as.character(x$nda_metadata$valueRange %||% "")
                 } else {
                   ""
                 }
               }, error = function(e) "")
             })

             notes <- sapply(field_names, function(fname) {
               tryCatch({
                 x <- data_definition$fields[[fname]]
                 if (!is.null(x$nda_metadata) && "notes" %in% names(x$nda_metadata)) {
                   as.character(x$nda_metadata$notes %||% "")
                 } else {
                   ""
                 }
               }, error = function(e) "")
             })

             aliases <- sapply(field_names, function(fname) {
               tryCatch({
                 x <- data_definition$fields[[fname]]
                 if (!is.null(x$nda_metadata) && "aliases" %in% names(x$nda_metadata)) {
                   as.character(x$nda_metadata$aliases %||% "")
                 } else {
                   ""
                 }
               }, error = function(e) "")
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
               stringsAsFactors = FALSE
             )

             write.csv(fields_df, file_path, row.names = FALSE)
             cat("Data definition exported to:", file_path, "\n")

           }, error = function(e) {
             warning("Error creating CSV export: ", e$message)
             cat("Debug info - field count:", length(field_names), "\n")
             cat("Debug info - fields:", paste(head(field_names, 5), collapse = ", "), "\n")
           })
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
