#' NdaDataStructure R6 Class
#'
#' @description
#' Represents a single field (data element) in an NDA data structure.
#' This is a typed struct (similar to Go structs) that enforces schema consistency
#' and provides validation for NDA field definitions.
#'
#' @details
#' This class replaces ad-hoc list construction for NDA field definitions.
#' It provides:
#' - Type safety and validation for field definitions
#' - Consistent structure across all code paths
#' - Factory methods for creating fields from different sources
#' - Helper methods for common operations
#' - Direct mapping to Excel export columns
#'
#' The field structure matches the NDA data dictionary schema:
#' ElementName, DataType, Size, Required, ElementDescription, ValueRange, Notes, Aliases
#'
#' Uses typed R6 classes from NdaClasses.R for all fields (ElementName, DataType, 
#' Size, RequirementLevel, Description, ValueRange, Notes, Aliases, etc.)
#'
#' @keywords internal
NdaDataStructure <- R6::R6Class("NdaDataStructure",
  public = list(
    #' @field element_name ElementName object - field name (ElementName in Excel)
    element_name = NULL,
    
    #' @field data_type DataType object - data type (String, Integer, Float, Date, GUID, Boolean)
    data_type = NULL,
    
    #' @field size Size object - size for String types
    size = NULL,
    
    #' @field required RequirementLevel object - requirement level (Required, Recommended, Conditional, No)
    required = NULL,
    
    #' @field element_description Description object - field description
    element_description = NULL,
    
    #' @field value_range ValueRange object - allowed values or range
    value_range = NULL,
    
    #' @field notes Notes object - field notes
    notes = NULL,
    
    #' @field aliases Aliases object - field aliases
    aliases = NULL,
    
    #' @field selection_order Integer - order in which field was selected
    selection_order = NULL,
    
    #' @field selected_for_submission Logical - whether field is selected for NDA submission
    selected_for_submission = TRUE,
    
    #' @field source_metadata SourceMetadata object - field source tracking
    source_metadata = NULL,
    
    #' @field missing_info MissingInfo object - missing data information
    missing_info = NULL,
    
    #' @field validation_rules ValidationRules object - validation rules
    validation_rules = NULL,
    
    #' @description
    #' Create a new NdaDataStructure instance
    #' @param element_name Field name (required) - accepts string or ElementName object
    #' @param data_type Data type (default: "String") - accepts string or DataType object
    #' @param size Size for String types - accepts numeric or Size object
    #' @param required Requirement level (default: "No") - accepts string or RequirementLevel object
    #' @param element_description Field description - accepts string or Description object
    #' @param value_range Allowed values or range - accepts string or ValueRange object
    #' @param notes Field notes - accepts string or Notes object
    #' @param aliases Field aliases - accepts string, list, or Aliases object
    #' @param selection_order Selection order
    #' @param source Field source (legacy - use source_metadata instead)
    #' @param source_metadata SourceMetadata object
    #' @param missing_info MissingInfo object or list
    #' @param validation_rules ValidationRules object or list
    #' @param ... Additional fields
    #' @return A new NdaDataStructure object
    initialize = function(element_name, 
                         data_type = "String",
                         size = NULL,
                         required = "No",
                         element_description = "",
                         value_range = "",
                         notes = "",
                         aliases = "",
                         selection_order = NULL,
                         source = NULL,
                         source_metadata = NULL,
                         missing_info = NULL,
                         validation_rules = NULL,
                         ...) {
      # Validate required fields
      if (missing(element_name) || is.null(element_name) || element_name == "") {
        stop("element_name is required")
      }
      
      # Convert to typed objects if needed
      self$element_name <- if (inherits(element_name, "ElementName")) {
        element_name
      } else {
        ElementName$new(element_name)
      }
      
      self$data_type <- if (inherits(data_type, "DataType")) {
        data_type
      } else {
        DataType$new(data_type)
      }
      
      self$size <- if (is.null(size)) {
        Size$new(NULL, self$data_type$value)
      } else if (inherits(size, "Size")) {
        size
      } else {
        Size$new(size, self$data_type$value)
      }
      
      self$required <- if (inherits(required, "RequirementLevel")) {
        required
      } else {
        RequirementLevel$new(required)
      }
      
      self$element_description <- if (inherits(element_description, "Description")) {
        element_description
      } else {
        Description$new(element_description %||% "")
      }
      
      self$value_range <- if (inherits(value_range, "ValueRange")) {
        value_range
      } else {
        ValueRange$new(value_range %||% "")
      }
      
      self$notes <- if (inherits(notes, "Notes")) {
        notes
      } else {
        Notes$new(notes %||% "")
      }
      
      self$aliases <- if (inherits(aliases, "Aliases")) {
        aliases
      } else {
        Aliases$new(aliases %||% "")
      }
      
      # Metadata fields
      self$selection_order <- selection_order
      
      # Source metadata
      self$source_metadata <- if (!is.null(source_metadata)) {
        source_metadata
      } else if (!is.null(source)) {
        # Legacy compatibility - convert string source to SourceMetadata
        SourceMetadata$new(primary_source = source)
      } else {
        SourceMetadata$new()
      }
      
      # Missing info
      self$missing_info <- if (inherits(missing_info, "MissingInfo")) {
        missing_info
      } else if (is.list(missing_info)) {
        MissingInfo$new(
          missing_count = missing_info$missing_count %||% 0L,
          total_count = missing_info$total_count %||% 0L
        )
      } else {
        NULL
      }
      
      # Validation rules
      self$validation_rules <- if (inherits(validation_rules, "ValidationRules")) {
        validation_rules
      } else if (is.list(validation_rules)) {
        ValidationRules$new(
          min_value = validation_rules$min_value,
          max_value = validation_rules$max_value,
          allowed_values = validation_rules$allowed_values,
          pattern = validation_rules$pattern
        )
      } else {
        NULL
      }
      
      # Handle additional args
      extra_args <- list(...)
      for (name in names(extra_args)) {
        if (name %in% names(self)) {
          self[[name]] <- extra_args[[name]]
        }
      }
    },
    
    #' @description
    #' Convert to Excel row (returns named list for data.frame row)
    #' @return Named list with Excel column names and values
    to_excel_row = function() {
      list(
        ElementName = self$element_name$value,
        DataType = self$data_type$value,
        Size = self$size$to_string(),
        Required = self$required$value,
        ElementDescription = self$element_description$value,
        ValueRange = self$value_range$to_nda_format(),
        Notes = self$notes$to_string(),
        Aliases = self$aliases$to_string()
      )
    },
    
    #' @description
    #' Convert to legacy list format for backward compatibility
    #' @return List with field definition
    to_list = function() {
      list(
        name = self$element_name$value,
        selection_order = self$selection_order,
        selected_for_submission = self$selected_for_submission,
        source = self$source_metadata$primary_source,
        data_type = self$data_type$value,
        description = self$element_description$value,
        required = self$required$value == "Required",
        validation_rules = if (!is.null(self$validation_rules)) {
          self$validation_rules$to_list()
        } else {
          NULL
        },
        nda_metadata = list(
          name = self$element_name$value,
          type = self$data_type$value,
          size = if (self$size$is_valid) self$size$value else NULL,
          required = self$required$value,
          description = self$element_description$value,
          valueRange = self$value_range$to_nda_format(),
          notes = self$notes$to_string(),
          aliases = self$aliases$to_string()
        ),
        missing_info = if (!is.null(self$missing_info)) {
          list(
            missing_count = self$missing_info$missing_count,
            missing_percentage = self$missing_info$missing_percentage,
            total_count = self$missing_info$total_count
          )
        } else {
          NULL
        },
        is_modified = self$source_metadata$is_modified,
        modification_notes = paste(self$source_metadata$modifications, collapse = "; ")
      )
    },
    
    #' @description
    #' Check if field is a super required field
    #' @return Logical
    is_super_required = function() {
      self$element_name$value %in% SUPER_REQUIRED_FIELDS
    },
    
    #' @description
    #' Check if field came from ndar_subject01
    #' @return Logical
    is_from_ndar_subject = function() {
      ndar_fields <- c(SUPER_REQUIRED_FIELDS, 
                      c("ethnic_group", "site", "study", "subsiteid"))
      self$element_name$value %in% ndar_fields
    },
    
    #' @description
    #' Check if field is a DCC required field
    #' @return Logical
    is_dcc_required = function() {
      self$element_name$value %in% DCC_REQUIRED_FIELDS
    },
    
    #' @description
    #' Check if field is a DCC recommended field
    #' @return Logical
    is_dcc_recommended = function() {
      self$element_name$value %in% DCC_RECOMMENDED_FIELDS
    },
    
    #' @description
    #' Create a modified copy of this field
    #' @param value_range New value range (string or ValueRange object)
    #' @param notes New notes (string or Notes object)
    #' @param modification_note Description of modification
    #' @param ... Other fields to modify
    #' @return New NdaDataStructure object
    modify = function(value_range = NULL, notes = NULL, modification_note = NULL, ...) {
      clone <- self$clone(deep = TRUE)
      
      # Mark as modified
      clone$source_metadata$add_modification(modification_note %||% "Field modified")
      
      # Update value range
      if (!is.null(value_range)) {
        clone$value_range <- if (inherits(value_range, "ValueRange")) {
          value_range
        } else {
          ValueRange$new(value_range)
        }
      }
      
      # Update notes
      if (!is.null(notes)) {
        clone$notes <- if (inherits(notes, "Notes")) {
          notes
        } else {
          Notes$new(notes)
        }
      }
      
      # Update other fields
      extra_args <- list(...)
      for (name in names(extra_args)) {
        if (name %in% names(clone)) {
          value <- extra_args[[name]]
          
          # Try to convert to appropriate type based on field name
          if (name == "element_name" && !inherits(value, "ElementName")) {
            clone[[name]] <- ElementName$new(value)
          } else if (name == "data_type" && !inherits(value, "DataType")) {
            clone[[name]] <- DataType$new(value)
          } else if (name == "size" && !inherits(value, "Size")) {
            clone[[name]] <- Size$new(value, clone$data_type$value)
          } else if (name == "required" && !inherits(value, "RequirementLevel")) {
            clone[[name]] <- RequirementLevel$new(value)
          } else if (name == "element_description" && !inherits(value, "Description")) {
            clone[[name]] <- Description$new(value)
          } else if (name == "aliases" && !inherits(value, "Aliases")) {
            clone[[name]] <- Aliases$new(value)
          } else {
            clone[[name]] <- value
          }
        }
      }
      
      clone
    },
    
    #' @description
    #' Print method for NdaDataStructure
    #' @return Self (invisibly)
    print = function() {
      cat(sprintf("NdaDataStructure: %s\n", self$element_name$value))
      cat(sprintf("  Type: %s", self$data_type$value))
      if (self$size$is_valid) {
        cat(sprintf(" (size: %s)", self$size$value))
      }
      cat("\n")
      cat(sprintf("  Required: %s\n", self$required$value))
      
      range_str <- self$value_range$to_nda_format()
      if (!is.null(range_str) && range_str != "") {
        range_preview <- if (nchar(range_str) > 50) {
          paste0(substring(range_str, 1, 50), "...")
        } else {
          range_str
        }
        cat(sprintf("  ValueRange: %s\n", range_preview))
      }
      
      cat(sprintf("  Source: %s\n", self$source_metadata$to_string()))
      
      if (self$source_metadata$is_modified) {
        cat("  [MODIFIED]\n")
        if (length(self$source_metadata$modifications) > 0) {
          cat(sprintf("    %s\n", paste(self$source_metadata$modifications, collapse = "\n    ")))
        }
      }
      
      if (!is.null(self$missing_info)) {
        cat(sprintf("  Missing: %d/%d (%.1f%%)\n", 
                   self$missing_info$missing_count,
                   self$missing_info$total_count,
                   self$missing_info$missing_percentage))
      }
      
      invisible(self)
    },
    
    #' @description
    #' Merge value ranges from multiple sources
    #' @param nda_range ValueRange from NDA (may be NULL)
    #' @param redcap_range ValueRange from REDCap (may be NULL)
    #' @param data_range ValueRange from data (may be NULL)
    #' @param missing_codes Character vector of missing data codes
    #' @return Self (invisibly) with merged value_range
    merge_value_ranges = function(nda_range = NULL, 
                                  redcap_range = NULL, 
                                  data_range = NULL,
                                  missing_codes = character(0)) {
      
      merge_result <- merge_value_ranges(
        nda_range = nda_range %||% self$value_range,
        redcap_range = redcap_range,
        data_range = data_range,
        missing_codes = missing_codes
      )
      
      # Update value range
      self$value_range <- merge_result$merged_range
      
      # Record modifications
      if (length(merge_result$modifications) > 0) {
        for (mod in merge_result$modifications) {
          self$source_metadata$add_modification(mod)
        }
      }
      
      # Store warnings for later retrieval
      if (length(merge_result$warnings) > 0) {
        private$merge_warnings <- c(private$merge_warnings, merge_result$warnings)
      }
      
      invisible(self)
    },
    
    #' @description
    #' Get any warnings from merge operations
    #' @return Character vector of warnings
    get_merge_warnings = function() {
      private$merge_warnings
    },
    
    #' @description
    #' Clear merge warnings
    #' @return Self (invisibly)
    clear_merge_warnings = function() {
      private$merge_warnings <- character(0)
      invisible(self)
    }
  ),
  
  private = list(
    merge_warnings = character(0)
  )
)

#' Create NdaDataStructure from NDA API response
#'
#' @description
#' Factory function to create an NdaDataStructure from an NDA data element
#' (as returned from the NDA API or nda_lookup)
#'
#' @param nda_element List - NDA data element from API
#' @param selection_order Integer - selection order
#' @param missing_info MissingInfo object or list - missing data information
#' @return NdaDataStructure object
#' @keywords internal
#' @noRd
nda_structure_from_nda <- function(nda_element, selection_order = NULL, missing_info = NULL) {
  if (is.null(nda_element) || !is.list(nda_element)) {
    stop("nda_element must be a list")
  }
  
  # Extract and validate name
  name <- nda_element$name %||% stop("nda_element must have 'name' field")
  
  # Create typed objects for each field
  element_name <- ElementName$new(name)
  data_type <- DataType$new(nda_element$type %||% "String")
  size <- Size$new(nda_element$size, data_type$value)
  required <- RequirementLevel$new(nda_element$required %||% "No")
  
  # Handle condition if present
  if (!is.null(nda_element$condition) && nda_element$condition != "") {
    required <- RequirementLevel$new("Conditional", condition = nda_element$condition)
  }
  
  element_description <- Description$new(nda_element$description %||% "")
  value_range <- ValueRange$new(nda_element$valueRange %||% "")
  notes <- Notes$new(nda_element$notes %||% "")
  
  # CRITICAL FIX: Handle aliases from NDA API
  # NDA API returns: list(c("alias1", "alias2", "alias3"))
  # We need to flatten and convert to comma-separated string
  aliases_value <- nda_element$aliases
  if (is.list(aliases_value)) {
    aliases_value <- unlist(aliases_value, use.names = FALSE)
  }
  aliases <- Aliases$new(aliases_value %||% "")
  
  # Create source metadata
  source_metadata <- SourceMetadata$new(primary_source = "nda")
  source_metadata$mark_nda_match()
  
  # Convert missing_info if it's a list
  if (is.list(missing_info) && !inherits(missing_info, "MissingInfo")) {
    missing_info <- MissingInfo$new(
      missing_count = missing_info$missing_count %||% 0L,
      total_count = missing_info$total_count %||% 0L
    )
  }
  
  # Create structure
  structure <- NdaDataStructure$new(
    element_name = element_name,
    data_type = data_type,
    size = size,
    required = required,
    element_description = element_description,
    value_range = value_range,
    notes = notes,
    aliases = aliases,
    selection_order = selection_order,
    source_metadata = source_metadata,
    missing_info = missing_info
  )
  
  structure
}

#' Create NdaDataStructure from data frame column
#'
#' @description
#' Factory function to create an NdaDataStructure by computing metadata
#' from actual data in a column
#'
#' @param column_name Character - column name
#' @param column_data Vector - column data
#' @param selection_order Integer - selection order
#' @param description Character - optional description override
#' @return NdaDataStructure object
#' @keywords internal
#' @noRd
nda_structure_from_data <- function(column_name, column_data, selection_order = NULL, description = NULL) {
  # Use existing compute_field_metadata helper (from createNdaDataDefinition.R)
  computed <- compute_field_metadata(column_name, column_data)
  
  # Create typed objects
  element_name <- ElementName$new(column_name)
  data_type <- DataType$new(computed$data_type)
  size <- Size$new(computed$size, computed$data_type)
  required <- RequirementLevel$new(computed$required %||% "No")
  element_description <- Description$new(description %||% computed$description %||% "")
  value_range <- ValueRange$new(computed$valueRange %||% "")
  notes <- Notes$new("")
  aliases <- Aliases$new("")
  
  # Create source metadata
  source_metadata <- SourceMetadata$new(primary_source = "computed_from_data")
  
  # Create missing info if we have data
  missing_info <- NULL
  if (!is.null(column_data)) {
    total_count <- length(column_data)
    missing_count <- sum(is.na(column_data))
    
    missing_info <- MissingInfo$new(
      missing_count = missing_count,
      total_count = total_count
    )
  }
  
  # Create structure
  structure <- NdaDataStructure$new(
    element_name = element_name,
    data_type = data_type,
    size = size,
    required = required,
    element_description = element_description,
    value_range = value_range,
    notes = notes,
    aliases = aliases,
    selection_order = selection_order,
    source_metadata = source_metadata,
    missing_info = missing_info
  )
  
  structure
}

#' Create NdaDataStructure from REDCap field
#'
#' @description
#' Factory function to create an NdaDataStructure from REDCap data dictionary
#'
#' @param redcap_field List - REDCap field from data dictionary
#' @param column_data Vector - actual data from column (optional, for missing info)
#' @param selection_order Integer - selection order
#' @return NdaDataStructure object
#' @keywords internal
#' @noRd
nda_structure_from_redcap <- function(redcap_field, column_data = NULL, selection_order = NULL) {
  if (is.null(redcap_field) || !is.list(redcap_field)) {
    stop("redcap_field must be a list")
  }
  
  # Extract field name
  field_name <- redcap_field$field_name %||% redcap_field$name %||% stop("redcap_field must have 'field_name'")
  
  # Map REDCap field type to NDA data type
  redcap_type <- redcap_field$field_type %||% "text"
  nda_type <- switch(redcap_type,
    "text" = "String",
    "notes" = "String",
    "calc" = "Float",
    "dropdown" = "Integer",
    "radio" = "Integer",
    "checkbox" = "Integer",
    "yesno" = "Integer",
    "truefalse" = "Integer",
    "slider" = "Integer",
    "String"  # default
  )
  
  # Create typed objects
  element_name <- ElementName$new(field_name)
  data_type <- DataType$new(nda_type)
  
  # Infer size from validation or default
  size_val <- NULL
  if (nda_type == "String") {
    size_val <- redcap_field$text_validation_max %||% 255
  }
  size <- Size$new(size_val, nda_type)
  
  # Map required
  required_val <- if (isTRUE(redcap_field$required_field) || redcap_field$required_field == "y") {
    "Required"
  } else {
    "No"
  }
  required <- RequirementLevel$new(required_val)
  
  # Description
  desc <- redcap_field$field_label %||% redcap_field$field_note %||% ""
  element_description <- Description$new(desc)
  
  # Value range from choices
  choices <- redcap_field$select_choices_or_calculations %||% ""
  value_range <- ValueRange$new(choices)
  
  # Notes (can include choice labels)
  notes <- Notes$new(choices)
  
  # No aliases from REDCap
  aliases <- Aliases$new("")
  
  # Source metadata
  source_metadata <- SourceMetadata$new(primary_source = "redcap")
  source_metadata$mark_redcap_match()
  
  # Missing info if we have data
  missing_info <- NULL
  if (!is.null(column_data)) {
    total_count <- length(column_data)
    missing_count <- sum(is.na(column_data))
    missing_info <- MissingInfo$new(
      missing_count = missing_count,
      total_count = total_count
    )
  }
  
  # Create structure
  structure <- NdaDataStructure$new(
    element_name = element_name,
    data_type = data_type,
    size = size,
    required = required,
    element_description = element_description,
    value_range = value_range,
    notes = notes,
    aliases = aliases,
    selection_order = selection_order,
    source_metadata = source_metadata,
    missing_info = missing_info
  )
  
  structure
}
