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
#' @keywords internal
NdaDataStructure <- R6::R6Class("NdaDataStructure",
  public = list(
    #' @field element_name Character - field name (ElementName in Excel)
    element_name = NULL,
    
    #' @field data_type Character - data type (String, Integer, Float, Date, GUID, Boolean)
    data_type = NULL,
    
    #' @field size Numeric - size for String types (NULL for other types)
    size = NULL,
    
    #' @field required Character - requirement level (Required, Recommended, Conditional, No)
    required = NULL,
    
    #' @field element_description Character - field description
    element_description = NULL,
    
    #' @field value_range Character - allowed values or range
    value_range = NULL,
    
    #' @field notes Character - field notes
    notes = NULL,
    
    #' @field aliases Character - field aliases
    aliases = NULL,
    
    #' @field selection_order Integer - order in which field was selected
    selection_order = NULL,
    
    #' @field selected_for_submission Logical - whether field is selected for NDA submission
    selected_for_submission = TRUE,
    
    #' @field source Character - field source (nda_validated, nda_modified, computed_from_data, etc.)
    source = NULL,
    
    #' @field is_modified Logical - whether field was modified from original NDA definition
    is_modified = FALSE,
    
    #' @field modification_notes Character - notes about modifications
    modification_notes = NULL,
    
    #' @field missing_info List - missing data information (missing_count, missing_percentage, total_count)
    missing_info = NULL,
    
    #' @field validation_rules List - validation rules (min_value, max_value, allowed_values, pattern)
    validation_rules = NULL,
    
    #' @description
    #' Create a new NdaDataStructure instance
    #' @param element_name Field name (required)
    #' @param data_type Data type (default: "String")
    #' @param size Size for String types
    #' @param required Requirement level (default: "No")
    #' @param element_description Field description
    #' @param value_range Allowed values or range
    #' @param notes Field notes
    #' @param aliases Field aliases
    #' @param selection_order Selection order
    #' @param source Field source
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
                         ...) {
      # Validate required fields
      if (missing(element_name) || is.null(element_name) || element_name == "") {
        stop("element_name is required")
      }
      
      # Validate data_type
      valid_types <- c("String", "Integer", "Float", "Date", "GUID", "Boolean")
      if (!data_type %in% valid_types) {
        warning(sprintf("data_type '%s' not in standard NDA types: %s. Using as-is.", 
                       data_type, paste(valid_types, collapse = ", ")))
      }
      
      # Validate required
      valid_required <- c("Required", "Recommended", "Conditional", "No")
      if (!required %in% valid_required) {
        warning(sprintf("required '%s' not in standard values: %s. Using as-is.", 
                       required, paste(valid_required, collapse = ", ")))
      }
      
      # Set core fields
      self$element_name <- as.character(element_name)
      self$data_type <- as.character(data_type)
      self$size <- size
      self$required <- as.character(required)
      self$element_description <- as.character(element_description %||% "")
      self$value_range <- as.character(value_range %||% "")
      self$notes <- as.character(notes %||% "")
      self$aliases <- as.character(aliases %||% "")
      self$selection_order <- selection_order
      self$source <- source
      
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
        ElementName = self$element_name,
        DataType = self$data_type,
        Size = if (self$data_type == "String" && !is.null(self$size)) as.character(self$size) else "",
        Required = self$required,
        ElementDescription = self$element_description,
        ValueRange = self$value_range,
        Notes = self$notes,
        Aliases = self$aliases
      )
    },
    
    #' @description
    #' Convert to legacy list format for backward compatibility
    #' @return List with field definition
    to_list = function() {
      list(
        name = self$element_name,
        selection_order = self$selection_order,
        selected_for_submission = self$selected_for_submission,
        source = self$source,
        data_type = self$data_type,
        description = self$element_description,
        required = self$required == "Required",
        validation_rules = self$validation_rules,
        nda_metadata = list(
          name = self$element_name,
          type = self$data_type,
          size = self$size,
          required = self$required,
          description = self$element_description,
          valueRange = self$value_range,
          notes = self$notes,
          aliases = self$aliases
        ),
        missing_info = self$missing_info,
        is_modified = self$is_modified,
        modification_notes = self$modification_notes
      )
    },
    
    #' @description
    #' Check if field is a super required field
    #' @return Logical
    is_super_required = function() {
      self$element_name %in% SUPER_REQUIRED_FIELDS
    },
    
    #' @description
    #' Check if field came from ndar_subject01
    #' @return Logical
    is_from_ndar_subject = function() {
      ndar_fields <- c(SUPER_REQUIRED_FIELDS, 
                      c("ethnic_group", "site", "study", "subsiteid"))
      self$element_name %in% ndar_fields
    },
    
    #' @description
    #' Create a modified copy of this field
    #' @param value_range New value range
    #' @param notes New notes
    #' @param ... Other fields to modify
    #' @return New NdaDataStructure object
    modify = function(value_range = NULL, notes = NULL, ...) {
      clone <- self$clone(deep = TRUE)
      clone$is_modified <- TRUE
      if (is.null(clone$source) || clone$source == "nda_validated") {
        clone$source <- "nda_modified"
      }
      
      if (!is.null(value_range)) clone$value_range <- as.character(value_range)
      if (!is.null(notes)) clone$notes <- as.character(notes)
      
      extra_args <- list(...)
      for (name in names(extra_args)) {
        if (name %in% names(clone)) {
          clone[[name]] <- extra_args[[name]]
        }
      }
      
      clone
    },
    
    #' @description
    #' Print method for NdaDataStructure
    #' @return Self (invisibly)
    print = function() {
      cat(sprintf("NdaDataStructure: %s\n", self$element_name))
      cat(sprintf("  Type: %s", self$data_type))
      if (!is.null(self$size) && self$data_type == "String") {
        cat(sprintf(" (size: %s)", self$size))
      }
      cat("\n")
      cat(sprintf("  Required: %s\n", self$required))
      if (!is.null(self$value_range) && self$value_range != "") {
        range_preview <- if (nchar(self$value_range) > 50) {
          paste0(substring(self$value_range, 1, 50), "...")
        } else {
          self$value_range
        }
        cat(sprintf("  ValueRange: %s\n", range_preview))
      }
      if (!is.null(self$source)) {
        cat(sprintf("  Source: %s\n", self$source))
      }
      if (self$is_modified) {
        cat("  [MODIFIED]\n")
      }
      invisible(self)
    }
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
#' @param missing_info List - missing data information
#' @return NdaDataStructure object
#' @keywords internal
#' @noRd
nda_structure_from_nda <- function(nda_element, selection_order = NULL, missing_info = NULL) {
  if (is.null(nda_element) || !is.list(nda_element)) {
    stop("nda_element must be a list")
  }
  
  structure <- NdaDataStructure$new(
    element_name = nda_element$name %||% stop("nda_element must have 'name' field"),
    data_type = nda_element$type %||% "String",
    size = nda_element$size,
    required = nda_element$required %||% "No",
    element_description = nda_element$description %||% "",
    value_range = nda_element$valueRange %||% "",
    notes = nda_element$notes %||% "",
    aliases = nda_element$aliases %||% "",
    selection_order = selection_order,
    source = "nda_validated"
  )
  
  structure$missing_info <- missing_info
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
  # Use existing compute_field_metadata helper
  computed <- compute_field_metadata(column_name, column_data)
  
  structure <- NdaDataStructure$new(
    element_name = column_name,
    data_type = computed$data_type,
    size = computed$size,
    required = computed$required,
    element_description = description %||% computed$description,
    value_range = computed$valueRange %||% "",
    selection_order = selection_order,
    source = "computed_from_data"
  )
  
  # Add missing info if we have data
  if (!is.null(column_data)) {
    total_count <- length(column_data)
    missing_count <- sum(is.na(column_data))
    missing_percentage <- round((missing_count / total_count) * 100, 1)
    
    structure$missing_info <- list(
      missing_count = missing_count,
      missing_percentage = missing_percentage,
      total_count = total_count
    )
  }
  
  structure
}
