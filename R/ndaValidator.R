#' NDA Data Structure Validator
#'
#' @description
#' Validates NDA data structure compliance using the modular validation system.
#' This function now uses ndaValidator_new() for improved reliability and maintainability.
#' 
#' The validator performs comprehensive checks including:
#' - Value range validation (with proper handling of unbounded types)
#' - Required field verification
#' - Data type compliance
#' - Missing field handling
#' - De-identification (date-shifting, age-capping)
#'
#' @param measure_name Name of measure/structure
#' @param api API type (redcap, qualtrics, mongo, csv, oracle, sql)
#' @param limited_dataset Logical - if TRUE, skip de-identification (date-shifting, age-capping)
#' @param nda_base_url NDA API base URL (default: "https://nda.nih.gov/api/datadictionary/v2")
#' @param verbose Logical - print detailed output (default: TRUE)
#' @param debug Logical - print debug information (default: FALSE)
#' @param auto_drop_unknown Logical - automatically drop unknown fields (default: FALSE)
#' @param interactive_mode Logical - allow user prompts (default: TRUE)
#' @param modified_structure Pre-enhanced NDA structure (from ndaRequest.R)
#' 
#' @return ValidationState object with validation results including:
#'   \itemize{
#'     \item measure_name - Name of the measure
#'     \item api - API type
#'     \item is_new_structure - Whether structure is new (not in NDA)
#'     \item is_modified_structure - Whether structure has modifications
#'     \item value_range_violations - List of fields with value range issues
#'     \item new_fields - Fields not in original NDA structure
#'     \item nda_structure - Full NDA structure definition
#'     \item needs_data_definition() - Method to check if data definition file needed
#'     \item get_modification_reason() - Method to get human-readable modification reason
#'   }
#'
#' @details
#' This validator uses a modular architecture with the following components:
#' - ValidationState R6 class for state management
#' - Separate modules for transformations, field mapping, and validation logic
#' - Proper handling of unbounded data types (String, GUID, Date, Integer, Float)
#' - Centralized value range violation tracking
#'
#' @examples
#' \dontrun{
#' # Basic validation
#' result <- ndaValidator("my_task01", api = "csv")
#'
#' # Check if data definition needed
#' if (result$needs_data_definition()) {
#'   reason <- result$get_modification_reason()
#'   print(reason)
#' }
#'
#' # Access violations
#' violations <- result$value_range_violations
#' print(length(violations))
#' }
#'
#' @seealso 
#' \code{\link{ndaValidator_new}} for the underlying implementation
#' \code{\link{ValidationState}} for the state management class
#' \code{\link{nda}} for the complete NDA workflow
#'
#' @export
ndaValidator <- function(measure_name,
                         api,
                         limited_dataset = FALSE,
                         nda_base_url = "https://nda.nih.gov/api/datadictionary/v2",
                         verbose = TRUE,
                         debug = FALSE,
                         auto_drop_unknown = FALSE,
                         interactive_mode = TRUE,
                         modified_structure = NULL) {
  
  # Call the new modular implementation
  ndaValidator_new(
    measure_name = measure_name,
    api = api,
    limited_dataset = limited_dataset,
    nda_base_url = nda_base_url,
    verbose = verbose,
    debug = debug,
    auto_drop_unknown = auto_drop_unknown,
    interactive_mode = interactive_mode,
    modified_structure = modified_structure
  )
}
