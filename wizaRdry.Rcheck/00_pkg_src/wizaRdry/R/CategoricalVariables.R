#' CategoricalVariables R6 Class
#'
#' @description
#' Manages categorical variables that should be preserved during data processing.
#' Categorizes variables by statistical type (nominal vs ordinal).
#'
#' These are STUDY-SPECIFIC categorical fields that contain meaningful data,
#' as opposed to StandardOutput fields which are API-generated metadata.
#'
#' Defaults:
#' - Nominal (no inherent order): phenotype
#' - Ordinal (ordered): visit, week
#'
#' @keywords internal
#' @noRd
CategoricalVariables <- R6::R6Class(
  "CategoricalVariables",
  
  public = list(
    #' @field nominal Character vector - Nominal categorical variables (no inherent order)
    #' Examples: phenotype, site, arm
    nominal = character(0),
    
    #' @field ordinal Character vector - Ordinal categorical variables (ordered)
    #' Examples: visit, week, session, timepoint
    ordinal = character(0),
    
    #' @description
    #' Initialize with default categorical variables
    initialize = function() {
      self$nominal <- c("phenotype")
      self$ordinal <- c("visit", "week")
    },
    
    #' @description
    #' Get all categorical variables (all subtypes combined)
    #'
    #' @return Character vector of all categorical variable names
    get_all = function() {
      unique(c(self$nominal, self$ordinal))
    },
    
    #' @description
    #' Get categorical variables by subtype
    #'
    #' @param subtype Character - Either "nominal" or "ordinal"
    #'
    #' @return Character vector of variable names for that subtype
    get_by_type = function(subtype = c("nominal", "ordinal")) {
      subtype <- match.arg(subtype)
      self[[subtype]]
    },
    
    #' @description
    #' Check if a field is categorical
    #'
    #' @param field Character - Field name to check
    #'
    #' @return Logical - TRUE if field is categorical (in any subtype)
    is_categorical = function(field) {
      if (!is.character(field) || length(field) != 1) {
        stop("field must be a single character string")
      }
      field %in% self$get_all()
    },
    
    #' @description
    #' Get categorical variables that exist in a dataframe
    #'
    #' @param df Data frame to check
    #'
    #' @return Character vector of categorical variables found in df
    get_existing = function(df) {
      if (!is.data.frame(df)) {
        return(character(0))
      }
      all_categorical <- self$get_all()
      intersect(all_categorical, names(df))
    }
  )
)
