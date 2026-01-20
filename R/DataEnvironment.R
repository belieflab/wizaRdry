#' DataEnvironment R6 Class
#'
#' @description
#' Manages dataframe storage in package environment (.pkg_env$.wizaRdry_env) with optional
#' convenience assignment to calling environment. CRAN-compliant environment management.
#'
#' @details
#' This class provides a clean interface for getting and setting dataframes using the package
#' environment (.pkg_env) as the authoritative source, with optional assignment to the calling
#' environment for user convenience. This eliminates global environment pollution and follows
#' R package best practices.
#'
#' @keywords internal
DataEnvironment <- R6::R6Class("DataEnvironment",
  public = list(
    #' @field measure_name Character string - name of the measure/dataframe
    measure_name = NULL,
    
    #' @description
    #' Create a new DataEnvironment instance
    #' @param measure_name Name of the measure/dataframe
    #' @param df Initial dataframe to store
    #' @return A new DataEnvironment object
    initialize = function(measure_name, df) {
      if (missing(measure_name) || is.null(measure_name) || measure_name == "") {
        stop("measure_name is required and cannot be empty")
      }
      if (missing(df) || is.null(df) || !is.data.frame(df)) {
        stop("df must be a valid data.frame")
      }
      
      self$measure_name <- measure_name
      self$set_df(df)
    },
    
    #' @description
    #' Get dataframe from package environment or calling environment
    #' @return The dataframe stored in package environment
    get_df = function() {
      # Priority order: .pkg_env$.wizaRdry_env (authoritative), then calling environment
      
      # Check package environment first (authoritative source)
      if (exists(".wizaRdry_env", envir = .pkg_env, inherits = FALSE)) {
        wizaRdry_env <- get(".wizaRdry_env", envir = .pkg_env)
        if (exists(self$measure_name, envir = wizaRdry_env, inherits = FALSE)) {
          return(base::get(self$measure_name, envir = wizaRdry_env))
        }
      }
      
      # Fallback: Try calling environment (for user convenience)
      calling_env <- parent.frame(2)
      if (exists(self$measure_name, envir = calling_env, inherits = FALSE)) {
        return(base::get(self$measure_name, envir = calling_env))
      }
      
      stop(sprintf("Dataframe '%s' not found in package environment or calling environment", self$measure_name))
    },
    
    #' @description
    #' Set dataframe in package environment with optional calling environment assignment
    #' @param df Data frame to set
    #' @return Self (invisibly) for method chaining
    set_df = function(df) {
      if (is.null(df) || !is.data.frame(df)) {
        stop("df must be a valid data.frame")
      }
      
      # Ensure .wizaRdry_env exists in package environment (CRAN compliant)
      if (!exists(".wizaRdry_env", envir = .pkg_env, inherits = FALSE)) {
        assign(".wizaRdry_env", new.env(parent = emptyenv()), envir = .pkg_env)
      }
      
      # Get wizaRdry environment from package environment
      wizaRdry_env <- base::get(".wizaRdry_env", envir = .pkg_env)
      
      # Set in package environment (authoritative source)
      base::assign(self$measure_name, df, envir = wizaRdry_env)
      
      # Also set in calling environment for user convenience (HYBRID approach)
      tryCatch({
        calling_env <- parent.frame(2)
        base::assign(self$measure_name, df, envir = calling_env)
      }, error = function(e) {
        # Calling env not accessible - this is OK, we have package env
      })
      
      invisible(self)
    },
    
    #' @description
    #' Get column names from the dataframe
    #' @return Character vector of column names
    get_colnames = function() {
      names(self$get_df())
    },
    
    #' @description
    #' Get number of rows in the dataframe
    #' @return Integer number of rows
    nrow = function() {
      nrow(self$get_df())
    },
    
    #' @description
    #' Get number of columns in the dataframe
    #' @return Integer number of columns
    ncol = function() {
      ncol(self$get_df())
    },
    
    #' @description
    #' Print method for DataEnvironment
    #' @return Self (invisibly)
    print = function() {
      cat("DataEnvironment:\n")
      cat(sprintf("  Measure: %s\n", self$measure_name))
      cat(sprintf("  Dimensions: %d rows x %d columns\n", self$nrow(), self$ncol()))
      cat(sprintf("  Columns: %s\n", paste(head(self$get_colnames(), 5), collapse = ", ")))
      if (self$ncol() > 5) {
        cat(sprintf("           ... and %d more\n", self$ncol() - 5))
      }
      invisible(self)
    }
  )
)
