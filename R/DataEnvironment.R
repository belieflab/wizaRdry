#' DataEnvironment R6 Class
#'
#' @description
#' Manages dataframe storage across multiple R environments (globalenv, .wizaRdry_env, origin_env).
#' Encapsulates the complex environment management pattern used throughout the validation workflow.
#'
#' @details
#' This class provides a clean interface for getting and setting dataframes across the three
#' environments used by wizaRdry: globalenv(), .wizaRdry_env, and origin_env (parent frame).
#' This eliminates scattered base::assign() and base::get() calls throughout the codebase.
#'
#' @export
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
    #' Get dataframe from the appropriate environment
    #' @return The dataframe stored across environments
    get_df = function() {
      # Check environments in priority order: globalenv, .wizaRdry_env, origin_env
      if (exists(self$measure_name, envir = globalenv(), inherits = FALSE)) {
        return(base::get(self$measure_name, envir = globalenv()))
      } else if (exists(".wizaRdry_env", envir = globalenv()) && 
                 exists(self$measure_name, envir = .wizaRdry_env, inherits = FALSE)) {
        return(base::get(self$measure_name, envir = .wizaRdry_env))
      } else {
        # Try to find in calling environment
        calling_env <- parent.frame(2)
        if (exists(self$measure_name, envir = calling_env, inherits = FALSE)) {
          return(base::get(self$measure_name, envir = calling_env))
        }
      }
      
      stop(sprintf("Dataframe '%s' not found in any environment", self$measure_name))
    },
    
    #' @description
    #' Set dataframe in ALL environments (globalenv, .wizaRdry_env, origin_env)
    #' @param df Data frame to set
    #' @return Self (invisibly) for method chaining
    set_df = function(df) {
      if (is.null(df) || !is.data.frame(df)) {
        stop("df must be a valid data.frame")
      }
      
      # Ensure .wizaRdry_env exists in globalenv
      if (!exists(".wizaRdry_env", envir = globalenv())) {
        .wizaRdry_env <- new.env(parent = globalenv())
        assign(".wizaRdry_env", .wizaRdry_env, envir = globalenv())
      }
      
      # Set in globalenv
      base::assign(self$measure_name, df, envir = globalenv())
      
      # Set in .wizaRdry_env
      wizaRdry_env <- base::get(".wizaRdry_env", envir = globalenv())
      base::assign(self$measure_name, df, envir = wizaRdry_env)
      
      # Try to set in calling environment (origin_env)
      tryCatch({
        calling_env <- parent.frame(2)
        base::assign(self$measure_name, df, envir = calling_env)
      }, error = function(e) {
        # Calling env not accessible - this is OK, we have globalenv and wizaRdry_env
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
