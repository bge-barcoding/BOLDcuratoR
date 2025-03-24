# R/utils/ErrorBoundary.R

#' @title ErrorBoundary R6 Class
#' @description A utility class for structured error handling in Shiny applications
#' @importFrom R6 R6Class
#' @export
ErrorBoundary <- R6::R6Class(
  "ErrorBoundary",
  
  public = list(
    #' @description Initialize a new ErrorBoundary
    #' @param logger Optional logger instance for error logging
    initialize = function(logger = NULL) {
      private$logger <- logger
      private$last_error <- NULL
    },
    
    #' @description Execute code within an error boundary
    #' @param expr Expression to evaluate (typically a code block)
    #' @param default_value Value to return if an error occurs (default: NULL)
    #' @param silent Logical indicating whether to suppress error messages (default: TRUE)
    #' @param finally_expr Expression to evaluate after try block, whether error occurs or not
    #' @return Result of the expression if successful, or default_value if an error occurs
    catch = function(expr, default_value = NULL, silent = TRUE, finally_expr = NULL) {
      private$last_error <- NULL
      
      result <- tryCatch({
        # Evaluate the expression
        force(expr)
      }, error = function(e) {
        # Store the error for later reference
        private$last_error <- e
        
        # Log the error if a logger is available
        if (!is.null(private$logger)) {
          private$logger$error(
            sprintf("Error in ErrorBoundary: %s", e$message),
            list(
              call = deparse(e$call),
              traceback = private$get_traceback()
            )
          )
        } else if (!silent) {
          # Print error message if not silent and no logger
          message("Error caught by ErrorBoundary: ", e$message)
        }
        
        # Return the default value
        default_value
      }, finally = {
        # Execute finally expression if provided
        if (!is.null(finally_expr)) {
          force(finally_expr)
        }
      })
      
      return(result)
    },
    
    #' @description Get the last error that occurred
    #' @return The last error object or NULL if no error has occurred
    get_last_error = function() {
      private$last_error
    },
    
    #' @description Check if an error has occurred
    #' @return Logical indicating whether an error occurred in the last catch
    has_error = function() {
      !is.null(private$last_error)
    },
    
    #' @description Get the message from the last error
    #' @return Error message string or NULL if no error
    get_error_message = function() {
      if (is.null(private$last_error)) return(NULL)
      private$last_error$message
    },
    
    #' @description Reset the error state
    #' @return Invisible self (for method chaining)
    reset = function() {
      private$last_error <- NULL
      invisible(self)
    }
  ),
  
  private = list(
    logger = NULL,
    last_error = NULL,
    
    # Helper to get a formatted traceback
    get_traceback = function() {
      tryCatch({
        trace <- sys.calls()
        # Filter out ErrorBoundary internal calls
        trace <- trace[!grepl("ErrorBoundary", deparse(trace))]
        # Convert to strings for easier handling
        vapply(trace, function(call) deparse(call, width.cutoff = 500L)[1], character(1))
      }, error = function(e) {
        "Failed to retrieve traceback"
      })
    }
  )
)