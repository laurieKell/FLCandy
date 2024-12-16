#' Safely Evaluate an Expression
#' 
#' @description 
#' Evaluates an expression and returns NULL if it fails, instead of throwing an error.
#' 
#' @param x An R expression to evaluate
#' @param silent Logical; if TRUE suppresses error messages (default FALSE)
#' 
#' @return 
#' Returns the result of evaluating the expression if successful, NULL if it fails
#' 
#' @details 
#' This function is useful for safely evaluating expressions that might fail,
#' particularly in loops or apply functions where you want to continue execution
#' even if some operations fail.
#' 
#' @export
#' 
#' @examples
#' # Successful evaluation
#' tryIt(1 + 1)
#' 
#' # Failed evaluation returns NULL
#' tryIt(log("a"))
#' 
#' # Suppress error messages
#' tryIt(log("a"), silent=TRUE)
tryIt<-function(x, silent=TRUE) {
  rtn=try(x, silent=silent)
  
  if ("try-error" %in% class(rtn)) 
    return(NULL)
  
  return(rtn)}