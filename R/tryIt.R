#' Safe Expression Evaluation with Error Handling
#' 
#' @description
#' Safely evaluates an expression and provides flexible error handling with customizable
#' default values and warning message control.
#'
#' @param x An R expression to be evaluated
#' @param silent Logical; if FALSE, displays warning messages for errors and warnings (default: TRUE)
#' @param default The value to return if evaluation fails (default: NULL)
#'
#' @return
#' Returns the evaluated expression if successful, the default value on error,
#' or the original expression on warning
#'
#' @examples
#' \dontrun{
#' # Silent error handling
#' tryIt(stop("error"))
#' 
#' # With warning messages
#' tryIt(stop("error"), silent = FALSE)
#' 
#' # Custom default value
#' tryIt(stop("error"), default = NA)
#' 
#' # Warning handling
#' tryIt(warning("warning"), silent = FALSE)
#' }
#'
#' @export

tryIt <- function(x, silent = TRUE, default = NULL) {
  rtn=tryCatch(
    expr = x,
    error<-function(e) {
      if (!silent) warning(e$message)
      default},
    warning<-function(w) {
      if (!silent) warning(w$message)
      x})
  
  return(rtn)}


