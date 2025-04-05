#' Standardize a Numeric Vector
#'
#' Standardises a numeric vector by subtracting the mean and dividing by the 
#' standard deviation. 
#' Missing values (`NA`) are ignored during computation.
#'
#' @param x A numeric vector to be standardized.
#'
#' @return A numeric vector where each element is standardized (mean = 0, standard deviation = 1).
#'
#' @examples
#' # Standardize a simple numeric vector
#' vec <- c(1, 2, 3, 4, 5)
#' stdz(vec)
#'
#' # Handle missing values
#' vec_with_na <- c(1, 2, NA, 4, 5)
#' stdz(vec_with_na)
#'
#' @export
#' \dontrun{
#' stdz <- function(x) {
#'  x <- x - mean(x, na.rm = TRUE)
#'  x / var(x, na.rm = TRUE)^0.5
#' }
#' 
