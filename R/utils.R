#' Convert Logit-Transformed Steepness to Original Scale
#'
#' Converts steepness parameter from logit-transformed scale back to the original
#' scale used in fisheries models.
#'
#' @param logit_h Numeric vector of logit-transformed steepness values
#' @return Numeric vector of steepness values on original scale (0.2 to 1.0)
#' 
#' @examples
#' # Convert logit steepness to original scale
#' from_logits(c(0, 1, 2))
#' 
#' @export
from_logits <- function(logit_h) {
  # Validate input
  if (!is.numeric(logit_h)) {
    stop("logit_h must be numeric")
  }
  
  # Convert using inverse logit transformation with bounds
  out <- 0.2001 + 0.7998 * 1 / (1 + exp(-logit_h))
  
  return(out)
}

#' Convert Steepness to Logit Scale
#'
#' Converts steepness parameter to logit-transformed scale for numerical
#' stability in optimization routines.
#'
#' @param h Numeric vector of steepness values (0.2 to 1.0)
#' @return Numeric vector of logit-transformed steepness values
#' 
#' @examples
#' # Convert steepness to logit scale
#' to_logits(c(0.3, 0.5, 0.8))
#' 
#' @export
to_logits <- function(h) {
  # Validate input
  if (!is.numeric(h)) {
    stop("h must be numeric")
  }
  
  # Check bounds
  if (any(h <= 0.2001 | h >= 1.0)) {
    stop("h must be between 0.2001 and 1.0")
  }
  
  # Convert to logit scale
  result <- -log(0.7998 / (h - 0.2001) - 1)
  
  return(result)
}


