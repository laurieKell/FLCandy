#' from_logits()
#'
#' convert steepness from logit
#' @param logit_s logit(steepness)
#' @return steepness h 
#' @export
from_logits <- function(logit_h){
  out=  0.2001 + 0.7998*1/(1+exp(-logit_h))
  out}

#' to_logits()
#'
#' convert steepness to logit
#' @param s steepness s
#' @return logit transformed steepness h 
#' @export
to_logits <- function(h){
  -log(0.7998/(h-0.2001)-1) 
}


