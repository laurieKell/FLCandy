  FLParPoint <- function(x) {
  
  dmx <- dim(x)
  dmi <- seq(length(dmx))[-length(dmx)] #seq(1, dmx[1] - 1)
  
  res <- cbind(
    # mean
    apply(x, dmi, mean),
    apply(x, dmi, median),
    apply(x, dmi, var),
    apply(x, dmi, quantile, 0.05),
    apply(x, dmi, quantile, 0.95))
  
  dimnames(res)$iter <- c("mean","median","var","lowq","uppq")
  
  return(res)
}

#' \dontrun{
#' x <- FLPar(a=runif(500, 1, 3), b=rlnorm(500, 0.4, 0.9), c=rlnorm(500, 0.2, 0.9)) 
#' FLParPoint(x)
#' }
