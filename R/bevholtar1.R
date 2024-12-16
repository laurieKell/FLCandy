bevholtSVAR1 <- function() {
  # Log-likelihood function with AR1 errors
  logl <- function(s, v, spr0, rho, rec, ssb) {
    pars <- FLPar(abPars("bevholt", s = s, v = v, spr0 = spr0))
    loglAR1(log(rec), 
            log(pars["a"] %*% ssb/(pars["b"] %+% ssb)), 
            rho = rho)}
  
  # Initial parameter estimation
  initial <- structure(function(rec, ssb) {
    s <- 0.75
    spr0 <- quantile(c(ssb/rec), prob = 0.9, na.rm = TRUE, names = FALSE)
    v <- mean(as.vector(ssb), na.rm = TRUE) * 2
    rho <- 0
    return(FLPar(s = s, v = v, spr0 = spr0, rho = rho))
  }, 
  lower = c(0.2, rep(1e-07, 2), -1),    # Add bounds for rho
  upper = c(0.999, Inf, Inf, 1))        # Add bounds for rho
  
  # Model formula
  model <- rec ~ FLPar(abPars("bevholt", s = s, v = v, spr0 = spr0))["a"] %*% 
    ssb%/%(FLPar(abPars("bevholt", s = s, v = v, spr0 = spr0))["b"] %+% ssb)
  
  return(list(logl = logl, model = model, initial = initial))}

