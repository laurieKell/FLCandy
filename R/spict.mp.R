# spict.mp <- function(inp, r.pr=c(0.2, 0.5, 1), bk.pr=c(1, 0.3, 1),
#                      shape.pr=c(1.01, 0.05, 1), proc.pr=c(0.07, 0.3, 1), fdevs=c(3, 0.5, 1),
#                      cdevs=c(0.05, 0.1, 1), dteuler=0.5, optimiser="nlminb") {
#   
#   # USE as discrete model with yearly time step
#   inp$dteuler <- dteuler
#   
#   # PRIORS
#   inp$priors$logbkfrac <- c(log(bk.pr[1]) - bk.pr[2] ^ 2 / 2,
#                             bk.pr[2], bk.pr[3])
#   
#   # DEACTIVATE
#   inp$priors$logalpha <- c(0, 0, 0)
#   inp$priors$logbeta <- c(0, 0, 0)
#   
#   inp$priors$logsdb <- c(log(proc.pr[1]), proc.pr[2], proc.pr[3])
#   inp$priors$logsdf <- c(log(fdevs[1]), fdevs[[2]], fdevs[3]) 
#   
#   # CONTROL catch error SD
#   inp$priors$logsdc <- c(log(0.05), 0.1, 1)
#   
#   # REDUCE shape CV
#   inp$priors$logn <- c(log(shape.pr[1]), shape.pr[2], shape.pr[3])
#   
#   inp$logr <- c(log(r.pr[1]), r.pr[2], r.pr[3])
#   
#   # DO NOT REPORT uncertainty on state variables (B, F).
#   inp$reportall <- 0
#   
#   # CHOOSE optimiser
#   inp$optimiser <- optimiser
#   
#   return(inp)
# }