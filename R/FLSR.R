# FLSR.R - DESC
# /FLSR.R

# Copyright Henning Winker (JRC) & Iago MOSQUEIRA (WMR), 2021
# Authors:  Henning Winker (JRC) <henning.winker@ec.europa.eu>
#           Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

# srrTMB {{{

#' Fits Stock Recruitment Relationships (SRR) in TBM
#'
#' @param object Input FLSR object.
#' @param s steepness parameter of SRR (fixed or prior mean)    
#' @param spr0 unfished spawning biomass per recruit from FLCore::spr0(FLStock) or FLCore:::spr0Yr
#' @param s.est option to estimate steepness
#' @param s.logitsd prior sd for logit(s), default is 1.3 (flat) if s.est = TRUE 
#' @param inits option to specify initial values of log(r0), log(SigR) and logit(s)
#' @param lower option to specify lower bounds of log(r0), log(SigR) and logit(s) 
#' @param upper option to specify upper bounds of log(r0), log(SigR) and logit(s)
#' @param upper option to specify upper bounds of log(r0), log(SigR) and logit(s)
#' @param SDreport option to converge hessian and get vcov
#'
#' @return A list containing elements 'FLSR', of class *FLSR*

srrTMB <- function(object, spr0, s=NULL, s.est=TRUE,s.logitsd=1.3,inits=NULL, lower=NULL, upper=NULL,
  SDreport=TRUE) {
  if(is.null(s)& s.est){s=0.6} # central value
  if(is.null(s)& !s.est){s=0.8}
  
  # IDENTIFY model
  model <- SRModelName(model(object))

  # GET rec, ssb
  rec <- c(rec(object))
  ssb <- c(ssb(object))
  
  if ("FLQuant"%in%is(spr0))
    spr0=c(spr0)
  else
    spr0=rep(spr0,length(rec))
  
  # SET init and bounds
  if(is.null(inits))
    inits <- c(mean(log(rec)), log(0.4),to_logits(s))
  if(is.null(lower))
    lower <- c(min(log(rec)), log(0.05),-20)
  if(is.null(upper))
    upper <- c(max(log(rec * 20)), log(1.5),20)

  # SET TMB input
  inp <- list(
    # data
    Data = list(ssb=ssb, rec=rec,prior_s = c(to_logits(s),s.logitsd), spr0=spr0, nyears=length(ssb),
    # model
    Rmodel = which(model==c("bevholtSV","rickerSV","segreg"))-1),
    # inits
    Params = list(log_r0 = inits[1], log_sigR = inits[2],logit_s=inits[3]),
    # bounds
    lower=lower, upper=upper,
    #
    ReportSD = SDreport
  )
  
  # Compile TMB inputs 
  Map = list()
  # Turn off steepness estimation
  if(!s.est) Map[["logit_s"]] = factor( NA ) 

  # CREATE TMB object
  Obj <- TMB::MakeADFun(data = inp$Data, parameters = inp$Params,map=Map,
    DLL = "FLSRTMB", silent = TRUE)

  Opt <- stats::nlminb(start=Obj$par, objective=Obj$fn, gradient=Obj$gr,
    control=list("trace"=1, "eval.max"=1e4, "iter.max"=1e4),
    lower=inp$lower, upper=inp$upper)

  Opt[["diagnostics"]] = data.frame(Est=Opt$par,
    final_gradient=Obj$gr(Opt$par))
  
  Report <- Obj$report()
  
  if(SDreport) {
    SD <- try(TMB::sdreport(Obj))
  }

  # LOAD output in FLSR

  # DEBUG HACK
  model(object) <- switch(model, bevholtSV=bevholt, rickerSV=ricker,segreg=segreg)

  fitted(object) <- c(Report$rec_hat)
  residuals(object) <- log(rec(object)) - log(fitted(object))
  
  #params(object) <- FLPar(a=Report$a, b=Report$b)
  
  params(object)=FLPar(s=Report$s, R0=Report$r0)
  
  #attr(object,"SV") = data.frame(s=Report$s,sigmaR=Report$sigR,R0=Report$r0)
  
  #if(SDreport){
  #  object@vcov = matrix(SD$cov,nrow=2,dimnames = list(c("a","b"),c("a","b")))}

  return(object)
}
