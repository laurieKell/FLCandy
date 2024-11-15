#' @title priors
#' 
#' @description priors for biomass dynamic assessment from an `FLBRP` object
#'
#' @param object An `FLBRP` from which to extract priors
#' @param ... additional arguments passed to methods
#' @return `priors` attribute as an `FLPar` 
#' @export

setGeneric("priors", function(object, ...) {
  standardGeneric("priors")})

#' Get priors for biomass dynamic assessment from an `FLBRP` object
#'
#' @param object An object of class `FLBRP`
#' @return priors attribute as an `FLPar` of the `FLBRP` object
#' @export
#' 
#' @examples
#' \dontrun{
#'   eql=FLBRP(...)  # Create or load an FLBRP object
#'   priors(eql)
#' }
setMethod("priors", signature(object="FLBRP"), function(object) {
    if (!("priors" %in% names(attributes(object)))) {
      warning("No priors attribute found for this FLBRP object.")
    return(NULL)}
    
  attributes(object)$priors})

setMethod("priors", signature(object="FLBRPs"), function(object) {
  rtn=FLPars() 
  for (i in objects){
    if (!("priors" %in% names(attributes(object)))){
       warning("No priors attribute found for this FLBRP object.")
       rtn[[i]]=NULL}
    else
      rtn[[i]]=attributes(object)$priors}
  
  return(rtn)})  

#' @description Assign an FLPar object as an attribute to an FLBRP object
#' using the '<-' operator.
#'
#' @param x An object of class FLBRP
#' @param i The name of the attribute to be assigned (as a character string)
#' @param value An object of class FLPar to be assigned as an attribute
#' @return The modified FLBRP object with the new attribute
#' @exportMethod `[<-`
#' @examples
#' \dontrun{
#'   eql=FLBRP(...)  # Create or load an FLBRP object
#'   eql["priors"]<-FLPar()
#'   attributes(eql)$priors=calcPriors(eql)
#' }
#' setMethod("[<-", signature(x="FLBRP", i="priors", j="missing", value="FLPar"),
#'   function(x, i, value) {
#'     if (!inherits(value, "FLPar")) {
#'       stop("The assigned value must be an FLPar object")
#'     }
#'     
#'     attributes(x,i)=value
#' 
#' return(x)})

#' Calculate priors for an `FLBRP` based on reference points and observations.
#'
#' @param object `An`FLBRP` object for which to calculate priors
#' @param ... Additional arguments passed to methods
#' @return A named vector of calculated prior values
#' @export
setGeneric("calcPriors", function(object, ...) {
  standardGeneric("calcPriors")})

#' Calculate priors for an FLBRP object
#'
#' This method calculates priors for an FLBRP object based on reference points and observations.
#'
#' @param object An object of class FLBRP
#' @return A named vector of calculated prior values
#' @export
#' @import FLife 
#' @importFrom stats optimize
#' @examples
#' \dontrun{
#'   eql=FLBRP(...)  # Create or load an FLBRP object
#'   calcPriors(eql)
#' }
setMethod("calcPriors", signature(object="FLBRP"), function(object) {
    # Extract reference points
    fmsy=c(refpts(object)["msy", "harvest"])
    bmsy=c(refpts(object)["msy", "ssb"])
    b0  =c(refpts(object)["virgin", "ssb"])
    
    # Calculate ratios for first and last years
    f.minyr=c(fbar.obs(object)[, 1]) 
    ssb.minyr=c(ssb.obs(object)[, 1])
    f.maxyr=c(fbar.obs(object)[, dim(fbar.obs(object))[2]]) 
    ssb.maxyr=c(ssb.obs(object)[, dim(ssb.obs(object))[2]]) 
    
    # Calculate shape parameter
    shape=bmsy / b0
    if (is.na(shape)) shape=0.4
    
    # Optimize to find m and calculate r
    optimize_func <- function(mi) {
      (mi^(-1 / (mi - 1)) - shape)^2
    }
    
    m=optimize(optimize_func, c(0.01, 2))$minimum
    r=(1-exp(-fmsy))*(m-1)/(1-m^-1)
    
    # Compile results
    rtn= c(r=r, mpar=m, fmsy=fmsy, bmsy=bmsy, b0=b0, shape=shape,
     ssb.minyr=ssb.minyr, ssb.maxyr=ssb.maxyr,
     f.minyr=f.minyr, f.maxyr=f.maxyr)
    
    names(rtn)= c("r", "mpar", "fmsy", "bmsy", "b0", "shape",
    "ssb.minyr", "ssb.maxyr",
    "f.minyr", "f.maxyr")
    
    return(FLPar(rtn))})


setMethod("calcPriors", signature(object="FLStock"), function(object,nmin=0:2,nmax=0:2){
    fmsy=unlist(c(attributes(object)$benchmark["Fmsy"]))
    
    bmsy=unlist(c(attributes(object)$eqsim["BMSY"]))
    b0  =unlist(c(attributes(object)$eqsim["B0"]))
    
    shape=bmsy/b0
    if (is.na(shape)) shape=0.4
    m    =NA
    r    =NA
    
    mi   =seq(0.01,2,0.001) 
    m    =(mi^(-1/(mi-1))-shape)^2
    m    =mi[m==min(m)]
    r    =(1-exp(-fmsy))*(m-1)/(1-m^-1)
 
    ssb.minyr=mean(ssb( object)[,1+nmin])
    f.minyr=mean(fbar(object)[,1+nmin])
    h.minyr=mean(hr(  object)[,1+nmin])
    ssb.maxyr=mean(ssb( object)[,dim(ssb( object))[2]-nmax])
    f.maxyr=mean(fbar(object)[,dim(fbar(object))[2]-nmax])
    h.maxyr=mean(hr(  object)[,dim(fbar(object))[2]-nmax])
    
   
    rtn=c(r=r,mpar=m,fmsy=fmsy,bmsy=bmsy,b0=b0,shape=shape,
          ssb.minyr=ssb.minyr,ssb.maxyr=ssb.maxyr,
          f.minyr=  f.minyr,  f.maxyr=  f.maxyr,
          h.minyr=  h.minyr,  h.maxyr=  h.maxyr)
    names(rtn)=c("r","mpar","fmsy","bmsy","b0","shape",
                 "ssb.minyr","ssb.maxyr",
                 "f.minyr",  "f.maxyr",
                 "h.minyr",  "h.maxyr")
    
    rtn})
  
setMethod("calcPriors", signature(object="FLStocks"), 
          function(object,nmin=0:2,nmax=0:2){
          ldply(object, function(x) tryIt(calcPriors(x, nmin=nmin, nmax=nmax)))})
          
  