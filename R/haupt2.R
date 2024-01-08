#' Calculate total mortality-at-age (Z) using length data.
#' 
#' This is a generic S4 method that calculates something using the `haupt` function.
#' The specific implementation depends on the class of the input object.
#' 
#' @param object An object of class FLQuant or data.frame (depends on the method).
#' @param pars A parameter object (class FLPar) containing necessary parameters.
#' @param lc A threshold value for lc.
#' @param lmax A threshold value for lmax.
#' @param ... Additional arguments .
#' 
#' @return estimates of Z.
#' 
#' @export
setGeneric("haupt", function(object, pars, lc, lmax, ...) {
  standardGeneric("haupt")
})

#' Calculate something using the haupt method for FLQuant and FLPar objects.
#' 
#' This method calculates something using the `haupt` function when the input object is of class FLQuant and the parameter object is of class FLPar.
#' 
#' @param object An object of class FLQuant.
#' @param pars A parameter object (class FLPar) containing necessary parameters.
#' @param lc A threshold value for lc.
#' @param lmax A threshold value for lmax.
#' @param ... Additional arguments (not used in this example).
#' 
#' @return The result of the calculation.
#' 
#' @export
setMethod("haupt", signature("FLQuant", "FLPar"), 
          function(object, pars, lc = FLife::vonB(pars["sel1"], pars) * 0.9, lmax = "missing", ...) {
            result = hauptFn(object, pars, lc = lc, lmax = lmax)
            return(result)
          })

#' Calculate something using the haupt method for data.frame and numeric objects.
#' 
#' This method calculates something using the `haupt` function when the input object is of class data.frame and pars is of class numeric.
#' 
#' @param object An object of class data.frame.
#' @param pars A parameter object (numeric).
#' @param lc A threshold value for lc.
#' @param lmax A threshold value for lmax.
#' @param ... Additional arguments (not used in this example).
#' 
#' @return The result of the calculation.
#' 
#' @export
setMethod("haupt", signature("data.frame", "numeric"), 
          function(object, pars, lc = FLife::vonB(pars["sel1"], pars) * 0.9, lmax = "missing", ...) {
            result = hauptFn(object, pars, lc = lc, lmax = lmax)
            return(result)
          })

#' Calculate something using the haupt method for numeric objects.
#' 
#' This method calculates something using the `haupt` function when both input object and pars are of class numeric.
#' 
#' @param object An object of class numeric.
#' @param pars A parameter object (numeric).
#' @param lc A threshold value for lc.
#' @param lmax A threshold value for lmax.
#' @param ... Additional arguments (not used in this example).
#' 
#' @return The result of the calculation.
#' 
#' @export
setMethod("haupt", signature("numeric", "numeric"), 
          function(object, pars, lc = "missing", lmax = "missing", ...) {
            result = hauptFn(object, pars, lc = lc, lmax = lmax)
            return(result)
          })

#' Internal function for performing the haupt calculation.
#' 
#' This function performs the actual calculation used in the haupt methods.
#' 
#' @param lfd Input data.
#' @param pars A parameter object.
#' @param lc A threshold value for lc.
#' @param lmax A threshold value for lmax.
#' 
#' @return The result of the calculation.


hauptFn<-function(lfd,pars,lc,lmax="missing"){
  
  dat=lfd[as.numeric(dimnames(lfd)$len)>c(lc)]
  if (!missing(lmax))
    dat=dat[as.numeric(dimnames(dat)$len)<c(lmax)]
  
  l  =dat
  l[]=as.numeric(dimnames(l)[[1]])
  
  l1  =dat[-dim(dat)[1]]
  l1[]=as.numeric(dimnames(l1)[[1]])
  
  l2  =dat[-1]
  l2[]=as.numeric(dimnames(l2)[[1]])
  
  # dt=-log(1-l2/linf)/k+log(1-l1/linf)/k
  # t =t0*log(1-(l/linf))/k  
  
  dt=-log(1-l2%/%pars["linf"])/pars["k"]+log(1-l1/pars["linf"])/pars["k"]
  t =pars["t0"]*log(1-(l/pars["linf"]))/pars["k"]  
  
  dat=model.frame(FLQuants("y"=log(dat[-dim(t)[1]]%/%dt),
                           "x"=      t[-dim(t)[1]]))
  dat=subset(dat,is.finite(y))
  #z  =ddply(dat,.(year,iter), with, data.frame(data=-lmRob(y~x)$coefficients["x"]))
  z  =ddply(dat,.(year,iter), with, data.frame(data=-lm(y~x)$coefficients["x"]))
  z  =transform(z,year=factor(year),iter=factor(iter))
  as.FLQuant(z)}

if (FALSE){
  
  library(ggplotFL)
  library(FLCore)
  
  data(ple4)
  
  load("/home/laurence-kell/Desktop/papers/COM3/R/runs/om/om.18.RData")
  load("/home/laurence-kell/Desktop/papers/COM3/R/runs/indicators/lfd.18.RData")
  load("/home/laurence-kell/Desktop/papers/COM3/data/lhs.RData")
  
  z=haupt(lfdc,lhs[[3]])
  
  plot(z)
}
