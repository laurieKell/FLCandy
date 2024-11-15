#' Extract Time Series Statistics from FLStock Objects
#'
#' @description
#' Creates a data frame of time series statistics from FLStock objects, including
#' catch, ebiomass, SSB, fishing mortality, harvest rate, and mean natural mortality.
#'
#' @param object An object of class FLStock or FLStocks
#' @param ... Additional arguments (not currently used)
#'
#' @return A data frame containing time series of:
#'   \itemize{
#'     \item catch - Catch values
#'     \item eb - Exploitable biomass
#'     \item ssb - Spawning stock biomass
#'     \item f - Fishing mortality (Fbar)
#'     \item h - Harvest rate (catch/ebiomass)
#'     \item m - Mean natural mortality
#'   }
#'
#' @examples
#' \dontrun{
#' # For single stock
#' data(ple4)
#' ts1 <- tseries(ple4)
#'
#' # For multiple stocks
#' stocks <- FLStocks(stock1=ple4, stock2=ple4)
#' ts2 <- tseries(stocks)
#' }
#'
#' @export
setGeneric("tseries", function(object, ...) standardGeneric("tseries"))


#' @rdname tseries
#' @export
setMethod("tseries", signature(object="FLStock"),
          function(object,flqs=list(catch=function(object) catch(object),
                                    eb   =function(object) ebiomass(object),
                                    ssb  =function(object) ssb(object),
                                    f    =function(object) fbar(object),
                                    h    =function(object) catch(object)/ebiomass(object),
                                    m    =function(object) FLQuant(aaply(m(object)[ac(range(object)["minfbar"]:range(object)["maxfbar"])],2,mean),
                                                                        dimnames=dimnames(fbar(object))))){
      model.frame(metrics(object,flqs),drop=TRUE)})

#' @rdname tseries
#' @export
setMethod("tseries", signature(object="FLStocks"),
          function(object,flqs=list(catch=function(object) catch(object),
                                   eb   =function(object) ebiomass(object),
                                   ssb  =function(object) ssb(object),
                                   f    =function(object) fbar(object),
                                   h    =function(object) catch(object)/ebiomass(object),
                                   m    =function(object) FLQuant(aaply(m(object)[ac(range(object)["minfbar"]:range(object)["maxfbar"])],2,mean),
                                                                  dimnames=dimnames(fbar(object))))){
            result=lapply(object, tseries, flqs=flqs)
            rtn=do.call(rbind, result)
            rtn=cbind(.id=gsub("\\.([^.]+)$","",dimnames(rtn)[[1]]),rtn)
            rtn})

#' Calculate Exploitable Biomass
#'
#' @description
#' Calculates the exploitable biomass from an FLStock object using selectivity-weighted
#' catch weights and stock numbers.
#'
#' @param object An object of class FLStock
#'
#' @return An FLQuant object containing the exploitable biomass time series
#'
#' @details
#' Exploitable biomass is based on weighting catch weights by selectivity normalised 
#' by peak selectivity, then multiplying by stock numbers and summing across ages
#'
#' @examples
#' \dontrun{
#' data(ple4)
#' eb <- ebiomass(ple4)
#' }
#'
#' @export
setGeneric("ebiomass", function(object) standardGeneric("ebiomass"))

#' @rdname ebiomass
#' @export
setMethod("ebiomass", signature(object="FLStock"),
          function(object) {
            sel   <- harvest(object)
            wt    <- catch.wt(object) %*% sel %/% fapex(sel)
            eb.wt <- qmax(wt, 0.000001)
            
            apply(eb.wt %*% stock.n(object), 2:6, sum)
          })

#' Standardize Values
#'
#' @description
#' Standardizes values by subtracting the mean and dividing by the standard deviation.
#'
#' @param x A numeric vector, matrix, array or FLQuant
#' @param na.rm Logical indicating whether to remove NA values when computing statistics
#'
#' @return An object of the same class as the input with standardized values
#'
#' @details
#' Standardization follows the formula: (x - mean(x))/sd(x)
#'
#' @examples
#' \dontrun{
#' # For numeric vector
#' x <- 1:10
#' stdz(x)
#'
#' # For FLQuant
#' data(ple4)
#' standardized_catch <- stdz(catch(ple4))
#' }
#'
#' @export
setGeneric("stdz", function(x, na.rm=TRUE) standardGeneric("stdz"))

#' @rdname stdz
#' @export
setMethod("stdz", signature(x="numeric"),
          function(x, na.rm=TRUE) {
            x=x-mean(  x, na.rm=na.rm)
            x/sqrt(var(x, na.rm=na.rm))
          })

#' @rdname stdz
#' @export
setMethod("stdz", signature(x="matrix"),
          function(x, na.rm=TRUE) {
            x=x-mean(x, na.rm=na.rm)
            x/sqrt(var(x, na.rm=na.rm))
          })

#' @rdname stdz
#' @export
setMethod("stdz", signature(x="array"),
          function(x, na.rm=TRUE) {
            x=x-mean(x, na.rm=na.rm)
            x/sqrt(var(x, na.rm=na.rm))
          })

#' @rdname stdz
#' @export
setMethod("stdz", signature(x="FLQuant"),
          function(x, na.rm=TRUE) {
            x=x-mean(x, na.rm=na.rm)
            x/sqrt(var(x, na.rm=na.rm))
          })

#' Extract Benchmark Reference Points
#'
#' @description
#' A generic function to extract benchmark reference points from FLStock objects
#'
#' @param object An FLStock or FLStocks object
#' @param ... Additional arguments (not currently used)
#'
#' @return An FLPar object containing benchmark reference points (Fmsy, Flim, Fpa, Blim, Bpa, Btrigger)
#'
#' @export
setGeneric("benchmark", function(object, ...) {
  standardGeneric("benchmark")
})

#' Internal Function for Benchmark Extraction
#'
#' @param x An FLStock object
#' @return An FLPar object
benchmarksFn <- function(x) {
  if ("logical"%in%is(attributes(x)$benchmark))
    return(FLPar(Fmsy=NA,Flim=NA,Fpa=NA,Blim=NA,Bpa=NA,Btrigger=NA))
  
  if ("numeric"%in%is(attributes(x)$benchmark))
    attributes(x)$benchmark=FLPar(attributes(x)$benchmark)
  
  as(attributes(x)$benchmark,"FLPar")
}

#' @rdname benchmark
#' @export
setMethod("benchmark", signature(object="FLStock"), function(object) {
  if (!("benchmark" %in% names(attributes(object)))) {
    warning("No benchmark attribute found for this FLStock object.")
    return(NULL)}
  
  benchmarksFn(object)
})

#' @rdname benchmark
#' @export
setMethod("benchmark", signature(object="FLStocks"), function(object) {
  ldply(llply(icesdata, function(x) t(benchmark(x))),rbind.fill)
})

#' Extract FishLife Parameters
#'
#' @description
#' A generic function to extract FishLife parameters from FLStock objects
#'
#' @param object An FLStock or FLStocks object
#' @param ... Additional arguments (not currently used)
#'
#' @return An FLPar object containing FishLife parameters
#'
#' @export
setGeneric("fishlife", function(object, ...) {
  standardGeneric("fishlife")
})

#' Internal Function for FishLife Parameter Extraction
#'
#' @param x An FLStock object
#' @return An FLPar object
fishlifesFn <- function(x) {
  if ("logical"%in%is(attributes(x)$fishlife))
    return(FLPar(Fmsy=NA,Flim=NA,Fpa=NA,Blim=NA,Bpa=NA,Btrigger=NA))
  
  if ("numeric"%in%is(attributes(x)$fishlife))
    attributes(x)$fishlife=FLPar(attributes(x)$fishlife)
  
  as(attributes(x)$fishlife,"FLPar")
}

#' @rdname fishlife
#' @export
setMethod("fishlife", signature(object="FLStock"), function(object) {
  if (!("fishlife" %in% names(attributes(object)))) {
    warning("No fishlife attribute found for this FLStock object.")
    return(NULL)}
  
  fishlifesFn(object)
})

#' @rdname fishlife
#' @export
setMethod("fishlife", signature(object="FLStocks"), function(object) {
  ldply(llply(icesdata, function(x) t(fishlife(x))),rbind.fill)
})

#' Extract EqSim Reference Points
#'
#' @description
#' A generic function to extract EqSim reference points from FLStock objects
#'
#' @param object An FLStock or FLStocks object
#' @param ... Additional arguments (not currently used)
#'
#' @return An FLPar object containing EqSim reference points
#'
#' @export
setGeneric("eqsim", function(object, ...) {
  standardGeneric("eqsim")
})

#' Internal Function for EqSim Reference Point Extraction
#'
#' @param x An FLStock object
#' @return An FLPar object
eqsimsFn <- function(x) {
  if ("logical"%in%is(attributes(x)$eqsim))
    return(FLPar(Fmsy=NA,Flim=NA,Fpa=NA,Blim=NA,Bpa=NA,Btrigger=NA))
  
  if ("numeric"%in%is(attributes(x)$eqsim))
    attributes(x)$eqsim=FLPar(attributes(x)$eqsim)
  
  as(attributes(x)$eqsim,"FLPar")
}

#' @rdname eqsim
#' @export
setMethod("eqsim", signature(object="FLStock"), function(object) {
  if (!("eqsim" %in% names(attributes(object)))) {
    warning("No eqsim attribute found for this FLStock object.")
    return(NULL)}
  
  eqsimsFn(object)
})

#' @rdname eqsim
#' @export
setMethod("eqsim", signature(object="FLStocks"), function(object) {
  ldply(llply(icesdata, function(x) t(eqsim(x))),rbind.fill)
})

#' Extract FLife Parameters
#'
#' @description
#' A generic function to extract FLife parameters from FLStock objects
#'
#' @param object An FLStock or FLStocks object
#' @param ... Additional arguments (not currently used)
#'
#' @return An FLPar object containing life history parameters
#'
#' @export
setGeneric("FLifePar", function(object, ...) {
  standardGeneric("FLifePar")
})

#' Internal Function for FLife Parameter Extraction
#'
#' @param x An FLStock object
#' @return An FLPar object with life history parameters
FLifeParFn <- function(x) {
  res=attributes(x)$fishlife
  
  if (!("fishlife"%in%names(attributes(x))))
    return(lhPar(FLPar(c("linf"=NA,"k"=NA,"l50"=NA,"s"=NA))))
  
  if ("lm"%in%names(res))
    names(res)[seq(length(res))[(names(res)=="lm")]]="l50"
  
  res=FLPar(res,units="NA")
  rtn=FLPar("linf"     =NA,
            "k"        =NA,       
            "winf"     =NA,       
            "tmax"     =NA,       
            "tm"       =NA,       
            "m"        =NA,      
            "lm"       =NA,       
            "rho"      =NA,       
            "sigmaR"   =NA,     
            "s"        =NA,     
            "fmsy"     =NA,     
            "r"        =NA,    
            "g"         =NA,     
            "sd.logit.s"=NA)
  
  lhPar(res[c("linf","k","l50","s")])
}

#' @rdname FLifePar
#' @export
setMethod("FLifePar", signature(object="FLStock"), function(object) {
  if (!("fishlife" %in% names(attributes(object)))) {
    warning("No benchmark attribute found for this FLStock object.")
    return(NULL)}
  
  FLifeParFn(object)
})

#' @rdname FLifePar
#' @export
setMethod("FLifePar", signature(object="FLStocks"), function(object) {
  rtn=ldply(llply(object, function(x) t(FLifePar(x))),rbind.fill)
  rtn
})


tryIt<-function(x){
  rtn=try(x)
  if ("try-error"%in%is(rtn)) return(NULL)
  return(rtn)}

tryIt<-function(x){
  rtn=try(x)
  if ("try-error"%in%is(rtn)) return(NULL)
  return(rtn)}

kobeFn<-function(x){
  names(attributes(x)$eqsim)    =tolower(names(attributes(x)$eqsim))
  names(attributes(x)$benchmark)=tolower(names(attributes(x)$benchmark))
  
  FLQuants(x, "stock"  =function(x) ssb(x)%/%eqsim(      x)["bmsy"],
           "harvest"=function(x) fbar(x)%/%benchmark(x)["fmsy"])}

setGeneric('kobe',  function(path,method,...) standardGeneric('kobe'))
setMethod( 'kobe',  signature(path='FLStock',method="missing"), 
           function(path,method) {tryIt(kobeFn(path))})

