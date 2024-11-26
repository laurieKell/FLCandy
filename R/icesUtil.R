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


#' @rdname ebiomass
#' @export
setMethod("ebiomass", signature(object="FLStock"),
          function(object) {
            sel   <- harvest(object)
            wt    <- catch.wt(object) %*% sel %/% fapex(sel)
            eb.wt <- qmax(wt, 0.000001)
            
            apply(eb.wt %*% stock.n(object), 2:6, sum)
          })

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