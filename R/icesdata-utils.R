#' @rdname ebiomass
#' @export
setMethod("ebiomass", signature(object="FLStock"),
          function(object) {
            sel   <- harvest(object)
            wt    <- catch.wt(object) %*% sel %/% fapex(sel)
            eb.wt <- qmax(wt, 0.000001)
            
            apply(eb.wt %*% stock.n(object), 2:6, sum)
          })

setMethod("ebiomass", signature(object="FLBRP"),
          function(object) {
            sel   <- harvest(object)
            wt    <- catch.wt(object) %*% sel %/% fapex(sel)
            eb.wt <- qmax(wt, 0.000001)
            
            apply(eb.wt %*% stock.n(object), 2:6, sum)
          })

#' @rdname stdz
#' @export
setMethod("stdz", signature(object="numeric"),
          function(object, na.rm=TRUE) {
            object=object-mean(  object, na.rm=na.rm)
            object/sqrt(var(object, na.rm=na.rm))
          })

#' @rdname stdz
#' @export
setMethod("stdz", signature(object="matrix"),
          function(object, na.rm=TRUE) {
            object=object-mean(object, na.rm=na.rm)
            object/sqrt(var(object, na.rm=na.rm))
          })

#' @rdname stdz
#' @export
setMethod("stdz", signature(object="array"),
          function(object, na.rm=TRUE) {
            object=object-mean(object, na.rm=na.rm)
            object/sqrt(var(object, na.rm=na.rm))
          })

#' @rdname stdz
#' @export
setMethod("stdz", signature(object="FLQuant"),
          function(object, na.rm=TRUE) {
            object=object-mean(object, na.rm=na.rm)
            object/sqrt(var(object, na.rm=na.rm))
          })

#' Internal Function for Benchmark Extraction
#'
#' @param x An FLStock object
#' @return An FLPar object
benchmarksFn <- function(object) {
  if ("logical"%in%is(attributes(object)$benchmark))
    return(FLPar(fmsy=NA,flim=NA,fpa=NA,blim=NA,bpa=NA,btrigger=NA))
  
  if ("numeric"%in%is(attributes(object)$benchmark))
    attributes(object)$benchmark=FLPar(attributes(object)$benchmark)
  
  names(attributes(object)$benchmark)=tolower(names(attributes(object)$benchmark))

  nms=names(attributes(object)$benchmark)[names(attributes(object)$benchmark)%in%
                                   c("fmsy","flim","fpa","blim","bpa","btrigger")]
                                 
  as(attributes(object)$benchmark[nms],"FLPar")
}

#' @rdname benchmark
#' @export
setMethod("benchmark", signature(object="FLStock"), function(object) {
  if (!("benchmark" %in% names(attributes(object)))) {
    warning("No benchmark attribute found for this FLStock object.")
    return(NULL)}
  
  names(attributes(object)$benchmark)=tolower(names(attributes(object)$benchmark))

  benchmarksFn(object)
})

#' @rdname benchmark
#' @export
setMethod("benchmark", signature(object="FLStocks"), function(object) {
  ldply(llply(object, function(x) t(benchmark(x))),rbind.fill)
})


#' Internal Function for FishLife Parameter Extraction
#'
#' @param object An FLStock object
#' @return An FLPar object
fishlifesFn <- function(object) {
  if ("logical"%in%is(attributes(object)$fishlife))
    return(FLPar(Fmsy=NA,Flim=NA,Fpa=NA,Blim=NA,Bpa=NA,Btrigger=NA))
  
  if ("numeric"%in%is(attributes(object)$fishlife))
    attributes(object)$fishlife=FLPar(attributes(object)$fishlife)
  
  as(attributes(object)$fishlife,"FLPar")
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
#' @param object An FLStock object
#' @return An FLPar object
eqsimFn <- function(object) {
  if ("logical"%in%is(attributes(object)$eqsim))
    return(FLPar(catchequi=NA,bmsy=NA,b0=NA,fmsyMedianC=NA,fmsyMedianL=NA,f5percRiskBlim=NA,flimEqsim=NA,r0)) 
  
  if ("numeric"%in%is(attributes(object)$eqsim))
    attributes(object)$eqsim=FLPar(attributes(object)$eqsim)
  
  names(attributes(object)$eqsim)=str_c(tolower(str_sub(names(attributes(object)$eqsim), 1, 1)), 
                                           str_sub(names(attributes(object)$eqsim), 2))

  names(attributes(object)$eqsim)=str_replace_all(names(attributes(object)$eqsim), "MSY", "msy")
  
  nms=names(attributes(object)$eqsim)[names(attributes(object)$eqsim)%in%
    c("catchequi","bmsy","b0","fmsyMedianC","fmsyMedianL","f5percRiskBlim","flimEqsim","r0")]

  as(attributes(object)$eqsim[nms],"FLPar")}

#' @rdname eqsim
#' @export
setMethod("eqsim", signature(object="FLStock"), function(object) {
  if (!("eqsim" %in% names(attributes(object)))) {
    warning("No eqsim attribute found for this FLStock object.")
    return(NULL)}
  
  eqsimFn(object)
})

#' @rdname eqsim
#' @export
setMethod("eqsim", signature(object="FLStocks"), function(object) {
  ldply(llply(icesdata, function(x) t(eqsim(x))),rbind.fill)
})

#' Internal Function for FLife Parameter Extraction
#'
#' @param object An FLStock object
#' @return An FLPar object with life history parameters
FLifeParFn <- function(object) {
  res=attributes(object)$fishlife
  
  if (!("fishlife"%in%names(attributes(object))))
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

#' Extracts Kobe Indicators  (SSB/BMSY and F/FMSY)
#' 
#' @description 
#' Calculates SSB and fishing mortality ratios (SSB/BMSY and F/FMSY)
#' for Kobe plot from an FLStock object.
#' 
#' @param path An object of class FLStock containing stock assessment results
#' @param method Not used, included for method consistency
#' 
#' @return An FLQuants object containing:
#' \itemize{
#'   \item stock: Time series of SSB/BMSY ratios
#'   \item harvest: Time series of F/FMSY ratios
#' }
#' 
#' @details 
#' The method requires that the FLStock object has:
#' \itemize{
#'   \item eqsim attribute containing BMSY reference point
#'   \item benchmark attribute containing FMSY reference point
#' }
#' 
#' @export
#' @rdname kobe
#' 
#' @examples
#' \dontrun{
#' data(ple4)
#' kobe_indicators <- kobe(ple4)
#' }
#' 
#' @seealso 
#' \code{\link[FLCore]{FLStock}}, \code{\link[FLCore]{FLQuants}}
#' 
#' @references 
setMethod( 'kobe',  signature(path='FLStock',method="missing"), 
           function(path,method){ 
               names(attributes(path)$eqsim)    =tolower(names(attributes(path)$eqsim))
               names(attributes(path)$benchmark)=tolower(names(attributes(path)$benchmark))
               
               FLQuants(path, "stock"  =function(x) ssb(x)%/%eqsim(      x)["bmsy"],
                              "harvest"=function(x) fbar(x)%/%benchmark(x)["fmsy"])})
           
setMethod( 'kobe',  signature(path='FLBRP',method="missing"), 
           function(path,method){ 
             
             FLQuants(path, "stock"  =function(x) ssb.obs( x)%/%refpts(x)["msy","ssb"],
                            "harvest"=function(x) fbar.obs(x)%/%refpts(x)["msy","harvest"])})
