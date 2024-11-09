#' Rebuild a fish population.
#'
#' @param 'object' of class \code{FLBRP} representing the fish population
#' at equilibrium
#' @param targetF The target fishing mortality rate during rebuilding, by default = 0
#' @param targetSSB The highest initial SSB to rebuild from, by default $B_{MSY}$
#' @param minSSB The lowest initial SSB to rebuild from, by default 1e-10
#' @param length.out Number of initial SSB levels.
#' @param burnin Number of iterations for burn-in, i.e., to make sure age-structure
#' at the start of rebuilding is at equilibrium.
#' @param truncate Logical, whether to truncate results, i.e., remove burn-in period.
#'
#' @export
setGeneric("rebuild", function(object, ...) {
  standardGeneric("rebuild")
})

setMethod("rebuild", signature(object = "FLBRP"), 
          function(object, 
                   targetF = computeRefpts(object)["msy","harvest"] * 0,
                   targetSSB = computeRefpts(object)["msy","ssb"],
                   minSSB = 1e-10,
                   length.out = 50, 
                   burnin = 20, 
                   truncate = TRUE) {
  
 
            eql=object
            fbar(eql)[]=0.2
            
            dimnames(landings.sel(eql))=dimnames(landings.sel(eql))
            dimnames(discards.sel(eql))=dimnames(discards.sel(eql))

            targetSSB=c(targetSSB)*c(1e-10,seq(length.out)/length.out)
            targetF=c(targetF)
            
            btar  =FLQuant(rep(targetSSB,each=dim(fbar(eql))[2]),dimnames=dimnames(propagate(ssb(eql),length.out+1)))
            stk   =propagate(as(eql,"FLStock"),length.out+1)
            stk   =fwd(stk,ssb_end=btar[,-seq(dims(stk)[["min"]]+2)],sr=eql)
            ftar  =fbar(stk)%=%targetF
            
            stk   =fwd(stk,f=ftar[,-seq(burnin)],sr=eql)
            
            if (truncate) stk=stk[,-seq(burnin)]
            
            stk=qapply(stk,function(x) {dimnames(x)$year=seq(length(dimnames(x)$year))
            x})
  stk
})

#' Calculate rebuilding time based on fish stock data.
#'
#' @param stk An object of class \code{FLStock} representing the fish stock data.
#'
#' @return A data frame with columns "Initial" and "Year" representing the initial depletion 
#' and the corresponding year, respectively.
#'
#' @export
setGeneric("rebuildTime", function(object, ...) {
  standardGeneric("rebuildTime")
})
setMethod("rebuildTime", signature(object = "FLStock"), 
          function(object) {
              
              msy=c(ssb(iter(object,dim(object)[6]))[,1])
              dt1=transmute(as.data.frame(ssb(object),drop=T),
                            
                            ssb    =data/msy,
                            initial=c(ssb(object[,1]))[an(ac(iter))]/msy,
                            year   =year)
              t=suppressWarnings(as.data.frame(akima:::interp(dt1$initial,dt1$ssb,dt1$year,
                                                              xo=seq(0,1,length.out=202)[-c(1,202)],yo=1,duplicate="mean")))
              
              data.frame(initial=t$x,year=t$z)})
#' @examples
#' \dontrun{
#' library(FLCore)
#' library(FLBRP)
#'
#' data(ple4brp)
#' 
#' stk=rebuild(eq)
#' 
#' rebuildTime(stk)
#' }

#' Rebuild Interpolation Method
#'
#' @param x Numeric vector representing the x-values.
#' @param y Numeric vector representing the y-values.
#' @param new Numeric vector representing the new values for interpolation.
#' @return Interpolated values.
#'
#' @export
setGeneric("rebuildInterp", function(x, y, new, ...) {
  standardGeneric("rebuildInterp")
})

setMethod("rebuildInterp", signature(x = "numeric", y = "numeric", new = "numeric"),
          function(x, y, new, ...) {
            splinefun(x, y)(new)
          }
)

blimFn<-function(x){
  rec=refpts(x)["virgin","rec",drop=T]*0.2
  refpts(x)=FLPar(NA,dimnames=list(refpt="rec",quant=c("harvest","yield","rec","ssb","biomass","revenue","cost","profit"),iter=1))
  refpts(x)[,"rec"]=rec
  computeRefpts(x)["rec"]}

## Define generic
setGeneric("blim", function(object, ...) standardGeneric("blim"))

## Method for FLBRP
setMethod("blim", signature(object="FLBRP"),
          function(object) {
            # Get virgin recruitment at 20%
            rec <- refpts(object)["virgin","rec",drop=TRUE] * 0.2
            
            # Create new FLPar with NAs
            refpts(object) <- FLPar(NA, 
                                    dimnames=list(refpt="rec",
                                                  quant=c("harvest","yield","rec",
                                                          "ssb","biomass","revenue",
                                                          "cost","profit"),
                                                  iter=dimnames(refpts(x))$iter))
            
            # Set recruitment
            refpts(object)[,"rec"]=rec
            
            # Return computed reference points
            rtn=computeRefpts(object)["rec"]
            dimnames(rtn)
            dimnames(rtn)$refpt="blim"
            rtn
          })


