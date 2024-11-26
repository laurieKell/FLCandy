#' @title tseries
#' 
#' @description Calculates the surplus production and expected yield etc for the estinates of SSB and biomass
#'
#' @param object an \code{FLBRP} object 
#' @param seasons a numeric with seasons
#' 
#' @aliases
#' 
#' @return \code{FLQuants} object
#'
#' @seealso \code{\link{expand}}
#'
#' @export tseries
#' @docType methods
#' @rdname tseries
#'
#' 
#' @examples
#' \dontrun{
#' }

setMethod("tseries", signature(object="FLBRP"), function(object){
  nms=dimnames(refpts(object))
  nms$refpt=paste("ssb",dimnames(ssb.obs(object))$year,sep="")
  
  rfs=FLPar(array(NA,laply(nms,length),dimnames=nms))
  rfs[,"ssb",]=ssb.obs(object)
  refpts(object)=rfs
  rtn=computeRefpts(object)
  
  rtn=alply(rtn,2,FLQuant,dimnames=dimnames(ssb.obs(object)))
  names(rtn)=as.character(unlist(attributes(rtn)$split_labels))
  
  discards.obs(object)[is.na(discards.obs(object))]=0
  
  rtn$spSSB    =ssb.obs(object)[,-1]-ssb.obs(object)[,-dim(ssb.obs(object))[2]]+catch.obs(object)[,-dim(ssb.obs(object))[2]]
  rtn$spBiomass=biomass.obs(object)[,-1]-biomass.obs(object)[,-dim(biomass.obs(object))[2]]+catch.obs(object)[,-dim(biomass.obs(object))[2]]

  rtn=mcf(as(rtn,"FLQuants"))
  
  rtn[["peSSB"]]    =rtn$spSSB-rtn$yield
  rtn[["peBiomass"]]=rtn$spB  -rtn$yield
  
  rtn})

surplusProduction<-tseries
