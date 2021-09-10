#' @title 
#' 
#' @description 
#'
#' @param object an \code{FLStock} object 
#' @param seasons a numeric with seasons
#' 
#' @aliases
#' 
#' @return \code{FLStock} object
#'
#' @seealso \code{\link{expand}}
#'
#' @export seasonalise
#' @docType methods
#' @rdname seasonalise
#'
#' 
#' @examples
#' \dontrun{
#' }

seasonalise<-function(object, season=1:4){
  
  ## Stock and recruit                                   ###
  ## set expected to 1 and model variability as deviates ###
  sr=as.FLSR(object,model="geomean")
  params(sr)=FLPar(1,dimnames=list(params="a",iter=1))
  
  recs=expand(rec(object),season=season)
  
  ## Add seasons                                         ###
  object=expand(object,season=1:4)
  
  ## Divide up mortality by season                       ###
  m(      object)=m(object)/dim(object)[4]
  harvest(object)=harvest(object)/dim(object)[4]
  
  ## Initial stock numbers                               ###
  #for (i in 1:4)
  #  stock.n(object)[,1,,i]=stock.n(object)[,1,,i]*exp(-m(object)[,1,,i]-harvest(object)[,1,,i])
  
  ## Seasonal growth                                     ###
  stock.wt(   object)[,-dim(stock.wt(   object))[2]]=wtInterp(stock.wt(   object))
  catch.wt(   object)[,-dim(catch.wt(   object))[2]]=wtInterp(catch.wt(   object))
  landings.wt(object)[,-dim(landings.wt(object))[2]]=wtInterp(landings.wt(object))
  discards.wt(object)[,-dim(discards.wt(object))[2]]=wtInterp(discards.wt(object))
  
  object=adjust(object)
  
  ## Project for historic F                              ###
  #fbar=as(FLQuants("fbar"=fbar(object)[,-1]),"fwdControl")
  #object=fwd(object,control=fbar,sr=sr,residuals=recs)
  
  object}

