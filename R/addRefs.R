## Adds the ICES PA & MSY reference points to refpts and fits a SRR
#' @title Adds ICES PA & MSY reference points to refpts and fits a SRR
#' 
#' @description Adds ICES PA & MSY reference points to an `FLStock` 
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
addRefs<-function(x,refs){
  x=FLPar(NA,dimnames=list(refpt=c(dimnames(x)$refpt,dimnames(refs)$params),
                           quant=c("harvest","yield","rec","ssb","biomass","revenue","cost","profit"),iter=1))
  
  x[unlist(gregexpr("B",dimnames(x)$refpt))==1,"ssb"]    =refs[unlist(gregexpr("B",dimnames(refs)[[1]])>0),]
  x[unlist(gregexpr("F",dimnames(x)$refpt))==1,"harvest"]=refs[unlist(gregexpr("F",dimnames(refs)[[1]])>0),]
  
  x} 
