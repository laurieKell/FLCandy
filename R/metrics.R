#' @title 
#' 
#' @description 
#'
#' @param object an \code{FLBRP} object 
#' 
#' @return \code{FLPar} with estimates of MSY based reference points
#'
#' @seealso \code{\link{FLBRP},\link{refpts}}
#'
#' @export metrics
#' @docType methods
#' @rdname metrics
#'
#' 
#' @examples
#' \dontrun{
#' 
#' data(ple4brp)
#' 
#' fbar(ple4brp)=FLQuant(seq(0,c(refpts(ple4brp)["crash","harvest"]),length.out=101))
#' refpts(ple4brp)=properties(ple4brp)
#' 
#' plot(ple4brp,ncol=2)
#' }
#' 
setGeneric("properties", function(object, ...)
  standardGeneric("metrics"))

setMethod("properties", signature(object="FLBRP"),
    function(object, ...) {
  
        msy=computeRefpts(object)["msy"]
  
        dmns=dimnames(msy)
        dmns$refpt=c(dmns$refpt,"0.5MSY","lower pgy","upper pgy","2*prod","virgin","crash")
        refpts(object)     =FLPar(NA,dimnames=dmns)
        refpts(object)["0.5MSY",   c("harvest","yield")]=msy[,c("harvest","yield")]*c(1.2,0.5)
        refpts(object)["lower pgy",c("harvest","yield")]=msy[,c("harvest","yield")]*c(1.2,0.8)
        refpts(object)["upper pgy",c("harvest","yield")]=msy[,c("harvest","yield")]*0.8
        refpts(object)["2*prod",   c("yield",  "ssb")]  =msy[,c("yield",  "ssb")]*c(2,1)
        
        computeRefpts(object)})
  
