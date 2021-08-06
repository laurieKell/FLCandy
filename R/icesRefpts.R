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

icesRefpts<-function(x,refs=NULL,model="bevholtSV",steepness=0.7,nyears=3) {
  eq=FLBRP(x,nyears=nyears)
  
  if (gregexpr("SV",model)[[1]][1]>0){
    sr=as.FLSR(x,model=model)
    sr=fmle(sr,
            fixed=list(s=steepness,spr0=spr0(eq)),
            control=list(silent=TRUE),
            method="Brent",
            lower=c(0.001),upper=max(ssb(sr))*10)
    params(eq)=ab(params(sr),substr(model,1,gregexpr("SV",model)[[1]][1]-1))[-dim(params(sr))[1]]
    model( eq)=do.call(substr(model,1,gregexpr("SV",model)[[1]][1]-1), list())$model
    refpts(eq)=computeRefpts(eq)
  }else{
    sr=fmle(as.FLSR(x,model=model),control=list(silent=TRUE))
    params(eq)=params(sr)
    model( eq)=do.call(model, list())$model}
  
  fbar(eq)[]=seq(0,100,1)/100*c(computeRefpts(eq)["crash","harvest"])
  
  if (!is.null(refs))
    refpts(eq)=addRefs(refpts(eq),refs)
  
  eq=brp(eq)
  eq}
