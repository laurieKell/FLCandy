#' @title Age at which a cohort achieves it;s maximum biomass
#' 
#' @description Age at which a cohort achieves it;s maximum biomass, i.e. where
#' losses due to M are greater than gains die to increase in size/mass 
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
#' data(ple4brp)
#' aopt(ple4brp)
#' }
#' 
aopt<-function(object){
  model(object)=geomean()$model
  params(object)=FLPar(a=1)
  
  fbar(object)=FLQuant(0,dimnames=list(year=1))
  stock.n(object)%*%stock.wt(object)
  
  ages(ao)[ao==max(ao)]}


