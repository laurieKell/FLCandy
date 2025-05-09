#' @title adjust
#' 
#' @description Takes an `FLStock` that was annual and adjusts numbers-at-age to 
#' take accoubt of seasons  
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

adjust<-function(object){
  dim=dim(object)
  
  un =units(catch.n( object))
  uwt=units(catch.wt(object))
  
  n  =stock.n(object)
  m  =m(object)
  f  =harvest(object)
  pg=stock.n(object)[dim[1],,,dim[4]]*exp(-f[dim[1],,,dim[4]]-m[dim[1],,,dim[4]])
  
  for (i in seq(dim(object)[2]-1))
    for (j in seq(dim(object)[4])){
      if (j!=dim(object)[4])
        stock.n(object)[,i,,j+1]=stock.n(object)[,i,,j]*exp(-f[,i,,j]-m[,i,,j])
      else{
        stock.n(object)[-1,i+1,,1]=stock.n(object)[-dim[1],i,,j]*exp(-f[-dim[1],i,,j]-m[-dim[1],i,,j])
        stock.n(object)[dim[1],i+1,,1]=stock.n(object)[dim[1],i+1,,1]+pg[,i,,1]}
    }
  
  catch.n(object)=stock.n(object)*f/(m+f)*(1-exp(-f-m))
  landings.n(object)[is.na(landings.n(object))]=0
  discards.n(object)[is.na(discards.n(object))]=0
  
  landings.n(object)=catch.n(object)*discards.n(object)/(discards.n(object)+landings.n(object))
  discards.n(object)=catch.n(object)-landings.n(object)
  
  units(catch.n(object))   =un
  units(landings.n(object))=un
  units(discards.n(object))=un
  
  units(catch.wt(object))   =uwt
  units(landings.wt(object))=uwt
  units(discards.wt(object))=uwt
  
  catch(object)=computeCatch(object,"all")  
  
  object}
