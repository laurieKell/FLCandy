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

spr0Cohort<-function(object){
  m     =FLCohort(m(object))
  mspwn =FLCohort(m.spwn(object))
  wt    =FLCohort(stock.wt(object))
  mat   =FLCohort(mat(object))
  
  survivors=exp(-apply(m,2,cumsum))
  survivors[-1]=survivors[-dim(survivors)[1]]
  survivors[1]=1
  expZ=exp(-m[dim(m)[1]])
  survivors[dim(m)[1]]=survivors[dim(m)[1]]*(-1.0/(expZ-1.0))
  fec      =mat*wt*exp(-m*mspwn)
  apply(fec*survivors,2,sum)}
  
if (FALSE){
## handy work
object=setPlusGroup(ple4,500)
survivors=exp(-apply(m(object),2,cumsum))
survivors[-1]=survivors[-dim(survivors)[1]]
survivors[1]=1
fec      =mat(object)*stock.wt(object)*exp(-m(object)*m.spwn(object))
apply(fec*survivors,2,sum)

plot(spr0Yr(ple4))
spr0Cohort(ple4)}
