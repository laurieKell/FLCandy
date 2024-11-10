# Plus group infinite series
#' @title spr0yr
#' 
#' @description Calculates the spawner per recruit in each year
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

spr0Yr<-function(object){
  survivors=exp(-apply(m(object),2,cumsum))
  survivors[-1]=survivors[-dim(survivors)[1]]
  survivors[1]=1
  expZ=exp(-m(object[dim(m(object))[1]]))
  if (!is.na(range(object)["plusgroup"]))
     survivors[dim(m(object))[1]]=survivors[dim(m(object))[1]]*(-1.0/(expZ-1.0))
  
  fec=mat(object)*stock.wt(object)*exp(-m(object)*m.spwn(object))
  
  rtn=apply(fec*survivors,2,sum)
  rtn}

# 
# spawnpr <- function(x) {
#   
#   # survival rates
#   surv <- exp(apply(-m(x), 2:6, cumsum))
#   
#   fsurv <- exp(apply(-m(x) - harvest(x), 2:6, cumsum))
#   
#   # SPR
#   res <- quantSums(fsurv  mat(x)  stock.wt(x)) /
#     quantSums(surv  mat(x)  stock.wt(x))
#   
#   return(res)}