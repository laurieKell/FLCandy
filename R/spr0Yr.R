# Plus group infinite series
#' @title Spr0Yr - Spawning Per Recruit by Year
#' 
#' @description Calculates the spawner per recruit in each year from an FLStock object.
#' This function computes the unfished spawning biomass per recruit for each year,
#' accounting for natural mortality, maturity, and stock weights.
#'
#' @param object an \code{FLStock} object 
#' 
#' @return \code{FLQuant} object with spawning per recruit values by year
#'
#' @details
#' This function calculates the spawning potential ratio at unfished conditions (SPR0)
#' for each year in the FLStock object. It accounts for:
#' - Natural mortality rates
#' - Maturity schedules
#' - Stock weights
#' - Plus group handling if specified
#'
#' @seealso \code{\link[FLCore]{spr0}} for the standard FLCore SPR0 calculation
#'
#' @export
#' 
#' @examples
#' \dontrun{
#' data(ple4)
#' spr0_vals <- spr0Yr(ple4)
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