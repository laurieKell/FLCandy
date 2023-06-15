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
  
  recs=FLCore:::expand(rec(object),season=season)
  
  ## Add seasons                                         ###
  object=FLCore:::expand(object,season=season)
  
  ## Divide up mortality by season                       ###
  m(      object)=wtInterp(m(object))/dim(object)[4]
  harvest(object)=harvest(object)/dim(object)[4]
  
  ## Seasonal growth                                     ###
  stock.wt(   object)=wtInterp(stock.wt(   object))
  catch.wt(   object)=wtInterp(catch.wt(   object))
  landings.wt(object)=wtInterp(landings.wt(object))
  discards.wt(object)=wtInterp(discards.wt(object))
  #m(          object)=wtInterp(          m(object))
  
  catch(object)=computeCatch(object,slot="all")

  object=adjust(object)
  
  ## Project for historic F                              ###
  #fbar=as(FLQuants("fbar"=fbar(object)[,-1]),"fwdControl")
  #object=fwd(object,control=fbar,sr=sr,residuals=recs)
  
  object}

adjust<-function (object) 
{
  dim = dim(object)
  un = units(catch.n(object))
  uwt = units(catch.wt(object))
  n = stock.n(object)
  m = m(object)
  f = harvest(object)
  pg = stock.n(object)[dim[1], , , dim[4]] * exp(-f[dim[1], 
                                                    , , dim[4]] - m[dim[1], , , dim[4]])
  for (i in seq(dim(object)[2] - 1)) for (j in seq(dim(object)[4])) {
    if (j != dim(object)[4]) 
      stock.n(object)[, i, , j + 1] = stock.n(object)[, 
                                                      i, , j] * exp(-f[, i, , j] - m[, i, , j])
    else {
      stock.n(object)[-1, i + 1, , 1] = stock.n(object)[-dim[1], 
                                                        i, , j] * exp(-f[-dim[1], i, , j] - m[-dim[1], 
                                                                                              i, , j])
      stock.n(object)[dim[1], i + 1, , 1] = stock.n(object)[dim[1], 
                                                            i + 1, , 1] + pg[, i, , 1]
    }
  }
  catch.n(object) = stock.n(object) * f/(m + f) * (1 - exp(-f-m))
  landings.n(object)[is.na(landings.n(object))|landings.n(object)<0] = 0
  discards.n(object)[is.na(discards.n(object))|discards.n(object)<0] = 0
  
  flag=discards.n(object)>0
  
  if (any(flag)){
     discards.n(object)[flag] = (catch.n(object) * discards.n(object)/(discards.n(object)+landings.n(object)))[flag]
     landings.n(object)[flag] = (catch.n(object) - discards.n(object))[flag]}
  else
    landings.n(object)[flag] = catch.n(object)
                                
  units(catch.n(object)) = un
  units(landings.n(object)) = un
  units(discards.n(object)) = un
  units(catch.wt(object)) = uwt
  units(landings.wt(object)) = uwt
  units(discards.wt(object)) = uwt
  catch(object) = computeCatch(object, "all")
  object
}

  