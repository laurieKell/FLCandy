################################################################################
## SSB: Spawning Stock Biomass                                                ##
################################################################################
setGeneric("ssb", function(object, ...) standardGeneric("ssb"))
#' @title ssb
#' 
#' @description  Calculates the spawning stock biomass for an FLStock object,
#' 
#' @rdname ssb
#' @aliases 
#' @seealso \code{\link{ssb.age}}
#'
#' @param object An FLStock object containing stock assessment data.
#' @param ... additional arguments, to replace slots in the FLStock object.
#' @return Returns the spawning stock biomass as an FLQuant.
#' @export
#' @examples
#' data(ple4)
#' ssb(ple4)
setMethod("ssb", signature(object="FLStock"), function(object, ...) {
  
  for (i in names(list(...)))
    slot(object,i)=list(...)[[i]]
  
  dis <- dims(object)
  
  # DEAL with age 0 in SSB
  if(dis$min == 0) {
    # DROP in yearly or single spawning season
    if(dis$season == 1 | dis$season > dis$unit)
      object <- object[-1,]
    # SET to 0 same unit & season, and later
    else if(dis$season  == dis$unit)
      for(i in seq(dis$unit))
        mat(object)["0", , i, i] <- 0
  }
  
  # CALCULATE by units
  uns <- units(harvest(object))
  
  if(uns == 'f') {
    return(quantSums(stock.n(object) * exp(-(harvest(object) *
       harvest.spwn(object) + m(object) * m.spwn(object))) *
   stock.wt(object) * mat(object)))
    
  } else if(uns == 'hr') {
    return(quantSums(stock.n(object) * stock.wt(object) * mat(object) *
   (1 - harvest(object) * harvest.spwn(object)) *
   exp(-m(object) * m.spwn(object))))
    
  } else {
    stop("Correct units (f or hr) not specified in the harvest slot")
  }
  })


################################################################################
##  ssb.age                                                                   ##
################################################################################
setGeneric("ssb.age", function(object, ...) standardGeneric("ssb.age"))
#' @title ssb.age
#' 
#' @description Calculates the spawning stock biomass by age for an FLStock object.
#' 
#' 
#' @rdname ssb.age
#' @aliases 
#' 
#' @seealso \code{\link{ssb}}
#'
#' @param object An FLStock object containing stock assessment data.
#' @param ... additional arguments, to replace slots in the FLStock object.
#' @return Returns the spawning stock biomass by age as an FLQuant.
#' @export
#' @examples
#' data(ple4)
#' ssb.age(ple4)
setMethod("ssb.age", signature(object="FLStock"),
function(object, ...) {
  
  for (i in names(list(...)))
    slot(object,i)=list(...)[[i]]
  
  # CALCULATE by units
  uns <- units(harvest(object))
  
  if(uns == 'f') {
    return(stock.n(object) * exp(-(harvest(object) *
       harvest.spwn(object) + m(object) * m.spwn(object))) *
   stock.wt(object) * mat(object))
    
  } else if(uns == 'hr') {
    return(stock.n(object) * stock.wt(object) * mat(object) *
   (1 - harvest(object) * harvest.spwn(object)) *
   exp(-m(object) * m.spwn(object)))
    
  } else {
    return(rec(object) %=% as.numeric(NA))
  }
  })


################################################################################
## spawnOnce                                                                  ##
################################################################################
setGeneric("spawnOnce", function(object, ...) standardGeneric("spawnOnce"))
#' @title spawnOnce
#' 
#' @description  Shifts maturity-at-age ogive by 1 age to estimate the propotion 
#' of individuals who have spawned at least once
#' 
#' @rdname spawnOnce
#' @aliases 
#' 
#' @seealso \code{\link{ssb}}, \code{\link{ssb.age}}, 
#' \code{\link{pos}}, \code{\link{spawnOnce}}, 
#' \code{\link{asa}}, \code{\link{awa}},   
#' \code{\link{ssb}}, \code{\link{amat}}, \code{\link{wmat}}, 
#' \code{\link{fjuv}}, \code{\link{fapex}}
#'
#' @param object An FLStock object.
#' @return object An FLQuant object with an ogive with the proportion of older spawners.
#' @export
#' @examples
#' data(ple4)
#' spawnOnce(ple4)
setMethod("spawnOnce", signature(object="FLQuant"), 
          function(object){
            rtn      =object
            rtn[1]   =0
            rtn[-1][]=object[-dim(object)[1]]
            rtn})
setMethod("spawnOnce", signature(object="FLStock"),
          function(object){
            amat(mat(object))})

################################################################################
## pos: Proportion of Old Spawners by Biomass                                 ##
################################################################################
setGeneric("pos", function(object, ...) standardGeneric("pos"))
#' @title  POS: proportion of the SSB made up of older individuals. 
#' 
#' @description Older spawners are important for the genetic diversity 
#' and resilience of fish populations, as they tend to produce more, and potentially 
#' higher quality, eggs. To calculate POS, first identify the older age classes,  
#' sum their biomass, then divide by the total spawning stock biomass (SSB).
#' The older age classes can be identified from the maturity ogive `mat`, for example
#' using the `spawnOnce` method. This shifts the maturity by 1 age soi that it idenifies
#' the proportion of individuals that have spawned before
#' 
#' @rdname pos
#'
#' @param object An FLStock object.
#' @param object An FLQuant object with an ogive with the proportion of older spawners,
#' by default calculated by soawnOnce
#' 
#' @return Returns the proportion of old spawners by biomass.
#' 
#' @export
#' @examples
#' \dontrun{
#' data(ple4)
#' pos(ple4)}
setMethod("pos", signature(object="FLStock"), 
          function(object,ogive=spawnOnce(mat(object))) 
              ssb(object,mat=ogive)%/%ssb(object))

################################################################################
## asa: the Average Age of Spawners                                           ##
################################################################################
setGeneric("asa", function(object, ...) standardGeneric("asa"))
#' @title ASA: the Average Age of Spawners
#' 
#' @description The Average Age of Spawners is the mean age of reproductive individuals. 
#' The average age of spawners can indicate the age structure of a fish population 
#' and its potential reproductive capacity. A decrease in the ASA could suggest 
#' overfishing or other stressors affecting older age classes. 
#' 
#' @rdname asa
#' 
#' @aliases
#' @seealso \code{\link{ssb}}, \code{\link{ssb.age}}, 
#' \code{\link{pos}}, \code{\link{spawnOnce}}, 
#' \code{\link{asa}}, \code{\link{awa}},   
#' \code{\link{ssb}}, \code{\link{amat}}, \code{\link{wmat}}, 
#' \code{\link{fjuv}}, \code{\link{fapex}}
#' 
#' @param object An FLStock object.
#' @param object An FLQuant object with an ogive with the proportion of older spawners,
#' by default calculated by soawnOnce
#' @return Returns the proportion of old spawners by biomass.
#' @export
#' @examples
#' \dontrun{
#' data(ple4)
#' asa(ple4)}
setMethod("asa", signature(object="FLStock"),
          function(object){
            quantSums(ages(mat(object))%*%ssb.age(object))%/%ssb(object)})

################################################################################
  ## amat: age at a specific maturity                                           ##
################################################################################
setGeneric("amat", function(object, ...) standardGeneric("amat"))
#' @title amat: age at a specific maturity
#' 
#' @description based on interpolation between values or a specuific value
#' 
#' @rdname amat
#' 
#' @aliases 
#' 
#' @seealso \code{\link{ssb}}, \code{\link{ssb.age}}, 
#' \code{\link{pos}}, \code{\link{spawnOnce}}, 
#' \code{\link{asa}}, \code{\link{awa}},   
#' \code{\link{ssb}}, \code{\link{amat}}, \code{\link{wmat}}, 
#' \code{\link{fjuv}}, \code{\link{fapex}}
#' 
#' 
#' @param object An FLStock object.
#' @param object An FLQuant object with an ogive with the proportion of older spawners,
#' by default calculated by soawnOnce
#' @return Returns the proportion of old spawners by biomass.
#' @export
#' @examples
#' \dontrun{
#' data(ple4)
#' amat(ple4)}
setMethod("amat", signature(object="FLQuant"), 
          function(object,value=1,what=c("i","g")[1]){
            
      amatFn<-function(mat,value=1){
        age=an(ac(dimnames(mat)[[1]]))
        apply(mat, 2:6, function(x) approx(x,age,xout=value,ties=min)$y)}

      ## greater than
      amatFn2<-function(mat,value=1){
        a  =ages(mat)
        b  =mat>=value
        apply(a%*%b, 2:6, function(x) min(x[x>0],na.rm=T))}
      
      switch(what[1],
             "i"=amatFn( object,value),
             "g"=amatFn2(object,value))})
setMethod("amat", signature(object="FLStock"),
          function(object,value=1,what=c("i","g")[1]){
            amat(mat(object),value,what)})
          
################################################################################
## wmat                                                                       ##
################################################################################
setGeneric("wmat", function(object, ...) standardGeneric("wmat"))
#' @title wmat
#' 
#' @description Wt-at-maturity via interpolation between values
#' 
#' @rdname wmat 
#' 
#' @aliases 
#' @seealso \code{\link{ssb}}, \code{\link{ssb.age}}, 
#' \code{\link{pos}}, \code{\link{spawnOnce}}, 
#' \code{\link{asa}}, \code{\link{awa}},   
#' \code{\link{ssb}}, \code{\link{amat}}, \code{\link{wmat}}, 
#' \code{\link{fjuv}}, \code{\link{fapex}}
#' 
#'
#' @param object An FLStock object.
#' @param object An FLQuant object with an ogive with the proportion of older spawners,
#' by default calculated by soawnOnce
#' @return Returns the proportion of old spawners by biomass.
#' @export
#' @examples
#' \dontrun{
#' data(ple4)
#' pos(ple4)}
setMethod("wmat", signature(object="FLStock"),
          function(object,value=0.5){
            res=cbind(model.frame(FLQuants(object, wt=FLCore:::stock.wt,mt=FLCore:::mat)),value=value)
            
            res=ddply(res, .(year,unit,season,area,iter), with, 
                      data.frame(data=approx(mt,wt,xout=value[1],ties=min)$y))
            
            as.FLQuant(res)})

################################################################################
## fjuv                                                                       ##
################################################################################
setGeneric("fjuv", function(object, ...) standardGeneric("fjuv"))
#' @title Fjuv
#' 
#' @description  The ratio of juvenile to apical fishing mortality and compares 
#' the fishing mortality rates of juvenile fish to those at the peak of the fishing mortality 
#' curve (fapex). It's used to assess the impact of fishing on different life 
#' stages of fish and to inform management strategies that protect juvenile 
#' fish to ensure the sustainability of fish stocks. 
#' The ratio of juvenile to apical fishing mortality (Fjuv/Fapical)
#' This ratio involves calculating fishing mortality rates for juvenile and 
#' apical age classes separately and then dividing them. 
#'
#' @rdname fjuv
#' 
#' @aliases 
#' @seealso \code{\link{ssb}}, \code{\link{ssb.age}}, 
#' \code{\link{pos}}, \code{\link{spawnOnce}}, 
#' \code{\link{asa}}, \code{\link{awa}},   
#' \code{\link{ssb}}, \code{\link{amat}}, \code{\link{wmat}}, 
#' \code{\link{fjuv}}, \code{\link{fapex}}
#' 
#' @examples
#' \dontrun{
#' data(ple4)
#' fjuv(ple4)}
setMethod("fjuv", signature(object="FLStock"), 
          function(object,value=0.5){
  
  age=amat(mat(object),0.5)-1
  age=floor(age)
  
  wts=FLQuant(rep(c(age),each=dim(mat(object))[1]), dimnames=dimnames(mat(object)))
  wts=FLQuant(wts>=ages(mat(object)))
  
  quantSums(harvest(object)%*%wts)%/%quantSums(wts)})

################################################################################
## awa                                                                        ##
################################################################################
setGeneric("awa", function(object, ...) standardGeneric("awa"))
#' @title awa Average Weight at Age anomaly (AWA)
#'
#' @description the average Weight at age anomaly are the deviations in the expected weight 
#' of fish at a certain age, which can be influenced by environmental conditions, 
#' food availability, and fishing pressure. Changes in average weight at age can 
#' affect the productivity and sustainability of fish stocks. This concept is 
#' important for understanding growth patterns and population dynamics
#' To calculate ASW anomalies, you first calculate the average weight at
#' age for your stock and then identify deviations from this average over time.
#'  
#' @rdname awa
#' 
#' @aliases 
#' @seealso \code{\link{ssb}}, \code{\link{ssb.age}}, 
#' \code{\link{pos}}, \code{\link{spawnOnce}}, 
#' \code{\link{asa}}, \code{\link{awa}},   
#' \code{\link{ssb}}, \code{\link{amat}}, \code{\link{wmat}}, 
#' \code{\link{fjuv}}, \code{\link{fapex}}
#' 
#' @examples
#' \dontrun{
#' data(ple4)
#' fjuv(ple4)}
setMethod("awa", signature(object="FLQuant"), 
          function(object){
            object%*%quantMeans(object)})
setMethod("awa", signature(object="FLStock"), 
          function(object){
            stock.wt(object)%*%quantMeans(stock.wt(object))})

#' @example 
#' \dontrun{
#' library(FLCore)
#' 
#' data(ple4)
#' 
#' awa(ple4)
#' 
#' 
if (FALSE){
plot(mcf(FLQuants(
          SSB   =ssb(ple4),
          F     =fbar(ple4),
          FRatio=fjuv(ple4)%/%fapex(ple4),
          amat  =amat(mat(ple4),0.5,what="i"),
          wmat  =wmat(ple4),
          POS   =pos(ple4),
          SPR   =ssb(ple4)/rec(ple4),
          SPR0  =spr0Yr(ple4),
          ASA   =asa(ple4))))
}
#'          
# load("C:/active/FLCandy/_data/bet.RData")
# bet=bet[[1]]
#'        
#' }