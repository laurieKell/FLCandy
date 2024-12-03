#' @title Age based indicators
#' 

#' @examples 
#' \dontrun{
#' library(FLCore)
#' 
#' data(ple4)
#' 
#' awa(ple4)
#' }
#' 
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


setMethod("spawnOnce", signature(object="FLQuant"), 
          function(object){
            rtn      =object
            rtn[1]   =0
            rtn[-1][]=object[-dim(object)[1]]
            rtn})
setMethod("spawnOnce", signature(object="FLStock"),
          function(object){
            amat(mat(object))})

setMethod("pos", signature(object="FLStock"), 
          function(object,ogive=spawnOnce(mat(object))) 
            ssb(object,mat=ogive)%/%ssb(object))

setMethod("asa", signature(object="FLStock"),
          function(object){
            quantSums(ages(mat(object))%*%ssb.age(object))%/%ssb(object)})

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

setMethod("wmat", signature(object="FLStock"),
          function(object,value=0.5){
            res=cbind(model.frame(FLQuants(object, wt=FLCore:::stock.wt,mt=FLCore:::mat)),value=value)
            
            res=ddply(res, .(year,unit,season,area,iter), with, 
                      data.frame(data=approx(mt,wt,xout=value[1],ties=min)$y))
            
            as.FLQuant(res)})

setMethod("fjuv", signature(object="FLStock"), 
          function(object,value=0.5){
            
            age=amat(mat(object),0.5)-1
            age=floor(age)
            
            wts=FLQuant(rep(c(age),each=dim(mat(object))[1]), dimnames=dimnames(mat(object)))
            wts=FLQuant(wts>=ages(mat(object)))
            
            quantSums(harvest(object)%*%wts)%/%quantSums(wts)})

setMethod("awa", signature(object="FLQuant"), 
          function(object){
            object%*%quantMeans(object)})
setMethod("awa", signature(object="FLStock"), 
          function(object){
            stock.wt(object)%*%quantMeans(stock.wt(object))})

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