
#' Inverse Age-Length Key
#' 
#' @description
#' Creates an inverse Age-Length Key from length-at-age data using normal distributions
#'
#' @param object An FLQuant object containing length-at-age data
#' @param model Not used, maintained for method consistency
#' @param age Vector of ages (default: ages from object)
#' @param cv Coefficient of variation for length distribution (default: 0.1)
#' @param lmax Multiplier for maximum length (default: 1.2)
#' @param bin Length bin size (default: 1)
#' @param max Maximum length (default: NULL)
#' @param reflen Reference length for SD calculation (default: NULL)
#'
#' @return
#' An FLPar object containing the probability matrix of age given length
#'
#' @importFrom stats pnorm dnorm
#' @importFrom plyr laply
#'
#' @export
setMethod("invALK", 
          signature(object="FLQuant", model="missing", age="missing"),
          function(object, model, age=ages(object), cv=0.1, lmax=1.2, bin=1,
                   max=NULL, reflen=NULL) {
            
            if (!is.FLQuant(object)) 
              stop("Input must be an FLQuant object")
            
            if (cv <= 0) 
              stop("CV must be positive")
            
            if (is.null(max))
              max = ceiling(max(object)*lmax)
            
            bins <- seq(0, max, bin)
            
            sd <- if(is.null(reflen)) abs(object * cv) else reflen * cv
            
            probs <- Map(function(x, y) {
              p <- c(pnorm(1, x, y),
                     dnorm(bins[-c(1, length(bins))], x, y),
                     pnorm(bins[length(bins)], x, y, lower.tail=FALSE))
              return(p / sum(p))
            }, x=object, y=sd)
            
            dmns <- c(dimnames(object)[1], 
                      dimnames(object)[-1], 
                      list(len=bins))
            
            res <- do.call(rbind, probs)
            alk <- aperm(array(unlist(c(res)), 
                               dim=laply(dmns, length), 
                               dimnames=dmns),
                         c(1,7,2:6))
            
            alk <- FLPar(alk, units=units(object))
            
            return(alk)
          })

#setMethod("invALK", signature(object="FLQuant", model="missing", age="missing"), 
iALK2<-function(object, model, age=ages(object), cv=0.1, lmax=1.2, bin=1,
                max=NULL, reflen=NULL) {
  
  if (is.null(max))
    max=ceiling(max(object)*lmax)
  
  bins <- seq(0, max, bin)
  
  if(is.null(reflen)) {
    sd=abs(object * cv)
  } else {
    sd=reflen * cv
  }
  
  probs=Map(function(x, y) {
    p <- c(pnorm(1, x, y),
           dnorm(bins[-c(1, length(bins))], x, y),
           pnorm(bins[length(bins)], x, y, lower.tail=FALSE))
    return(p / sum(p))
  }, x=object, y=sd)
  
  dmns=c(dimnames(object)[1], dimnames(object)[-1], list(len=bins))
  res =do.call(rbind, probs)
  alk =aperm(array(unlist(c(res)), dim=laply(dmns, length), dimnames=dmns),c(1,7,2:6))
  alk =FLPar(alk,units=units(object))
  
  return(alk)}

lenSamp2<-function(object,invALK,n=300){
    rtn=FLQuant(0,dimnames=c(list(len=dimnames(invALK)$len),dimnames(object)[-1]))
    for (i in dimnames(object)$year)
      rtn[,i]=lenSamples(object[,i],FLPar(invALK@.Data[,,i,1,1,1,1]),n)
    rtn}

#' params=lhPar(FLPar(linf=100))
#' eql   =lhEql(params)
#' stk   =as(eql,"FLStock")
#' stk   =fwd(stk,f=fbar(stk)[,-1],sr=eql)

#' alkV2=invALK(wt2len(stock.wt(ple4),params))

