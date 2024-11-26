setMethod("invALK", signature(object="FLQuant", model="missing", age="missing"), 
          function(object, model, age=ages(object), cv=0.1, lmax=1.2, bin=1,
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
  
  return(alk)})


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

