setGeneric("Brebuild", function(object, ...)
  standardGeneric("Brebuild"))

# Brebuild {{{

#' @rdname Brebuild
#' @description Calculates the SSB that would take a given time to rebuild to Bmsy from 
#' @examples
#' data(ple4brp)
#' globalMsy(ple4brp)
#' 

setMethod('Brebuild', signature(object='FLBRP'), function(object){
  rebuildFn(object)})

# }}}

rebuild<-function(object,
                  targetB=refpts(object)["msy","ssb"],
                  targetF=refpts(object)["msy","harvest"]*0,
                  n=50,burnin=20,truncate=TRUE){

  eql=object
  targetB=c(targetB)*c(1e-10,seq(n)/n)
  targetF=c(targetF)
  rfs        =refpts(eql)
  refpts(eql)=FLPar(NA,dimnames=list(refpt="ssb",quant=dimnames(rfs)[[2]],iter=seq(n+1)))
  refpts(eql)["ssb","ssb"][]=targetB
  
  refpts(eql)=computeRefpts(eql)
  
  fbar(eql)  =propagate(fbar(eql)%=%1,n+1)%*%refpts(eql)["ssb","harvest"]
  stk        =as(brp(eql),"FLStock")
  flag       =aaply(stock.n(stk),6, function(x) !any(is.na(x)))
  flag       =seq(length(flag))[flag]
  stk        =fwd(iter(stk,flag),f=fbar(eql)[,-(1:2),,,,flag],sr=eql)
  
  fbar(eql)[,-seq(burnin)][]=targetF
  
  stk=fwd(stk,f=fbar(eql)[,-1,,,,flag],sr=eql)
  
  if (truncate) stk=stk[,-seq(burnin)]
  
  stk=qapply(stk,function(x) {dimnames(x)$year=seq(length(dimnames(x)$year))
                              x})
  stk}

rebuildDep<-function(object,
                     targetB,
                     targetF=0,
                     n=50,burnin=30,truncate=TRUE){
  
  eql=object
  fbar(eql)[]=0.2

  targetB=c(targetB)*c(1e-10,seq(n)/n)
  targetF=c(targetF)

  btar  =FLQuant(rep(targetB,each=dim(fbar(eql))[2]),dimnames=dimnames(propagate(ssb(eql),n+1)))
  stk   =propagate(as(eql,"FLStock"),n+1)
  stk   =fwd(stk,ssb_end=btar[,-c(1:2)],sr=eql)
  ftar  =fbar(stk)%=%targetF

  stk   =fwd(stk,f=ftar[,-seq(burnin)],sr=eql)
  
  if (truncate) stk=stk[,-seq(burnin)]
  
  stk=qapply(stk,function(x) {dimnames(x)$year=seq(length(dimnames(x)$year))
  x})
  stk}

rebuildTime<-function(stk){

  msy=c(ssb(iter(stk,dim(stk)[6]))[,1])
  dt1=transmute(as.data.frame(ssb(stk),drop=T),

  SSB    =data/msy,
  Initial=c(ssb(stk[,1]))[an(ac(iter))]/msy,
  Year   =year)
  t=suppressWarnings(as.data.frame(akima:::interp(dt1$Initial,dt1$SSB,dt1$Year,
                                                  xo=seq(0,1,length.out=202)[-c(1,202)],yo=1,duplicate="mean")))
  
  data.frame(Initial=t$x,Year=t$z)}

rebuildInterp<-function(x,y,new) splinefun(x, y)(new)