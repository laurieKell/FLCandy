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

rebuild<-function(object,n=51,nyear=50,burnin=20,fproj=0,bflag=TRUE){
  
  if(bflag){
    n1=(n/4)
    mlt=c(seq(1,0.25,length.out=n1)[-n1],seq(0.25,0,length.out=n-n1+1)[-(n-n1+1)])
    btar=FLQuant(
      mlt*c(refpts(object)["msy","ssb"]),
                 dimnames=list(year=1,iter=seq(n)))
    refpts(object)=propagate(refpts(object)[1,],n)
    dimnames(refpts(object))
    dimnames(refpts(object))[[1]]="b"
    
    refpts(object)[1,]=NA
    refpts(object)[1,"ssb"]=c(btar)
    ftar=computeRefpts(object)[,"harvest"]}
  else
  ftar=FLQuant(seq(c(refpts(object)["msy","harvest"]),c(refpts(object)["crash","harvest"]),length.out=n),
               dimnames=list(year=1,iter=seq(n)))
  
  ftar=FLQuant(1,dimnames=list(year=seq(nyear+burnin),iter=seq(n)))%*%ftar
  fbar(object)=ftar
  fbar(object)[,-seq(burnin)]=fproj

  stk=as(object,"FLStock")
  stk=fwd(stk,f=fbar(stk)[,-1],sr=object)
  stk=window(stk,start=burnin+1)
  stk=qapply(stk,function(x) {dimnames(x)$year=seq(length(dimnames(x)$year))
                              x})
  
  stk}

rebuildTime<-function(object,stk=rebuild(stk)){
  dt1=transmute(as.data.frame(ssb(stk),drop=T), 
                SSB    =data/c(refpts(object)["msy","ssb"]),
                Initial=c(ssb(stk[,1]))[an(ac(iter))]/c(refpts(object)["msy","ssb"]),
                Year   =year)
  
  t=suppressWarnings(as.data.frame(akima:::interp(dt1$Initial,dt1$SSB,dt1$Year,xo=seq(0,1,length.out=202)[-c(1,202)],yo=1,duplicate="mean")))
  data.frame(Initial=t$x,Year=t$z)}

rebuildInterp<-function(x,y,new){
  f=splinefun(x, y)
  f(new)} 
