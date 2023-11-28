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
  rebuildTime(object,rebuild(object))})
# }}}


rebuild<-function(object,f=0,n=51,nyear=101){
  ftar=FLQuant(seq(c(refpts(object)["msy","harvest"]),c(refpts(object)["crash","harvest"]),length.out=n),
               dimnames=list(year=1,iter=seq(n)))
  ftar=FLQuant(1,dimnames=list(year=seq(nyear),iter=seq(n)))%*%ftar
  fbar(object)=ftar
  
  stk=as(object,"FLStock")
  stk=fwd(stk,f=fbar(stk)[,-1],sr=object)
  stk=fwd(stk,f=fbar(object)[,-seq(round(nyear/2))]*f,sr=object)
  stk=window(stk,start=round(nyear/2)+1)
  stk=qapply(stk,function(x){
    dimnames(x)$year=an(ac(dimnames(x)$year))-(round(nyear/2)+1)
    x})
  stk}

rebuildTime<-function(object,stk=rebuild(object)){
  dt1=transmute(as.data.frame(ssb(stk),drop=T), 
                SSB    =data/c(refpts(object)["msy","ssb"]),
                Initial=c(ssb(stk[,1]))[an(ac(iter))]/c(refpts(object)["msy","ssb"]),
                Year   =year)
  
  t=suppressWarnings(as.data.frame(akima:::interp(dt1$Initial,dt1$SSB,dt1$Year,
                                                  duplicate="mean",
                                                  xo=seq(0,1,length.out=52)[-c(1,52)],yo=1)))
  data.frame(Initial=t$x,Year=t$z)}

rebuildInterp<-function(x,y,new){
  f=splinefun(x, y)
  f(new)} 
