require(FLCore)
require(FLBRP)
require(FLasher)
require(FLife)

setGeneric("m1", function(object, ...)
  standardGeneric("m1"))
setGeneric("m2", function(object, ...)
  standardGeneric("m2"))
setGeneric("forage", function(object, ...)
  standardGeneric("forage"))
setGeneric("predNeed", function(object, ...)
  standardGeneric("predNeed"))

setMethod("m1", "FLComp", function(object, ...) {
  if(units(harvest(object)) != 'f') 
    stop("Exploitation not defined as 'f', cannot be combined with  'm'")
  if (!"m1"%in%names(attributes(object)))
    stop("m1 is not an attribute")
 
  return(attributes(object)$m1)})

setMethod("m2", "FLComp", function(object, ...) {
  if(units(harvest(object)) != 'f') 
    stop("Exploitation not defined as 'f', cannot be combined with  'm'")
  if (!"m1"%in%names(attributes(object)))
    stop("m1 is not an attribute")
  
  return(m(object) - m1(object))})

setMethod("forage", "FLComp", function(object, ...) {
  if(units(harvest(object)) != 'f') 
    stop("Exploitation not defined as 'f', cannot be combined with  'm'")
  if (!"m1"%in%names(attributes(object)))
    stop("m1 is not an attribute")

  zFn<-function(object) m(object)%+%harvest(object)
  
  return(FLCore:::apply((stock.wt(object)%*%stock.n(object)%*%(m2(object))%/%(zFn(object))%*%(1-exp(-zFn(object))))[-1],c(2,6),sum))})

setMethod("predNeed", "FLBRP", function(object, ...) {
  
  rfs =refpts(object)[,"harvest"]
  dmns=dimnames(rfs)
    
  res =aperm(rfs,c(2,1,3:length(dim(rfs))))
    
  dimnames(res)[1:2]=list(age="all","year"=seq(dim(res)[2]))
  names(dimnames(res))[2]="year"
    
  fbar(object)=FLQuant(res)
  frg=forage(object)
    
  rtn=rfs
  rtn[]=c(frg)
    
  dimnames(rtn)[[2]]="forage"
    
  rtn})

if (FALSE){
lh=FLPar(linf=25)
lh=lhPar(lh)
eq=lhEql(lh)

stk=as(eq,"FLStock")
stk=qapply(stk, function(object) {dimnames(object)$year=an(dimnames(object)$year)+1960; object})
stk=fwd(stk,f=fbar(stk)[,-1],sr=eq)

attributes(eq)$m1 =FLQuant(0.025,dimnames=dimnames(m(eq)))
attributes(stk)$m1=FLQuant(0.025,dimnames=dimnames(m(stk)))

plot(eq,refpts="msy")


ggplot(model.frame(FLQuants(eq,ssb=ssb,yield=catch,forage=forage),drop=T))+
  geom_line(aes(ssb,forage),col="red")+
  geom_line(aes(ssb,yield),col="blue")+
  xlab("SSB")+ylab("Production")

plot(stk, metrics=list(SSB=ssb,Forage=forage,Yield=catch))+
  xlab("Year")
}