varyMSY<-function(object,sr,nyear=2){
  
  rtn=mdply(dimnames(object)$year[-1], function(year){
    stk        =window(object,end=ac(year))
    res        =FLBRP(stk,nyear=nyear,model=model(sr),params=params(sr))
    refpts(res)=refpts(res)["msy"]
    
    refpts(res)[]=NA
    msy=computeRefpts(res)
    
    cbind("year"=as.numeric(year),model.frame(msy))})
  
  flqs=FLQuants(dlply(rtn, .(quant), with, as.FLQuant(data.frame(year=year,iter=iter,data=msy))))
  
  flqs} 

relMSY<-function(object,sr,nyear=2,eq=NULL){
  
  rtn=mdply(dimnames(object)$year[-1], function(year){
    stk        =FLCore:::window(object,end=an(year))
    res        =FLBRP(stk,nyear=nyear,model=model(sr),params=params(sr))
    refpts(res)=computeRefpts(res)
    
    if (is.null(eq))    
      msy=refpts(res)["msy"]
    else
      msy=propagate(refpts(eq)["msy"],dim(object)[6])
    
    refpts(res)=refpts(res)[1]
    dimnames(refpts(res))[[1]]="b"
    refpts(res)[]=NA
    refpts(res)["b","ssb"]=c(ssb(stk)[,ac(year)])
    rtn=computeRefpts(res)
    
    rtn=rtn%/%msy
    
    cbind("year"=as.numeric(year),model.frame(rtn[,1:5]))})
  
  flqs=FLQuants(dlply(rtn, .(quant), with, as.FLQuant(data.frame(year=year,iter=iter,data=b))))
  
  names(flqs)[1:5]=c("FFmsy","Ymsy","Rmsy","SSBBmsy","BBmsy")
  
  flqs} 

relVirgin<-function(object,sr,nyear=2,eq=NULL){
  
  rtn=mdply(dimnames(object)$year[-1], function(year){
    stk        =window(object,end=an(year))
    res        =FLBRP(stk,nyear=nyear,model=model(sr),params=params(sr))
    refpts(res)=computeRefpts(res)
    
    if (is.null(eq))    
      virgin=refpts(res)["virgin"]
    else
      virgin=propagate(refpts(eq)["virgin"],dim(object)[6])
    
    refpts(res)=refpts(res)[1]
    dimnames(refpts(res))[[1]]="b"
    refpts(res)[]=NA
    refpts(res)["b","ssb"]=c(ssb(stk)[,ac(year)])
    rtn=computeRefpts(res)
    
    rtn=rtn%/%virgin
    
    cbind("year"=as.numeric(year),model.frame(rtn[,3:5]))})
  
  flqs=FLQuants(dlply(rtn, .(quant), with, as.FLQuant(data.frame(year=year,iter=iter,data=b))))
  
  names(flqs)[1:3]=c("Rvirgin","SSBvirgin","Bvirgin")
  
  flqs} 

if (FALSE){
rtn=relMSY(window(rbst2[[1]],start=2001),eq)
plot(rtn)

rtn=relVirgin(window(rbst2[[1]],start=2001),eq)
plot(rtn)
}
