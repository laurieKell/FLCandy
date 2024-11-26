setMethod('globalMsy', signature(object='FLBRP'), function(object){
  globalMsyFn2(object)})


globalMsyFn<-function(object){
    
  res=qapply(object,function(x) apply(x,c(1:5),median))
  res=propagate(res, dim(m(res))[1])
  
  range(res)[c("minfbar","maxfbar")]=range(res)["max"]

  mat=array(1,c(dim(m(object))[1],dim(m(object))[1]))
  mat[upper.tri(mat)]=0
  
  discards.sel(res)[]=0
  
  landings.sel(res)=FLQuant(c(mat),dimnames=dimnames(landings.sel(res)))
  
  refpts(res)=refpts(res)["msy",]
  refpts(res)[,"harvest"]=1
  msy=computeRefpts(res)["msy","yield",drop=T]
  msy[is.na(msy)]=0
  
  msy=seq(dim(m(res))[1])[msy==max(msy)][1]
  
  FLPar(c(age=an(dimnames(m(res))$age[msy]),computeRefpts(res)[,,msy,drop=T]))}


globalMsyFn2<-function(object){
  
  range(object)[c("minfbar","maxfbar")]=range(object)["max"]
  
  mat=array(1,c(dim(m(object))[1],dim(m(object))[1]))
  mat[upper.tri(mat)]   =0
  discards.sel(object)[]=0
  
  lsel=FLQuant(c(mat),dimnames=dimnames(propagate(iter(landings.sel(object),1),dim(m(object))[1])))
  
  refpts(object)=refpts(object)["msy",]
  
  rtn=NULL
  for (i in seq(dim(m(object))[1])){
    
    landings.sel(object)=iter(lsel,i)
    msy=computeRefpts(object)["msy","yield",drop=T]
    msy[is.na(msy)]=0
  
    msy=seq(dim(m(object))[1])[msy==max(msy)][1]
  
    res=cbind(age=an(dimnames(m(object))$age[i]),t(computeRefpts(object)[drop=T]))
    res=cbind(iter=an(dimnames(object)[[6]]),res)
    
    rtn=rbind(rtn,res)}

  rtn=as.data.frame(rtn)

  order=do.call("order",as.data.frame(rtn[,c("iter","harvest","age")]))
  rtn=rtn[rev(order),]
  rtn=rtn[!duplicated(rtn[,"iter"]),]

  order=do.call("order",as.data.frame(rtn[,"iter"]))
  rtn=rtn[order,]
  
  as(as.data.frame(rtn[,-1]),"FLPar")}

