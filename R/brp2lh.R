require(FLBRP)
require(FLife)

brp2lh<-function(object,params=lhPar(linf=30)){
  
  ## lengths ###################################################################
  sln=(stock.wt(   object)%/%params["a"])^(1/params["b"])
  cln=(catch.wt(   object)%/%params["a"])^(1/params["b"])
  lln=(landings.wt(object)%/%params["a"])^(1/params["b"])
  dln=(discards.wt( object)%/%params["a"])^(1/params["b"])

  
  params=rbind(params,FLPar(c(cage=0,lage=0)))

  fn=function(par,sln,cln,lln){
    
    names(par)=c("linf","k","t0","cage","lage")
    par=FLPar(par)
    
    sum((sln-vonB(ages(sln),par))^2+
        (cln-vonB(ages(cln)+par["cage"],par))^2+
        (lln-vonB(ages(lln)+par["lage"],par))^2)
    }
  rtn=optim(c(params[c("linf","k","t0","cage","lage")]),fn,sln=sln,cln=cln,lln=lln) 

  params[c("linf","k","t0","cage","lage")]=rtn$par


  ## maturity ##################################################################
  fn=function(par,mat){
    
    names(par)=c("a50","ato95","asym")
    par=FLPar(par)
    
    sum((mat%-%logistic(mat,par))^2)
    }    
  rtn=optim(c(params[c("a50","ato95","asym")]),fn,mat=mat(object)) 
  
  #vonB(params=params,length=wt2len(params["a50"],params))

  
  params[c("a50","ato95","asym")]=rtn$par

  ## selectivity################################################################
  fn=function(par,sel){
    
    names(par)=c("sel1","sel2","sel3")
    par=FLPar(par)
    
    sum((sel%-%dnormal(sel,par))^2)
  }    
  rtn=optim(c(params[c("sel1","sel2","sel3")]),fn,sel=catch.sel(object)) 
  
  params[c("sel1","sel2","sel3")]=rtn$par
  
  params}

setBmsy<-function(new,par,params){
  
  fn<-function(par,new,params,nms){
    params[nms]=par
    abs(new-computeRefpts(lhEql(params))["msy","ssb"])}

  nms=dimnames(par)$params
  
  #fn(par,new,params,nms)
    
  rtn=optim(c(par),fn,new=new,params=params,nms=nms,method="Brent",
            lower=c(par)*0.5,upper=c(par)*2)
  
  params[nms]=rtn$par
  
  params}
  

if (FALSE){
    
  load("/home/laurie/Desktop/projects/ddmse/data/om/icesMac.RData")
  
  params=brp2lh(mac)
  obj=lhEql(params)
  plot(obj,refpts="msy")
  
  par=setBmsy(50,params["k"],params)
  obj2=lhEql(par)
  plot(obj2,refpts="msy")
  
  par=setBmsy(75,params["m1"],params)
  obj3=lhEql(par)
  plot(obj3,refpts="msy")
  }

