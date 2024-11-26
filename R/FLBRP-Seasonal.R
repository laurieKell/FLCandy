seasonalRefs<-function(om){
    
  stk=qapply(om, function(x) {  
    
    if (dim(x)[4]==1) return(x)
    if (dim(x)[1]==1) {
      x=x[,,,1]
      return(x)}
    
    dnms=dimnames(x)
    
    dnms[[4]]=1
    dnms[[1]]=(as.numeric(rep(dnms[[1]],each=dim(x)[4]))+seq(0,1,length.out=dim(x)[4]+1)[-(dim(x)[4]+1)])*dim(x)[4]
    
    FLQuant(c(aperm(x,c(4,1,2,3,5:6))),dimnames=dnms,units=units(x))})
  
  ## tidy up
  range(stk)["min"]=min(as.numeric(dimnames(m(stk))[[1]]))
  range(stk)["max"]=max(as.numeric(dimnames(m(stk))[[1]]))
  if (!is.na(range(stk)["plusgroup"]))
    range(stk)["plusgroup"]=range(stk)["max"]
  range(stk)[c("minfbar","maxfbar")]=range(stk)[c("minfbar","maxfbar")]*dim(m(om4))[4]
  range(stk)["minfbar"]=range(stk)["maxfbar"]-3
  m(stk)=m(stk)*1.5
  
  ## Calculate Reference points ##################################################
  eq4=FLBRP(stk[-1],sr=list(model=model(eq),params=params(eq)))  
  
  fbar(eq4)=FLQuant(seq(0,refpts(eq4)["crash","harvest",1]/refpts(eq4)["msy","harvest",1],length.out=101))%*%refpts(eq4)["msy","harvest",1]
  
  eq4}

