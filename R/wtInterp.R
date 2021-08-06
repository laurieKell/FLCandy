wtInterp<-function(wt){
  
  mlt=wt[,-dim(wt)[2]]
  mlt=FLQuant(rep(seq(0,(dim(mlt)[4]-1)/dim(mlt)[4],1/dim(mlt)[4]),each=max(cumprod(dim(mlt)[1:3]))),
              dimnames=dimnames(mlt))[-dim(mlt)[1]]
  
  incmt=wt[,,,1]
  incmt=-incmt[-dim(incmt)[1],-dim(incmt)[2]]+incmt[-1,-1]
  
  wt[dimnames(incmt)$age,dimnames(incmt)$year]=
    wt[dimnames(incmt)$age,dimnames(incmt)$year]+
    incmt%+%(mlt%*%incmt)
  
  wt[,-dim(wt)[2]]}
