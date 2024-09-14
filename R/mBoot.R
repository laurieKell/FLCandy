n     =100
object=m(ple4)
devs  =mDev 
mBoot<-function(n,object,devs){
  
  object=propagate(object,n)
  
  rtn=mdply(data.frame(year=dimnames(object)$year), function(year){
    object[,year]%*%devs[,,,,,sample(dimnames(devs)$iter,n,TRUE)]})
  
  rtn}

dev=rBoot(100,m(ple4),mDev)

