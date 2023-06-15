xoy<-function(object,x=2,y=3,yref="missing"){

  rfYrs=seq(dim(object)[2]-x-y+1)+x+y-1
  
  rtn=object[,rfYrs]
  for (i in rfYrs)
    if(missing(yref))
       rtn[,i-x-y+1]=yearMeans(object[,i-seq(x)+1])%/%yearMeans(object[,i-x-y+seq(y)])
    else
       rtn[,i-x-y+1]=yearMeans(object[,i-seq(x)+1])%/%yearMeans(object[,ac(yref)])
  
  rtn}

trnd<-function(object,n=5){
  
  m  =expand.grid(iter=dimnames(object)$iter,year=dimnames(object)$year[-seq(n-1)])

  rtn=mdply(m, function(iter,year){
    dat=as.data.frame(object[,rev(ac(an(ac(year))+1-seq(n))),,,,iter])
    data.frame(data=lm(data~year,data=dat)$coefficients[2])})
  
  as.FLQuant(rtn)}
