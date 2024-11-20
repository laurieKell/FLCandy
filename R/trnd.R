trnd<-function(x,y){
  
  res=loess(y~x, data.frame(x=x,y=y), control=loess.control(surface="direct"),span=1)
  rtn=predict(res,newdata=data.frame(x=max(x)+0:1))
  (rtn[2]-rtn[1])/mean(rtn[2]+rtn[1])}
  
trnd<-function(object,n=5){
  
  m  =expand.grid(iter=dimnames(object)$iter,year=dimnames(object)$year[-seq(n-1)])
  
  rtn=mdply(m, function(iter,year){
    dat=as.data.frame(object[,rev(ac(an(ac(year))+1-seq(n))),,,,iter])
    data.frame(data=lm(data~year,data=dat)$coefficients[2])})
  
  as.FLQuant(rtn)}

trnd<-function(data,year=seq(length(data)),nyrs=5) {
  
  # Ensure data is sorted by year
  data=data[order(year)]
  
  # Initialise trend vector
  trends=rep(NA, length(data))
  
  # Calculate trends
  for(i in nyrs:length(data)) {
    # Fit linear model
    model=lm(data[(i-nyrs+1):i]~year[(i-nyrs+1):i])
    
    # Store slope coefficient
    trends[i] <- coef(model)[2]}
  
  return(trends)}

xoy<-function(object,x=2,y=3,yref="missing"){

  rfYrs=seq(dim(object)[2]-x-y+1)+x+y-1
  
  rtn=object[,rfYrs]
  for (i in rfYrs)
    if(missing(yref))
      rtn[,i-x-y+1]=yearMeans(object[,i-seq(x)+1])%/%yearMeans(object[,i-x-y+seq(y)])
  else
    rtn[,i-x-y+1]=yearMeans(object[,i-seq(x)+1])%/%yearMeans(object[,ac(yref)])
  
  rtn}

if(FALSE){
  trnd(x=seq(1:10),y=seq(1:10)*rlnorm(10))
  
  
  state=xoy(ssb(om)[,ac(21:40)])
  trnd =xoy(idx[,ac(21:40)])
  
  idxSkill=FLCore::roc(state>1,trnd)
  
  state=trnd(ssb(om)[,ac(21:40)]/mean(ssb(om)[,ac(21:40)]))
  trnd =trnd(idx[,ac(21:40)]/mean(idx[,ac(21:40)]))
  
  idxSkill=FLCore::roc(state>0,trnd) 
  }