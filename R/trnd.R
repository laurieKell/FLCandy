trnd<-function(x,y){
  
  res=loess(y~x, data.frame(x=x,y=y), control=loess.control(surface="direct"),span=1)
  rtn=predict(res,newdata=data.frame(x=max(x)+0:1))
  (rtn[2]-rtn[1])/mean(rtn[2]+rtn[1])}
  
trnd(x=seq(1:10),y=seq(1:10)*rlnorm(10))