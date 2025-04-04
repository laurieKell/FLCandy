stdz<-function(x){
  x=x-mean(x,na.rm=TRUE)
  x/var(x,na.rm=TRUE)^0.5}
