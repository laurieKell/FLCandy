len2age<-function(x,par){
  age=as.data.frame(x)
  age=transform(age,age=invVonB(an(length),par))
  as.FLQuant(age[,-1])}