amplitude<-function(object,ref){
  res=object%/%ref
  apply(window(res,start=2021),c(1,3:6),base:::min)}

responsiveness<-function(object){
  min =object%/%apply(object,c(1,3:6),base:::min)
  yr  =object
  yr[]=rep(as.numeric(dimnames(object)$year),each=dim(object)[1])
  
  FLQuant(yr[min==1],dimnames=dimnames(object)[-2])}

risk<-function(object,ref){
  res=object%/%ref
  mean(apply(res,c(1,3:6),base:::min)<1)}

recoverySpeed<-function(object,ref){
  yr  =object
  yr[]=rep(as.numeric(dimnames(object)$year),each=dim(object)[1])
  min=object%=%as.numeric(object%/%ref>1)
  
  dat=rbind(ddply(transform(subset(model.frame(FLQuants(yr=yr,min=min)),min==1),iter=as.numeric(iter)),
                  .(iter), with,
                  data.frame(data=year[min(year)==year])),
            data.frame(iter=as.numeric(dimnames(object)$iter),data=NA))
  as.FLQuant(dat[!duplicated(dat$iter),])
}
