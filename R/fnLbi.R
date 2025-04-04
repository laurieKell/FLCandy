
lfm<-function(lc,linf) 0.75*lc+0.25*linf

lc<-function(object){
  dat=subset(as.data.frame(object),data>0)
  dat=ddply(dat,.(len), with, cumsum(sum(data))/sum(data))
  dat$V1=cumsum(dat$V1)/sum(dat$V1)
  dat[(abs(dat$V1-0.5)==min(abs(dat$V1-0.5))),"len"][1]}

lmax<-function(x){
  rtn=reshape2::melt(aaply(x,2:6, function(x) rev(names(x[x>0&!is.na(x)]))[1]))
  names(rtn)[dim(rtn)[2]]="data"
  rtn$data=an(ac(rtn$data))
  as.FLQuant(rtn)}

boot<-function(x,nits){
  
  rtn=adply(propagate(x,nits),c(2:6), function(x){  
    reshape2::melt(table(sample(names(x), linewidth=sum(x), replace=TRUE, prob=x)),
                   value.name="data")})    
  rtn=as.FLQuant(rtn)
  names(dimnames(rtn))[1]=names(x)[1]
  rtn[is.na(rtn)]=0
  rtn}
