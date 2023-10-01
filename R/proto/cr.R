require(FSA)
require(FLCore)
require(ggplotFL)
require(plyr)
require(dplyr)
require(ggpubr)
require(FLSRTMB)

source("~/pCloudDrive/flr/doc/R/candidates/roc.R")

load("~/pCloudDrive/papers/inPrep/lengthMethods/jabbaz/data/ices.stks.n78.rdata")

## Generation time & r
gr=llply(stks, function(x) FLSRTMB::productivity(x, s=c(x@fishlife["s"])))


## Get F/FMSY
zHat=llply(stks, function(object) {
  
  cn=catch.n(object)[ac(seq(range(object)["minfbar"],range(object)["plusgroup"]-1))]
  cc=try(as.FLQuant(transmute(ddply(as.data.frame(cn), .(year,iter), with,
                                    chapmanRobson(age,data)$est["Z",]),data=Estimate,year=year)))
  
  load(file.path("/home/laurie/pCloudDrive/papers/inPrep/lengthMethods/jabbaz/jabbaz",paste(name(object),".perfectZ.rdata",sep="")))
  jabba=FLQuant(c(fit$timeseries[,"mu","FFmsy"]),dimnames=dimnames(fbar(object)))
  
  load(file.path("/home/laurie/pCloudDrive/papers/inPrep/lengthMethods/jabbaz/jabbaz",paste(name(object),".zHat.rdata",sep="")))
  jabbaz=FLQuant(c(fit$timeseries[,"mu","FFmsy"]),dimnames=dimnames(fbar(object)))
  
  load(file.path("/home/laurie/pCloudDrive/papers/inPrep/lengthMethods/jabbaz/jabbaz",paste(name(object),"Zhat2010.rdata",sep="")))
  j2010=FLQuant(c(fit$timeseries[,"mu","FFmsy"]),dimnames=dimnames(fbar(object)))
  
  mcf(FLQuants("ICES"                   =fbar(object)%/%FLPar(object@benchmark["Fmsy"]),
               "Catch Curve Z"          =cc,
               "Catch Curve F/M"        =(cc-object@fishlife["m"])%/%FLPar(object@fishlife["m"]),
               "JABBA \nperfect Z index"=jabba,
               "JABBA CC index"         =jabbaz,
               "JABBA \n2010 CC index"  =j2010))
})

dat=ldply(zDat, function(x) model.frame(x, drop=T))[,1:6]
dat=subset(dat,!is.na(obs)&!is.na(hat))

## By stock, i.e. trends
dt1=ddply(dat,.(.id), with, roc(ICES,`Catch Curve`))
ggplot(dt1)+
  geom_line(aes(FPR,TPR,group=.id))+
  geom_abline(aes(intercept=0,slope=1))

auc=ddply(dt1,.(.id), with, {
  rtn=try(pROC:::auc(label,indicator))
  
  if ("try-error"%in%is(rtn)) return(NULL)
  
  data.frame("AUC"=rtn)})
gghistogram(auc,"AUC")+
  geom_vline(aes(xintercept=0.5),col="red")

## lags
lg=ldply(zDat, function(x) {
   mdply(data.frame(lag=0:6), function(lag){
   dmns=dimnames(x[["ICES"]])
   dmns$year=as.numeric(dmns$year)-lag
   rtn=mcf(FLQuants("indicator"=FLQuant(c(x[["Catch Curve"]]),dimnames=dmns),
                    "state"    =x[["ICES"]]))
     
   model.frame(rtn,drop=T)})})[,seq(5)]
lg=subset(lg,!is.na(state)&!is.na(indicator))
lg1=ddply(lg,.(.id,lag), with, roc(`state`,`indicator`))

ggplot(lg1)+
  geom_line(aes(FPR,TPR,group=.id))+
  geom_abline(aes(intercept=0,slope=1))+
  facet_wrap(~lag)

dt=subset(lg1,lag==2&.id==unique(lg1$.id)[2])

ggplot(dt)+
  geom_line(aes(TPR,FPR,col=ac(lag)))+
  geom_abline(aes(slope=1,intercept=0))
1-with(subset(dt,lag==1),FLCore:::auc(TPR,FPR))
with(subset(dt,lag==1),pROC:::auc(state>1,indicator))

auc=ddply(lg1, .(.id,lag), with, {
  rtn=try(pROC:::auc(label,indicator))
  rtn=try(FLCore:::auc(TPR,FPR))
  
  if ("try-error"%in%is(rtn)) return(NULL)
  
  data.frame("AUC"=rtn)})
gghistogram(auc,"AUC")+
  geom_vline(aes(xintercept=0.5),col="red")+
  facet_grid(lag~.)

best=ddply(auc, .(.id), with, data.frame(lag=lag[AUC==max(AUC)]))
best=ddply(subset(lg1,!is.na(TSS)), .(.id), with, data.frame(lag=lag[TSS==max(TSS)][1]))

lg2=subset(merge(best,lg1,by=c(".id","lag")))
#lg2=ddply(lg2,.(.id), with, data.frame(order(FPR,decreasing=T),FPR,lag=lag[1]))
lg2=lg2[do.call("order",lg2[,c(".id","TPR")]),]

ggplot(lg1)+
  geom_line(aes(FPR,TPR,group=paste(.id,lag)),col="grey70")+
  geom_abline(aes(intercept=0,slope=1))+
  facet_wrap(~lag)+
  geom_line(aes(FPR,TPR,group=paste(.id,lag)),data=lg2,col="red")



## Final F/FMSY
dt2=ddply(dat, .(.id), with, data.frame(lag=rev(0:5),
                                        obs=obs[year%in%(max(year)-(0:5))],
                                        hat=rep(hat[year==max(year)],6)))
dt3=ddply(dt2,.(lag), with, roc(obs,hat))

ggplot(dt3)+
  geom_line(aes(FPR,TPR,col=as.character(lag)))+
  geom_abline(aes(intercept=0,slope=1))

ddply(dt3,.(lag), with, data.frame("AUC"=pROC:::auc(label,indicator)))



## Get F/FMSY
zRes=llply(stks, function(object) {
  
  cn=catch.n(object)[ac(seq(range(object)["minfbar"],range(object)["plusgroup"]-1))]
  
  rtn=try(as.FLQuant(transmute(ddply(as.data.frame(cn), .(year,iter), with,
                                     chapmanRobson(age,data)$est["Z",]),data=Estimate,year=year)))
  
  if ("try-error"%in%is(rtn)) {
    print(object@name)
    print(range(object)[c(6,3)]-c(0,1))
    return(NULL)}
  
  load(file.path("/home/laurie/pCloudDrive/papers/inPrep/lengthMethods/jabbaz/jabbaz",paste(name(object),"Z.rdata",sep="")))
  jabba=FLQuant(c(fit$timeseries[,"mu","FFmsy"]),dimnames=dimnames(fbar(object)))

  load(file.path("/home/laurie/pCloudDrive/papers/inPrep/lengthMethods/jabbaz/jabbaz",paste(name(object),"Zhat.rdata",sep="")))
  jabba_z=FLQuant(c(fit$timeseries[,"mu","FFmsy"]),dimnames=dimnames(fbar(object)))
  
  mcf(FLQuants(obs=fbar(object)%/%FLPar(object@benchmark["Fmsy"]),
               hat=rtn,
               ind=(rtn-object@fishlife["m"])%/%FLPar(object@fishlife["m"]),
               bdm=jabba,
               bdz=jabba_z
  ))
})
