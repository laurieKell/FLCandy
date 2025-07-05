library(FLCore)
library(ggplotFL)
library(FLBRP)
library(FLasher)
library(FLife)
library(mydas)
library(popbio)
library(spatstat)

library(plyr)
library(dplyr)
library(reshape)
library(GGally)

library(LBSPR)

library(doParallel)
library(foreach)

#library(googledrive)
#fls=drive_find()
#drive_download("lhs.RData",path="lhs.RData")

load("/home/laurence-kell/Desktop/projects/mydas/papers/roc/data/lhs.RData")

dirDat="/home/laurence-kell/Desktop/papers/roc/results"

## Scenarios
design=data.frame(s      =c( 0.9, 0.7, 0.9, 0.9, 0.9, 0.9),
                  sel2   =c( 1.0, 1.0,5000, 1.0, 1.0, 1.0),
                  sel3   =c(5000,5000,5000,  50,5000,5000),
                  nsample=c( 500, 500, 500, 500, 250, 500),
                  m      =c(rep("gislason",5),"constant"))
design=mdply(expand.grid(Stock=names(lhs),CV=c("0.3","0.5","AR"),stringsAsFactors=FALSE), 
             function(Stock,CV) design)

f=FLQuant(c(rep(0.1,60),seq(0.1,2.5,length.out=40)[-40],
            seq(2.5,0.7,length.out=11),rep(0.7,20)))

## Stochasticity
nits=100
set.seed(234)
srDev=FLQuants(NULL)
srDev[["0.3"]]=rlnoise(nits,f%=%0,0.3,0.0);set.seed(234)
srDev[["0.5"]]=rlnoise(nits,f%=%0,0.5,0.0);set.seed(234)
srDev[["AR"]] =rlnoise(nits,f%=%0,0.3,0.7)

cl <- makePSOCKcluster(5)
registerDoParallel(cl)
res<-foreach(i=seq(1,19), 
              .combine=rbind,
              .multicombine=TRUE,
              .export=c("dirDat","design","srDev","f","lhs"),
              .packages=c("FLCore","ggplotFL","FLBRP","FLasher",
                          "FLife","mydas","popbio","spatstat",
                          "plyr","dplyr","reshape","GGally","LBSPR")) %dopar% {
  
   source('~/Desktop/flr/mydas-lk/R/oemLn.R', echo=TRUE)

#for (i in c(78,84)){
  #source('~/Desktop/sea++/mydas/pkg/R/lbspr.R')
  
  ## modify parameters                                                  
  par        =lhs[[design[i,"Stock"]]]
  par["s"]   =design[i,"s"]
  par["sel2"]=design[i,"sel2"]
  par["sel3"]=design[i,"sel3"]

  ## simulated stock
  if (design[i,"m"]=="gislason")
     eq=lhEql(par)
  else{
    gislasonFn<-function(x,params) {
      
      length=wt2len(stock.wt(x),params)
      length=length%=%params["linf"]
      exp(params["m1"]%+%(params["m2"]%*%log(length))%+%(params["m3"]%*%log(params["linf"]))%+%log(params["k"]))}
    
     eq=lhEql(par,m=function(x,params) {
                           gislasonFn(x,params)})
     }
  
  fbar(eq)=f%*%fbar(eq)[catch(eq)==max(catch(eq))]
  om      =as(eq,"FLStock")
  om      =propagate(om,dim(srDev[[design[i,"CV"]]])[6])
  om      =fwd(om,fbar=fbar(eq)[,2:130],
                  sr  =eq,residuals=srDev[[design[i,"CV"]]])

  ## length frequencies
  ak   =invAlk(par,cv=0.1)  
  prior=popdyn(par)

  ## Indicators
 
  ## Catch
  set.seed(789)
  lfdc=lenSample(catch.n(om)[,50:125],ak,nsample=design[i,"nsample"])

  ### model based
  #tmp=capture.output({
  #  lbc=lbspr(lfdc,prior)
  #  save(lbc,file=file.path(dirDat,"sims","catch",paste("lb",i,"RData",sep=".")))
  #},type="message")

  ### empirical
  indc=transform(subset(as.data.frame(lfdc,drop=TRUE),data>0),
                wt=c(par["a"])*len^c(par["b"]),
                lopt=c(2/3*par["linf"]))
  indc =ddply(indc, .(year,iter), with, lenInd(len,data,wt,lopt))
  indc=cbind(indc,linf=c(par["linf"]),l50=c(par["l50"]))
  save(lfdc,indc,file=file.path(dirDat,"sims","catch",paste("ind",i,"RData",sep=".")))

  ## Stock
  set.seed(789)
  stock.n(om)[1]=0
  lfds=lenSample(stock.n(om)[,50:125]*exp(-(harvest(om)[,50:125]+m(om)[,50:125])*0.5),
                 ak,nsample=design[i,"nsample"])
  
  ### model based
  #tmp=capture.output({
  #  lbs=lbspr(lfds,prior)
  #  save(lbs,file=file.path(dirDat,"sims","stock",paste("lb",i,"RData",sep=".")))
  #},type="message")
  
  ### empirical
  inds=transform(subset(as.data.frame(lfds,drop=TRUE),data>0),
                wt=c(par["a"])*len^c(par["b"]),
                lopt=c(2/3*par["linf"]))
  inds =ddply(inds, .(year,iter), with, lenInd(len,data,wt,lopt))
  inds=cbind(inds,linf=c(par["linf"]),l50=c(par["l50"]))
  save(lfds,inds,file=file.path(dirDat,"sims","stock",paste("ind",i,"RData",sep=".")))

  design[i,]}
                            
stopCluster(cl)

indc=mdply(dimnames(design)[[1]], function(x) {
  load(file.path("/home/laurence-kell/Desktop/papers/roc/results/sims/catch",paste("ind",x,"RData",sep=".")))
  indc=subset(indc,year>50&year<121)
  cbind(scen=x,indc)})[,-1]
indc=cbind(Fishery="Dependent",design[indc$scen,],indc)

inds=mdply(dimnames(design)[[1]], function(x) {
  load(file.path("/home/laurence-kell/Desktop/papers/roc/results/sims/stock",paste("ind",x,"RData",sep=".")))
  inds=subset(inds,year>50&year<121)
  cbind(scen=x,inds)})[,-1]
inds=cbind(Fishery="Independent",design[inds$scen,],inds)

indicators=rbind(inds,indc)

lopt=ldply(lhs, function(x) data.frame(x["lopt"]))
names(lopt)=c("Stock","lopt")
indicators=merge(indicators,lopt,by="Stock")

save(indicators,file="/home/laurence-kell/Desktop/papers/roc/results/indicators.RData",compress="xz")

