seasonShift<-function(flq,season){
  rtn=aaply(flq, c(2,6), function(x) {res=c(rep(NA,season-1),t(x))[seq(length(c(x)))]
                                      t(array(res,dim(x)[2:1]))
                                      })
  if (dim(flq)[6]>1) rtn=aperm(rtn,c(3,1,4,2)) else rtn=aperm(rtn,c(2,1,3))
  
  as.FLQuant(c(rtn),dimnames=dimnames(flq))}

seasonFLStocks<-function(object,season){
  
  stk =seasonalise(object,season=seq(season))
  stks=FLStocks(mlply(seq(season), function(x) stk))
  
  for (i in seq(season))      
    mat(stks[[i]])[,,,-i]=NA
    
  for (i in seq(season)[-1]){              
    stock.wt(   stks[[i]])=seasonShift(stock.wt(   stks[[i]]),season=i)
    catch.wt(   stks[[i]])=seasonShift(catch.wt(   stks[[i]]),season=i)
    landings.wt(stks[[i]])=seasonShift(landings.wt(stks[[i]]),season=i)
    discards.wt(stks[[i]])=seasonShift(discards.wt(stks[[i]]),season=i)
    catch(stks[[i]])=computeCatch(stks[[i]],slot="all")}
  
    stks}

seasonFLStock<-function(object,season){
  
  stk =seasonalise(object,season=seq(season))
  stk =FLCore:::expand(stk,unit=seq(season))
  
  for (i in seq(season))      
    mat(stks[[i]])[,,-1,-i]=NA
  
  for (i in seq(season)[-1]){              
    stock.wt(   stk)[,,i]=seasonShift(stock.wt(   stk)[,,i],season=i)
    catch.wt(   stk)[,,i]=seasonShift(catch.wt(   stk)[,,i],season=i)
    landings.wt(stk)[,,i]=seasonShift(landings.wt(stk)[,,i],season=i)
    discards.wt(stk)[,,i]=seasonShift(discards.wt(stk)[,,i],season=i)}
  
  catch(stks[[i]])=computeCatch(stks[[i]],slot="all")
  
  stock.wt(stk)[is.na(stock.wt(stk))]       =0.01*min(stock.wt(stk),na.rm=T)
  catch.wt(stk)[is.na(catch.wt(stk))]       =0.01*min(catch.wt(stk),na.rm=T)
  landings.wt(stk)[is.na(landings.wt(stk))]=0.01*min(landings.wt(stk),na.rm=T)
  discards.wt(stk)[is.na(discards.wt(stk))]=0.01*min(discards.wt(stk),na.rm=T)
  
  stock.n(stk)[is.na(stock.n(stk))]      =0.01*min(stock.n(stk),na.rm=T)
  catch.n(stk)[is.na(catch.n(stk))]      =0.01*min(catch.n(stk),na.rm=T)
  landings.n(stk)[is.na(landings.n(stk))]=0.01*min(landings.n(stk),na.rm=T)
  discards.n(stk)[is.na(discards.n(stk))]=0.01*min(discards.n(stk),na.rm=T)
  
  
  stk}


if (FALSE){

library(FLCore)
library(ggplotFL)
library(plyr)
library(dplyr)
  
data(ple4)
    
#source("~/Desktop/flr/FLCandy/R/wtInterp.R")
#source("~/Desktop/flr/FLCandy/R/seasonalise.R")

p4=seasonFLStock(ple4,4)

ggplot(stock.wt(p4))+
  geom_line(aes(age+(an(ac(season))-1)/4,data,group=year))+facet_wrap(~unit)

ggplot(stock.wt(p4)[,"2010"])+
  geom_line(aes(age+(an(ac(season))-1)/4,data,col=unit))

plot(p4, metrics=list(Recruits=rec,SSB=ssb,Catch=catch,F=fbar))

ctrl=data.frame(expand.grid(dimnames(fbar(p4)[,-1])[-c(1,5)]),
                            quant = "f",
                            value = 0.05)
ctrl$year=ac(ctrl$year)
#ctrl=transform(subset(ctrl,season==unit),value=value*(season==unit))
f4=fwd(p4,control=fwdControl(ctrl),sr=rec(p4))

plot(f4[,-1])

sr=ab(FLPar(s=0.7,v=1e6,spr0=spr0(p4[,,1,1])),"bevholt")[c("a","b")]

sr.par=FLPar(NA, dimnames=list(params=dimnames(sr)$params[1:2],unit=dimnames(p4)$unit,season=dimnames(p4)$season,iter=1))
for (i in seq(dim(p4)[3]))
  sr.par[,i,i,]=sr[1:2]
sr=predictModel(model="bevholt", params=sr.par)

s4=fwd(p4,control=fwdControl(ctrl),sr=sr)

plot(s4[,-1])

plot(rec(s4[,-1]))+facet_grid(unit~season)


}