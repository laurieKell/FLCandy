library(FLCore)
library(FLBRP)
library(FLasher)
library(ggplotFL)
library(FLSRTMB)
library(ggpubr)

data(ple4)

srd=fmle(as.FLSR(ple4, model=bevholtDa), fixed=list(d=10.0),control=list(silent=TRUE))
plot(srd)
eqd=FLBRP(ple4,sr=srd)
plot(eqd)

sr=srrTMB(as.FLSR(ple4, model=bevholt), spr0=spr0y(ple4))
sr=fmle(  as.FLSR(ple4, model=bevholt), control=list(silent=TRUE))
plot(sr)

eq=FLBRP(ple4,sr=sr)
plot(eq)

plot(FLSRs("Bev Holt"=sr,"Depensation"=srd))
ggplot()+
  geom_line(aes(SSB,Recruits),data=model.frame(FLQuants(SSB=ssb(eq), Recruits=rec(eq))))+
  geom_line(aes(SSB,Recruits),data=model.frame(FLQuants(SSB=ssb(eqd),Recruits=rec(eqd))),col="red")+
  scale_x_continuous(limits=c(0,8e5))

stk=propagate(as(eqd,"FLStock"),100)
fbar=FLQuant(rep(seq(1e-5,computeRefpts(eqd)["crash","harvest"],length.out=100),each=101),
             dimnames=dimnames(fbar(stk)))
stk=fwd(stk,f=fbar[,-(1:5)],sr=eqd)

dat=model.frame(FLQuants(window(stk,start=100),SSB=ssb,Rec=rec,F=function(x) fbar(x),Yield=catch),drop=TRUE)

p2=ggplot()+
  geom_line(aes(SSB,Rec),data=model.frame(FLQuants(eq,F=function(x) fbar(x),Yield=catch,SSB=ssb,Rec=rec)),col="brown")+
  geom_line(aes(SSB,Rec),data=dat,col="red")

p1=ggplot()+
  geom_line(aes(F,SSB),data=model.frame(FLQuants(eq,F=function(x) fbar(x),Yield=catch,SSB=ssb,Rec=rec)),col="brown")+
  geom_line(aes(F,SSB),data=dat,col="red")

p3=ggplot()+
  geom_line(aes(F,Yield),data=model.frame(FLQuants(eq,F=function(x) fbar(x),Yield=catch,SSB=ssb,Rec=rec)),col="brown")+
  geom_line(aes(F,Yield),data=dat,col="red")

p4=ggplot()+
  geom_line(aes(SSB,Yield),data=model.frame(FLQuants(eq,F=function(x) fbar(x),Yield=catch,SSB=ssb,Rec=rec)),col="brown")+
  geom_line(aes(SSB,Yield),data=dat,col="red")

p5=ggplot()+
  geom_path(aes(SSB/Rec,SSB),data=model.frame(FLQuants(eq,F=function(x) fbar(x),Yield=catch,SSB=ssb,Rec=rec)),col="brown")+
  geom_path(aes(SSB/Rec,SSB),data=dat,col="red")


ggarrange(p1,p2,p3,p4,p5,ncol=2,nrow=3)

plot(FLSRs("Bev Holt"=sr,"Depensation"=srd))

computeRefpts(eq2)[,1:5]

computeRefpts(eq2)["crash",1]

r=a/(1+(b/S)^d)




