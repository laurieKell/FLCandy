library(FLCore)
library(ggplotFL)
library(FLife)
library(interp)

par     =lhPar(FLPar(linf=100))
par     =propagate(par,21)
par["s"]=seq(0.21,0.99,length.out=21)
eq      =lhEql(lhPar(par))

dat=model.frame(FLQuants(list("Recruit"=rec(eq)/max(rec(eq)),"SSB"=ssb(eq)/1000)),drop=T)
dat=transform(dat,steepness=seq(0.21,0.99,length.out=21)[an(ac(iter))])
dt2=data.frame(with(dat,interp(Recruit,steepness,SSB,output="points",yo=seq(0.21,0.99,length.out=21),xo=rep(0.7,21))))

ggplot(dat)+
  geom_line(aes(SSB,Recruit,group=iter))+
  scale_x_continuous(labels=scales::percent)+
  scale_y_continuous(labels=scales::percent)+
  geom_hline(aes(yintercept=0.7),col="red")+
  xlab("SSB")+ylab("Recruitment")+
  geom_point(aes(z,x),col="red",
             data=dt2)

ggplot(dt2)+geom_line(aes(y,z))+
  xlab("Steepness")+ylab("Blim (recruitment  70% of Virgin) \nas fraction of Virgin Biomass")+
  theme_bw(24)




