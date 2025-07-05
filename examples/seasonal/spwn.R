library(FLCore)
library(FLBRP)
library(ggplotFL)
library(FLasher)

load("~/Dropbox/NEA.mac.MSE.Base_case.AL/Updated_mac_stock.RData")
range(mac_stock)[3]=12

om=window(mac_stock,end=2017)
catch(mac_stock)=computeCatch(mac_stock,slot="all")
om=setPlusGroup(om,60)

eq=FLBRP(om)
harvest.spwn(om)=0.3
m.spwn(om)=0.3

#sr=as.FLSR(om,model="bevholtSV");upper(sr)[1:2]=1e12;lower(sr)[1:2]=1e-12
#sr=fmle(sr, fixed=list(s=0.7,spr0=spr0(eq)),
#        control=list(silent=TRUE),
#        method="Brent")
#model( eq)=bevholt()$model
#params(eq)=ab(params(sr),"bevholt")[c("a","b")]
#eq=brp(eq)
om=window(mac_stock,end=2017)
sr=fmle(as.FLSR(om,model="bevholt"))
eq=FLBRP(om,sr=sr)

fbar(eq)=fbar(eq)/max(fbar(eq))*refpts(eq)["crash","harvest"]

om=fwdWindow(om,eq,end=2100)
om=fwd(om,fbar=fbar(om)[,ac(2018:2100)]%=%refpts(eq)["msy","harvest"],sr=eq)

spwn=cbind(m.spwn=0.3,harvest.spwn=0.3,FLBRP=c(computeRefpts(eq)["msy","yield",1]),FLStock=c(catch(om)[,"2100",,,,1]))

ggplot(model.frame(FLQuants(eq, Rec=rec,SSB=ssb)))+
  geom_line(aes(SSB,Rec))+
  geom_point(aes(SSB,Rec),data=model.frame(FLQuants(window(om,start=2018), Rec=rec,SSB=ssb)))+
  #geom_point(aes(SSB,rec),col="red",data=model.frame(FLQuants(sr,SSB=ssb,rec=predict)))


### redo with all harvest.spwn the same
om=window(mac_stock,end=2017)
harvest.spwn(om)=0.0
m.spwn(om)=0.0

sr=as.FLSR(om,model="bevholtSV");upper(sr)[1:2]=1e12;lower(sr)[1:2]=1e-12
sr=fmle(sr, fixed=list(s=0.7,spr0=spr0(eq)),
        control=list(silent=TRUE),
        method="Brent")
params(eq)=ab(params(sr),"bevholt")[c("a","b")]
eq=brp(eq)
om=fwdWindow(om,eq,end=2100)
om=fwd(om,fbar=fbar(om)[,ac(2018:2100)]%=%refpts(eq)["msy","harvest"],sr=eq)

spwn=rbind(spwn,cbind(m.spwn=0.0,harvest.spwn=0.0,FLBRP=c(computeRefpts(eq)["msy","yield"]),FLStock=c(catch(om)[,"2100"])))

### redo with only m.spwn=0
harvest.spwn(om)=0.3
m.spwn(om)=0.0

om=window(mac_stock,end=2017)
sr=as.FLSR(om,model="bevholtSV");upper(sr)[1:2]=1e12;lower(sr)[1:2]=1e-12
sr=fmle(sr, fixed=list(s=0.7,spr0=spr0(eq)),
        control=list(silent=TRUE),
        method="Brent")
params(eq)=ab(params(sr),"bevholt")[c("a","b")]
eq=brp(eq)
om=fwdWindow(om,eq,end=2100)
om=fwd(om,fbar=fbar(om)[,ac(2018:2100)]%=%refpts(eq)["msy","harvest"],sr=eq)

spwn=rbind(spwn,cbind(m.spwn=0.0,harvest.spwn=0.3,FLBRP=c(computeRefpts(eq)["msy","yield"]),FLStock=c(catch(om)[,"2100"])))

### redo with only harvest.spwn=0
om=window(mac_stock,end=2017)
harvest.spwn(om)=0.0
m.spwn(om)=0.3

sr=as.FLSR(om,model="bevholtSV");upper(sr)[1:2]=1e12;lower(sr)[1:2]=1e-12
sr=fmle(sr, fixed=list(s=0.7,spr0=spr0(eq)),
        control=list(silent=TRUE),
        method="Brent")
params(eq)=ab(params(sr),"bevholt")[c("a","b")]
eq=brp(eq)
om=fwdWindow(om,eq,end=2100)
om=fwd(om,fbar=fbar(om)[,ac(2018:2100)]%=%refpts(eq)["msy","harvest"],sr=eq)

## These are different
spwn=rbind(spwn,cbind(m.spwn=0.3,harvest.spwn=0.0,FLBRP=c(computeRefpts(eq)["msy","yield"]),FLStock=c(catch(om)[,"2100"])))

### redo with harvest.spwn<m.spwn
om=window(mac_stock,end=2017)
harvest.spwn(om)=0.2
m.spwn(om)=0.3

sr=as.FLSR(om,model="bevholtSV");upper(sr)[1:2]=1e12;lower(sr)[1:2]=1e-12
sr=fmle(sr, fixed=list(s=0.7,spr0=spr0(eq)),
        control=list(silent=TRUE),
        method="Brent")
params(eq)=ab(params(sr),"bevholt")[c("a","b")]
eq=brp(eq)
om=fwdWindow(om,eq,end=2100)
om=fwd(om,fbar=fbar(om)[,ac(2018:2100)]%=%refpts(eq)["msy","harvest"],sr=eq)

## These are different
spwn=rbind(spwn,cbind(m.spwn=0.3,harvest.spwn=0.2,FLBRP=c(computeRefpts(eq)["msy","yield"]),FLStock=c(catch(om)[,"2100"])))

### redo with harvest.spwn>m.spwn
om=window(mac_stock,end=2017)
om=setPlusGroup(om,100)
eq=FLBRP(om)
harvest.spwn(om)=0.3
m.spwn(om)=0.2

sr=as.FLSR(om,model="bevholtSV");upper(sr)[1:2]=1e12;lower(sr)[1:2]=1e-12
sr=fmle(sr, fixed=list(s=0.7,spr0=spr0(eq)),
        control=list(silent=TRUE),
        method="Brent")
params(eq)=ab(params(sr),"bevholt")[c("a","b")]
eq=brp(eq)
om=fwdWindow(om,eq,end=2100)
om=fwd(om,fbar=fbar(om)[,ac(2018:2100)]%=%refpts(eq)["msy","harvest"],sr=eq)

## These are different
spwn=rbind(spwn,cbind(m.spwn=0.2,harvest.spwn=0.3,FLBRP=c(computeRefpts(eq)["msy","yield"]),FLStock=c(catch(om)[,"2100"])))

spwn
(spwn[,3]-spwn[,4])/spwn[,3]

ggplot(model.frame(FLQuants(eq, Rec=rec,SSB=ssb)))+
  geom_line(aes(SSB,Rec))+
  geom_point(aes(SSB,Rec),data=model.frame(FLQuants(window(om,start=2018), Rec=rec,SSB=ssb)))+
  geom_point(aes(SSB,rec),col="red",data=model.frame(FLQuants(sr,SSB=ssb,rec=predict)))

