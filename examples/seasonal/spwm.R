library(FLCore)
library(FLBRP)
library(ggplotFL)
library(FLasher)

load("~/Dropbox/NEA.mac.MSE.Base_case.AL/Updated_mac_stock.RData")
catch(mac_stock)=computeCatch(mac_stock,slot="all")

sr=fmle(as.FLSR(mac_stock,model="bevholt"),control=list(silent=TRUE))
eq=FLBRP(mac_stock,sr=sr)

### error message a bit obscure, the problem is NAs in the last year
om=fwdWindow(mac_stock,eq,end=2100)

om=fwdWindow(window(mac_stock,end=2017),eq,end=2100)

### bug/feature, error message obscure and no plus group should work. 
#NA plusgroup worked for FLBPR, I think the code is OK its just "over checking"
fbar=fbar(om)[,ac(2017:2100)]%=%refpts(eq)["msy","harvest"]

om=fwd(om,fbar=fbar,sr=eq)

## Need to specify the plusgroup
range(om)["plusgroup"]=range(om)["max"]
eq=FLBRP(om,sr=sr)
om=fwd(om,fbar=fbar,sr=eq)

### bug/feature
# fails if there are iters for FLStock, FLBRP and target.
# I.e. "over checking"
om=propagate(om,10)
eq=propagate(eq,10)
fbar=fbar(om)[,ac(2017:2100)]%=%refpts(eq)["msy","harvest"]

## this should work, as all have 10 iters
om=fwd(om,fbar=fbar,sr=eq)

om=fwd(om,fbar=iter(fbar,1),sr=eq)

### bug/feature
# These FLBRP and F:Stock value in the long-term shpuld  be the same
refpts(eq)["msy","yield"]
catch(om)[,"2100"]

om=window(iter(om,1),end=2017)
harvest.spwn(om)=0.3
m.spwn(om)=0.3

sr=fmle(as.FLSR(om,model="bevholt"),control=list(silent=TRUE))
eq=FLBRP(om,sr=sr)
om=fwdWindow(om,eq,end=2100)
om=fwd(om,fbar=fbar(om)[,ac(2017:2100)]%=%refpts(eq)["msy","harvest"],sr=eq)

### redo with all harvest.spwn the same
spwn=cbind(m.spwn=0.3,harvest.spwn=0.3,FLBRP=c(computeRefpts(eq)["msy","yield",1]),FLStock=c(catch(om)[,"2100",,,,1]))

om=window(iter(om,1),end=2017)
harvest.spwn(om)=0.0
m.spwn(om)=0.0

sr=fmle(as.FLSR(om,model="bevholt"),control=list(silent=TRUE))
eq=FLBRP(om,sr=sr)
om=fwdWindow(om,eq,end=2100)
om=fwd(om,fbar=fbar(om)[,ac(2017:2100)]%=%refpts(eq)["msy","harvest"],sr=eq)

##these are the same
spwn=rbind(spwn,cbind(m.spwn=0.0,harvest.spwn=0.0,FLBRP=c(computeRefpts(eq)["msy","yield"]),FLStock=c(catch(om)[,"2100"])))

### redo with only m.spwn=0
om=window(iter(om,1),end=2017)
harvest.spwn(om)=0.3
m.spwn(om)=0.0

sr=fmle(as.FLSR(om,model="bevholt"),control=list(silent=TRUE))
eq=FLBRP(om,sr=sr)
om=fwdWindow(om,eq,end=2100)
om=fwd(om,fbar=fbar(om)[,ac(2017:2100)]%=%refpts(eq)["msy","harvest"],sr=eq)

## These are different
spwn=rbind(spwn,cbind(m.spwn=0.0,harvest.spwn=0.3,FLBRP=c(computeRefpts(eq)["msy","yield"]),FLStock=c(catch(om)[,"2100"])))

### redo with only harvest.spwn=0
om=window(iter(om,1),end=2017)
harvest.spwn(om)=0.0
m.spwn(om)=0.3

sr=fmle(as.FLSR(om,model="bevholt"),control=list(silent=TRUE))
eq=FLBRP(om,sr=sr)
om=fwdWindow(om,eq,end=2100)
om=fwd(om,fbar=fbar(om)[,ac(2017:2100)]%=%refpts(eq)["msy","harvest"],sr=eq)

## These are different
spwn=rbind(spwn,cbind(m.spwn=0.3,harvest.spwn=0.0,FLBRP=c(computeRefpts(eq)["msy","yield"]),FLStock=c(catch(om)[,"2100"])))

### redo with only harvest.spwn!=m.spwn
om=window(iter(om,1),end=2017)
harvest.spwn(om)=0.2
m.spwn(om)=0.3

sr=fmle(as.FLSR(om,model="bevholt"),control=list(silent=TRUE))
eq=FLBRP(om,sr=sr)
om=fwdWindow(om,eq,end=2100)
om=fwd(om,fbar=fbar(om)[,ac(2017:2100)]%=%refpts(eq)["msy","harvest"],sr=eq)

## These are different
spwn=rbind(spwn,cbind(m.spwn=0.3,harvest.spwn=0.2,FLBRP=c(computeRefpts(eq)["msy","yield"]),FLStock=c(catch(om)[,"2100"])))

### redo with only harvest.spwn!=m.spwn
om=window(iter(om,1),end=2017)
harvest.spwn(om)=0.3
m.spwn(om)=0.2

sr=fmle(as.FLSR(om,model="bevholt"),control=list(silent=TRUE))
eq=FLBRP(om,sr=sr)
om=fwdWindow(om,eq,end=2100)
om=fwd(om,fbar=fbar(om)[,ac(2017:2100)]%=%refpts(eq)["msy","harvest"],sr=eq)

## These are different
spwn=rbind(spwn,cbind(m.spwn=0.2,harvest.spwn=0.3,FLBRP=c(computeRefpts(eq)["msy","yield"]),FLStock=c(catch(om)[,"2100"])))

spwn
