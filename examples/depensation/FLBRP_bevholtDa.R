# FLBRP_bevholtDa.R - DESC
# /home/mosqu003/FLR/bugs/FLBRP_bevholtDa.R

# Copyright (c) WUR, 2023.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

install_github('flr/FLBRP')


library(FLBRP)
library(FLSRTMB)
library(patchwork)

data(ple4brp)
data(ple4)

# - FITTING

# FIT bevholt

bh <- srrTMB(as.FLSR(ple4, model=bevholt), spr0=spr0y(ple4))

# FIT bevholtDa

bhd <- srrTMB(as.FLSR(ple4, model=bevholtDa), spr0=spr0y(ple4))

# BUG: d < 1
params(bhd)

# FIT in fmle with fixed d

bhd <- fmle(as.FLSR(ple4, model=bevholtDa), fixed=list(d=1.4))

# PLOT
plotsrs(FLSRs(BH=bh, BHDa=bhd))

# - REFPTS

pbh <- brp(FLBRP(ple4, sr=bh))
pbhd <- brp(FLBRP(ple4, sr=bhd))

refpts(pbh)
refpts(pbhd)

plot(pbh) + plot(pbhd)

# - FORECAST at twice fbar

fbh <- fwd(ple4, sr=bh, fbar=fbar(ple4)[, ac(2000:2017)] * 2)

fbhd <- fwd(ple4, sr=bhd, fbar=fbar(ple4)[, ac(2000:2017)] * 2)

# PLOT

plot(fbh, fbhd)

### Run as FLStock projection to long-term and compare
stk=propagate(as(pbhd,"FLStock"),100)
fbar=FLQuant(rep(seq(1e-5,computeRefpts(pbhd)["crash","harvest"],length.out=100),each=101),
             dimnames=dimnames(fbar(stk)))
stk=fwd(stk,f=fbar[,-(1:5)],sr=pbhd)

dat=model.frame(FLQuants(window(stk,start=100),SSB=ssb,Rec=rec,F=function(x) fbar(x),Yield=catch),drop=TRUE)

ggplot()+
  geom_line(aes(F,Yield),data=model.frame(FLQuants(pbh, F=function(x) fbar(x),Yield=catch,SSB=ssb,Rec=rec)))+
  geom_line(aes(F,Yield),data=model.frame(FLQuants(pbhd,F=function(x) fbar(x),Yield=catch,SSB=ssb,Rec=rec)),col="brown")+
  geom_line(aes(F,Yield),data=dat,col="red")

ggplot()+
  geom_line(aes(F,SSB),data=model.frame(FLQuants(pbh, F=function(x) fbar(x),Yield=catch,SSB=ssb,Rec=rec)))+
  geom_line(aes(F,SSB),data=model.frame(FLQuants(pbhd,F=function(x) fbar(x),Yield=catch,SSB=ssb,Rec=rec)),col="brown")+
  geom_line(aes(F,SSB),data=dat,col="red")
