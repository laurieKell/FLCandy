library(FLCore)
library(FLBRP)
library(FLasher)
library(FLCandy)
library(ggplotFL)

# Original object
data(ple4sex)
plot(ple4sex)

# make Fs compatible with Ns
ple4sex=update(ple4sex)
plot(ple4sex)

# fwd
sr=as.FLSR(ple4sex,model="geomean")
params(sr)=FLPar(1,dimnames=list(params="a",iter=1))

recs=rec(ple4sex)

#fbar=fwdControl(FLQuants(fbar=apply(fbar(ple4sex)[,-1],2,mean)))

control=fwdControl(
  lapply(dimnames(ple4sex)$year[-1], function(x) list(year=x, quant="fbar", value=0.1)))

ple4fwd=fwd(ple4sex,control=control,sr=sr,residuals=recs)

plot(ple4fwd)

#Target for both sexes
apply(fbar(ple4fwd),2,mean)

quantMeans(harvest(unitSums(stock.n(ple4fwd)), unitSums(catch.n(ple4fwd)), unitMeans(m(ple4fwd)))[2:6,])


load("/home/laurence-kell/Downloads/alb-byyear.RData")
yby=alb

load("/home/laurence-kell/Downloads/alb-fmsy.RData")

for (y in seq(2019,2040,1)) {
  yalb <- fwd(alb, sr=srr,
  control=fwdControl(year=y+0, value=0.1562, quant="fbar",
  minAge=1, maxAge=12))}

plot(FLStocks("fwd"=alb,"ylab"=yalb))


