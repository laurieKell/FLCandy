library(FLasher)
library(ggplotFL)
library(FLCandy)

load("/home/laurence-kell/Downloads/alb-byyear.RData")
yalb=alb

load("/home/laurence-kell/Downloads/alb-fmsy.RData")

plot(FLStocks(fwd=alb[,ac(2010:2040)], fwd.byyear=yalb[,ac(2010:2040)]))

for (y in seq(2020,2040-0,1)) {
  print(y)
  yalb<-fwd(yalb, sr=srr,
                  control=fwdControl(year=y+(0:0), value=0.15, quant="fbar",
                                     minAge=1, maxAge=12))}
plot(FLStocks(fwd=alb[,ac(2010:2039)], fwd.byyear=yalb[,ac(2010:2039)], update=FLCandy::update(yalb[,ac(2020:2039)])))

(stock.n(yalb[,ac(2030:2039)])*exp(-m(yalb)[,ac(2030:2039)]-harvest(yalb)[,ac(2030:2039)]))[-15]/stock.n(yalb[-1,ac(2030:2039)])

load("/home/laurence-kell/Downloads/alb-byyear.RData")
yalb=alb
load("/home/laurence-kell/Downloads/alb-fmsy.RData")

plot(FLStocks(fwd=alb[,ac(2010:2040)], fwd.byyear=yalb[,ac(2010:2040)]))

alb<-fwd(yalb, sr=srr,
         control=fwdControl(year=2020:2040, value=10000, quant="catch"))

for (y in seq(2020,2040-0,1)) {
  print(y)
  yalb<-fwd(yalb, sr=srr,
            control=fwdControl(year=y+(0:0), value=10000, quant="catch"))}
plot(FLStocks(fwd=alb[,ac(2010:2040)], fwd.byyear=yalb[,ac(2010:2040)], update=FLCandy::update(yalb[,ac(2020:2040)])))

load("/home/laurence-kell/Downloads/alb-byyear.RData")
yalb=alb

for (y in seq(1980,2020-0,1)) {
  print(y)
  yalb<- fwd(yalb, sr=srr,
                         control=as(FLQuants(fbar=0.5*unitMeans(fbar(yalb)[,ac(y+0:0)])),'fwdControl'),
                         residuals=rec(yalb)/5000)}

plot(FLStocks(fwd=alb[,ac(1980:2020)], fwd.byyear=yalb[,ac(1980:2020)]))

load("/home/laurence-kell/Downloads/alb-byyear.RData")
synth=alb

m(      synth)= 0.0
harvest(synth)= 0.1

stock.n(synth)=as.FLQuant(transform(as.data.frame(stock.n(synth)),data=year+age*10000+ifelse(unit=="M",0.1,0)))

for (y in seq(1980,1994-0,1)) {
  print(y)
  synth<- fwd(synth, sr=srr,
             control=as(FLQuants(fbar=unitMeans(fbar(synth)[,ac(y+0:0)])),'fwdControl'),
             residuals=rec(synth)/5000)}

stock.n(synth[,ac(1980:1994)])%*%exp(as.FLQuant(seq(0.0,0.2*14,0.2),dimnames=list(age=0:14)))
