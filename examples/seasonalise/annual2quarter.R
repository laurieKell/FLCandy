library(FLife)
library(FLCandy)

par=lhPar(linf=24)
eql=lhEql(par)
fbar(eql)[]=refpts(eql)["msy","harvest"]

stk=as(eql,"FLStock")
stk=fwd(stk,f=fbar(eql)[,-1],sr=eql)

qrt=seasonalise(stk)[,-101]

plot(FLStocks("Annual"=stk,"Quaterly"=qrt))

p=plot(FLStocks("Annual"=stk,"Quaterly"=qrt), 
            metrics=list(rec  =function(x) rec(x)[,,,1],
                         SSB  =function(x) ssb(x)[,,,1],
                         catch=function(x) apply(catch(x),c(2,3),sum),
                         fbar =function(x) apply(fbar(x),c(2,3),sum)))

