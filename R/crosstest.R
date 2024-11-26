#basically i) coerce from FLstock to FLSAM, ii) compute FLBRP, iii) calc status relative to refpts iv) plot ROC curve to estimate classification skill and bias

setMethod("crosstest", signature(object="FLStock"),
          function(object=object,...){
            
            res=crosstestFn(object)  
            
            res})  

if (FALSE){
## Mackerel ####################################################################
load("/home/laurie/Desktop/inPrep/pew/github/erp/data/om/om.RData")
load("/home/laurie/Desktop/inPrep/pew/github/erp/data/inputs/ices/mac.RData")

## FLife #######################################################################

par=FLPar(c(linf=8.0,
            k   =0.7,
            t0  =-0.1,
            s   =0.7,
            v   =1000,
            l50 =3.5))
par=lhPar(par)
eq=lhEql(par,m=function(...) 0.2)

fbar   =FLQuant(c(rep(1,20),seq(1,2,length.out=10),seq(2,0.8,length.out=11)[-1]))
eq@fbar=fbar%*%refpts(eq)["msy","harvest"]

om=as(eq,"FLStock")
om=qapply(om, function(x){ dimnames(x)$year=seq(1981,2020); x})
om=fwd(om,fbar=fbar(om)[,-1],sr=eq)

om=propagate(om,100)
om=fwd(om,fbar=fbar(om)[,-1],
       sr  =eq,residuals=rlnorm(100,fbar(om)*0,0.3))

mp=om
m(mp)[]=0.2

control=as(FLQuants("catch"=catch[om][,"2022"],"f"=fbar[om][,"2023"]),"fwdControl")

crosstestFn<-function(om,
                      eq     =FLBRP(om,params=FLPar(mean(rec(om))),model=geomean()$model),
                      control="missing",indices="missing"){
  
           retrun(mp)}
}

