library(devtools)
install_github("flr/mse")

library(mse)
library(FLife)

data(ple4)
data(ple4brp)

sage<-function(object) apply(stock.n(object)%*%ages(stock.n(object)),2:6,sum)%/%
  apply(stock.n(object),2:6,sum)
cage<-function(object) apply(catch.n(object)%*%ages(catch.n(object)),2:6,sum)%/%
  apply(catch.n(object),2:6,sum) 
swt<-function(object) apply(stock.n(object)%*%stock.wt(object),2:6,sum)%/%
  apply(stock.n(object),2:6,sum)
cwt<-function(object) apply(catch.n(object)%*%catch.wt(object),2:6,sum)%/%
  apply(catch.n(object),2:6,sum) 
hvt<-function(object) catch(object)/stock(object)
ebiomass<-function(object){
  sel=harvest(object)
  wt =catch.wt(object)%*%sel%/%fapex(sel)
  wt =qmax(wt,0.000001)
  apply(wt%*%stock.n(object),2:6,sum)}
recs<-function(object) {
  res=rec(object)
  dimnames(res)[[1]]="all"
  res}
catchJuv<-function(object) 
  apply(catch.n(object)%*%(1-mat(object))%*%catch.wt(object),2:6,sum)
plusgroup<-function(object){
  res=stock.n(object)[ac(range(ple4)["plusgroup"])]
  dimnames(res)[[1]]="all"
  res}

omStock<-function(object){
  res=FLQuants(object,
               "ssb"      =FLCore::ssb,
               "stock"    =FLCore::stock,
               "ebiomass" =plusgroup,
               "plusgroup"=plusgroup,
               "rec"      =recs,
               "catch"    =FLCore::catch,
               "catchjuv" =catchJuv,
               "fbar"     =FLCore::fbar,
               "hvt"      =hvt,
               "swt"      =swt,
               "cwt"      =cwt,
               "sage"     =sage,
               "cage"     =cage)
  
  model.frame(mcf(res),drop=TRUE)}

res=list("ssb"     =FLCore::ssb,
         "stock"   =FLCore::stock,
         "ebiomass"=ebiomass,
         "plusgroup"=plusgroup,
         "rec"     =recs,
         "catch"   =FLCore::catch,
         "catchjuv"=catchJuv,
         "fbar"    =FLCore::fbar,
         "hvt"     =hvt,
         "harvest" =FLCore::fbar,
         "swt"     =swt,
         "cwt"     =cwt,
         "sage"    =sage,
         "cage"    =cage)


lenFn<-function(x,y=FLPar(a=1,b=3)){
  sln<-function(object) apply(stock.n(object)%*%exp(log(stock.wt(object)%/%y["a"])%/%y["b"]),2:6,sum)%/%
    apply(stock.n(object),2:6,sum)
  cln<-function(object) apply(catch.n(object)%*%exp(log(catch.wt(object)%/%y["a"])%/%y["b"]),2:6,sum)%/%
    apply(catch.n(object),2:6,sum) 
  
  model.frame(FLQuants(x,"sln"=sln,"cln"=cln),drop=TRUE)}

omRefs<-function(object){
  
  refs=rbind(as.data.frame(object["crash",c("harvest")]),
             as.data.frame(object["virgin",c("rec","ssb")]),
             as.data.frame(object["msy",c("yield","ssb","biomass","harvest")]))
  refs=cast(refs,iter~refpt+quant,value="data")
  
  refs}

flqs=FLQuants(ple4,metrics=res)

head(omStock(ple4))
head(lenFn(ple4,FLPar(a=1,b=3)))
omRefs(refpts(ple4brp))

head(cbind(omStock(ple4),
           lenFn(ple4,FLPar(a=1,b=3)),
           omRefs(refpts(ple4brp))))

ind=list(Ind1=list(~ssb))


data(p4om)
 indicators <- list(
   T1=list(~yearMeans(C[, -1]/C[, -dims(C)$year]), name="mean(C[t] / C[t-1])",
   desc="Mean absolute proportional change in catch"),
 T2=list(~yearVars(C), name="var(C)", desc="Variance in catch"),
 T3=list(~yearVars(F), name="var(F)", desc="Variance in fishing mortality"))
run <- window(stock(om), start=2000, end=2015)
performance(run, indicators, refpts=FLPar(MSY=0),
metrics=list(C=catch, F=fbar), years=list(20:25, 20:30))
   