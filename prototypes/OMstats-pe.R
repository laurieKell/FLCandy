################################################################################
#### OM descriptive statistics & SPM priors ####################################
################################################################################
#### Fit a non-parametric production function to FLBRP and                  ####
####      estimate process error from FLStock (Bt+1 - Bt - Ct + SP(Bt))/Bt  ####
################################################################################

require(ggplot2); theme_set(theme_bw())

require(FLCore)
require(FLBRP)
require(ggplotFL)

ebiomass<-function(object){
  sel=harvest(object)
  wt =catch.wt(object)%*%sel%/%fapex(sel)
  wt =qmax(wt,0.000001)
  apply(wt%*%stock.n(object),2:6,sum)}

setwd("/home/laurie/Desktop/inPrep/sprat")
dirOM  =file.path(getwd(),"data/om")
dirRes =file.path(getwd(),"data/results")
dirRuns=file.path(getwd(),"data/runs")

data(ple4)
data(ple4brp)

load(file.path(dirOM,"om4Devs.RData"))
fbar(eq4)=FLQuant(seq(0,1,length.out=101))*refpts(eq4)["crash","harvest"]

## Non-parametric production function                                          
sp<-function(stk,eq,metric=ebiomass){
  
  #fbar(eq)=FLQuant(seq(0,1,length.out=201))*refpts(eq)["crash","harvest"]
  dat=with(model.frame(FLQuants(eq,"stock"=function(x) metric(x),"catch"=function(x) catch(x)),drop=T), 
           approx(stock,catch,xout=c(metric(stk))),dimnames=dimnames(metric(stk)))
  FLQuant(dat$y,dimnames=dimnames(stock(stk)))}

## Process Error
pe<-function(stk,eq="missing",metric=ebiomass){
  
  ### Set up next year biomass i.e. B[t+1]
  b1=FLQuant(as.numeric(NA),dimnames=dimnames(metric(stk)))
  b1[,-1,,1]=metric(stk)[,-dim(stk)[2],,dim(metric(stk))[4]]
  
  ##### If seasonal model then shift by 1 season
  if (dim(stk)[4]>1)
    b1[,,,-1]=metric(stk)[,,,-dim(stk)[4]]

  pd=FLQuant(0,dimnames=dimnames(metric(stk)))
  
  ### If FLBPR available
  if (!missing(eq)){
    pd=sp(stk,eq,metric)
    }
    
  log((metric(stk)-catch(stk)+pd)%/%b1)}

pe.om=procerr(window(om4.rec.1,start=1960),eq4,vb)
plot(pe.om)+facet_grid(season~.,scale="free")

plot(pe(ple4,ple4brp,metric=ebiomass))
plot(procerr(ple4,ple4brp,metric=ssb))

load(file.path(dirOM,"om4Devs.RData"))

om4=iter(om4.rec.1,1)

oma=annualise(window(iter(om4,1),start=1960))
om4=window(iter(om4.rec.1,1),start=1960)
ggplot(FLQuants("Seasonal"=apply(catch(om4),2,sum),
                "Annual"  =catch(oma)))+geom_line(aes(year,data,col=qname))+ylab("Catch")

ggplot(FLQuants("Seasonal"=ssb(om4)[,,,2],
                "Annual"  =ssb(oma)))+geom_line(aes(year,data,col=qname))+ylab("SSB")

ggplot(FLQuants("Seasonal"=rec(om4)[,,,2],
                "Annual"  =rec(oma)))+geom_line(aes(year,data,col=qname))+ylab("Rec")

ggplot(FLQuants("Seasonal"=stock.n(om4)[10,,,1],
                "Annual"  =stock.n(oma)[10]))+geom_line(aes(year,data,col=qname))+ylab("10 Year Olds")

ggplot(FLQuants("Seasonal"=apply(fbar(om4),2,sum),
                "Annual"  =fbar(oma)))+geom_line(aes(year,data,col=qname))+ylab("FBar")

harvest(oma)[1]=0
eqa=FLBRP(oma)
params(eqa)=params(eq4)
model(eqa)=model(eq4)
pe.om=procerr(window(oma,start=1960),eqa,vb)
plot(pe.om)+facet_grid(season~.)

pe.om=FLBRP:::procerr(window(oma,start=1960),eqa,vb)
plot(pe.om)+facet_grid(season~.)

prc.om=procerr(window(oma,start=1960),eqa,vb)
plot(-prc.om)+facet_grid(season~.)

ggplot()+
  geom_line(aes(year,data),col="blue",data=as.data.frame(-prc.om))+
  geom_line(aes(year,data),col="red", data=as.data.frame(pe.om))
