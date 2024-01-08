library(FLBRP)
library(FLife)
library(mydas)
library(ggplotFL)
library(plyr)
library(popbio)
library(interp)

# E. Adding Flexibility in Rebuilding Plans
# 
# Calculating Tmax
# 
# The NS1 guidelines provide guidance on determining the minimum (Tmin), maximum (Tmax), and target (Ttarget) time to rebuild a stock to a level that supports MSY (Bmsy). In the past, Councils have had difficulties calculating Tmax based on the original data-intensive method (i.e., Tmin + one generation time) that requires data on life history, natural mortality, age at maturity, fecundity, and maximum age of the stock (Restrepo, et al. 1998). In order to allow Councils to make Tmax calculations despite variable information and data availability amongst stocks, NMFS proposed specifying three methods to calculate Tmax within the guidelines: (1) Tmin plus one mean generation time (status quo); (2) the amount of time the stock is expected to take to rebuild to its Bmsy if fished at 75 percent of the MFMT; or (3) Tmin multiplied by two. Further background and rationale on the proposed revisions to the guidance on the calculation of Tmax was provided on pages 2795-2796 of the proposed rule. See80 FR 2795-2796, January 20, 2015.
# https://www.federalregister.gov/documents/2016/10/18/2016-24500/magnuson-stevens-act-provisions-national-standard-guidelines
# 
# The IUCN Red List of Threatened Species (IUCN, 2017a) is the primary authority for extinction risk assessments. In the Red List assessment, species are categorized as threatened based on criteria related to population decline, geographic range size, fragmentation, and small population size (IUCN, 2017b). In criteria related to population decline, generation time acts as a standardization for time units that allows using the same criteria on species with extremely different life spans (Mace et al., 2008). In criterion A, for example, population size reduction is measured over 10 years or three generations, whichever is longer.
# 
# it is applied for decline
# 
# but I wonder if we can turn it around and use for recovery

if (FALSE){
par=lhPar(FLPar(linf=250,s=0.9))
eq      =lhEql(par)
ftar    =seq(c(refpts(eq)["msy","harvest"]),c(refpts(eq)["crash","harvest"]),length.out=51)

fbar(eq)=FLQuant(ftar)
r       =mdply(c(fbar(eq)), function(x)  log(lambda(FLife:::leslie(eq,f=c(x))[,,1,drop=T])))

dt1=model.frame(FLQuants(ssb=ssb(eq),f=fbar(eq)),drop=T)
dt1=cbind(dt1,r=r[,2],bmsy=c(refpts(eq)["msy","ssb"]))
dt1=transform(dt1,doubling=log(bmsy/ssb)/r)

ggplot(dt1)+
  geom_line(aes(ssb/bmsy,doubling))+
  geom_hline(aes(yintercept=c(gt(eq))),col="red")+
  geom_hline(aes(yintercept=c(gt(eq))*0.5),col="orange")+
  xlab(expression(SSB/B[MSY]))+ylab("Time to recovery")+
  scale_y_continuous(limits=c(0,40))

eq      =lhEql(par)
fbar(eq)=FLQuant(rep(1,100))
fbar(eq)=propagate(fbar(eq),51)%*%FLQuant(ftar,dimnames=list(iter=seq(51)))
stk     =as(eq,"FLStock")
stk     =fwd(stk,f=fbar(eq)[,-1],sr=eq)
stk     =fwd(stk,f=fbar(eq)[,-seq(50)]*0,sr=eq)

dat=as.data.frame(ssb(stk)[,-1]/c(refpts(eq)["msy","ssb"]),drop=T)

ggplot(subset(dat,year>50))+
  geom_line(aes(year,data,group=iter))+
  xlab("Year")+ylab(expression(SSB/B[MSY]))+
  geom_vline(aes(xintercept=c(gt(iter(eq,1)))+50),col="red")+
  geom_vline(aes(xintercept=c(gt(iter(eq,1)))*0.5+50),col="orange")+
  geom_hline(aes(yintercept=1))+
  scale_y_continuous(limits=c(0,1.2))

ggplot((res.x%/%res.y)[,ac(55:100)])+
  geom_line(aes(year-51,data,group=iter))+
  geom_hline(aes(yintercept=1),col="red")

plot(ABI(om,eq))

dt2=data.frame(with(dat,interp(iter,data,year,
                               output="points",o=seq(),xo=rep(1,51))))

dt3=ddply(dat,.(iter), with, year[(data-1)^2==min((data-1)^2)])
ggplot(dt3)+
  geom_point(aes(c(ssb(eq)[,2,,,,,1])/c(refpts(eq)["msy","ssb"])[an(ac(iter))],V1))


ggplot(dt3)+
  geom_smooth(aes(V1,(51-an(ac(iter)))/51),se=F)+
  geom_vline(aes(xintercept=c(gt(iter(eq,1)))),col="red")+
  geom_vline(aes(xintercept=c(gt(iter(eq,1)))*0.5),col="orange")+
  xlab("Time to recovery")+ylab(expression(SSB/B[MSY]))+
  scale_x_continuous(limits=c(0,30))
  

data(wklife)

dat=dlply(wklife[-4,], .(stock), with, {
  print(stock[1])
  
  par=lhPar(FLPar(a=a,b=b,linf=linf,k=k,t0=t0,lmax=lmax,l50=l50,a50=a50))
  
  print(0)
  eq      =lhEql(par)
  print(1)
  fbar(eq)=FLQuant(rep(1,50))
  print(3)
  fbar(eq)=propagate(fbar(eq),51)%*%FLQuant(ftar,dimnames=list(iter=seq(51)))
  stk     =as(eq,"FLStock")
  print(4)
  stk     =try(fwd(stk,f=fbar(eq)[,-1]*0,sr=eq))
  print(5)
  
  if("FLStock"%in%is(stk)) return(list(ssb(stk)%/%refpts(eq)["msy","ssb"],gt(iter(eq,1))))
  
  return(NULL)})

dt1=ldply(dat,function(x) as.data.frame(x[[1]],drop=T))
dt2=ddply(dt1,.(stock,iter), with, year[(data-1)^2==min((data-1)^2)][1])

ggplot(dt2)+
  geom_smooth(aes(data,(51-an(ac(iter)))/51,V1,group=stock),se=F)+
  geom_vline(aes(xintercept=c(gt(iter(eq,1)))),col="red")+
  geom_vline(aes(xintercept=c(gt(iter(eq,1)))*0.5),col="orange")+
  xlab("Time to recovery")+ylab(expression(SSB/B[MSY]))+
  scale_x_continuous(limits=c(0,30))
}


