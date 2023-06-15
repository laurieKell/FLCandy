# library(devtools)
# devtools::install_github("jabbamodel/JABBA")

library(JABBA)
library(FLCore)
library(FLBRP)
library(ggplotFL)
library(ggplot2)
library(kobe)
library(plyr)

# load complete Pollack OM        
load("/home/laurie/pCloudDrive/flr/FLCandy/data/omPollack2.RData")

# priors
msy  =an(refpts(eq)["msy","yield"])
fmsy =an(refpts(eq)["msy","harvest"])
bmsy =an(refpts(eq)["msy","ssb"])

b0   =an(refpts(eq)["virgin","ssb"])
shape=bmsy/b0
mi   =seq(0.01,2,0.001) 
m    =(mi^(-1/(mi-1))-shape)^2
m    =mi[m==min(m)]

r    =(1-exp(-fmsy))*(m-1)/(1-m^-1)

# Haupt function
haupt<-function(lfd,pars,lc=40,lmax=0.8*pars[[1]]){
  lfd=lfd[as.numeric(dimnames(lfd)$len)>c(lc) & as.numeric(dimnames(lfd)$len)<lmax]
  l  =lfd
  l[]=as.numeric(dimnames(l)[[1]])
  
  l1  =lfd[-dim(lfd)[1]]
  l1[]=as.numeric(dimnames(l1)[[1]])
  
  l2  =lfd[-1]
  l2[]=as.numeric(dimnames(l2)[[1]])
  
  dt=-log(1-l2%/%pars["linf"])/pars["k"]+log(1-l1/pars["linf"])/pars["k"]
  t =pars["t0"]*log(1-(l/pars["linf"]))/pars["k"]  
  
  dat=model.frame(FLQuants("y"=log(lfd[-dim(t)[1]]%/%dt),
                           "x"=      t[-dim(t)[1]]))
  dat=subset(dat,is.finite(y))
  yrs=an(dimnames(lfd)[[2]])
  
 
  z  =ddply(dat,.(year,iter), with, data.frame(data=-lm(y~x,na.rm=TRUE)$coefficients["x"]))
  z  =transform(z,year=factor(year),iter=factor(iter))
  as.FLQuant(z)
  
}

# Compute Z 
Z = haupt(lfd,pars=lh,lc=an(lh["l50"]),lmax=0.8*lh[[1]])
Zom = quantMeans(z(om)[ac(3:39),])
quant(Z) = "age"
plot(Z,Zom)

# Transform to rate
Z = 1-exp(-Z)

# Prepare df (DATA-Rich)
set.seed(123)
it = 1
obs.yrs = an(dimnames(Z)$year) 
c.yrs = 45:max(obs.yrs)
stk = iter(om,it)
Catch = data.frame(as.data.frame(catch(stk)[,ac(c.yrs)]),qname="catch")
z = data.frame(as.data.frame(iter(Z[,ac(obs.yrs)],it)),qname="z")
Index = data.frame(as.data.frame(vb(stk)[,ac(obs.yrs)]*ar1rlnorm(rho=0,years=obs.yrs,sdlog=0.2)),qname="index")

df = rbind(Catch,Index,z)
inp = reshape2::dcast(df,year~qname,value.var="data",fun.aggregate=sum)

# JABBA data.frames
cpue = inp[,c("year","index")]
cpue[cpue==0] = NA
cpue$index = cpue$index/mean(cpue$index,na.rm=T)
catch = inp[,c("year","catch")]
z = inp[,c("year","z")]
z[z==0] = NA

# Fit with Catch + Index: Simple Fox with r = Fmsy
jbI= build_jabba(catch=catch,cpue=cpue,
                  model.type = "Fox",scenario="DR-Index",
                  r.prior = c(fmsy,0.3),
                  verbose=F,
                  psi.prior=c(0.9,0.3))
fI= fit_jabba(jbI,quickmcmc = T,verbose=F)

jbIZ= build_jabba(catch=catch,cpue=cpue,auxiliary = z,
                  model.type = "Fox",scenario="DR-Index+Z",
                  r.prior = c(fmsy,0.3),
                  verbose=F,
                  auxiliary.sigma = TRUE, # Here estimated
                  auxiliary.obsE =0.1, # lag effect between impact and Z pop structure
                  auxiliary.lag = 5, # lag effect between impact and Z pop structure
                  auxiliary.type = "z",
                  psi.prior=c(0.9,0.3))


fIZ= fit_jabba(jbIZ,quickmcmc = T,verbose=F)

# com
jbCZ= build_jabba(catch=catch,cpue=NULL,auxiliary=z,
                  model.type = "Fox",scenario="DR-Com+Z",
                  r.prior = c(fmsy,0.3),
                  verbose=F,
                  auxiliary.sigma = TRUE, # Can be estimated with long time series
                  auxiliary.obsE = 0.1, 
                  auxiliary.lag = 5, # lag effect between impact and Z pop structure
                  auxiliary.type = "z",
                  psi.prior=c(0.9,0.3))


fCZ= fit_jabba(jbCZ,quickmcmc = T,verbose=F)

# DATA RICH
jbpar(mfrow=c(2,2))
jbplot_ensemble(list(fI,fIZ,fCZ),subplots = 1,add=T,plotCIs = T)
lines(data~year,as.data.frame(ssb(stk)/bmsy),lwd=2,lty=2)
jbplot_ensemble(list(fI,fIZ,fCZ),subplots = 2,add=T)
lines(data~year,as.data.frame(fbar(stk)/fmsy),lwd=2,lty=2)
jbplot_ensemble(list(fI,fIZ,fCZ),subplots = 5,add=T)
jbplot_ensemble(list(fI,fIZ,fCZ),subplots = 6,add=T)
mtext("DATA-RICH",outer=T)

# Check Fits
jbplot_cpuefits(fI)
jbplot_cpuefits(fIZ)
jbplot_cpuefits(fCZ)

## DATA-MODERATE
obs.yrs = 80:115
yrs = 70:115
catch1 = catch[catch$year%in%yrs,]
cpue1 = cpue[cpue$year%in%yrs,]
z1 =  z[z$year%in%yrs,]
cpue1$index[!cpue1$year%in%obs.yrs] = NA
z1$z[!z1$year%in%obs.yrs] = NA


# Fit with Catch + Index: Simple Fox with r = Fmsy
jbI1= build_jabba(catch=catch1,cpue=cpue1,
                 model.type = "Fox",scenario="DM-Index",
                 r.prior = c(fmsy,0.3),
                 verbose=F,
                 psi.prior=c(0.75,0.3))
fI1= fit_jabba(jbI1,quickmcmc = T,verbose=F)

jbIZ1= build_jabba(catch=catch1,cpue=cpue1,auxiliary = z1,
                  model.type = "Fox",scenario="DM-Index+Z",
                  r.prior = c(fmsy,0.3),
                  verbose=F,
                  auxiliary.sigma = TRUE,
                  auxiliary.lag = 5, # lag effect between impact and Z pop structure
                  auxiliary.type = "z",
                  psi.prior=c(0.75,0.3))


fIZ1= fit_jabba(jbIZ1,quickmcmc = T,verbose=F)

# com
jbCZ1= build_jabba(catch=catch1,cpue=NULL,auxiliary = z1,
                  model.type = "Fox",scenario="DM-Com+Z",
                  r.prior = c(fmsy,0.3),
                  verbose=F,
                  auxiliary.sigma = FALSE, 
                  auxiliary.obsE = 0.1, # Needs high precision
                  auxiliary.lag = 5, # lag effect between impact and Z pop structure
                  auxiliary.type = "z",
                  psi.prior=c(0.75,0.3))


fCZ1= fit_jabba(jbCZ1,quickmcmc = T,verbose=F)

jbplot_ppdist(fCZ1)

# DATA MODERATE
jbpar(mfrow=c(2,2))
jbplot_ensemble(list(fI1,fIZ1,fCZ1),subplots = 1,add=T,plotCIs = T)
lines(data~year,as.data.frame(ssb(stk)/bmsy),lwd=2,lty=2)
jbplot_ensemble(list(fI1,fIZ1,fCZ1),subplots = 2,add=T)
lines(data~year,as.data.frame(fbar(stk)/fmsy),lwd=2,lty=2)
jbplot_ensemble(list(fI1,fIZ1,fCZ1),subplots = 5,add=T)
jbplot_ensemble(list(fI1,fIZ1,fCZ1),subplots = 6,add=T)
mtext("DATA-MODERATE",outer=T)

# Check Fits
jbplot_cpuefits(fI1)
jbplot_cpuefits(fIZ1)
jbplot_cpuefits(fCZ1)

## Project 15 years ahead
prjIZ = fw_jabba(fIZ1,quant="F",type="ratio",imp.yr =2,
                stochastic = T,imp.values=1,nyears = 10,AR1=T )

jbpar(mfrow=c(2,2))
jbplot_ensemble(prjIZ,subplots = 1,add=T)
lines(data~year,as.data.frame(ssb(stk)/bmsy),lwd=2,lty=2)
jbplot_ensemble(prjIZ,subplots = 2,add=T)
lines(data~year,as.data.frame(fbar(stk)/fmsy),lwd=2,lty=2)
jbplot_ensemble(prjIZ,subplots = 5,add=T)
jbplot_ensemble(prjIZ,subplots = 6,add=T)
lines(data~year,as.data.frame(catch(stk)),lwd=2,lty=2)


kbs = rbind(fI$kbtrj,fIZ$kbtrj,fCZ$kbtrj)

# DATA-Rich - Year 120
kobe:::kobePhaseMar2(subset(kbs,kbs$year==120,c(stock,harvest,run)),
               ylim=2.5,xlim=2.5,
               xlab=expression(B/B[MSY]),
               ylab=expression(F/F[MSY]),
               quadcol=c("red","green","yellow","orange"))

kbs1 = rbind(fI1$kbtrj,fIZ1$kbtrj,fCZ1$kbtrj)

kobe:::kobePhaseMar2(subset(kbs1,kbs1$year==115,c(stock,harvest,run)),
                     ylim=2.5,xlim=4,
                     xlab=expression(B/B[MSY]),
                     ylab=expression(F/F[MSY]),
                     quadcol=c("red","green","yellow","orange"))
