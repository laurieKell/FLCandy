haupt<-function(lfd,pars){
  lc=FLife::vonB(pars["sel1"],pars)
  
  lfd=lfd[dimnames(l)$len>c(lc)]
  l  =lfd
  l[]=as.numeric(dimnames(l)[[1]])

  l1  =lfd[-dim(lfd)[1]]
  l1[]=as.numeric(dimnames(l1)[[1]])

  l2  =lfd[-1]
  l2[]=as.numeric(dimnames(l2)[[1]])

  dt=-log(1-l2%/%pars["linf"])/pars["k"]+log(1-l1/pars["linf"])/pars["k"]
  t =pars["t0"]*log(1-(l/pars["linf"]))/pars["k"]  
  
  dat=model.frame(FLQuants("y"=log(lfd[-dim(t)[1]]%/%dt),
                           "x"=      t[-dim(t)[1]]),drop=T)
  dat=subset(dat,is.finite(y))
  z  =ddply(dat,.(year,iter), with, data.frame(data=-lm(y~x)$coefficients["x"]))
  z  =transform(z,year=factor(year),iter=factor(iter))
  as.FLQuant(z)}

if (FALSE){
require(ggplotFL)
require(ggpubr)

load("/home/laurence-kell/Desktop/papers/COM3/R/runs/om/om.18.RData")
load("/home/laurence-kell/Desktop/papers/COM3/R/runs/indicators/lfd.18.RData")
load("/home/laurence-kell/Desktop/papers/COM3/data/lhs.RData")

z=haupt(lfdc,lhs[[3]])
  
plot(z)
}