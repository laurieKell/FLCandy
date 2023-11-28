require(FLCore)
require(FLBRP)
require(ggplotFL)

#' i.e. i) calc ref age, ii) calc P( N ) at FMSY, iii) calc P obs, iv) calc ratio of iii & iii


ABI<-function(x,y=FLBRP(x),ref="msy",p=0.9){
  
  ## Set up numbers-at-age at equilibrium for MST
  fbar(y)=as.FLQuant(computeRefpts(y)[ref,"harvest",drop=T],dimnames=list(iter=seq(dims(y)$iter)))
  
  ## Find age at 90% 
  stk.n=propagate(stock.n(y)[-1],dim(x)[6])
  cumN =apply(stk.n,c(2,6),cumsum)%/%quantSums(stk.n)
  ages =ages(stk.n)
  
  ages[cumN<=p]=NA
  A=apply(ages,c(2:6),min,na.rm=T)+1
  
  ## Find proportion > Amsy
  flag =FLQuant(ages(stk.n)>=expand(A,age=dimnames(stk.n)$age))
  flag.x=expand(flag,year=dimnames(stock.n(x)[-1])$year,fill=T)
  flag.y=FLQuant(ages(stk.n)>=expand(A,age=dimnames(stk.n)$age))
  
  res.x=apply(stock.n(x)[-1]%*%flag.x,c(2,6),sum)%/%apply(stock.n(x)[-1],c(2,6),sum)
  res.y=apply(stock.n(y)[-1]%*%flag.y,c(2,6),sum)%/%apply(stock.n(y)[-1],c(2,6),sum)
  
  res.x%/%res.y}


#{{{
#' ABItgt()
#
#' Computes ABI for target F, e.g. ABImsy (Griffith et al. 2023)
#'
#' @param stock object of class FLStock 
#' @param ftgt target F at equilibrium, e.g. Fmsy
#' @param thresh quantile ageref treshold, default 0.9
#' @return FLQuant 
#' @export
#' @examples
#' data(ple4)
#' ABImsy = ABItgt(ple4,ftgt=0.22,thresh=0.9)
#' plot(ABImsy)+ylim(0,2)+
#'  geom_hline(yintercept = 1)+ylab(expression(ABI[MSY]))

ABItgt <- function(stock,ftgt=0.2,thresh=0.9){
  eqstk = brp(FLBRP(stock))
  fbar(eqstk)[,1][] = 0.00001 # compute for eq Fmsy
  fbar(eqstk)[,1:101][] = ftgt # compute for equilibrium Fmsy
  eqstk = brp(eqstk)
  eqstk = window(as(eqstk, "FLStock"),start=2,end=2) # year1 F=0, year2=Fmsy
  eqstk@name = stock@name # name stk
  n_a = stock.n(eqstk)[-1,] # remove first age
  ages = dims(n_a)$min:dims(n_a)$max
  cums = apply(n_a,2:6,cumsum)
  n_thresh = sum(n_a*thresh)
  aref = min(ages[which((n_thresh-cums)^2==min((n_thresh-cums)^2))]+1,range(eqstk)["plusgroup"]-1)
  rp = sum(n_a[ac(aref:max(ages)),])/sum(n_a) # ref proportion
  flq= quantSums(stock.n(stock)[ac(aref:range(stock)[2]),])/quantSums(stock.n(stock)[-1,])/rp
  
  return(flq)}

if(FALSE){
  setwd("~/Desktop/inPrep/pew/github/erp/Rmd")
  
  load("../data/inputs/ices/mac.RData")
  
  source("~/Desktop/flr/FLCandy/R/nonStationarity.R")
  
  mac=iter(mac,1)
  
  sr =fmle(as.FLSR(mac,model="bevholt"),control=list(silent=TRUE))
  eq=FLBRP(mac,sr=sr,nyear=dim(mac)[2])
  
  plot(FLQuants("BBMSY"=ssb(mac)%/%computeRefpts(eq)["msy","ssb"],
                "Laurie"=ABI(mac,eq,ref="msy"),
                "Henning"=ABItgt(mac,computeRefpts(eq)["msy","harvest"])))+
  geom_hline(aes(yintercept=1),col="red",linetype=2)
  
  rfs=nonStationarity(mac,sr)
  
  ggplot(rbind(subset(rfs,refpt%in%c("virgin","msy")&(quant!="harvest")),
               subset(rfs,refpt%in%c("crash","msy")&(quant=="harvest"))))+
    geom_line(aes(year,data,col=factor(refpt,levels=c("crash","msy","virgin"),
                                       labels=c("Crash","MSY","Virgin"))))+
    facet_grid(quant~.,scale="free")+
    scale_color_manual("",values=rainbow(3))+
    xlab("Year")+ylab("")
}


## gets reference age
ABIAge<-function(y,ref="msy",p=0.9){
  
  ## Set up numbers-at-age at equilibrium for MST
  fbar(y)=as.FLQuant(computeRefpts(y)[ref,"harvest",drop=T],dimnames=list(iter=seq(dim(y)[6])))
  
  ## Find age at 90% 
  stk.n=stock.n(y)[-1]
  cumN =apply(stk.n,c(2,6),cumsum)%/%quantSums(stk.n)
  ages =ages(stk.n)
  
  ages[cumN<=p]=NA
  apply(ages,c(2:6),function(x) min(c(x+1,dims(y)$max),na.rm=T))}

## Gets P>ref age for FLBRP
ABIMSY<-function(y,ref="msy",p=0.9){
  
  fbar(y)=as.FLQuant(computeRefpts(y)[ref,"harvest",drop=T],dimnames=list(iter=seq(dim(y)[6])))
  A=ABIAge(y,ref,p)
  
  stk.n=stock.n(y)[-1]
  
  ## Find proportion > Amsy
  flag=FLQuant(ages(stk.n)>=FLCore:::expand(A,age=dimnames(stk.n)$age))
  
  apply(stk.n%*%flag,c(2,6),sum)%/%apply(stk.n,c(2,6),sum)}

## Gets P>ref age for stock
ABIstock<-function(x,A){
  
  stk.n=stock.n(x)[-1]
  
  if (dim(x)[6]>1&dim(A)[6]==1)
     A=propagate(A,dim(x)[6])

  ## Find proportion > Amsy
  flag =FLQuant(ages(stk.n)>=FLCore:::expand(A,age=dimnames(stk.n)$age,year=dimnames(stk.n)$year,fill=T))
  
  apply(stk.n%*%flag,c(2,6),sum)%/%apply(stk.n,c(2,6),sum)}

ABI<-function(x,y,ref="msy",p=0.9){
  A   =ABIAge(y,ref,p)
  pmsy=ABIMSY(y,ref,p)
  pt  =ABIstock(x,A)
  pt%/%pmsy}


if(FALSE){
  library(FLCore)
  library(FLBRP)
  
  data(ple4)
  data(ple4brp)

  #' calc ref age
  A   =ABIAge(ple4brp)
  
  #' calc P( N) at FMSY, 
  pmsy=ABIMSY(ple4brp)
  
  #' calc P obs
  pt  =ABIstock(ple4,A)
  
  #' calc ratio 
  pt%/%pmsy}

