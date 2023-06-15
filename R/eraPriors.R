---
title: "PSA priors"
author: "L Kell"
date: "`r format(Sys.time(), '%d %B, %Y')`"
output: pdf_document
mathjax: TRUE
tags: FLR priors
license: Creative Commons Attribution-ShareAlike 4.0 International Public License
# bibliography: refs.bib
knit: (function(inputFile, encoding) {
  rmarkdown::render(inputFile, encoding = encoding, output_dir = "pdf") })
---
  
  
  
  library(ggplot2)

library(FishLife)
library(SPMpriors)
library(FLCore)
library(ggplotFL)
library(FLRef)

library(FLife)
library(ggpubr)

library(plyr)


## i) Get parameters from FishBase via FishLife & SPMpriors
Genus="Gadus";Species="morhua"

taxa  =FishLife::Search_species(Genus=Genus,Species = Species,add_ancestors=TRUE)$match_taxonomy
predfl=FishLife::Plot_taxa(taxa,mfrow=c(3,2))

#plot(  princomp(cov[,,1,drop=T]))
#biplot(princomp(cov[,,1,drop=T]))

mu =predfl[[1]]$Mean_pred
mu =mu[ c("Loo","K","Lm","h")]
names(mu)=c("linf","k","l50","s")
mu =FLPar(mu)

cov=predfl[[1]]$Cov_pred
cov=cov[c("Loo","K","Lm","h"),c("Loo","K","Lm","h")]
cov=as(array(cov,c(4,4,1),dimnames=list(params=c("linf","k","l50","s"),params=c("linf","k","l50","s"),iter=1)),"FLPar")


## ii) simulate multinormal distribution
set.seed(12234)
par=mvtnorm::rmvnorm(100,mu[,1,drop=T],cov[,,1,drop=T])
par=as(t(par),"FLPar")
dimnames(par)=list(params=c("linf","k","l50","s"),iter=seq(100))
par[c("linf","k","l50")]=exp(par[c("linf","k","l50")])

## iii) Fill missing with default parameters
par2=par
lh=lhPar(par)

## iv) natural mortality
dat=c(m1= 0.28, m1=(0.48- 0.07)/(2 * 1.96),
      m2=-1.30, m2=(- 1.19 +1.42)/(2 * 1.96),
      m3= 1.08, m3=(1.24 - 0.92)/(2 * 1.96))
mPar=FLPar(dat[seq(1,6,2)])
mSD =FLPar(dat[seq(2,6,2)])

set.seed(8899)
mPar=rbind(rnorm(100,mPar["m1"],mSD["m1"]),
           rnorm(100,mPar["m2"],mSD["m2"]),
           rnorm(100,mPar["m3"],mSD["m3"]))

lh=rbind(lh[!dimnames(lh)$params%in%c("m1","m2","m3")],mPar)

## v) Create FLBRP
mFn=function(object,par) {
   len=wt2len(stock.wt(object),par)

   #lnM= a + b ln(L/L∞) + c*lnK
   
   res= exp(par["m1"]%+%(par["m2"]%*%log(len/par["linf"]))%+%(par["m3"]%*%log(par["k"])))
   
   res}
eq =lhEql(lh, m=mFn)


## vi) Get FMSY
ggdensity(x="data", data=as.data.frame(refpts(eq)["msy","harvest"]))

