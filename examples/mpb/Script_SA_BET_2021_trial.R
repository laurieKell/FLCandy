# ---
# title: Atlantic bigeye stock assessment
# subtitle: Biomass dynamic model, biodyn
# author: "Gorka Merino"
# date: "10/06/2021"
# R: 4.0.4 (64bits)
# ---
## ----init------------------------------------------------------------
# Libraries:
rm(list=ls())
library(reshape)
library(plyr)
library(FLCore)
library(ggplotFL)
library(diags)
library(kobe)
library(mpb)
library(corrplot)
library(plotrix)
library(gam)
library(tidyr)

# -----------------------------------------------------------------------
# 1) Set directories
# -----------------------------------------------------------------------

dirPost= "/home/laurence-kell/Desktop/flr/bugs/mpb"
dirDat  =file.path(dirPost,"/data")
diroutPost= file.path(dirPost,"/output/")
diroutFigs= file.path(dirPost,"/Figures/")

# -----------------------------------------------------------------------
# 2) Upload and check input CPUE series
# -----------------------------------------------------------------------

v=read.csv(paste(dirDat,"data.csv", sep="/" ), sep=";")[,-6]
names(v)=c("name", "year", "index", "cv", "ref")

v=subset(v, name!="catch")

v  =v[!duplicated(v[,c("name","year")]),c("name","year","index")]

## Prettify names
nms=c("Joint_LL_5978", "Joint_LL_7919")
names(nms)=sort(unique(v$name))

nm2=c("Joint_LL_5978", "Joint_LL_7919")

names(nm2)=sort(unique(v$name))

v  =transform(v,name=factor(name,levels=sort(unique(v$name)),labels=nm2))

cpue=FLQuants(dlply(v,.(name),with, 
                    as.FLQuant(data.frame(year=year,data=index))))
names(cpue)=nm2

# Look at the CPUE's -------------------------------------
cpue[[1]]@units="SCRS/2021/052"
cpue[[2]]@units="SCRS/2021/052"

setwd(diroutPost)
save(cpue, file="CPUE.RData")

CPUEFIG=plot(cpue)+ylim(0,2)+ylab("Standardized CPUE")+facet_wrap(~qname)+theme_bw()+xlab("Years")

ggsave(filename = "CPUEs.png", plot = CPUEFIG, path=diroutFigs, width=7, height=4)

# Look at CPUE correlations

cr=cor(cast(v,year~name,value="index")[,-1],
       use="pairwise.complete.obs")
dimnames(cr)=list(names(cpue), names(cpue))
cr[is.na(cr)]=0

setwd(dirPost)

names(cpue)=nms
cc=mdply(expand.grid(a=names(cpue),b=names(cpue)),
         function(a,b){
           #print(paste(a,b))
           res=model.frame(mcf(FLQuants(cpue[c(a,b)])))
           res=subset(res,!is.na(res[,7])&!is.na(res[,8]))
           
           if (dim(res)[1]>10){
             res=data.frame(lag=-10:10,data=ccf(res[,7],res[,8],plot=F,
                                                lag.max=10)$acf)
             return(res)}else{return(NULL)}}
)

# -----------------------------------------------------------------------
# 3) Do Stock Assessment with biodyn
# -----------------------------------------------------------------------

setwd(dirDat)

catch=subset(read.csv(paste(dirDat,"data.csv", sep="/" ), sep=";"), fleet=="catch")[,c(2,3)]
names(catch)=c("year", "data")

catch=as.FLQuant(catch)
catch=window(catch, end=2019) # Terminal year 2019 acording to the Data Prep report.
# catch=window(catch, start=1975) # Terminal year 2019 acording to the Data Prep report.

load(paste(diroutPost, "CPUE.RData", sep="/"))

# Create a first bd object with approx values:

bd=biodyn("pellat", params=FLPar(r=0.3,k=5.5e5,b0=1,p=0.001),
          catch = catch)

# Create the list of objects that inlcudes the Base CAse (with both indices) 
# and Sensitivity with only the late index.

bds2=mpb::biodyns(list("BC: JointLL (early and late)"=bd,
                        "Sens 1: Joint LL (late only)"=bd))

params(bds2[[1]])=FLPar(r=0.23, k=1200000, b0=.95, p=0.001)
setParams(bds2[[1]])=cpue[1:2]
setControl(bds2[[1]])=params(bds2[[1]])
bds2[[1]]=fit(bds2[[1]],cpue[1:2])

params(bds2[[2]])=FLPar(r=0.15, k=1200000, b0=.95, p=0.001)
catch(bds2[[2]])=catch
setParams(bds2[[2]])=cpue[2]
setControl(bds2[[2]])=params(bds2[[2]])
bds2[[2]]=fit(bds2[[2]],cpue[2])

plot(bds2)+theme(legend.position="bottom")

bds2=mpb::biodyns(list("BC: JointLL (early and late)"=bd,
                        "Sens 1: Joint LL (late only)"=bd))

params(bds2[[1]])=FLPar(r=0.15, k=1200000, b0=.95, p=0.001)
catch(bds2[[1]])=catch
setParams(bds2[[1]])=cpue[1:2]
setControl(bds2[[1]])=params(bds2[[1]])
bds2[[1]]=fit(bds2[[1]],cpue[1:2])

params(bds2[[2]])=FLPar(r=0.15, k=1200000, b0=.95, p=0.001)
catch(bds2[[2]])=catch
setParams(bds2[[2]])=cpue[2]
setControl(bds2[[2]])=params(bds2[[2]])
bds2[[2]]=fit(bds2[[2]],cpue[2])

plot(bds2)+theme(legend.position="bottom")

# Relative trajectories

bd=biodyn("pellat", params=FLPar(r=0.3,k=5.5e5,b0=1,p=0.001),
          catch = catch)

# Create the list of objects that inlcudes the Base CAse (with both indices) 
# and Sensitivity with only the late index.

bd=biodyn("pellat", params=FLPar(r=0.125,k=1.2e6,b0=0.95,p=0.001),catch=catch)

bds2=mpb::biodyns(list("BC: JointLL (early and late)"=bd,
                        "Sens 1: Joint LL (late only)"=bd))

params(bds2[[1]])=FLPar(r=0.15, k=1.2e6, b0=.95, p=0.001)
catch(bds2[[1]])=catch
setParams(bds2[[1]])=cpue[1:2]
setControl(bds2[[1]])=params(bds2[[1]])
bds2[[1]]=fit(bds2[[1]],cpue[1:2])

params(bds2[[2]])=FLPar(r=0.15, k=1.2e6, b0=.95, p=0.001)
catch(bds2[[2]])=catch
setParams(bds2[[2]])=cpue[2]
setControl(bds2[[2]])=params(bds2[[2]])
bds2[[2]]=fit(bds2[[2]],cpue[2])

plot(bds2)+theme(legend.position="bottom")