library(r4ss)
library(TAF)
library(ss3om)
library(data.table)
library(ggplot2)

library(foreach)
library(doParallel)

setwd("P:/rfmo/ices/WKBSEABASS/north")

ncore=detectCores()-4  
registerDoParallel(ncore)

# M values
ms  =seq(0.125, 0.50, by=0.025)
ctrl="BassVIIIab_3.30_control.ss"

mruns=foreach(m=setNames(ms, nm=paste0('m', ms)), .packages=c("TAF","r4ss")) %dopar% {

  path <- paste0("M/", m)

  mkdir(path)
  cp("base/*",path)

  parlines=SS_parlines(file.path(path,ctrl))
  
  newm=m

  SS_changepars(dir=path, ctlfile=ctrl, newctlfile=ctrl,linenums=seq(73, 73),newvals=newm)

  r4ss::run(path, exe="ss3", show_in_console=TRUE, skipfinished=FALSE)

#  retro(path, exe="ss3", show_in_console=TRUE)

  rtn=smrySS(path)
  
  return(rtn)}

smry=foreach(m=setNames(ms, nm=paste0('m', ms)), .packages=c("TAF","r4ss","plyr","dplyr")) %dopar% {
  smrySS(paste0("M/", m))}

