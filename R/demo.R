# metrics_FLR.R - DESC
# /metrics_FLR.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

library(mse)
library(FLBRP)

source("functions.R")

# DATA

data(ple4brp)
data(ple4)

# DEFINE statistics

stats <- list(
  MTC=~apply(C, c(1,3:6), median, na.rm=TRUE),
  MSS=~apply(B, c(1,3:6), median, na.rm=TRUE),
  PSBMSY=~yearMeans((SB / SBMSY) > 1)
  )

# 

compute(metrics(ple4, metrics=list(C=catch, F=fbar, SB=ssb, B=stock)),
  statistics=stats, refpts=FLPar(SBMSY=7.3e5))

#

compute(metrics(ple4, metrics=list(C=catch, F=fbar, SB=ssb, B=stock)),
  statistics=stats, refpts=convert(refpts(ple4brp)))

# STANDARD metrics in compute()

# - FLStock: C=catch, L=landings, D=discards, F=fbar, SB=ssb, B=stock, ...

# DEFAULT map in convert()

# - FMSY=c("msy", "harvest")
# - SBMSY=c("msy", "ssb"))
# - MSY=c("msy", "yield")



