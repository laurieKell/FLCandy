# data.R - DESC
# /data.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(ss3om)

alb <- readFLSss3("sa", range=c(minfbar=1, maxfbar=12))

sr <- readFLSRss3("sa")

# SET mat to NA

mat(alb)[,,,-1] <- NA

# LOAD correct deviances

library(ss3om)

out <- readOutputss3("sa")

devts <- FLQuant(DT(out$recruit)[era %in% c("Main", "Late"),
  exp(log(pred_recr) - log(exp_recr))], dimnames=list(age=0, year=1975:2017))

devs <- window(expand(devts, unit=c("F", "M"), season=1:4), start=1950, end=2017)

# Zero for years 1950-1974
devs[, ac(c(1950:1974, 2017))] <- 1

# Zero for seasons 2-4
devs[,,,-1] <- 0

# REC mean = rec(alb)

pars <- FLPar(NA, dimnames=list(params='a', year=1951:2017, unit=1:2, season=1:4, iter=1))
pars['a', , 1, 1]  <- c(rec(alb)[, -1, 1, 1, drop=TRUE] )
pars['a', , 2, 1]  <- c(rec(alb)[, -1, 1, 1, drop=TRUE] )

rec <- predictModel(model=rec~a, params=pars)

pars <- FLPar(NA, dimnames=list(params=c("s", "R0", "v", "sratio"),
  season=1:4, iter=1))
pars[,1] <- c(params(sr))

srr <- predictModel(model=model(sr), params=pars)

save(alb, srr, rec, devs, file="alb.RData", compress="xz")
