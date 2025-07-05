# alb.R - DESC
# /alb.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


# IOTC albacore: SS3 3.30, 2 sex, 4 seasons, spawning season 1.


load_all('~/Projects/FLR/pkgs/mine/FLasher')

# LOAD

load("alb.RData")

# FUT

fut <- alb

stock.n(fut)[, -1] <- NA
harvest(fut)[, -1] <- catch.sel(fut)[, -1] 


# --- 1. HINDCAST based on F

control <- as(FLQuants(fbar=unitMeans(fbar(alb)[,-1])), 'fwdControl')

futf <- fwd(fut, control=control, sr=rec)

plot(futf, alb)

range(ssb(alb[,,1,1]) / ssb(futf[,,1,1]))


# --- 2. HINDCAST based on F with SRR + deviances

futfr <- fwd(fut, control=control, sr=srr, deviances=devs)

# RANGES

range(rec(alb) / rec(futfr))

range(ssb(alb[,,1,1]) / ssb(futfr[,,1,1]))

plot(alb, futfr)


# --- 3. HINDCAST based on catch

control <- as(FLQuants(catch=unitSums(catch(alb)[,-1])), 'fwdControl')

futc <- fwd(fut, control=control, sr=srr, deviances=devs)

plot(futc, alb)

range(ssb(alb[,,1,1]) / ssb(futc[,,1,1]))


# --- 4. HINDCAST yearly model with SRR + deviances

fut <- simplify(alb, "season")

control <- as(FLQuants(fbar=unitMeans(fbar(fut)[,-1])), 'fwdControl')

params(srr) <- params(srr)[,1]

futfy <- fwd(fut, control=control, sr=srr, deviances=devs[,,,1])

# RANGES

range(rec(fut) / rec(futfy))

range(ssb(alb[,,1,1]) / ssb(futfy[,,1,1]))

plot(FLStocks(SIMP=fut, FWD=futfy))


