library(msy)
library(xtable)
library(FLCore)
library(ggplotFL)
library(ggplot2)
library(ss3diags)
library(gridExtra)

load("~/Desktop/rfmo/ices/wkref/data/ices.stks28.rdata")


i=1
stk=stks[[i]]

eqsim<-function(stk){
    
  ###Set units
  stock(stk)              =computeStock(stk)
  units(catch(stk))       ="tonnes"
  units(catch.n(stk))     ="1000"
  units(catch.wt(stk))    ="kg"
  units(discards(stk))    ="tonnes"
  units(discards.n(stk))  ="1000"
  units(discards.wt(stk)) ="kg"
  units(landings(stk))    ="tonnes"
  units(landings.n(stk))  ="1000"
  units(landings.wt(stk)) ="kg"
  units(stock(stk))       ="tonnes"
  units(stock.n(stk))     ="1000"
  units(stock.wt(stk))    ="kg"
  units(m(stk))           ="NA"
  units(mat(stk))         ="NA"
  units(harvest(stk))     ="f"
  units(harvest.spwn(stk))="NA"
  units(m.spwn(stk))      ="NA"
  
  
    if (any(is.na(stk@discards.n))) {
      stk@discards.n[] = 0.001
      stk@discards.wt[] = stock.wt(stk)
      stk@discards[] = 1e-04}
  
  
    ###Set reference points
    Fmsy    =attributes(stk)$frp$Fmsy
    Blim    =attributes(stk)$frp$Blim
    Bpa     =attributes(stk)$frp$Bpa
    Btrigger=attributes(stk)$frp$Btrigger
    
    ####Create the Hockey stick SR with Blim
    #stk = trim(stk, year=c(range(stk)["minyear"]:2019))
    #segreg3  = function(ab, ssb) log(ifelse(ssb >= Blim, ab$a * Blim, ab$a * ssb))
    
    #Fit the SR data using only segreg model to estimate Blim
    #FIT = eqsr_fit(stk, nsamp = 1000, models = c("segreg3"))
    FIT = eqsr_fit(stk, nsamp = 1000, models = c("Segreg"))
    #FIT = eqsr_fit(stk, nsamp = 1000, models = c("Bevholt"))
    
    BlimEqsim  =FIT[["sr.det"]][,"b"]
    BpaEqsim   =BlimEqsim*exp(1.645*0.2)
    BtriggerEqsim = BpaEqsim
    
    #Fit the SR data with the 3 models combined 
    #FIT = eqsr_fit(stk, nsamp = 1000, models = c("Bevholt", "Ricker","Segreg"))
    #FIT = eqsr_fit(Herring, nsamp = 1000, models = c("Bevholt"))
    
    #Define the F range to which run the simulations
    Fscan = seq(0,2,len=60)
    
    #Setting the the biology and selectivity
    bio.years = c(range(stk)["maxyear"]-2,range(stk)["maxyear"])
    sel.years = c(range(stk)["maxyear"]-2,range(stk)["maxyear"])
    
    ##STEP 1 with Segmented Regression with breakpoint at Blim
    SIM = eqsim_run(FIT, bio.years=bio.years, bio.const=FALSE, sel.years=sel.years, sel.const=FALSE, Fcv=0.212, Fphi=0.423, Blim=BlimEqsim, Bpa=Btrigger, Fscan = Fscan, verbose=TRUE, process.error=TRUE, Nrun=200, Btrigger=Btrigger, rhologRec=TRUE, SSBcv=0, extreme.trim=c(0.05,0.95))
    
    #Create a table with the reference points 
    #t(SIM$refs_interval)
    
    #Estimate the catch at the equilibrium for a given F
    SIM$rbp$p50[SIM$rbp$variable=="Catch"]
    
    ##Interpolate to get equilbrium catches
    Catch_50perc = approx(Fscan, SIM$rbp$p50[SIM$rbp$variable=="Catch"], xout=seq(min(Fscan),max(Fscan),length=40))
    
    ##The following matrix now gives you the 50th percentile for a whole long list of F values from which then you can extract the one corresponding to Fmsy
    
    Catchequi = Catch_50perc$y[which.min(abs(Catch_50perc$x - Fmsy))]
    ## where Fmsy is the preliminary Fmsy value identified in Step 1 above
    
    ##The following lines gives you the 50th percentile of the long-term equilibrium distribution of SSB (BMSY) when fishing at each of the values F in the vector Fscan (the vector of F values you used to run EqSim):
    SIM$rbp$p50[SIM$rbp$variable=="Spawning stock biomass"]
    
    ##Interpolate to get this for more F values:
    BF_50perc1 = approx(Fscan, SIM$rbp$p50[SIM$rbp$variable=="Spawning stock biomass"], xout=seq(min(Fscan),max(Fscan),length=1000))
    
    ##The following matrix now gives you the 50th percentile for a whole long list of F values from which then you can extract the one corresponding to Fmsy and F0
    
    #Estimate B0 and BMSY
    F.0 =0
    
    c(Bmsy     =BF_50perc1$y[which.min(abs(BF_50perc1$x-Fmsy))],
      B0       =BF_50perc1$y[which.min(abs(BF_50perc1$x-F.0))],
      Flim     =BF_50perc1$x[which.min(abs(BF_50perc1$y-Blim))],
      Blim     =BlimEqsim,
      Bpa      =BpaEqsim,
      Btrigger =BtriggerEqsim,
      Catchequi=Catchequi,
      unlist(c(SIM$refs_interval["FmsyMedianC"])),
      unlist(c(SIM$refs_interval["FmsyMedianL"])),
      unlist(c(SIM$refs_interval["F5percRiskBlim"])))}


ldply(stks, eqsim)
