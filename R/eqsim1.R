#' @title 
#' 
#' @description 
#'
#' @param object an \code{FLStock} object 
#' @param seasons a numeric with seasons
#' 
#' @aliases
#' 
#' @return \code{FLStock} object
#'
#' @seealso \code{\link{expand}}
#'
#' @export seasonalise
#' @docType methods
#' @rdname seasonalise
#'
#' 
#' @examples
#' \dontrun{
#' }

eqsim1<-function(object, 
                #Blim, Bpa=Blim*exp(1.645*0.2), Btrigger=Bpa, 
                Blim    =c("Blim"=unlist(c(object@frp["Blim"]))),
                Bpa     =unlist(c(object@frp["Bpa"])),
                Btrigger=unlist(c(object@frp["Btrigger"])),
                Fmsy    =unlist(c(object@frp["Fmsy"])),
                Fscan   ="missing", 
                Fcv=0.212,   Fphi=0.423, 
                bio.years=3, bio.const=FALSE, 
                sel.years=3, sel.const=FALSE, 
                verbose=FALSE, process.error=TRUE, Nrun=200, nsamp=1000,
                rhologRec=TRUE, SSBcv=0, 
                extreme.trim=c(0.05,0.95)){
  
  stk=trim(object, year=c(range(object)["minyear"]:range(object)["maxyear"]))
  
  ####Create the Hockey stick SR with Blim
  segreg3<-function(ab, ssb) log(ifelse(ssb >= Blim, ab$a * Blim, ab$a * ssb))
  
  segreg3<<-segreg3
  Blim<<-Blim
  
  #Fit the SR data using only segreg3 model (HS at blim)
  FIT=eqsr_fit(stk, nsamp=nsamp, models = c("segreg3"))
  
  
  #Fit the SR data with the 3 models combined 
  #FIT <- eqsr_fit(stk, nsamp = 1000, models = c("Bevholt", "Ricker","Segreg"))
  #FIT <- eqsr_fit(Herring, nsamp = 1000, models = c("Bevholt"))
  
  #Define the F range to which run the simulations
  if (missing(Fscan))
    Fscan = seq(0,2,len=60)
  
  #Setting the the biology and selectivity
  bio.years=c(range(stk)["maxyear"]-bio.years+1,range(stk)["maxyear"])
  sel.years=c(range(stk)["maxyear"]-sel.years+1,range(stk)["maxyear"])
  
  ##STEP 1 with Segmented Regression with breakpoint at Blim
  SIM=eqsim_run(FIT, 
                bio.years=bio.years, bio.const=bio.const, 
                sel.years=sel.years, sel.const=sel.const, 
                Fcv=Fcv, Fphi=Fphi, 
                Blim=Blim, Bpa=Btrigger, 
                Fscan=Fscan, 
                verbose=verbose, 
                process.error=process.error, 
                Nrun=Nrun, 
                Btrigger=Btrigger,
                rhologRec=rhologRec, SSBcv=SSBcv, 
                extreme.trim=extreme.trim)
  
  #Create a table with the reference points 
  #t(SIM$refs_interval)
  
  #Estimate the catch at the equilibrium for a given F
  #SIM$rbp$p50[SIM$rbp$variable=="Catch"]
  
  ##Interpolate to get equilbrium catches
  Catch_50perc=approx(Fscan, SIM$rbp$p50[SIM$rbp$variable=="Catch"], xout=seq(min(Fscan),max(Fscan),length=40))
  
  Catchequi=Catch_50perc$y[which.min(abs(Catch_50perc$x - Fmsy))]
  ## where Fmsy is the preliminary Fmsy value identified in Step 1 above
  
  ##Interpolate to get this for more F values:
  BF_50perc1=approx(Fscan, SIM$rbp$p50[SIM$rbp$variable=="Spawning stock biomass"], xout=seq(min(Fscan),max(Fscan),length=1000))
  
  #Estimate B0 and BMSY
  F.0 = 0
  Bmsy=BF_50perc1$y[which.min(abs(BF_50perc1$x - Fmsy))]
  B0  =BF_50perc1$y[which.min(abs(BF_50perc1$x - F.0))]
  Flim=BF_50perc1$x[which.min(abs(BF_50perc1$y-Blim))]
  
  rtn=FLPar(c(Catchequi     =Catchequi,
              Bmsy          =Bmsy,
              B0            =B0,
              Flim          =Flim,
              unlist(c(SIM$refs_interval["FmsyMedianC"])),
              unlist(c(SIM$refs_interval["FmsyMedianL"])),
              unlist(c(SIM$refs_interval["F5percRiskBlim"]))))
  
  rtn}

