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

eqsim2<-function(stk,fbar=seq(0, 4, length=101), nsamp=2000, 
                  bio.years=c(-4,-0) + dims(stk)$maxyear,
                  sel.years=c(-4,-0) + dims(stk)$maxyear,
                  remove.years=NULL, 
                  pa=0.2, Fcv=0.212, Fphi=0.423,
                  Fs=seq(0,4,length.out=101)){
  
  #attach(args)
  
  # Fit all SRR models
  #srfit0=eqsr_fit(stk, nsamp=nsamp, models=c("Segreg","Bevholt"))
  
  # segreg to obtain Blim & Bpa 
  srfit1=eqsr_fit(stk, nsamp=nsamp, models="Segreg")
  Blim  =srfit1[["sr.det"]][,"b"]
  Bpa   =Blim*exp(1.645*pa)
  
  # get Flim by simulating for 10 years with Fcv=Fphi=0, Btrigger=0
  srsim1=eqsim_run(srfit1,
                   bio.years = bio.years, sel.years = sel.years,
                   Fcv = 0, Fphi = 0,
                   Btrigger=0, Blim = Blim, Bpa = Bpa,
                   Fscan = Fs,
                   verbose = FALSE)
  
  Flim=srsim1$Refs2["catF", "F50"]
  Fpa =Flim/pa
  
  # fit and remove last remove.years
  srfit2 <- eqsr_fit(stk, nsamp = nsamp,
                     models = "Segreg",
                     remove.years=remove.years)
  
  srsim2 <- eqsim_run(srfit2,
                      bio.years = bio.years, sel.years = sel.years,
                      bio.const = FALSE, sel.const = FALSE,
                      Fcv=Fcv, Fphi=Fphi,
                      Btrigger=0, Blim=Blim, Bpa=Bpa,
                      Fscan = Fs,
                      verbose = FALSE)
  
  cFmsy=srsim2$Refs2["lanF", "medianMSY"]
  F05  =srsim2$Refs2["catF", "F05"]
  
  # Btrigger
  srsim3 <- eqsim_run(srfit2,
                      bio.years = bio.years, sel.years = sel.years,
                      bio.const = FALSE, sel.const = FALSE,
                      Fcv = 0, Fphi = 0,
                      Btrigger=0, Blim=Blim, Bpa=Bpa,
                      Fscan   =Fs,
                      verbose =FALSE)
  
  # Btrigger < Bpa -> Bpa
  x        =srsim3$rbp[srsim3$rbp$variable=="Spawning stock biomass", ]
  cBtrigger=x[which(abs(x$Ftarget - cFmsy) == min(abs(x$Ftarget - cFmsy))), "p05"]
  
  # F05
  srsim4 <- eqsim_run(srfit2,
                      bio.years = bio.years, sel.years = sel.years,
                      bio.const = FALSE, sel.const = FALSE,
                      Fcv=0.212, Fphi=0.423,
                      Btrigger=cBtrigger, Blim = Blim, Bpa = Bpa,
                      Fscan = seq(0, 1.2, len = 40),
                      verbose = FALSE)
  
  F05 <- srsim4$Refs2["catF", "F05"]
  
  # If F05 < Fmsy, then Fmsy = F05
  if(cFmsy > F05){ 
    Fmsy <- F05
  } else {
    Fmsy <- cFmsy}
  
  # IF Btrigger < Bpa, then Btrigger = Bpa, then redo srsim4
  # OR IF Fbar 5yr != Fmsy
  
  if(cBtrigger < Bpa | all(tail(fbar(stk), 5) > Fmsy)) {
    
    Btrigger <- Bpa
    
    srsim4 <- eqsim_run(srfit2,
                        bio.years = bio.years, sel.years = sel.years,
                        bio.const = FALSE, sel.const = FALSE,
                        Fcv=0.212, Fphi=0.423,
                        Btrigger=Btrigger, Blim = Blim, Bpa = Bpa,
                        Fscan = seq(0, 1.2, len = 40),
                        verbose = FALSE)
    
    cFmsy <- srsim4$Refs2["lanF", "medianMSY"]
    F05 <- srsim4$Refs2["catF", "F05"]
    
    # If F05 < Fmsy, then Fmsy = F05
    if(cFmsy > F05) {
      Fmsy <- F05
    }}
  
  # FMSY (low - upp) w/o Btrigger
  lFmsy <- srsim3$Refs2["lanF", "Medlower"]
  uFmsy <- srsim3$Refs2["lanF", "Medupper"]
  
  ##### Refpts!
  refpts=FLPar(Btrigger=Btrigger, Fmsy=Fmsy, 
               Blim=Blim, Bpa=Bpa,
               Flim=Flim, Fpa=Fpa, 
               lFmsy=lFmsy, uFmsy=uFmsy, F05=F05,
               units=c("t", "f", rep("t", 2), rep("f", 4), rep("t", 3)))
  
  return(refpts)}

if(FALSE){
  library("future.apply")
  plan(multisession)
  
  res   =future_lapply(BalticSea, FUN=eqSim)
  eqsims=ldply(res,model.frame)
}
