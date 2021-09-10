# {{{
# selage 
#
#' coverts harvest() and catch.sel() selectivity at age Sa  
#'
#' @param stk Input FLStock object.
#' @param nyears numbers of last years to compute selectivity
#' @param year option to specify year range, overwrites nyears
#' @return FLQuant of Selectivity-at-age Sa  
#' @export
#' @author Henning Winker (JRC)
selage <- function (stk, nyears=5,year=NULL){
if(is.null(year)){yr= (range(stk)["maxyear"]-nyears+1):range(stk)["maxyear"]} else {
yr = year  
}
Sa = apply(harvest(stk[,ac(yr)]),1,mean,na.rm=T)/max(apply(harvest(stk[,ac(yr)]),1,mean,na.rm=T),na.rm=T)
Sa@units = "NA"
return(Sa)
}

#{{{
# plot_selage 
#
#' plots mean selectivity at age Sa across selected years 
#'
#' @param stk Input FLStock object.
#' @param nyears numbers of last years to compute selectivity
#' @param year specific years (will overwrite nyears)
#' @return FLQuant of Selectivity-at-age Sa  
#' @export
#' @author Henning Winker (JRC)
plotselage<- function(stk,nyears=5,year=NULL){
  if(is.null(year)){yr= (range(stk)["maxyear"]-nyears+1):range(stk)["maxyear"]} else {
    yr = year } 
  Sa = as.data.frame(selage(stk,nyears=nyears,year=year))
  p = ggplot(data=(as.data.frame(catch.sel(stk[,ac(yr)]))),aes(x=age,y=data))+
    geom_line(aes(color = factor(year)))+ theme(legend.title=element_blank())+
    ylab("Selectivity")+xlab("Age")+geom_line(data=Sa,aes(x=age,y=data),size=1)
  return(p)  
}
# }}}


#{{{
# fabs() 
#
#' Compute instantaneous F, such that  F_a = Fabs*Sa   
#'
#' @param stk Input FLStock object.
#' @param nys numbers of last years to compute selectivity
#' @return value Fabs  
#' @export
#' @author Henning Winker (JRC)
fabs <- function (stk, nyears=5,year=NULL){
  if(is.null(year)){yr= (range(stk)["maxyear"]-nyears+1):range(stk)["maxyear"]} else {
    yr = year  
  }
  Fabs = max(apply(harvest(stk[,ac(yr)]),1,mean,na.rm=T),na.rm=T)
  return(Fabs)
}
# }}}

#{{{
# selexpars
#
#' computes initial values for selex pars 
#'
#' @param Sa selectivity at age Sa = selage(stk)
#' @param S50 age-at-50%-selectivty
#' @param S95 age-at-95%-selectivty
#' @param Smax age at peak of halfnormal or top of descending slop
#' @param Dcv CV of halfnormal determining the steepness of the descending slope
#' @param Dmin height of the descending slop at maximum age
#'
#' @return vector of selex pars 
#' @export 
#' @author Henning Winker (JRC)

selexpars <- function(Sa,S50=NULL,S95=NULL,Smax=NULL,Dcv=NULL,Dmin=NULL){
  sa = as.data.frame(Sa)
  age = sa$age
  sel = sa$data
  
  selex.dat = data.frame(age=c((0:3)[which(!0:3%in%age)],age),
                         sel=c(rep(0.01,3)[which(!0:3%in%age)],sel))
  peak = which(selex.dat[,2]>0.85)[1]
  neg=sel[age>peak]
  dcv = ifelse(length(neg)>1, abs(0.5+2*quantile(neg[-1]-neg[-length(neg)],0.2)[[1]]),0.3)
   
  if(is.null(S50))  S50= selex.dat[peak[1],1]*0.6
  if(is.null(S95))  S95=selex.dat[peak[1],1]*0.9
  if(is.null(Smax))  Smax=selex.dat[max(peak),1]*1.3
  if(is.null(Dcv))  Dcv=(1-sel[nrow(sa)])#/length(age[age>peak])
  if(is.null(Dmin))  Dmin=sel[nrow(sa)]
  
  pars = FLPar(S50=S50,S95=S95,Smax=Smax,Dcv=Dcv,Dmin=Dmin)
  return(pars)
}
# }}}

#{{{
# selex
#
#' computes selex curves 
#'
#' @param Sa selectivity at age Sa = selage(stk)
#' @param pars selexpars S50, S95, Smax, Dcv, Dmin 
#' @param ssq if TRUE return sum-of-squares
#' @return FLquants selex predictions
#' @export 
#' @author Henning Winker (JRC)

selex <- function(Sa,pars,ssq=FALSE){
  sa = as.data.frame(Sa)
  age = sa$age
  sel = sa$data
  S50 = pars[[1]]
  S95 = pars[[2]]
  Smax =pars[[3]]
  Dcv =pars[[4]]
  Dmin =pars[[5]]
  
  selex.dat = data.frame(age=c((0:3)[which(!0:3%in%age)],age),
                         sel=c(rep(0.01,3)[which(!0:3%in%age)],sel))
  subs = which(selex.dat$age%in%age)
  
  
  psel_a = 1/(1+exp(-log(19)*(selex.dat$age-S50)/(S95-S50)))
  psel_b = dnorm(selex.dat$age,Smax,Dcv*Smax)/max(dnorm(selex.dat$age,Smax,Dcv*Smax))
  psel_c = 1+(Dmin-1)*(psel_b-1)/-1
  psel = ifelse(selex.dat$age<=Smax,psel_a,psel_c)
  psel = psel/max(psel)
  #psel = pmin(psel,0.999)
  
  #resids = log(selex.dat$sel)-log(psel)
  
  #fits=data.frame(age=selex.dat$age[subs],obs=selex.dat$sel[subs],fit=psel[subs],logis=psel_a[subs],halfnorm=psel_b[subs],height=psel_c[subs])
  observed = fitted = logis = hnorm = height = Sa
  fitted[]=matrix(psel[subs])
  logis[]=matrix(psel_a[subs])
  hnorm[]=matrix(psel_b[subs])
  height[]=matrix(psel_c[subs])
  
  pred = FLQuants(observed,fitted,logis,hnorm,height)
  pred@names = c("observed","fitted","logis","hnorm","height")  
  
  
 
  return(pred)
}
# }}}

#{{{
# fitselex
#
#' fits selex selectivity function to F_at_age from FLStock 
#'
#' @param Sa selectivity at age Sa = selage(stk)
#' @param S50 init value age-at-50%-selectivty
#' @param S95 init value age-at-95%-selectivty
#' @param Smax init value age at peak of halfnormal or top of descending slop
#' @param Dcv init value CV of halfnormal determining the steepness of the descending slope
#' @param Dmin init value height of the descending slop at maximum age
#' @return list with fit and FLquants selex predictions
#' @export 
#' @author Henning Winker (JRC)

fitselex <- function(Sa,S50=NULL,S95=NULL,Smax=NULL,Dcv=NULL,Dmin=NULL){
  
  pars =selexpars(Sa=Sa,S50=S50,S95=S95,Smax=Smax,Dcv=Dcv,Dmin=Dmin)
  
  imp = c(pars)
  lower = imp*0.3
  upper = imp*2
  upper[4] = max(imp[4],1)
  lower[4] = 0.05
  lower[5]= 0

  # Likelihood
  jsel.ll = function(par=imp,data=Sa){
    Sa=data
    flimp = FLPar(S50=par[1],S95=par[2],Smax=par[3],Dcv=par[4],Dmin=par[5])
    pred= selex(Sa=Sa,pars=flimp)
   return(sum(((pred$observed)-(pred$fitted))^2))
  }
  
  
  fit = optim(par=imp, fn = jsel.ll,method="L-BFGS-B",lower=lower,upper=upper, data=Sa, hessian = TRUE)
  fit$par = FLPar(S50=fit$par[1],S95=fit$par[2],Smax=fit$par[3],Dcv=fit$par[4],Dmin=fit$par[5])
  fit$name
  fit$fits = selex(Sa,fit$par)
  return(fit)
}
# }}}

#{{{
# plotselex 
#
#' plots mean selectivity at age Sa across selected years 
#'
#' @param object selexpars FLPars(s) or output from fitselex()
#' @param Sa observed selectivity-at-age (FLQuant) with dimnames(age) 
#' @param obs show observations if TRUE
#' @param compounds option to show selex compounds
#'
#' @return FLQuant of Selectivity-at-age Sa  
#' @export
plotselex<- function(object,Sa=NULL,obs=NULL,compounds=FALSE){
  if(class(object)=="list"){
  pars = object$par
  Sa = object$fits$observed
  } else {
  pars=object  
  }
  if(class(pars)=="FLPar"){
    pars = FLPars(pars)
    pars@names = paste0(round(pars[[1]][[1]],2))
  }
  if(is.null(Sa)){
   Sa = FLQuant(c(0.01,0.5,rep(1,ceiling(pars[[1]][[2]]*2)-2)),dimnames=list(age=1:ceiling(pars[[1]][[2]]*2)))  
  }
  pdat = FLQuant(0.5,dimnames=list(age=seq(dims(Sa)$min,dims(Sa)$max,0.05))) 
  pred = lapply(pars,function(x){
    selex(pdat,x)
    })
  if(is.null(obs)){
    obs = ifelse(length(pred)>1,FALSE,TRUE)
  }
 
  if(length(pred)>1){
    seldat = FLQuants(lapply(pred,function(x){
      x[["fitted"]]}))
    }
  
  if(length(pred)==1 & compounds==TRUE){
    seldat = as.data.frame(pred[[1]][c(3:5,2)]) 

    cols=c(rainbow(3),"black")
  }
  if(length(pred)==1 & compounds==FALSE){
    seldat = as.data.frame(pred[[1]][2])  
    cols=c("black")
  }
  # Plot
  p = ggplot(as.data.frame(seldat))+
    geom_line(aes(x=age,y=data,colour=qname))+geom_hline(yintercept = 0.5,linetype="dotted")
  if(length(pred)==1){
    p=p + scale_color_manual("Selex",values=cols)
  } else {
    p = p +scale_colour_discrete("S50")
  }
  if(obs & length(pred)==1) p = p+geom_point(data=as.data.frame(Sa),aes(x=age,y=data), fill="white",shape=21,size=2)
  if(obs & length(pred)>1) p = p+geom_line(data=as.data.frame(Sa),aes(x=age,y=data),linetype="dashed")
  
  p = p +ylab("Selectivity")+xlab("Age")+
    scale_x_continuous(breaks = 1:100)+scale_y_continuous(breaks = seq(0, 1, by = 0.25))+ylim(0,1.03) 
  if(length(pred)>15) p = p+theme(legend.position = "n")
  
  return(p)  
}
# }}}

#{{{
#' aopt()
#'
#' Function to compute Aopt, the age where an unfished cohort attains maximum biomass  
#' @param object class FLStock
#' @return FLQuant with annual spr0y  
#' 
#' @export
#' @author Henning Winker (JRC)
aopt<-function(object,nyears=3){
  age = dims(object)[["min"]]:dims(object)[["max"]]
  survivors=exp(-apply(m(object),2,cumsum))
  survivors[-1]=survivors[-dim(survivors)[1]]
  survivors[1]=1
  expZ=exp(-m(object[dim(m(object))[1]]))
  if (!is.na(range(object)["plusgroup"]))
    survivors[dim(m(object))[1]]=survivors[dim(m(object))[1]]*(-1.0/(expZ-1.0))
  ba = yearMeans(tail((stock.wt(object)*survivors)[-dims(object)[["max"]],],nyears))
  aopt = age[which(ba==max(ba))[1]]
  # Condition that at aopt fish had spawned 1 or more times on average
  aopt =  max(aopt,(which(yearMeans(tail(object@mat,nyears))>0.5)+1)[1])
  # ensure that aopt <= maxage
  aopt = min(aopt,dims(object)[["max"]])
  
  return(aopt)
}
# }}}


#{{{
# varselex
#
#' function to dynamically vary selex parameters 
#'
#' @param selexpar 5 selex parameters of class FLPar 
#' @param stock optional stock object for tuning of age range  
#' @param step step size of change in one or several pars
#' @param amin start of S50
#' @param amax end of S50, required if stock = NULL
#' @param amax end of S50, required if stock = NULL
#' @param nyears end years for computing aopt() as automated amax limit
#' @param type option of selectivity change "crank","shift" or "dynamic"
#' @return FLPars of selex parameters
#' 
#' @export 
#' @author Henning Winker (JRC)

varselex = function(selpar,stock=NULL,step=0.1,amin=NULL,amax=NULL,nyears=5,type=c("crank","shift","dynamic")){
type = type[1]
if(is.null(amin)) amin = round(selpar[[1]]*0.7,1)
if(type=="crank"){
  if(is.null(amax)) amax = round(selpar[[2]]*0.95,1)
}
if(type%in%c("shift","dynamic","selective")){
  if(is.null(stock)){
    if(is.null(amax)) stop("amax must be provided if stock = NULL")
  } else {
    if(is.null(amax)) amax = max(aopt(stock,nyears),selpar[[1]]) 
  }}
seqi = seq(amin,amax,step)
diff = seqi-selpar[[1]]
if(type=="crank"){
pars = FLPars(lapply(as.list(diff),function(x){
  out = selpar
  out[1]=out[1]+x
  out
}))  
}  
if(type=="dynamic"){
  pars = FLPars(lapply(as.list(diff),function(x){
    out = selpar
    ds = selpar[3]-selpar[2]
    out[1]= out[1]+x
    out[2] = max(selpar[2],out[1]*1.2)
    out[3] = max(out[2]+ds,selpar[3])
    out
  }))  
}  
if(type=="shift"){
  pars = FLPars(lapply(as.list(diff),function(x){
    out = selpar
    out[1:3]=out[1:3]+x
    out
  }))  
}  
pars@names = paste0(seqi)
return(pars)
}
# }}}

#' allcatch()
#' 
#' Function to assign all discards to landings for FLBRP refs  
#' @param stock class FLStock
#' @return FLStock
#' @export
#' @author Henning Winker (JRC)
 
allcatch <- function(stock){
landings.n(stock) = catch.n(stock)  
discards.n(stock)[] = 0.000001  
discards.wt(stock) = stock.wt(stock)
landings(stock) = computeLandings(stock)
discards(stock) = computeDiscards(stock)
}


#{{{
# selex.backtest() 
#
#' function to do nyears backtest of selex pattern in FLStocks 
#' @param stock stock object of class FLStock 
#' @param pars list of selex parameters of class of FLPars()  
#' @param byears number of backtest years   
#' @param nyears number of years for reference conditions   
#' @param quantity observed "f" or "catch" for fwdControl() 
#' @return FLStocks object
#' @export
#selex.backtest = function(stock,pars,byears=10,nyears=5,quantity=c("f","catch")){

  