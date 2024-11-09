require(JABBA)
require(FLCore)
require(FLBRP)

require(plyr)
require(dplyr)

priors<-function(x) attributes(x)$priors

stdz<-function(x){
  x=x-mean(x,na.rm=TRUE)
  x/var(x,na.rm=TRUE)^0.5}



jabFn<-function(catch,pr,index=NULL,q_bounds=NULL,model="Pella_m",
                fit=NULL,fix=NULL,auxIndex=FALSE,
                pr.sd=0.3,
                sigma.proc=TRUE,
                trunc=0,
                assessment="",scenario="",...){
  
  ## priors
  r        =unlist(c(pr[c("r")]))
  psi      =unlist(c(pr[c("ssb.minyr")]/pr[c("b0")]))
  if (is.na(psi)) psi=0.9
  shape    =unlist(c(pr[c("shape")]))
  r.prior  =c(r,  pr.sd[1])
  psi.prior=c(psi,pr.sd[length(pr.sd)])
 
  b.prior  =c(unlist(c(pr["ssb.maxyr"]/pr["bmsy"])),pr.sd,max(catch$year),"bbmsy") 
  f.prior  =c(unlist(c(pr[  "f.maxyr"]/pr["fmsy"])),pr.sd,max(catch$year),"ffmsy") 

  if (!is.null(q_bounds))
    args=list(q_bounds=q_bounds)
  else
    args=list()
  
  if (!is.null(fit)){
    mu=apply(fit$pars_posterior,2,mean)
    sd=apply(fit$pars_posterior,2,var)^0.5
 
    m        =c(mu["m"])
    m.CV     =1e-6
    r.prior  =c(mu["r"],  pr.sd)
    psi.prior=c(mu["psi"],pr.sd)
    
    if (!is.null(fix)){
      if (fix=="b")  
        args=list(b.prior=c(mean(fit$kobe$stock),  pr.sd,fit$kobe$yr[1],"bbmsy"))
      if (fix=="f")  
        args=list(b.prior=c(mean(fit$kobe$harvest),pr.sd,fit$kobe$yr[1],"ffmsy"))}
    }

  args=c(args,
         list(scenario  ="",
              model.type=model,
              BmsyK     =shape,
               
              catch     =catch,
              cpue      =index,
              
              r.prior   =r.prior,
              psi.prior =psi.prior,
               
              sigma.proc=sigma.proc,
              verbose   =FALSE))

  if (auxIndex){
    ## auxillary index
    aux =auxFn(...)
    aux$auxiliary=data.frame(year =as.numeric(dimnames(fit$timeseries)[[1]]),
                             ffmsy=fit$timeseries[,"mu","FFmsy"])
    if (trunc>0)
       aux$auxiliary[1:(dim(aux$auxiliary)[1]-trunc),"ffmsy"]=NA
    aux$auxiliary.type="ffmsy"
    args=c(args,aux)}
  
  ## Fit with Catch + Index: Simple Fox with r = Fmsy
  input=try(do.call("build_jabba", args))
  
  if ("try-error"%in%is(input)) return(NULL)

  fit=try(fit_jabba(input,quickmcmc=T,verbose=F))
  
  if ("try-error"%in%is(fit)) return(NULL)
  
  fit$assessment=assessment
  fit$scenario  =scenario
  
  list(input=input,fit=fit)}


hindJabba<-function (jbinput, fit, ni = NULL, nt = NULL, nb = NULL, nc = NULL, 
          quickmcmc = TRUE, init.values = TRUE, peels = 1:5, verbose = FALSE, save.all=TRUE){
  runs = as.list(peels)
  if (is.null(ni) & !quickmcmc) 
    ni = fit$settings$mcmc$ni
  if (is.null(nt) & !quickmcmc) 
    ni = fit$settings$mcmc$nt
  if (is.null(nb) & !quickmcmc) 
    ni = fit$settings$mcmc$nb
  if (is.null(nc) & !quickmcmc) 
    ni = fit$settings$mcmc$nc
  if (init.values) {
    Kin = fit$pars[1, 1]
    rin = fit$pars[2, 1]
    qin = fit$pars[(3:(2 + fit$settings$nq)), 1]
  }
  else {
    Kin = NULL
    rin = NULL
    qin = NULL
  }
  fithc = lapply(runs, function(x) {
    fit_jabba(jbinput, save.trj = TRUE, ni = ni, nt = nt, 
              nb = nb, nc = nc, init.values = init.values, init.K = Kin, 
              init.r = rin, init.q = qin, quickmcmc = T, peels = x, 
              verbose = verbose, save.all=save.all)
  })
  retro = c(list(fit), fithc)
  names(retro) = c(fit$scenario, paste0("-", max(fit$yr) - 
                                          peels + 1))
  retro = Map(function(x, y) {
    x$settings$scenario = y
    x
  }, x = retro, y = as.list(names(retro)))
  return(retro)
}

hindFn1<-function(catch,prior,index=NULL,model="Pella_m",fix=NULL,...){
  
  ## priors
  r        =unlist(c(prior[,c("r")]))
  psi      =unlist(c(prior[,c("ssb.minyr")]/prior[,c("b0")]))
  shape     =unlist(c(prior[,c("shape")]))
  r.prior  =c(r,  0.3)
  psi.prior=c(psi,0.3)
  
  b.prior  =unlist(c(prior[,"ssb.maxyr"]/prior[,"bmsy"]))
  b.prior  =c(b.prior,1e-6,max(catch$year),"bbmsy") 
  f.prior  =unlist(c(prior[,"f.maxyr"]/prior[,"fmsy"]))
  f.prior  =c(f.prior,1e-6,max(catch$year),"ffmsy") 
  
  ## auxillary index
  aux=auxFn(...)
  
  args=c(list(scenario  ="",
              model.type=model,
              BmsyK     =shape,
              
              catch     =catch,
              cpue      =index,
              
              r.prior   =r.prior,
              psi.prior =psi.prior,
              
              verbose   =FALSE),aux)
  
  if (!is.null(fix))
    if (fix=="b") args=c(args,list(b.prior=b.prior)) else
      if (fix=="f") args=c(args,list(b.prior=f.prior))
  
  ## Fit with Catch + Index: Simple Fox with r = Fmsy
  jbInput=try(do.call("build_jabba", args))
  
  if ("try-error"%in%is(jbInput)) return(NULL)
  
  jbFit=try(fit_jabba(jbInput,quickmcmc = T,verbose=F))
  
  if ("try-error"%in%is(jbFit)) return(NULL)

  hnd=try(hindcast_jabba(jbInput, jbFit, verbose=FALSE,save.all=TRUE))
  
  if ("try-error"%in%is(hnd)) return(NULL)
  
  list(input=jbInput,fit=jbFit,hind=hnd)}


source("C:/active/FLCandy/R/OMstats.R")


benchmarks<-function(x) {
  if ("logical"%in%is(attributes(x)$benchmark))
    return(FLPar(Fmsy=NA,Flim=NA,Fpa=NA,Blim=NA,Bpa=NA,Btrigger=NA))
  
  as(attributes(x)$benchmark,"FLPar")}

fishlife2lhPar<-function(x) {
  res=attributes(x)$fishlife
  
  if ("lm"%in%names(res))
    names(res)[seq(length(res))[(names(res)=="lm")]]="l50"
  
  res=FLPar(res,units="NA")
  
  lhPar(res[c("linf","k","l50","s")])}

priorFn<-function(x,nmin=0:2,nmax=0:2){
  
  fmsy=NA;bmsy=NA;b0=NA;btrigger=NA
  names(attributes(x)$eqsim)    =tolower(names(attributes(x)$eqsim))
  names(attributes(x)$benchmark)=tolower(names(attributes(x)$benchmark))
  
  if ("fmsy"%in%names(attributes(x)$benchmark)) 
    fmsy=unlist(c(attributes(x)$benchmark["fmsy"]))
  if ("bmsy"%in%names(attributes(x)$eqsim)) 
    bmsy     =unlist(c(attributes(x)$eqsim["bmsy"]))
  if ("b0"%in%names(attributes(x)$eqsim)) 
    b0       =unlist(c(attributes(x)$eqsim["b0"]))
  if ("btrigger"%in%names(attributes(x)$benchmark)) 
    btrigger =unlist(c(attributes(x)$benchmark["btrigger"]))
  
  ssb.minyr=mean(ssb( x)[,1+nmin])
    f.minyr=mean(fbar(x)[,1+nmin])
  ssb.maxyr=mean(ssb( x)[,dim(ssb( x))[2]-nmax])
    f.maxyr=mean(fbar(x)[,dim(fbar(x))[2]-nmax])
  
  shape=bmsy/b0
  if (is.na(shape)) shape=0.4
  m    =NA
  r    =NA
  
  mi   =seq(0.01,2,0.001) 
  m    =(mi^(-1/(mi-1))-shape)^2
  m    =mi[m==min(m)]
  r    =(1-exp(-fmsy))*(m-1)/(1-m^-1)
  
  rtn=c(r=r,mpar=m,fmsy=fmsy,bmsy=bmsy,b0=b0,btrigger=btrigger,shape=shape,
        ssb.minyr=ssb.minyr,ssb.maxyr=ssb.maxyr,
          f.minyr=  f.minyr,  f.maxyr=  f.maxyr)
  names(rtn)=c("r","mpar","fmsy","bmsy","b0","btrigger","shape",
               "ssb.minyr","ssb.maxyr",
               "f.minyr",  "f.maxyr")
  
  rtn}

calcPriors<-function(x){
  
  fmsy=c(refpts(x)["msy",   "harvest"])
  bmsy=c(refpts(x)["msy",   "ssb"])
  b0  =c(refpts(x)["virgin","ssb"])
  
  f.minyr=c(fbar.obs(x)[,1])/fmsy
  ssb.minyr= c(ssb.obs(x)[,1])/bmsy
  f.maxyr=c(fbar.obs(x)[,dim(fbar.obs(x))[2]])/fmsy
  ssb.maxyr= c(ssb.obs(x)[,dim(ssb.obs(x))[2]])/bmsy
  
  shape=bmsy/b0
  if (is.na(shape)) shape=0.4
  m    =NA
  r    =NA
  
  mi   =seq(0.01,2,0.001) 
  m    =(mi^(-1/(mi-1))-shape)^2
  m    =mi[m==min(m)]
  r    =(1-exp(-fmsy))*(m-1)/(1-m^-1)
  
  rtn=c(r=r,mpar=m,fmsy=fmsy,bmsy=bmsy,b0=b0,shape=shape,
        ssb.minyr=ssb.minyr,ssb.maxyr=ssb.maxyr,
        f.minyr=  f.minyr,  f.maxyr=  f.maxyr)
  names(rtn)=c("r","mpar","fmsy","bmsy","b0","shape",
               "ssb.minyr","ssb.maxyr",
               "f.minyr",  "f.maxyr")
  
  rtn}

ebiomass<-function(object){
  sel  =harvest(object)
  wt   =catch.wt(object)%*%sel%/%fapex(sel)
  eb.wt=qmax(wt,0.000001)
  
  apply(eb.wt%*%stock.n(object),2:6,sum)}

tseriesFn<-function(object){
  
  model.frame(FLQuants(catch   =catch(object),
                       eb      =ebiomass(object),
                       ssb     =ssb(object),
                       p       =production(object),
                       f       =fbar(object),
                       h       =catch(object)/ebiomass(object),
                       m       =FLQuant(aaply(m(object)[ac(range(object)["minfbar"]:range(object)["maxfbar"])],2,mean),
                                        dimnames=dimnames(fbar(object)))),drop=TRUE)}
dataFn<-tseriesFn

rickerSV2<-function () {
  logl <- function(s, v, spr0, rec, ssb) {
    pars <- FLPar(abPars("ricker", s=s, v=v, spr0 = spr0))

    loglAR1(log(rec), log(pars["a"] %*% ssb %*%exp((-pars["b"] %*% ssb))))
  }
  
  initial <- structure(function(rec, ssb) {
    s   =0.75
    spr0=quantile(c(ssb/rec), prob=0.9, na.rm=TRUE,names=FALSE)
    v   =mean(as.vector(ssb), na.rm=TRUE)*2
    return(FLPar(s=s, v=v, spr0=spr0))
    
  }, lower = c(0.2,rep(1e-07,2)), upper=c(0.999,Inf,Inf))
  
  model <- rec ~ FLPar(abPars("ricker", s=s, v=v, spr0=spr0))["a"]%*%ssb%*% 
            exp(-FLPar(abPars("ricker", s=s, v=v, spr0=spr0))["b"]%*%ssb)
  
  return(list(logl=logl, model=model, initial=initial))}

eqlFn<-function(x,model="bevholtSV"){
  
  if (grepl("SV",model)>0){
    sr=fmle(as.FLSR(x,model=model), 
               fixed=list(s=attributes(x)$fishlife["s"],spr0=spr0(x)),
              control=list(silent=TRUE))
    rtn=brp(FLBRP(x,nyears=dim(x)[2],sr=list(model=do.call(gsub("SV", "", gsub("2", "", model)),list())$model,
                                    params=ab(params(sr),model="bevholt"))))}
  else if (grepl("bevholt",model)>0){
    sr=fmle(as.FLSR(x,model=model),control=list(silent=TRUE),
            method="L-BFGS-B",lower=c(1e-10,1e-10),upper=c(Inf,Inf))
    rtn=brp(FLBRP(x,nyears=dim(x)[2],sr=list(model=do.call(model,list())$model,params=params(sr))))}
  else{
    sr=fmle(as.FLSR(x,model=model),control=list(silent=TRUE))
    rtn=brp(FLBRP(x,nyears=dim(x)[2],sr=list(model=do.call(model,list())$model,params=params(sr))))}
  
  attributes(rtn)[["logLik"]] =logLik(sr)
  attributes(rtn)[["priors"]] =priors(rtn)
  attributes(rtn)[["prod"]]   =spFn(rtn)
  attributes(rtn)[["tseries"]]=tseriesFn(x)
    
  return(rtn)}

peFn<-function(stk,eq,stock=FLCore:::ssb){
  (stock(stk)%-%
     window(stock(stk)[,-1],end=dims(stk)$maxyear+1)-
     catch(stk)%+%sp(stk,eq,stock))%/%stock(stk)}

peFn2<-function(x){
  (ssb.obs(x)%-%
     window(ssb.obs(x)[,-1],end=dims(ssb.obs(x))$maxyear+1)%-%
     catch.obs(x)%+%sp(stk,eq))%/%ssb.obs(x)}

spFn<-function(x){
  rfs=FLPar(c(ssb.obs(x)),dimnames=list(refpts="ssb",
                                        quant =dimnames(refpts(x))$quant,
                                        iter  =seq(dim(ssb.obs(x))[2])))
  rfs[,-4]=NA
  refpts(x)=rfs
  
  rtn=data.frame(model.frame(FLQuants(x,ssb=ssb.obs,catch=catch.obs),drop=TRUE),
                 sp=c(computeRefpts(x)[,"yield"]))
  rtn$pe=(c(rtn$ssb[-1]-rtn$ssb[-dim(rtn)[1]]+rtn$catch[-dim(rtn)[1]]-rtn$sp[-dim(rtn)[1]],NA))/rtn$ssb

  rtn}

ldfFn<-function(object){
  
  ak =invALK(iter(par,1),cv=0.1,age=an(dimnames(object)$age),bin=1)
  lfd=lenSamples(catch.n(object),ak,n=5000)
  
  units(lfd)="cm"
  
  lfd}

maseBoot<-function(data, indices) {
  # select bootstrap sample
  d=data[indices, ] 
  return(mase(d$observed, d$predicted))}

if(FALSE){
# Load required packages
require(Metrics)
require(boot)

# Data is in a data frame  with columns:
# 'observed', 'predicted', 'sd'
df=data.frame(observed=rlnorm(12),predicted=rlnorm(12,0,0.2),sd=rep(0.1,12))

# Calculate MASE
val=mase(df$observed, df$predicted)

# Function to calculate MASE for bootstrapping

# Perform bootstrap with 1000 replicates
set.seed(123) # for reproducibility
bootRes<-boot(df, statistic=maseBoot, R=1000)

# Get 95% confidence intervals
ci     =boot.ci(bootRes, type="basic")
lower95=ci$basic[4] 
upper95=ci$basic[5]

cat("MASE:",   round(val, 3), "\n")
cat("95% CI:", round(lower95, 3), "-", round(upper95, 3), "\n")

rtn=ddply(subset(pp,par%in%c("K","r")), .(.id,par), with, 
            data.frame(posterior=median(posterior),
                       prior    =median(prior)))
rtn=merge(cast(rtn,.id~par,value="prior"),
            cast(rtn,.id~par,value="posterior"),
            by=".id")
  
ggplot(rtn)+geom_point(aes(r.x,K.y))
}

jabTS<-function(x){
  rtn=as.data.frame(x$fit$timeseries)
  names(rtn)=c("")
  }


