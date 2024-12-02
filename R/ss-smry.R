rfs=c("ssb0",            "b0",               "b0.",              "r0",         "ssb1", "virgin",
      "ssbtar",          "sprtar",           "ftar",             "biomasstar", 
      "ssbsprtar",       "fsprtar",          "catchsprtar",      "ssbmsy", 
      "sprmsy",          "fmsy",             "msy",              "retainmsy",
      "msy","fmsy2") 
names(rfs)=tolower(
    c("SSB_Unfished",    "TotBio_Unfished",  "SmryBio_Unfished", "Recr_Unfished", "SSB_Initial","SSB_Virgin",
      "SSB_Btgt",        "SPR_Btgt",         "Fstd_Btgt",        "TotYield_Btgt", 
      "SSB_SPRtgt",      "Fstd_SPRtgt",      "TotYield_SPRtgt",  "SSB_MSY",  
      "SPR_MSY",         "Fstd_MSY",         "TotYield_MSY",     "RetYield_MSY",
      "Dead_Catch_MSY","annF_MSY")) 

# Convert nested list to list of data frames
unnest <- function(x) {
  # Get names from inner lists
  Inner = unique(unlist(lapply(x, names)))
  Outer = names(x)
  
  # Create result list with one element per inner name
  result = setNames(vector("list", length(Inner)), Inner)
  
  # Combine corresponding data frames and add outer list name
  for (name in Inner) {
    result[[name]] = do.call(rbind, lapply(seq_along(x), function(i) {
      df= x[[i]][[name]]
      df$Scenario = Outer[i]
      return(df)}
      ))}
  
  return(result)}

unnest<-function(nested_list) {
  # Validation
  if (!is.list(nested_list)) stop("Input must be a list")
  
  # Get unique inner names across all elements
  inner_names=unique(unlist(lapply(nested_list, names)))
  outer_names=names(nested_list)
  
  # Initialize result list
  result=setNames(vector("list", length(inner_names)), inner_names)
  
  # Process each inner element
  result=lapply(inner_names, function(inner_name) {
    do.call(rbind, Map(function(outer_name, data) {
      if (inner_name %in% names(data)) {
        df=data[[inner_name]]
        df$Scenario=outer_name
        df
      }
    }, outer_names, nested_list))
  })
  
  # Set names and return
  names(result)=inner_names
  result}


getPath<-function(file) {
  if (!grepl(.Platform$file.sep,file))
    res <- getwd()
  else
    res <- substr(file,1,max(gregexpr(.Platform$file.sep,file)[[1]])-1)
  return(res)}

getFile<-function(file) {
  res <- substr(file,max(gregexpr(.Platform$file.sep,file)[[1]])+1,
                nchar(file))
  return(res)}

getSS<-function(x,covar=TRUE,forecast=TRUE,ncols=320){
  
  if (dir.exists(x))  {
    res=try(SS_output(x, verbose=FALSE,printstats=FALSE,covar=covar,forecast=forecast,ncols=ncols))
    if ("tr-error"%in%is(res))  stop("dir supplied can not be read by 'SS_output'") else ss=res
  }else if (file.exists(x)) {
    load(x)
    if (!("ss"%in%ls())) stop("file supplied does not contain object called 'ss'")}
  else stop("nothing found")
  
  ss}    

getTs<-function(x) {
  ts=merge(x$timeseries[,c(1:5,7:8)],ddply(x$catch,.(Yr), with,sum(Obs)))
  if (length(names(ts))==8)
    names(ts)=c("year","area","era","season","biomass","ssb","rec","catch")
  else
    names(ts)=c("year","era","season","biomass","ssb","rec","catch")
  
  ts}

getRf<-function(x){
  names(x$derived_quants)=tolower(names(x$derived_quants))

  x$derived_quants$label=tolower(x$derived_quants$label)
  
  rf=subset(x$derived_quants,
            label%in%tolower(c("SSB_Unfished","TotBio_Unfished","SSB_Initial","SSB_MSY",
                               "TotYield_MSY","Dead_Catch_MSY","Fstd_MSY","annF_MSY")))
  rf[,1]=rfs[rf[,1]]
  dimnames(rf)[[1]]=rf[,1]
  
  cbind("quant"=c("hat","var"),as.data.frame(t(rf[,2:3])))
  }

getAic<-function(x){

  Total_LL=x$likelihoods_used$values[1]
  N_Params=x$N_estimated_parameters
  AIC = 2*N_Params + 2*Total_LL
  
  c(ll=Total_LL,n=N_Params,aic=AIC)}

getRefpts<-function(object,value=TRUE){
  names(object$derived_quants)=tolower(names(object$derived_quants))
  rf=subset(object$derived_quants,label%in%rfs[c(1,12,15)])[,2:3]
  names(rf)=c("value","var")
  dimnames(rf)[[1]]=c("k","bmsy","msy")
  rf=t(as.matrix(rf))
  rf["var",]=rf["var",]^2
  rf=as.data.frame(rf)
  
  rf[ifelse(value,1,2),]}

hat<-function(ssb,r,k,p) productionFn(ssb,r,k,p)
 pe<-function(year,ssb,catch,hat){
  data.frame(year=year[-length(year)],
             pe=log((ssb[-1]+catch[-length(ssb)]-hat[-length(ssb)])/ssb[-length(ssb)]))}

getPellaT<-function(object){
  rfs=c("SSB_Unfished",    "TotBio_Unfished",  "SmryBio_Unfished", "Recr_Unfished",    
        "SSB_Btgt",        "SPR_Btgt",         "Fstd_Btgt",        "TotYield_Btgt",   
        "SSB_SPRtgt",      "Fstd_SPRtgt",      "TotYield_SPRtgt",  "SSB_MSY",         
        "SPR_MSY",         "Fstd_MSY",         "TotYield_MSY",     "RetYield_MSY",
        "SSB_unfished","Dead_Catch_MSY") 
  
  names(object$derived_quants)=tolower(names(object$derived_quants))
  rf=subset(object$derived_quants,label%in%rfs[c(1,17,12,15,18)])[,2:3]
  names(rf)=c("value","var")
  dimnames(rf)[[1]]=c("k","bmsy","msy")
  rf=t(as.matrix(rf))
  rf["var",]=rf["var",]^2
  rf=as.data.frame(rf)
  
  pt=transmute(rf["value",],shape=bmsy/k,
               k    =k,
               p    =optimise(function(x,y) (y-(1/(1+x))^(1/x))^2,
                              c(-0.9999,10),y=shape)$minimum,
               r    =(1+p)*(msy/bmsy))
  
  pt[,c("k","r","p","shape")]}

getPellaTBiomass<-function(object){
  rf=subset(object$derived_quants,label%in%c("TotBio_Unfished","TotYield_MSY"))[,2:3]
  names(rf)=c("value","var")
  dimnames(rf)[[1]]=c("k","msy")
  rf=t(as.matrix(rf))
  rf["var",]=rf["var",]^2
  rf=as.data.frame(rf)
  
  pt=transmute(rf["value",],
               shape=bmsy/k,
               k    =k,
               p    =optimise(function(x,y) (y-(1/(1+x))^(1/x))^2,
                              c(-0.9999,10),y=shape)$minimum,
               r    =(1+p)*(msy/bmsy))
  
  pt[,c("k","r","p","shape")]}

productionFn<-function(b,r,k,p){
  t1=b*r/p
  t3=(b/k)^p
  t1*(1-t3)}

spFunc<-function(x,add=FALSE,col2="blue",labels=c("","","biomass","Surplus Production"),plotIt=FALSE){
  # function to calculate and plot surplus production
  
  # timeseries excluding equilibrium conditions and forecasts
  ts <- x$timeseries[!x$timeseries$Era %in% c("VIRG","FORE"),]
  
  # get total dead catch
  stringB <- "dead(B)"
  catchmat <- as.matrix(ts[, substr(names(ts),1,nchar(stringB))==stringB])
  # aggregate catch across fleets
  catch <- rowSums(catchmat)
  
  # aggregate catch and biomass across seasons and areas
  catch_agg <- aggregate(x=catch, by=list(ts$Yr), FUN=sum)$x
  Bio_agg <- aggregate(x=ts$Bio_all, by=list(ts$Yr), FUN=sum)$x
  
  # number of years to consider
  Nyrs <- length(Bio_agg)
  sprod <- rep(NA, Nyrs)
  
  # calculate surplus production as difference in biomass adjusted for catch
  sprod[1:(Nyrs-1)] <- Bio_agg[2:Nyrs] - Bio_agg[1:(Nyrs-1)] + catch_agg[1:(Nyrs-1)]
  sprodgood <- !is.na(sprod)
  Bio_agg_good <- Bio_agg[sprodgood]
  sprod_good <- sprod[sprodgood]
  xlim <- c(0, max(Bio_agg_good, na.rm=TRUE))
  ylim <- c(min(0, sprod_good, na.rm=TRUE), max(sprod_good, na.rm=TRUE))
  
  if (plotIt){
    # make empty plot
    if(!add){
      plot(0, ylim=ylim, xlim=xlim, xlab=labels[3], ylab=labels[4], type="n")
    }
    # add lines
    lines(Bio_agg_good, sprod_good, col=col2)
    # make arrows
    old_warn <- options()$warn      # previous setting
    options(warn=-1)                # turn off "zero-length arrow" warning
    s <- seq(length(sprod_good)-1)
    arrows(Bio_agg_good[s], sprod_good[s], Bio_agg_good[s+1], sprod_good[s+1],
           length=0.06, angle=20, col=col2, lwd=1.2)
    options(warn=old_warn)  #returning to old value
    
    # add lines at 0 and 0
    abline(h=0,col="grey")
    abline(v=0,col="grey")
    # add blue point at start
    points(Bio_agg_good[1], sprod_good[1], col=col2, bg="white", pch=21)}
  
  invisible(data.frame(year=seq(length(Bio_agg)),biomass=Bio_agg, sp=sprod, catch=catch_agg,ssb=ddply(ts,.(Yr),with, sum(SpawnBio,na.rm=TRUE))[,2]))}

cpueSS=function(x,...) {
  
  if (is.list(x))
    res=x$cpue
  else
    res=SS_output(x,verbose=FALSE,printstats=FALSE,covar=FALSE,forecast=FALSE,...)$cpue
  
  res=res[,c(2:5,9:11,14,15)]
  
  names(res)=c("name","area","year","season","vulBiomass","obs","hat","se","se2")
  
  if (!FALSE)
     res=FLQuants(dlply(res, .(name), with, as.FLQuant(data.frame(year=year,data=obs))))

  res}

lenSS=function(x,...) {
  
  if (is.list(x))
    lfd=x$lendbase
  else
    lfd=SS_output(x,verbose=FALSE,printstats=FALSE,covar=FALSE,forecast=FALSE,...)$lendbase
  
  lfd=lfd[,c("Fleet","Yr","Obs","Bin")]
  names(lfd)=c("fleet","year","data","len")
  
  lfd=ddply(lfd,.(fleet,year,len), with, data.frame(data=sum(data,na.rm=TRUE)))
  
  FLQuants(dlply(lfd, .(fleet), with, as.FLQuant(data.frame(year=year,len=len,data=data))))}

smrySS<-function(x,covar=TRUE,forecast=TRUE,ncols=320){
  
  #if ("character"%in%is(x)) names(x)=1
  #if (require(doParallel)&require(foreach))
  #  ss=foreach(run=x, .combine='list', .multicombine=TRUE, .packages=c("r4ss"), .export=c("covar","forecast","ncols")) %dopar% 
  #      getSS(run,covar=covar,forecast=forecast,ncols=ncols)  
  #else
  
  if ("list"%in%is(x)&"Data_File"%in%names(x)) ss=list(x) else 
  if ("list"%in%is(x)&"Data_File"%in%names(x[[1]])) ss=x else
       if (is.null((names(x)))) {
         names(x)=seq(length(x))
         ss=mlply(data.frame(run=x), function(run) getSS(run,covar=covar,forecast=forecast,ncols=ncols))}

  ts=mdply(seq(length(ss)), function(x) getTs(ss[[x]]))
  pf=mdply(seq(length(ss)), function(x) getPellaT(ss[[x]]))
  rf=mdply(seq(length(ss)), function(x) getRf(ss[[x]]))
  ll=mdply(seq(length(ss)), function(x) cbind(t(getAic(ss[[x]])),grad=ss[[x]]$maximum_gradient_component))
  sp=mdply(seq(length(ss)), function(x) spFunc(ss[[x]]))
  pt=mdply(seq(length(ss)), function(x) getPellaT(ss[[x]]))
  kb=mdply(seq(length(ss)), function(x){
       names(ss[[x]]$Kobe)=c("year","stock","harvest")
       mj=ss[[x]]$derived_quants[,1:3]
       mj=subset(mj,Label%in%c("Bratio_2014","SSB_Unfished","SSB_MSY","F_2014"))
       bmsy=subset(mj,Label=="SSB_MSY")[,2]
       if (("SSB_Unfished"%in%mj$Label)) k=subset(mj,Label=="SSB_Unfished")[,2] else k=NA 
         transform(ss[[x]]$Kobe,stock.lim=stock*bmsy/k)})
  dg=mdply(seq(length(ss)), function(x){
    u=ss[[x]]$cpue[,c("Fleet_name","Yr","Seas","Obs","Exp","Dev")]
    names(u)=c("name","year","season","obs","hat","residual")
    u})
  
  if (!is.null(attributes(x)$split_labels)){
    ts=cbind(attributes(x)$split_labels[ts$.id,],ts)
    pf=cbind(attributes(x)$split_labels[pf$.id,],pf)
    rf=cbind(attributes(x)$split_labels[rf$.id,],rf)
    ll=cbind(attributes(x)$split_labels[rf$.id,],ll)
    sp=cbind(attributes(x)$split_labels[sp$.id,],sp)
    pt=cbind(attributes(x)$split_labels[pt$.id,],pt)
    kb=cbind(attributes(x)$split_labels[kb$.id,],kb)
    dg=cbind(attributes(x)$split_labels[dg$.id,],dg)
  }
    
  return(list(ts    =cbind(run=ts$X1,ts)[,-2],
              refpts=cbind(run=rf$X1,rf)[,-2],
              pfunc =cbind(run=pf$X1,pf,m=1+pf$p)[,-2],
              ll    =cbind(run=ll$X1,ll)[,-2],
              sp    =cbind(run=sp$X1,sp)[,-2],
              pt    =cbind(run=pt$X1,pt)[,-2],
              kb    =cbind(run=kb$X1,kb)[,-2],
              dg    =cbind(run=dg$X1,dg)[,-2]))}

parSS<-function(x,trace=FALSE){
  
  runs=maply(x, function(x) if (file.exists(x)) runs=getFile(getPath(x)) else runs=getFile(x))
  names(runs)=NULL
  
  mdply(x, function(x) {
           if (trace) print(getFile(getPath(x)))
           ss=getSS(x)
           rf=getRf(ss)
           ll=c(getAic(ss),grad=ss$maximum_gradient_component)
           pt=getPellaT(ss)
  
           cbind(run=getFile(getPath(x)),pt,t(unlist(rf[1,-1])),sd=t(unlist(rf[2,-1])),t(ll))})}

tsSSFn<-function(ss){
    rf=getRf(ss)
    sp=spFunc(ss)[-1,]
    names(ss$Kobe)=c("year","stock","harvest")
    mj=ss$derived_quants[,1:3]
    mj=subset(mj,Label%in%c("Bratio_2014","SSB_Unfished","SSB_MSY","F_2014"))
    bmsy=subset(mj,Label=="SSB_MSY")[,2]
    if (("SSB_Unfished"%in%mj$Label)) k=subset(mj,Label=="SSB_Unfished")[,2] else k=NA 
    kb     =transform(ss$Kobe,stock.lim=stock*bmsy/k)
    rec    =transmute(ss$recruit,rec=pred_recr)
    rec.hat=transmute(ss$recruit,exp_recr)
    
    cbind(year=kb[seq(dim(sp)[1]),1],
               sp[,c(4,2,5,3)],
               kb[seq(dim(sp)[1]),-1],
               catch.msy=sp$catch/rf[1,"msy"],
               rec=rec[seq(dim(sp)[1]),1],
               rec.hat=rec.hat[seq(dim(sp)[1]),1])
    }

tsSS<-function(ss){

  # Check what arg is #
  ## list?
  if ("list"%in%is(ss)){
    ## An SS_output object
    if (all(c("Data_File","Control_File")%in%names(ss)))
      return(tsSSFn(ss))
    
    ## A list of SS_output objects
    if (all(laply(ss, function(x) all(c("Data_File","Control_File")%in%names(x)))))
      return(ldply(ss, tsSSFn))
  }else
    return(mdply(ss, function(x) tsSSFn(getSS(x))))
    
  return(NULL)}


phase<-function(bmsy,msy,maxyield){
  
    fn1<-function(bmsy,msy)
            data.frame(x=c(0,  0, bmsy,bmsy,0),
                       y=c(0, Inf, Inf,   0,0))
    fn2<-function(bmsy,msy)
            data.frame(x=c(Inf, Inf,bmsy,bmsy,Inf),
                       y=c(Inf, Inf, Inf,   0,  0))
    fn3<-function(bmsy,msy)
            data.frame(x=c(0, bmsy,bmsy,0),
                       y=c(0,  msy, 0,  0))
    fn4<-function(bmsy,msy,maxyield)
            data.frame(x=c(bmsy,    bmsy, bmsy/msy*maxyield,bmsy),
                       y=c( msy,maxyield,          maxyield, msy))
  
    rbind(cbind(what=1,fn1(bmsy,msy)),
                 cbind(what=2,fn2(bmsy,msy)),
                 cbind(what=3,fn3(bmsy,msy)),
                 cbind(what=4,fn4(bmsy,msy,maxyield)))}

curve<-function(r,k,p){
    data.frame(biomass=seq(0,k,length.out=101), 
               catch  =productionFn(seq(0.001,k,length.out=101),r,k,p))}
masep<-function(obs,hat,h=1){
  if (length(obs)<(h+1)) return(NULL)
  
  naive=c(rep(NA,h),obs)[seq(length(obs))]
  t1=log(obs/hat)
  t2=log(naive/hat)
  
  rsdl1=t1[!is.na(t1*t2)]
  rsdl2=t2[!is.na(t1*t2)]
  
  mase=sum(abs(rsdl1))/sum(abs(rsdl2))
  p   =dm.test(rsdl1,rsdl2,alternative="g")$p.value
  data.frame(mase=mase,p=p)}

dtime<-function(fmsy){
  log(2)/(-log(1-fmsy))}

dtime<-function(r,p){
  fmsy=r*(1/(1+p))
  log(2)/fmsy}

isLeft<-function(a,b,c){
  return(((b[1] - a[1])*(c[2] - a[2]) - (b[2] - a[2])*(c[1] - a[1]))<0) }

getCom<-function(x){
  rf =getRf(x)
  sp =spFunc(x)[-1,]
  res=transform(sp,stock=ssb/rf[1,"ssbmsy"])[,c(1,4:6)]}
