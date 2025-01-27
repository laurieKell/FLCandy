auxFn<-function(lag=0,obsE=0.3,sigma=TRUE,type="",...){
  
  args=list(...)
  
  auxilary      =NULL
  
    if(any(names(args)%in%c("z", "f", "ffmsy","effort","bbmsy", "bk"))){
      type     =names(args)[names(args)%in%c("z", "f", "ffmsy","effort","bbmsy", "bk")][1]
      auxilary=args[[type]]}
  
  list(
    auxiliary      =auxilary, 
    auxiliary.type =type,
    auxiliary.sigma=sigma, # estimated?
    auxiliary.obsE =obsE,   
    auxiliary.lag  =lag)   # lag effect between impact and Z pop structure
}

mFn<-function(shape,fmsy){
  
  mi   =seq(0.01,2,0.001) 
  m    =(mi^(-1/(mi-1))-shape)^2
  m    =mi[m==min(m)]
  r    =(1-exp(-fmsy))*(m-1)/(1-m^-1)
  c(m=m,r=r)}

jabbaPriors<-function(icesdata){
  
  eqsm   =eqsim(    icesdata)
  benchm =benchmark(icesdata)
  initial=ldply(    icesdata, function(x) {rtn=tseries(x);rtn[rtn$year==min(rtn$year),]})
  current=ldply(    icesdata, function(x) {rtn=tseries(x);rtn[rtn$year==max(rtn$year),]})
  fl     =fishlife( icesdata)
  
  priors=merge(benchm[,-8],eqsm[,c(".id","bmsy","b0")],       by=".id")
  priors=merge(priors,transmute(fl,.id=.id,r      =r),        by=".id")
  priors=merge(priors,transmute(initial, .id=.id,initial=ssb),by=".id")
  priors=merge(priors,transmute(current, .id=.id,current=ssb),by=".id")
  priors=transform(priors,shape=bmsy/b0,ssb.maxyear=current/bmsy,ssb.minyear=initial/b0,psi=initial/b0)
  
  priors}

jabbaData<-function(id,icesdata,ctc1903=NULL,indices=NULL){
  
  ts=model.frame(tseries(icesdata[[id]]),drop=TRUE) 
  
  catch=ts[, c("year", "catch")]
  names(catch)[2]="catch"
  small=mean(ts$catch,na.rm=TRUE)*1e-6
  catch$catch[is.na(catch$catch)|catch$catch<=0]=small
  
  eb    =transmute(ts, year=year, index=eb)
  eb[is.na(eb)]=NA
  
  fmsy=benchmark(icesdata[[id]])["fmsy"]
  ffmsy=transmute(ts, year=year, ffmsy=(1-exp(-f))/(1-exp(-c(fmsy))))
  
  rtn=merge(merge(catch,eb,by="year"),ffmsy,by="year")
  
  if (!is.null(ctc1903)){
    ctc1903=subset(ctc1903,.id==id&year<=dims(icesdata[[id]])$maxyear)[,2:3]
    if (dim(ctc1903)[1]>0){
      rtn=merge(rtn[,-2],ctc1903,by="year",all.y=TRUE)
      rtn$catch[is.na(rtn$catch)|rtn$catch<=0]=small
      }}
  
  if (is.null(indices)) return(rtn)
  
  idx =subset(indices,.id==id)
    
  if (dim(idx)[1]>0){
    idx   =try(cast(subset(idx,.id==id&year<=dims(icesdata[[id]])$maxyear),year~survey,value="data",fun="mean"))
    idx   =merge(idx,catch,by="year",all.y=TRUE)[,seq(dim(idx)[2])]
    idx[is.na(idx)]=NA
    
    return(merge(rtn,idx,by="year",all.x=TRUE))}
  
  return(rtn)}

jabbaWrapper<-function(catch,
                       pr,       
                       pr.sd      =pr/pr*0.3,
                       model      ="Pella_m",
                       assessment ="",scenario="",
                       index      =NULL,q_bounds=NULL,
                       sigma.est  =TRUE,
                       fixed.obsE =0.1,
                       sigma.proc =TRUE,
                       fixed.procE=0.1,
                       igamma     =c(0.001, 0.001),
                       q_bound    =c(1e-3,1e-3),
                       currentDepletion="",
                       initialDepletion=NA,...){
  
  ## priors
  r        =unlist(c(pr[c("r")]))
  r.prior  =c(r,  pr.sd["r"])

  psi      =unlist(c(pr[c("psi")]))
  if (is.na(psi)) psi=0.9
  psi.prior=c(psi,pr.sd["psi"])

  shape    =unlist(c(pr[c("shape")]))
  shape.cv =pr.sd["shape"]
  
  #k        =unlist(c(pr[c("k")]))
  #k.prior  =c(k,pr.sd["k"])
  
  if (!is.null(q_bounds))
    args=list(q_bounds=q_bounds)
  else
    args=list()
                  
  args=c(args,list(
    scenario  =scenario,
    assessment=assessment,
    model.type=model,
    BmsyK     =shape,
    shape.CV = shape.cv,
    catch     =catch,
    cpue      =index,
    
    r.prior   =r.prior,
    #K.prior   =k.prior,
    psi.prior =psi.prior,
    
    sigma.proc =sigma.proc,
    sigma.est  =sigma.est,
    fixed.obsE =fixed.obsE,
    igamma     =igamma,
    
    verbose   =FALSE))
  
  if (substr(currentDepletion[1],1,1)=="b")  
    args=c(args,list(b.prior=c(c(pr["ssb.maxyear"]), pr.sd["ssb.maxyear"], max(catch$year), "bbmsy")))
  if (substr(currentDepletion[1],1,1)=="f")  
    args=c(args,list(b.prior=c(c(pr["ssb.maxyear"]), pr.sd["ssb.maxyear"], max(vatch$year), "ffmsy")))

  if (!is.na(initialDepletion)){
  #    aux=list(auxiliary.se  =NULL,
  #             auxiliary.type="ffmsy",
  #             auxiliary     =cbind(year=catch$year,index=NA))
  #    aux$auxiliary[1,"index"]=initialDepletion
      }
  else
    aux=auxFn(...)
  
  args=c(args,aux)
  
  ## Fit with Catch + Index: Simple Fox with r = Fmsy
  input=try(do.call("build_jabba", args))
  
  if ("try-error"%in%is(input)) return(NULL)
  
  fit=try(fit_jabba(input,quickmcmc=T,verbose=F))
  
  if ("try-error"%in%is(fit)) list(input=input)
  
  list(input=input,fit=fit)}



jabbaExtractFn<-function(x) {
  # Return NULL if x is NULL
  if (is.null(x)) return(NULL)
  
  # Safely extract posteriors with NULL checking
  posteriors <- tryCatch({
    if (!is.null(x$fit)) {
      cbind(x$fit$pars_posterior,
            x$fit$refpts_posterior,
            x$fit$kobe)
    } else NULL
  }, error = function(e) NULL)
  
  # Safely extract trajectory
  trajectory <- tryCatch({
    if (!is.null(x$fit)) x$fit$kbtrj else NULL
  }, error = function(e) NULL)
  
  names(trajectory)[names(trajectory)=="yr"]
  
  # Safely extract priors
  priors <- tryCatch({
    if (!is.null(x$fit$settings)) {
      prior_vals <- c(unlist(x$fit$settings[c("r.pr","K.pr","psi.pr")]),
                      unlist(x$fit$settings[c("mu.m","m.CV")]))
      names(prior_vals) <- c("r","r.pr","k","k.pr","psi","psi.pr","m","m.pr")
      prior_vals
    } else NULL
  }, error = function(e) NULL)
  
  list(posteriors=posteriors,
       trajectory=trajectory,
       priors=priors)
}

jabbaExtractList<-function(jabbaList) {
  library(data.table)
  
  # Extract all elements with NULL checking
  results <- lapply(names(jabbaList), function(nm) {
    res <- jabbaExtract(jabbaList[[nm]])
    if (is.null(res)) return(NULL)
    
    # Add identifier column to each component if not NULL
    if (!is.null(res$posteriors)) 
      res$posteriors <- cbind(Scenario=nm, as.data.frame(res$posteriors))
    if (!is.null(res$trajectory)) 
      res$trajectory <- cbind(Scenario=nm, as.data.frame(res$trajectory))
    if (!is.null(res$priors)) 
      res$priors <- cbind(Scenario=nm, as.data.frame(t(res$priors)))
    
    return(res)
  })
  
  # Remove NULL elements
  results <- results[!sapply(results, is.null)]
  
  # Combine into data frames, handling empty lists
  combined <- list(
    posteriors = if (length(unlist(lapply(results, function(x) x$posteriors))) > 0) 
      rbindlist(lapply(results, function(x) x$posteriors), fill=TRUE) else NULL,
    trajectory = if (length(unlist(lapply(results, function(x) x$trajectory))) > 0) 
      rbindlist(lapply(results, function(x) x$trajectory), fill=TRUE) else NULL,
    priors = if (length(unlist(lapply(results, function(x) x$priors))) > 0) 
      rbindlist(lapply(results, function(x) x$priors), fill=TRUE) else NULL
  )
  
  names(combined$trajectory)[names(combined$trajectory)=="yr"]
  
  return(combined)
}

jabbaExtractLists<-function(listOflists) {
  library(data.table)
  
  # Initialize empty lists for each component
  allPosteriors=list()
  allTrajectory=list()
  allPriors    =list()
  
  # Loop through the outer list
  for(.id in names(listOflists)) {
    # Get the inner list
    innerList <- listOflists[[.id]]
    if (is.null(innerList)) next
    
    # Loop through inner lists
    for(Scenario in names(innerList)) {
      # Extract data using your original function
      res <- jabbaExtract(innerList[[Scenario]])
      if (is.null(res)) next
      
      # Add identifiers to non-NULL components
      if (!is.null(res$posteriors)) {
        res$posteriors <- cbind(
          .id=.id,
          Scenario=Scenario,
          as.data.frame(res$posteriors))
        allPosteriors[[paste(.id, Scenario, sep="_")]] <- res$posteriors
      }
      
      if (!is.null(res$trajectory)) {
        res$trajectory <- cbind(
          .id=.id,
          Scenario=Scenario,
          as.data.frame(res$trajectory))
        allTrajectory[[paste(.id, Scenario, sep="_")]] <- res$trajectory
      }
      
      if (!is.null(res$priors)) {
        res$priors <- cbind(
          .id=.id,
          Scenario=Scenario,
          as.data.frame(t(res$priors)))
        allPriors[[paste(.id, Scenario, sep="_")]] <- res$priors
      }
    }
  }
  
  # Combine into final data frames, handling empty lists
  combined <- list(
    posteriors = if (length(allPosteriors) > 0) rbindlist(allPosteriors, fill=TRUE) else NULL,
    trajectory = if (length(allTrajectory) > 0) rbindlist(allTrajectory, fill=TRUE) else NULL,
    priors = if (length(allPriors) > 0) rbindlist(allPriors, fill=TRUE) else NULL
  )
  
  
  names(combined$trajectory)[names(combined$trajectory)=="yr"]
  
  return(combined)}

# Define generic
setGeneric("jabbaExtract", function(object, ...) standardGeneric("jabbaExtract"))

# Method with correct signature specification
setMethod("jabbaExtract", signature(object="ANY"),definition=function(object, ...) {
  if (is.null(object)) return(NULL)
  
  if (is.list(object)) {
    if ("input" %in% names(object) && "fit" %in% names(object)) 
      return(jabbaExtractFn(object))
    else if (!is.null(object[[1]]) && all(c("input","fit") %in% names(object[[1]]))) 
      return(jabbaExtractList(object))
    else if (!is.null(object[[1]][[1]]) && all(c("input","fit") %in% names(object[[1]][[1]]))) 
      return(jabbaExtractLists(object))}
  
  return(NULL)})