auxFn<-function(lag=0,obsE=0.3,sigma=TRUE,type="",...){
  
  args=list(...)
  
  auxilary      =NULL
  
  if (length(args)>0)
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
  
jabbaWrapper<-function(catch,
                       pr,       
                       pr.sd     =pr/pr*0.3,
                       model     ="Pella_m",
                       assessment="",  scenario="",
                       index     =NULL,q_bounds=NULL,
                       sigma.proc=TRUE,
                       currentDepletion="",...){
  
  ## priors
  r        =unlist(c(pr[c("r")]))
  psi      =unlist(c(pr[c("ssb.minyr")]))
  if (is.na(psi)) psi=0.9
  shape    =unlist(c(pr[c("shape")]))
  r.prior  =c(r,  pr.sd["r"])
  psi.prior=c(psi,pr.sd["ssb.minyr"])
  
  if (!is.null(q_bounds))
    args=list(q_bounds=q_bounds)
  else
    args=list()
  
  args=c(args,list(
    scenario  =scenario,
    assessment=assessment,
    model.type=model,
    BmsyK     =shape,
    
    catch     =catch,
    cpue      =index,
    
    r.prior   =r.prior,
    psi.prior =psi.prior,
    
    sigma.proc=sigma.proc,
    verbose   =FALSE))
  
  if (substr(currentDepletion[1],1,1)=="b")  
    args=c(args,list(b.prior=c(c(pr["ssb.maxyr"]), pr.sd["ssb.maxyr"], max(om$year), "bbmsy")))
  if (substr(currentDepletion[1],1,1)=="f")  
    args=c(args,list(b.prior=c(c(pr["ssb.maxyr"]), pr.sd["f.maxyr"],   max(om$year), "ffmsy")))
  
  aux =auxFn(...)
  
  args=c(args,aux)
  
  ## Fit with Catch + Index: Simple Fox with r = Fmsy
  input=try(do.call("build_jabba", args))
  
  if ("try-error"%in%is(input)) return(NULL)
  
  fit=try(fit_jabba(input,quickmcmc=T,verbose=F))
  
  if ("try-error"%in%is(fit)) list(input=input)
  
  list(input=input,fit=fit)}
