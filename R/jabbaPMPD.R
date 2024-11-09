jabbaPMPD<-function(catch,
                  pr,       
                  pr.sd     =pr/pr*0.3,
                  model     ="Pella_m", assessment="PMPD",  
                  scenario  ="",
                  sigma.proc=TRUE,
                  #index     =NULL,q_bounds=NULL,
                  fix="",...){
  
  ## priors
  r        =unlist(c(pr["r"]))
  r.prior  =c(r,  pr.sd["r"])
  
  psi      =unlist(c(pr["initial"]))
  psi.prior=c(psi,pr.sd["initial"])
  
  m        =unlist(c(pr[c("m")]))
  shape    =1-1/m
  
  ## Fixed values
  if (substr(fix,1,1)=="b")  
    args=list(b.prior=c(pr[,"current"]), pr.sd["current"], max(catch$year), "bbmsy")
  if (substr(fix,1,1)=="f")  
    args=list(b.prior=c(pr[,"fmax"]), pr.sd["fmax"], max(catch$year), "ffmsy")
  
  #if (!is.null(q_bounds))
  #  args=list(q_bounds=q_bounds)
  #else
  args=list()
  
  args=c(args,list(
    scenario  =scenario,
    assessment=assessment,
    model.type=model,
    BmsyK     =shape,
    
    catch     =catch,
    cpue      =NULL,
    
    r.prior   =r.prior,
    psi.prior =psi.prior,
    
    sigma.proc=sigma.proc,
    verbose   =FALSE))
  
  #aux =auxFn(...)
  #args=c(args,aux)
  
  ## Fit with Catch Only: Simple Fox with r = Fmsy
  input=try(do.call("build_jabba", args))
  if ("try-error"%in%is(input)) return(NULL)
  
  fit=try(fit_jabba(input,quickmcmc=T,verbose=F))
  if ("try-error"%in%is(fit)) return(NULL)
  
  list(input=input,fit=fit)}

