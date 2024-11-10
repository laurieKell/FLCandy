ebiomass<-function(object){
  sel  =harvest(object)
  wt   =catch.wt(object)%*%sel%/%fapex(sel)
  eb.wt=qmax(wt,0.000001)
  
  apply(eb.wt%*%stock.n(object),2:6,sum)}

tseriesFn<-function(object) { 
  if (is.null(object)) return(NULL)
      
  cbind(year=as.numeric(dimnames(object[["timeseries"]][,"mu",])[[1]]),
        as.data.frame(object[["timeseries"]][,"mu",]),
        object[["refpts"]][1,c("k","bmsy","fmsy","msy")])}

tseriesFn2<-function(x) ldply(names(x), function(.id) { 
  if (is.null(x[[.id]][["fit"]])) return(NULL)
  
  cbind(.id=.id,tseriesFn(x[[.id]][["fit"]]))})

kobeFn<-function(object) { 
  if (is.null(object)) return(NULL)
  
  cbind(object$kobe,
        object$refpts_posterior,
        object$pars_posterior)[,-(1:3)]}

kobeFn2<-function(x) ldply(names(x), function(.id) { 
  if (is.null(x[[.id]][["fit"]])) return(NULL)
  
    cbind(.id=.id,kobeFn(x[[.id]][["fit"]]))})

priorsFn<-function(object){
  if (is.null(object)) return(NULL)
  
  rtn=unlist(object$jagsdata[c("r.pr","psi.pr","K.pr","mu.m","m.CV")])
  names(rtn)=c("r","r.pr","psi","psi.pr","K","K.pr","m","m.pr")
  rtn}

priorsFn2<-function(x) 
  ldply(names(x), function(.id) { 
    if (is.null(x[[.id]])) return(NULL)
    if (is.null(x[[.id]][["input"]])) return(NULL)
    rtn=c(.id,unlist(x[[.id]][["input"]]$jagsdata[c("r.pr","psi.pr","K.pr","mu.m","m.CV")]))
    names(rtn)=c(".id","r","r.pr","psi","psi.pr","K","K.pr","m","m.pr")
      
    rtn})

posteriorsFn<-function(object){
  if (is.null(object)) return(NULL)
  
  cbind(object$kobe,
        object$pars_posterior,
        object$refpts_posterior)}

posteriorsFn2<-function(x) 
  ldply(names(x), function(.id) { 
    if (is.null(x[[.id]][["fit"]])) return(NULL)
    
    cbind(.id=.id,posteriorsFn(x[[.id]][["fit"]]))})


postPriorFn<-function(x) {
  rtn=try({
    prior=data.frame(par  =dimnames(x$fit$pars)[[1]],
                     prior=x$fit$pars[,"Median"])
    post =melt(x$fit$pars_posterior)
    rtn=merge(prior,post,by.y="variable",by.x="par")
    names(rtn)[3]="posterior"
    rtn})
  
  if ("try-error"%in%is(rtn)) return(NULL)
  rtn}

