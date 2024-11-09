
jabbaFFmsy<-function(om,pr){
  catch=om[, c("year", "catch")]
  if (any(catch[,"catch"]<=0))
    catch[catch[,"catch"]<=0,"catch"]=1e-6
  
  index=transmute(om, year=year, index=eb)
  if (any(is.na(index[,"index"])))
    index[is.na(index[,2]),2]=NA
  
  minyr=an(om$year[1:5])
  maxyr=an(rev(om$year)[1:5])
  top  =om$year[    order(om$f) ][1:5]
  toe  =om$year[rev(order(om$f))][1:5]
  
  kb=NULL;ts=NULL
  
  rtn=try(jabbaWrapper(catch,pr,index=index))
  if (!("try-error"%in%is(rtn))&!is.null(rtn)){
    kb=cbind(What="Perfect Index",kbFn(  rtn))
    ts=cbind(What="Perfect Index",smryFn(rtn))}
  
  auxiliary=transmute(om, year=year, ffmsy=f/c(pr["fmsy"]))
  auxiliary.type="ffmsy"
  rtn=try(jabbaWrapper(catch,pr,auxiliary.type=auxiliary.type,
                                auxiliary     =auxiliary))
  if (!("try-error"%in%is(rtn))&!is.null(rtn)){
    kb=rbind.fill(kb,cbind(What="ffmsy",kbFn(  rtn)))
    ts=rbind.fill(ts,cbind(What="ffmsy",smryFn(rtn)))}
  
  auxiliary=transmute(om, year=year, ffmsy=f/c(pr["fmsy"]))
  auxiliary[seq(dim(auxiliary)[1]-5),"ffmsy"]=NA
  rtn=try(jabbaWrapper(catch,pr,auxiliary.type=auxiliary.type,
                       auxiliary     =auxiliary))
  if (!("try-error"%in%is(rtn))&!is.null(rtn)){
    kb=rbind.fill(kb,cbind(What="max",kbFn(  rtn)))
    ts=rbind.fill(ts,cbind(What="max",smryFn(rtn)))}
  
  auxiliary=transmute(om, year=year, ffmsy=f/c(pr["fmsy"]))
  auxiliary[!auxiliary$year%in%unique(c(minyr,maxyr)),"ffmsy"]=NA
  rtn=try(jabbaWrapper(catch,pr,auxiliary.type=auxiliary.type,
                       auxiliary     =auxiliary))
  if (!("try-error"%in%is(rtn))&!is.null(rtn)){
    kb=rbind.fill(kb,cbind(What="min max",kbFn(  rtn)))
    ts=rbind.fill(ts,cbind(What="min max",smryFn(rtn)))}
  
  auxiliary=transmute(om, year=year, ffmsy=f/c(pr["fmsy"]))
  auxiliary[!auxiliary$year%in%unique(c(minyr,maxyr-5,maxyr)),"ffmsy"]=NA
  rtn=try(jabbaWrapper(catch,pr,auxiliary.type=auxiliary.type,
                       auxiliary     =auxiliary))
  if (!("try-error"%in%is(rtn))&!is.null(rtn)){
    kb=rbind.fill(kb,cbind(What="min max 10",kbFn(  rtn)))
    ts=rbind.fill(ts,cbind(What="min max 10",smryFn(rtn)))}
  
  auxiliary=transmute(om, year=year, ffmsy=f/c(pr["fmsy"]))
  auxiliary[!auxiliary$year%in%unique(c(maxyr,toe)),"ffmsy"]=NA
  rtn=try(jabbaWrapper(catch,pr,auxiliary.type=auxiliary.type,
                       auxiliary     =auxiliary))
  if (!("try-error"%in%is(rtn))&!is.null(rtn)){
    kb=rbind.fill(kb,cbind(What="max toe",kbFn(  rtn)))
    ts=rbind.fill(ts,cbind(What="max toe",smryFn(rtn)))}
  
  auxiliary=transmute(om, year=year, ffmsy=f/c(pr["fmsy"]))
  auxiliary[!auxiliary$year%in%unique(c(maxyr,top)),"ffmsy"]=NA
  rtn=try(jabbaWrapper(catch,pr,auxiliary.type=auxiliary.type,
                       auxiliary     =auxiliary))
  if (!("try-error"%in%is(rtn))&!is.null(rtn)){
    kb=rbind.fill(kb,cbind(What="max top",kbFn(  rtn)))
    ts=rbind.fill(ts,cbind(What="max top",smryFn(rtn)))}
  
  auxiliary=transmute(om, year=year, ffmsy=f/c(pr["fmsy"]))
  auxiliary[!auxiliary$year%in%unique(c(maxyr,top,toe)),"ffmsy"]=NA
  rtn=try(jabbaWrapper(catch,pr,auxiliary.type=auxiliary.type,
                       auxiliary     =auxiliary))
  if (!("try-error"%in%is(rtn))&!is.null(rtn)){
    kb=rbind.fill(kb,cbind(What="max top toe",kbFn(  rtn)))
    ts=rbind.fill(ts,cbind(What="max top toe",smryFn(rtn)))}
  
  
  auxiliary=transmute(om, year=year, ffmsy=f/c(pr["fmsy"]))
  auxiliary[!auxiliary$year%in%unique(c(minyr,maxyr,top,toe)),"ffmsy"]=NA
  rtn=try(jabbaWrapper(catch,pr,auxiliary.type=auxiliary.type,
                       auxiliary     =auxiliary))
  if (!("try-error"%in%is(rtn))&!is.null(rtn)){
    kb=rbind.fill(kb,cbind(What="min max top toe",kbFn(  rtn)))
    ts=rbind.fill(ts,cbind(What="min max top toe",smryFn(rtn)))}
  
  return(list(kobe=kb,tseries=ts))}


