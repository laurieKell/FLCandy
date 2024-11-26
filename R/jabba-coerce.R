
jabba2biodyn<-function(object, phase=c("b0"=-1,"r"=4,"k"=3,"p"=-2,"q"=2,"sigma"=1),
                               min=0.1,max=10){
    
  res=biodyn()
  res@name     =paste(object$assessment,object$scenario)
  
  res@desc     ="coerced from JABBA"
  
  if (!("timeseries"%in%names(object))&"catch"%in%names(object)){
    catch(res)   =as.FLQuant(transmute(object$catch,year=Yr,data=Total))
    return(res)}
  
  params(res)[]=object$pars[c("r","K","m","psi"),"Median"]-c(0,0,1,0)
  catch(res)   =as.FLQuant(transmute(object$inputseries$catch,year=year,data=catch))
  res@stock    =as.FLQuant(data.frame(year=as.numeric(names(object$timeseries[,"mu","B"])),data=object$timeseries[,"mu","B"]))
  
  indices=list()
  for (i in dimnames(object$inputseries$cpue)[[2]][-1])
    indices[[i]]=as.FLQuant(data.frame(year=object$inputseries$cpue[,1],
                                       data=object$inputseries$cpue[,i]))
  res@indices=FLQuants(indices)

  #bug
  setParams(res)=res@indices      
  setControl(res,min=min,max=max)=res@params
  
  for (i in names(phase[names(phase)%in%dimnames(control(res))[[1]]]))
    control(res)[i,"phase"]=phase[i]
  
  if ("q"%in%names(phase))
    control(res)[grep("q",dimnames(control(res))[[1]]),"phase"]=phase["q"]
  
  if ("sigma"%in%names(phase))
    control(res)[grep("s",dimnames(control(res))[[1]]),"phase"]=phase["sigma"]

  res@kobe=object$kobe
  res@posteriors=object$pars_posterior
  
  if ("refpts_posterior"%in%names(object))
    res@posteriors=cbind(res@posteriors,object$refpts_posterior)
  
  if ("trj_posterior"%in%names(object))
    res@kobe=subset(object$trj_posterior,year==min(year)|year==max(year))
  
  return(res)}

setMethod("as.biodyn", signature(object="list"),
    function(object, ...) {
            
      jabba2biodyn(object)})

setAs(from='list', to='biodyn',  def=function(from) as.biodyn(from))

jabbas2biodyns<-function(from,
                         phase=c("b0"=-1,"r"=4,"k"=3,"p"=-1,"q"=2,"sigma"=1),
                         min=0.5,max=1.5)
  biodyns(llply(from,jabba2biodyn,phase=phase,min=min,max=max))
  
setAs(from='list', to='biodyns', def=function(from) jabbas2biodyns(from))