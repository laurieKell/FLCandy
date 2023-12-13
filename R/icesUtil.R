benchmarks<-function(x) {
  if ("logical"%in%is(attributes(x)$benchmark))
    return(FLPar(Fmsy=NA,Flim=NA,Fpa=NA,Blim=NA,Bpa=NA,Btrigger=NA))
  
  if ("numeric"%in%is(attributes(x)$benchmark))
    attributes(x)$benchmark=FLPar(attributes(x)$benchmark)
  
  as(attributes(x)$benchmark,"FLPar")}

fishlife2lhPar<-function(x) {
  res=attributes(x)$fishlife
  
  if (!("fishlife"%in%names(attributes(x))))
    return(lhPar(FLPar(c("linf"=NA,"k"=NA,"l50"=NA,"s"=NA))))
  
  if ("lm"%in%names(res))
    names(res)[seq(length(res))[(names(res)=="lm")]]="l50"
  
  res=FLPar(res,units="NA")
  rtn=FLPar("linf"     =NA,
            "k"        =NA,       
            "winf"     =NA,       
            "tmax"     =NA,       
            "tm"       =NA,       
            "m"        =NA,      
            "lm"       =NA,       
            "rho"      =NA,       
            "sigmaR"   =NA,     
            "s"        =NA,     
            "fmsy"     =NA,     
            "r"        =NA,    
            "g"         =NA,     
            "sd.logit.s"=NA)
  
  lhPar(res[c("linf","k","l50","s")])}

#dat=ldply(llply(stks, fishlife2lhPar), function(x) if("try-error"%in%is(x)) NULL else model.frame(x)
