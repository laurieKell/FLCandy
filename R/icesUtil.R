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
