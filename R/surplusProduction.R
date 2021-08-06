
## Calculates the surplus production and expected yield etc for the estinates of SSB and biomass
surplusProduction<-function(x){
  nms=dimnames(refpts(x))
  nms$refpt=paste("ssb",dimnames(ssb.obs(x))$year,sep="")
  
  rfs=FLPar(array(NA,laply(nms,length),dimnames=nms))
  rfs[,"ssb",]=ssb.obs(x)
  refpts(x)=rfs
  rtn=computeRefpts(x)
  
  rtn=alply(rtn,2,FLQuant,dimnames=dimnames(ssb.obs(x)))
  names(rtn)=as.character(unlist(attributes(rtn)$split_labels))
  
  discards.obs(x)[is.na(discards.obs(x))]=0
  
  rtn$spSSB=ssb.obs(x)[,-1]-ssb.obs(x)[,-dim(ssb.obs(x))[2]]+catch.obs(x)[,-dim(ssb.obs(x))[2]]
  rtn$spBiomass=biomass.obs(x)[,-1]-biomass.obs(x)[,-dim(biomass.obs(x))[2]]+catch.obs(x)[,-dim(biomass.obs(x))[2]]
  
  rtn=mcf(as(rtn,"FLQuants"))
  
  rtn[["peSSB"]]=rtn$spSSB-rtn$yield
  rtn[["peBiomass"]]=rtn$spBiomass-rtn$yield
  
  rtn}
