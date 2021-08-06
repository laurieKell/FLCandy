## Adds the ICES PA & MSY reference points to refpts and fits a SRRR
addRefs<-function(x,refs){
  x=FLPar(NA,dimnames=list(refpt=c(dimnames(x)$refpt,dimnames(refs)$params),
                           quant=c("harvest","yield","rec","ssb","biomass","revenue","cost","profit"),iter=1))
  
  x[unlist(gregexpr("B",dimnames(x)$refpt))==1,"ssb"]    =refs[unlist(gregexpr("B",dimnames(refs)[[1]])>0),]
  x[unlist(gregexpr("F",dimnames(x)$refpt))==1,"harvest"]=refs[unlist(gregexpr("F",dimnames(refs)[[1]])>0),]
  
  x} 


