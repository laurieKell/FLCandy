setMethod("properties", signature(object="FLBRP"),
    function(object, ...) {
  
        msy=computeRefpts(object)["msy"]
  
        dmns=dimnames(msy)
        dmns$refpt=c(dmns$refpt,"0.5MSY","lower pgy","upper pgy","2*prod","virgin","crash")
        refpts(object)     =FLPar(NA,dimnames=dmns)
        refpts(object)["0.5MSY",   c("harvest","yield")]=msy[,c("harvest","yield")]*c(1.2,0.5)
        refpts(object)["lower pgy",c("harvest","yield")]=msy[,c("harvest","yield")]*c(1.2,0.8)
        refpts(object)["upper pgy",c("harvest","yield")]=msy[,c("harvest","yield")]*0.8
        refpts(object)["2*prod",   c("yield",  "ssb")]  =msy[,c("yield",  "ssb")]*c(2,1)
        
        computeRefpts(object)})
  
