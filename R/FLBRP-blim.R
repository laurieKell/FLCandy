setMethod("blim", signature(object="FLBRP"),
      function(object, ratio=0.3) {
            # Get virgin recruitment 
            rec = refpts(object)["virgin","rec",drop=TRUE] * ratio
            
            # Create new FLPar with NAs
            refpts(object) = FLPar(NA,
                                   dimnames=list(refpt="rec",
                                                 quant=c("harvest","yield","rec",
                                                         "ssb","biomass","revenue",
                                                         "cost","profit"),
                                                 iter=dimnames(refpts(object))$iter))
            
            # Set recruitment
            refpts(object)[,"rec"] = rec
            
            # Return computed reference points
            rtn = computeRefpts(object)["rec"]
            dimnames(rtn)$refpt = "blim"
            
            return(rtn)})