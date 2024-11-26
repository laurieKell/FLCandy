#' @rdname processError
setMethod("processError", signature(object="FLBRP"),
          function(object) {
            # Create reference points FLPar
            rfs = FLPar(c(ssb.obs(object)),
                        dimnames=list(refpts="ssb",
                                      quant=dimnames(refpts(object))$quant,
                                      iter=seq(dim(ssb.obs(object))[2])))
            rfs[,-4] = NA
            refpts(object) = rfs
            
            # Calculate metrics
            rtn = data.frame(model.frame(FLQuants(object, 
                                                  ssb=ssb.obs,
                                                  catch=catch.obs), 
                                         drop=TRUE),
                             sp=c(computeRefpts(object)[,"yield"]))
            
            # Calculate process error
            rtn$pe = (c(rtn$ssb[-1] - rtn$ssb[-dim(rtn)[1]] + 
                          rtn$catch[-dim(rtn)[1]] - 
                          rtn$sp[-dim(rtn)[1]], NA)) / rtn$ssb
            
            rtn=FLQuants(ssb =as.FLQuant(transmute(rtn,year=year, data=ssb)),
                        catch=as.FLQuant(transmute(rtn,year=year, data=catch)),
                        pe   =as.FLQuant(transmute(rtn,year=year, data=pe)),
                        sp   =as.FLQuant(transmute(rtn,year=year, data=sp)))
            
            return(rtn)})