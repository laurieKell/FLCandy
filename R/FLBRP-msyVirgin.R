setMethod("msyVirgin", signature(object="FLBRP"),
          function(object) {
            
            # Set F values for virgin and MSY states
            fbar(object) = fbar(object)[,1:2]
            fbar(object)[] = c(1e-12, refpts(object)["msy","harvest"])
            
            # Calculate metrics
            rtn = melt(t(model.frame(metrics(object, 
                                             list(f = fbar,
                                                  ssb = ssb,
                                                  catch = function(x) catch(x),
                                                  ebiomass = function(x) ebiomass(x))),
                                     drop=TRUE)[,-1]))
            
            # Format output
            value = rtn$value
            names(value) = paste(c(rep("virgin",each=4), rep("msy",each=4)), rtn$X1, sep=".")
            
            # Return values excluding SSB metrics
            return(value[-c(1,3)])
          })
