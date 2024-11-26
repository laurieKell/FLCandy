
#' @rdname rebuild
#' @export
setMethod("rebuild", signature(object="FLBRP"),
          function(object, targetF=computeRefpts(object)["msy","harvest"] * 0,
                   targetSSB=computeRefpts(object)["msy","ssb"],
                   nInitial=100, growthRate=0.25, minVal=1e-6, maxVal=1,
                   burnin=20, truncate=TRUE) {
            
            # Input validation
            if (!is(object, "FLBRP"))
              stop("object must be an FLBRP object")
            
            if (!all(sapply(list(nInitial, burnin), function(x) is.numeric(x) && x > 0)))
              stop("nInitial and burnin must be positive integers")
            
            if (!all(sapply(list(growthRate, minVal, maxVal), is.numeric)))
              stop("growthRate, minVal, and maxVal must be numeric")
            
            if (minVal >= maxVal)
              stop("minVal must be less than maxVal")
            
            # Generate SSB sequence
            targetSSB=c(targetSSB) * seq(minVal^growthRate, maxVal^growthRate, 
                                            length.out=nInitial)^(1/growthRate)
            targetF=c(targetF)
            
            # Setup equilibrium
            eql=object
            fbar(eql)[]=0.2
            
            # Create target biomass array
            btar=FLQuant(rep(targetSSB, each=dim(fbar(eql))[2]),
                            dimnames=dimnames(propagate(ssb(eql), nInitial)))
            
            # Project stock
            stk=propagate(as(eql, "FLStock"), nInitial)
            stk=fwd(stk, ssb_end=btar[,-seq(dims(stk)[["min"]]+2)], sr=eql)
            
            # Apply target F
            ftar=fbar(stk) %=% targetF
            stk=fwd(stk, f=ftar[,-seq(burnin)], sr=eql)
            
            # Post-process
            if (truncate) 
              stk=stk[,-seq(burnin)]
            
            stk=qapply(stk, function(x) {
              dimnames(x)$year=seq(length(dimnames(x)$year))
              x
            })
            
            return(stk)
          })

#' Calculate rebuilding time
#'
#' @param object An FLStock object
#' @param nx Number of interpolation points (default = 101)
#' @return A data frame with columns year and initial
#' @export
setMethod("rebuildTime", signature(object="FLStock"),
          function(object, nx=101) {
            if (!is(object, "FLStock"))
              stop("object must be an FLStock object")
            
            if (!is.numeric(nx) || nx <= 0)
              stop("nx must be a positive integer")
            
            bmsy=c(ssb(object)[,1,,,,dim(object)[6]])
            
            dat=transmute(as.data.frame(ssb(object), drop=TRUE),
                             ssb=data/bmsy,
                             initial=c(ssb(object[,1]))[an(ac(iter))]/bmsy,
                             year=year)
            
            rtn=suppressWarnings(
              as.data.frame(with(dat, 
                                 akima::interp(x=initial, y=ssb, z=year, yo=1,
                                               duplicate="mean", nx=nx, jitter=1e-6)))[,c(3,1)])
            
            names(rtn)=c("year", "initial")
            return(data.frame(rtn))
          })
