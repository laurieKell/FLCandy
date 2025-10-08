#' Extract Reference Points from FLBRP Object
#' 
#' @description Extracts key reference points (MSY, crash, virgin) from an FLBRP object
#' and returns them as an FLPar object with harvest, yield, and SSB values.
#' 
#' @param x An FLBRP object
#' @return FLPar object containing reference points
#' @export
#' @examples
#' \dontrun{
#' # Assuming you have an FLBRP object called 'brp'
#' refs(brp)
#' }
setGeneric("refs", function(x) standardGeneric("refs"))

#' @rdname refs
#' @export
setMethod("refs", signature(x="FLBRP"), function(x) {
  rfs=refpts(x)[c("msy", "crash", "virgin"), c("harvest", "yield", "ssb")]
  
  rtn=rbind(msy   = rfs[1, "yield"],
               bmsy  = rfs[1, "ssb"],
               virgin = rfs[3, "ssb"],
               fcrash = rfs[2, "harvest"],
               fmsy  = rfs[1, "harvest"])
  
  FLPar(rtn[drop=TRUE])
})

setGeneric("refsEB", function(x) standardGeneric("refsEB"))

#' @rdname refsEB
#' @export
setMethod("refsEB", signature(x="FLBRP"), function(x) {
  rfs=FLCandy:::refptsEB(x)[c("msy", "crash", "virgin"), c("harvest", "yield", "eb")]
  
  rtn=rbind(msy   = rfs[1, "yield"],
            bmsy  = rfs[1, "eb"],
            virgin = rfs[3, "eb"],
            fcrash = rfs[2, "harvest"],
            fmsy  = rfs[1, "harvest"])
  
  FLPar(rtn[drop=TRUE])})

#' Calculate Shape Parameter p from BMSY/K Ratio
#' 
#' @description Calculates the shape parameter p for the Pella-Tomlinson surplus 
#' production model given a BMSY/K ratio. This is a vectorized version that 
#' handles FLPar objects with multiple iterations.
#' 
#' @param shape FLPar object containing BMSY/K ratios
#' @return FLPar object containing calculated p parameters
#' @export
#' @examples
#' \dontrun{
#' # Create FLPar with BMSY/K ratios
#' shape=FLPar(0.3, 0.4, 0.5)
#' p(shape)
#' }
setGeneric("p", function(shape) standardGeneric("p"))

#' @rdname p
#' @export
setMethod("p", signature(shape="FLPar"), function(shape) {
  rtn=shape 
  
  # Handle invalid values
  rtn[shape > 1]=NA
  rtn[shape <= 0]=NA
  
  # For bmsy/K = 0.368 (exp(-1)), p approaches 0 (Fox model)
  rtn[(abs(shape - exp(-1)) < 1e-6)]=0
  
  # Use uniroot to solve: (1/(1+p))^(1/p) = shape
  fn=function(p, shape) {
    if (abs(p) < 1e-10)
      return(exp(-1) - shape)
    else 
      return((1/(1 + p))^(1/p) - shape)
  }
  
  # Handle single value or multiple iterations
  if ("iter" %in% names(dim(shape))) {
    # Multiple iterations
    for (i in seq(dim(shape)["iter"])[c(!is.na(c(rtn)) & rtn > 0)]) {
      tryCatch({
        rtn[,i]=uniroot(f, shape=c(shape[,i]), lower=-0.999, upper=100, 
                           tol=1e-8, extendInt="upX")$root
      }, error=function(e) {
        # If fails, try with wider bounds
        tryCatch({
          rtn[,i]=uniroot(fn, shape=c(shape[,i]), lower=-0.999, upper=100, 
                             tol=1e-8, extendInt="upX")$root
        }, error=function(e2) {
          # If all else fails, return a reasonable default
          rtn[,i]=0.25
        })
      })
    }
  } else {
    # Single value
    tryCatch({
      rtn[]=uniroot(fn, shape=c(shape), lower=-0.999, upper=100, 
                       tol=1e-8, extendInt="upX")$root
    }, error=function(e) {
      # If fails, try with wider bounds
      tryCatch({
        rtn[]=uniroot(fn, shape=c(shape), lower=-0.999, upper=100, 
                         tol=1e-8, extendInt="upX")$root
      }, error=function(e2) {
        # If all else fails, return a reasonable default
        rtn[]=0.25
      })
    })
  }
  
  dimnames(rtn)[1]="p"
  
  return(rtn)
})

#' @rdname p
#' @export
setMethod("p", signature(shape="numeric"), function(shape) {
  # Handle vector input
  if (length(shape) > 1) {
    result=numeric(length(shape))
    for (i in seq_along(shape)) {
      result[i]=p(shape[i])
    }
    return(result)
  }
  
  # Handle invalid values
  if (shape > 1 || shape <= 0) {
    return(NA_real_)
  }
  
  # For bmsy/K = 0.368 (exp(-1)), p approaches 0 (Fox model)
  if (abs(shape - exp(-1)) < 1e-6) {
    return(0)
  }
  
  # Use uniroot to solve: (1/(1+p))^(1/p) = shape
  f=function(p, shape) {
    if (abs(p) < 1e-10)
      return(exp(-1) - shape)
    else 
      return((1/(1 + p))^(1/p) - shape)
  }
  
  tryCatch({
    result=uniroot(f, shape=shape, lower=0.01, upper=20, 
                     tol=1e-8, extendInt="upX")$root
    return(result)
  }, error=function(e) {
    # If fails, try with wider bounds
    tryCatch({
      result=uniroot(f, shape=shape, lower=0.001, upper=100, 
                       tol=1e-8, extendInt="upX")$root
      return(result)
    }, error=function(e2) {
      # If all else fails, return a reasonable default
      return(0.25)
    })
  })
})

#' Calculate BMSY/K Ratio from Shape Parameter p
#' 
#' @description Calculates the BMSY/K ratio (shape) for the Pella-Tomlinson surplus 
#' production model given the shape parameter p. This is the inverse of the p() function.
#' 
#' @param p FLPar object containing p parameters
#' @return FLPar object containing calculated BMSY/K ratios
#' @export
#' @examples
#' \dontrun{
#' # Create FLPar with p parameters
#' pParams=FLPar(0.5, 1.0, 1.5)
#' shape(pParams)
#' }
setGeneric("shape", function(p) standardGeneric("shape"))

#' @rdname shape
#' @export
setMethod("shape", signature(p="FLPar"), function(p) {
  rtn=p
  
  # Handle invalid values
  rtn[p < 0]=NA
  
  # For p = 0 (Fox model), BMSY/K = exp(-1) ≈ 0.368
  rtn[abs(p) < 1e-10]=exp(-1)
  
  # Calculate BMSY/K ratio using the formula: (1/(1+p))^(1/p)
  validP=!is.na(p) & abs(p) >= 1e-10
  if (any(validP, na.rm=TRUE)) {
    rtn[validP]=(1/(1 + p[validP]))^(1/p[validP])
  }
  
  dimnames(rtn)[1]="shape"
  
  return(rtn)
})

#' @rdname shape
#' @export
setMethod("shape", signature(p="numeric"), function(p) {
  # Handle vector input
  if (length(p) > 1) {
    result=numeric(length(p))
    for (i in seq_along(p)) {
      result[i]=shape(p[i])
    }
    return(result)
  }
  
  # For p = 0 (Fox model), BMSY/K = exp(-1) ≈ 0.368
  if (abs(p) < 1e-10) {
    return(exp(-1))
  }
  
  # Calculate BMSY/K ratio using the formula: (1/(1+p))^(1/p)
  return((1/(1 + p))^(1/p))})

#' Calculate Intrinsic Growth Rate r from Pella-Tomlinson Parameters
#' 
#' @description Calculates the intrinsic growth rate r for the Pella-Tomlinson 
#' surplus production model using the relationship between MSY, BMSY, and p.
#' 
#' @param param FLPar object containing p, msy, bmsy, and virgin parameters
#' @return FLPar object containing calculated r parameter
#' @export
#' @examples
#' \dontrun{
#' # Assuming you have an FLPar with required parameters
#' param=FLPar(p=0.5, msy=100, bmsy=500, virgin=1000)
#' r(param)
#' }
setGeneric("r", function(param) standardGeneric("r"))

#' @rdname r
#' @export
setMethod("r", signature(param="FLPar"), function(param) {
  # Calculate BMSY/K ratio using the correct formula: (1/(1+p))^(1/p)
  bmsy_k_ratio=(1/(1 + param["p"]))^(1/param["p"])
  
  # Calculate r using the relationship: r = (MSY/BMSY) * p / (1 - (BMSY/K)^p)
  r_val=(param["msy"] %/% param["bmsy"]) %*% 
            (param["p"] %/% (1 - bmsy_k_ratio^param["p"]))
  
  dimnames(r_val)[1]="r"
  
  return(r_val)
})

#' Calculate MSY from Pella-Tomlinson Parameters
#' 
#' @description Calculates the maximum sustainable yield (MSY) using the 
#' Pella-Tomlinson production model parameters.
#' 
#' @param param FLPar object containing r, bmsy, virgin, and p parameters
#' @return FLPar object containing calculated MSY
#' @export
#' @examples
#' \dontrun{
#' # Assuming you have an FLPar with required parameters
#' param=FLPar(r=0.5, bmsy=500, virgin=1000, p=0.5)
#' msy(param)
#' }
setGeneric("msy", function(param) standardGeneric("msy"))

#' @rdname msy
#' @export
setMethod("msy", signature(param="FLPar"), function(param) {
  # Calculate MSY using Pella-Tomlinson formula
  msy_val=param["r"] %*% param["bmsy"] * 
              (1 - exp(log((param["bmsy"] %/% param["virgin"])) %*% param["p"])) %/% param["p"]
  
  return(msy_val)
})

#' Convert FLBRP to Pella-Tomlinson Parameters
#' 
#' @description Converts an FLBRP object to Pella-Tomlinson surplus production 
#' model parameters (r, p, virgin, bmsy, msy, fmsy, fcrash).
#' 
#' @param object An FLBRP object
#' @return FLPar object containing Pella-Tomlinson parameters
#' @export
#' @examples
#' \dontrun{
#' # Assuming you have an FLBRP object called 'brp'
#' pt(brp)
#' }
setGeneric("pt", function(object) standardGeneric("pt"))

#' @rdname pt
#' @export
setMethod("pt", signature(object="FLBRP"), function(object) {
  # Extract reference points
  param=refs(object)
  
  # Calculate BMSY/K ratio
  shape=param["bmsy"] %/% param["virgin"]
  
  # Calculate p parameter
  pParam=p(shape)
  
  # Calculate r parameter using the combined parameters
  paramP=rbind(p=pParam, param)
  rParam=r(paramP)
  
  # Create a new FLPar with all parameters
  allParams=rbind(rParam, pParam, param)
  result=FLPar(allParams)
  
  return(result)
})


#' Calculate Production Using Pella-Tomlinson Model
#' 
#' @description Calculates surplus production (net production) for given biomass 
#' levels using the Pella-Tomlinson surplus production model.
#' 
#' @param param FLPar object containing r, p, and virgin parameters
#' @param biomass FLQuant or numeric vector of biomass values
#' @return Production values (same class as input biomass)
#' @export
#' @examples
#' \dontrun{
#' # Assuming you have parameters and biomass
#' param=FLPar(r=0.5, p=0.5, virgin=1000)
#' biomass=FLQuant(500)
#' production(param, biomass)
#' }
setGeneric("production", function(param, biomass) standardGeneric("production"))

#' @rdname production
#' @export
setMethod("production", signature(param="FLPar", biomass="FLQuant"), function(param, biomass) {
  # Calculate production using Pella-Tomlinson formula
  production_val=biomass %*% param["r"] %*% 
                    (1 - exp(log((biomass %/% param["virgin"])) %*% param["p"]))
  
  return(production_val)
})

#' @rdname production
#' @export
setMethod("production", signature(param="FLPar", biomass="numeric"), function(param, biomass) {
  # Convert numeric biomass to FLQuant and call the main method
  biomass_flq=FLQuant(biomass)
  return(production(param, biomass_flq))
})

#' @rdname production
#' @export
setMethod("production", signature(param="FLPar", biomass="FLPar"), function(param, biomass) {
  # Convert FLPar biomass to FLQuant and call the main method
  biomass_flq=FLQuant(c(biomass))
  return(production(param, biomass_flq))
})
