#' Calculate Virgin and MSY Reference Points from Production Curves
#' 
#' @description
#' Calculates reference points by comparing unfished (virgin) state with Maximum 
#' Sustainable Yield (MSY) state using production curves. Extracts metrics for fishing
#' mortality, catch, and exploitable biomass at both states.
#'
#' @param object An FLBRP object containing stock-recruitment and yield calculations
#'
#' @details
#' The method:
#' - Sets F=0 for virgin state and F=FMSY for MSY state
#' - Calculates key metrics (F, catch, exploitable biomass) at both states
#' - Excludes SSB metrics from final output
#'
#' @return
#' A named numeric vector containing:
#' - virgin.f: Fishing mortality in virgin state (≈0)
#' - virgin.catch: Catch in virgin state
#' - virgin.ebiomass: Exploitable biomass in virgin state
#' - msy.f: Fishing mortality at MSY
#' - msy.catch: Catch at MSY
#' - msy.ebiomass: Exploitable biomass at MSY
#'
#' @examples
#' \dontrun{
#' data(ple4)
#' brp <- FLBRP(ple4)
#' ref_pts <- msyVirgin(brp)
#' }
#'
#' @importFrom FLCore fbar ssb catch metrics model.frame refpts
#'
#' @export
#'
#' @aliases msyVirgin,FLBRP-method
#'
#' @seealso
#' \code{\link{FLBRP}} \code{\link{refpts}} \code{\link{metrics}}
#' 
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
