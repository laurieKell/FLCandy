#' Calculate the reference age for a FLBRP object.
#'
#' This function calculates the reference age for a FLBRP object.
#'
#' @param object FLBRP object.
#' @param ref Reference point, e.g., "msy" (default) or "f0.1".
#' @param p Probability threshold (default = 0.9).
#'
#' @return An FLQuant object containing reference ages.
#'
#' @export
setGeneric("abiAge", function(object, ref = "msy", p = 0.9) {
  standardGeneric("abiAge")
})


#' Calculate P obs for a FLStock object.
#'
#' This function calculates P obs for a FLStock object.
#'
#' @param object A FLStock object.
#' @param age Reference ages obtained from abiAge.
#'
#' @return An FLQuant object containing P obs.
#'
#' @export
setGeneric("abi", function(object, age, ...) {
  standardGeneric("abi")})


#' Calculate P(N) at FMSY for a FLBRP object.
#'
#' This function calculates P(N) at FMSY for a FLBRP object.
#'
#' @param y A FLBRP object.
#' @param ref Reference point, e.g., "msy" (default) or "f0.1".
#' @param p Probability threshold (default = 0.9).
#'
#' @return An FLQuant object containing P(N) at FMSY.
#'
#' @export
setGeneric("abiMsy", function(object, ref = "msy", p = 0.9) {
  standardGeneric("abiMsy")
})


#' Calculate P(N) at FMSY for a FLBRP object.
#'
#' This function calculates P(N) at FMSY for a FLBRP object.
#'
#' @param y A FLBRP object.
#' @param ref Reference point, e.g., "msy" (default) or "f0.1".
#' @param p Probability threshold (default = 0.9).
#'
#' @return An FLQuant object containing P(N) at FMSY.
#'
#' @export
setGeneric("abiMsy", function(object, ref = "msy", p = 0.9) {
  standardGeneric("abiMsy")
})

setGeneric("crosstest", function(object,...)
  standardGeneric("crosstest"))

#' Calculate Blim Reference Point
#' 
#' @param object An FLBRP object
#' @param ratio Ratio of virgin recruitment to use (default 0.3)
#' @return An FLPar object containing Blim reference points
setGeneric("blim", function(object, ...) standardGeneric("blim"))


#' Calculate MSY and Virgin State Metrics
#'
#' @description 
#' Calculates key metrics (F, SSB, catch, ebiomass) at virgin and MSY states for an FLBRP object
#'
#' @param object An object of class FLBRP
#'
#' @return A named vector containing metrics for virgin and MSY states:
#'   \item{virgin.f}{F at virgin state}
#'   \item{virgin.catch}{Catch at virgin state}
#'   \item{virgin.ebiomass}{Equilibrium biomass at virgin state}
#'   \item{msy.f}{F at MSY}
#'   \item{msy.catch}{Catch at MSY}
#'   \item{msy.ebiomass}{Equilibrium biomass at MSY}
#'
#' @note The function excludes virgin.ssb and msy.ssb from the output
#'
#' @export
#'
#' @rdname msyVirgin
#' 
#' @examples
#' \dontrun{
#' data(ple4brp)
#' msyVirgin(ple4brp)
#' }
setGeneric("msyVirgin", function(object, ...) standardGeneric("msyVirgin"))
#' Calculate Process Error for FLBRP Object
#'
#' @description 
#' Calculates process error from observed SSB and catch data compared to predicted surplus production
#'
#' @param object An object of class FLBRP
#'
#' @return A data frame containing:
#'   \item{ssb}{Observed spawning stock biomass}
#'   \item{catch}{Observed catch}
#'   \item{sp}{Predicted surplus production}
#'   \item{pe}{Process error calculated as (SSB[t+1] - SSB[t] + Catch[t] - SP[t])/SSB[t]}
#'
#' @note Process error quantifies the deviation between observed and predicted population dynamics
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data(ple4brp)
#' processError(ple4brp)
#' }
setGeneric("processError", function(object, ...) standardGeneric("processError"))

#' Extract Time Series Statistics from FLStock Objects
#'
#' @description
#' Creates a data frame of time series statistics from FLStock objects, including
#' catch, ebiomass, SSB, fishing mortality, harvest rate, and mean natural mortality.
#'
#' @param object An object of class FLStock or FLStocks
#' @param ... Additional arguments (not currently used)
#'
#' @return A data frame containing time series of:
#'   \itemize{
#'     \item catch - Catch values
#'     \item eb - Exploitable biomass
#'     \item ssb - Spawning stock biomass
#'     \item f - Fishing mortality (Fbar)
#'     \item h - Harvest rate (catch/ebiomass)
#'     \item m - Mean natural mortality
#'   }
#'
#' @examples
#' \dontrun{
#' # For single stock
#' data(ple4)
#' ts1 <- tseries(ple4)
#'
#' # For multiple stocks
#' stocks <- FLStocks(stock1=ple4, stock2=ple4)
#' ts2 <- tseries(stocks)
#' }
#'
#' @export
setGeneric("tseries", function(object, ...) standardGeneric("tseries"))

#' pt
#'
#' pt function
#' 
#' @param object \code{FLPar} or \code{numeric} with values for $b_{msy}$, $f_{msy}$ as a minimum; $msy$ and $v$ (virgin biomass) can also be provided. 
#' @param shape \code{FLPar} or \code{numeric} the ratio of $b_{msy}$ to virgin biomass, by default is estimated from $b_{msy}$ and $v$ in \code{object} 
#' @param ... other arguments
#' 
#' @export
#' @docType methods
#' @rdname pt
#' 
#' @aliases pt pt-method pt,FLPar-method
#' 
#' @seealso \code{\link{gompertz}}
#' 
#' @examples
#' \dontrun{
#' params=FLPar(a50=4,ato95=1,asym=1.0)
#' age=FLQuant(1:10,dimnames=list(age=1:10))
#' pt(age,params)
#' }
setGeneric("pt", function(object,...)  standardGeneric("pt"))


setGeneric("globalMsy", function(object, ...)
  standardGeneric("globalMsy"))

# globalMsy {{{

#' @rdname globalMsy
#' @description Calculates the global MSY, i.e. the largest catch that can be made with a knife edge selection pattern
#' @examples
#' data(ple4brp)
#' globalMsy(ple4brp)
#' 


#' hcrICES
#' 
#' @title hcrICES 
#' 
#' @description Harvest Control Rule, calculates Total Allowable Catch (TAC) based on a hockey stock harvest control rule.
#' @author Laurence Kell, Sea++
#'  
#' @usage hcrICES(object,eql,sr_deviances,params, 
#'          start=max(dimnames(object)$year)-10, end=start+10, interval=1,
#'          err=NULL,bndTac=c(0,Inf),...)
#'          
#' @param object an object of class \code{FLStock} 
#' @param eql \code{FLBRP} with a stock recruitment relationship used for projection
#' @param sr_deviances \code{FLQuant} recuitment deviates on the log scale, i.e. multiplicative
#' @param params \code{FLPar} HCR parameters, specifying blim, btrig, bmin, ftar and fmin
#' @param start \code{numeric} first year for simulation
#' @param end \code{numeric}   last year for simulation
#' @param interval  \code{numeric} time step, 1 year by default
#' @param err \code{FLQuant} assessment error on SSB for year used for HCR
#' @param bndTac \code{numeric} bounds on TAC, by default these are turned off, for 20 percent constraint set to c(0.8,1.2)
#' @param ... any additional arguments
#' 
#' @aliases hcrICES hcrICES-method hcrICES,FLStock,FLBRP-method
#' 
#' @export hcrICES
#' @docType methods
#' 
#' @rdname hcrICES
#' 
#' @return returns a \code{list} with \code{FLStock} and \code{FLPar} objects for the stock and HCR 
#'
#' @import FLCore 
#' @import FLBRP
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' data(pl4)
#' }
#' 
#' 
setGeneric('hcrICES', function(object,eql,...) standardGeneric('hcrICES'))

#' Extract Time Series Statistics from FLStock Objects
#'
#' @description
#' Creates a data frame of time series statistics from FLStock objects, including
#' catch, ebiomass, SSB, fishing mortality, harvest rate, and mean natural mortality.
#'
#' @param object An object of class FLStock or FLStocks
#' @param ... Additional arguments (not currently used)
#'
#' @return A data frame containing time series of:
#'   \itemize{
#'     \item catch - Catch values
#'     \item eb - Exploitable biomass
#'     \item ssb - Spawning stock biomass
#'     \item f - Fishing mortality (Fbar)
#'     \item h - Harvest rate (catch/ebiomass)
#'     \item m - Mean natural mortality
#'   }
#'
#' @examples
#' \dontrun{
#' # For single stock
#' data(ple4)
#' ts1 <- tseries(ple4)
#'
#' # For multiple stocks
#' stocks <- FLStocks(stock1=ple4, stock2=ple4)
#' ts2 <- tseries(stocks)
#' }
#'
#' @export
setGeneric("tseries", function(object, ...) standardGeneric("tseries"))


#' Standardize Values
#'
#' @description
#' Standardizes values by subtracting the mean and dividing by the standard deviation.
#'
#' @param x A numeric vector, matrix, array or FLQuant
#' @param na.rm Logical indicating whether to remove NA values when computing statistics
#'
#' @return An object of the same class as the input with standardized values
#'
#' @details
#' Standardization follows the formula: (x - mean(x))/sd(x)
#'
#' @examples
#' \dontrun{
#' # For numeric vector
#' x <- 1:10
#' stdz(x)
#'
#' # For FLQuant
#' data(ple4)
#' standardized_catch <- stdz(catch(ple4))
#' }
#'
#' @export
setGeneric("stdz", function(object, na.rm=TRUE) standardGeneric("stdz"))

#' Calculate Exploitable Biomass
#'
#' @description
#' Calculates the exploitable biomass from an FLStock object using selectivity-weighted
#' catch weights and stock numbers.
#'
#' @param object An object of class FLStock
#'
#' @return An FLQuant object containing the exploitable biomass time series
#'
#' @details
#' Exploitable biomass is based on weighting catch weights by selectivity normalised 
#' by peak selectivity, then multiplying by stock numbers and summing across ages
#'
#' @examples
#' \dontrun{
#' data(ple4)
#' eb <- ebiomass(ple4)
#' }
#'
#' @export
setGeneric("ebiomass", function(object) standardGeneric("ebiomass"))

#' Calculate total mortality-at-age (Z) using length data.
#' 
#' This is a generic S4 method that calculates something using the `haupt` function.
#' The specific implementation depends on the class of the input object.
#' 
#' @param object An object of class FLQuant or data.frame (depends on the method).
#' @param pars A parameter object (class FLPar) containing necessary parameters.
#' @param lc A threshold value for lc.
#' @param lmax A threshold value for lmax.
#' @param ... Additional arguments .
#' 
#' @return estimates of Z.
#' 
#' @export
setGeneric("haupt", function(object, pars, lc, lmax, ...) {
  standardGeneric("haupt")
})

#' Calculate something using the haupt method for FLQuant and FLPar objects.
#' 
#' This method calculates something using the `haupt` function when the input object is of class FLQuant and the parameter object is of class FLPar.
#' 
#' @param object An object of class FLQuant.
#' @param pars A parameter object (class FLPar) containing necessary parameters.
#' @param lc A threshold value for lc.
#' @param lmax A threshold value for lmax.
#' @param ... Additional arguments (not used in this example).
#' 
#' @return The result of the calculation.
#' 
#' @export


#' Extract Benchmark Reference Points
#'
#' @description
#' A generic function to extract benchmark reference points from FLStock objects
#'
#' @param object An FLStock or FLStocks object
#' @param ... Additional arguments (not currently used)
#'
#' @return An FLPar object containing benchmark reference points (Fmsy, Flim, Fpa, Blim, Bpa, Btrigger)
#'
#' @export
setGeneric("benchmark", function(object, ...) {
  standardGeneric("benchmark")
})


#' Extract FishLife Parameters
#'
#' @description
#' A generic function to extract FishLife parameters from FLStock objects
#'
#' @param object An FLStock or FLStocks object
#' @param ... Additional arguments (not currently used)
#'
#' @return An FLPar object containing FishLife parameters
#'
#' @export
setGeneric("fishlife", function(object, ...) {
  standardGeneric("fishlife")
})

#' Internal Function for FishLife Parameter Extraction
#'
#' @param x An FLStock object
#' @return An FLPar object
#' 
#' 



#' Extract FLife Parameters
#'
#' @description
#' A generic function to extract FLife parameters from FLStock objects
#'
#' @param object An FLStock or FLStocks object
#' @param ... Additional arguments (not currently used)
#'
#' @return An FLPar object containing life history parameters
#'
#' @export
setGeneric("FLifePar", function(object, ...) {
  standardGeneric("FLifePar")
})


#' Extract EqSim Reference Points
#'
#' @description
#' A generic function to extract EqSim reference points from FLStock objects
#'
#' @param object An FLStock or FLStocks object
#' @param ... Additional arguments (not currently used)
#'
#' @return An FLPar object containing EqSim reference points
#'
#' @export
setGeneric("eqsim", function(object, ...) {
  standardGeneric("eqsim")
})

#' Calculate Exploitable Biomass
#'
#' @description
#' Calculates the exploitable biomass from an FLStock object using selectivity-weighted
#' catch weights and stock numbers.
#'
#' @param object An object of class FLStock
#'
#' @return An FLQuant object containing the exploitable biomass time series
#'
#' @details
#' Exploitable biomass is based on weighting catch weights by selectivity normalised 
#' by peak selectivity, then multiplying by stock numbers and summing across ages
#'
#' @examples
#' \dontrun{
#' data(ple4)
#' eb <- ebiomass(ple4)
#' }
#'
#' @export
setGeneric("ebiomass", function(object) standardGeneric("ebiomass"))

setGeneric('kobe',  function(path,method,...) standardGeneric('kobe'))

#' Extract FLife Parameters
#'
#' @description
#' A generic function to extract FLife parameters from FLStock objects
#'
#' @param object An FLStock or FLStocks object
#' @param ... Additional arguments (not currently used)
#'
#' @return An FLPar object containing life history parameters
#'
#' @export
setGeneric("FLifePar", function(object, ...) {
  standardGeneric("FLifePar")
})


#' Extract EqSim Reference Points
#'
#' @description
#' A generic function to extract EqSim reference points from FLStock objects
#'
#' @param object An FLStock or FLStocks object
#' @param ... Additional arguments (not currently used)
#'
#' @return An FLPar object containing EqSim reference points
#'
#' @export
setGeneric("eqsim", function(object, ...) {
  standardGeneric("eqsim")
})

#' Extract FishLife Parameters
#'
#' @description
#' A generic function to extract FishLife parameters from FLStock objects
#'
#' @param object An FLStock or FLStocks object
#' @param ... Additional arguments (not currently used)
#'
#' @return An FLPar object containing FishLife parameters
#'
#' @export
setGeneric("fishlife", function(object, ...) {
  standardGeneric("fishlife")
})

#' Extract Benchmark Reference Points
#'
#' @description
#' A generic function to extract benchmark reference points from FLStock objects
#'
#' @param object An FLStock or FLStocks object
#' @param ... Additional arguments (not currently used)
#'
#' @return An FLPar object containing benchmark reference points (Fmsy, Flim, Fpa, Blim, Bpa, Btrigger)
#'
#' @export
setGeneric("benchmark", function(object, ...) {
  standardGeneric("benchmark")
})


#' Standardize Values
#'
#' @description
#' Standardizes values by subtracting the mean and dividing by the standard deviation.
#'
#' @param x A numeric vector, matrix, array or FLQuant
#' @param na.rm Logical indicating whether to remove NA values when computing statistics
#'
#' @return An object of the same class as the input with standardized values
#'
#' @details
#' Standardization follows the formula: (x - mean(x))/sd(x)
#'
#' @examples
#' \dontrun{
#' # For numeric vector
#' x <- 1:10
#' stdz(x)
#'
#' # For FLQuant
#' data(ple4)
#' standardized_catch <- stdz(catch(ple4))
#' }
#'
#' @export
setGeneric("stdz", function(x, na.rm=TRUE) standardGeneric("stdz"))

################################################################################
## awa                                                                        ##
################################################################################
setGeneric("awa", function(object, ...) standardGeneric("awa"))
#' @title awa Average Weight at Age anomaly (AWA)
#'
#' @description the average Weight at age anomaly are the deviations in the expected weight 
#' of fish at a certain age, which can be influenced by environmental conditions, 
#' food availability, and fishing pressure. Changes in average weight at age can 
#' affect the productivity and sustainability of fish stocks. This concept is 
#' important for understanding growth patterns and population dynamics
#' To calculate ASW anomalies, you first calculate the average weight at
#' age for your stock and then identify deviations from this average over time.
#'  
#' @rdname awa
#' 
#' @aliases 
#' @seealso \code{\link{ssb}}, \code{\link{ssb.age}}, 
#' \code{\link{pos}}, \code{\link{spawnOnce}}, 
#' \code{\link{asa}}, \code{\link{awa}},   
#' \code{\link{ssb}}, \code{\link{amat}}, \code{\link{wmat}}, 
#' \code{\link{fjuv}}, \code{\link{fapex}}
#' 
#' @examples
#' \dontrun{
#' data(ple4)
#' fjuv(ple4)
#' }

################################################################################
## fjuv                                                                       ##
################################################################################
setGeneric("fjuv", function(object, ...) standardGeneric("fjuv"))
#' @title Fjuv
#' 
#' @description  The ratio of juvenile to apical fishing mortality and compares 
#' the fishing mortality rates of juvenile fish to those at the peak of the fishing mortality 
#' curve (fapex). It's used to assess the impact of fishing on different life 
#' stages of fish and to inform management strategies that protect juvenile 
#' fish to ensure the sustainability of fish stocks. 
#' The ratio of juvenile to apical fishing mortality (Fjuv/Fapical)
#' This ratio involves calculating fishing mortality rates for juvenile and 
#' apical age classes separately and then dividing them. 
#'
#' @rdname fjuv
#' 
#' @aliases 
#' @seealso \code{\link{ssb}}, \code{\link{ssb.age}}, 
#' \code{\link{pos}}, \code{\link{spawnOnce}}, 
#' \code{\link{asa}}, \code{\link{awa}},   
#' \code{\link{ssb}}, \code{\link{amat}}, \code{\link{wmat}}, 
#' \code{\link{fjuv}}, \code{\link{fapex}}
#' 
#' @examples
#' \dontrun{
#' data(ple4)
#' fjuv(ple4)}

################################################################################
## wmat                                                                       ##
################################################################################
setGeneric("wmat", function(object, ...) standardGeneric("wmat"))
#' @title wmat
#' 
#' @description Wt-at-maturity via interpolation between values
#' 
#' @rdname wmat 
#' 
#' @aliases 
#' @seealso \code{\link{ssb}}, \code{\link{ssb.age}}, 
#' \code{\link{pos}}, \code{\link{spawnOnce}}, 
#' \code{\link{asa}}, \code{\link{awa}},   
#' \code{\link{ssb}}, \code{\link{amat}}, \code{\link{wmat}}, 
#' \code{\link{fjuv}}, \code{\link{fapex}}
#' 
#'
#' @param object An FLStock object.
#' @param object An FLQuant object with an ogive with the proportion of older spawners,
#' by default calculated by soawnOnce
#' @return Returns the proportion of old spawners by biomass.
#' @export
#' @examples
#' \dontrun{
#' data(ple4)
#' pos(ple4)}

################################################################################
## amat: age at a specific maturity                                           ##
################################################################################
setGeneric("amat", function(object, ...) standardGeneric("amat"))
#' @title amat: age at a specific maturity
#' 
#' @description based on interpolation between values or a specuific value
#' 
#' @rdname amat
#' 
#' @aliases 
#' 
#' @seealso \code{\link{ssb}}, \code{\link{ssb.age}}, 
#' \code{\link{pos}}, \code{\link{spawnOnce}}, 
#' \code{\link{asa}}, \code{\link{awa}},   
#' \code{\link{ssb}}, \code{\link{amat}}, \code{\link{wmat}}, 
#' \code{\link{fjuv}}, \code{\link{fapex}}
#' 
#' 
#' @param object An FLStock object.
#' @param object An FLQuant object with an ogive with the proportion of older spawners,
#' by default calculated by soawnOnce
#' @return Returns the proportion of old spawners by biomass.
#' @export
#' @examples
#' \dontrun{
#' data(ple4)
#' amat(ple4)}

################################################################################
## asa: the Average Age of Spawners                                           ##
################################################################################
setGeneric("asa", function(object, ...) standardGeneric("asa"))
#' @title ASA: the Average Age of Spawners
#' 
#' @description The Average Age of Spawners is the mean age of reproductive individuals. 
#' The average age of spawners can indicate the age structure of a fish population 
#' and its potential reproductive capacity. A decrease in the ASA could suggest 
#' overfishing or other stressors affecting older age classes. 
#' 
#' @rdname asa
#' 
#' @aliases
#' @seealso \code{\link{ssb}}, \code{\link{ssb.age}}, 
#' \code{\link{pos}}, \code{\link{spawnOnce}}, 
#' \code{\link{asa}}, \code{\link{awa}},   
#' \code{\link{ssb}}, \code{\link{amat}}, \code{\link{wmat}}, 
#' \code{\link{fjuv}}, \code{\link{fapex}}
#' 
#' @param object An FLStock object.
#' @param object An FLQuant object with an ogive with the proportion of older spawners,
#' by default calculated by soawnOnce
#' @return Returns the proportion of old spawners by biomass.
#' @export
#' @examples
#' \dontrun{
#' data(ple4)
#' asa(ple4)}

################################################################################
## pos: Proportion of Old Spawners by Biomass                                 ##
################################################################################
setGeneric("pos", function(object, ...) standardGeneric("pos"))
#' @title  POS: proportion of the SSB made up of older individuals. 
#' 
#' @description Older spawners are important for the genetic diversity 
#' and resilience of fish populations, as they tend to produce more, and potentially 
#' higher quality, eggs. To calculate POS, first identify the older age classes,  
#' sum their biomass, then divide by the total spawning stock biomass (SSB).
#' The older age classes can be identified from the maturity ogive `mat`, for example
#' using the `spawnOnce` method. This shifts the maturity by 1 age soi that it idenifies
#' the proportion of individuals that have spawned before
#' 
#' @rdname pos
#'
#' @param object An FLStock object.
#' @param object An FLQuant object with an ogive with the proportion of older spawners,
#' by default calculated by soawnOnce
#' 
#' @return Returns the proportion of old spawners by biomass.
#' 
#' @export
#' @examples
#' \dontrun{
#' data(ple4)
#' pos(ple4)}

################################################################################
## spawnOnce                                                                  ##
################################################################################
setGeneric("spawnOnce", function(object, ...) standardGeneric("spawnOnce"))
#' @title spawnOnce
#' 
#' @description  Shifts maturity-at-age ogive by 1 age to estimate the propotion 
#' of individuals who have spawned at least once
#' 
#' @rdname spawnOnce
#' @aliases 
#' 
#' @seealso \code{\link{ssb}}, \code{\link{ssb.age}}, 
#' \code{\link{pos}}, \code{\link{spawnOnce}}, 
#' \code{\link{asa}}, \code{\link{awa}},   
#' \code{\link{ssb}}, \code{\link{amat}}, \code{\link{wmat}}, 
#' \code{\link{fjuv}}, \code{\link{fapex}}
#'
#' @param object An FLStock object.
#' @return object An FLQuant object with an ogive with the proportion of older spawners.
#' @export
#' @examples
#' data(ple4)
#' spawnOnce(ple4)

################################################################################
## SSB: Spawning Stock Biomass                                                ##
################################################################################
setGeneric("ssb", function(object, ...) standardGeneric("ssb"))
#' @title ssb
#' 
#' @description  Calculates the spawning stock biomass for an FLStock object,
#' 
#' @rdname ssb
#' @aliases 
#' @seealso \code{\link{ssb.age}}
#'
#' @param object An FLStock object containing stock assessment data.
#' @param ... additional arguments, to replace slots in the FLStock object.
#' @return Returns the spawning stock biomass as an FLQuant.
#' @export
#' @examples
#' data(ple4)
#' ssb(ple4)

setGeneric("invALK", function(object, model=vonbert, age, 
                              cv=0.1, lmax=1.2, bin=1, max=ceiling(object["linf"]*lmax), reflen=NULL) 
  standardGeneric("invALK"))

setGeneric('as.biodyn', function(object, ...) standardGeneric('as.biodyn'))

setGeneric('leslie', function(object, fec, ...) standardGeneric('leslie'))

setGeneric("m1", function(object, ...)
  standardGeneric("m1"))
setGeneric("m2", function(object, ...)
  standardGeneric("m2"))
setGeneric("forage", function(object, ...)
  standardGeneric("forage"))
setGeneric("predNeed", function(object, ...)
  standardGeneric("predNeed"))

#' Calculate Pella-Tomlinson Parameters
#' 
#' @description Estimates r, k, and p parameters for Pella-Tomlinson model from 
#' MSY reference points based on an age-based model with instantaneous fishing mortality
#' 
#' @param object containing fmsy, bmsy and b0 reference points
#' @param interval Numeric vector of length 2 for shape parameter optimisation bounds
#' @return FLPar object with r, k, and p parameters
#' @export
setGeneric("pellaTparams", function(object, ...) standardGeneric("pellaTparams"))

#' Calculate priors for an `FLBRP` based on reference points and observations.
#'
#' @param object `An`FLBRP` object for which to calculate priors
#' @param ... Additional arguments passed to methods
#' @return A named vector of calculated prior values
#' @export
setGeneric("calcPriors", function(object, ...) {
  standardGeneric("calcPriors")})

#' Calculate priors for an FLBRP object
#'
#' This method calculates priors for an FLBRP object based on reference points and observations.
#'
#' @param object An object of class FLBRP
#' @return A named vector of calculated prior values
#' @export
#' @import FLife 
#' @importFrom stats optimize
#' @examples
#' \dontrun{
#'   eql=FLBRP(...)  # Create or load an FLBRP object
#'   calcPriors(eql)
#' }

#' @title properties 
#' 
#' @description Calculates additional reference points
#'
#' @param object an \code{FLBRP} object 
#' 
#' @return \code{FLPar} with estimates of MSY based reference points
#'
#' @seealso \code{\link{FLBRP},\link{refpts}}
#'
#' @export metrics
#' @docType methods
#' @rdname metrics
#'
#' 
#' @examples
#' \dontrun{
#' 
#' data(ple4brp)
#' 
#' fbar(ple4brp)=FLQuant(seq(0,c(refpts(ple4brp)["crash","harvest"]),length.out=101))
#' refpts(ple4brp)=properties(ple4brp)
#' 
#' plot(ple4brp,ncol=2)
#' }
#' 
setGeneric("properties", function(object, ...)
  standardGeneric("properties"))


#' @title Calculate True Skill Statistic (TSS)
#'
#' @description Calculates the True Skill Statistic (TSS) from confusion matrix elements
#'
#' @param TP Number of True Positives
#' @param TN Number of True Negatives
#' @param FP Number of False Positives
#' @param FN Number of False Negatives
#'
#' @return Numeric value of TSS (sensitivity + specificity - 1)
#'
#' @details
#' TSS ranges from -1 to +1, where +1 indicates perfect agreement and values of zero 
#' or less indicate a performance no better than random
#'
#' @examples
#' \dontrun{
#' tss_value <- TSS(TP=10, TN=8, FP=2, FN=3)
#' }
#'
#' @export
setGeneric("TSS", function(TP, TN, FP, FN) standardGeneric("TSS"))


#' @title Confusion Matrix Statistics
#'
#' @description Calculates True Positive (TP), True Negative (TN), False Positive (FP), 
#' and False Negative (FN) counts from predicted and actual values
#'
#' @param x Numeric vector of predicted values
#' @param y Numeric vector of actual values
#'
#' @return A data frame containing TP, TN, FP, and FN counts
#'
#' @examples
#' \dontrun{
#' pred <- c(1, -1, 1, -1)
#' actual <- c(1, -1, -1, 1)
#' confusion_stats <- PN(pred, actual)
#' }
#'
#' @export
setGeneric("PN", function(x, y) standardGeneric("PN"))


#' @title ROC Curve Coordinates
#'
#' @description Calculates the coordinates for a Receiver Operating Characteristic (ROC) curve
#'
#' @param labels Binary vector of true labels
#' @param scores Numeric vector of prediction scores
#'
#' @return A data frame containing:
#'   \itemize{
#'     \item TPR - True Positive Rate (Sensitivity)
#'     \item FPR - False Positive Rate (1-Specificity)
#'     \item labels - Ordered labels
#'     \item reference - Sorted scores
#'   }
#'
#' @examples
#' \dontrun{
#' labels <- c(1,0,1,1,0)
#' scores <- c(0.9, 0.1, 0.8, 0.7, 0.3)
#' roc_coords <- rocFn(labels, scores)
#' }
#'
#' @export
setGeneric("rocFn", function(labels, scores) standardGeneric("rocFn"))

#' Calculate ROC (Receiver Operating Characteristic) statistics for two numeric vectors.
#'
#' This function calculates ROC statistics, including True Positive Rate (TPR), False Positive Rate (FPR), True Positives (TP), True Negatives (TN), False Positives (FP), False Negatives (FN), and True Skill Score (TSS) for two numeric vectors.
#'
#' @param state A numeric vector representing the state values.
#' @param indicator A numeric vector representing the indicator values.
#' 
#' @return A data frame containing the following columns:
#'   \describe{
#'     \item{state}{The state values.}
#'     \item{label}{A logical vector indicating whether each state is greater than 1 (TRUE) or not (FALSE).}
#'     \item{indicator}{The indicator values.}
#'     \item{TPR}{The True Positive Rate (TPR) calculated as TP / (TP + FN).}
#'     \item{FPR}{The False Positive Rate (FPR) calculated as FP / (FP + TN).}
#'     \item{TP}{The True Positives (TP).}
#'     \item{TN}{The True Negatives (TN).}
#'     \item{FP}{The False Positives (FP).}
#'     \item{FN}{The False Negatives (FN).}
#'     \item{TSS}{The True Skill Score (TSS) calculated as (TP / (TP + FN)) - (FP / (FP + TN)).}
#'     \item{order}{The order of the indicator values after sorting in descending order.}
#'   }
#' 
#' @export
setGeneric("roc", function(state, indicator, ...) {
  standardGeneric("roc")
})

################################################################################
##  ssb.age                                                                   ##
################################################################################
setGeneric("ssb.age", function(object, ...) standardGeneric("ssb.age"))
#' @title ssb.age
#' 
#' @description Calculates the spawning stock biomass by age for an FLStock object.
#' 
#' 
#' @rdname ssb.age
#' @aliases 
#' 
#' @seealso \code{\link{ssb}}
#'
#' @param object An FLStock object containing stock assessment data.
#' @param ... additional arguments, to replace slots in the FLStock object.
#' @return Returns the spawning stock biomass by age as an FLQuant.
#' @export
#' @examples
#' data(ple4)
#' ssb.age(ple4)

#' @title priors
#' 
#' @description priors for biomass dynamic assessment from an `FLBRP` object
#'
#' @param object An `FLBRP` from which to extract priors
#' @param ... additional arguments passed to methods
#' @return `priors` attribute as an `FLPar` 
#' @export

setGeneric("priors", function(object, ...) {
  standardGeneric("priors")})

#' Get priors for biomass dynamic assessment from an `FLBRP` object
#'
#' @param object An object of class `FLBRP`
#' @return priors attribute as an `FLPar` of the `FLBRP` object
#' @export
#' 
#' @examples
#' \dontrun{
#'   eql=FLBRP(...)  # Create or load an FLBRP object
#'   priors(eql)
#' }

#' Rebuild a fish population
#' 
#' @description Projects rebuilding trajectories from different initial SSB levels
#'
#' @param object An object representing the population
#' @param targetF Target fishing mortality during rebuilding
#' @param targetSSB Target spawning stock biomass 
#' @param nInitial Number of initial SSB levels
#' @param growthRate Growth rate for depletion sequence
#' @param minVal Minimum depletion value
#' @param maxVal Maximum depletion value 
#' @param burnin Number of years for burn-in period
#' @param truncate Whether to remove burn-in period
#' @return An object with rebuilding trajectories
#' @export
setGeneric("rebuild", function(object, targetF=NULL, targetSSB=NULL,
                               nInitial=100, growthRate=0.25, minVal=1e-6, maxVal=1,
                               burnin=20, truncate=TRUE) {
  standardGeneric("rebuild")
})

#' Calculate rebuilding time
#'
#' @param object An object containing rebuilding trajectories
#' @param nx Number of interpolation points
#' @return A data frame with columns year and initial
#' @export
setGeneric("rebuildTime", function(object, nx=101) {
  standardGeneric("rebuildTime")
})
