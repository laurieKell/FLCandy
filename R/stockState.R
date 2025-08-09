#' Identify Stock State
#' 
#' Creates a variable that identifies stock state as increasing, decreasing, or stable
#' based on trends in SSB and F. Provides a simple classification that can be used for
#' grouping and filtering.
#' 
#' @param object An FLStock object, FLQuant object, data frame with columns 'ssb', 'f', 'year', 
#'               or a list of FLStock/FLQuant objects
#' @param ssb An FLQuant object for spawning stock biomass (optional if object is FLStock)
#' @param f An FLQuant object for fishing mortality (optional if object is FLStock)
#' @param method Method for trend analysis: "linear" (linear regression) or "mann_kendall" (Mann-Kendall test)
#' @param p Significance threshold for trend detection (default: 0.05)
#' @param minYears Minimum number of years required for analysis (default: 5)
#' @param recentYears Number of recent years to focus on (default: NULL, uses all data)
#' @param windowSize Size of sliding window for year-by-year classification (default: 5)
#' @param byYear Logical. If TRUE, returns classification for each year using sliding window. If FALSE, returns overall trend.
#' @param simplify Logical. If TRUE, returns only the state classification. If FALSE, returns detailed results.
#' 
#' @return If `simplify=TRUE`: A character vector with states ("Increasing", "Decreasing", "Stable")
#'         If `simplify=FALSE`: A list with detailed results for each stock
#' 
#' @examples
#' \dontrun{
#' # Single stock
#' data(ple4)
#' state=stockState(ple4)
#' print(state)  # "Increasing"
#' 
#' # Using FLQuant objects
#' ssbQuant=ssb(ple4)
#' fQuant=fbar(ple4)
#' state=stockState(ssb=ssbQuant, f=fQuant)
#' 
#' # Multiple stocks
#' stocks=list(ple4=ple4, ple4_2=ple4)
#' states=stockState(stocks)
#' print(states)  # c("Increasing", "Increasing")
#' 
#' # With detailed results
#' results=stockState(ple4, simplify=FALSE)
#' print(results$trend)
#' 
#' # Year-by-year classification
#' yearlyResults=stockState(ple4, byYear=TRUE)
#' print(yearlyResults$yearlyStates)
#' }
#' 
#' @export
setGeneric("stockState", function(object, ssb = NULL, f = NULL, method = "linear", 
                                  p = 0.05, minYears = 5, recentYears = NULL, 
                                  windowSize = 5, byYear = FALSE, simplify = TRUE) {
  standardGeneric("stockState")
})

#' @rdname stockState
#' @export
setMethod("stockState", signature(object = "FLStock", ssb = "missing", f = "missing"),
          function(object, ssb, f, method, p, minYears, recentYears, windowSize, byYear, simplify) {
            result = stockTrend(object, method = method, pThreshold = p, 
                               minYears = minYears, recentYears = recentYears, 
                               windowSize = windowSize, byYear = byYear)
            
            if (simplify) {
              if (byYear) {
                return(result$yearlyStates)
              } else {
                return(result$trend)
              }
            } else {
              return(result)
            }
          })

#' @rdname stockState
#' @export
setMethod("stockState", signature(object = "FLQuant", ssb = "missing", f = "missing"),
          function(object, ssb, f, method, p, minYears, recentYears, windowSize, byYear, simplify) {
            stop("For FLQuant objects, please provide both ssb and f as separate arguments")
          })

#' @rdname stockState
#' @export
setMethod("stockState", signature(object = "missing", ssb = "FLQuant", f = "FLQuant"),
          function(object, ssb, f, method, p, minYears, recentYears, windowSize, byYear, simplify) {
            result = stockTrend(ssb = ssb, f = f, method = method, pThreshold = p, 
                               minYears = minYears, recentYears = recentYears,
                               windowSize = windowSize, byYear = byYear)
            
            if (simplify) {
              if (byYear) {
                return(result$yearlyStates)
              } else {
                return(result$trend)
              }
            } else {
              return(result)
            }
          })

#' @rdname stockState
#' @export
setMethod("stockState", signature(object = "data.frame", ssb = "missing", f = "missing"),
          function(object, ssb, f, method, p, minYears, recentYears, windowSize, byYear, simplify) {
            result = stockTrend(object, method = method, pThreshold = p, 
                               minYears = minYears, recentYears = recentYears,
                               windowSize = windowSize, byYear = byYear)
            
            if (simplify) {
              if (byYear) {
                return(result$yearlyStates)
              } else {
                return(result$trend)
              }
            } else {
              return(result)
            }
          })



#' Create Stock State Factor
#' 
#' Creates a factor variable for stock states with ordered levels
#' (Decreasing < Stable < Increasing) for use in statistical analysis.
#' 
#' @param object An FLStock object, FLQuant object, data frame, or list of these objects
#' @param ssb An FLQuant object for spawning stock biomass (optional if object is FLStock)
#' @param f An FLQuant object for fishing mortality (optional if object is FLStock)
#' @param method Method for trend analysis
#' @param p Significance threshold
#' @param min_years Minimum years required
#' @param recent_years Number of recent years to focus on
#' @param levels Order of factor levels (default: c("Decreasing", "Stable", "Increasing"))
#' 
#' @return A factor with stock states
#' 
#' @examples
#' \dontrun{
#' data(ple4)
#' state_factor=stockStateFactor(ple4)
#' print(state_factor)  # Factor with levels: Decreasing < Stable < Increasing
#' 
#' # Using FLQuant objects
#' ssb_quant=ssb(ple4)
#' f_quant=fbar(ple4)
#' state_factor=stockStateFactor(ssb=ssb_quant, f=f_quant)
#' }
#' 
#' @export
setGeneric("stockStateFactor", function(object, ssb = NULL, f = NULL, method = "linear", 
                                        p = 0.05, minYears = 5, recentYears = NULL,
                                        levels = c("Decreasing", "Stable", "Increasing")) {
  standardGeneric("stockStateFactor")
})

#' @rdname stockStateFactor
#' @export
setMethod("stockStateFactor", signature(object = "FLStock", ssb = "missing", f = "missing"),
          function(object, ssb, f, method, p, minYears, recentYears, levels) {
            states = stockState(object, method = method, p = p, 
                               minYears = minYears, recentYears = recentYears, 
                               simplify = TRUE)
            
            # Convert "Declining" to "Decreasing" for consistency
            states = gsub("Declining", "Decreasing", states)
            
            # Create factor with specified levels
            factor(states, levels = levels, ordered = TRUE)
          })

#' @rdname stockStateFactor
#' @export
setMethod("stockStateFactor", signature(object = "missing", ssb = "FLQuant", f = "FLQuant"),
          function(object, ssb, f, method, p, minYears, recentYears, levels) {
            states = stockState(ssb = ssb, f = f, method = method, p = p, 
                               minYears = minYears, recentYears = recentYears, 
                               simplify = TRUE)
            
            # Convert "Declining" to "Decreasing" for consistency
            states = gsub("Declining", "Decreasing", states)
            
            # Create factor with specified levels
            factor(states, levels = levels, ordered = TRUE)
          })

#' @rdname stockStateFactor
#' @export
setMethod("stockStateFactor", signature(object = "data.frame", ssb = "missing", f = "missing"),
          function(object, ssb, f, method, p, minYears, recentYears, levels) {
            states = stockState(object, method = method, p = p, 
                               minYears = minYears, recentYears = recentYears, 
                               simplify = TRUE)
            
            # Convert "Declining" to "Decreasing" for consistency
            states = gsub("Declining", "Decreasing", states)
            
            # Create factor with specified levels
            factor(states, levels = levels, ordered = TRUE)
          })




  



  




