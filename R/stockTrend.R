#' Analyze Stock Trends: SSB and F Trajectories
#' 
#' Determines if a stock is declining or increasing based on the trends in 
#' spawning stock biomass (SSB) and fishing mortality (F). A stock is considered:
#' - **Increasing**: SSB is increasing while F is decreasing
#' - **Declining**: SSB is decreasing while F is increasing
#' - **Mixed**: Other combinations (SSB increasing with F increasing, or SSB decreasing with F decreasing)
#' - **Stable**: No significant trend in either variable
#' 
#' @param object An FLStock object, FLQuant object, or a data frame with columns 'ssb', 'f', and 'year'
#' @param ssb An FLQuant object for spawning stock biomass (optional if object is FLStock)
#' @param f An FLQuant object for fishing mortality (optional if object is FLStock)
#' @param method Method for trend analysis: "linear" (linear regression) or "mann_kendall" (Mann-Kendall test)
#' @param pThreshold Significance threshold for trend detection (default: 0.05)
#' @param minYears Minimum number of years required for analysis (default: 5)
#' @param recentYears Number of recent years to focus on (default: NULL, uses all data)
#' @param windowSize Size of sliding window for year-by-year classification (default: 5)
#' @param byYear Logical. If TRUE, returns classification for each year using sliding window. If FALSE, returns overall trend.
#' 
#' @return A list containing:
#'   - `trend`: Character string indicating overall trend ("Increasing", "Declining", "Mixed", "Stable")
#'   - `yearlyStates`: Data frame with year-by-year classifications (if byYear=TRUE)
#'   - `ssbTrend`: SSB trend analysis results
#'   - `fTrend`: F trend analysis results
#'   - `classification`: Detailed classification with reasoning
#'   - `plotData`: Data frame with standardized values for plotting
#' 
#' @examples
#' \dontrun{
#' # Using FLStock object
#' data(ple4)
#' result <- stockTrend(ple4, byYear=TRUE)
#' print(result$trend)
#' print(result$yearlyStates)
#' 
#' # Using FLQuant objects
#' ssbQuant <- ssb(ple4)
#' fQuant <- fbar(ple4)
#' result <- stockTrend(ssb = ssbQuant, f = fQuant, byYear=TRUE)
#' 
#' # Using data frame
#' tsData <- data.frame(
#'   year = 2000:2020,
#'   ssb = rnorm(21, 100, 10),
#'   f = rnorm(21, 0.3, 0.05)
#' )
#' result <- stockTrend(tsData, byYear=TRUE)
#' }
#' 
#' @export
setGeneric("stockTrend", function(object, ssb = NULL, f = NULL, method = "linear", 
                                  pThreshold = 0.05, minYears = 5, recentYears = NULL, 
                                  windowSize = 5, byYear = FALSE) {
  standardGeneric("stockTrend")
})

#' @rdname stockTrend
#' @export
setMethod("stockTrend", signature(object = "FLStock", ssb = "missing", f = "missing"),
          function(object, ssb, f, method, pThreshold, minYears, recentYears, windowSize, byYear) {
            # Extract SSB and F from FLStock
            ssbData = c(ssb(object))
            fData = c(fbar(object))
            years = dimnames(ssb(object))$year
            
            # Create data frame
            data = data.frame(
              year = as.numeric(years),
              ssb = ssbData,
              f = fData
            )
            
            # Call internal function
            .stockTrendInternal(data, method, pThreshold, minYears, recentYears, windowSize, byYear)
          })

#' @rdname stockTrend
#' @export
setMethod("stockTrend", signature(object = "FLQuant", ssb = "missing", f = "missing"),
          function(object, ssb, f, method, pThreshold, minYears, recentYears, windowSize, byYear) {
            stop("For FLQuant objects, please provide both ssb and f as separate arguments")
          })

#' @rdname stockTrend
#' @export
setMethod("stockTrend", signature(object = "missing", ssb = "FLQuant", f = "FLQuant"),
          function(object, ssb, f, method, pThreshold, minYears, recentYears, windowSize, byYear) {
            # Extract data from FLQuant objects
            ssbData = c(ssb)
            fData = c(f)
            years = dimnames(ssb)$year
            
            # Create data frame
            data = data.frame(
              year = as.numeric(years),
              ssb = ssbData,
              f = fData
            )
            
            # Call internal function
            .stockTrendInternal(data, method, pThreshold, minYears, recentYears, windowSize, byYear)
          })

#' @rdname stockTrend
#' @export
setMethod("stockTrend", signature(object = "data.frame", ssb = "missing", f = "missing"),
          function(object, ssb, f, method, pThreshold, minYears, recentYears, windowSize, byYear) {
            # Use data frame directly
            data = object
            if (!all(c("ssb", "f", "year") %in% names(data))) {
              stop("Data frame must contain columns: 'ssb', 'f', 'year'")
            }
            
            # Call internal function
            .stockTrendInternal(data, method, pThreshold, minYears, recentYears, windowSize, byYear)
          })

#' Internal function for stock trend analysis
#' @param data Data frame with year, ssb, and f columns
#' @param method Method for trend analysis
#' @param pThreshold Significance threshold
#' @param minYears Minimum years required
#' @param recentYears Recent years to focus on
#' @param windowSize Window size for year-by-year analysis
#' @param byYear Whether to return year-by-year analysis
#' @return stockTrend object
.stockTrendInternal = function(data, method, pThreshold, minYears, recentYears, windowSize, byYear) {
  # Remove NA values
  data = na.omit(data)
  
  # Check minimum years requirement
  if (nrow(data) < minYears) {
    stop(paste("Insufficient data: need at least", minYears, "years, got", nrow(data)))
  }
  
  # Focus on recent years if specified
  if (!is.null(recentYears)) {
    data = tail(data, recentYears)
  }
  
  # Standardize data for analysis
  data$ssbStd = scale(data$ssb)
  data$fStd = scale(data$f)
  
  # Analyze trends based on method
  if (method == "linear") {
    ssbTrend = analyzeLinearTrend(data$year, data$ssb, "SSB")
    fTrend = analyzeLinearTrend(data$year, data$f, "F")
  } else if (method == "mann_kendall") {
    ssbTrend = analyzeMannKendallTrend(data$ssb, "SSB")
    fTrend = analyzeMannKendallTrend(data$f, "F")
  } else {
    stop("Method must be 'linear' or 'mann_kendall'")
  }
  
  # Classify stock trend
  trendResult = classifyStockTrend(ssbTrend, fTrend, pThreshold)
  
  # Initialize result list
  result = list(
    trend = trendResult$trend,
    ssbTrend = ssbTrend,
    fTrend = fTrend,
    classification = trendResult$classification,
    plotData = data,
    method = method,
    pThreshold = pThreshold,
    yearsAnalyzed = range(data$year),
    byYear = byYear
  )
  
  # Add year-by-year classification if requested
  if (byYear) {
    yearlyStates = classifyByYear(data, windowSize, method, pThreshold)
    result$yearlyStates = yearlyStates
  }
  
  class(result) = "stockTrend"
  return(result)
}

#' Classify Stock State by Year
#' 
#' Uses a sliding window approach to classify stock state for each year
#' 
#' @param data Data frame with year, ssb, and f columns
#' @param windowSize Size of sliding window
#' @param method Method for trend analysis
#' @param pThreshold Significance threshold
#' @return Data frame with year-by-year classifications
classifyByYear = function(data, windowSize, method, pThreshold) {
  nYears = nrow(data)
  yearlyStates = data.frame(
    year = data$year,
    state = rep(NA, nYears),
    ssbDirection = rep(NA, nYears),
    fDirection = rep(NA, nYears),
    ssbP = rep(NA, nYears),
    fP = rep(NA, nYears),
    windowStart = rep(NA, nYears),
    windowEnd = rep(NA, nYears)
  )
  
  for (i in 1:nYears) {
    # Calculate window boundaries
    startIdx = max(1, i - floor(windowSize/2))
    endIdx = min(nYears, i + floor(windowSize/2))
    
    # Ensure minimum window size
    if (endIdx - startIdx + 1 < 3) {
      if (startIdx == 1) {
        endIdx = min(nYears, startIdx + 2)
      } else {
        startIdx = max(1, endIdx - 2)
      }
    }
    
    # Extract window data
    windowData = data[startIdx:endIdx, ]
    
    # Analyze trends in window
    if (method == "linear") {
      ssbTrend = analyzeLinearTrend(windowData$year, windowData$ssb, "SSB")
      fTrend = analyzeLinearTrend(windowData$year, windowData$f, "F")
    } else if (method == "mann_kendall") {
      ssbTrend = analyzeMannKendallTrend(windowData$ssb, "SSB")
      fTrend = analyzeMannKendallTrend(windowData$f, "F")
    }
    
    # Classify state for this window
    trendResult = classifyStockTrend(ssbTrend, fTrend, pThreshold)
    
    # Store results
    yearlyStates$state[i] = trendResult$trend
    yearlyStates$ssbDirection[i] = ssbTrend$direction
    yearlyStates$fDirection[i] = fTrend$direction
    yearlyStates$ssbP[i] = ssbTrend$p_value
    yearlyStates$fP[i] = fTrend$p_value
    yearlyStates$windowStart[i] = windowData$year[1]
    yearlyStates$windowEnd[i] = windowData$year[nrow(windowData)]
  }
  
  return(yearlyStates)
}

#' Analyze Linear Trend
#' @param x Predictor variable (usually year)
#' @param y Response variable (SSB or F)
#' @param varName Name of variable for reporting
#' @return List with trend analysis results
analyzeLinearTrend = function(x, y, varName) {
  model = lm(y ~ x)
  slope = coef(model)[2]
  pValue = summary(model)$coefficients[2, 4]
  rSquared = summary(model)$r.squared
  
  # Determine trend direction
  if (pValue < 0.05) {
    if (slope > 0) {
      direction = "increasing"
    } else {
      direction = "decreasing"
    }
  } else {
    direction = "stable"
  }
  
  return(list(
    variable = varName,
    slope = slope,
    p_value = pValue,
    r_squared = rSquared,
    direction = direction,
    significant = pValue < 0.05
  ))
}

#' Analyze Mann-Kendall Trend
#' @param y Response variable (SSB or F)
#' @param varName Name of variable for reporting
#' @return List with trend analysis results
analyzeMannKendallTrend = function(y, varName) {
  # Simple Mann-Kendall implementation
  n = length(y)
  s = 0
  
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      s = s + sign(y[j] - y[i])
    }
  }
  
  # Calculate variance
  varS = n * (n - 1) * (2 * n + 5) / 18
  
  # Calculate Z statistic
  if (s > 0) {
    z = (s - 1) / sqrt(varS)
  } else if (s < 0) {
    z = (s + 1) / sqrt(varS)
  } else {
    z = 0
  }
  
  # Calculate p-value (two-tailed)
  pValue = 2 * (1 - pnorm(abs(z)))
  
  # Determine direction
  if (pValue < 0.05) {
    if (s > 0) {
      direction = "increasing"
    } else {
      direction = "decreasing"
    }
  } else {
    direction = "stable"
  }
  
  return(list(
    variable = varName,
    s_statistic = s,
    z_statistic = z,
    p_value = pValue,
    direction = direction,
    significant = pValue < 0.05
  ))
}

#' Classify Stock Trend
#' @param ssbTrend SSB trend analysis results
#' @param fTrend F trend analysis results
#' @param pThreshold Significance threshold
#' @return List with trend classification
classifyStockTrend = function(ssbTrend, fTrend, pThreshold) {
  
  # Check if trends are significant
  ssbSig = ssbTrend$p_value < pThreshold
  fSig = fTrend$p_value < pThreshold
  
  # Classify based on trend directions
  if (ssbSig && fSig) {
    if (ssbTrend$direction == "increasing" && fTrend$direction == "decreasing") {
      trend = "Increasing"
      classification = "Stock is increasing: SSB is increasing while F is decreasing"
    } else if (ssbTrend$direction == "decreasing" && fTrend$direction == "increasing") {
      trend = "Declining"
      classification = "Stock is declining: SSB is decreasing while F is increasing"
    } else if (ssbTrend$direction == "increasing" && fTrend$direction == "increasing") {
      trend = "Mixed"
      classification = "Mixed signals: Both SSB and F are increasing"
    } else {
      trend = "Mixed"
      classification = "Mixed signals: Both SSB and F are decreasing"
    }
  } else if (ssbSig && !fSig) {
    if (ssbTrend$direction == "increasing") {
      trend = "Increasing"
      classification = "Stock is increasing: SSB is increasing (F stable)"
    } else {
      trend = "Declining"
      classification = "Stock is declining: SSB is decreasing (F stable)"
    }
  } else if (!ssbSig && fSig) {
    if (fTrend$direction == "decreasing") {
      trend = "Increasing"
      classification = "Stock is increasing: F is decreasing (SSB stable)"
    } else {
      trend = "Declining"
      classification = "Stock is declining: F is increasing (SSB stable)"
    }
  } else {
    trend = "Stable"
    classification = "Stock is stable: No significant trends in SSB or F"
  }
  
  return(list(trend = trend, classification = classification))
}

#' Print method for stockTrend objects
#' @param x stockTrend object
#' @param ... Additional arguments
#' @export
print.stockTrend = function(x, ...) {
  cat("Stock Trend Analysis\n")
  cat("===================\n")
  cat("Trend:", x$trend, "\n")
  cat("Classification:", x$classification, "\n")
  cat("Method:", x$method, "\n")
  cat("Years analyzed:", x$yearsAnalyzed[1], "to", x$yearsAnalyzed[2], "\n")
  cat("P-threshold:", x$pThreshold, "\n")
  cat("By year analysis:", x$byYear, "\n\n")
  
  if (x$byYear && !is.null(x$yearlyStates)) {
    cat("Year-by-year states (first 10 years):\n")
    print(head(x$yearlyStates[, c("year", "state", "ssbDirection", "fDirection")], 10))
    cat("\n")
  }
  
  cat("SSB Trend:\n")
  cat("  Direction:", x$ssbTrend$direction, "\n")
  cat("  P-value:", round(x$ssbTrend$p_value, 4), "\n")
  if (x$method == "linear") {
    cat("  R-squared:", round(x$ssbTrend$r_squared, 4), "\n")
  }
  cat("\n")
  
  cat("F Trend:\n")
  cat("  Direction:", x$fTrend$direction, "\n")
  cat("  P-value:", round(x$fTrend$p_value, 4), "\n")
  if (x$method == "linear") {
    cat("  R-squared:", round(x$fTrend$r_squared, 4), "\n")
  }
}

#' Plot method for stockTrend objects
#' @param x stockTrend object
#' @param type Plot type: "trends" (separate SSB and F), "combined" (both on same scale), or "states" (year-by-year states)
#' @param ... Additional arguments passed to ggplot
#' @export
plot.stockTrend = function(x, type = "trends", ...) {
  if (type == "trends") {
    # Create separate plots for SSB and F
    p1 = ggplot(x$plotData, aes(x = year, y = ssb)) +
      geom_line(color = "blue", size = 1) +
      geom_smooth(method = "lm", se = TRUE, color = "red") +
      labs(title = paste("SSB Trend:", x$ssbTrend$direction),
           x = "Year", y = "SSB") +
      theme_minimal()
    
    p2 = ggplot(x$plotData, aes(x = year, y = f)) +
      geom_line(color = "green", size = 1) +
      geom_smooth(method = "lm", se = TRUE, color = "red") +
      labs(title = paste("F Trend:", x$fTrend$direction),
           x = "Year", y = "F") +
      theme_minimal()
    
    # Combine plots
    require(gridExtra)
    grid.arrange(p1, p2, ncol = 2)
    
  } else if (type == "combined") {
    # Create combined plot with standardized values
    ggplot(x$plotData) +
      geom_line(aes(x = year, y = ssbStd, color = "SSB"), size = 1) +
      geom_line(aes(x = year, y = fStd, color = "F"), size = 1) +
      geom_smooth(aes(x = year, y = ssbStd), method = "lm", se = TRUE, color = "blue", alpha = 0.3) +
      geom_smooth(aes(x = year, y = fStd), method = "lm", se = TRUE, color = "green", alpha = 0.3) +
      labs(title = paste("Stock Trend:", x$trend),
           subtitle = x$classification,
           x = "Year", y = "Standardized Value", color = "Variable") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
  } else if (type == "states" && x$byYear && !is.null(x$yearlyStates)) {
    # Plot year-by-year states
    ggplot(x$yearlyStates, aes(x = year, fill = state)) +
      geom_bar() +
      scale_fill_manual(values = c("Increasing" = "green", "Declining" = "red", 
                                  "Mixed" = "orange", "Stable" = "gray")) +
      labs(title = "Stock State by Year",
           x = "Year", y = "State", fill = "State") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
} 