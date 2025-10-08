#' Pella-Tomlinson Maximum Likelihood Estimation
#'
#' @description Fits Pella-Tomlinson production model using maximum likelihood estimation.
#' Optimized for performance with vectorized calculations and efficient parameter estimation.
#' 
#' @param object The object to fit the model to:
#' - data.frame: Must contain biomass and yield columns
#' - FLBRP: Extracts time series if available, otherwise uses reference point approach
#' - FLBRPs: Applies pt() to each FLBRP and returns combined data frame
#' @param biomass Character string specifying the biomass column name (default: "ssb")
#' @param ... Additional arguments passed to optimization
#' @return FLPar object for data.frame/FLBRP, data frame for FLBRPs
#' @export
#' @examples
#' # data.frame method
#' df = data.frame(ssb = c(1000, 2000, 3000, 4000, 5000),
#'                 yield = c(100, 200, 250, 200, 150))
#' result = ptMle(df, biomass = "ssb")
#' 
#' # FLBRP method (extracts time series if available)
#' result = ptMle(flbrp_object, biomass = "ssb")
#' 
#' # FLBRPs method (returns data frame with one row per iteration)
#' result = ptMle(flbrps_object, biomass = "ssb")
setGeneric("ptMle",
           function(object, biomass = "ssb", ...) standardGeneric("ptMle"))

#' @rdname ptMle
#' @export
setMethod("ptMle", signature(object="data.frame"),
          function(object, biomass = "ssb", ...) {
            
            # Validate and extract data
            data_list = validateAndExtractData(object, biomass)
            
            # Fit model using core MLE function
            result = pellaTMleCore(data_list, biomass = biomass, ...)
            
            return(result)
          })

#' @rdname ptMle
#' @export
setMethod("ptMle", signature(object="FLBRP"),
          function(object, biomass = "ssb", ...) {
            
            # Input validation
            if (!inherits(object, "FLBRP")) {
              stop("Object must be of class 'FLBRP'")
            }
            
            if (!biomass %in% c("ssb", "eb")) {
              stop("Biomass must be either 'ssb' or 'eb'")
            }
            
            # Extract biomass and catch time series
            biomass_ts = if (biomass == "ssb") ssb(object) else eb(object)
            catch_ts = catch(object)
               
            # Check if we have time series data
            if (hasNoTimeSeriesData(biomass_ts, catch_ts)) {
              warning("No time series data available in FLBRP. Using reference point approach instead.")
              return(pt(object))}
              
            # Convert to data frame for fitting
            df = createDataFrameFromTimeSeries(biomass_ts, catch_ts)
              
            if (nrow(df) < 3) {
                 warning("Insufficient valid time series data. Using reference point approach instead.")
                 return(pt(object))}
              
      # Fit Pella-Tomlinson model using the data frame method
      return(ptMle(df, biomass = "ssb", ...))
      })


#' Core MLE function for Pella-Tomlinson parameter estimation
#'
#' @description Internal function that performs maximum likelihood estimation
#' of Pella-Tomlinson model parameters from time series data.
#' Optimized with vectorized calculations and efficient parameter estimation.
#'
#' @param object List containing biomass and yield vectors
#' @param biomass Character string specifying biomass type
#' @param ... Additional optimization parameters
#' @return FLPar object with estimated parameters
#' @keywords internal
#' @export
pellaTMleCore<-function(object, biomass = "ssb", ...) {
  
  # Extract data
  biomass_data = object$biomass
  yield_data = object$yield
  n = length(biomass_data)
  
  # Get initial parameters and bounds
  init_params = getInitialParameters(biomass_data, yield_data)
  bounds = getOptimizationBounds(biomass_data)
  
  # Run optimization
  opt_result = runOptimization(init_params, bounds, biomass_data, yield_data, n, ...)
  
  # Extract and return results
  return(createResultFLPar(opt_result, biomass_data, yield_data))
}

#' Validate and extract data from data frame
#' @keywords internal
validateAndExtractData<-function(object, biomass) {
  # Input validation
  if (!inherits(object, "data.frame")) {
    stop("Object must be a data.frame")
  }
  
  if (nrow(object) < 3) {
    stop("Data frame must have at least 3 rows for meaningful fitting")
  }
  
  # Check for required columns
  if (!biomass %in% colnames(object)) {
    stop("Biomass column '", biomass, "' not found in data frame")
  }
  
  # Look for yield/catch column
  yield_cols = c("yield", "catch", "harvest", "landings")
  yield_col = intersect(yield_cols, colnames(object))[1]
  
  if (is.na(yield_col)) {
    stop("No yield column found. Expected one of: ", paste(yield_cols, collapse = ", "))
  }
  
  # Extract data
  biomass_data = object[[biomass]]
  yield_data = object[[yield_col]]
  
  # Validate data
  if (any(is.na(biomass_data)) || any(is.na(yield_data))) {
    stop("Biomass and yield data must not contain NA values")
  }
  
  if (any(biomass_data <= 0) || any(yield_data < 0)) {
    stop("Biomass must be positive and yield must be non-negative")
  }
  
  return(list(biomass = biomass_data, yield = yield_data))
}

#' Check if time series data has no meaningful dimensions
#' @keywords internal
hasNoTimeSeriesData<-function(biomass_ts, catch_ts) {
  return(all(dim(biomass_ts) == 1) || all(dim(catch_ts) == 1) || 
         length(dim(biomass_ts)) == 1 || length(dim(catch_ts)) == 1)
}

#' Create data frame from time series data
#' @keywords internal
createDataFrameFromTimeSeries<-function(biomass_ts, catch_ts) {
  # Convert to vectors
  biomass_data = c(biomass_ts)
  catch_data = c(catch_ts)
  
  # Remove any NA values and ensure positive values
  valid_indices = !is.na(biomass_data) & !is.na(catch_data) & 
                  biomass_data > 0 & catch_data >= 0
  
  # Create clean data frame
  return(data.frame(
    ssb = biomass_data[valid_indices],
    yield = catch_data[valid_indices]
  ))
}

#' Get initial parameter estimates
#' @keywords internal
getInitialParameters<-function(biomass_data, yield_data) {
  # Fast initial parameter estimates using simple heuristics
  k_init = max(biomass_data) * 1.1  # K slightly above max observed biomass
  bmsy_init = median(biomass_data)  # BMSY around median biomass
  msy_init = max(yield_data) * 0.9  # MSY around max observed yield
  
  # Fast p parameter estimation (avoid expensive FLPar operations)
  bmsy_k_ratio = bmsy_init / k_init
  p_init = if (abs(bmsy_k_ratio - exp(-1)) < 1e-6) {
    0.001  # Close to Fox model
  } else if (bmsy_k_ratio > 0.5) {
    0.1    # Conservative estimate for high BMSY/K
  } else {
    0.5    # Default estimate
  }
  
  # Fast r parameter estimation
  r_init = msy_init / (bmsy_init * (1 - (bmsy_init/k_init)^p_init))
  r_init = max(0.01, min(2.0, r_init))  # Constrain to reasonable range
  
  return(list(r = r_init, p = p_init, k = k_init))
}

#' Get optimization bounds
#' @keywords internal
getOptimizationBounds<-function(biomass_data) {
  return(list(
    lower = c(r = 0.001, p = -0.95, k = max(biomass_data) * 1.05),
    upper = c(r = 2.0,   p = 10.0,  k = max(biomass_data) * 2.0)
  ))
}

#' Vectorized production function for speed
#' @keywords internal
production_fast<-function(r, p, k, biomass) {
  if (abs(p) < 1e-10) {
    # Fox model approximation
    return(r * biomass * log(k / biomass))
  } else {
    # Pella-Tomlinson model
    return(r * biomass * (1 - (biomass/k)^p) / p)
  }
}

#' Run optimization
#' @keywords internal
runOptimization<-function(init_params, bounds, biomass_data, yield_data, n, ...) {
  # Define negative log-likelihood function (vectorized)
  nll<-function(params) {
    r = params[1]
    p = params[2]
    k = params[3]
    
    # Vectorized production calculation
    predicted_yield = production_fast(r, p, k, biomass_data)
    
    # Calculate residuals and log-likelihood
    residuals = yield_data - predicted_yield
    sigma = sqrt(mean(residuals^2))  # Use RMS instead of sd for stability
    
    if (sigma <= 0 || !is.finite(sigma)) return(1e10)
    
    # Normal log-likelihood (vectorized)
    ll = -n/2 * log(2 * pi * sigma^2) - sum(residuals^2) / (2 * sigma^2)
    
    if (!is.finite(ll)) return(1e10)
    return(-ll)}
  
  # Run optimization with better control parameters
  opt_result = optim(
    par = c(init_params$r, init_params$p, init_params$k),
    fn = nll,
    method = "L-BFGS-B",
    lower = bounds$lower,
    upper = bounds$upper,
    control = list(
      maxit = 500,      # Reduced from 1000
      factr = 1e7,      # Tighter convergence tolerance
      pgtol = 1e-5,     # Gradient tolerance
      trace = 0         # No tracing for speed
    )
  )
  
  if (opt_result$convergence != 0) {
    warning("Optimization did not converge. Code: ", opt_result$convergence)
  }
  
  return(opt_result)
}

#' Create result FLPar object
#' @keywords internal
createResultFLPar<-function(opt_result, biomass_data, yield_data) {
  # Extract fitted parameters
  r_fitted = opt_result$par[1]
  p_fitted = opt_result$par[2]
  k_fitted = opt_result$par[3]
  
  # Calculate derived parameters efficiently
  bmsy_fitted = if (abs(p_fitted) < 1e-10) {
    k_fitted / exp(1)  # Fox model
  } else {
    k_fitted * (1 / (1 + p_fitted))^(1 / p_fitted)  # Pella-Tomlinson model
  }
  
  # Calculate MSY using fast production function
  msy_fitted = production_fast(r_fitted, p_fitted, k_fitted, bmsy_fitted)
  
  # Calculate FMSY
  fmsy_fitted = msy_fitted / bmsy_fitted
  
  # Create result FLPar
  return(FLPar(
    r = r_fitted,
    p = p_fitted,
    k = k_fitted,
    virgin = k_fitted,
    bmsy = bmsy_fitted,
    fmsy = fmsy_fitted,
    msy = msy_fitted,
    scaling = 1.0
  ))
}
