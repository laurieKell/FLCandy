#' @param r Intrinsic rate of population increase
#' @param K Carrying capacity
#' @param p Shape parameter of the Pella-Tomlinson model (use NULL if specifying shape)
#' @param shape Ratio of BMSY to K (use NULL if specifying p)
#'
#' @return A list containing FMSY, BMSY, MSY, shape parameter p, and BMSY/K ratio
#' @examples
#' # Using shape parameter (p=1 for Schaefer model)
#' calc_pt_ref_points(0.2, 1000, p=1)
#'
#' # Using BMSY/K ratio (0.4 is between Schaefer and Fox)
#' calc_pt_ref_points(0.2, 1000, shape=0.4)
#'
pellatRefpts<-function(r,K,p=NULL,shape=NULL) {

# Check if exactly one of p or shape is provided

if (is.null(p) & is.null(shape)) {
stop("Either p or shape must be provided")}

if (!is.null(p) & !is.null(shape)) {
stop("Only one of p or shape should be provided")}

# If shape is provided, solve for p

if (is.null(p)) {
# Validate shape is in valid range (0,1)
if (shape <= 0 || shape >= 1) {
stop("shape must be between 0 and 1")}

    # Function to find p given shape
    calcP<-function(shape){
      
      fn<-function(x,y)
        (y-(1/(1+x))^(1/x))^2
      
      if (shape<0.3678794)
        optimise(fn,c(-0.9999,-1e-20),y=shape)$minimum
      else
        optimise(fn,c(1e-20,10),y=shape)$minimum}
    
    p=calcP(shape)
    } else {
  # Calculate shape from p
  shape=p2shape(p)}

  
# Calculate BMSY

bmsy=K*shape

# Calculate FMSY

#fmsy=r*(p/(p+1))

m=p+1
fmsy=r*(1-1/m)/(m-1)

# Calculate MSY

msy=r*bmsy*(1-(bmsy/K)^m)

# Return the results as a list

return(c(
fmsy=fmsy,
bmsy=bmsy,
msy =msy,
p   =p,
shape= shape
))}

#' Calculate Reference Points from Pella-Tomlinson Model Parameters
#'
#' S4 generic method to calculate FMSY, BMSY, MSY and related parameters 
#' from Pella-Tomlinson model parameters. Works with both numeric vectors
#' and FLPar objects.
#'
#' @param r Intrinsic rate of population increase
#' @param K Carrying capacity
#' @param p Shape parameter of the Pella-Tomlinson model (use NULL if specifying shape)
#' @param shape Ratio of BMSY to K (use NULL if specifying p)
#' @param ... Additional arguments (not used)
#'
#' @return For numeric inputs: a named vector (for single values) or data frame (for multiple values)
#'         For FLPar inputs: an FLPar object
#' 
#' @examples
#' # Using shape parameter (p=1 for Schaefer model)
#' pellatRefpts(0.2, 1000, p=1)
#' 
#' # Using BMSY/K ratio (0.4 is between Schaefer and Fox)
#' pellatRefpts(0.2, 1000, shape=0.4)
#'
#' # Using vectors
#' pellatRefpts(c(0.2, 0.3), c(1000, 2000), p=c(1, 0.5))
#'

# Helper functions
p2shape <- function(p) {
  (1/(p+1))^(1/p)
}

calcP <- function(shape) {
  fn <- function(x, y) {
    (y-(1/(1+x))^(1/x))^2
  }
  
  if (shape < 0.3678794)  # Approximately 1/e
    optimise(fn, c(-0.9999, -1e-20), y=shape)$minimum
  else
    optimise(fn, c(1e-20, 10), y=shape)$minimum
}

# Define generic function
setGeneric("pellatRefpts", function(r, K, p=NULL, shape=NULL, ...) {
  standardGeneric("pellatRefpts")
})

# Method for numeric vectors
setMethod("pellatRefpts", signature(r="numeric", K="numeric"),
  function(r, K, p=NULL, shape=NULL, ...) {
    # Input validation
    if (is.null(p) && is.null(shape)) {
      stop("Either p or shape must be provided")
    }
    if (!is.null(p) && !is.null(shape)) {
      stop("Only one of p or shape should be provided")
    }
    
    # Vectorize calculations
    len <- max(length(r), length(K), 
               ifelse(is.null(p), 0, length(p)), 
               ifelse(is.null(shape), 0, length(shape)))
    
    # Replicate inputs to match length
    r <- rep_len(r, len)
    K <- rep_len(K, len)
    
    # Determine p and shape
    if (is.null(p)) {
      # Validate shape is in valid range (0,1)
      if (any(shape <= 0 | shape >= 1)) {
        stop("shape must be between 0 and 1")
      }
      shape <- rep_len(shape, len)
      p <- sapply(shape, calcP)
    } else {
      p <- rep_len(p, len)
      shape <- sapply(p, p2shape)
    }
    
    # Calculate reference points
    bmsy <- K * shape
    m <- p + 1
    fmsy <- r * (1-1/m) / (m-1)
    msy <- r * bmsy * (1 - (bmsy/K)^m)
    
    # Create result
    if (len == 1) {
      # Return named vector for single values
      result <- c(
        fmsy = fmsy,
        bmsy = bmsy,
        msy = msy,
        p = p,
        shape = shape
      )
    } else {
      # Return data frame for multiple values
      result <- data.frame(
        fmsy = fmsy,
        bmsy = bmsy,
        msy = msy,
        p = p,
        shape = shape
      )
    }
    
    return(result)
  }
)

# Method for FLPar objects
setMethod("pellatRefpts", signature(r="FLPar", K="FLPar"),
  function(r, K, p=NULL, shape=NULL, ...) {
    require(FLCore)
    
    # Convert FLPar to vectors for calculation
    r_vec <- c(r)
    K_vec <- c(K)
    
    # Handle p or shape parameter
    if (!is.null(p)) {
      if (is(p, "FLPar")) {
        p_vec <- c(p)
      } else {
        p_vec <- p
      }
      result <- pellatRefpts(r_vec, K_vec, p=p_vec)
    } else {
      if (is(shape, "FLPar")) {
        shape_vec <- c(shape)
      } else {
        shape_vec <- shape
      }
      result <- pellatRefpts(r_vec, K_vec, shape=shape_vec)
    }
    
    # Convert result to FLPar
    if (is.data.frame(result)) {
      # Create FLPar from data frame
      params <- c("fmsy", "bmsy", "msy", "p", "shape")
      out <- FLPar(array(as.matrix(result), 
                         dim=c(length(params), nrow(result)),
                         dimnames=list(params=params, iter=1:nrow(result))))
    } else {
      # Create FLPar from named vector
      out <- FLPar(array(result, 
                         dim=c(length(result), 1),
                         dimnames=list(params=names(result), iter=1)))
    }
    
    return(out)
  }
)

# Optional: Method for mixed inputs (when one is FLPar and one is numeric)
setMethod("pellatRefpts", signature(r="numeric", K="FLPar"),
  function(r, K, p=NULL, shape=NULL, ...) {
    # Convert numeric to FLPar with same dimensions as K
    r_flpar <- K
    r_flpar[] <- r
    pellatRefpts(r_flpar, K, p=p, shape=shape)
  }
)

setMethod("pellatRefpts", signature(r="FLPar", K="numeric"),
  function(r, K, p=NULL, shape=NULL, ...) {
    # Convert numeric to FLPar with same dimensions as r
    K_flpar <- r
    K_flpar[] <- K
    pellatRefpts(r, K_flpar, p=p, shape=shape)
  }
)

