#' Calculate Pella-Thompson Production Model Parameters
#' 
#' @description 
#' Methods to calculate parameters (r, K, p) for the Pella-Thompson production model
#' from reference points (FMSY, BMSY, B0). Multiple methods are provided for different
#' input object classes.
#' 
#' @param object Input object of class 'FLPar', 'FLBRP', or 'numeric'
#' @param interval Numeric vector of length 2 specifying the interval for shape parameter optimization.
#'                Default c(1.1, 10)
#' 
#' @return An FLPar object containing:
#' \itemize{
#'   \item r - Intrinsic rate of population growth
#'   \item k - Carrying capacity
#'   \item p - Shape parameter
#' }
#' 
#' @details
#' The methods estimate Pella-Thompson model parameters using different input types:
#' \itemize{
#'   \item FLPar: Requires fmsy, bmsy, and k parameters
#'   \item FLBRP: Extracts reference points from FLBRP object
#'   \item numeric: Requires named vector with fmsy, bmsy, and b0
#' }
#' 
#' @export
#' @rdname pellatParams
#' 
#' @examples
#' # Using numeric vector
#' refs <- c(fmsy=0.2, bmsy=1000, b0=2000)
#' pellatParams(refs)
#' 
#' @references
#' Pella, J.J. and Tomlinson, P.K. (1969) A generalized stock production model. 
#' Inter-American Tropical Tuna Commission Bulletin 13: 419-496
#' 
#' @seealso 
#' \code{\link[FLCore]{FLPar}}, \code{\link[FLCore]{FLBRP}}
#' @rdname pellatParams
setGeneric("pellatParams", function(object,biomass,...) standardGeneric("pellatParams"))

setMethod("pellatParams", signature(object="FLPar"),
    function(object){ 

    if ("b0"%in%dimnames(object)$params)
      dimnames(object)$params["b0"==dimnames(object)$params]="k"
      
    if ("fmsyMedianC"%in%dimnames(object)$params)
      dimnames(object)$params["fmsyMedianC"==dimnames(object)$params]="fmsy"
      
    if(!all(c("fmsy","bmsy","k") %in% dimnames(object)$params))
      stop("FLPar must contain fmsy, bmsy and k parameters")
            
    # Get dimensions
    dims=dim(object)[2]
            
    # Initialize output FLPar
    res=FLPar(array(NA, dim     =c(3,dims),
                        dimnames=list(params=c("r","k","p"),
                                             iter=seq(dims))))
            
    # Loop over iterations
    for(i in seq(dims)) {
        # Calculate shape parameter m from Bmsy/K ratio
        BmsyK=c(object["bmsy",i])/c(object["k",i])
        fmsy =1 - exp(-c(object["fmsy",i]))
              
        m=getM(BmsyK)
        
        # Calculate r
        r=fmsy*(m-1)/(1-1/m)
              
        # Store results
        res["r",i]=r
        res["k",i]=c(object["k",i])
        res["p",i]=m-1}
            
    return(res)})

#' @rdname pellatParams
setMethod("pellatParams", signature(object="FLBRP"),
    function(object) {
      rfpts=refpts(object)
            
       params=FLPar(
              fmsy=rfpts["msy","harvest"],
              bmsy=rfpts["msy","ssb"],
              k   =rfpts["virgin","ssb"],
              iter=dim(rfpts)[3])
            
        pellatParams(params)})

#' @rdname pellatParams
setMethod("pellatParams", signature(object="numeric"),
    function(object) {
      if(!all(c("fmsy","bmsy","b0") %in% names(object)))
        stop("fmsy, bmsy and b0 not found")
            
      params=FLPar(array(object, 
                    dim     =c(3,1),
                    dimnames=list(params=names(object),
                                                iter=1)))
            
          return(pellatParams(params))})

setMethod("pellatParams", signature(object="FLBRP",biomass="function"),
  function(object,biomass=ssb) {
      
    ctc=catch(  object)
    eb =biomass(object)
      
    pars=FLPar(msy =max(ctc),                     
               bmsy=eb[ctc==max(ctc)],
               fmsy=max(ctc)/eb[ctc==max(ctc)],
               k   =max(eb,na.rm=TRUE))
      
    return(pellatParams(pars))})

#' @rdname pellatParams
#' @export
setMethod("pellatParams", signature(object="missing", biomass="missing"),
          function(object, biomass, fmsy, bmsy, k=NULL, b0=NULL, virgin=NULL, ...) {
            # Determine which carrying capacity parameter to use
            if(is.null(k)) k=virgin
            if(is.null(k)) k=b0
            
            if(is.null(k))
              stop("One of k, virgin, or b0 must be provided")
            
            # Create numeric vector with named elements
            refs=FLPar(fmsy = fmsy,
                       bmsy = bmsy,
                       b0   = k)
            
            # Call the numeric method
            model.frame(pellatParams(refs))[,-4]})


pellatParamFn<-function(fmsy,bmsy,b0){
  # Calculate shape parameter m from Bmsy/K ratio
  BmsyK=bmsy/b0
  m    =optimize(function(x) abs(BmsyK - (1/x)^(1/(x-1))), interval=c(0.1,10))$minimum
  r   =fmsy*(m-1)/(1-1/m)
  
  return(c(r=r,m=m,k=b0))}

BMSYFn<-function(M,B0,r) {
  prodFn <- function(B)
    r*B*(1-(B/B0)^M)
  
  BMSY=optimize(prodFn, interval = c(0, B0), maximum = TRUE)$maximum
  return(BMSY)}

MFn<-function(M) {
  BMSY  =BMSYFn(M, B0, r)
  ratio =BMSY/B0
  return(ratio-target)}

getM<-function(bmsyK){
    
  # Function to solve for m
  fn<-function(m, bmsyK) 
    (m/(m+1))^(1/m)-bmsyK
    
  # Use uniroot to find the root of the equation
  result=uniroot(fn, interval=c(1e-6, 1e6), bmsyK=bmsyK)
    
  # Extract the shape parameter m
  result$root}


if(FALSE){
  pellatParams(Fmsy=0.1,Bmsy=600,B0=1000)
  
  target=0.4
  
  B0=1000  
  r =0.4    
  
  result=uniroot(MFn, interval = c(0.001, 2), tol = 1e-6)
  BMSYFn(result$root,B0,r)
  
  result$root}

p2shape<-function(p) (1/(1+p))^(1/p)