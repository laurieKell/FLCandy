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
#' @rdname pellaTparams
#' 
#' @examples
#' # Using numeric vector
#' refs <- c(fmsy=0.2, bmsy=1000, b0=2000)
#' pellaTparams(refs)
#' 
#' @references
#' Pella, J.J. and Tomlinson, P.K. (1969) A generalized stock production model. 
#' Inter-American Tropical Tuna Commission Bulletin 13: 419-496
#' 
#' @seealso 
#' \code{\link[FLCore]{FLPar}}, \code{\link[FLCore]{FLBRP}}
#' @rdname pellaTparams
setMethod("pellaTparams", signature(object="FLPar"),
          function(object, interval=c(1.1,10)) {
            if(!all(c("fmsy","bmsy","k") %in% dimnames(object)$params))
              stop("FLPar must contain fmsy, bmsy and k parameters")
            
            # Get dimensions
            dims=dim(object)[2]
            
            # Initialize output FLPar
            res=FLPar(array(NA, dim=c(3,dims),
                               dimnames=list(params=c("r","k","p"),
                                             iter=seq(dims))))
            
            # Loop over iterations
            for(i in seq(dims)) {
              # Calculate shape parameter m from Bmsy/K ratio
              BmsyK=c(object["bmsy",i])/c(object["k",i])
              fmsy =1 - exp(-c(object["fmsy",i]))
              
              # Set interval based on BmsyK
              if(BmsyK < 0.37) 
                int=c(0.01,0.99)
              else 
                int=interval
              
              # Optimize for m
              m=optimize(function(x) abs(BmsyK - (1/x)^(1/(x-1))), 
                            interval=int)$minimum
              
              # Calculate r
              r=fmsy * (m-1)/(1-1/m)
              
              # Store results
              res["r",i]=r
              res["k",i]=c(object["k",i])
              res["p",i]=m-1
            }
            
            return(res)
          })

#' @rdname pellaTparams
setMethod("pellaTparams", signature(object="FLBRP"),
          function(object, interval=c(1.1,10)) {
            rfpts=refpts(object)
            
            params=FLPar(
              fmsy=rfpts["msy","harvest"],
              bmsy=rfpts["msy","ssb"],
              b0=rfpts["virgin","ssb"],
              iter=dim(rfpts)[3])
            
            pellaTparams(params, interval=interval)
          })

#' @rdname pellaTparams
setMethod("pellaTparams", signature(object="numeric"),
          function(object, interval=c(1.1,10)) {
            if(!all(c("fmsy","bmsy","b0") %in% names(object)))
              stop("fmsy, bmsy and b0 not found")
            
            params=FLPar(array(object, dim=c(3,1),
                                  dimnames=list(params=names(object),
                                                iter=1)))
            
            return(pellaTparams(params, interval=interval))
          })

BMSYFn<-function(M,B0,r) {
  
  prodFn <- function(B)
    r*B*(1-(B/B0)^M)
  
  BMSY=optimize(prodFn, interval = c(0, B0), maximum = TRUE)$maximum
  return(BMSY)}

MFn<-function(M) {
  BMSY  =BMSYFn(M, B0, r)
  ratio =BMSY/B0
  return(ratio-target)}

if(FALSE){
  pellaTparams(Fmsy=0.1,Bmsy=600,B0=1000)
  
  target=0.4
  
  B0=1000  
  r =0.4    
  
  result=uniroot(MFn, interval = c(0.001, 2), tol = 1e-6)
  BMSYFn(result$root,B0,r)
  
  result$root}

msy2pellatFn=function(x,refs) {
  r=x[1]
  k=x[2]
  p=x[3]
  
  msyFn  =function(r,k,p) r*k*(1/(1+p))^(1/p+1)
  bmsyFn =function(r,k,p) k*(1/(1+p))^(1/p)
  fmsyFn =function(r,  p) r*(1/(1+p))
  ratioFn=function(    p) (1/(1+p))^(1/p)
  
  res=return(((refs["bmsy"] -bmsyFn(r,k,p))/refs["bmsy"])^2+
               ((refs[ "msy"] -msyFn(r,k,p))/refs["msy"])^2+
               ((refs["ratio"]-ratioFn(p))/refs["ratio"])^2)
  
  res}

# setMethod('pellat',  signature(object="FLBRP"),  
#    function(object,quantity="ssb"){
#      
#        par=c(c(FLBRP:::refpts(object)["msy","yield"]),
#              c(FLBRP:::refpts(object)["msy",quantity]),
#              c(FLBRP:::refpts(object)["msy",quantity]/FLBRP:::refpts(object)["virgin",quantity]))
#       
#        par=FLPar(array(par,dim=c(3,dim(FLBRP:::refpts(object))[3]),
#                               dimnames=list(params=c("msy","bmsy","ratio"),
#                                             iter=seq(dim(FLBRP:::refpts(object))[3]))))  
#        pellat(par)})


