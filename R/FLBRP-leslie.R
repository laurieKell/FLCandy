#' Leslie Matrix Population Model
#' 
#' @description 
#' Creates a Leslie Matrix from population parameters to model age-structured population 
#' dynamics at equilibrium. Calculates survival rates and fecundity to estimate 
#' population growth rate (r).
#'
#' @param object An object of class FLBRP, numeric, FLQuant, FLPar, FLBiol, or FLStock
#' @param fec Fecundity vector (optional)
#' @param f Fishing mortality rate (default: crash F from reference points)
#' @param numbers Logical; if TRUE returns numbers, if FALSE returns biomass (default: TRUE)
#'
#' @details
#' The Leslie matrix (L) is constructed as:
#' - First row contains fecundity values
#' - Sub-diagonal contains survival probabilities
#' - Plus group handled in last row
#' 
#' Reference points derived as:
#' - Fmsy = r/2 (general) or 0.87M (teleosts) or 0.41M (sharks)
#' - Flim = 1.5Fmsy
#' - Fcrash = 2Fmsy
#'
#' @return
#' A matrix (for numeric inputs) or array (for FLR objects) containing the Leslie matrix
#'
#' @examples
#' \dontrun{
#' # Simple numeric example
#' survivors <- cumprod(rep(0.5, 10))
#' fec <- c(0, rep(1, 9))
#' L <- leslie(survivors, fec)
#' 
#' # FLR example
#' eql <- lhEql(lhPar(FLPar(linf=100)))
#' L <- leslie(eql)
#' }
#'
#' @importFrom methods setMethod
#' @importFrom FLCore fbar harvest m mat stock.n stock.wt harvest.spwn m.spwn
#'
#' @export
#' @docType methods
#' @rdname leslie
#'
#' @aliases leslie leslie-method leslie,FLBRP-method leslie,numeric-method 
#' leslie,FLQuant-method leslie,FLPar-method leslie,FLBiol-method leslie,FLStock-method
#'
#' @seealso 
#' \code{\link{lhRef}} \code{\link{lhPar}} \code{\link{lhEql}}
#' 

if(FALSE){
require(FLCore)
require(FLBRP)
require(FLasher)

require(FLife)
require(FLSRTMB)

require(popbio)

require(FishLife)
require(SPMpriors)

require(demogR)
require(epimdr2)
require(lefko3)
require(popReconstruct)

ag=c(1:10,39:41)

## convert between rates and instantaneous values
h<-function(x) 1-exp(-x)
f<-function(x) -log(1-x)

par=lhPar(FLPar(linf=100))
brp=lhEql(par)
fls=as(brp,"FLStock")
fls=fwd(fls,f=fbar(brp)[,-1],sr=brp)
flb=as(fls,"FLBiol")

## Current Methods #############################################################
#setGeneric("leslie", function(object, ...)
#  standardGeneric("leslie"))

# FLCore leslie
#setMethod("leslie", signature(object="FLBiol"),
#          function(object, plusgroup = FALSE, ...) {
Lbiol=FLCore:::leslie(flb)
is(Lbiol)

r(m(fls),mat(fls))

# FLife leslie
#setMethod("leslie", signature(object="FLBRP"),
#          function(object,fbar=FLQuant(0),numbers=TRUE,...){
Lbrp=FLife:::leslie(brp,fbar=FLQuant(c(refpts(brp)["crash","harvest"])))
is(Lbrp)

exp(lambda(Lbrp[drop=T]))


# SPMPriors leslie_r()
#SPMPriors:::leslie_r <- function(Loo=80,K=0.2,t0=-0.5,aW=0.01,bW=3.04,mat=c(35,0),dmat=NULL,minage=0,maxage=12,M=0.25,h=0.7){
leslie_r(Loo=c(par["linf"]),K=c(par["k"]),t0=c(par["t0"]),aW=c(par["a"]),bW=c(par["b"]),mat=c(par["l50"],0),dmat=NULL,minage=0,maxage=12,M=0.25,h=c(par["s"]))

## leslie should give same r for numbers and mass
leslie_r(Loo=c(par["linf"]),K=c(par["k"]),t0=c(par["t0"]),aW=1,bW=1,mat=c(par["l50"],0),dmat=NULL,minage=0,maxage=12,M=0.25,h=c(par["s"]))


# FLSRTMB used internally
FLSRTMB::productivity(fls)


# r=FCrash
1-exp(-(refpts(brp)["crash","harvest"]))

            
             
# FishLife LeslieM
FishLife:::get_r(vec=NULL,
      Linf =c(par["linf"]),
      K    =c(par["k"]),
      t0   =c(par["t0"]),
      W_a  =c(par["a"]),
      W_b  =c(par["b"]),
      tm   =c(20),
      dm   =20/4,
      maxage=ceiling(min(100,2*20)),
      h=c(par["s"]),
      M=0.2 )


# leslie.matrix {demogR}
leslie.matrix(c(1-h(z(fls)[,100])),c(mat(fls)[,100]))
eigen.analysis(L)
              
# leslie {epimdr2}
fa=c(1-h(z(fls)[,100]))
sa=c(mat(fls)[,100])
L<-matrix(0, nrow=41, ncol=41)
L[1,]<-fa
L[row(L)==col(L)+1] <-sa[1:2]
leslie(L)
L
L[1:5,1:5]

# rleslie {lefko3}
# make.leslie.matrix {popReconstruct}


require(FLCore)
require(FLife)
require(mydas)
require(popbio)
require(numDeriv)

eq=lhEql(lhPar(FLPar(linf=100)))
L =leslie(eq,f=c(refpts(eq)["msy","harvest"]))

fn<-function(x,L) {L[1,]=x; exp(lambda(L))}

x=L[1,drop=T]
jac=jacobian(fn,x,L=L[drop=T])

vc=diag(dim(L)[1])  
diag(vc)=(x/.33)^2
var =(jac) %*% vc %*% t((jac))
var^0.5

exp(lambda(L[drop=T]))
}


                 



