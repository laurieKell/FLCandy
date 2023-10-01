#' @title Leslie matrix
#' 
#' @description
#' Creates a Leslie Matrix from a \code{FLBRP} object that represents a population at equilibrium
#'  
#' @param object \code{FLBRP}
#' @param fbar \code{numeric} F at whicj survival calculated
#' @param numbers \code{boolean} numbers or biomass, numbers bt default
#' @param ... any other arguments
#' 
#' @aliases leslie leslie-method leslie,FLBRP-method
#' 
#' @return \code{matrix}  
#' 
#' @export
#' @docType methods
#' @rdname leslie
#' 
#' @seealso \code{\link{lhRef}}, \code{\link{lhPar}}, \code{\link{lhEql}}
#'  
#' 
#' @examples
#' \dontrun{
#' eql=lhEql(lhPar(FLPar(linf=100)))
#' leslie(eql)
#' }
#' 
### Leslie Methods to estimate "r" #############################################
### Should reuse same code, and be dispatched on "FLObjects", i.e.
### FLPar
### FLBiol
### FLStock
### FLBRP
##  calculated as the log of the eigen value of L, and 
## should be calculated for different levels of survival, i.e. Zs

### Fishing mortality reference points can be derived as
# Fmsy=r/2, or as 
# Fmsy=0.87M for teleosts and 
# Fmsy=0.41M for sharks. 

### Limit reference points can be derived as 
# Flim = 1.5Fmsy and 
# Fcrash = 2Fmsy.  

leslieFn<-function(object,fec){
      if (length(object)!=length(fec)) stop("Vectors differ in length")
      
      L=matrix(0,length(object),length(object))
      diag(L[-1,-length(object)])=object[-length(object)]
      L[1,]=fec
      
      #plusgroup
      L[length(object),length(object)]=L[length(object),length(object)-1]
      L}

if(FALSE){
survivors=cumprod(rep(0.5,10))
fec      =c(0,rep(1,9))
L        =leslieFn(survivors,fec)
L
  with(subset(mvln$kb,year==2021),cov(log(stock),log(harvest)))
with(subset(mvln$kb,year==2021),cov(log(stock),log(harvest)))
demogR:::eigen.analysis(L)
}

setGeneric('leslie', function(object, fec, ...) standardGeneric('leslie'))
setMethod("leslie", signature(object="numeric",fec="numeric"),  
          function(object,fec) {
                 leslieFn(object,fec)})

if(FALSE){
L=leslie(survivors,fec)
eigen.analysis(L)
}

setMethod("leslie", signature(object="FLBRP",  fec="missing"),  
          function(object,f=refpts(object)["crash","harvest"]) {
            
            # need to coerce to FLQuant and keep iters      
            fbar(object)=as.FLQuant(f[drop=T])
            
            survivors=exp(-m(object)-harvest(object))
            fec      =stock.n(object)%*%
              exp(-(harvest(object)%*%(harvest.spwn(object))%+%
                      m(      object)%*%(m.spwn(      object))))%*%
              stock.wt(object)%*%mat(object)
            
            L=array(0,c(dim(fec)[1],dim(fec)[1],dim(fec)[6]))
            for (i in seq(dims(object)$iter))
              L[,,i]=leslieFn(iter(survivors,i)[drop=TRUE],iter(fec,i)[drop=TRUE])
            
            
            return(L)})

if(FALSE){
L=leslie(ple4brp)
eigen.analysis(L[,,1,drop=T])
}

setMethod("leslie", signature(object="FLQuant",fec="FLQuant"),  function(object,fec) {
  })
  
setMethod("leslie", signature(object="FLPar",  fec="missing"),  function(object,f=function(x) refpts(object)["crash","harvest"]) {
  res=lhEql(lhPar(object))
  
  leslie(res,f=f(res))
  })

setMethod("leslie", signature(object="FLBiol", fec="missing"),  function(object) {
 })
setMethod("leslie", signature(object="FLStock",fec="missing"),  function(object) {
 })


if(FALSE){
library(FLCore)
library(FLBRP)
library(FLasher)

library(FLife)
library(FLSRTMB)

library(popbio)

library(FishLife)
library(SPMpriors)

library(demogR)
library(epimdr2)
library(lefko3)
library(popReconstruct)

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


library(FLCore)
library(FLife)
library(mydas)
library(popbio)
library(numDeriv)

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


                 



