################################################################################
#### OM descriptive statistics & SPM priors ####################################
################################################################################
library(FLCore)
library(FLBRP)
library(mpb)
library(ggplotFL)

data(ple4)
data(ple4brp)

### Aopt FLBRP #################################################################
aopt <- function(object) {
  
  #
  fbar(object) <- FLQuant(0)
  
  res <- stock.wt(object)[,1] * stock.n(object)[,1]
  
  if (is.na(range(object)["plusgroup"])) {
    return(FLPar(aopt=apply(res, c(3,6), function(x)
      as.numeric(dimnames(x)[[1]][x==max(x)]))))
  } else {
    return(FLPar(aopt=apply(res[-dim(res)[1]], c(3, 6), function(x)
      as.numeric(dimnames(x)[[1]][x==max(x)]))))
  }
}

#aopt(ple4)
aopt(ple4brp)

### Lopt #####################################################
pars=FLPar(linf=100)

lopt=vonB(aopt(ple4brp),lhPar(pars))
lopt%/%pars["linf"]
lopt%/%pars["l50"]

### Aopt FLStock#################################################################
aopt <- function(object) {
  
  fbar=fbar(object)%=%0
  object=window(object,start=dims(object)$minyear-1)
  stock.n(object)[1]=1

  object=fwd(object,fbar=fbar[,-1],sr=list(model="geomean",params=FLPar(a=1)))[,-1]
  
  res <- stock.wt(object)[,-1] * stock.n(object)[,-1]
  
  if (is.na(range(object)["plusgroup"])) {
    apply(res,c(2:6), function(x) as.numeric(names(x)[x==max(x)]))
  } else {
    apply(res[-dim(res)[1]],c(2:6), function(x) as.numeric(names(x)[x==max(x)]))
  }
}

plot(aopt(ple4))+ylab("Age at max Biomass")
