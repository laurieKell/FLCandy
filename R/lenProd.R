
#' lenProd
#'
#' @title lenProd
#'
#' @description
#' Derive **length** probabilities from an \code{FLQuant} and an inverse age–length key.
#'
#' @author Laurence Kell, Sea++
#'
#' @name lenProd
#'
#' @param object \code{FLQuant} (e.g. numbers-at-age) used to generate length distributions.
#' @param invAlk \code{FLPar} with inverse age–length key.
#' @param nsample \code{numeric} sample size, kept for interface compatibility but
#'   ignored when returning probabilities.
#' @param ... any additional arguments.
#'
#' @docType methods
#'
#' @export lenProd
#' @rdname lenProd
#' @seealso setInvAlk
#'
#' @aliases lenProd
#'          lenProd-method
#'          lenProd,FLQuant,FLPar,missing-method
#'          lenProd,FLQuant,FLPar,numeric-method
#'
#' @examples
#' \dontrun{
#' lfd=lenProd(catch.n(ple4)[, ac(2000:2005)], invAlk, nsample = 100)
#' }
setGeneric('lenProd', function(object, invAlk, nsample, ...)
  standardGeneric('lenProd'))

## internal worker: returns probabilities by length
lenProdFn <- function(object, invAlk) {
  
  lfd=mdply(
    expand.grid(iter = seq(dim(object)[6]),
                year = seq(dim(object)[2])),function(iter, year) {
                  
                  ## length-frequency “densities”
                  res=object[, year, , , , iter,drop=TRUE] * invAlk[, , min(iter, dim(invAlk)[3]),drop=TRUE]
                  
                  data.frame(len=dimnames(invAlk)$len,data=(apply(res,2,sum)/sum(res)))})
  
  lfd=as.FLQuant(lfd)
  lfd[is.na(lfd)]=0
  lfd}

setMethod('lenProd',
          signature(object = "FLQuant", invAlk = "FLPar"),
          function(object, invAlk, ...)
            lenProdFn(object, invAlk))

#' lenLik
#'
#' @title lenLik
#'
#' @description
#' Multinomial **length**-composition log-likelihood for two \code{FLQuant} objects.
#'
#' @name lenLik
#'
#' @param obs \code{FLQuant} of observed numbers (or proportions) at length.
#' @param pred \code{FLQuant} of predicted numbers at length.
#' @param effN \code{numeric} effective sample size (scalar or vector over non-length cells).
#' @param weights \code{numeric} weights for each non-length cell, or single scalar weight.
#'   If missing, defaults to 1 for all cells.
#' @param log \code{logical} return log-likelihood (default) or likelihood.
#' @param ... additional arguments.
#'
#' @docType methods
#'
#' @export lenLik
#' @rdname lenLik
#'
#' @aliases lenLik
#'          lenLik-method
#'          lenLik,FLQuant,FLQuant-method
setGeneric("lenLik",
           function(obs, pred, effN = NULL, weights = NULL, log = TRUE, ...)
             standardGeneric("lenLik"))

setMethod("lenLik",
          signature(obs = "FLQuant", pred = "FLQuant"),
          function(obs, pred, effN = NULL, weights = NULL, log = TRUE, ...) {
            
            ## default weights: 1 for each non-length cell
            if (is.null(weights)) {
              dLen  = dim(obs)
              nCell = prod(dLen[-1])
              weights = rep(1, nCell)
            }
            
            lenLik(
              obs     = obs,
              pred    = pred,
              effN    = effN,
              weights = weights,
              log     = log
            )
          })

lenLik = function(obs, pred, effN = NULL, weights = NULL, log = TRUE) {
  
  if (!all(dim(obs) == dim(pred)))
    stop("obs and pred must have identical dimensions")
  
  y  = as.array(obs)
  mu = as.array(pred)
  
  dLen  = dim(y)
  nLen  = dLen[1]
  nCell = prod(dLen[-1])
  
  y  = matrix(y,  nrow = nLen, ncol = nCell)
  mu = matrix(mu, nrow = nLen, ncol = nCell)
  
  p = sweep(mu, 2, colSums(mu), "/")
  
  if (!is.null(effN)) {
    if (length(effN) == 1L) effN = rep(effN, nCell)
    if (length(effN) != nCell)
      stop("effN must be length 1 or match number of non-length cells")
    
    yProp = sweep(y, 2, colSums(y), "/")
    yCnt  = sweep(yProp, 2, effN, "*")
  } else {
    yCnt = y
  }
  
  keep = (yCnt > 0) | (p > 0)
  yCnt[!keep] = 0
  p[!keep]    = 0
  
  p[p <= 0] = .Machine$double.eps
  
  llVec = colSums(yCnt * log(p))
  
  if (!is.null(weights)) {
    if (length(weights) == 1L) weights = rep(weights, nCell)
    if (length(weights) != nCell)
      stop("weights must be length 1 or match number of non-length cells")
    llVec = llVec * weights
  }
  
  ll = sum(llVec)
  
  if (log) return(ll) else return(exp(ll))}


if (FALSE){
  library(FLCore)
  library(FLife)
  library(plyr)
  library(dplyr)
  
  load("/home/laurence/Desktop/sea++/mydas/tasks/task4/data/turbot.RData")
  invAlk=invAlk(FLPar(lh[,1]))
  lfd=lenSample(stock.n(om)[,95:100,,,,1:2],invAlk,nsample=5000)
  
  ggplot(melt(lfd))+
    geom_histogram(aes(Var.3,weight=value),binwidth=1)+
    facet_grid(year~iter)+
    xlab("Length (cm)")+ylab("Frequency")+
    scale_x_continuous(limits=c(0,45))  
}
