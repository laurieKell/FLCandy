#' @title Calculate Fishery Indicators
#' @description Computes a suite of biological reference indicators for stock assessment using FLR objects.
#'
#' @param x An FLStock object containing stock data
#' @param y An optional FLBRP object containing reference points (default missing)
#' 
#' @return A data.frame of biological indicators including:
#' \itemize{
#'   \item SSB/SSBmsy
#'   \item F/Fmsy
#'   \item Spawning potential ratio (SPR)
#'   \item Maturity/age structure metrics
#' }
#' 
#' @examples
#' \dontrun{
#' data(ple4)
#' brp <- FLBRP(ple4)
#' ind(ple4) # Without reference points
#' ind(ple4, brp) # With reference points
#' }
#' 
#' @rdname ind
#' @export
setGeneric("ind", function(x, y, ...) standardGeneric("ind"))

#' @rdname ind
setMethod("ind", signature(x="FLStock", y="missing"),
          function(x){          
           fmsy=FLPar(attributes(x)$benchmark["Fmsy"])
           bmsy=FLPar(attributes(x)$eqsim["BMSY"]) 
            
            rtn=FLQuants(SSB   =ssb( x)%/%bmsy,
                         F     =fbar(x)%/%fmsy,
                         SPR0  =FLCandy:::spr0Yr(x),
                         #FRatio=fjuv(x[-1])%/%fapex(x[-1]),
                         SPR   =ssb(x)/rec(x),
                         amat  =FLCandy:::amat(mat(x),0.5,what="i"),
                         wmat  =FLCandy:::wmat(x),
                         POS   =FLCandy:::pos(x),
                         ASA   =FLCandy:::asa(x),
                         POS_  ={stock.n(x)=catch.n(x); m.spwn(x)=0; pos(x)},
                         ASA_  ={stock.n(x)=catch.n(x); m.spwn(x)=0; asa(x)})
            
            if (!missing(y)) rtn[["ABI"]] = abi(x,y)
            
            model.frame(rtn,drop=T)})

setMethod("ind", signature(x="FLStock", y="FLBRP"),
          function(x,y){
            
            bmsy=refpts(y)["msy","ssb"]
            fmsy=refpts(y)["msy","harvest"]
            
            rtn=FLQuants(SSB   =ssb( x)%/%bmsy,
                         F     =fbar(x)%/%fmsy,
                         SPR0  =FLCandy:::spr0Yr(x),
                         #FRatio=fjuv(x[-1])%/%fapex(x[-1]),
                         SPR   =ssb(x)/rec(x),
                         amat  =FLCandy:::amat(mat(x),0.5,what="i"),
                         wmat  =FLCandy:::wmat(x),
                         POS   =FLCandy:::pos(x),
                         ASA   =FLCandy:::asa(x),
                         POS_  ={stock.n(x)=catch.n(x); m.spwn(x)=0; pos(x)},
                         ASA_  ={stock.n(x)=catch.n(x); m.spwn(x)=0; asa(x)})
            
            if (!missing(y)) rtn[["ABI"]] = abi(x,y)
            
            model.frame(rtn,drop=T)})

#' @title Equilibrium Model Fitting
#' @description Fits stock-recruitment models and computes biological reference points using FLR framework.
#'
#' @param object An FLStock object
#' @param model Stock-recruitment model type (default="bevholtSV")
#' 
#' @return An FLBRP object with additional attributes:
#' \itemize{
#'   \item sr - Fitted stock-recruitment model
#'   \item logLik - Model likelihood
#'   \item prod - Production characteristics
#'   \item tseries - Time series metrics
#' }
#' 
#' @examples
#' \dontrun{
#' data(ple4)
#' eql <- eql(ple4, model="rickerSV")
#' summary(eql)
#' }
#' 
#' @rdname eql
#' @export
setGeneric("eql", function(object, model) standardGeneric("eql"))

#' @rdname eql
setMethod("eql", signature(object="FLStock"),
        function(object,model="bevholtSV"){
  
  spFn<-function(x){
    rfs=FLPar(c(ssb.obs(x)),dimnames=list(refpts="ssb",
                                          quant =dimnames(refpts(x))$quant,
                                          iter  =seq(dim(ssb.obs(x))[2])))
    rfs[,-4]=NA
    refpts(x)=rfs
    
    rtn=data.frame(model.frame(FLQuants(x,ssb=ssb.obs,catch=catch.obs),drop=TRUE),
                   sp=c(computeRefpts(x)[,"yield"]))
    rtn$pe=(c(rtn$ssb[-1]-rtn$ssb[-dim(rtn)[1]]+rtn$catch[-dim(rtn)[1]]-
                rtn$sp[-dim(rtn)[1]],NA))/rtn$ssb
    
    rtn}
  
  spr0=FLCandy:::spr0Yr(object)
  sr  =as.FLSR(object,model=model)
  sr  =FLCandy:::ftmb(sr,s.est    =T,
                      s        =0.7, #fishlife(object)["s"],
                      s.logitsd=0.4, #fishlife(object)["sd.logit.s"],
                      spr0     =spr0)
  
  rtn=brp(FLBRP(object,nyears=dim(object)[2],
                sr=list(model =do.call(gsub("SV","", model),list())$model,
                        params=FLPar(apply(params(sr),1,median)))))
  
  attributes(rtn)[["sr"]]     =sr
  attributes(rtn)[["logLik"]] =logLik(sr)
  attributes(rtn)[["prod"]]   =spFn(rtn)
  attributes(rtn)[["tseries"]]=tseries(object)
  attributes(rtn)[["priors"]] =tryIt(calcPriors(rtn))
  attributes(rtn)[["prior2"]] =tryIt(FLCandy:::getPriors(rtn))
  
  return(rtn)})
