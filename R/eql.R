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
#' eql=eql(ple4, model="rickerSV")
#' summary(eql)
#' }
#' 
#' @rdname eql
#' @export
setGeneric("eql", function(object, model,...) standardGeneric("eql"))

#' @rdname eql
#' @export
setMethod("eql", signature(object="FLStock"),
          eqFn<-function(object,model="bevholtSV",nyears=dim(object)[2],
                         prior_s=NULL,cv_s=NULL,prior_r0=NULL,cv_r0=NULL){
            
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
      
            sr  =as.FLSR(object,model=model)
            spr0=mean(FLCandy::spr0Yr(object)[,dim(object)[2]-(1:nyears)+1])
            
            if (model!="segreg"){
              sr  =FLCandy::ftmb2(sr,s.est =T,
                                   s        =0.7, #fishlife(object)["s"],
                                   s.logitsd=0.4, #fishlife(object)["sd.logit.s"],
                                   spr0     =spr0,
                                   prior_s=prior_s,cv_s=cv_s,prior_r0=prior_r0,cv_r0=cv_r0)
            }else{
              sr  =FLCandy::ftmb2(sr,s.est =T,
                           inflect  =ifelse("benchmark"%in%names(attributes(object)),
                                            benchmark(object)["blim",drop=TRUE],NA),
                           spr0=spr0)}
            
            # ftmb2 should always return an FLSR object
            if(is(sr, "FLPar")) {
              # If ftmb2 returned FLPar, create FLSR object
              sr_params=FLPar(apply(sr,1,median))
              sr_obj=as.FLSR(object, model=model)
              params(sr_obj)=sr_params
            } else {
              # ftmb2 returned FLSR object
              sr_params=FLPar(apply(params(sr),1,median))
              sr_obj=sr
            }
          
            rtn=brp(FLBRP(object,nyears=nyears,
                          sr=list(model =do.call(gsub("SV","", model),list())$model,
                                  params=sr_params)))
         
            attributes(rtn)[["sr"]]     =sr_obj
            attributes(rtn)[["logLik"]] =logLik(sr_obj)
            attributes(rtn)[["prod"]]   =spFn(rtn)
            attributes(rtn)[["tseries"]]=FLCandy::tseries(object)
            attributes(rtn)[["eb.obs"]] =ebiomass(object)
            attributes(rtn)[["priors"]] =FLCandy::tryIt(FLCandy::calcPriors(rtn))
            attributes(rtn)[["prior2"]] =FLCandy::tryIt(FLCandy::getPriors(rtn))
            
            if ("benchmark"%in%names(attributes(object)))
              attributes(rtn)[["benchmark"]]=benchmark(object)
            
            return(rtn)})

#' Simple spr0Yr function
#' @param object FLStock object
#' @return Numeric value
spr0Yr <- function(object) {
  FLCandy::spr0Yr(object)
  }

