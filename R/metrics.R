#' omSmry
#' 
#' @title omSmry 
#' 
#' @description create time series of summary statistics from \code{FLStock} relative to reference points
#' @author Laurence Kell, Sea++
#' @name omSmry
#' @param x \code{FLStock}  blah,blah,blah,...
#' @param y \code{FLBRP} or \code{FLPar} with reference points, optional 
#' @param z \code{FLPar} with `a` and `b` of length weight relationship \code{FLPar(a=0.0003,b=3)} by default
#' @param ... any additional arguments
#' 
#' @docType methods
#' @rdname omSmry
#'
#' @aliases omSmry 
#'          omSmry-method
#'          omSmry,FLStock-method 
#'          omSmry,FLStock,missing-method 
#'          omSmry,FLStock,FLBRP-method
#'          omSmry,FLStock,FLBRP,missing-method
#'          omSmry,FLStock,FLBRP,FLPar-method
#'          omSmry,FLStock,missing,missing-method
#'          
#' 
#' @seealso smryStat
#' 
#' @export omSmry
#' 
#' @examples
#' \dontrun{
#' res=omSmry(om,eq)
#' }
setGeneric('omSmry', function(x,y,z,...) standardGeneric('omSmry'))

setMethod('omSmry', signature(x="FLStock",y="missing","z"="missing"),
          function(x,y,z,...){
            
            omSmryFn(x,y=NULL,z=NULL)})

setMethod('omSmry', signature(x="FLStock",y="FLBRP","z"="missing"),
          function(x,y,z,...)
            
            omSmryFn(x,y,z=NULL))

setMethod('omSmry', signature(x="FLStock",y="FLBRP","z"="FLPar"),
          function(x,y,z,...){

            omSmryFn(x,y,z)})

setMethod('omSmry', signature(x="FLStock",y="FLPar","z"="missing"),
          function(x,y,z,...)
            
            omSmryFn(x,y="missing",z=y))

omSmryFn<-function(x,y="missing",z="missing"){
  
  nms=c("iter","year","ssb","stock","rec","ebiomass","catch","catchjuv","fbar",
        "crash_harvest","virgin_rec","virgin_ssb",
        "msy_harvest","msy_ssb","msy_biomass","msy_yield","rec_hat",
        "swt","cwt","sage","cage","sln","cln") 
  
  res=omStock(x)
  
  if ("FLBRP" %in% is(y)){
    if (dims(y)$iter==1)
      y=propagate(y,dim(x)[6])
    
    res=merge(res,omRefs(refpts(y)))

    rec=as.data.frame((params(y)["a"]%*%ssb(x))%/%(params(y)["b"]%+%ssb(x)))

    names(rec)[(seq(dim(rec)[2]))[names(rec)=="data"]]="rec_hat"
    res=merge(res,rec)
    
    abi=as.data.frame(ABI(x,y))
    names(abi)[(seq(dim(abi)[2]))[names(abi)=="data"]]="abi"
    res=merge(res,abi)
    
    #print(ABI(x,y))
    }
  else if ("FLPar" %in% is(y)){

    if (dims(y)$iter==1)
      y=propagate(y,dim(x)[6])
    
    rs2=omRefs(y)
    res=merge(res,rs2,by="iter")
    }
  
  if ("FLPar" %in% is(z))
    if (all(c("a","b") %in% dimnames(z)$params)){
      z=propagate(z,dims(x)$iter)

      res=merge(res,lenFn(x,z))}

  res=res[do.call(order,res[,names(res)[names(res)%in%c("year","season","unit","area","iter")]]),]
  
  return(res)}

sage<-function(object) apply(stock.n(object)%*%ages(stock.n(object)),2:6,sum)%/%
  apply(stock.n(object),2:6,sum)
cage<-function(object) apply(catch.n(object)%*%ages(catch.n(object)),2:6,sum)%/%
  apply(catch.n(object),2:6,sum) 
swt<-function(object) apply(stock.n(object)%*%stock.wt(object),2:6,sum)%/%
  apply(stock.n(object),2:6,sum)
cwt<-function(object) apply(catch.n(object)%*%catch.wt(object),2:6,sum)%/%
  apply(catch.n(object),2:6,sum) 
hvt   <-function(object) catch(object)/stock(object)
ebiomass<-function(object){
  sel=harvest(object)
  wt =catch.wt(object)%*%sel%/%fapex(sel)
  eb.wt =qmax(wt,0.000001)
  apply(eb.wt%*%stock.n(object),2:6,sum)}
recs<-function(object) {res=rec(object)
  dimnames(res)[[1]]="all"
  res}
catchJuv<-function(object) 
  apply(catch.n(object)%*%(1-mat(object))%*%catch.wt(object),2:6,sum)

omStock<-function(object){
  
  res=FLQuants(object,
               "ssb"=FLCore::ssb,
               "stock"=FLCore::stock,
               "ebiomass"=ebiomass,
               "rec"=recs,
               "catch"=FLCore::catch,
               "catchjuv"=catchJuv,
               "fbar"=FLCore::fbar,
               "hvt" =hvt,
               "swt"=swt,
               "cwt"=cwt,
               "sage"=sage,
               "cage"=cage)
  
  model.frame(mcf(res),drop=TRUE)}

omRefs<-function(object){
  
  refs=rbind(as.data.frame(object["crash",c("harvest")]),
             as.data.frame(object["virgin",c("rec","ssb")]),
             as.data.frame(object["msy",c("yield","ssb","biomass","harvest")]))
  refs=cast(refs,iter~refpt+quant,value="data")
  
  refs}

lenFn<-function(x,y){
  sln<-function(object) apply(stock.n(object)%*%exp(log(stock.wt(object)%/%y["a"])%/%y["b"]),2:6,sum)%/%
    apply(stock.n(object),2:6,sum)
  cln<-function(object) apply(catch.n(object)%*%exp(log(catch.wt(object)%/%y["a"])%/%y["b"]),2:6,sum)%/%
    apply(catch.n(object),2:6,sum) 
  
  model.frame(FLQuants(x,"sln"=sln,"cln"=cln),drop=TRUE)}


# if (FALSE){
# 
# library(FLCore)
# library(FLBRP)
#   
# data(ple4)
# data(ple4brp)
# setGeneric('ssbMetric', function(object,y,...) standardGeneric('ssbMetric'))
# setMethod('ssbMetric', signature(object="FLStock",y="missing"),
#           function(object,y,refpt="msy",quant="ssb",...){
#             ssb(object)})
# setMethod('ssbMetric', signature(object="FLStock",y="FLPar"),
#           function(object,y,refpt="msy",quant="ssb",...){
#             ssb(object)%/%y[refpt,quant]})
# setMethod('ssbMetric', signature(object="FLStock",y="FLBRP"),
#           function(object,y,refpt="msy",quant="ssb",...){
#             ssb(object)%/%properties(y)[refpt,quant]})
# setMethod('ssbMetric', signature(object="FLStock",y="FLQuant"),
#           function(object,y,...){
#             ssb(object)%/%y})
# 
# ssbMetric(ple4)
# ssbMetric(ple4,properties(ple4brp))
# 
# ssbMetric(ple4,ple4brp)
# ssbMetric(ple4,ple4brp,"lower pgy")
# 
# ssbMetric(ple4,ple4brp,"virgin")
# 
# ssbMetric(ple4,ple4brp,"virgin")
# 
#     
#   #ssb , stock, ebiomass, rec, juv,  catch, catchjuv, landings, landingsjuv, discards, discardsjuv, fbar , hvt , swt , cwt, sage, cage 
#   load("~/Desktop/rfmo/ices/wkref/data/ICESStocks_2021_eqsim_corrected_fishlife.Rdata")
#   
#   ple4=ICESStocks[["ple.27.420"]]
#   
#   head(omSmry(ple4))
#   head(omSmry(ple4,eq))
#   head(omSmry(ple4,FLPar(attributes(ICESStocks[[1]])$fishlife)))
#   head(omSmry(ple4,eq,FLPar(attributes(ICESStocks[[1]])$fishlife)))
# }
