## Set M, wt, sel & mat to vary by iter based on annual values to look at non-stationarity
#' @title 
#' 
#' @description 
#'
#' @param object an \code{FLStock} object 
#' @param seasons a numeric with seasons
#' 
#' @aliases
#' 
#' @return \code{FLStock} object
#'
#' @seealso \code{\link{expand}}
#'
#' @export seasonalise
#' @docType methods
#' @rdname seasonalise
#'
#' 
#' @examples
#' \dontrun{
#' load(neamac)
#' 
#' spr0=spr0Yr(neamac)
#' 
#' sr=as.FLSR(neamac,model="bevholtSV")
#' sr=ftmb(sr,s.est=T,s=0.7,s.logitsd=0.3,spr0)
#' 
#' refs=nonStationarity(neamac,sr)
#' 
#' refs=subset(refs,data>0&!is.na(data))
#' dat=subset(refs,(refpt=="spr.100"&quant%in%c("ssb"))|
#'                 (refpt=="msy"&quant%in%c("ssb","harvest","rec","yield"))|
#'                 (refpt=="virgin"&quant%in%c("ssb")))
#' 
#' ggplot(dat)+
#'   geom_line(aes(year,data,col=refpt))+facet_grid(quant~.,scale="free")

nonStationarity<-function(object,sr,slots=c("m","mat","stock.wt","catch.wt","catch.sel")){
  
  eq=FLBRP(object)
  
  eq=propagate(eq,dim(object)[2])
  
  year2iter<-function(x){
    tmp=as.data.frame(x,drop=T)
    names(tmp)[names(tmp)=="year"]="iter"
    as.FLQuant(tmp)}
  
  if ("m"%in%slots)     m(eq)=year2iter(m(object))
  if ("mat"%in%slots) mat(eq)=year2iter(mat(object))
  
  if ("stock.wt"%in%slots) stock.wt(   eq)=year2iter(stock.wt(object))
  if ("catch.wt"%in%slots|"landinhgs.wt"%in%slots) landings.wt(eq)=year2iter(landings.wt(object))
  if ("catch.wt"%in%slots|"discards.wt" %in%slots) discards.wt(eq)=year2iter(discards.wt(object))
  
  if ("catch.sel"%in%slots|"landings.sel"%in%slots) {
    sel=catch.sel(object)%*%landings.n(object)%/%catch.n(object)
    sel[is.na(sel)]=0
    sel[!is.finite(sel)]=0
    landings.sel(eq)=year2iter(sel)}
  
  if ("catch.sel"%in%slots|"discards.sel"%in%slots)  {
    sel=catch.sel(object)%*%discards.n(object)%/%catch.n(object)
    sel[is.na(sel)]=0
    sel[!is.finite(sel)]=0
    discards.sel(eq)=year2iter(sel)}
  
  nms=dimnames(refpts(eq))
  nms[[1]]=c(nms[[1]],"spr.100")
  refpts(eq)=FLPar(array(NA,lapply(nms,length),nms))
  
  model(eq) =model(sr)
  params(eq)=params(sr)
 
  rtn=rbind(computeRefpts(eq),
            properties(eq))
  
  rtn=rtn[!duplicated(dimnames(rtn)[[1]])]
  rtn=rtn[,apply(rtn,2,function(x) all(is.na(x)))==0]
  
  names(dimnames(rtn))=names(refpts(eq))

  transform(as.data.frame(rtn),year=as.numeric(dimnames(object)$year[iter]))[,-3]}

processError<-function(object,sr,slots=c("m","mat","stock.wt","catch.wt","catch.sel")){
  
  eq=FLBRP(object)
  eq=propagate(eq,dim(object)[2])
  
  year2iter<-function(x){
    tmp=as.data.frame(x,drop=T)
    names(tmp)[names(tmp)=="year"]="iter"
    as.FLQuant(tmp)}
  
  if ("m"%in%slots)     m(eq)=year2iter(m(object))
  if ("mat"%in%slots) mat(eq)=year2iter(mat(object))
  
  if ("stock.wt"%in%slots) stock.wt(   eq)=year2iter(stock.wt(object))
  if ("catch.wt"%in%slots|"landinhgs.wt"%in%slots) landings.wt(eq)=year2iter(landings.wt(object))
  if ("catch.wt"%in%slots|"discards.wt" %in%slots) discards.wt(eq)=year2iter(discards.wt(object))
  
  if ("catch.sel"%in%slots|"landinhgs.sel"%in%slots) landings.sel(eq)=year2iter(catch.sel(object)%*%landings.n(object)%/%catch.n(object))
  if ("catch.sel"%in%slots|"discards.sel"%in%slots)  discards.sel(eq)=year2iter(catch.sel(object)%*%discards.n(object)%/%catch.n(object))
  
  nms=dimnames(refpts(eq))
  nms[[1]]=c(nms[[1]],"spr.100")
  refpts(eq)=FLPar(array(NA,lapply(nms,length),nms))
  
  model(eq) =model(sr)
  params(eq)=params(sr)
                  
  dmns=list(refpt=c("production"),
             quant=c("harvest","yield","rec","ssb","biomass","revenue","cost","profit"),
             iter =dimnames(eq)$iter)
  prd=FLPar(array(NA,dim=laply(dmns,length),dimnames=dmns))
  prd[,"ssb"]=c(ssb(object))
  refpts(eq)=prd
  prd=computeRefpts(eq)
  
  production=as.FLQuant(c(prd[,"yield"]),dimnames=dimnames(ssb(object)))
  ssb.t=window(ssb(object),start=dims(ssb(object))$minyear+1,end=dims(ssb(object))$maxyear+1)

  FLQuants(ssb=ssb(object),
           catch=catch(object),
           production=production,
           error=-ssb(object)+ssb.t-catch(object)+production)
  }