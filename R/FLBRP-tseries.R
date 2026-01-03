#' @title tseries
#' 
#' @description Calculates the surplus production and expected yield etc for the estinates of SSB and biomass
#'
#' @param object an \code{FLBRP} object 
#' @param seasons a numeric with seasons
#' 
#' @aliases
#' 
#' @return \code{FLQuants} object
#'
#' @seealso \code{\link{expand}}
#'
#' @export tseries
#' @docType methods
#' @rdname tseries
#'
#' 
#' @examples
#' \dontrun{
#' }

setMethod("tseries", signature(object="FLBRP"), function(object){
  ebiomass.obs<-function(x) attributes(x)$eb.obs

  nms=dimnames(refpts(object))
  nms$refpt=paste("ssb",dimnames(ssb.obs(object))$year,sep="")
  
  vrgn=refpts(object)["virgin","ssb"]
  
  discards.obs(object)[is.na(discards.obs(object))]=0
  
  landings.sel(object)=landings.sel(object)+discards.sel(object)
  discards.sel(object)[]=0
  
  rfs=FLPar(array(NA,laply(nms,length),dimnames=nms))
  rfs[,"ssb",]=ssb.obs(object)
  refpts(object)=rfs
  rtn=refptsEB(object)
  fbar(object)=FLQuant(c(rtn[,"harvest"]))
  
  rtn=alply(rtn,2,FLQuant,dimnames=dimnames(ssb.obs(object)))
  names(rtn)=as.character(unlist(attributes(rtn)$split_labels))
  
  rtn$eb =ebiomass.obs(object)
  
  rtn$spSSB=ssb.obs(object)[,-1]-ssb.obs(object)[,-dim(ssb.obs(object))[2]]+catch.obs(object)[,-dim(ssb.obs(object))[2]]
  rtn$spEB =ebiomass.obs(object)[,-1]-ebiomass.obs(object)[,-dim(ebiomass.obs(object))[2]]+catch.obs(object)[,-dim(ebiomass.obs(object))[2]]
  
  rtn=mcf(as(rtn,"FLQuants"))
  
  ord=c("harvest","yield","rec","ssb","eb","revenue","cost","profit","spSSB","spEB")
  
  rtn=rtn[ord]
  rtn[["peSSB"]]=rtn$spSSB-rtn$yield
  rtn[["peSSB"]]=rtn[["peSSB"]]%/%rtn[["ssb"]]
  rtn[["peEB"]] =rtn$spEB -rtn$yield
  rtn[["peEB"]]=rtn[["peEB"]]%/%rtn[["eb"]]
  
  chk=ssb.obs(object)
  chk=chk%=%vrgn
  chk=chk<=ssb.obs(object)
  
  rtn[["ssb"]]=ssb.obs(object)
  
  if(any(chk)){
    for (i in names(rtn))
      rtn[[i]][chk]=NA}
  
  rtn})

setMethod("tseries", signature(object="FLBRPs"), function(object){
      ldply(object, function(x) model.frame(FLCandy::tseries(x)))})
   
setGeneric("prodPts", function(object, ...) standardGeneric("prodPts"))
setMethod( "prodPts", signature(object="FLBRP"),  function(object){ tseries(object)})
setMethod( "prodPts", signature(object="FLBRPs"), function(object){
  ldply(object, function(x) model.frame(prodPts(x)))})

setGeneric("prodFn", function(object, ...) standardGeneric("prodFn"))
setMethod( "prodFn", signature(object="FLBRPs"), function(object){
  ldply(object, function(x) model.frame(prodFn(x)))})

setMethod("prodFn",signature(object="FLBRP"),function(object) {
  
  # Extract data
  pf=model.frame(FLQuants(object,
                          "f"    =fbar, 
                          "yield"=function(x) catch(x)))
  maxF=subset(pf,f>0&yield==0)[1,"f"]
  if (!is.na((maxF)))
    fbar(object)=FLQuant(seq(0,maxF,length.out=101))

  pf=model.frame(FLQuants(object,
                          "harvest" =fbar, 
                          "yield"   =function(x) catch(x),
                          "rec"     =function(x) rec(x),
                          "ssb"     =function(x) ssb(x),
                          "biomass" =function(x) stock(x),
                          "eb"      =function(x) ebiomass(x)))
  
  return(pf)})

setGeneric("MLP", function(object,...) standardGeneric("MLP"))
setMethod("MLP", signature(object="FLBRPs"), function(object,rm.neg=TRUE){
  ldply(object, function(x) model.frame(MLP(x,rm.neg)))})

setMethod("MLP",signature(object="FLBRP"), 
    function(object,rm.neg=TRUE) {
            
        pt=model.frame(prodPts(object))
        pf=model.frame(prodFn( object))
            
        msy=pf[which.min(abs(pf$yield-max(pf$yield))), ]
        pf2=subset(pf,ssb<=msy$ssb)
            
        if (rm.neg) pt=subset(pt,yield>0)
        yield=median(pt$yield,na.rm=TRUE)
        
        # Use approx for linear interpolation
        hat=approx(x=pf2$yield,y=pf2$ssb,xout=yield)$y
            
        rtn=data.frame(ssb=hat,yield=yield)
        return(rtn)})

if (FALSE){
  library(FLBRP)
  library(FLCandy)
  library(mgcv)
  library(nnet)
  
  load("C:/flrpapers/haf/jabba-f/data/results/bhs.RData")
  load("C:/flrpapers/haf/jabba-f/data/results/rks.RData")
  
  for (i in names(bhs)){
    landings.sel(bhs[[i]])=landings.sel(bhs[[i]])+discards.sel(bhs[[i]])
    discards.sel(bhs[[i]])[]=0
    landings.sel(rks[[i]])=landings.sel(rks[[i]])+discards.sel(rks[[i]])
    discards.sel(rks[[i]])[]=0}
  
  i="anf.27.3a46"
  pt =prodPts(bhs[i])
  pf =prodFn(bhs[[i]])
  msy=computeRefpts(bhs[[i]])["msy",drop=TRUE]
  mlp=MLP(bhs[[i]],FALSE)
  
  ggplot(pf)+ 
    geom_line( aes(ssb,yield),linewidth=0.5)+
    geom_point(aes(ssb,yield),size=2.0,col="grey50",shape=21,     data=pt)+
    #geom_point(aes(msy$ssb,msy$yield),size=2.0,col="grey25",shape=21)+
    geom_point(aes(ssb,yield),size=3.0,col="grey25",              data=mlp)+
    geom_hline(aes(yintercept=yield),        linetype ="dashed",  data=mlp)+
    geom_vline(aes(xintercept=ssb),linetype ="dashed",            data=mlp)+
    theme_minimal(12)+
    theme(legend.position="top", 
          strip.text =element_blank(),
          #axis.text.x  =element_blank(),
          #axis.ticks.x =element_blank(),
          axis.text.y  =element_blank(),
          axis.ticks.y =element_blank())+  
    labs(x    ="SSB",
         y    ="Production")
  
  bhPt =prodPts(bhs)
  bhPf =prodFn( bhs)
  bhMsy=ldply(  bhs, function(x) computeRefpts(x)["msy",drop=TRUE])
  bhMlp=MLP(    bhs)
  
  rkPt =prodPts(rks)
  rkPf =prodFn( rks)
  rkMsy=ldply(  rks, function(x) computeRefpts(x)["msy",drop=TRUE])
  rkMlp=MLP(    rks)
  
  pt =rbind.fill(cbind(SRR="B & Holt",bhPt), cbind(SRR="Ricker",rkPt))
  pf =rbind.fill(cbind(SRR="B & Holt",bhPf), cbind(SRR="Ricker",rkPf))
  msy=rbind.fill(cbind(SRR="B & Holt",bhMsy),cbind(SRR="Ricker",rkMsy))
  mlp=rbind.fill(cbind(SRR="B & Holt",bhMlp),cbind(SRR="Ricker",rkMlp))
  
  ggplot(pf)+ 
    facet_wrap(~.id,scale="free",ncol=5)+
    geom_line( aes(ssb,yield,       col=SRR),linewidth=0.5)+
    geom_point(aes(ssb,yield,      fill=SRR),size=1.0,col="grey50",shape=21,data=pt)+
    #geom_point(aes(ssb,yield,      fill=SRR),size=2.0,col="grey25",shape=21,data=msy)+
    geom_point(aes(ssb,yield,      fill=SRR),size=2.0,col="grey25",shape=21,data=mlp)+
    geom_hline(aes(yintercept=yield),        linetype ="dashed",            data=mlp)+
    geom_vline(aes(xintercept=ssb,  col=SRR),linetype ="dashed",            data=mlp)+
    scale_color_manual(values=c("#E69F00","#56B4E9"))+ 
    geom_text(aes(label=.id),
              x    =Inf, y    =Inf,
              hjust=1,   vjust=1,
              size =3)+   
    theme_minimal(12)+
    theme(legend.position="top", 
          strip.text   =element_blank(),
          axis.text.x  =element_blank(),
          axis.ticks.x =element_blank(),
          axis.text.y  =element_blank(),
          axis.ticks.y =element_blank())+  
    labs(x    ="SSB",
         y    ="Production")
  
  chk=transform(merge(mlp,msy,by=c(".id","SRR")),ratio=ssb.x/ssb.y)
        
  ggplot(chk)+
    geom_histogram(aes(ratio))+
    facet_grid(SRR~.)+
    xlab(expression(MLP/B[MSY]))
  
  mlp =rbind.fill(cbind(SRR="B & Holt",MLP(bhs,rm.neg=FALSE)),
                  cbind(SRR="Ricker",  MLP(rks,rm.neg=FALSE)))
  chk=transform(merge(mlp,msy,by=c(".id","SRR")),ratio=ssb.x/ssb.y)
  
  ggplot(chk)+
    geom_histogram(aes(ratio))+
    facet_grid(SRR~.)+
    xlab(expression(MLP/B[MSY]))
  
  }
