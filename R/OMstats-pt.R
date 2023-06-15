#' pt
#'
#' pt function
#' 
#' @param object \code{FLPar} or \code{numeric} with values for $b_{msy}$, $f_{msy}$ as a minimum; $msy$ and $v$ (virgin biomass) can also be provided. 
#' @param shape \code{FLPar} or \code{numeric} the ratio of $b_{msy}$ to virgin biomass, by default is estimated from $b_{msy}$ and $v$ in \code{object} 
#' @param ... other arguments
#' 
#' @export
#' @docType methods
#' @rdname pt
#' 
#' @aliases pt pt-method pt,FLPar-method
#' 
#' @seealso \code{\link{gompertz}}
#' 
#' @examples
#' \dontrun{
#' params=FLPar(a50=4,ato95=1,asym=1.0)
#' age=FLQuant(1:10,dimnames=list(age=1:10))
#' pt(age,params)
#' }
setGeneric("pt", function(object,...)  standardGeneric("pt"))

#### Exploitable biomass #######################################################
ebiomass<-function(object){
  sel=harvest(object)
  wt =catch.wt(object)%*%sel%/%fapex(sel)
  eb.wt =qmax(wt,0.000001)
  apply(eb.wt%*%stock.n(object),2:6,sum)}

#### Production functions ######################################################
prdFn<-function (params, biomass=0, mdl="pellat") {
  if (!is.FLQuant(biomass)) 
    biomass = FLQuant(biomass)
  if (dims(params)$iter == 1 & dims(biomass)$iter > 1) 
    params = propagate(params, dims(biomass)$iter)
  if (dims(params)$iter > 1 & dims(biomass)$iter == 1) 
    biomass = propagate(biomass, dims(params)$iter)
  mdl = tolower(as.character(mdl))
  fox <- function(biomass, params) {
    res = exp(1) * (params["msy"]%/%params["k"]) %*% biomass %*% 
      log(params["k"]) %*% (1 - log(biomass)%/%log(params["k"]))
    res
  }
  schaefer <- function(biomass, params) params["r"] * biomass * 
    (1 - biomass/params["k"])
  pellat <- function(biomass, params) {
    a = sweep(biomass, 6, params["r"]/params["p"], "*")
    b = sweep(biomass, 6, params["k"], "/")
    c = sweep(b, 6, params["p"], "^")
    a * (1 - c)
  }
  shepherd <- function(biomass, params) params["r"] * biomass/(1 + 
                                                                 biomass/params["k"]) - params["m"] * biomass
  gulland <- function(biomass, params) params["r"] * biomass * 
    (params["k"] - biomass)
  fletcher <- function(biomass, params) {
    params["p"] = params["p"] + 1
    lambda <- (params["p"]^(params["p"]/(params["p"] - 1)))/(params["p"] - 
                                                               1)
    lambda * params["msy"] * (biomass/params["k"]) - lambda * 
      params["msy"] * (biomass/params["k"])^params["p"]
  }
  logistic <- function(biomass, params) {
    r = 4 * params["msy"]/params["k"]
    r * biomass %*% (1 - biomass%/%params["k"])
  }
  genfit <- function(biomass, params) params["r"] * biomass * 
    (1 - biomass/params["k"])
  res <- switch(substr(mdl, 1, 2), fo = fox(biomass, params), 
                sc = schaefer(biomass, params), gu = gulland(biomass, 
                                                             params), fl = fletcher(biomass, params), pe = pellat(biomass, 
                                                                                                                  params = params), sh = shepherd(biomass, params), 
                ge = pellat(biomass, params), lo = logistic(biomass, 
                                                            params))
  return(res)}

#### Estimate p from shape #####################################################
p<-function(shape){
  
  calcP<-function(shape){
    
    fn<-function(x,y)
      (y-(1/(1+x))^(1/x))^2
    
    if (shape<0.3678794)
      optimise(fn,c(-0.9999,-1e-20),y=shape)$minimum
    else
      optimise(fn,c(1e-20,10),y=shape)$minimum}
  
  res=aaply(shape,seq(length(dim(shape)))[-1], calcP)
  
  dmns=dimnames(shape)
  dmns[[1]]="p"
  
  FLPar(array(res,dim=unlist(laply(dmns,length)),dimnames=dmns))}

#### PT Method #################################################################
setMethod('pt', signature(object="FLPar"), 
          function(object,shape=object["bmsy"]%/%object["v"]) {
          
            chk=c("bmsy","fmsy","msy")[c("bmsy","fmsy","msy")%in%dimnames(object)[[1]]]
            
            object=rbind(object,FLPar(p=p(FLPar(shape))))
            
            ## MSY not supplied
            if (all(c("bmsy","fmsy")%in%chk)&!("msy"%in%chk))
              object=rbind(object,FLPar(msy=object["bmsy"]%*%object["fmsy"]))
            
            ## FMSY not supplied
            if (all(c("bmsy","msy")%in%chk)&!("fmsy"%in%chk))
              object=rbind(object,FLPar(fmsy=object["msy"]%*%object["bmsy"]))
            
            ## BMSY not supplied
            if (all(c("fmsy","msy")%in%chk)&!("bmsy"%in%chk))
              object=rbind(object,FLPar(bmsy=object["msy"]%/%object["fmsy"]))
            
            v=object["bmsy"]%/%((1/(object["p"]+1))^(1/object["p"]))
            object=rbind(object,FLPar(k=v))
            r=object["msy"]%/%(object["k"]%*%((1/(object["p"]+1))^(1+1/object["p"])))
            object=rbind(object,FLPar(r=r))
            
            object})

setMethod('pt', signature(object="numeric"), 
          function(object) {
            pt(FLPar(object))}) 

setMethod('pt', signature(object="FLBRP"), 
          function(object,what=ssb){
              fbar(object)=fbar(object)[,1:2]
              fbar(object)[,1]=fbar(object)[,1]%=%1e-20
              fbar(object)[,2]=fbar(object)[,2]%=%refpts(object)["msy","harvest"]
              
              msy =catch(object)[,2]
              fmsy=1-exp(-fbar(object)[,2])
              virgin=what(object[,1])
              
              bmsy =what(object)[,2]
              shape=bmsy%/%virgin
              
              p.=p(shape)
              
              K=bmsy%/%((1/(p.+1))^(1/p.))
              r=msy%/%(K%*%((1%/%(p.+1))^(1+1/p.)))
              
              rbind(FLPar(r=r[ drop=T]),
                    FLPar(k=K[ drop=T]),
                    FLPar(p=p.[drop=T]),
                    FLPar(msy =msy[ drop=T]),
                    FLPar(bmsy=bmsy[drop=T]),
                    FLPar(fmsy=fmsy[drop=T]),
                    FLPar(virgin=virgin[drop=T]))
          }) 


if(FALSE){
################################################################################
#### OM descriptive statistics & SPM priors ####################################
################################################################################
#### Estimate Pella-T parameters fro FLBRP production function              ####
####      based on MSY ref pts                                              ####
################################################################################
library(FLCore)
library(FLBRP)
library(FLife)

library(ggplotFL)
library(ggpubr)

library(plyr)

eq=lhEql(lhPar(FLPar(linf=100)))

object=FLPar(c(bmsy=500, msy=125, v=1000))
pt(object)

object=FLPar(c(bmsy=500,fmsy=0.25,v=1000))
pt(object)

object=FLPar(c(msy=125,fmsy=0.25))
pt(object,shape=0.5)

#### SSB #######################################################################
#### FMSY same as FLStock
parSSB=pt(eq)

fbar(eq)=FLQuant(seq(0,1,length.out=101))*refpts(eq)["crash","harvest"]
fbar(eq)[,1]=fbar(eq)[,2]*1e-10
hat=prdFn(parSSB, ssb(eq),"pellat")

p1=ggplot()+
  geom_line( aes(x,y),data=model.frame(FLQuants(x=ssb(eq),y=hat)))+
  geom_point(aes(x,y),data=model.frame(FLQuants(x=ssb(eq),y=catch(eq))),col="red")+
  xlab("SSB")+ylab("Yield")

#### MSY same as FLStock
parSSB=pt(eq)
parSSB["k"]=parSSB["k"]%*%(parSSB["msy"]%/%prdFn(parSSB,parSSB["bmsy"],"pellat"))
fbar(eq)=FLQuant(seq(0,1,length.out=101))*refpts(eq)["crash","harvest"]
fbar(eq)[,1]=fbar(eq)[,2]*1e-10
hat=prdFn(parSSB, ssb(eq),"pellat")

p2=ggplot()+
  geom_line( aes(x,y),data=model.frame(FLQuants(x=ssb(eq),y=hat)))+
  geom_point(aes(x,y),data=model.frame(FLQuants(x=ssb(eq),y=catch(eq))),col="red")+
  xlab("SSB")+ylab("Yield")

#### Exploitable biomass #######################################################
#### FMSY same as FLStock
parEB= pt(eq,what=ebiomass)

fbar(eq)=FLQuant(seq(0,1,length.out=101))*refpts(eq)["crash","harvest"]
fbar(eq)[,1]=fbar(eq)[,2]*1e-10
hat=prdFn(parEB, ebiomass(eq),"pellat")

p3=ggplot()+
  geom_line( aes(x,y),data=model.frame(FLQuants(x=ebiomass(eq),y=hat)))+
  geom_point(aes(x,y),data=model.frame(FLQuants(x=ebiomass(eq),y=catch(eq))),col="red")+
  xlab("Exploitable Biomass")+ylab("Yield")

#### MSY same as FLStock
parEB=pt(eq,what=ebiomass)
parEB["k"]=parEB["k"]%*%(parEB["msy"]%/%prdFn(parEB,parEB["bmsy"],"pellat"))
fbar(eq)=FLQuant(seq(0,1,length.out=101))*refpts(eq)["crash","harvest"]
fbar(eq)[,1]=fbar(eq)[,2]*1e-10
hat=prdFn(parEB, ebiomass(eq),"pellat")

p4=ggplot()+
  geom_line( aes(x,y),data=model.frame(FLQuants(x=ebiomass(eq),y=hat)))+
  geom_point(aes(x,y),data=model.frame(FLQuants(x=ebiomass(eq),y=catch(eq))),col="red")+
  xlab("Exploitable Biomass")+ylab("Yield")

ggarrange(p1,p3,p2,p4, 
          widths = c(5, 5), heights = c(5, 5),
          nrow   = 2,          ncol    = 2,
          labels = c("SSB: FMSY","EB: FMSY","SSB: MSY","EB: MSY"),
          common.legend = TRUE)+theme_bw()

#stock Catchequi     BMSY      B0 FmsyMedianC FmsyMedianL F5percRiskBlim FlimEqsim


#load("/home/laurie/Desktop/inPrep/mse/ices/data/om/ices.stks.n78.wkref1.rdata")
}
