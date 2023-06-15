#' If life history parameters for growth, natural mortality and maturity and the 
#' selection pattern is known then a yield- or spawning stock biomass (SSB)-per-recruit 
#' analysis can be conducted. The per-recruit analysis can be combined with a 
#' stock recruitment relationship (Sissenwine and Shepherd, 1987) to calculate 
#' the equilibrium stock size as a function of fishing mortality (F), and to derive 
#' target (i.e. MSY) and limit reference points. Such analyses rely on the existence
#' of multiple year classes and fixed values-at-age which allow the decline in
#' the abundance of individual age-classes to be tracked. For short-lived species 
#' which are present as a single cohort at any one time, such models can relate
#' catch of a cohort over time to population size if values for fishing and natural 
#' mortality rates are known (Arkhipkin et al., 2021). Expressing F as a function 
#' of effort and catchability (Rosenberg et al., 1990) allows cohort models to
#' estimate fishing mortality by fitting to trends in catch and effort 
#' (e.g. Hendrickson and Hart, 2006, Royer et al., 2001).
#'   
#'  Arkhipkin, A.I., Hendrickson, L.C., Payá, I., Pierce, G.J., Roa-Ureta, R.H.,
#'  Robin, J.P. and Winter, A., 2021. Stock assessment and management of cephalopods: 
#'  advances and challenges for short-lived fishery resources.
#'  ICES Journal of Marine Science, 78(2), pp.714-730.
#'  
#'  Hendrickson, L. C., and Hart, D. R. 2006. An age-based cohort model for estimating
#'  the spawning mortality of semelparous cephalopods with an application to per-recruit 
#'  calculations for the northern shortfin squid, Illex illecebrosus. Fisheries Research, 78: 4–13.
#'   
#' Rosenberg, A. A., Kirkwood, G. P., Crombie, J. A., and Beddington, J. R. 1990. 
#' The assessment of stocks of annual squid species. Fisheries Research, 8: 335–350.
#' 
#' Royer, J., Périès, P., and Robin, J. P. 2002. Stock assessments of English Channel 
#' loliginid squids: updated depletion methods and new analytical methods. 
#' ICES Journal of Marine Science, 59:445–457.


################################################################################
## Calculate refeence points with seasonality ##################################
################################################################################
library(FLCore)
library(FLBRP)
library(ggplotFL)

load("~/Dropbox/NEA.mac.MSE.Base_case.AL/test/sea4_stoch_rec.RData")

## Get rid of iters & projections
object=window(iter(sea4_stoch_rec,1),end=2015)

## make years+seasons into psuedo+years  #######################################
## i.e. "annualize" ############################################################
stk=qapply(object, function(x) {
  
  if (dim(x)[4]==1) return(x)
  if (dim(x)[1]==1) {
    x=x[,,,1]
    return(x)}
  
  dnms=dimnames(x)
  
  dnms[[4]]=1
  dnms[[1]]=(as.numeric(rep(dnms[[1]],each=dim(x)[4]))+seq(0,1,length.out=dim(x)[4]+1)[-(dim(x)[4]+1)])*dim(x)[4]
  
  FLQuant(c(aperm(x,c(4,1,2,3,5:6))),dimnames=dnms,units=units(x))})

## tidy up
range(stk)["min"]=min(as.numeric(dimnames(m(stk))[[1]]))
range(stk)["max"]=max(as.numeric(dimnames(m(stk))[[1]]))
if (!is.na(range(stk)["plusgroup"]))
   range(stk)["plusgroup"]=range(stk)["max"]
range(stk)[c("minfbar","maxfbar")]=range(stk)[c("minfbar","maxfbar")]*dim(m(object))[4]

## Calculate Reference points ##################################################
eq=FLBRP(stk)
## set fbar max to 5
fbar(eq)=fbar(eq)/max(fbar(eq))*5

## vectors-at-age
ggplot(FLQuants(eq,"Mass"=stock.wt,"M"=m,"Mat"=mat,"Selection"=catch.sel))+
  geom_line(aes(age,data))+
  facet_wrap(~qname,scale="free")

plot(eq,refpts="msy")

## Fish in season 4 only #######################################################
eq=eqeq=propagate(eq,2)
landings.sel(eq)[-seq(4,64,4),,,,,2]=0

## vectors-at-age
ggplot(FLQuants(eq,"Mass"=stock.wt,"M"=m,"Mat"=mat,"Selection"=catch.sel))+
  geom_line(aes(age,data,col=iter))+
  facet_wrap(~qname,scale="free")

## MSY is the old one
eq=brp(eq)
plot(eq,refpts="msy")

