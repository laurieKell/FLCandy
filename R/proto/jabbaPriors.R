source("~/Desktop/flr/mydas/R/mseMPJabba.R", echo=TRUE)

jabbaPriors<-function(object,mi=seq(0.01,2,0.01)){
  
  fmsy =object["msy","harvest"]
  bmsy =object["msy","ssb"]
  b0   =object["virgin","ssb"]
  shape=bmsy%/%b0
  
  m    =as(mdply(data.frame(x=an(round(shape,2))), function(x) data.frame(m=median(mi[round(mi^(-1/(mi-1)),2)==round(x,2)]))),"FLPar")[-1]

  hmsy=m
  hmsy[]=1-exp(-fmsy)
  r    =(hmsy%*%(m-1))%/%(1-(1/m))
  dimnames(r)$params="r"

  rbind(r,m)}

prs=jabbaPriors(refpts(eqOM))

load("~/Desktop/tmp/t.RData")

# JABBA data.frames
cpue = vb(om)
cpue[cpue==0]=NA
cpue =cpue%/%iterMeans(cpue)
cpue =model.frame(FLQuants(index=vb(om)),   drop=T)

catch=model.frame(FLQuants(catch=catch(om)),drop=T)

z      =catch(om)%/%vb(om)
z[z==0]=NA
z    =model.frame(FLQuants(z=z),drop=T)

catch=subset(catch,iter==1&year%in%50:100)[,-2]
cpue =subset(cpue, iter==1&year%in%50:100)[,-2]
z    =subset(z,    iter==1&year%in%50:100)[,-2]

jbI= build_jabba(catch=catch,
                 cpue =cpue,
                 model.type = "Fox",scenario="DR-Index",
                 r.prior = c(prs[,1],0.3),
                 verbose=F,
                 psi.prior=c(0.9,0.3))
fI= fit_jabba(jbI,quickmcmc = T,verbose=F)

jbIZ= build_jabba(catch    =catch,
                  cpue     =cpue,
                  auxiliary=z,
                  model.type = "Fox",scenario="DR-Index+Z",
                  r.prior = c(prs[,1],0.3),
                  verbose=F,
                  auxiliary.sigma = TRUE, # Here estimated
                  auxiliary.obsE =0.1, # lag effect between impact and Z pop structure
                  auxiliary.lag = 5, # lag effect between impact and Z pop structure
                  auxiliary.type = "z",
                  psi.prior=c(0.9,0.3))

fIZ= fit_jabba(jbIZ,quickmcmc = T,verbose=F)

plot(om[,ac(50:100)])
plot(FLQuants(dlply(fIZ[[23]],.(qname), with, as.FLQuant(data.frame(year=year,data=data)))))

FLI=jabba2biodyn(fI)
FLZ=jabba2biodyn(fIZ)

plot(mpb:::biodyns(list("I"=FLI,"Z"=FLZ)))

