### Aopt #####################################################
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


## Process Error #############################################
sp<-function(stk,eq,stock=FLCore:::ssb){
  
  fbar(eq)=FLQuant(seq(0,1,length.out=201))*computeRefpts(eq)["crash","harvest"]
  dat=with(model.frame(FLQuants(eq,"stock"=function(x) stock(x),"catch"=function(x) catch(x)),drop=T), 
           approx(stock,catch,xout=c(ssb(stk))),dimnames=dimnames(stock(stk)))
  FLQuant(dat$y,dimnames=dimnames(stock(stk)))}

pe<-function(stk,eq,stock=FLCore:::ssb){
    (stock(stk)%-%
     window(stock(stk)[,-1],end=dims(stk)$maxyear+1)-
     catch(stk)%+%sp(stk,eq,stock))%/%stock(stk)}


## calculates process error 
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
  
  FLQuants(ssb  =transmute(rtn,data.frame(year=year,data=ssb)),
           catch=transmute(rtn,data.frame(year=year,data=catch)),
           sp   =transmute(rtn,data.frame(year=year,data=sp)),
           pe   =transmute(rtn,data.frame(year=year,data=pe)))}


getPriors<-function(x){
  rfs       =as.data.frame(t(refpts(x)["msy",c("ssb","harvest","yield"),drop=TRUE]))
  names(rfs)=c("bmsy","fmsy","msy")
  
  priors =cbind(as.data.frame(t(tryIt(pellaTparams(x)[drop=TRUE]))),
                rfs,
                data.frame(initial.ssb=ssb.obs( x)[,1,drop=TRUE],
                           initial.f  =fbar.obs(x)[,1,drop=TRUE],
                           current.ssb=ssb.obs( x)[,dim(ssb.obs(x))[2],drop=TRUE],
                           current.f  =fbar.obs(x)[,dim(fbar.obs(x))[2],drop=TRUE]))
  priors=transform(priors,shape    =bmsy/k,
                   ssb.maxyr=current.ssb,
                   ssb.minyr=initial.ssb/bmsy*k)
  return(priors)}

eqlFn<-function(object,model="bevholtSV"){
  
  spr0=FLCandy:::spr0Yr(object)
  sr  =as.FLSR(object,model=model)
  sr  =ftmb(sr,s.est    =T,
            s        =0.7, #fishlife(object)["s"],
            s.logitsd=0.4, #fishlife(object)["sd.logit.s"],
            spr0     =spr0)
  
  rtn=brp(FLBRP(object,nyears=dim(object)[2],
                sr=list(model =do.call(gsub("SV","", model),list())$model,
                        params=FLPar(apply(params(sr),1,median)))))
  
  attributes(rtn)[["logLik"]]       =logLik(sr)
  attributes(rtn)[["rec.residuals"]]=residuals(sr)
  #attributes(rtn)[["sr"]]     =sr
  #attributes(rtn)[["prod"]]   =tryIt(spFn(      rtn))
  #attributes(rtn)[["tseries"]]=tryIt(tseries(   object))
  #attributes(rtn)[["priors"]] =tryIt(calcPriors(rtn))
  #attributes(rtn)[["prior2"]] =tryIt(getPriors( rtn))
  
  return(rtn)}


if(FALSE){
  
################################################################################
#### OM descriptive statistics & SPM priors ####################################
################################################################################
require(FLCore)
require(FLBRP)
require(mpb)
require(ggplotFL)

pars=lhPar(FLPar(linf=100))

### Entropy ##################################################
require(statcomp)

data(ple4)
data(ple4brp)

permutation_entropy(ordinal_pattern_distribution(x=ssb(ple4), ndemb=5))
permutation_entropy(ordinal_pattern_distribution(x=stock(ple4), ndemb=5))
permutation_entropy(ordinal_pattern_distribution(x=FLCore:::vb(ple4), ndemb=5))
permutation_entropy(ordinal_pattern_distribution(x=rec(ple4), ndemb=5))

permutation_entropy(ordinal_pattern_distribution(x=seq(0,10,0.1), ndemb=5))
permutation_entropy(ordinal_pattern_distribution(x=rlnorm(100), ndemb=5))

aopt(ple4brp)

### Lopt #####################################################
lopt=vonB(aopt(ple4brp),pars)
lopt%/%pars["linf"]
lopt%/%pars["l50"]

plot(pe(ple4,ple4brp,ebiomass))
plot(pe(ple4,ple4brp,ssb))


### Production function ########################################################
eq=lhEql(lhPar(FLPar(linf=100)))

plot(mpb:::prdFn("pellat",FLPar(r=0.3,k=100000,p=0.3),FLQuant(seq(0,100000,length.out=100))))

#### Pella=T priors ############################################################

ebiomass<-function(object){
  sel=harvest(object)
  wt =catch.wt(object)%*%sel%/%fapex(sel)
  eb.wt =qmax(wt,0.000001)
  apply(eb.wt%*%stock.n(object),2:6,sum)}

## estimate p from shape
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

pellaPriors<-function(eq,what=ssb){
  fbar(eq)=fbar(eq)[,1:2]
  fbar(eq)[,1]=fbar(eq)[,1]%=%1e-20
  fbar(eq)[,2]=fbar(eq)[,2]%=%refpts(eq)["msy","harvest"]
  
  msy =catch(eq)[,2]
  fmsy=1-exp(-fbar(eq)[,2])
  
  K    =what(eq)[,1]
  bmsy =what(eq)[,2]
  shape=bmsy%/%K
  
  p.   =p(shape)
  K=bmsy%/%((1/(p.+1))^(1/p.))
  r=msy%/%(K%*%((1%/%(p.+1))^(1+1/p.)))
  
  rbind(FLPar(r=r[ drop=T]),
        FLPar(K=K[ drop=T]),
        FLPar(p=p.[drop=T]),
        FLPar(msy =msy[ drop=T]),
        FLPar(bmsy=bmsy[drop=T]),
        FLPar(fmsy=fmsy[drop=T]))}

pellaPriors(eq)
pellaPriors(eq,ebiomass)

### r Prior ####################################################################

# Find m i.e. the shape parameter of pella-t
# m is the parameter of the pella-t, so look at all possible values and pick the best 
mi   =seq(0.01,2,0.001) 
m    =(mi^(-1/(mi-1))-shape)^2
m    =mi[m==min(m)]
# Then calc r, change fmsy from rate to instantaneous value
fmsy =refpts(eq)["msy","harvest"]
r    =(1-exp(-fmsy))*(m-1)/(1-m^-1)

fn<-function(x=c(r=0.5),k,p,eq){
  yield=catch(eq)
  eB   =ebiomass(eq)
  
  hat=mpb:::prdFn("pellat",FLPar(r=x[1],k=x[2],p=c(p)),eB)
  
  sum((hat-yield)^2,na.rm=T)}

rtn=optim(par=c(r=0.3,k=b0), fn, eq=eq,p=m)$par


#### Example p v shape ############################################
shape=FLQuant(seq(0.1,0.75,length.out=101))

ggplot(model.frame(FLQuants(p=as.FLQuant(p(shape)),shape=shape)))+
  geom_line(aes(p,shape))


#### Eq sample ####################################################
shape=FLPar(shape=(refpts(eq)["msy",c("ssb")]/refpts(eq)["virgin",c("ssb")])[drop=T])
pPar=p(shape)

## Fit I fit all ###############################################################
fn<-function(x,stock,yield){
  
  hat=mpb:::prdFn("pellat",FLPar(r=x[1],k=x[2],p=x[3]),stock)
  
  sum((hat-yield)^2,na.rm=T)}

par=
  FLPar(c(optim(par=c(r=0.1,k=c(refpts(eq)["virgin","ssb"]),p=pPar), fn,stock=ssb(eq),yield=catch(eq))$par))

fbar(eq)=FLQuant(seq(0,1,length.out=101))*refpts(eq)["crash","harvest"]
fbar(eq)[,1]=fbar(eq)[,2]*1e-10
hat=mpb:::prdFn("pellat",par,ssb(eq))

ggplot()+
  geom_line( aes(x,y),data=model.frame(FLQuants(x=ssb(eq),y=hat)))+
  geom_point(aes(x,y),data=model.frame(FLQuants(x=ssb(eq),y=catch(eq))),col="red")

## Fit II fix shape ############################################################
fn<-function(x,p,stock,yield){
  
  hat=mpb:::prdFn("pellat",FLPar(r=x[1],k=x[2],p=p),stock)
  
  sum((hat-yield)^2,na.rm=T)}

par=optim(par=c(r=0.1,k=c(refpts(eq)["virgin","ssb"])),fn,p=pPar,stock=ssb(eq),yield=catch(eq))$par
par=rbind(FLPar(par),pPar)

fbar(eq)=FLQuant(seq(0,1,length.out=101))*refpts(eq)["crash","harvest"]
fbar(eq)[,1]=fbar(eq)[,2]*1e-10
hat=mpb:::prdFn("pellat",par,ssb(eq))

ggplot()+
  geom_line( aes(x,y),data=model.frame(FLQuants(x=ssb(eq),y=hat)))+
  geom_point(aes(x,y),data=model.frame(FLQuants(x=ssb(eq),y=catch(eq))),col="red")

## Fit III fix k ###############################################################
fn<-function(x,k,stock,yield){
  
  hat=mpb:::prdFn("pellat",FLPar(r=x[1],p=x[2],k=k),stock)
  
  sum((hat-yield)^2,na.rm=T)}

par=optim(par=c(r=0.3,p=pPar),fn,k=c(refpts(eq)["virgin","ssb"]),stock=ssb(eq),yield=catch(eq))$par
par=rbind(FLPar(par),FLPar(k=refpts(eq)["virgin","ssb",drop=T]))

fbar(eq)=FLQuant(seq(0,1,length.out=101))*refpts(eq)["crash","harvest"]
fbar(eq)[,1]=fbar(eq)[,2]*1e-10
hat=mpb:::prdFn("pellat",par,ssb(eq))

ggplot()+
  geom_line( aes(x,y),data=model.frame(FLQuants(x=ssb(eq),y=hat)))+
  geom_point(aes(x,y),data=model.frame(FLQuants(x=ssb(eq),y=catch(eq))),col="red")

## Fit IV fir r only ###########################################################
fn<-function(x,k,p,stock,yield){
  
  hat=mpb:::prdFn("pellat",FLPar(r=x[1],k=k,p=p),stock)
  
  sum((hat-yield)^2,na.rm=T)}

par=optimise(fn, c(0.1,1),k=c(refpts(eq)["virgin","ssb"]),p=pPar,stock=ssb(eq),yield=catch(eq))$minimum
par=FLPar(c("r"=par,"k"=refpts(eq)["virgin","ssb",drop=T],"p"=pPar))

fbar(eq)=FLQuant(seq(0,1,length.out=101))*refpts(eq)["crash","harvest"]
fbar(eq)[,1]=fbar(eq)[,2]*1e-10
hat=mpb:::prdFn("pellat",par,ssb(eq))

ggplot()+
  geom_line( aes(x,y),data=model.frame(FLQuants(x=ssb(eq),y=hat)))+
  geom_point(aes(x,y),data=model.frame(FLQuants(x=ssb(eq),y=catch(eq))),col="red")

### E biomass ##################################################################
fn<-function(x,stock,yield){
  
  hat=mpb:::prdFn("pellat",FLPar(r=x[1],k=x[2],p=x[3]),stock)
  
  sum((hat-yield)^2,na.rm=T)}

par=
  FLPar(c(optim(par=c(r=0.3,k=c(refpts(eq)["virgin","ssb"]),p=pPar), fn,stock=ebiomass(eq),yield=catch(eq))$par))

fbar(eq)=FLQuant(seq(0,1,length.out=101))*refpts(eq)["crash","harvest"]
fbar(eq)[,1]=fbar(eq)[,2]*1e-10
hat=mpb:::prdFn("pellat",par,ebiomass(eq))

ggplot()+
  geom_line( aes(x,y),data=model.frame(FLQuants(x=ssb(eq),y=hat)))+
  geom_point(aes(x,y),data=model.frame(FLQuants(x=ssb(eq),y=catch(eq))),col="red")
}

