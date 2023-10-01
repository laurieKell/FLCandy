if(FALSE){
################################################################################
#### OM descriptive statistics & SPM priors ####################################
################################################################################
require(FLCore)
require(FLBRP)
require(FLife)
require(mpb)
require(ggplotFL)
require(plyr)

pars=lhPar(FLPar(linf=100))
eq  =lhEql(pars)

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

