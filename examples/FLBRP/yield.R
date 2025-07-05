library(FLBRP)
library(cmna)

data("ple4")
brp=FLBRP(ple4)

yieldRef<-function(object,frac){
    
  ## create ref pt object with MSY and yield=0.5MSY
  dimnames(refpts(brp))[[1]][7]="yield"
  refpts(brp)=refpts(brp)[c("msy","virgin","yield")]
  refpts(brp)["yield","yield"]=frac*refpts(brp)["msy","yield"]
  
  ## Does not work
  #computeRefpts(brp)
  
  ## Do it by hand using bisection
  fn<-function(x,yield=frac,eq=brp) {
    refpts(eq)["yield",]=NA
    refpts(eq)["yield","harvest"]=x
    
    refpts(eq)=computeRefpts(eq)
    
    rtn=refpts(eq)["msy","yield"]*yield-refpts(eq)["yield","yield"]
    
    # trace
    #print(cat(x,c(rtn)))
    
    c(rtn)}
  
  res=bisection(fn, refpts(brp)["msy","harvest"], refpts(brp)["msy","harvest"]*100, tol=1e-20, m=100)
  
  refpts(brp)["yield",]=c(res,rep(NA,7))
  
  refpts(brp)=computeRefpts(brp)
  brp}

brp=yieldRef(brp,0.25)

## 50% yield achived as a fraction pf BMSY
refpts(brp)["yield","ssb"]/refpts(brp)["msy","ssb"]
  
## 50% yield achived as a fraction pf Virgin
refpts(brp)["yield","ssb"]/refpts(brp)["virgin","ssb"]

dimnames(refpts(brp))


refpts(brp)["yield",1:8]=c(NA, 0.05,rep(NA,6))
computeRefpts(brp)
refpts(brp)["yield",1:8]=c(refpts(brp)["msy","harvest"]*0.5, 0.05,rep(NA,6))
computeRefpts(brp)
refpts(brp)["yield",1:8]=c(refpts(brp)["msy","harvest"]*2.0, 0.05,rep(NA,6))
computeRefpts(brp)

#Y/S
refpts(brp)["yield",1:8]=c(NA, 9.50e-02,NA,9.44e-01,rep(NA,4))
refpts(brp)=computeRefpts(brp)

data(ple4brp)
dimnames(refpts(ple4brp))[[1]][7]="yield"
refpts(ple4brp)["yield"]=c(NA,refpts(ple4brp)["msy","yield"]*2,NA,refpts(ple4brp)["msy","ssb"],rep(NA,4))
plot(brp(ple4brp),ncol=2)

dat=transform(model.frame(FLQuants(ple4brp,ssb=ssb,catch=catch,f=fbar)),p=catch/ssb)


ggplot(subset(dat,ssb>0))+
  geom_line(aes(ssb,p))+
  geom_vline(aes(xintercept=c(refpts(ple4brp)["msy","ssb"])),col="green")+
  geom_vline(aes(xintercept=c(computeRefpts(ple4brp)["yield","ssb"])),col="red")

computeRefpts(ple4brp)["yield","ssb"]
refpts(ple4brp)["msy","yield"]/refpts(ple4brp)["msy","ssb"]
