# Nephrops Harvest Control Rule (HCR) Implementation
# Based on ICES guidelines for survey-based management

hcrNephrops<-function(iYr,index,catch,cntrl,minCatch = 0.1,lag=1){
  
  # Error checking
  if (any(c(index[,ac(iYr-lag)], cntrl["btrig"], cntrl["bbuf"]) <= 0)) 
    stop("Biomass values must be positive")
  
  ## Decision rule
  # default
  abc=FLQuant(c(index[,ac(iYr-lag)]%*%cntrl["fmsy"]),dimnames=dimnames(catch[,ac(iYr)]))
  
  btrig=index[,ac(iYr-lag)]<c(cntrl["btrig"])
  bbuf =index[,ac(iYr-lag)]<c(cntrl["bbuf"])
  
  ## If stock below bbuffer reduce catch
  if (any(btrig))
    abc[,,,,,bbuf]=catch[,ac(iYr-lag),,,,bbuf]*0.8
  
  ## If stock below bbuffer reduce catch
  if (any(bbuf)){
    redFactor=(index[,ac(iYr-lag)]-cntrl["bbuf"])%/%(cntrl["btrig"]%-%cntrl["bbuf"])
    abc[,,,,,btrig]=index[,ac(iYr-lag),,,,btrig]%*%cntrl["fmsy"]%*%redFactor[,,,,,btrig]}
  
  # Apply minimum catch constraint
  abc=qmax(abc,catch[,ac(iYr-lag)]*minCatch)
  
  return(abc)}

if(FALSE){
  cntrl= FLPar(c(fmsy=0.16,btrig=5000,bbuf=2500))
  
  index=FLQuant(seq(1,10000,length.out=52),dimnames=list(year=1990:2041))
  catch=index%*%cntrl["fmsy"]
  
  for (i in 1991:2040) catch[,ac(i)]=hcrNephrops(i,index,catch,cntrl,lag=0)
  
  maxCatch=max(catch)
  ggplot(subset(model.frame(FLQuants(Index=index,Catch=catch)),year<=2040))+
    annotate("rect", xmin=5000, xmax=Inf,  ymin=0, ymax=maxCatch, fill="darkgreen", alpha=0.8) +
    annotate("rect", xmin=2500, xmax=5000, ymin=0, ymax=maxCatch, fill="gold",      alpha=0.8) +
    annotate("rect", xmin=0,    xmax=2500, ymin=0, ymax=maxCatch, fill="red",       alpha=0.8) +
    geom_vline(aes(xintercept=c(cntrl["btrig"])),col="orange",  lwd=1,lty=2)+
    geom_vline(aes(xintercept=c(cntrl["bbuf"])), col="darkred", lwd=1,lty=2)+
    geom_line(aes(Index,Catch),linewidth=1)+
    annotate("text", label=expression(B[buf]),    x=c(cntrl["bbuf"])*0.95,  y=0.1, size=4,hjust=1, vjust=1,col="darkred")+
    annotate("text", label=expression(B[trigger]),x=c(cntrl["btrig"]*0.975),y=0.1, size=4,hjust=1, vjust=1,col="orange")+
    labs(x="UWT Index",y="TAC",title="Nephrops Empirical Control Rule")+
    FLCandy:::theme_my()
}
