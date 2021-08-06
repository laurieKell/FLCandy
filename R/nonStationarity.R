## Set M, wt, sel & mat to vary by iter based on annual values to look at non-stationarity
nonStationarity<-function(x,sr=NULL,nyears=dim(x)[2],slots=c("m","mat","stock.wt","catch.wt","catch.sel"),model="bevholtSV",h=0.8){
  
  if (is.null(sr)){
    eq=FLBRP(x,nyears=nyears)
    sr=as.FLSR(x,model=model)
    sr=fmle(sr,
            fixed=list(s=h,spr0=spr0(eq)),
            control=list(silent=TRUE),
            method="Brent",
            lower=c(0.001),upper=max(ssb(sr))*10)
    params(eq)=ab(params(sr),substr(model,1,gregexpr("SV",model)[[1]][1]-1))[-dim(params(sr))[1]]
    model( eq)=do.call(substr(model,1,gregexpr("SV",model)[[1]][1]-1), list())$model
    refpts(eq)=computeRefpts(eq)}
  else if ("FLBRP"%in%is(sr)) eq=sr
  else if ("FLSR"%in%is(sr))  eq=FLBRP(x,sr=sr,nyears=nyears)
  
  eq=propagate(eq,dim(ssb(x))[2])
  
  q2p<-function(x){
    tmp=as.data.frame(x,drop=T)
    names(tmp)[names(tmp)=="year"]="iter"
    as.FLQuant(tmp)}
  
  if ("m"%in%slots)   m(eq)=q2p(m(x))
  if ("mat"%in%slots) mat(eq)=q2p(mat(x))
  
  if ("stock.wt"%in%slots) stock.wt(   eq)=q2p(stock.wt(   x))
  if ("catch.wt"%in%slots|"landinhgs.wt"%in%slots) landings.wt(eq)=q2p(landings.wt(x))
  if ("catch.wt"%in%slots|"discards.wt" %in%slots) discards.wt(eq)=q2p(discards.wt(x))
  
  if ("catch.sel"%in%slots|"landinhgs.sel"%in%slots) landings.sel(eq)=q2p(catch.sel(x)%*%landings.n(x)%/%catch.n(x))
  if ("catch.sel"%in%slots|"discards.sel"%in%slots)  discards.sel(eq)=q2p(catch.sel(x)%*%discards.n(x)%/%catch.n(x))
  
  nms=dimnames(refpts(eq))
  nms[[1]]=c(nms[[1]],"spr.100")
  refpts(eq)=FLPar(array(NA,lapply(nms,length),nms))
  
  params(eq)=propagate(params(eq),dim(ssb(x))[2])
  pars      =rbind(params(eq),FLPar("v"=c(spr0Yr(x))*c(params(eq)["R0"])),FLPar("spr0"=c(spr0Yr(x))))[-2]
  params(eq)=as(adply(pars, 2, function(x) t(ab(FLPar(x),"bevholt")[-3]))[,-1],"FLPar")
  
  rtn=computeRefpts(eq)
  
  transform(as.data.frame(rtn),year=as.numeric(dimnames(x)$year[iter]))[,-3]}