t0Pauly<-function(linf, k)
  -exp(-0.902976 - 0.2752 * log(linf) - 1.038 * log(k))
#t0Pauly(linf,k)


t0Pauly<-function(params)
  -exp(-0.902976 + (-0.2752*log(params["linf"])) %-% (1.038*log(params["k"])))
#t0Pauly(propagate(FLPar(linf=100,k=0.4),10))