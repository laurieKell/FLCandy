
mThen<-function(x) 4.899*(x^(-0.916))

mGlim=function(object,par) {
  len=wt2len(stock.wt(object),par)
  
  #lnM= a + b ln(L/L∞) + c*lnK
  
  res= exp(par["m1"]%+%(par["m2"]%*%log(len%/%par["linf"]))%+%(par["m3"]%*%log(par["k"])))
  
  res}

dat=c(m1= 0.28, m1=(0.48- 0.07)/(2 * 1.96),
      m2=-1.30, m2=(- 1.19 +1.42)/(2 * 1.96),
      m3= 1.08, m3=(1.24 - 0.92)/(2 * 1.96))
mPar=FLPar(dat[seq(1,6,2)])
mSD =FLPar(dat[seq(2,6,2)])