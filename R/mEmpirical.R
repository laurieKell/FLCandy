mEmpirical=function(object,par) {
  len=wt2len(stock.wt(object),par)
  res=len
  res[]=0.27
  res[len>3.0]=0.21
  res[len>=5.0]=0.4
  res}

