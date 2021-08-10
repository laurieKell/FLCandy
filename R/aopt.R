aopt<-function(object){
  model(object)=geomean()$model
  params(object)=FLPar(a=1)
  
  fbar(object)=FLQuant(0,dimnames=list(year=1))
  stock.n(object)%*%stock.wt(object)}

#aopt(ple4brp)

