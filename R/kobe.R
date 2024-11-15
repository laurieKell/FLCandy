tryIt<-function(x){
  rtn=try(x)
  if ("try-error"%in%is(rtn)) return(NULL)
  return(rtn)}

kobeFn<-function(x){
  names(attributes(x)$eqsim)    =tolower(names(attributes(x)$eqsim))
  names(attributes(x)$benchmark)=tolower(names(attributes(x)$benchmark))
    
  FLQuants(x, "stock"  =function(x) ssb(x)%/%eqsim(      x)["bmsy"],
              "harvest"=function(x) fbar(x)%/%benchmark(x)["fmsy"])}

setGeneric('kobe',  function(path,method,...) standardGeneric('kobe'))
setMethod( 'kobe',  signature(path='FLStock',method="missing"), 
           function(path,method) {tryIt(kobeFn(path))})
           