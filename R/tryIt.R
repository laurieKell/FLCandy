tryIt<-function(x){
  rtn=try(x)
  if ("try-error"%in%is(rtn)) return(NULL)
  return(rtn)}
