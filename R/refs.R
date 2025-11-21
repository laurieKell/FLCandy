updateRefs<-function(x) {
  refpts(x) = rbind(
    refCreate(c("virgin", "msy", "crash", "spr.30", "spr.20")),
    rmax(x, 1.0),
    rmax(x, 0.3),
    rmsy(x, 0.5),
    rvirgin(x, 0.3),
    refCreate(attributes(x)$benchmark))
  refpts(x) = computeRefpts(x)
  refpts(x)=refpts(x)[sort(dimnames(refpts(x))$refpt)]
  return(x)}