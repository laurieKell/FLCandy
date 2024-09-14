
BMSYFn<-function(M, B0, r) {
  
  prodFn <- function(B)
    r*B*(1-(B/B0)^M)

  BMSY=optimize(prodFn, interval = c(0, B0), maximum = TRUE)$maximum
  return(BMSY)}

MFn<-function(M) {
  BMSY  =BMSYFn(M, B0, r)
  ratio =BMSY/B0
  return(ratio-target)}

if(FALSE){
  target=0.4

  B0=1000  
  r =0.4    

  result=uniroot(MFn, interval = c(0.001, 2), tol = 1e-6)
  BMSYFn(result$root,B0,r)

  result$root}



