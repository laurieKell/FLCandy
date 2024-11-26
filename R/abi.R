setMethod("abiAge",
          signature(object = "FLBRP"),
          function(object, ref = "msy", p = 0.9) {
            fbar(object) <- as.FLQuant(computeRefpts(object)[ref, "harvest", drop = TRUE], dimnames = list(iter = seq(dim(object)[6])))
            stk.n <- stock.n(object)[-1]
            cumN <- apply(stk.n, c(2, 6), cumsum) %/% quantSums(stk.n)
            ages <- ages(stk.n)
            ages[cumN <= p] <- NA
            apply(ages, c(2:6), function(x) min(c(x + 1, dims(object)$max), na.rm = TRUE))
          }
)

setMethod("abiMsy",
          signature(object = "FLBRP"),
          function(object, ref = "msy", p = 0.9) {
            fbar(object) <- as.FLQuant(computeRefpts(object)[ref, "harvest", drop = TRUE], dimnames = list(iter = seq(dim(object)[6])))
            A <- abiAge(object, ref, p)
            stk.n <- stock.n(object)[-1]
            flag <- FLQuant(ages(stk.n) >= FLCore:::expand(A, age = dimnames(stk.n)$age))
            apply(stk.n %*% flag, c(2, 6), sum) %/% apply(stk.n, c(2, 6), sum)
          }
)

## Gets P>ref age for stock
abistock<-function(x,A){
  
  stk.n=stock.n(x)[-1]
  
  if (dim(x)[6]>1&dim(A)[6]==1)
    A=propagate(A,dim(x)[6])
  
  ## Find proportion > Amsy
  amsy =FLQuant(rep(c(A),each=prod(dim(stk.n)[-6])),dimnames=dimnames(stk.n))
  flag =FLQuant(ages(stk.n)>=amsy)
  
  apply(stk.n%*%flag,c(2,6),sum)%/%apply(stk.n,c(2,6),sum)}


setMethod("abi",
          signature(object = "FLStock", age = "FLBRP"),
          function(object, age, ref = "msy", p = 0.9) {

            pmsy=abiMsy(age, ref, p)
           
            age =abiAge(age, ref, p)
            pt  =abistock(object, age)
           
            pt %/% pmsy})

setMethod("abi",
          signature(object = "FLStock", age = "FLQuant"),
          function(object, age) {
            stk.n <- stock.n(object)[-1]
            if (dim(object)[6] > 1 & dim(age)[6] == 1) {
              age <- propagate(age, dim(object)[6])
            }
            amsy <- FLQuant(rep(c(age), each = prod(dim(stk.n)[-6])), dimnames = dimnames(stk.n))
            flag <- FLQuant(ages(stk.n) >= amsy)
            apply(stk.n %*% flag, c(2, 6), sum) %/% apply(stk.n, c(2, 6), sum)
          }
)

#' @examples
#' \dontrun{
#' library(FLCore)
#' library(FLBRP)
#' 
#' data(ple4)
#' data(ple4brp)
#' 
#' abiAge(ple4brp)
#' abiMsy(ple4brp)
#' abi(ple4, ple4brp)

#' }
