#' Calculate the reference age for a FLBRP object.
#'
#' This function calculates the reference age for a FLBRP object.
#'
#' @param object FLBRP object.
#' @param ref Reference point, e.g., "msy" (default) or "f0.1".
#' @param p Probability threshold (default = 0.9).
#'
#' @return An FLQuant object containing reference ages.
#'
#' @export
setGeneric("abiAge", function(object, ref = "msy", p = 0.9) {
  standardGeneric("abiAge")
})

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

#' Calculate P(N) at FMSY for a FLBRP object.
#'
#' This function calculates P(N) at FMSY for a FLBRP object.
#'
#' @param y A FLBRP object.
#' @param ref Reference point, e.g., "msy" (default) or "f0.1".
#' @param p Probability threshold (default = 0.9).
#'
#' @return An FLQuant object containing P(N) at FMSY.
#'
#' @export
setGeneric("abiMsy", function(object, ref = "msy", p = 0.9) {
  standardGeneric("abiMsy")
})

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

#' Calculate P obs for a FLStock object.
#'
#' This function calculates P obs for a FLStock object.
#'
#' @param object A FLStock object.
#' @param age Reference ages obtained from abiAge.
#'
#' @return An FLQuant object containing P obs.
#'
#' @export
setGeneric("abi", function(object, age, ...) {
  standardGeneric("abi")})

setMethod("abi",
          signature(object = "FLStock", age = "FLBRP"),
          function(object, age, ref = "msy", p = 0.9) {
            brp <- age
            age <- abiAge(brp, ref, p)
            pmsy <- abiMsy(brp, ref, p)
            abistock(object, age)
            pt <- abistock(object, age)
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
