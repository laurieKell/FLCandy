#' Growth models
#'
#' @name Growth models
#' @rdname growth-models
#' @description Functions to model fish growth.
#' @details This group of functions provides different growth models commonly used 
#' in fisheries science.
#' 
#' @examples
#' data(ple4)
#' vonbert(linf = 35, k = 0.352, t0 = -0.26, age = 1:14)
#' vonbert
#'
#' @description Von Bertalanffy growth model.
#'
#' @param linf Asymptotic length.
#' @param k Growth coefficient.
#' @param t0 Theoretical age at length zero.
#' @param age Vector of ages for which to calculate lengths.
#'
#' @return A vector of lengths corresponding to the given ages.
#' 
#' @examples
#' vonbert(linf = 35, k = 0.352, t0 = -0.26, age = 1:14)
vonbert <- function(linf, k, t0, age) {
  linf * (1.0 - exp((-k * (age - t0))))
}

#' ivonbert
#'
#' @rdname growth-models
#' @description Inverse Von Bertalanffy growth model.
#'
#' @param linf Asymptotic length.
#' @param k Growth coefficient.
#' @param t0 Theoretical age at length zero.
#' @param len Vector of lengths for which to calculate ages.
#'
#' @return A vector of ages corresponding to the given lengths.
#' 
#' @examples
#' ivonbert(35, 0.352, -0.26, 1:34)
ivonbert <- function(linf, k, t0, len) {
  pmax(0, log(1 - (len / linf)) / (-k) + t0)
}

#' gompertz
#'
#' @rdname growth-models
#' @description Gompertz growth model.
#'
#' @param linf Asymptotic length.
#' @param a Parameter a.
#' @param k Growth coefficient.
#' @param age Vector of ages for which to calculate lengths.
#'
#' @return A vector of lengths corresponding to the given ages.
#' 
#' @examples
#' gompertz(linf = 179.13, k = 0.4088, a = 1.7268, age = 1:12)
gompertz <- function(linf, a, k, age) {
  linf * exp(-a * exp(log(k) * age))
}

#' richards
#'
#' @rdname growth-models
#' @description Richards growth model.
#'
#' @param linf Asymptotic length.
#' @param k Growth coefficient.
#' @param b Parameter b.
#' @param m Parameter m.
#' @param age Vector of ages for which to calculate lengths.
#'
#' @return A vector of lengths corresponding to the given ages.
#' 
#' @examples
#' richards(linf = 178.63, k = 0.424, b = -7.185, m = 2880.4, age = 1:12)
richards <- function(linf, k, b, m, age) {
  linf / exp(log(1 + exp(-k * age + b)) * m)
}
