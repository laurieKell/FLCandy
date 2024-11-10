#' @title 
#' 
#' @description Takes an `FLStock` with seasons and collapses into years
#'
#' @param object an \code{FLStock} object 
#' @param seasons a numeric with seasons
#' 
#' @aliases
#' 
#' @return \code{FLStock} object
#'
#' @seealso \code{\link{expand}}
#'
#' @export seasonalise
#' @docType methods
#' @rdname seasonalise
#'
#' 
#' @examples
#' \dontrun{
#' }

annualise<-function(x) {
  
  # ADD slots (catch/landings.discards, m)
  res <- qapply(x, function(s) unitSums(seasonSums(s)))
  
  # MEAN wts
  catch.wt(res)[] <- unitSums(seasonSums(catch.wt(x) * catch.n(x))) / 
    unitSums(seasonSums(catch.n(x)))
  
  landings.wt(res)[] <- unitSums(seasonSums(landings.wt(x) * landings.n(x))) / 
    unitSums(seasonSums(landings.n(x)))
  
  discards.wt(res)[] <- unitSums(seasonSums(discards.wt(x) * discards.n(x))) / 
    unitSums(seasonSums(discards.n(x)))
  
  stock.wt(res)[] <- unitSums(seasonSums(stock.wt(x) * stock.n(x))) / 
    unitSums(seasonSums(stock.n(x)))
  
  # RECONSTRUCT N: N0 = N1 / exp(-M - F)
  
  stkn <- unitSums(stock.n(x)[,,,4])
  m(res) <- unitMeans(seasonSums(m(x)))
  harvest(res) <- unitMeans(seasonSums(harvest(x)))
  
  stock.n(res)[] <- stkn / exp(-m(res) - harvest(res))
  
  # mat
  
  mat(res)[] <- unitSums(seasonSums(mat(x) * stock.n(x))) / 
    unitSums(seasonSums(stock.n(x)))
  mat(res) <- mat(res) %/% apply(mat(res), c(1,3:6), max)
  mat(res)[is.na(mat(res))] <- 0
  
  # spwn
  
  m.spwn(res) <- 0.5
  harvest.spwn(res) <- 0.5
  
  # totals
  
  catch(res)    <- computeCatch(res)
  landings(res) <- computeLandings(res)
  discards(res) <- computeDiscards(res)
  stock(res)    <- computeStock(res)
  
  return(res)}
