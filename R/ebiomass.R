#' @rdname ebiomass
#' @export
setMethod("ebiomass", signature(object="FLStock"),
          function(object) {
            sel   <- harvest(object)
            wt    <- catch.wt(object) %*% sel %/% fapex(sel)
            eb.wt <- qmax(wt, 0.000001)
            
            apply(eb.wt %*% stock.n(object), 2:6, sum)
          })

setMethod("ebiomass", signature(object="FLBRP"),
          function(object) {
            sel   <- harvest(object)
            wt    <- catch.wt(object) %*% sel %/% fapex(sel)
            eb.wt <- qmax(wt, 0.000001)
            
            apply(eb.wt %*% stock.n(object), 2:6, sum)
          })
