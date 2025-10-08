#' Calculate Reference Points Including ebiomass (EB)
#'
#' @description
#' Calculates reference points for an FLBRP object, including the ebiomass (EB) as an additional quant.
#'
#' @param object An FLBRP object.
#' @return An FLPar object with reference points, including the EB quant.
#' @export
setGeneric("refptsEB", function(object, ...) standardGeneric("refptsEB"))

#' @rdname refptsEB
#' @export
setMethod("refptsEB", signature(object="FLBRP"),
  function(object,biomass="ssb",...) {
    fbar(object) = FLQuant(refpts(object)[, "harvest", drop=TRUE])
    fbar(object) = qmax(fbar(object), 1e-12)
    object = brp(object)
    dmns = dimnames(refpts(object))
    dmns$quant = c(dimnames(refpts(object))$quant, "eb")
    rfpts = FLPar(NA, dimnames=dmns)
    rfpts[, -length(dmns$quant)] = computeRefpts(object)
    rfpts[, length(dmns$quant)] = ebiomass(object)
    rfpts
  })




