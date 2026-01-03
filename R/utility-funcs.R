#' @title Calculate Fishery Indicators
#' @description Computes a suite of biological reference indicators for stock assessment using FLR objects.
#'
#' @param x An FLStock object containing stock data
#' @param y An optional FLBRP object containing reference points (default missing)
#' 
#' @return A data.frame of biological indicators including:
#' \itemize{
#'   \item SSB/SSBmsy
#'   \item F/Fmsy
#'   \item Spawning potential ratio (SPR)
#'   \item Maturity/age structure metrics
#' }
#' 
#' @examples
#' \dontrun{
#' data(ple4)
#' brp <- FLBRP(ple4)
#' ind(ple4) # Without reference points
#' ind(ple4, brp) # With reference points
#' }
#' 
#' @rdname ind
#' @export
setGeneric("ind", function(x, y, ...) standardGeneric("ind"))

#' @rdname ind
setMethod("ind", signature(x="FLStock", y="missing"),
          function(x) {          
            # Extract reference points from attributes if available
            fmsy <- FLPar(attributes(x)$benchmark["Fmsy"])
            bmsy <- FLPar(attributes(x)$eqsim["BMSY"]) 
            
            # Calculate biological indicators
            rtn <- FLQuants(
              SSB   = ssb(x) %/% bmsy,
              F     = fbar(x) %/% fmsy,
              SPR0  = FLCandy::spr0Yr(x),
              #FRatio = fjuv(x[-1]) %/% fapex(x[-1]),
              SPR   = ssb(x) / rec(x),
              amat  = FLCandy::amat(mat(x), 0.5, what="i"),
              wmat  = FLCandy::wmat(x),
              POS   = FLCandy::pos(x),
              ASA   = FLCandy::asa(x),
              POS_  = {stock.n(x) = catch.n(x); m.spwn(x) = 0; pos(x)},
              ASA_  = {stock.n(x) = catch.n(x); m.spwn(x) = 0; asa(x)}
            )
            
            # Add ABI if reference points provided
            if (!missing(y)) rtn[["ABI"]] <- abi(x, y)
            
            model.frame(rtn, drop=TRUE)
          })

setMethod("ind", signature(x="FLStock", y="FLBRP"),
          function(x, y) {
            
            # Extract reference points from FLBRP object
            bmsy <- refpts(y)["msy", "ssb"]
            fmsy <- refpts(y)["msy", "harvest"]
            
            # Calculate biological indicators
            rtn <- FLQuants(
              SSB   = ssb(x) %/% bmsy,
              F     = fbar(x) %/% fmsy,
              SPR0  = FLCandy::spr0Yr(x),
              #FRatio = fjuv(x[-1]) %/% fapex(x[-1]),
              SPR   = ssb(x) / rec(x),
              amat  = FLCandy::amat(mat(x), 0.5, what="i"),
              wmat  = FLCandy::wmat(x),
              POS   = FLCandy::pos(x),
              ASA   = FLCandy::asa(x),
              POS_  = {stock.n(x) = catch.n(x); m.spwn(x) = 0; pos(x)},
              ASA_  = {stock.n(x) = catch.n(x); m.spwn(x) = 0; asa(x)}
            )
            
            # Add ABI if reference points provided
            if (!missing(y)) rtn[["ABI"]] <- abi(x, y)
            
            model.frame(rtn, drop=TRUE)
          })


#' Get Observed Exploitable Biomass from FLBRP Object
#'
#' @description Retrieves observed exploitable biomass values stored as an 
#'              attribute in an FLBRP object.
#'
#' @param x An object of class \code{FLBRP}
#' @param ... Additional arguments (not used)
#'
#' @return FLQuant containing observed exploitable biomass or NULL if the 
#'         attribute doesn't exist
#'
#' @examples
#' \dontrun{
#' data(ple4brp)
#' eb.obs(ple4brp)
#' }
#'
#' @seealso \code{\link{ssb.obs}} for observed spawning stock biomass
#' @export
#'
setGeneric("eb.obs", function(x, ...) standardGeneric("eb.obs"))

#' @rdname eb.obs
#' @export
setMethod("eb.obs", signature(x="FLBRP"),
          function(x){ 
            
            if (!("eb.obs"%in%names(attributes(x))))
              return(NULL)
            
            attributes(x)$eb.obs})

