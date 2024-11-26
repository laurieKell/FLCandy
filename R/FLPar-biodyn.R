#' Create biodyn Object from FLPar Parameters
#'
#' @description Creates a biodyn object initialized with biomass at BMSY and catch at MSY
#'
#' @param params An FLPar object containing parameters r, k, and p
#' @param nyrs Number of years for projection (default = 50)
#' @return A biodyn object initialized for the specified number of years
#' @export
#' @examples
#' params=FLPar(r=0.5, k=1000, p=1)
#' bd=biodyn(params)
setMethod("biodyn", signature(object="FLPar",params="missing"), 
          function(object, nyrs=50) {
            if (!is(object, "FLPar"))
              stop("params must be an FLPar object")
            
            if (!all(c("r", "k", "p") %in% dimnames(object)$params))
              stop("params must contain r, k, and p")
            
            if (!is.numeric(nyrs) || nyrs <= 0)
              stop("nyrs must be a positive integer")
            
            rtn=new("biodyn")
            params(rtn)[c("r","k","p")]=object[c("r","k","p")]
            
            # Set initial states
            rtn@stock=window(FLQuant(), end=nyrs)
            rtn@stock[]=refpts(rtn)["bmsy"]
            rtn@catch=window(FLQuant(), end=nyrs)
            rtn@catch[]=refpts(rtn)["msy"]
            
            range(rtn)=unlist(dims(stock(rtn))[c("minyear","maxyear")])
            
            rtn=mpb:::fwd(rtn,catch=catch[,-1])
            
            return(rtn)})

