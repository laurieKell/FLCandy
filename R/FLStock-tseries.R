#' @rdname tseries
#' @export
setMethod("tseries", signature(object="FLStock"),
          function(object,flqs=list(catch=function(object) catch(object),
                                    eb   =function(object) ebiomass(object),
                                    ssb  =function(object) ssb(object),
                                    f    =function(object) fbar(object),
                                    h    =function(object) catch(object)/ebiomass(object),
                                    m    =function(object) FLQuant(aaply(m(object)[ac(range(object)["minfbar"]:range(object)["maxfbar"])],2,mean),
                                                                   dimnames=dimnames(fbar(object))))){
            model.frame(metrics(object,flqs),drop=TRUE)})

#' @rdname tseries
#' @export
setMethod("tseries", signature(object="FLStocks"),
          function(object,flqs=list(catch=function(object) catch(object),
                                    eb   =function(object) ebiomass(object),
                                    ssb  =function(object) ssb(object),
                                    f    =function(object) fbar(object),
                                    h    =function(object) catch(object)/ebiomass(object),
                                    m    =function(object) FLQuant(aaply(m(object)[ac(range(object)["minfbar"]:range(object)["maxfbar"])],2,mean),
                                                                   dimnames=dimnames(fbar(object))))){
            result=lapply(object, tseries, flqs=flqs)
            rtn=do.call(rbind, result)
            rtn=cbind(.id=gsub("\\.([^.]+)$","",dimnames(rtn)[[1]]),rtn)
            rtn})
