# statistics.R - DESC
# /statistics.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


# compute {{{

compute <- function(x, statistics, refpts=FLPar(),
    years=setNames(list(dimnames(x[[1]])$year), nm=dims(x[[1]])$maxyear),
    probs=NULL) {
    
    # CREATE years list
    if(!is.list(years))
      years <- setNames(as.list(years), as.character(years))

    # CHECK years
    if(any(unlist(lapply(years,
      function(y) !all(as.character(y) %in% dimnames(x[[1]])$year)))))
      stop("years must be present in input object 'x' dimensions.")

    # SET names if not present
    if(is.null(names(years)))
      names(years) <- as.character(unlist(lapply(years,
        function(x) x[length(x)])))

    # CHECK dimensions
    if(any(unlist(lapply(x, function(y) any(dim(y)[c(1,3,4,5)] != 1)))))
      warning("metrics have length > 1 for 'quant', 'unit', 'season' or 'area', recycling over refpts might be wrong.")

    # CHECK statistics are unique
    if(length(names(statistics)) != length(unique(names(statistics))))
      stop("'statistics' must have unique names.")

    # LOOP over years
    res <- data.table::rbindlist(lapply(years, function(i) {
      # LOOP over statistics
      data.table::rbindlist(lapply(statistics, function(j) {
        # EVAL statistic
        as.data.frame(eval(j[[2]],
          c(FLCore::window(x, start=i[1], end=i[length(i)]),
            # REPEAT refpts by year because recycling goes year first
            lapply(as(refpts, 'list'), rep, each=length(i)))), drop=FALSE)
      }), idcol="statistic", fill=TRUE)[,c("statistic", "data", "iter")]
    }), idcol="year")
    
    # Set DT keys
    setkey(res, statistic, year)
    
    # QUANTILES if probs
    if(!is.null(probs)) {
      # statistic year name data prob
      res <- res[, .(data=quantile(data, probs=probs, na.rm=TRUE), prob=probs),
        by=.(statistic, year)]
    }
    
    return(res)
  }
# }}}

# convert {{{

convert <- function(x,
  map=list(FMSY=c("msy", "harvest"), SBMSY=c("msy", "ssb"))) {

  res <- Map(function(i, j) FLPar(c(x[i[1], i[2]]),
    dimnames=list(params=j, iter=seq(1, dim(x)[3]))),
    i=map, j=names(map))

  return(Reduce(rbind, res))
}

# }}}
