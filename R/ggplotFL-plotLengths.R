#' @examples
#' data(ple4)
#' iak <- invALK(FLPar(linf=42, k=2.1, t0=0.1), age=1:10)
#' les <- lenSamples(catch.n(ple4)[, 1:10], iak)
#' units(les) <- "cm"
#' plotLengths(les)
#' plotLengths(les) + geom_vline(aes(xintercept=mean), colour="red")
#' plotLengths(les) + geom_vline(aes(xintercept=median, colour="green"))
#' plotLengths(les) + geom_vline(aes(xintercept=mode), colour="black")
#' plotLengths(group(les, sum, year=year - year%%5))
#' plotLengths(group(les, mean, year=year - year%%5))
#' plotLengths(group(les, sum, year=year - year%%10))
#' plotLengths(les, block="decade")
#' plotLengths(les, direction="vertical")
#' plotLengths(group(les, mean, year=year - year%%5), direction="vertical")
#' plotLengths(les, direction="vertical", block="decade")
#' plotLengths(les) +
#'   geom_vline(data=as.data.frame(FLPar(L50=38)), aes(xintercept=data),
#'   linewidth=1)
#' plotLengths(les) +
#'   geom_vline(data=as.data.frame(FLPar(seq(38, 46, length=10), dimnames=list(params='L50', year=1957:1966, iter=1))), aes(xintercept=data),
#'   linewidth=1)
#' plotLengths(les, block="lustrum") +
#'   geom_vline(data=as.data.frame(FLPar(seq(38, 46, length=10), dimnames=list(params='L50', year=1957:1966, iter=1))), aes(xintercept=data),
#'   linewidth=1)

# TODO: plotComps
# TODO: OPTION to remove mean/median

setMethod("plotLengths", signature(x="FLQuant"),
          function(x, direction = c("horizontal", "vertical"),
                   block=c("lustrum", "decade"), palette=flpalette) {
            
            # args
            direction <- match.arg(direction)
            block <- match.arg(block)
            step <- switch(block, 'decade'=10, 'lustrum'=5)
            
            # CONVERT to data.table w/date, timestep
            dat <- data.table(as.data.frame(x, timestep=TRUE,
                                            date=TRUE))
            
            # CALCULATE props by timestep & unit
            dat[, prop:=data / min(data[data>0]), by=.(timestep, unit)]
            # median
            dat[, median:=median(rep(len, prop)), by=.(timestep, unit)]
            # mean
            dat[, mean:=mean(rep(len, prop)), by=.(timestep, unit)]
            # mode
            dat[, mode:=unique(len[prop == max(prop)]), by=.(timestep, unit)]
            
            # min & max
            dat[, min:=min(len[data > 0]), by=.(timestep, unit)]
            dat[, max:=max(len[data > 0]), by=.(timestep, unit)]
            
            # PLOT without facets
            p <- ggplot(dat, aes(x=len, y=data)) +
              geom_col(fill="gray", alpha=0.4, colour="black") +
              ylab("") + xlab(paste0("Length (", units(x), ")")) +
              theme(axis.text.x = element_blank(),
                    axis.ticks.x = element_blank()) +
              # mean
              # geom_vline(aes(xintercept=mean), color=palette[1], linewidth=1, alpha=0.5) +
              # geom_point(aes(x=mean, y=1), color=palette[1], size=2, alpha=0.5) + 
              # median
              # geom_vline(aes(xintercept=median), color=palette[2], linewidth=1,
              #   alpha=0.5) +
              # geom_point(aes(x=median, y=1), color=palette[2], size=2, alpha=0.5) +
              xlim(c(0,NA)) +
              theme(legend.position="none")
            
            # PARSE dims and CREATE formula
            dm <- dim(x)
            
            #  - year + season ~ area
            #  - fill =  unit | area
            
            if(direction == "horizontal") {
              
              # SET facetting
              if(dm[4] > 1)
                facets <- ~ year + season
              else
                facets <- ~ year
              
              p <- p + facet_grid(facets, scales="free") +
                coord_flip() + scale_y_reverse() +
                geom_vline(aes(xintercept=0,
                               color=as.factor(year -  year %% step)), linewidth=3)
            } else {
              
              # SET facetting
              if(dm[4] > 1)
                facets <- (year %% step) + season ~ (year - year %% step)
              else
                facets <- (year %% step) ~ (year - year %% step)
              
              p <- p + facet_grid(facets, scales="free")
            }
            
            return(p)
          })

# }}}
