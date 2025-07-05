# Load required libraries
library(ggplot2)
library(plyr)
library(dplyr)
library(dplyr)
library(corrplot)

library(FLCore)
library(ggplotFL)

#' Time Series Summary for FLQuant objects
#'
#' Calculates mean, median, and variance across years for each age and iteration.
#'
#' @param object An FLQuant object
#' @return An FLQuants object containing mean, median, and variance
#' @export
#' @examples
#' data(ple4)
#' tsSmry(stock.wt(ple4))
setGeneric("tsSmry", function(object) standardGeneric("tsSmry"))

#' @rdname tsSmry
setMethod("tsSmry", signature(object="FLQuant"),
          function(object) {
            FLQuants(mean  =FLQuant(apply(object, c(1,3:6), mean)),
                     median=FLQuant(apply(object, c(1,3:6), median)),
                     var   =FLQuant(apply(object, c(1,3:6), var)))
          }
)

#' Time Series Correlation for FLQuant objects
#'
#' Calculates correlation matrix across years for each age and iteration.
#'
#' @param object An FLQuant object
#' @return An FLPar object containing correlation matrices
#' @export
#' @examples
#' data(ple4)
#' tsCor(stock.wt(ple4))
setGeneric("tsCor", function(object) standardGeneric("tsCor"))

#' @rdname tsCor
setMethod("tsCor", signature(object="FLQuant"),
          function(object) {
            rtn=aaply(object, c(3:6), function(x) cor(t(x)))
            
            FLPar(array(rtn, c(dim(object)[1], dim(object)[-2]),
                        list(age=dimnames(object)[[1]], age=dimnames(object)[[1]],
                             dimnames(object)[[3]], dimnames(object)[[4]],
                             dimnames(object)[[5]], dimnames(object)[[6]])))
          }
)

#' Time Series Covariance for FLQuant objects
#'
#' Calculates covariance matrix across years for each age and iteration.
#'
#' @param object An FLQuant object
#' @return An FLPar object containing covariance matrices
#' @export
#' @examples
#' data(ple4)
#' tsCov(stock.wt(ple4))
setGeneric("tsCov", function(object) standardGeneric("tsCov"))

#' @rdname tsCov
setMethod("tsCov", signature(object="FLQuant"),
          function(object) {
            rtn=aaply(object, c(3:6), function(x) cov(t(x)))
            
            FLPar(array(rtn, c(dim(object)[1], dim(object)[-2]),
                        list(age=dimnames(object)[[1]], age=dimnames(object)[[1]],
                             dimnames(object)[[3]], dimnames(object)[[4]],
                             dimnames(object)[[5]], dimnames(object)[[6]])))
          }
)

#' Time Series Autocorrelation Function for FLQuant objects
#'
#' Calculates autocorrelation function for each age and iteration.
#'
#' @param object An FLQuant object
#' @param lag.max Maximum lag at which to calculate the ACF (default is NULL)
#' @param type Character string giving the type of ACF to be computed: "correlation" (default), "covariance", or "partial"
#' @param na.action Function to handle missing values (default is na.fail)
#' @param demean Logical. Should the mean be subtracted? (default is TRUE)
#' @param ... Additional arguments passed to acf function
#' @return An FLQuant object containing autocorrelation function values
#' @export
#' @examples
#' data(ple4)
#' tsACF(stock.wt(ple4))
#' tsACF(stock.wt(ple4), lag.max=5, type="partial")
setGeneric("tsACF", function(object, ...) standardGeneric("tsACF"))

#' @rdname tsACF
setMethod("tsACF", signature(object="FLQuant"),
          function(object, lag.max=NULL, type="correlation", na.action=na.fail, demean=TRUE, ...) {
            rtn=aaply(object, c(1,3:6), function(x) {
              acf(x, lag.max=lag.max, type=type, plot=FALSE, 
                  na.action=na.action, demean=demean, ...)$acf
            })
            
            names(dimnames(rtn))[2]="year"
            
            FLQuant(rtn)
          })

#' Fit LOESS to FLQuant object
#'
#' This function fits a LOESS model to each age group in an FLQuant object
#' and returns fitted values and residuals.
#'
#' @param object An FLQuant object
#' @param ... Additional arguments passed to loess function
#'
#' @return An FLQuants object containing fitted values and residuals
#'
#' @export
#'
#' @examples
#' data(ple4)
#' result=flLoess(stock.wt(ple4))
setGeneric("flLoess", function(object, ...) standardGeneric("flLoess"))

#' @rdname flLoess
setMethod("flLoess", signature(object="FLQuant"),
          function(object, ...) {
            require(plyr)
            require(dplyr)
            
            # Fit LOESS for each age group
            rtn=adply(object, c(1,3:6), function(x) {
              rtn=loess(x ~ as.numeric(names(x)), ...)
              
              data.frame(year=as.numeric(names(x)),
                         hat =predict(rtn),residuals=residuals(rtn))
            })
          
            
            # Create FLQuants object with fitted values and residuals
            fitted   =as.FLQuant(transform(rtn[,1:6], data=rtn[,"hat"]))
            residuals=as.FLQuant(transform(rtn[,1:6], data=rtn[,"residuals"]))
            
            FLQuants(fitted=fitted, residuals=residuals)
          })


#' Summarise FLQuant objects
#'
#' This function calculates summary statistics for data stored in an FLQuant object.
#'
#' @param object An FLQuant object 
#' @return An FLQuants object containing mean, median, variance, and coefficient of variation
#' @export
#' @examples
#' data(ple4)
#' summarise(stock.wt(ple4))
setGeneric("summarise", function(object) standardGeneric("summarise"))

#' @rdname summarise
setMethod("summarise", signature(object = "FLQuant"),
          function(object) {
            # Calculate summary statistics
            mean  =apply(object, c(1,3:6), mean, na.rm = TRUE)
            median=apply(object, c(1,3:6), median, na.rm = TRUE)
            var   =apply(object, c(1,3:6), var, na.rm = TRUE)
            cv    =sqrt(var)/mean
            
            # Create FLQuants object with summary statistics
            summaryStats=FLQuants(
              mean    =FLQuant(mean,   dimnames=dimnames(object), units=units(object)),
              median  =FLQuant(median, dimnames=dimnames(object), units=units(object)),
              variance=FLQuant(var,    dimnames=dimnames(object), units=paste0("(", units(object), ")^2")),
              cv      =FLQuant(cv,     dimnames=dimnames(object), units="")
            )
            
            return(summary_stats)
          }
)

#' Generate Correlated Random Deviates for FLQuant Objects
#'
#' This function generates correlated random deviates based on the variance and 
#' correlation structure of an FLQuant object.
#'
#' @param object An FLQuant object
#' @param n Number of random deviates to generate (default is 100)
#' @param ... Additional arguments passed to mvtnorm::rmvnorm
#'
#' @return An FLQuant object containing the generated random deviates
#'
#' @import FLCore
#' @import mvtnorm
#'
#' @export
#'
#' @examples
#' data(ple4)
#' deviates(100,stock.wt(ple4),tsSmry(stock.wt(ple4))[["var"]],tsCor(stock.wt(ple4))))
setGeneric("deviates", function(n,object,...) 
  standardGeneric("deviates"))

#' @rdname deviates
setMethod("deviates", signature(n="numeric", object="FLQuant"),
      function(n, object, var, cov, ...) {
            
        var=var[drop=TRUE]
        cov=cov[,,drop=TRUE]
        # Generate random deviates by year
        devs=mvtnorm::rmvnorm(n    =n*prod(dim(object)[-(1)]), 
                              mean =rep(0, length(var)), 
                              sigma=cov,
                             ...)
            
        # Convert to FLQuant
        flq=FLQuant(c(t(devs)),dimnames=dimnames(propagate(object,n)))
          
        return(flq)})

#'  
#'  ##  Trends
#'  ggplot(stock.wt(ple4),aes(x=year, y=data, color=factor(age)))+
#'    geom_line(linewidth=0.1)+
#'    geom_point(alpha=0.5) +
#'    geom_smooth(se=FALSE) +
#'    labs(title="Weight Trends by Age Group",
#'         x    ="Year", y="Individual Mass",
#'         color="Age")+
#'    theme_minimal()+theme(legend.position="bottom")
#'  
#'  rtn=flLoess(stock.wt(ple4))
#'  
#'  
#'  ##  Residuals
#'  ggplot(rtn[["residuals"]])+
#'    geom_tile(aes(year, factor(age), fill=data)) +
#'    scale_fill_viridis_c()+
#'    labs(title="Heatmap of Residuals-at-Age Over Time",
#'         x="Year", y="Age",
#'         fill="Residuals") +
#'    theme_minimal()+theme(legend.position="bottom")
#'  
#'  
#'  ##  Correlations
#'  cor =tsCor(stock.wt(ple4))
#'  heatmap(cor[,,drop=TRUE], main="Correlation Between Age Groups")
#'  
#'  ##  ACF
#'  ggplot(tsACF(stock.wt(ple4)))+
#'    geom_line(aes(year,data,col=factor(age)))+
#'    labs(title="Autocorrelation",
#'         x="Year", y="ACF",
#'         colour="Age") +
#'    theme_minimal()+theme(legend.position="bottom")
#'  
#'  deviates(100,stock.wt(ple4),tsSmry(stock.wt(ple4))[["var"]],tsCov(stock.wt(ple4))[,,drop=TRUE])
#'  
#' data(ple4)






