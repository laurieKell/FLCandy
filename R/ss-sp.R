
spFunc<-function(x,add=FALSE,col2="blue",labels=c("","","biomass","Surplus Production"),plotIt=FALSE){
  # function to calculate and plot surplus production
  
  # timeseries excluding equilibrium conditions and forecasts
  ts <- x$timeseries[!x$timeseries$Era %in% c("VIRG","FORE"),]
  
  # get total dead catch
  stringB <- "dead(B)"
  catchmat <- as.matrix(ts[, substr(names(ts),1,nchar(stringB))==stringB])
  # aggregate catch across fleets
  catch <- rowSums(catchmat)
  
  # aggregate catch and biomass across seasons and areas
  catch_agg <- aggregate(x=catch, by=list(ts$Yr), FUN=sum)$x
  Bio_agg <- aggregate(x=ts$Bio_all, by=list(ts$Yr), FUN=sum)$x
  
  # number of years to consider
  Nyrs <- length(Bio_agg)
  sprod <- rep(NA, Nyrs)
  
  # calculate surplus production as difference in biomass adjusted for catch
  sprod[1:(Nyrs-1)] <- Bio_agg[2:Nyrs] - Bio_agg[1:(Nyrs-1)] + catch_agg[1:(Nyrs-1)]
  sprodgood <- !is.na(sprod)
  Bio_agg_good <- Bio_agg[sprodgood]
  sprod_good <- sprod[sprodgood]
  xlim <- c(0, max(Bio_agg_good, na.rm=TRUE))
  ylim <- c(min(0, sprod_good, na.rm=TRUE), max(sprod_good, na.rm=TRUE))
  
  if (plotIt){
    # make empty plot
    if(!add){
      plot(0, ylim=ylim, xlim=xlim, xlab=labels[3], ylab=labels[4], type="n")
    }
    # add lines
    lines(Bio_agg_good, sprod_good, col=col2)
    # make arrows
    old_warn <- options()$warn      # previous setting
    options(warn=-1)                # turn off "zero-length arrow" warning
    s <- seq(length(sprod_good)-1)
    arrows(Bio_agg_good[s], sprod_good[s], Bio_agg_good[s+1], sprod_good[s+1],
           length=0.06, angle=20, col=col2, lwd=1.2)
    options(warn=old_warn)  #returning to old value
    
    # add lines at 0 and 0
    abline(h=0,col="grey")
    abline(v=0,col="grey")
    # add blue point at start
    points(Bio_agg_good[1], sprod_good[1], col=col2, bg="white", pch=21)}
  
  invisible(data.frame(year=seq(length(Bio_agg)),biomass=Bio_agg, sp=sprod, catch=catch_agg,ssb=ddply(ts,.(Yr),with, sum(SpawnBio,na.rm=TRUE))[,2]))}

