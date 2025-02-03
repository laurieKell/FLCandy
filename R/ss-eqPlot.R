require(tidyr)
require(ggplot2)
require(plyr)
require(dplyr)
require(r4ss)


#' Extracts yield and surplus production from Stock Synthesis output
#' processes Stock Synthesis output to calculate yield and surplus production.
#'
#' @param object An output from SS_outputs 
#' @return A list containing processed time series data, equilibrium yield,
#'  reference points, and triangle data.
#' @examples
#' # Assuming 'ss_output' is an instance of StockSynthesisOut#' # result <- curveSS(ss_output)
#' @export
setMethod("curveSS", signature(object="list"), function(object,maxY=1.5){
  eqlYield  =object[["equil_yield"]]
  timeseries=object[["timeseries"]]
  dq        =object[["derived_quants"]][unique(object$derived_quants$Label)[210:226],]
  
  if (tolower(names(timeseries))%in%"yr")
    names(timeseries)[tolower(names(timeseries))=="yr"]="year"

  # Filter and prepare data for plotting
  ts=timeseries %>%
    filter(!Era %in% c("VIRG", "FORE")) %>%
    mutate(catch_total = rowSums(select(., starts_with("dead(B)")), na.rm = TRUE)) %>%
    group_by(year, Seas) %>%
    summarise(
      sum_bio_all = sum(Bio_all, na.rm = TRUE),
      sum_spawn_bio = sum(SpawnBio, na.rm = TRUE),
      sum_catch_total = sum(catch_total, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    group_by(year) %>%
    summarise(
      mean_bio_all = mean(sum_bio_all),
      mean_spawn_bio = mean(sum_spawn_bio),
      catch_total = sum(sum_catch_total)
    )
  names(ts)=c("year","biomass","ssb","yield")
  
  nyears=nrow(ts)
  
  ts$sp   =c(NA, ts$biomass[-1] - ts$biomass[-nyears] + ts$yield[-nyears])
  eql     =transmute(eqlYield, yield=Tot_Catch,ssb=SSB)
  rfs     =transmute(subset(eqlYield, Tot_Catch==max(Tot_Catch)), msy=Tot_Catch,bmsy=SSB)[1,]
  maxY    =signif(max(c(ts$yield,eql$yield))*maxY,1)
  triangle=data.frame(x=c(rfs$bmsy, rfs$bmsy, rfs$bmsy*maxY/rfs$msy, rfs$bmsy),
                      y=c(rfs$msy,      maxY,                  maxY, rfs$msy))
  
  return(list(tseries=ts,curve=eql,refpts=rfs,triangle=triangle,derived=dq))})
