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
  
  if (!("Tot_Catch"%in%names(eqlYield))&("Catch"%in%names(eqlYield)))
    names(eqlYield)[names(eqlYield)=="Catch"]="Tot_Catch"

  if (any(tolower(names(timeseries))%in%"yr"))
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
  vBiomass=vBio(object)
  
  return(list(tseries=merge(ts,vBiomass),curve=eql,refpts=rfs,triangle=triangle,derived=dq))})



logistic <- plogis  # maps to standard logistic function
logit <- qlogis     # maps to inverse logistic function

vBio<-function(rep) {
  # Get selectivity parameters
  sel_F_peak  =unlist(c(subset(rep$parameters, Label=="SzSel_Fem_Peak_Fishery_2(2)","Value")))
  sel_F_ascend=unlist(c(subset(rep$parameters, Label=="SzSel_Fem_Ascend_Fishery_2(2)","Value")))
  sel_M_peak  =unlist(c(subset(rep$parameters, Label=="SzSel_Male_Peak_Fishery_3(3)","Value")))
  sel_M_ascend=unlist(c(subset(rep$parameters, Label=="SzSel_Male_Ascend_Fishery_3(3)","Value")))
  
  # Calculate selectivity using logistic transform
  calc_sel<-function(len, peak, ascend) 
    logistic((len - peak)/exp(ascend))
  
  # Get length bins from data
  len_bins=as.numeric(gsub("\\D", "", names(rep$sizeselex)[-(1:4)]))
  len_bins[is.na(len_bins)]=0
  
  # Calculate sex-specific selectivity
  sel_F=calc_sel(len_bins, sel_F_peak, sel_F_ascend)
  sel_M=calc_sel(len_bins, sel_M_peak, sel_M_ascend)
  
  # Extract and scale biomass (assuming SmryBio is in log space)
  bio_F=rep$timeseries$`SmryBio_SX:1_GP:1`
  bio_M=rep$timeseries$`SmryBio_SX:2_GP:1`
  
  # Calculate vulnerable biomass
  data.frame(
    year   = rep$timeseries$Yr,
    female = bio_F * sum(sel_F),
    male  = bio_M * sum(sel_M))}
