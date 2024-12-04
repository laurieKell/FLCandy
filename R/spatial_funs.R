#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>                    Spatial indicator functions
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#> List of functions:
#>
#> 1. spi(): index of aggregation
#> 2. pa_rect(): index of occupancy
#> 3. pa_haul(): index of occupancy
#> 4. lorenz_data(): 
#> 5. lorenz_plot(): plot Lorenz curve 
#> 6. Gini(): index of aggregation
#> 7. d95(): index of aggregation
#>! 8. spreadingarea_data(): prep data for spreading area calculation
#>! 9. spreadingarea_calc(): index of aggregation
#>! 10. equivalentarea(): index of aggregation
#> 11. chullarea(): index of spread
#> 12. cog(): index of location
#> 13. inertia(): index of spread
#>! 14. coginert(): computes multiple indices (CoG, Inert, EllipseArea[spread])
#>! 15. coginertMap(): plots CoG and Ellipse Area
#> 16. shift_legend(): moves legend into empty facet cell
#>
#> Pending changes:
#>
#> 1. filtersurveydata()
#>> hlhh data is filtered within each spatind function. Create function that
#>> filters data then pass the resulting dataset to spatind funs rather than
#>> having to filter data every time. 
#> 2. hlhh arguments to funs
#>> Some funs currently require data to be filtered first and then passed to the
#>> fun that calcs the spatind (e.g. inert, cog, sa, ea). Standardise funs so that
#>> hlhh data can be passed and spatinds can be calc'd all in one fun
#> 3. Add function to plot multiple surveys
#> 4. Add function to plot presence/absence data with marginal distributions
#>> add fucntionality to add in depth (conditional colour) and density (conditional size)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
spi <- function(hlhh, yrs, qrs, species_aphia, stk_divs){
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>SPI>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
  #> This function calculate SPI based on the observed catches. The are argument
  #> can be used to weight areas in circumstances of unequal sized areas
  #> Since ICES rectangles have the same area, this should be set to 1 so that all
  #> rectangles are given equal weight
  #> Index from Plowman, 2003:
  #> https://doi.org/10.1016/S0168-1591(03)00142-4
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
  yrs <- as.numeric(yrs)
  qrs <- as.numeric(qrs)
  # Filter data
  s1 <- hlhh %>%
    filter(Area_27 %in% stk_divs, 
           Year %in% c(yrs),
           Quarter %in% c(qrs),
           HaulVal != "I") %>% # remove invalid hauls %>%
    distinct() # remove duplicates 
  
  # Check what data is available
  if(!identical(as.numeric(unique(s1$Quarter)), qrs)){
    warning(paste0("Data not found in all survey quarters provided. 
", "Only data for quarters ", paste(sort(unique(s1$Quarter)), collapse = ", ")), " available in ", unique(hlhh$Survey.x), " survey data (years ", min(yrs), ":", max(yrs), "), region(s) ", paste0(stk_divs, collapse = ", "), ".
", "No data for quarter ", paste(c(setdiff(qrs, unique(s1$Quarter)), setdiff(unique(s1$Quarter), qrs)), collapse = ", "), "\n", immediate. = TRUE)
  }
  if(!identical(as.numeric(sort(unique(s1$Year))), yrs)){
    warning(paste0("Species not found in all survey years provided. 
", "No data for years ", paste(c(setdiff(yrs, unique(s1$Year)), setdiff(unique(s1$Year), yrs)), collapse = ", "), " in ", unique(hlhh$Survey.x), " survey data (quarters ", paste(sort(unique(s1$Quarter)), collapse = ", "), "), region(s) ", paste0(stk_divs, collapse = ", "), ".\n"), immediate. = TRUE)
  }
  
  # All rectangles sampled each year and the total duration of hauls within them for ALL species
  rectsamp <- s1 %>% 
    select(Year,StatRec,HaulDur) %>%
    group_by(Year, StatRec) %>%
    summarise(TotalHaulDur = sum(HaulDur))
  
  # Fo: observed frequency -- catches in each positive rectangle/area
  observed2 <- s1 %>% 
    filter(Valid_Aphia == species_aphia) %>%
    select(haul.id, Valid_Aphia, Year, Quarter, StatRec, Area_27, TotalNo, HaulVal, HaulDur) %>%
    distinct() %>%
    group_by(Year, StatRec) %>%
    summarise(Fo = sum(TotalNo),
              TotSpcsHaulDur = sum(HaulDur),
              Fo.Dur = Fo/TotSpcsHaulDur)
  
  # Merge and divide the total number of catches by total duration hauled in each rectangle
  observed <- left_join(rectsamp, observed2, by = c("Year", "StatRec")) # left_join keeps the rectangles where there were no species observations as NAs
  observed$Fo[is.na(observed$Fo)] <- 0 # change NAs in Fo to 0
  observed$TotSpcsHaulDur[is.na(observed$TotSpcsHaulDur)] <- 0 # change NAs in Fo to 0
  observed$Fo.Dur[is.na(observed$Fo.Dur)] <- 0 # change NAs in Fo to 0
  
  # Total recorded observations 
  TotalObs <- observed %>%
    group_by(Year) %>%
    summarise(YrTotObs = sum(Fo),
              YrTotObs.Dur = sum(Fo.Dur))
  TotalObs <- left_join(observed, TotalObs, by = "Year")
  
  # What proportion of the total surveyd area do each rectangle take up?
  # Assumming equal areas of rectangles, by the total number of rectangles
  AreaPerRect <- s1 %>%
    select(Year, StatRec) %>%
    group_by(Year) %>%
    summarise(N.rects = length(unique(StatRec))) %>%
    mutate(Area = 1/N.rects) # Proportion area of each rectangle 
  
  # Fe: the expected dsitribution of catches if total catch were evenly
  # distributed across surveyd rectangles 
  # Fe = total recorded obs x area -- (for equal areas)
  # Fo-Fe = the absolte difference between Fo and Fe
  fefo <- left_join(TotalObs, AreaPerRect, by = "Year") %>%
    mutate(Fe = YrTotObs*Area,
           Fe.Dur = (YrTotObs.Dur)*Area,
           `fo-fe` = abs(Fo - Fe),
           `fo-fe.dur` = abs(Fo.Dur - Fe.Dur))
  
  # sum of fo-fe
  spi1 <- fefo %>%
    group_by(Year) %>%
    summarise(sumFoFe = sum(`fo-fe`),
              sumFoFe.dur = sum(`fo-fe.dur`))
  spi2 <- left_join(distinct(TotalObs[,c("Year", "YrTotObs", "YrTotObs.Dur")]), spi1, by = "Year")
  
  # fe min - the expeted density in the smallest zone
  # all zones equal for ICES rectangles so just any Fe value
  femin <- fefo %>%
    group_by(Year) %>%
    select(Year, Fe, Fe.Dur) %>%
    summarise(Femin = min(Fe),
              Femin.dur = min(Fe.Dur))
  spi2 <- left_join(spi2, femin, by = "Year")
  
  # SPI
  spi3 <- spi2 %>%
    mutate(Denom = 2*(YrTotObs - Femin),
           Denom.dur = 2*(YrTotObs.Dur - Femin.dur),
           SPI = 1-sumFoFe/Denom,
           SPI.dur = 1-sumFoFe.dur/Denom.dur) %>%
    select(Year, SPI, SPI.dur) %>%
    distinct() %>%
    mutate(Quarter = paste(sort(unique(s1$Quarter)), collapse = ", ")) %>%
    relocate(Year, Quarter)
  
  return(spi3)
}



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
pa_rect <- function(hlhh, yrs, qrs, species_aphia, stk_divs){
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> PosArea_Rect >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
  #> The proportion of ICES rectangles where species were fond at least once.
  #> Calculated per year to give annual time series. Value of 1 indicates that 
  #> all rectangles were occupied. Value of 0 indicates that no rectangles were
  #> occupied. Indicator is bounded by 0 and 1
  #> The proportion of rectangles with species presence. A measure of spatial 
  #> occupancy. PA uses binary presence-absence data. Species considered present if 
  #> number of target species caught in a rectangle > 0. 
  #> Indicator on scale 0-1. High values = greater occupancy:
  #> 1 = species present in all rectangles. 0 = species not found in any rectangles    
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
  yrs <- as.numeric(yrs)
  qrs <- as.numeric(qrs)
  ### Filter data
  n.rects2 <- hlhh %>%
    filter(Area_27 %in% stk_divs, 
           Year %in% c(yrs),
           Quarter %in% c(qrs),
           HaulVal != "I")# remove invalid hauls
  
  # Check what data is available
  if(!identical(as.numeric(unique(n.rects2$Quarter)), qrs)){
    warning(paste0("Data not found in all survey quarters provided. 
", "Only data for quarters ", paste(sort(unique(n.rects2$Quarter)), collapse = ", ")), " available in ", unique(hlhh$Survey.x), " survey data (years ", min(yrs), ":", max(yrs), "), region(s) ", paste0(stk_divs, collapse = ", "), ".
", "No data for quarter ", paste(c(setdiff(qrs, unique(n.rects2$Quarter)), setdiff(unique(n.rects2$Quarter), qrs)), collapse = ", "), "\n")
  }
  if(!identical(as.numeric(sort(unique(n.rects2$Year))), yrs)){
    warning(paste0("Species not found in all survey years provided. 
", "No data for years ", paste(c(setdiff(yrs, unique(n.rects2$Year)), setdiff(unique(n.rects2$Year), yrs)), collapse = ", "), " in ", unique(hlhh$Survey.x), " survey data (quarters ", paste(sort(unique(n.rects2$Quarter)), collapse = ", "), "), region(s) ", paste0(stk_divs, collapse = ", "), ".\n"))
  }
  
  ### How many rectangles sampled per year?
  n.rects <- n.rects2 %>%
    group_by(Year) %>%
    summarise(nrects = length(unique(StatRec))) %>%
    mutate(Quarter = paste(sort(unique(n.rects2$Quarter)), collapse = ", "))
  
  ### How many rectangles sampled per year where species were present?
  p.rects <- n.rects2 %>%
    filter(Valid_Aphia == species_aphia) %>% # remove invalid hauls
    group_by(Year) %>%
    filter() %>%
    summarise(nrects_p = length(unique(StatRec))) %>%
    mutate(Quarter = paste(sort(unique(n.rects2$Quarter)), collapse = ", "))
  
  ### Merge but keep years where no species were found
  np.rects <- left_join(n.rects, p.rects, by = c("Year", "Quarter"))
  np.rects$nrects_p[is.na(np.rects$nrects_p)] <- 0 # change NA to 0 
  np.rects$Year <- as.numeric(as.character(np.rects$Year))
  np.rects$PosAreaR <- np.rects$nrects_p/np.rects$nrects
  
  ### Return
  return(np.rects)
}



pa_haul <- function(hlhh, yrs, qrs, species_aphia, stk_divs){
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> PosArea_Haul >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
  #> The proportion of hauls with species presence. An implicit measure of spatial 
  #> occupancy. PA uses binary presence-absence data. Species considered present if 
  #> number of target species caught in a haul > 0. 
  #> Indicator on scale 0-1. High values = greater occupancy:
  #> 1 = species present in all hauls. 0 = species not found in any hauls                                                              
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
  yrs <- as.numeric(yrs)
  qrs <- as.numeric(qrs)
  ### Filter data
  n.hauls2 <- hlhh %>% 
    filter(Area_27 %in% stk_divs,
           Year %in% c(yrs),
           Quarter %in% c(qrs),
           HaulVal != "I") # remove invalid hauls
  
  # Check what data is available
  if(!identical(as.numeric(unique(n.hauls2$Quarter)), qrs)){
    warning(paste0("Data not found in all survey quarters provided. 
", "Only data for quarters ", paste(sort(unique(n.hauls2$Quarter)), collapse = ", ")), " available in ", unique(hlhh$Survey.x), " survey data (years ", min(yrs), ":", max(yrs), "), region(s) ", paste0(stk_divs, collapse = ", "), ".
", "No data for quarter ", paste(c(setdiff(qrs, unique(n.hauls2$Quarter)), setdiff(unique(n.hauls2$Quarter), qrs)), collapse = ", "), "\n", immediate. = TRUE)
  }
  if(!identical(as.numeric(sort(unique(n.hauls2$Year))), yrs)){
    warning(paste0("Species not found in all survey years provided. 
", "No data for years ", paste(c(setdiff(yrs, unique(n.hauls2$Year)), setdiff(unique(n.hauls2$Year), yrs)), collapse = ", "), " in ", unique(hlhh$Survey.x), " survey data (quarters ", paste(sort(unique(n.hauls2$Quarter)), collapse = ", "), "), region(s) ", paste0(stk_divs, collapse = ", "), ".\n"), immediate. = TRUE)
  }
  
  ### How many hauls in each year?
  n.hauls <- n.hauls2 %>%
    group_by(Year) %>%
    summarise(no_haul.ids = length(unique(haul.id))) %>%
    mutate(Quarter = paste(sort(unique(n.hauls2$Quarter)), collapse = ", "))
  
  ### How many hauls in each year where species were present?
  p.hauls <- n.hauls2 %>% 
    filter(Valid_Aphia == species_aphia) %>% # remove invalid hauls
    group_by(Year) %>%
    summarise(pr_hauls = length(unique(haul.id))) %>%
    mutate(Quarter = paste(sort(unique(n.hauls2$Quarter)), collapse = ", "))
  
  ### Merge but keep years where no species were found
  np.hauls <- left_join(n.hauls, p.hauls, by = c("Year", "Quarter"))
  np.hauls$pr_hauls[is.na(np.hauls$pr_hauls)] <- 0 # change NA to 0 
  np.hauls$Year <- as.numeric(as.character(np.hauls$Year))
  np.hauls$PosAreaH <- np.hauls$pr_hauls/np.hauls$no_haul.ids
  
  ### return
  return(np.hauls)
}


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
lorenz_data <- function(hlhh, yrs, qrs, species_aphia, stk_divs){
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Create Lorenz Data >>>>>>>>>>>>>>>>>>>>>>>>>>>#
  #> To calculate Gini Index and D95 we must calculate data needed to form a 
  #> Lorenz curve. Then we can plot this curve and derive our spatial indicators
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
  yrs <- as.numeric(yrs)
  qrs <- as.numeric(qrs)
  ### Get all hauls sampled in the given years, quarters, and stock divisions
  allspcs_lor2 <- hlhh %>%
    filter(Area_27 %in% stk_divs,
           Year %in% c(yrs),
           Quarter %in% c(qrs),
           HaulVal != "I") # remove invalid hauls
  # Checks
  if(!identical(as.numeric(unique(allspcs_lor2$Quarter)), qrs)){
    warning(paste0("Data not found in all survey quarters provided. 
", "Only data for quarters ", paste(sort(unique(allspcs_lor2$Quarter)), collapse = ", ")), " available in ", unique(hlhh$Survey.x), " survey data (years ", min(yrs), ":", max(yrs), "), region(s) ", paste0(stk_divs, collapse = ", "), ".
", "No data for quarter ", paste(c(setdiff(qrs, unique(allspcs_lor2$Quarter)), setdiff(unique(allspcs_lor2$Quarter), qrs)), collapse = ", "), "\n", immediate. = TRUE)
  }
  if(!identical(as.numeric(sort(unique(allspcs_lor2$Year))), yrs)){
    warning(paste0("Species not found in all survey years provided. 
", "No data for years ", paste(c(setdiff(yrs, unique(allspcs_lor2$Year)), setdiff(unique(allspcs_lor2$Year), yrs)), ", "), " in ", unique(hlhh$Survey.x), " survey data (quarters ", paste(sort(unique(allspcs_lor2$Quarter)), collapse = ", "), "), region(s) ", paste0(stk_divs, collapse = ", "), ".\n"), immediate. = TRUE)
  }
  
  allspcs_lor <- allspcs_lor2 %>%
    select(haul.id, Year, Quarter, StatRec, HaulDur) %>%
    distinct() %>%
    na.omit() %>%
    # Create some artificial variables. We want to include all hauls conducted, 
    # not just the ones where target species were found
    mutate(TotalNo = as.numeric(0), # add 0 TotalNo 
           Valid_Aphia = species_aphia) %>% # add species aphia to all data
    select(haul.id, Valid_Aphia, Year, Quarter, StatRec, TotalNo, HaulDur) # rearrange cols
  # Checks
  if(all(unique(allspcs_lor$Year), unique(hlhh$Year)) == FALSE){
    warning("Not all survey years were retained in `allspcs_lor`\n", immediate. = TRUE)
  }
  if(any(duplicated(allspcs_lor$haul.id))==TRUE){
    warning("Some rows of haul.id are duplicated in `allspcs_lor`\n", immediate. = TRUE)
  }
  
  ### Filter to hauls where our target species were found 
  spcs_lor2 <- allspcs_lor2 %>%
    filter(Valid_Aphia == species_aphia)
  
  # Restrict dataset to what we need   
  spcs_lor <- spcs_lor2 %>%
    select(haul.id, Valid_Aphia, Year, Quarter, StatRec, TotalNo, HaulDur) %>%
    distinct() %>%
    na.omit() %>%
    mutate(TotalNo = as.numeric(TotalNo)) %>% #, 
    #Quarter = paste(sort(unique(spcs_lor2$Quarter)), collapse = ", ")) %>%
    relocate(haul.id, Valid_Aphia, Year, Quarter, StatRec, TotalNo, HaulDur)
  
  
  if(janitor::compare_df_cols_same(allspcs_lor, spcs_lor)==FALSE){
    warning("Columns in `allspcs_lor` and `spcs_lor` do not match.\n", immediate. = TRUE)
  }
  # Join the zero dataset to the presence dataset, but only the combinations of
  # haul.id, year, quarter, and statrec that are not already in the present dataset
  # essentially, this creates a full dataframe of all survey hauls, both where 
  # target species were found and not found. 
  # bind hauls where species were not found to data where species present data
  # hauls where species were not found will now have the haul.id with TotalNo = 0
  spcsmissing <- anti_join(allspcs_lor[1:4], spcs_lor) # the rows not in the species dataset
  spcsmissing <- filter(allspcs_lor, allspcs_lor$haul.id %in% spcsmissing$haul.id == TRUE)
  
  lorenz <- bind_rows(spcs_lor, spcsmissing) %>%
    #select(-Quarter, -haul.id) %>%
    distinct() %>%
    arrange(Year) %>%
    mutate(TotalNo_Dur = TotalNo/HaulDur) %>% # standardise by haul duration
    arrange(Year, TotalNo_Dur) %>% # order ascending
    group_by(Year) %>%
    mutate(CumSum = cumsum(TotalNo_Dur),
           rect_num = row_number(),
           cumsum_prop = CumSum/max(CumSum),
           rect_num_prop = row_number()/max(row_number()))
  lorenz$Year <- as.numeric(as.character(lorenz$Year))
  
  if(suppressWarnings(all(unique(lorenz$Year), unique(hlhh$Year)) == FALSE)){
    warning("Not all survey years were retained in `lorenz`.\n", immediate. = TRUE)
  }
  return(lorenz)
}

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
lorenz_plot <- function(lorenz_data){
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Plot Lorenz Curve >>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
  #> Feed the output of lorenz_data into this function to get lorenz curves 
  #> for each survey year
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
  lplot <- ggplot(data = lorenz_data, aes(x = rect_num_prop, y = cumsum_prop)) + 
    geom_line(aes(group = Year, colour = Year), alpha = 0.3, linewidth = 2) +
    geom_abline() +
    geom_vline(xintercept = 0.95, colour = "black", linewidth = 0.5) +
    geom_hline(yintercept = 0.95, colour = "black", linewidth = 0.5) +
    
    coord_cartesian(ylim= c(0,1), xlim = c(0,1), expand = FALSE) +
    labs(title = paste0("Lorenz Curve: Distribution of ", species, " (", min(lorenz_data$Year),":", max(lorenz_data$Year), ", Q", paste(sort(unique(lorenz_data$Quarter)), collapse = ", "), ")"), 
         subtitle = paste0(
           "Where vertical line intercepts Lorenz curve = Proportion of population observed within 95% of rectangles (D95 population).",  
           "\nWhere horizontal line intercepts Lorenz curve = Proportion of survey area occupied by 95% of the population (D95 area).",
           "\nGini index = area between indentity function and Lorenz curve.",
           "\nSpreading area = area below Lorenz curve."),
         x = "Culmuative Proportion of Rectangles Sampled", 
         y = "Culmuative Proportion of Catch") +
    theme(axis.line.x = element_line(colour = 'black', linetype='solid'),
          axis.line.y = element_line(colour = 'black', linetype='solid'),
          plot.title = element_text(size = 10),
          plot.subtitle = element_text(size = 8))+
    scale_colour_gradientn(colours = rainbow(3), name = "Year") +
    guides(alpha = "none")
  
  return(lplot)
}

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
Gini <- function(lorenz){
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Gini Index >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
  #> Gini Index is the area between the Lorenz curve and the linear "line of 
  #> equality". Typically a high score means that there is high inequality. In 
  #> other words, only a few ICES rectangles are repsonsible for the majority
  #> of the total catch. On the other hand, low values indicate that the total 
  #> catch is spread across ICES rectangles equally. 
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
  G <- lorenz %>%
    group_by(Year) %>%
    summarise('Gini Index' = ineq(TotalNo_Dur, type = "Gini")) %>%
    mutate(Quarter = paste(sort(unique(lorenz$Quarter)), collapse = ", ")) %>%
    relocate(Year, Quarter)
  G$'Gini Index' <- 1- G$'Gini Index' # take inverse, higher = more distributed
  #G$'Gini Index'[is.nan(G$'Gini Index')] <- 0 # change NaNs to 0
  G$Year <- as.numeric(as.character(G$Year))
  return(G)
}

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
d95 <- function(lorenz, level = 0.95, type = "population"){
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> D95 >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
  #> lorenz = lorenz data produced lorenz_data()
  #> level = the proportion used to calculate D95. 
  #>         The threshold set on the x (for area) or y (for population) axis that intercepts with the Lorenz curve.
  #> type = 'population' or 'area'. 
  #>>        D95 with 'population' gives an estimate of the proprtoion of the population recorded in x% of ICES rectangles; x = level.
  #>>        D95 with 'area' gives an estimate of the proprotion of ICES rectangles survyed that contain x% of the population; x = level.
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
  if(!level > 0 | !level < 1){
    stop("Argument 'level' must be between 0 and 1. Default = 0.95.")
  }
  
  D95_d <- data.frame()
  for(yr in unique(lorenz$Year)){
    g <- filter(lorenz, Year == yr)
    # Get the point where the vertical line from the xaxis at 0.95
    # intersects the Lorenz curve
    if(any(is.na(g$cumsum_prop))){
      D95 <- NA
    } else if(type == "area"){
      intrsct <- approxfun(g$cumsum_prop, g$rect_num_prop)
      D95 <- intrsct(level)
    } else if(type == "population"){
      intrsct <- approxfun(g$rect_num_prop, g$cumsum_prop)
      D95 <- intrsct(level)
    } else{stop("Argument 'type' must be set to 'area' or 'population'. Default = 'population'.")}
    
    output <- cbind(yr, D95)
    D95_d <- rbind(D95_d, output)
    
  }
  D95_d <- D95_d %>% 
    rename("Year" = yr) %>%
    mutate(Quarter = paste(sort(unique(lorenz$Quarter)), collapse = ", ")) %>%
    relocate(Year, Quarter, D95)
  if(type == "population"){
    writeLines(paste0("D95 is an estimate of the proportion of the population that exists in ", level*100, "% of surveyed rectangles."))
  }
  if(type == "area"){
    writeLines(paste0("D95 is an estimate, as a proportion, of the surveyed area that ", level*100, "% of the population occupy."))
  }
  return(D95_d)
}

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
spreadingarea_data <- function(hlhh, yrs, qrs, species_aphia, stk_divs){
  #>>>>>>>>>>>>>>>>>>>>>>>>> Spreading Area Data Prep >>>>>>>>>>>>>>>>>>>>>>>>>>>#
  #> Function to get the total number of catches divided by the haul duration
  #> for each haul
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
  yrs <- as.numeric(yrs)
  qrs <- as.numeric(qrs)
  sa2 <- hlhh %>%
    filter(Area_27 %in% stk_divs,
           Year %in% c(yrs),
           Quarter %in% c(qrs),
           HaulVal != "I") # remove invalid hauls
  
  # Check what data is available
  if(!identical(as.numeric(unique(sa2$Quarter)), qrs)){
    warning(paste0("Data not found in all survey quarters provided. 
", "Only data for quarters ", paste(sort(unique(sa2$Quarter)), collapse = ", ")), " available in ", unique(hlhh$Survey.x), " survey data (years ", min(yrs), ":", max(yrs), "), region(s) ", paste0(stk_divs, collapse = ", "), ".
", "No data for quarter ", paste(c(setdiff(qrs, unique(sa2$Quarter)), setdiff(unique(sa2$Quarter), qrs)), collapse = ", "), "\n", immediate. = TRUE)
  }
  if(!identical(as.numeric(sort(unique(sa2$Year))), yrs)){
    warning(paste0("Species not found in all survey years provided. 
", "No data for years ", paste(c(setdiff(yrs, unique(sa2$Year)), setdiff(unique(sa2$Year), yrs)), collapse = ", "), " in ", unique(hlhh$Survey.x), " survey data (quarters ", paste(sort(unique(sa2$Quarter)), collapse = ", "), "), region(s) ", paste0(stk_divs, collapse = ", "), ".\n"), immediate. = TRUE)
  }
  
  sa <- sa2 %>%
    select(haul.id, Valid_Aphia, Year, StatRec, TotalNo, HaulDur) %>%
    mutate(TotalNo = as.numeric(TotalNo), 
           Quarter = paste(sort(unique(sa2$Quarter)), collapse = ", ")) %>%
    filter(Valid_Aphia == species_aphia) %>%
    relocate(haul.id, Valid_Aphia, Year, Quarter, StatRec, TotalNo, HaulDur) %>%
    distinct() %>%
    na.omit() %>%
    mutate(TotalNo_Dur = TotalNo/HaulDur) %>% #standardise by haul duration
    arrange(Year, desc(TotalNo_Dur))
  return(sa)
}

# old code ---  
#  sa <- sa2 %>%
#    arrange(Year) %>%
#    mutate(TotalNo_Dur = TotalNo/HaulDur) %>% # standardise by haul duration
#    arrange(Year, desc(TotalNo_Dur)) %>% # order asce
#    group_by(Year) %>%
#    mutate("Q(T)" = cumsum(TotalNo_Dur),
#           "T" = row_number(),
#           "Tprop" = row_number()/max(row_number()),
#           "Q" = sum(TotalNo_Dur),
#           "(Q-Q(T)/Q" = (Q-`Q(T)`)/Q,
#           "n" = length(Q))
# old code ---  


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
spreadingarea_calc <- function(z, w = NA, plot = F){
  #>>>>>>>>>>>>>>>>>>>>>>>>> Spreading Area Calculation >>>>>>>>>>>>>>>>>>>>>>>>>#
  #> Routine from EU program Fisboat, DG-Fish, STREP n° 502572
  #> Authors : M.Woillez and J.Rivoirard (Mines-ParisTech)
  #> Last update : 01 march 2008 
  #>
  #> Arguments:
  #> z     variable of interest (i.e. fish density)
  #> w     appropriate areas of influence set as weighted factors
  #> plot  if TRUE, the curve expressing (Q-Q(T))/Q as a function of T is 
  #>       plotted with T the cumulated area occupied by the density values,
  #>       ranked in decreasing order, Q(T) the corresponding cumulated abundance,
  #>       and Q the overall abundance. The spreading area SA (expressed in square
  #>       nautical miles) is then simply defined as twice the area below this 
  #>       curve
  #>       
  #>                              !!!NOT MY CODE!!!
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
  # extract data
  nb <-length(z)
  
  # sort data in increasing order
  #zi <- sort(z,index.return=T)
  z <- sort(z)
  
  if(is.na(w)){
    w <- rep(1, length(z))
  } else{
    w <- w[order(sort(z))]
  }
  
  # computation of the spreading area 
  Q <- sum(z*w)
  QT <- c(0,cumsum(z*w))
  SA <- sum((QT[1:nb]+QT[2:(nb+1)])*w)/Q
  
  # computation of (Q-Q(T))/Q as a function of T
  fT <- c(0,cumsum(w))
  fT <- fT[nb+1] - fT
  fT <- rev(fT)
  Tprop <- fT/max(fT)
  QT <- QT[nb+1] - QT
  QT <- rev(QT)
  
  # display
  if(plot)
    plot(fT, (Q-QT)/Q, main="Curve (Q-Q(T))/Q", type="o", pch="+")
  
  
  x <- fT/sum(w)
  y <- QT
  id <- order(x)
  auc <- sum(diff(x[id])*rollmean(y[id],2))
  
  # outputs
  return(SA)
}


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
equivalentarea <- function(z, w = 1){
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Equivalent Area >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
  #>                                                             
  #> Routine from EU program Fisboat, DG-Fish, STREP n° 502572
  #> Authors : M.Woillez and J.Rivoirard (Mines-ParisTech)
  #> Last update : 01 march 2008 
  #>
  #> Arguments:
  #> z     variable of interest (i.e. fish density)
  #> w     appropriate areas of influence set as weighted factors
  #>
  #>                              !!!NOT MY CODE!!!
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
  EA <- sum(z*w,na.rm=T)^2 / sum(z^2*w,na.rm=T)
  return(EA)
}


## for me, the areas of influence accounts for biased smapling, i.e. if some rectangles are sampled more than others.
## The AOI is then used as a weighting factor
## But i have already resolved this by dividing catch by haul duration 
## However i have not resolved this for the binary indicators



# Load the grDevices and rgeos packages
require(grDevices)
require(rgeomstats)
require(sp)
require(sf)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
chullarea <- function(hlhh, yrs, qrs, species_aphia, stk_divs){
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>> Convex Hull Area >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#                                                                          
  #> Creates a polygon (the convex hull) around data points and then 
  #> calculates the area of this polygon. This is an index of range extent        
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
  
  yrs <- as.numeric(yrs)
  qrs <- as.numeric(qrs)
  chull_data2 <- hlhh %>%
    filter(Area_27 %in% stk_divs,
           Year %in% c(yrs),
           Quarter %in% c(qrs),
           HaulVal != "I")
  
  # Check what data is available
  if(!identical(as.numeric(unique(chull_data2$Quarter)), qrs)){
    warning(paste0("Data not found in all survey quarters provided. 
", "Only data for quarters ", paste(sort(unique(chull_data2$Quarter)), collapse = ", ")), " available in ", unique(hlhh$Survey.x), " survey data (years ", min(yrs), ":", max(yrs), "), region(s) ", paste0(stk_divs, collapse = ", "), ".
", "No data for quarter ", paste(c(setdiff(qrs, unique(chull_data2$Quarter)), setdiff(unique(chull_data2$Quarter), qrs)), collapse = ", "), "\n", immediate. = TRUE)
  }
  if(!identical(as.numeric(sort(unique(chull_data2$Year))), yrs)){
    warning(paste0("Species not found in all survey years provided. 
", "No data for years ", paste(c(setdiff(yrs, unique(chull_data2$Year)), setdiff(unique(chull_data2$Year), yrs)), collapse = ", "), " in ", unique(hlhh$Survey.x), " survey data (quarters ", paste(sort(unique(chull_data2$Quarter)), collapse = ", "), "), region(s) ", paste0(stk_divs, collapse = ", "), ".\n"), immediate. = TRUE)
  }
  
  chull_data <- chull_data2 %>%
    filter(Valid_Aphia == species_aphia) 
  
  # get world map
  world <- map_data("world")
  worldsf <- sfheaders::sf_polygon(
    obj = world
    , x = "long"
    , y = "lat"
    , polygon_id = "group"
  ) 
  
  chull <- data.frame()
  # Pseudo-dataset (latitude and longitude coordinates)
  for(yr in yrs){
    chull_yearly <- chull_data %>%
      filter(Year == yr)
    longitude <- chull_yearly$ShootLong
    latitude <- chull_yearly$ShootLat
    
    # Combine latitude and longitude into a matrix
    coordinates <- cbind(longitude, latitude)
    # Geometries - closed shapes must have at least 4 rows
    if(nrow(coordinates) <= 4){
      convex_hull_area <- 0
      areaoccupied <- 0
    } else{
      # Calculate the convex hull
      convex_hull <- chull(coordinates)
      # Extract the convex hull points
      convex_hull_points <- as.data.frame(coordinates[convex_hull, ])
      
      # Create a SpatialPolygons object from the convex hull points
      convex_hull_sf <- sfheaders::sf_polygon(
        obj = convex_hull_points
        , x = "longitude"
        , y = "latitude") 
      # Calculate area of the convex hull
      convex_hull_area <- st_area(convex_hull_sf)
      
      # get area of convex hull minus area of intersecting land
      land <- st_intersection(convex_hull_sf, worldsf)
      landarea <- sum(st_area(land))
      areaoccupied <- convex_hull_area - landarea
    }
    outputs <- cbind(yr, convex_hull_area, areaoccupied)
    chull <- rbind(chull, outputs)
  }
  # currently scaled by diising by max value. 
  # But could be scaled by dividing by the area of the whole stock region
  # i.e. the true possible maximum. That way if the indictaor = 1 then know that 
  # the stock is dispersed across the whole region. 
  StkArea <- ices_rect %>% 
    filter(Area_27 %in% stk_divs)
  StkArea <- length(unique(StkArea$ICESNAME))*0.5
  
  chull <- chull %>% 
    mutate(convex_hull_area_scaled = convex_hull_area/StkArea,
           areaoccupied_scaled = areaoccupied/StkArea,
           Quarter = paste(sort(unique(chull_data2$Quarter)), collapse = ", ")) %>%
    relocate(yr, Quarter) %>%
    rename("Year" = yr)
  return(chull)
}

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
ellarea <- function(hlhh, yrs, qrs, species_aphia, stk_divs){
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Ellipse Area >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#                                                                          
  #> The are of the ellipse that encompasses 95% of data points where species were
  #> present.
  #> Simple indicator. Does not weight by density. An indicator of dispersion.
  #> Ellipse area can be visualised using mapdis()
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
  
  yrs <- as.numeric(yrs)
  qrs <- as.numeric(qrs)
  d2 <- hlhh %>%
    filter(Area_27 %in% stk_divs,
           Year %in% c(yrs),
           Quarter %in% c(qrs),
           HaulVal != "I", 
           Valid_Aphia == species_aphia) %>%
    distinct()
  
  # Check what data is available
  if(!identical(as.numeric(unique(d2$Quarter)), qrs)){
    warning(paste0("Data not found in all survey quarters provided. 
    ", "Only data for quarters ", paste(sort(unique(d2$Quarter)), collapse = ", ")), " available in ", unique(hlhh$Survey.x), " survey data (years ", min(yrs), ":", max(yrs), "), region(s) ", paste0(stk_divs, collapse = ", "), ".
    ", "No data for quarter ", paste(c(setdiff(qrs, unique(d2$Quarter)), setdiff(unique(d2$Quarter), qrs)), collapse = ", "), "\n", immediate. = TRUE)
  }
  if(!identical(as.numeric(sort(unique(d2$Year))), yrs)){
    warning(paste0("Species not found in all survey years provided. 
    ", "No data for years ", paste(c(setdiff(yrs, unique(d2$Year)), setdiff(unique(d2$Year), yrs)), collapse = ", "), " in ", unique(hlhh$Survey.x), " survey data (quarters ", paste(sort(unique(d2$Quarter)), collapse = ", "), "), region(s) ", paste0(stk_divs, collapse = ", "), ".\n"), immediate. = TRUE)
  }
  
  df <- data.frame()
  for(yr in yrs){
    data <- data.frame(x = d2[d2$Year == yr,]$ShootLong, 
                       y = d2[d2$Year == yr,]$ShootLat)
    if(nrow(data) < 2){
      ela = NA
    } else{
      p <- ggplot(data, aes(x = x, y = y)) +
        geom_point() +
        stat_ellipse(type = "t")
      
      pb <- ggplot_build(p)
      if(nrow(pb$data[[1]]) == 0){
        ela <- 0
      } else{
        el <- pb$data[[2]][c("x", "y")]
        ctr <- MASS::cov.trob(data)$center #updated previous answer here
        dist2center <- sqrt(rowSums((t(t(el) - ctr))^2))
        ela <- pi * min(dist2center) * max(dist2center)
      }
    }
    output <- cbind("Year" = yr, "Ellipse Area" = ela)
    df <- rbind(df, output)
  }
  df <- df %>%
    mutate(Quarter = paste(sort(unique(hlhh$Quarter)), collapse = ", ")) %>%
    relocate(Year, Quarter,`Ellipse Area`)
  return(df)
}

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
cog <- function(x, y, z = 1, plot = FALSE, lonlat2km = FALSE, km2lonlat = FALSE){
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>> Centre of Gravity >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
  #> Defintion: The mean location of the population
  #> 
  #> cog() function allows for xy cordinates to be weighted by density, and for 
  #> transformation of xy cordinates between km and lonlat
  #> z = density (not depth...yes confusing)
  #> z = 1: each coordnoate equally weighted i.e. ignores density
  #> Pending...: Add functionality to calculate and plot mean depth
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
  if(length(z) == 1){
    z <- rep(z, length(x))
  }
  # Density weighted CoG, 1 = equal weighting
  cog_x <- sum(x*z)/sum(z)
  cog_y <- sum(y*z)/sum(z)
  cog_xy <- cbind(cog_x, cog_y)
  # Data for plotting
  d <- cbind(x,y,z)
  
  # Convert lons and lats to km 
  if(lonlat2km){
    d <- cbind(SpatialEpi::latlong2grid(d), z)
    cog_xy <- SpatialEpi::latlong2grid(cog_xy)
    colnames(cog_xy) <- c("cog_x", "cog_y")
    rownames(cog_xy) <- NULL
  } 
  # Convert kms to lon and lat
  if(km2lonlat){
    d <- cbind(SpatialEpi::grid2latlong(d), z)
    cog_xy <- SpatialEpi::grid2latlong(cog_xy)
    colnames(cog_xy) <- c("cog_x", "cog_y")
    rownames(cog_xy) <- NULL
  }
  
  d <- as.data.frame(d)
  cog_xy <- as.data.frame(cog_xy)
  
  # Plot
  if(plot){
    if(length(unique(z))==1){
      a = NULL
    } else{
      d$a <- d$z
    }
    print(d)
    p <- ggplot() +
      geom_point(data = d, aes(x,y, colour = a), size = z) +
      geom_point(data = cog_xy, aes(cog_x, cog_y), colour = "blue", shape = 15, size = 2) +
      geom_text(data = cog_xy, aes(cog_x, cog_y-(cog_y*0.02), label = "CoG")) +
      paletteer::scale_colour_paletteer_c("grDevices::Geyser") +
      labs(colour = "z")
    print(p)
  }
  
  return(cog_xy)
}

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
inertia <- function(x, y, z = 1, plot = FALSE, lonlat2km = FALSE, km2lonlat = FALSE, rand = FALSE){
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Inertia >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
  #> Defintion: The mean square variation around CoG. Inertia is a measure of 
  #> range. Higher values indicate the biomass of the population is further spreading
  #> Like CoG, inertia can be calculated using density (z) as a weighting factor. 
  #> Set rand == T to generate outputs with random data (if no x and y data supplied)
  #> xy coordinates can be transformed between km and lon/lat
  #> 
  #> IMPORTANT: code for PCA is taken from:
  #> https://github.com/fate-spatialindicators/spatialindicators/blob/master/Spatial_indicators_functions_Woillez2009.r
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
  # Use rnorm generated data
  if(missing(x) & missing(y) & rand == T){
    x <- rnorm(50)
    y <- rnorm(50)
    z <- abs(rnorm(50)^2)
  }
  # For equal weighting of xy points
  if(length(z) == 1){
    z <- rep(z, length(x))
  }
  # Measurement transformation
  if(lonlat2km){
    xy <- SpatialEpi::latlong2grid(cbind(x,y))
    d <- cbind(xy,z)
  } else if(km2lonlat){
    xy <- SpatialEpi::grid2latlong(cbind(x,y))
    d <- cbind(xy,z)
  } else{
    d <- as.data.frame(cbind(x,y,z))
  }
  # Density weighted CoG
  cog_x <- sum(d$x*d$z)/sum(d$z)
  cog_y <- sum(d$y*d$z)/sum(d$z)
  cog_xy <- as.data.frame(cbind(cog_x, cog_y))
  # Inertia
  dx <- d$x - cog_x
  dy <- d$y - cog_y
  ix <- sum((dx^2)*d$z)/sum(d$z)
  iy <- sum((dy^2)*d$z)/sum(d$z)
  inert <- ix+iy
  # Weighted PCA
  M11 <- sum(dx^2*d$z)
  M22 <- sum(dy^2*d$z)
  M21 <- sum(dx*dy*d$z)
  M12 <- M21
  M <- matrix(c(M11, M12, M21, M22), ncol = 2)
  x1 <- eigen(M)$vectors[1, 1]
  y1 <- eigen(M)$vectors[2, 1]
  x2 <- eigen(M)$vectors[1, 2]
  y2 <- eigen(M)$vectors[2, 2]
  r1 <- eigen(M)$values[1]/(eigen(M)$values[1] + eigen(M)$values[2])
  # Principal axis coordinates
  e1 <- (y1/x1)^2
  sx1 <- x1/abs(x1)
  sy1 <- y1/abs(y1)
  sx2 <- x2/abs(x2)
  sy2 <- y2/abs(y2)
  xa <- cog_x + sx1 * sqrt((r1 * inert)/(1 + e1))
  ya <- cog_y + sy1 * sqrt((r1 * inert)/(1 + (1/e1)))
  xb <- 2 * cog_x - xa
  yb <- 2 * cog_y - ya
  xc <- cog_x + sx2 * sqrt(((1 - r1) * inert)/(1 + (1/e1)))
  yc <- cog_y + sy2 * sqrt(((1 - r1) * inert)/(1 + e1))
  xd <- 2 * cog_x - xc
  yd <- 2 * cog_y - yc
  Imax <- r1*inert 
  Imin <- (1-r1)*inert
  isotropy <- sqrt(Imin/Imax)
  
  # Plot
  if(plot){
    paletteer::scale_colour_paletteer_c("grDevices::Geyser")
    pal <- paletteer::paletteer_c("grDevices::Geyser", length(d$x))
    d <- d %>% arrange(z) %>%
      mutate(pal = pal)
    
    par(pty = "s")
    plot(d$x,d$y, cex = d$z, col = d$pal, pch = 16)
    segments(xa,ya,xb,yb, col = "blue")
    segments(xc,yc,xd,yd, col = "blue")
    points(cog_x, cog_y, col = "blue", pch = 15)
    ell <- car::dataEllipse(c(xa,xb,xc,xd),c(ya,yb,yc,yd), 
                            levels = 0.455, add = TRUE, plot.points = F, 
                            center.pch = FALSE, col = "lightblue",
                            fill = TRUE)
    points(xa,ya, col = "blue", cex = 1, pch = 16)
    points(xb,yb, col = "blue", cex = 1, pch = 16)
    points(xc,yc, col = "blue", cex = 1, pch = 16)
    points(xd,yd, col = "blue", cex = 1, pch = 16)
    text(x = median(d$x), y = max(d$y), labels = paste0("CoG: ", round(cog_x, 2), ", ", round(cog_y, 2)))
    text(x = median(d$x), y = max(d$y)*0.9, labels = paste0("Inertia: ", round(inert, 2)))
    text(x = median(d$x), y = max(d$y)*0.8, labels = paste0("Isotropy: ", round(isotropy, 2)))
  }
  
  output <- as.data.frame(cbind(cog_xy, inert, isotropy))
  return(output)
}

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
coginis <- function(hlhh, yrs, qrs, species_aphia, stk_divs, # data specifics
                    cog = T, inertia = T, iso = T,           # spatial indicators
                    plot = F, xlim = NA, ylim = NA,          # plot
                    density = F,                             # density weighted spat inds
                    lonlat2km = F, km2lonlat = F){           # conversion of lonlats to/from km
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Inertia >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
  #> This function computes three spatial indicators and is integrated with 
  #> DATRAS data format:
  #> 
  #> 1: CoG - mean location (can be weighted by density)
  #> 2: Inertia - mean variance around CoG (can be weighted by density)
  #> 3: Isotropy - The shape of Inertia. If perfect cirlce, isotropy = 1. 
  #>> The more elliptical the inertia (i.e. mean variance in one direction is 
  #>> greater than the mean variance in another orthogonal direction) the closer 
  #>> Isotropy is to zero. Gives inidication on how symetrical the variation is 
  #>> around the CoG
  #>
  #> Toggle which indicators you want reported
  #> Can transform longitiude and latitude to km and vice versa
  #> Plot also available
  #> 
  #> IMPORTANT: code for PCA is taken from:
  #> https://github.com/fate-spatialindicators/spatialindicators/blob/master/Spatial_indicators_functions_Woillez2009.r
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
  yrs <- as.numeric(yrs)
  qrs <- as.numeric(qrs)
  d2 <- hlhh %>%
    filter(Area_27 %in% stk_divs,
           Year %in% c(yrs),
           Quarter %in% c(qrs),
           HaulVal != "I")
  # Check what data is available
  if(!identical(as.numeric(unique(d2$Quarter)), qrs)){
    warning(paste0("Data not found in all survey quarters provided. 
  ", "Only data for quarters ", paste(sort(unique(d2$Quarter)), collapse = ", ")), " available in ", unique(hlhh$Survey.x), " survey data (years ", min(yrs), ":", max(yrs), "), region(s) ", paste0(stk_divs, collapse = ", "), ".
  ", "No data for quarter ", paste(c(setdiff(qrs, unique(d2$Quarter)), setdiff(unique(d2$Quarter), qrs)), collapse = ", "), "\n", immediate. = TRUE)
  }
  if(!identical(as.numeric(sort(unique(d2$Year))), yrs)){
    warning(paste0("Species not found in all survey years provided. 
  ", "No data for years ", paste(c(setdiff(yrs, unique(d2$Year)), setdiff(unique(d2$Year), yrs)), collapse = ", "), " in ", unique(hlhh$Survey.x), " survey data (quarters ", paste(sort(unique(d2$Quarter)), collapse = ", "), "), region(s) ", paste0(stk_divs, collapse = ", "), ".\n"), immediate. = TRUE)
  }
  d1 <- d2 %>% 
    filter(Valid_Aphia == species_aphia) %>%
    distinct() %>%
    mutate(TotalNo_Dur = TotalNo/HaulDur)
  
  df <- data.frame()
  
  for(yr in yrs){
    dyrly <- d1 %>%
      filter(Year == yr)
    x <- dyrly$ShootLong
    y <- dyrly$ShootLat
    # Density
    if(density == TRUE){
      z <- dyrly$TotalNo_Dur
    } else{z <- rep(1, length(x))}
    
    # Measurement transformation
    if(lonlat2km){
      xy <- SpatialEpi::latlong2grid(cbind(x,y))
      d <- cbind(xy,z)
    } else if(km2lonlat){
      xy <- SpatialEpi::grid2latlong(cbind(x,y))
      d <- cbind(xy,z)
    } else{
      d <- as.data.frame(cbind(x,y,z))
    } 
    if(nrow(d) < 1){
      cg_x <- NaN
      cg_y <- NaN
      cg_xy <- as.data.frame(cbind(cg_x, cg_y))
      Inertia <- NaN
      Isotropy <- NaN
    } else{
      # Density weighted CoG
      cg_x <- sum(d$x*d$z)/sum(d$z)
      cg_y <- sum(d$y*d$z)/sum(d$z)
      cg_xy <- as.data.frame(cbind(cg_x, cg_y))
      
      if(any(is.na(cg_xy)) & inertia == TRUE){
        Inertia <- NaN
      } else if(any(is.na(cg_xy)) & iso == TRUE){
        Isotropy = NaN
      } else{
        # Inertia
        dx <- d$x - cg_x
        dy <- d$y - cg_y
        ix <- sum((dx^2)*d$z)/sum(d$z)
        iy <- sum((dy^2)*d$z)/sum(d$z)
        Inertia <- ix+iy
        # Weighted PCA
        M11 <- sum(dx^2*d$z)
        M22 <- sum(dy^2*d$z)
        M21 <- sum(dx*dy*d$z)
        M12 <- M21
        M <- matrix(c(M11, M12, M21, M22), ncol = 2)
        x1 <- eigen(M)$vectors[1, 1]
        y1 <- eigen(M)$vectors[2, 1]
        x2 <- eigen(M)$vectors[1, 2]
        y2 <- eigen(M)$vectors[2, 2]
        r1 <- eigen(M)$values[1]/(eigen(M)$values[1] + eigen(M)$values[2])
        # Principal axis coordinates
        e1 <- (y1/x1)^2
        sx1 <- x1/abs(x1)
        sy1 <- y1/abs(y1)
        sx2 <- x2/abs(x2)
        sy2 <- y2/abs(y2)
        xa <- cg_x + sx1 * sqrt((r1 * Inertia)/(1 + e1))
        ya <- cg_y + sy1 * sqrt((r1 * Inertia)/(1 + (1/e1)))
        xb <- 2 * cg_x - xa
        yb <- 2 * cg_y - ya
        xc <- cg_x + sx2 * sqrt(((1 - r1) * Inertia)/(1 + (1/e1)))
        yc <- cg_y + sy2 * sqrt(((1 - r1) * Inertia)/(1 + e1))
        xd <- 2 * cg_x - xc
        yd <- 2 * cg_y - yc
        Imax <- r1*Inertia 
        Imin <- (1-r1)*Inertia
        Isotropy <- sqrt(Imin/Imax)
      }
      
      # Plot
      if(plot){
        if(nrow(d) < 1){
          warning("Cannot plot. No catched in survey data for year provided")
        } else{
          pal <- paletteer::paletteer_c("grDevices::Geyser", length(d$x))
          dplot <- d %>% arrange(z) %>%
            mutate(pal = pal)
          
          if(any(is.na(xlim))){
            xlim <- c(min(hlhh$ShootLong)-2, max(hlhh$ShootLong)+2)
          } else{xlim <- xlim}
          if(any(is.na(ylim))){
            ylim <- c(min(hlhh$ShootLat)-2, max(hlhh$ShootLat)+2)
          } else{ylim <- ylim}
          
          posy <- min(ylim)+2
          
          #par(pty = "s")
          map(xlim = xlim, ylim = ylim)
          points(dplot$x, dplot$y, cex = scale(dplot$z), col = scales::alpha(dplot$pal, 0.05), pch = 16, xlim = xlim, ylim = ylim)
          segments(xa,ya,xb,yb, col = "blue")
          segments(xc,yc,xd,yd, col = "blue")
          points(cg_x, cg_y, col = "blue", pch = 15)
          ell <- car::dataEllipse(c(xa,xb,xc,xd),c(ya,yb,yc,yd), 
                                  levels = 0.455, add = TRUE, plot.points = F, 
                                  center.pch = FALSE, col = "lightblue",
                                  fill = TRUE)
          points(xa,ya, col = "blue", cex = 1, pch = 16)
          points(xb,yb, col = "blue", cex = 1, pch = 16)
          points(xc,yc, col = "blue", cex = 1, pch = 16)
          points(xd,yd, col = "blue", cex = 1, pch = 16)
          
          text(x = cg_x, y = posy, labels = paste0("CoG: ", round(cg_x, 2), ", ", round(cg_y, 2)))
          text(x = cg_x, y = posy*0.995, labels = paste0("Inertia: ", round(Inertia, 2)))
          text(x = cg_x, y = posy*0.99, labels = paste0("Isotropy: ", round(Isotropy, 2)))
          title(sub = yr)
        }
      }
    }
    output <- as.data.frame(cbind(yr, cg_xy, Inertia, Isotropy))
    df <- rbind(df, output)
  }
  colnames(df)[2:3] <- c("CoG (x)", "CoG (y)")
  
  df <- df[,c(TRUE, rep(cog, 2), inertia, iso)]
  df <- df %>%
    rename("Year" = yr) %>%
    mutate(Quarter = paste(sort(unique(d2$Quarter)), collapse = ", ")) %>%
    relocate(Year, Quarter)
  return(df)
}

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>>>>>>>>>>>>>>>>>>> Centre of Gravity, Inertia, Area of Ellipse
coginert <- function(hlhh, yrs, qrs, species_aphia, stk_divs){
  yrs <- as.numeric(yrs)
  qrs <- as.numeric(qrs)
  cog_data2 <- hlhh %>%
    filter(Area_27 %in% stk_divs,
           Year %in% c(yrs),
           Quarter %in% c(qrs),
           HaulVal != "I")
  # Check what data is available
  if(!identical(as.numeric(unique(cog_data2$Quarter)), qrs)){
    warning(paste0("Data not found in all survey quarters provided. 
", "Only data for quarters ", paste(sort(unique(cog_data2$Quarter)), collapse = ", ")), " available in ", unique(hlhh$Survey.x), " survey data (years ", min(yrs), ":", max(yrs), "), region(s) ", paste0(stk_divs, collapse = ", "), ".
", "No data for quarter ", paste(c(setdiff(qrs, unique(cog_data2$Quarter)), setdiff(unique(cog_data2$Quarter), qrs)), collapse = ", "), "\n", immediate. = TRUE)
  }
  if(!identical(as.numeric(sort(unique(cog_data2$Year))), yrs)){
    warning(paste0("Species not found in all survey years provided. 
", "No data for years ", paste(c(setdiff(yrs, unique(cog_data2$Year)), setdiff(unique(cog_data2$Year), yrs)), collapse = ", "), " in ", unique(hlhh$Survey.x), " survey data (quarters ", paste(sort(unique(cog_data2$Quarter)), collapse = ", "), "), region(s) ", paste0(stk_divs, collapse = ", "), ".\n"), immediate. = TRUE)
  }
  cog_data <- cog_data2 %>% 
    filter(Valid_Aphia == species_aphia)
  
  cgi <- data.frame()
  for(yr in yrs){
    cog_yearly <- cog_data %>%
      filter(Year == yr)
    longitude <- cog_yearly$ShootLong
    latitude <- cog_yearly$ShootLat
    ### Centre of gravity
    cg_x <- mean(longitude)
    cg_y <- mean(latitude)
    ### Inertia
    inertia <- sum(abs(longitude) -cg_x)^2 + sum(abs(latitude)-cg_y)^2
    ### Area of Inertia
    # Create a 2x2 covariance matrix
    cov_matrix <- cov(cbind(longitude, latitude))
    if(any(is.na(cov_matrix))){
      area_of_ellipse <- NA
    } else{
      # Calculate the eigenvalues and eigenvectors of the covariance matrix
      eigen_result <- eigen(cov_matrix)
      eigenvalues <- eigen_result$values
      # Calculate the semi-major and semi-minor axes (2 standard deviations)
      # The factor 5.991 represents the Chi-square likelihood for the 95% confidence of the data.
      # https://www.visiondummy.com/2014/04/draw-error-ellipse-representing-covariance-matrix/
      semi_major_axis <- sqrt(5.991 * eigenvalues[1]) # 5.991 corresponds to 95% confidence interval for a 2D Gaussian distribution
      semi_minor_axis <- sqrt(5.991 * eigenvalues[2])
      # Calculate the area of the ellipse
      area_of_ellipse <- pi * semi_major_axis * semi_minor_axis
    }
    
    ### Output
    outputs <- data.frame(yr, cg_x, cg_y, inertia, area_of_ellipse)
    outputs <- outputs %>%
      relocate(yr, cg_x, cg_y, inertia) %>%
      rename("Year"= yr)
    cgi <- rbind(cgi, outputs)
  }
  cgi <- cgi %>%
    mutate(Quarter = paste(sort(unique(cog_data2$Quarter)), collapse = ", ")) %>%
    relocate(Year, Quarter)
  return(cgi)
}

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
mapdis <- function(hlhh, yrs, qrs, species_aphia, stk_divs, ices_rect,  # data specifics
                   cog = T, inertia = T, chull = T, ellipse95 = T, # spatial indicators
                   coginert_density = T,                           # weight cog and inertia
                   km2lonlat = F,                                  # convert km to lonlat
                   title = "",                                     # plot title
                   xlim = NA, ylim = NA){                          # plot bounds                                   
  #>>>>>>>>>>>>>>>>>>>>> Map Spatial Distribution Indicators >>>>>>>>>>>>>>>>>>>>#
  #> Function for visualising surveys points where species were found, convex     
  #> hull area, centre of gravity, inertia ellipse with world map. Filter hlhh    
  #> before providing it to the function
  #> 
  #> Pending...: update to toggle cog, inertia, chull, and ellipse95 on plot
  #> Pending...: plot stock division boundaries 
  #> Pending...: add spatind values on plot      
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
  
  yrs <- as.numeric(yrs)
  qrs <- as.numeric(qrs)
  d2 <- hlhh %>%
    filter(Area_27 %in% stk_divs,
           Year %in% c(yrs),
           Quarter %in% c(qrs),
           HaulVal != "I") %>%
    mutate(colour = if_else(Valid_Aphia == species_aphia, 3, NA))
  
  # Check what data is available
  if(!identical(as.numeric(unique(d2$Quarter)), qrs)){
    warning(paste0("Data not found in all survey quarters provided. 
  ", "Only data for quarters ", paste(sort(unique(d2$Quarter)), collapse = ", ")), " available in ", unique(hlhh$Survey.x), " survey data (years ", min(yrs), ":", max(yrs), "), region(s) ", paste0(stk_divs, collapse = ", "), ".
  ", "No data for quarter ", paste(c(setdiff(qrs, unique(d2$Quarter)), setdiff(unique(d2$Quarter), qrs)), collapse = ", "), "\n", immediate. = TRUE)
    qrs <- as.numeric(unique(d2$Quarter))
  }
  if(!identical(as.numeric(sort(unique(d2$Year))), yrs)){
    warning(paste0("Species not found in all survey years provided. 
  ", "No data for years ", paste(c(setdiff(yrs, unique(d2$Year)), setdiff(unique(d2$Year), yrs)), collapse = ", "), " in ", unique(hlhh$Survey.x), " survey data (quarters ", paste(sort(unique(d2$Quarter)), collapse = ", "), "), region(s) ", paste0(stk_divs, collapse = ","), ".\n"), immediate. = TRUE)
    yrs <- as.numeric(sort(unique(d2$Year)))
  }
  d1 <- d2 %>% 
    filter(Valid_Aphia == species_aphia) %>%
    distinct() %>%
    mutate(TotalNo_Dur = TotalNo/HaulDur)
  
  # worldsf 
  world <- map_data("world")
  worldsf <- sfheaders::sf_polygon(
    obj = world
    , x = "long"
    , y = "lat"
    , polygon_id = "group"
  ) 
  
  df <- data.frame()
  
  for(yr in yrs){
    dyrly <- d1 %>%
      filter(Year == yr)
    x <- dyrly$ShootLong
    y <- dyrly$ShootLat
    
    # All sites
    d3 <- d2 %>% 
      filter(Year == yr) %>% 
      select(ShootLong, ShootLat, StatRec) %>% 
      distinct()
    # Pos sites only 
    d4 <- dyrly %>%
      select(ShootLong, ShootLat, StatRec) %>% 
      distinct()
    # All Sites minus pos sites 
    d5 <- anti_join(d3,d4, by = join_by(ShootLong, ShootLat))
    # ices_rect
    ices_rect2 <- ices_rect %>% 
      mutate('ICES Rectangles' = if_else(ICESNAME %in% d4$StatRec, "3", if_else(ICESNAME %in% d2$StatRec, "2", NA)))
    
    # Density
    if(coginert_density == TRUE){
      z <- dyrly$TotalNo_Dur
    } else{z <- rep(1, length(x))}
    
    # Measurement transformation
    if(km2lonlat){
      xy <- SpatialEpi::grid2latlong(cbind(x,y))
      d <- cbind(xy,z)
    } else{
      d <- as.data.frame(cbind(x,y,z))
    }
    # Density weighted CoG
    cog_x <- sum(d$x*d$z)/sum(d$z)
    cog_y <- sum(d$y*d$z)/sum(d$z)
    cog_xy <- as.data.frame(cbind(cog_x, cog_y))
    
    if(any(is.na(cog_xy)) & inertia == TRUE){
      inert <- NaN
    } else{
      # Inertia
      dx <- d$x - cog_x
      dy <- d$y - cog_y
      ix <- sum((dx^2)*d$z)/sum(d$z)
      iy <- sum((dy^2)*d$z)/sum(d$z)
      inert <- ix+iy
      # Weighted PCA
      M11 <- sum(dx^2*d$z)
      M22 <- sum(dy^2*d$z)
      M21 <- sum(dx*dy*d$z)
      M12 <- M21
      M <- matrix(c(M11, M12, M21, M22), ncol = 2)
      x1 <- eigen(M)$vectors[1, 1]
      y1 <- eigen(M)$vectors[2, 1]
      x2 <- eigen(M)$vectors[1, 2]
      y2 <- eigen(M)$vectors[2, 2]
      r1 <- eigen(M)$values[1]/(eigen(M)$values[1] + eigen(M)$values[2])
      # Principal axis coordinates
      e1 <- (y1/x1)^2
      sx1 <- x1/abs(x1)
      sy1 <- y1/abs(y1)
      sx2 <- x2/abs(x2)
      sy2 <- y2/abs(y2)
      xa <- cog_x + sx1 * sqrt((r1 * inert)/(1 + e1))
      ya <- cog_y + sy1 * sqrt((r1 * inert)/(1 + (1/e1)))
      xb <- 2 * cog_x - xa
      yb <- 2 * cog_y - ya
      xc <- cog_x + sx2 * sqrt(((1 - r1) * inert)/(1 + (1/e1)))
      yc <- cog_y + sy2 * sqrt(((1 - r1) * inert)/(1 + e1))
      xd <- 2 * cog_x - xc
      yd <- 2 * cog_y - yc
      Imax <- r1*inert 
      Imin <- (1-r1)*inert
      isotropy <- sqrt(Imin/Imax)
    }
    # Inertia Ellipse Points
    ell <- as.data.frame(car::dataEllipse(c(xa,xb,xc,xd),c(ya,yb,yc,yd), 
                                          levels = 0.25, draw = F, add = F, robust = T))
    # Combine latitude and longitude into a matrix
    coordinates <- d[,c("x", "y")]
    # Calculate the convex hull
    convex_hull <- chull(coordinates)
    # Extract the convex hull points
    convex_hull_points <- as.data.frame(coordinates[convex_hull, ])
    
    # Create a SpatialPolygons object from the convex hull points
    convex_hull_sf <- sfheaders::sf_polygon(
      obj = convex_hull_points
      , x = "x"
      , y = "y"
    ) 
    
    # Plot area
    #xlim <- c(min(convex_hull_points[1:nrow(convex_hull_points),1]), max(convex_hull_points[1:nrow(convex_hull_points),1]))
    #ylim <- c(min(convex_hull_points[1:nrow(convex_hull_points),2]), max(convex_hull_points[1:nrow(convex_hull_points),2]))
    if(any(is.na(xlim))){
      xlim <- c(min(hlhh$ShootLong), max(hlhh$ShootLong))
    } else{xlim <- xlim}
    if(any(is.na(ylim))){
      ylim <- c(min(hlhh$ShootLat), max(hlhh$ShootLat))
    } else{ylim <- ylim}
    
    #xlim <- c(-10, 15)
    #ylim <- c(46.5, 65)
    cord <- as.data.frame(coordinates)
    xcros <- rbind(xa,xb,xc,xd)
    ycros <- rbind(ya,yb,yc,yd)
    inertcross <- as.data.frame(cbind(xcros, ycros))
    d3 <- d2 %>%
      select(ShootLong, ShootLat, colour) %>%
      distinct()
    d <- mutate(d, 
                Hauls = "grey10",
                Density = z)
    d5 <- mutate(d5, 
                 Hauls = "grey30")
    convex_hull_sf <- mutate(convex_hull_sf,
                             `Dispersion Indicator` = "orange")
    cord <- mutate(cord, 
                   `Dispersion Indicator` = "black")
    ell <- mutate(ell,
                  `Dispersion Indicator` = "blue")
    cog_xy <- mutate(cog_xy,
                     `Location Indicator` = 15)
    
    p <- ggplot() +
      # ICES rectangles (if both present & absent available)
      geom_tile(data = ices_rect2, mapping = aes(x = stat_x, y = stat_y, fill = `ICES Rectangles`), stat = "identity", alpha = 0.05, colour = "darkgrey", show.legend = T) +
      {if(all(c(3,2) %in% unique(ices_rect2$`ICES Rectangles`))) list(
        scale_fill_identity(guide = "legend", breaks = c("3", "2"), labels = c("Present", "Absent")),
        guides(fill = guide_legend(override.aes = list(fill = c("green", "red"), 
                                                       colour = "grey10",
                                                       alpha = 0.5,
                                                       shape = 1)))
      )} +
      # if only absent rectangles
      {if(any(c(3,2) %in% unique(ices_rect2$`ICES Rectangles`) == FALSE) & any(2 %in% unique(ices_rect2$`ICES Rectangles`))) list(
        message("Species absent in all rectangles. Green 'Present' symbology in legend will be missing. Code does not currently support displaying symbology of features that are not present."),
        scale_fill_identity(guide = "legend", breaks = c("2"), labels = c("Absent")),
        guides(fill = guide_legend(override.aes = list(fill = c("red"), 
                                                       colour = "grey10",
                                                       alpha = 0.5,
                                                       shape = 1)))
      )} +
      # if only present rectangles
      {if(any(c(3,2) %in% unique(ices_rect2$`ICES Rectangles`) == FALSE) & any(3 %in% unique(ices_rect2$`ICES Rectangles`))) list(
        message("Species present in all rectangles. Red 'Absent' symbology in legend will be missing. Code does not currently support displaying symbology of features that are not present."),
        scale_fill_identity(guide = "legend", breaks = c("3"), labels = c("Present")),
        guides(fill = guide_legend(override.aes = list(fill = c("green"), 
                                                       colour = "grey10",
                                                       alpha = 0.5,
                                                       shape = 1)))
      )} +
      # World map
      geom_sf(data = worldsf, size = 0.1) + 
      # ICES Divisions
      geom_path(data = ices_divs, mapping = aes(x = long, y = lat, group = group, fill = NULL), color = "black") +
      # Convex Hull Polygon
      geom_sf(data = convex_hull_sf, aes(colour = `Dispersion Indicator`), fill = "orange", alpha = 0.1, show.legend = T) +
      # 95% CI Ellipse
      stat_ellipse(data = cord, aes(x = x, y = y, colour = `Dispersion Indicator`), type = "t") + 
      # Inertia
      stat_ellipse(data = ell, aes(x = x, y = y, colour = `Dispersion Indicator`), level = 0.945, type = "norm") +
      # Plot bounds
      coord_sf(xlim, ylim) +
      scale_colour_identity(guide = "legend", breaks = c("orange", "black", "blue"), labels = c("Convex Hull", "95% CI Ellipse", "Inertia")) +
      guides(colour = guide_legend(override.aes = list(shape = c(NA),
                                                       size = c(3),
                                                       fill = c("orange", "white", "white")))) +
      ggnewscale::new_scale_color() +
      # Hauls with species presence (size weighted by density)
      geom_point(data = d, aes(x = x, y = y, size = Density, colour = Hauls)) +
      # Haul locations where species were absent
      geom_point(data = d5, aes(x = ShootLong, y = ShootLat, colour = Hauls), shape = 4) +
      {if(nrow(d5) > 0) list(
        scale_colour_identity(guide = "legend", breaks = c("grey10", "grey30"), labels = c("Present", "Absent")),
        suppressWarnings(guides(colour = guide_legend(override.aes = list(shape = c(16, 4),
                                                                          size = c(3,3),
                                                                          colour = c("grey10", "grey30"),
                                                                          fill = c("white", "white")))))
      )} +
      # If species are present in all hauls
      {if(nrow(d5) == 0) list(
        message("Species are present in all hauls. 'Absent' hauls symbology will be removed from legend. Code does not currently support displaying symbology of features that are not present"),
        scale_colour_identity(guide = "legend", breaks = c("grey10"), labels = c("Present")),
        guides(colour = guide_legend(override.aes = list(shape = c(16),
                                                         size = c(3),
                                                         colour = c("grey10"),
                                                         fill = c("white"))))
      )} +
      # If species are absent in all hauls
      {if(nrow(d4) == 0) list(
        message("Species are absent in all hauls. 'Present' hauls symbology will be removed from legend. Code does not currently support displaying symbology of features that are not present"), 
        scale_colour_identity(guide = "legend", breaks = c("grey10"), labels = c("Present")),
        guides(colour = guide_legend(override.aes = list(shape = c(4),
                                                         size = c(3),
                                                         colour = c("grey30"),
                                                         fill = c("white"))))
      )} +
      # Inertia PCA
      geom_line(data = inertcross[c(1:2),], aes(x = V1, y = V2), colour = "blue") +
      geom_line(data = inertcross[c(3:4),], aes(x = V1, y = V2), colour = "blue") +
      # CoG
      geom_point(data = cog_xy, aes(x= cog_x, y = cog_y, shape = `Location Indicator`), colour = "blue", size = 3) + # centre of gravity
      scale_shape_identity(guide = "legend", labels = "Centre of Gravity") +
      guides(shape = guide_legend(override.aes = list(fill = NA)),
             size = guide_legend(override.aes = list(fill = NA))) +
      # Formatting
      labs(title = title, subtitle = paste0("Year: ", yr, " \nQuarter: ", paste0(as.character(qrs), collapse = ", "), " \nStock Divisions: ", paste0(stk_divs, collapse = ", "))) +
      xlab("Longitude") +
      ylab("Latitude") +
      theme_minimal() 
  }
  return(p)
}


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
shift_legend <- function(p){
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Shift Legend >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
  #>                          function by Z.Lin      
  #> https://stackoverflow.com/questions/54438495/shift-legend-into-empty-facets-of-a-faceted-plot-in-ggplot2
  #> When creating a facet_wrap plot, you are often left with empty space. The 
  #> legend makes this white space even larger by appending to one of the four
  #> sides of the plot. This function moves the legend into one of the empty 
  #> spaces created by facetting, reducing overall plot size and empty space
  #>                              NOT MY CODE!
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
  # check if p is a valid object
  if(!"gtable" %in% class(p)){
    if("ggplot" %in% class(p)){
      gp <- ggplotGrob(p) # convert to grob
    } else {
      message("This is neither a ggplot object nor a grob generated from ggplotGrob. Returning original plot.\n")
      return(p)
    }
  } else {
    gp <- p
  }
  
  # check for unfilled facet panels
  facet.panels <- grep("^panel", gp[["layout"]][["name"]])
  empty.facet.panels <- sapply(facet.panels, function(i) "zeroGrob" %in% class(gp[["grobs"]][[i]]))
  empty.facet.panels <- facet.panels[empty.facet.panels]
  if(length(empty.facet.panels) == 0){
    message("There are no unfilled facet panels to shift legend into. Returning original plot.\n")
    return(p)
  }
  
  # establish extent of unfilled facet panels (including any axis cells in between)
  empty.facet.panels <- gp[["layout"]][empty.facet.panels, ]
  empty.facet.panels <- list(min(empty.facet.panels[["t"]]), min(empty.facet.panels[["l"]]),
                             max(empty.facet.panels[["b"]]), max(empty.facet.panels[["r"]]))
  names(empty.facet.panels) <- c("t", "l", "b", "r")
  
  # extract legend & copy over to location of unfilled facet panels
  guide.grob <- which(gp[["layout"]][["name"]] == "guide-box")
  if(length(guide.grob) == 0){
    message("There is no legend present. Returning original plot.\n")
    return(p)
  }
  gp <- gtable_add_grob(x = gp,
                        grobs = gp[["grobs"]][[guide.grob]],
                        t = empty.facet.panels[["t"]],
                        l = empty.facet.panels[["l"]],
                        b = empty.facet.panels[["b"]],
                        r = empty.facet.panels[["r"]],
                        name = "new-guide-box")
  
  # squash the original guide box's row / column (whichever applicable)
  # & empty its cell
  guide.grob <- gp[["layout"]][guide.grob, ]
  if(guide.grob[["l"]] == guide.grob[["r"]]){
    gp <- gtable_squash_cols(gp, cols = guide.grob[["l"]])
  }
  if(guide.grob[["t"]] == guide.grob[["b"]]){
    gp <- gtable_squash_rows(gp, rows = guide.grob[["t"]])
  }
  gp <- gtable_remove_grobs(gp, "guide-box")
  
  return(gp)
}