### Set up fire events log df: ----
if(file.exists(file.path(landisOutputDir,'scrapple-events-log.csv'))){
  fire.df<-read.csv(file.path(landisOutputDir,'scrapple-events-log.csv'), strip.white = T)
} else {
  fire.df<-read.csv(file.path(landisOutputDir,'socialclimatefire-events-log.csv'), strip.white = T)
}
fire.df <- fire.df |>
  mutate(color = as.numeric(factor(IgnitionType)),
         IgnitionType = ifelse(IgnitionType=='Rx', 'Prescribed', IgnitionType),
         FIRE_SIZE = TotalSitesBurned * 0.81,  # fire size in Ha
         InitCell = cellFromRowCol(ecos.r, InitRow, InitColumn)  # get initial cell for each fire for severity df later
  ) |>
  mutate(color = ifelse(color == 1, grey(0.25,0.75), ifelse(color==2, rgb(0.25,0.05,0.05,0.75), rgb(0.05,0.25,0.05,0.75))),
         PWG = as.factor(pull(pwg.r$PWG[InitCell])),
         PercentCohortsKilled = round(CohortsKilled/AvailableCohorts*100,0),
         PercentCohortsKilled = replace_na(PercentCohortsKilled, 0))  

### Load fire maps: ----
cat('\nLoading fire maps...\n')
severityStack.r <- rast(file.path(fireOutput, 'fire-dnbr-yr.tif'))
flamingConsumptionStack.r <- rast(file.path(fireOutput, 'flaming-consumptions-yr.tif'))
smolderConsumptionStack.r <- rast(file.path(fireOutput, 'smolder-consumption-yr.tif'))
fireIdStack.r <- rast(file.path(fireOutput, 'event-ID-yr.tif'))
fineFuelStack.r <- rast(file.path(fireOutput, 'fine-fuels-yr.tif'))
ignitionTypeStack.r <- rast(file.path(fireOutput, 'ignition-type-yr.tif'))
burned.N.empirical <- rast(file.path(dataDir, 'MTBS_and_FOD_Fires', LANDIS.EXTENT, '_Fires_N.tif'))

severityStackClassified.r <- classify(severityStack.r,rcl=severity.reclass.df,include.lowest=T)
set.cats(severityStackClassified.r, layer=0, data.frame(id = c(1,2,3,4), severity=c('Unburned','Low','Moderate','High')))
names(severityStackClassified.r) <- names(severityStack.r)  # for some reason, the classify operation overwrites names

if(!file.exists(file.path(fireOutput, 'fire-dnbr-classified-yr.tif'))){
  severityStackSmoothedClassified.r <- ifel(severityStack.r<=1, NA, severityStack.r) |>  # have to class 0 as NA so that median window only operates on unburned cells
    focal(w = 3, fun = median, na.rm=T, pad = TRUE, na.policy = 'only') |>
    classify(rcl=severity.reclass.df,include.lowest=T) 
  set.cats(severityStackSmoothedClassified.r, layer=0, data.frame(id = c(1,2,3,4), severity=c('Unburned','Low','Moderate','High')))
  names(severityStackSmoothedClassified.r) <- names(severityStack.r)
  writeRaster(severityStackSmoothedClassified.r, file.path(fireOutput, 'fire-dnbr-classified-yr.tif'))
} 

severityStackSmoothedClassified.r <- rast(file.path(fireOutput, 'fire-dnbr-classified-yr.tif'))
set.cats(severityStackSmoothedClassified.r, layer=0, data.frame(id = c(1,2,3,4), severity=c('Unburned','Low','Moderate','High')))
names(severityStackSmoothedClassified.r) <- names(severityStack.r)
  
if(!file.exists(file.path(fireOutput, 'burn-footprints-yr.tif'))){
  burnFootprints.r <- ifel(fireIdStack.r == 0|is.na(fireIdStack.r), 0, 1)
  burnFootprintsSmooth.r <- fireIdStack.r |>  # classifying by fire ID and TotalSitesBurned in fire df takes FOREVER
    as.data.frame(xy = T) |>
    pivot_longer(starts_with("event-ID"), names_to = "Year", values_to = "event_ID", names_prefix = "event-ID-") |>
    filter(event_ID!=0) |>  # eliminate unburned areas
    group_by(event_ID) |>
    mutate(fire_size = n()) |> ungroup() |>  # calculate number of pixels in each event
    mutate(footprint = ifelse(fire_size > 3, 1, 0)) |>  # filter out fires smaller than three pixels -- if a pixel belongs to a fire of larger size, it's flagged as in the footprint
    pivot_wider(names_from = "Year", names_prefix = "fire-Size-", values_from = "footprint", names_sort = T) |> 
    select(!c(event_ID, fire_size)) |>
    rast(type = "xyz") |>
    focal(w=matrix(1,3,3), fun = median, na.rm=T, pad = TRUE, na.policy = 'only') |>  # finally, smooth footprints of fires bigger than 3 pixels
    extend(ext(pwg.r))  # add back in na cells removed by filtering above
  crs(burnFootprints.r) <- crs(pwg.r)
  crs(burnFootprintsSmooth.r) <- crs(pwg.r)
  writeRaster(burnFootprints.r, file.path(fireOutput, "burn-footprints-yr.tif"))
  writeRaster(burnFootprintsSmooth.r, file.path(fireOutput, "burn-footprints-smooth-yr.tif"))
} else {
  burnFootprintsSmooth.r <- rast(file.path(fireOutput, "burn-footprints-smooth-yr.tif"))
  burnFootprints.r <- rast(file.path(fireOutput, "burn-footprints-yr.tif"))
}
#-----------------------------------------------------------------------------------------------------------------------
## Fire Size Distribution: ----
cat('\n      -----------------------------------------------
     ---> Calculating fire size distribution...
    -----------------------------------------------\n')

#  Combine MTBS from 1984-2019 and the 1992-2015 ignition point datasets:
obs.ignitions.df<-vect(file.path(bigDataDir,"Fire_Ignitions_1992_2015_RDS-2013-0009/Fires_1992-2015_WA/Fires_1992-2015_WA.shp")) |>
  project(crs(ecos.r)) |> 
  crop(extend(ext(ecos.r),c(10000, 10000))) |> 
  as.data.frame() |>
  select(c('SOURCE_R_1','FIRE_NAME','DISCOVERY_','FIRE_YEAR','FIRE_SIZE','STAT_CAU_1','MTBS_ID')) |>
  rename(
    'IgnitionType' = 'STAT_CAU_1',
    'SIZE_ac_from_igntion_df' = 'FIRE_SIZE',
    'Event_ID' = 'MTBS_ID'
  ) |>
  mutate(SIZE_ha_from_igntion_df = SIZE_ac_from_igntion_df*0.4046864) |>  # round size for binned counts
  filter(SIZE_ac_from_igntion_df<1000)  ## Drop big fires, as these should all be duplicates of MTBS:

### Use MTBS for more appropriate comparison with 21st century fires:
mtbs.df.full<-read.csv(file.path(dataDir,paste0('MTBS_Raster_Summary_Stats_',LANDIS.EXTENT,'.csv')))  # Load MTBS layer, created with the script "Extract_MTBS_Maps.R" in the MTBS folder in BigData

mtbs.fod.df<-read.csv(file.path(dataDir,'MTBS_US_Fire_Occurence_Dataset.csv')) |>
  rename('FIRE_NAME' = 'Incid_Name') |>
  select(c('Event_ID','FIRE_NAME','Incid_Type','BurnBndAc'))

mtbs.df <- mtbs.df.full |>
  filter(Area_in_AOI>0) |>
  mutate(Event_ID = toupper(Event_ID)) |>
  left_join(mtbs.fod.df, by='Event_ID') |>  # Define fire type by merging with MTBS Fire Occurence Dataset from the MTBS website.
  full_join(obs.ignitions.df, by=c('FIRE_NAME','FIRE_YEAR')) |>  # Merge ignitions and mtbs data frames
  mutate(IgnitionType = ifelse(IgnitionType!='Lightning'|is.na(IgnitionType), 'Accidental', IgnitionType)) |>
  mutate(IgnitionType = ifelse(Incid_Type=='Prescribed Fire'&!is.na(Incid_Type), 'Prescribed', IgnitionType)) |>
  mutate(FIRE_SIZE = ifelse(!is.na(Area_in_AOI), Area_in_AOI, SIZE_ha_from_igntion_df)) |>  # if we have an MTBS-derived area in AOI value, use it, otherwise use size from ignition df
  rename(PercentUnburned = Percent_UB, PercentLow = Percent_Low, PercentModerate = Percent_Mod, PercentHigh = Percent_High)

head(mtbs.df)

## Make your own data frame to make pretty histograms:
fire.size.classes<-data.frame('Fire_size'=c('0-1','1-10','10-100','100-300','300-1000','1000-3000','3000-10000','10000+'),
                              'min'=c(0,1,10,100,300,1000,3000,10000),
                              'max'=c(1,10,100,300,1000,3000,10000,100000))

fire.size.classes<-rbind(data.frame(fire.size.classes,'Type'='Lightning'),
                         data.frame(fire.size.classes,'Type'='Accidental'),
                         data.frame(fire.size.classes,'Type'='Prescribed')) |>
  rowwise() |>  # use rowwise to do janky table lookup in mtbs.df and fire.df
  mutate(Actual_count_1984_2019 = nrow(mtbs.df[mtbs.df$FIRE_SIZE>=min & mtbs.df$FIRE_SIZE<max & mtbs.df$IgnitionType==Type,]),
         LANDIS_II_simulation_2020_2120 = nrow(fire.df[fire.df$FIRE_SIZE>=min & fire.df$FIRE_SIZE<max & fire.df$IgnitionType==Type,])) |> ungroup() |>
  mutate(Actual_count_per_year = round(ifelse(min < 300, Actual_count_1984_2019/23, Actual_count_1984_2019/35), 2), # Fire Occurence Dataset spans 1992-2015, MTBS spans 1984-2019
         LANDIS_II_simulation_per_year = round(LANDIS_II_simulation_2020_2120/simLength,2))  

#----------------------------------------------------#
## Log 10 plot for future power law fits: 
# Re-make fire.size.classes data frame with log10 categories
fire.size.classes.log10<-data.frame('Fire_size'=c('0-1','1-10','10-100','100-1000','1000-10000','10000+'),
                                    'min'=c(0,1,10,100,1000,10000),
                                    'max'=c(1,10,100,1000,10000,100000))  |>
  rowwise() |>  # use rowwise to do janky table lookup in mtbs.df and fire.df
  mutate(Actual_count_1984_2019 = nrow(mtbs.df[mtbs.df$FIRE_SIZE>=min & mtbs.df$FIRE_SIZE<max,]),
         LANDIS_II_simulation_2020_2120 = nrow(fire.df[fire.df$FIRE_SIZE>=min & fire.df$FIRE_SIZE<max,])) |> ungroup() |>
  mutate(Actual_count_per_year = round(ifelse(min < 300, Actual_count_1984_2019/23, Actual_count_1984_2019/35), 2), # Fire Occurence Dataset spans 1992-2015, MTBS spans 1984-2019
         LANDIS_II_simulation_per_year = round(LANDIS_II_simulation_2020_2120/simLength,2))  

#----------------------------------------------------#
## Power-law plot: ----
landis.sizes<-fire.df$FIRE_SIZE[order(fire.df$FIRE_SIZE)]
mtbs.sizes<-mtbs.df$FIRE_SIZE[order(mtbs.df$FIRE_SIZE)]
mtbs.sizes<-mtbs.sizes[mtbs.sizes>0.1]

## Summarize frequency above each unique size for LANDIS-II fires:
powerLaw.df.LANDIS <-data.frame("Size" = unique(landis.sizes), "Source" = 'LANDIS-II') |>
  rowwise() |>
  mutate(Freq.gt = length(landis.sizes[landis.sizes > Size])/simLength)

powerLaw.df <- data.frame("Size" = unique(mtbs.sizes), "Source" = 'MTBS') |>
  rowwise() |>
  mutate(Freq.gt = length(mtbs.sizes[mtbs.sizes > Size])) |>
  mutate(Freq.gt = ifelse(Size<400, Freq.gt/23, Freq.gt/35)) |>  # correct for FOD & MTBS year spans
  bind_rows(powerLaw.df.LANDIS) |>
  mutate(Freq.gt = round(Freq.gt, 2)) |>
  filter(Freq.gt>0, !is.na(Size), Size>1)

# Power law fit:
landis.lm<-lm(log10(Freq.gt)~log10(Size),data=powerLaw.df[powerLaw.df$Source=='LANDIS-II',])
mtbs.lm<-lm(log10(Freq.gt)~log10(Size),data=powerLaw.df[powerLaw.df$Source=='MTBS',])

#---------------------------------------------------------------------#
### Generate patch sizes dataframe: ----
cat('   summarizing severity patch metrics...\n')

if(!file.exists(file.path(landisOutputDir,'Fire_severity_patch_metrics.csv'))|simOpts$rerunFires==T){
  Annual_area_total <- global(ifel(!is.na(severityStackSmoothedClassified.r), 0.81, 0), fun = "sum", na.rm = TRUE) |> 
    select(sum) |> rename(Total_Area = sum) |> mutate(Year = row_number())
  
  cat('smoothed; ')
  patch.size.smooth.df <- lsm_c_area_mn(severityStackSmoothedClassified.r,directions = 8) |>
    bind_rows(lsm_c_area_mn(severityStackSmoothedClassified.r,directions = 8) |> mutate(metric = "patchArea_mn")) |>  # class area mean
    bind_rows(lsm_p_area(severityStackSmoothedClassified.r,directions = 8) |> mutate(metric = "patchArea_mx") |> group_by(layer, class, metric) |> summarise(value = max(value), level = 'class')) |>  # class area max
    bind_rows(lsm_c_ca(severityStackSmoothedClassified.r,directions = 8) |> mutate(metric = "Area_tot")) |>  # total class area
    bind_rows(lsm_c_core_mn(severityStackSmoothedClassified.r,directions = 8) |> mutate(metric = "coreArea")) |>  # core area mean
    bind_rows(lsm_c_cai_mn(severityStackSmoothedClassified.r,directions = 8) |> mutate(metric = "cai")) |>  # core area index
    filter(level=="class") |>
    rename(Year = layer, value_smoothed = value) |>
    select(!c(level, id))
  
  cat('non-smoothed; ')
  patch.size.df <- lsm_c_area_mn(severityStackClassified.r,directions = 8) |>
    bind_rows(lsm_c_area_mn(severityStackClassified.r,directions = 8) |> mutate(metric = "patchArea_mn")) |>
    bind_rows(lsm_p_area(severityStackClassified.r,directions = 8) |> mutate(metric = "patchArea_mx") |> group_by(layer, class, metric) |> summarise(value = max(value), level = 'class')) |>
    bind_rows(lsm_c_ca(severityStackClassified.r,directions = 8) |> mutate(metric = "Area_tot")) |>
    bind_rows(lsm_c_core_mn(severityStackClassified.r,directions = 8) |> mutate(metric = "coreArea")) |>
    bind_rows(lsm_c_cai_mn(severityStackClassified.r,directions = 8) |> mutate(metric = "cai")) |>
    filter(level=="class") |>
    rename(Year = layer) |>
    select(!c(level, id)) |>
    left_join(patch.size.smooth.df) |>
    mutate(Severity = factor(class, levels = c(1, 2, 3, 4), labels = c("UB", 'low', 'mod', 'high'))) |>
    complete(Severity, metric, Year, fill = list(value = 0, value_smoothed = 0)) |>  # expand with zero values for all metric x severity x year combos
    left_join(Annual_area_total)  # join annual area total last so it propagates to all years
  
  rm(patch.size.smooth.df)
  write.csv(patch.size.df, file.path(landisOutputDir,'Fire_severity_patch_metrics.csv'))
  cat('done!\n\n')
}

## Fire Severity patch metrics: ----
cat('\n      -----------------------------------------------
     ---> Preparing fire severity patch metric dataframes...
    -----------------------------------------------\n')
patch.size.df <- read.csv(file.path(landisOutputDir,'Fire_severity_patch_metrics.csv')) |>
  mutate(Year = Year+2019) |> filter(!is.na(Total_Area)) |>
  mutate(Severity = factor(Severity, levels = c("UB", 'low', 'mod', 'high'), labels = c("UB", 'low', 'mod', 'high')))

mtbs.patch.size.AOI<-mtbs.df.full |>
  filter(Area_in_AOI>0) |>
  pivot_longer(starts_with(c("UB", 'low', 'mod', 'high')), names_to = 'Sev_metric', values_to = 'value') |>
  mutate(Severity = as.factor(str_split_i(Sev_metric, '_', 1)),
         metric = str_replace(Sev_metric, paste0(Severity, '_'), ''))

patch.size.df.melted <- patch.size.df |> 
  mutate(Source = "LANDIS-II") |>
  bind_rows(mtbs.patch.size.AOI |> mutate(Source = "MTBS", value_smoothed = value)) |>
  select(Year, Source, Severity, metric, value, value_smoothed) |>
  mutate(Source = as.factor(Source))

#---------------------------------------------------------------------#
cat('\n      -----------------------------------------------
     ---> Calculating percent area per severity class ...
    -----------------------------------------------\n')

if(file.exists(file.path(landisOutputDir,'Fire_severity_df.csv'))&simOpts$rerunFires==F){
  severity.df<-read.csv(file.path(landisOutputDir,'Fire_severity_df.csv'))
  all.fire.dnbr<-read.csv(file.path(landisOutputDir,'Fire_all_dnbr_values.csv'))$x
} else {
  cat('\n - Summarizing severity distribution... \n')
  
  all.fire.dnbr <- severityStack.r |> as.data.frame() |>
    pivot_longer(cols = everything(), names_to = 'lyr', values_to = 'dnbr') |> filter(!is.na(dnbr), dnbr > 1) |>
    select(!lyr) |> pull()
  write.csv(all.fire.dnbr,file.path(landisOutputDir,'Fire_all_dnbr_values.csv'),row.names=F)
  
  fireSeverityPixels.df <- as.data.frame(severityStackClassified.r, xy=T) |>
    pivot_longer(starts_with("fire-dnbr-"), names_to = "SimulationYear", names_prefix = "fire-dnbr-", values_to = "Severity")
  
  percent_severity.df <-  as.data.frame(fireIdStack.r, xy=T) |>
    pivot_longer(starts_with("event-ID"), names_to = "SimulationYear", names_prefix = "event-ID-", values_to = "EventID") |> 
    filter(EventID != 0) |>
    left_join(fireSeverityPixels.df) |>
    filter(!is.na(Severity)) |>
    group_by(SimulationYear, EventID) |> mutate(FirePx = n()) |> ungroup() |>
    group_by(SimulationYear, EventID, Severity) |>
    summarise(Percent = round(100*n()/mean(FirePx), 2)) |>
    pivot_wider(names_from = "Severity", names_prefix = "Percent", names_expand = T, values_from = "Percent", values_fill = 0) |> 
    mutate(SimulationYear = as.integer(SimulationYear))
  
  severity.df<-fire.df |>
    select(EventID, SimulationYear, FIRE_SIZE, PercentCohortsKilled, PWG, IgnitionType) |>
    left_join(percent_severity.df)

  write.csv(severity.df,file.path(landisOutputDir,'Fire_severity_df.csv'),row.names=F)
}

#---------------------------------------------------------------------#
#   Severity density distribution: ----
emp.fire.dnbr <- read.csv(file.path(dataDir,'MTBS_and_FOD_Fires', LANDIS.EXTENT,'Fire_all_dnbr_values_empirical.csv')) |> select(x) |> mutate(Source='MTBS') |>
  dplyr::bind_rows(data.frame(x=all.fire.dnbr, Source='LANDIS')) |>
  mutate(dNBR = ifelse(x>1000,1000,x))

ev_log_comp <- fire.df |>
  select(EventID, SimulationYear, InitialFireWeatherIndex, TotalSitesBurned, InitialDayOfYear, MaximumSpreadArea, MeanWindSpeed, MeanEffectiveWindSpeed, MeanFWI, MeanSpreadProbability, MeanPET, MeanWD, MeanFineFuels, MeanLadderFuels, MeanDNBR, TotalBiomassMortality) |>
  mutate(Severity = ifelse(MeanDNBR > 376, 'High (>376)', 'LowToMod')) |> 
  mutate(Severity_full = cut(MeanDNBR, c(0, 41,176,376,2001), c('Unburned','Low','Moderate','High'))) |>
  mutate(ev_Size = cut(TotalSitesBurned, c(0, 12.3, 123, 1234, 12345, 123456), c("<10", "<100", "<1000", "<10000", "<100000"))) |> 
  pivot_longer(cols=!c(EventID, Severity, Severity_full, ev_Size), names_to = 'Var', values_to = 'Value') 

#-----------------------------------------------------------------------------------------------------------------------
## N.fires.per.pixel raster and N.fires.by.severity raster: ----
cat('\n      -----------------------------------------------
     ---> Generating Number of Times Burned per Pixel and Severity class...
    -----------------------------------------------\n')

if(!file.exists(file.path(fireOutput,'_Fires_N_smoothed.tif'))|simOpts$rerunFires){
  ## Number of fires: ----
  burned.N.r <- burnFootprints.r |> sum(na.rm=T)
  burned.N.smoothed.r <- burnFootprintsSmooth.r |>
    sum(na.rm = T)
  
  ## Number of fires per severity class: ----
  cat('\n      - Summing number of times burned per severity class...')
  for(c in severity.reclass.df$becomes[2:5]){
    r.sum <- ifel(severityStackClassified.r == c, 1, 0) |> sum(na.rm = T)
    if(c==severity.reclass.df$becomes[2]) {burned.N.sev.r <- r.sum} else {burned.N.sev.r <- c(burned.N.sev.r,r.sum)}
  }
  names(burned.N.sev.r)<-c('Unburned','Low','Moderate','High')
  names(burned.N.r) <- "burned.N"
  names(burned.N.smoothed.r) <- "burned.N.smoothed"
  
  writeRaster(burned.N.r,file.path(fireOutput,'_Fires_N.tif'),overwrite=T)
  writeRaster(burned.N.smoothed.r,file.path(fireOutput,'_Fires_N_smoothed.tif'),overwrite=T)
  for(sev in names(burned.N.sev.r)){
    writeRaster(burned.N.sev.r[[sev]],file.path(fireOutput,paste0('_Fires_N_',sev,'.tif')), overwrite=T)
  }
  
} else {
  burned.N.r<-rast(file.path(fireOutput,'_Fires_N.tif'))
  names(burned.N.r) <- "burned.N"
  burned.N.smoothed.r<-rast(file.path(fireOutput,'_Fires_N_smoothed.tif'))
  names(burned.N.smoothed.r) <- "burned.N.smoothed"
  burned.N.sev.r<-c(rast(file.path(fireOutput,paste0('_Fires_N_Unburned.tif'))),
                    rast(file.path(fireOutput,paste0('_Fires_N_Low.tif'))),
                    rast(file.path(fireOutput,paste0('_Fires_N_Moderate.tif'))),
                    rast(file.path(fireOutput,paste0('_Fires_N_High.tif'))))
  names(burned.N.sev.r)<-c('Unburned','Low','Moderate','High')
}

#-----------------------------------------------------------------------------------------------------------------------
## Fire Return Interval by PWG and Severity class: ----
cat('\n      -----------------------------------------------
     ---> Generating FRI data frame and fire frequency figure...
    -----------------------------------------------\n')

cat('\n      - Calculating FRI...')
FRI.df <- c(pwg.r, burned.N.smoothed.r, burned.N.r, burned.N.sev.r) |>
  as.data.frame() |> 
  filter(!is.na(PWG), !PWG%in%c(10,11)) |>
  mutate(burned = ifelse(burned.N>0, 1, 0)) |>
  group_by(PWG) |>
  summarise(
    Area.ha = n()*0.81,
    AreaBurned = sum(burned)*0.81,
    CumulativeAreaBurned.All = sum(burned.N)*0.81,
    CumulativeAreaBurned.UB = sum(Unburned,na.rm=T)*0.81,
    CumulativeAreaBurned.Low = sum(Low,na.rm=T)*0.81,
    CumulativeAreaBurned.Mod = sum(Moderate,na.rm=T)*0.81,
    CumulativeAreaBurned.High = sum(High,na.rm=T)*0.81,
    CumulativeAreaBurned.Smoothed = sum(burned.N.smoothed,na.rm=T)*0.81,
  ) |>
  mutate(
    AreaBurnedPercent = 100*AreaBurned/Area.ha,
    AreaBurnedPerYear = AreaBurned/simLength,
    CumulativeAreaBurnedPercent = 100*CumulativeAreaBurned.All/Area.ha,
    FRI.All = Area.ha/(CumulativeAreaBurned.All/simLength),
    FRI.Smoothed = Area.ha/(CumulativeAreaBurned.Smoothed/simLength),
    FRI.UB = Area.ha/(CumulativeAreaBurned.UB/simLength),
    FRI.Low = Area.ha/(CumulativeAreaBurned.Low/simLength),
    FRI.Mod = Area.ha/(CumulativeAreaBurned.Mod/simLength),
    FRI.High = Area.ha/(CumulativeAreaBurned.High/simLength),
  ) |>
  mutate(across(everything(), \(x) round(x, 0)))

## Check result:
sum(fire.df$FIRE_SIZE)
sum(FRI.df$CumulativeAreaBurned.All)
## The difference between these totals is that some fires burn in inactive PWGs (10 and 11). 

write.csv(FRI.df,file.path(landisOutputDir,'Fire_return_interval_df.csv'),row.names=F)

#   - Number of fires per pixel per severity class: ----
#     For each PWG, inspect pixels that burn more than once:
cat('\n      - Counting number of times burned per PWG per pixel... PWG: ')

nFires.df <- c(pwg.r, burned.N.r, burned.N.sev.r) |>  # make a df with 
  as.data.frame() |>
  filter(PWG %in% 12:50) |>
  rename(All = burned.N) |>  # rename the column denoting all severity categories to "All"
  group_by(PWG) |> mutate(area.ha = n()*0.81) |> ungroup() |>
  pivot_longer(c(All, Unburned, Low, Moderate, High), names_to = 'Severity', values_to = 'N.fires') |>
  mutate(N.fires = replace_na(N.fires, 0)) |>  # replace NA with zero for the severity rasters
  group_by(PWG, N.fires, Severity) |>
  summarise(N.pixels = n(), area.ha = median(area.ha)) |>
  mutate(Severity = factor(Severity, levels=c('All','Unburned','Low','Moderate','High')))
  

#-----------------------------------------------------------------------------------------------------------------------
## Create annual severity ratios plot (Povak Fig. 4): ----
# #---------------------------------------------------------------------#
cat('\n Making Povak Fig. 4 style plots for cold vs. dry forests\n')

PWG_reclass <- pwg.r 
PWG_reclass[PWG_reclass==20] <- 30  # DMC 20 + MMC 30
PWG_reclass[PWG_reclass==40] <- 50  # CMC 40 + CDC 50
PWG_reclass[!PWG_reclass%in%c(30,50)] <- NA
plot(PWG_reclass)

forest_type_area <- as.data.frame(PWG_reclass) |> group_by(PWG) |> summarise(forest_area = 0.81*dplyr::n()) |> mutate(PWG = as.factor(PWG))

MTBS_dir <- file.path(dataDir,'MTBS_and_FOD_Fires', LANDIS.EXTENT)

annual_sev.df <- data.frame('year' = integer(0), 'PWG'=character(0), 'sev_class'=character(0), 'count' = integer(0))

for (i in 1984:2019){
  cat(paste0(i,'...'))
  fire_sev <- rast(file.path(MTBS_dir, paste0('Observed_fires_', i, '.tif'))) 
  fire_sev[fire_sev<min(severity.reclass.df$from)]<-NA
  
  fire_sev <- fire_sev |> 
    terra::classify(severity.reclass.df, include.lowest = T) 
  # plot(fire_sev)
  
  stack <- c(PWG_reclass, 'dnbr'= fire_sev)
  
  yr_df <- as.data.frame(stack) |> 
    filter(!is.na(PWG)) |> 
    mutate(sev_class = cut(dnbr, c(0, 1, 2, 3, 4), c('UB', 'Low', 'Moderate', 'High'))) |>
    group_by(PWG, sev_class) |> summarise(count = dplyr::n()) |>
    mutate(PWG = as.factor(PWG), year = i, sev_class = as.character(sev_class)) |> filter(!is.na(sev_class))
  
  #hist(yr_df$dnbr)
  annual_sev.df <- dplyr::bind_rows(annual_sev.df, yr_df)
}

landis_sev.df <- c(PWG_reclass, severityStackSmoothedClassified.r) |>
  as.data.frame() |>
  pivot_longer(starts_with("fire-dnbr"), names_to = c('drop', 'drop2', 'year'), names_sep = '-', values_to = 'sev_class') |>
  filter(!is.na(sev_class), !is.na(PWG)) |>
  select(!c('drop', 'drop2')) |>
  mutate(year = as.numeric(year) + 2019, PWG = as.factor(PWG)) |>
  group_by(year, PWG, sev_class) |>
  summarise(count = n())

annual_sev.df <- annual_sev.df |>
  bind_rows(landis_sev.df) |>
  mutate(sev_class = factor(sev_class, levels = c('UB', 'Low', 'Moderate', 'High')),
         area_ha = count * 0.81) |>
  left_join(forest_type_area) |> 
  mutate(pct_forest_type = area_ha/forest_area * 100,
         PWG = ifelse(PWG == 30, "Dry & Moist Mixed Conifer (PWG 20, 30)", "Cold Forest (PWG 40, 50)"))

#-----------------------------------------------------------------------------------------------------------------------
## Attach PWG to fire events log: ----
# #---------------------------------------------------------------------#
cat('\n Linking PWG to fire events log\n')

event_pwgs.df <- c(pwg.r, fireIdStack.r) |>
  as.data.frame() |> 
  pivot_longer(starts_with("event-ID"), names_to = "SimulationYear", names_prefix = "event-ID-", values_to = "EventID") |> 
  filter(EventID != 0) |>
  mutate(SimulationYear = as.integer(SimulationYear)) |>
  group_by(EventID, SimulationYear) |> mutate(event_px_count = dplyr::n()) |> ungroup() |>
  group_by(PWG, EventID, SimulationYear) |>
  summarise(pct = n() / median(event_px_count) * 100, event_px_count = median(event_px_count)) |> ungroup() |>
  pivot_wider(id_cols = c(EventID, SimulationYear), names_from = PWG, values_from = pct, names_prefix = 'pct_', values_fill = 0) 



event_pwg.df <- severity.df |> select(EventID, SimulationYear, FIRE_SIZE) |> 
  left_join(event_pwgs.df)

write.csv(event_pwg.df,file.path(landisOutputDir,'Fire_pwg_df.csv'),row.names=F)

#-----------------------------------------------------------------------------------------------------------------------
## Done with fire: ----
cat('\n      -----------------------------------------------
     ---> Fire post-processing complete!
    ===============================================\n')
cat(paste('\n\nFire routine complete.', Sys.time(), '\n'), file = outFile, append = T)
#-----------------------------------------------------------------------------------------------------------------------


