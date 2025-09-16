#-----------------------------------------------------------------------------------------------------------------------
cat('\n\n      ----------------------------------------
     ---> Generating Biomass_Annual_Dynamics.csv file...
    ----------------------------------------\n\n')

cat('------------------------\n Tallying Species Biomass\n------------------------')


ecos.area.df <- c(pwg.r) |> as.data.frame() |>
  mutate(PWG = as.factor(PWG)) |>
  group_by(PWG) |>
  summarise(PWG_area_Ha = n() * 0.81)

gc()

biomass.df <- data.frame(PWG = NULL, Species = NULL, Year = NULL, ActiveSites = NULL, Biomass_sum_Mg = NULL)

for (spp in c("AbieAmab","AbieGran","AbieLasi","LariOcci","PiceEnge","PinuCont",
                                                         "PinuMont","PinuPond","PseuMenz","ThujPlic","TsugHete","TsugMert","TotalBiomass")){
  cat(paste0('\n...', spp))
  biomassStack.selected.r <- biomassStack.r |> select(starts_with(spp))
  
  df <- c(pwg.r, biomassStack.selected.r) |> as.data.frame() |> 
    filter(!is.na(PWG)&PWG>=12) |>
    pivot_longer(contains("biomass"), names_to = c('Species', 'Year', 'Drop'), names_sep = '-', values_to = 'Biomass_gm2') |>
    mutate(Biomass_Mg = Biomass_gm2*0.01*0.81, # convert from g/m^2 to Mg/Ha to Mg
           active = ifelse(Biomass_gm2 > 0, 1, 0),
           PWG = as.factor(PWG), 
           Year = as.numeric(Year)) |>  # mark year-species-pixel observations as active if biomass is nonzero
    group_by(PWG, Species, Year) |>
    summarise(
      ActiveSites = sum(active),
      Biomass_sum_Mg = sum(Biomass_Mg)
    )
  
  biomass.df <- bind_rows(biomass.df, df)
}



#### If fire is active, tally fire: flamingconsumptionMg, SmoulderingconsumptionMg, Biomasskilledbyfire_mg, Burned_ha, 
# biomasskilledbyfire_mgHa, BiomasskilledbyfireMgHaburned, BiomassKilledbyfireMgHaEco,  
if(dir.exists(fireOutput)){
  cat('\n\n------------------------\n Calculating Fire Effects\n------------------------\n\n')
  cat('Mortality...')
  
  bioKilled.df <- c(pwg.r, fireIdStack.r) |> as.data.frame() |> 
    filter(!is.na(PWG)&PWG>=12) |> 
    pivot_longer(cols=names(fireIdStack.r), names_to = c('drop1', 'drop2', 'Year'), names_sep = '-', values_to = 'EventID') |>
    filter(EventID != 0) |>
    select(!c(drop1, drop2)) |>
    group_by(Year, PWG, EventID) |> tally() |>
    left_join(fire.df |> select(c(EventID, TotalSitesBurned, TotalBiomassMortality, IgnitionType)), by = 'EventID') |>
    mutate(PWG_frac = n/TotalSitesBurned,
           Rx = ifelse(IgnitionType == "Prescribed", 1, 0),
           TotalMortality_Mg = TotalBiomassMortality*0.01*0.81) |>
    group_by(PWG, Year) |>
    summarise(BiomassKilledByFire_Mg = sum(TotalMortality_Mg*PWG_frac),
              BiomassKilledByRxFire_Mg = sum(TotalMortality_Mg*PWG_frac*Rx),
              Burned_Ha = sum(n*0.81)) |>
    mutate(PWG = as.factor(PWG),
           Year = as.numeric(Year),
           Species = "TotalBiomass")
  
  gc()
  
  cat('Fire consumption...\n\n')
  
  consumption.df <- zonal(c(flamingConsumptionStack.r, smolderConsumptionStack.r), pwg.r, fun='sum', na.rm=T) |>
    filter(!is.na(PWG), !PWG%in%c(10,11,99)) |> 
    pivot_longer(starts_with(c('flaming-consumptions-', 'smolder-consumption-')), names_to = c('consumptionType', 'drop', 'Year'), names_sep = '-', values_to = 'consumption_gm2') |>
    select(!drop) |>  # have to remove this because consumption and consumptions are different and act as ID values if not eliminated
    pivot_wider(names_from = 'consumptionType', values_from = 'consumption_gm2') |>
    mutate(
      FlamingConsumption_Mg = flaming*0.01*0.81,  # convert gm2 to Mg
      SmolderingConsumption_Mg = smolder*0.01*0.81,
      Species = 'TotalBiomass',
      Year = as.numeric(Year),
      PWG = as.factor(PWG)
    ) |>
    select(!c(flaming, smolder))
  
  finefuels.df <- zonal(fineFuelStack.r, pwg.r, fun = 'sum', na.rm=T) |>
    filter(!is.na(PWG), !PWG%in%c(10,11,99)) |> 
    pivot_longer(starts_with('fine-fuels'), names_to = c('drop1', 'drop2', 'Year'), names_sep = '-', values_to = 'fine_fuels_gm2') |>
    select(!c(drop1, drop2)) |>
    mutate(FineFuelsMg = fine_fuels_gm2 * 0.81 * 0.01,
           Species = 'TotalBiomass',
           Year = as.numeric(Year),
           PWG = as.factor(PWG)) |>
    select(PWG, Year, Species, FineFuelsMg)

  
} else {
  bioKilled.df <- expand.grid('Species'="TotalBiomass",'PWG'=c('12', '13', '14', '15', '20', '30', '40', '50'),'Year'=0:simLength, 'BiomassKilledByFire_Mg'=0, 'Burned_Ha' = 0, 'BiomassKilledByRxFire_Mg'=0)
  consumption.df <- expand.grid('Species'="TotalBiomass",'PWG'=c('12', '13', '14', '15', '20', '30', '40', '50'),'Year'=0:simLength, 'FlamingConsumption_Mg'=0, 'SmolderingConsumption_Mg' = 0)
  finefuels.df <- zonal(rast(file.path(necnOutput, "SurfaceLitterBiomass-yr.tif")), pwg.r, fun = 'sum', na.rm=T) |>
    filter(!is.na(PWG), !PWG%in%c(10,11,99)) |> 
    pivot_longer(starts_with('SurfaceLitterBiomass'), names_to = c('drop1', 'Year'), names_sep = '-', values_to = 'fine_fuels_gm2') |>
    select(!c(drop1)) |>
    mutate(FineFuelsMg = fine_fuels_gm2 * 0.81 * 0.01,
           Species = 'TotalBiomass',
           Year = as.numeric(Year),
           PWG = as.factor(PWG)) |>
    select(PWG, Year, Species, FineFuelsMg)
}
#### If harvest is active, tally harvest: harvested mg, harvested mgha, harvested mgha cut, harvested mgha eco, treatments, harvested rastermg, harvested ha  
if(dir.exists(harvestOutput)){
  if(exists('harvestEvents.df')){
    cat('\n\n------------------------\n Tallying Harvest\n------------------------\n\n')
      
    harvest.biomass.raster.df <- zonal(biomassRemoved.r, pwg.r, 'sum') |>
      pivot_longer(cols = names(biomassRemoved.r), names_to = c('drop1', 'drop2', 'Year'), names_sep='-', values_to = 'HarvestedRaster_gm2') |>
      select(!c(drop1, drop2)) |>
      mutate(PWG = as.factor(PWG), 
             Year = as.numeric(str_replace(Year, ".tif", "")), 
             HarvestedRaster_Mg = HarvestedRaster_gm2 * 0.01 * 0.81, 
             Species = 'TotalBiomass')
    
    gc()
    harvest.biomass.df <- c(pwg.r, standIDmaps.r) |> as.data.frame(na.rm=T) |>  # one pixel per row, columns are years
      pivot_longer(cols=names(standIDmaps.r), names_to=c("drop1", "drop2", 'Year'), names_sep = '-', values_to='Stand') |>  # one row per pixel-year
      select(!c(drop1, drop2)) |>
      mutate(Year = as.numeric(Year)) |>
      filter(Stand%in%unique(harvestEvents.df$Stand), !PWG%in%c(10,11,99)) |>
      group_by(PWG, Year, Stand) |> tally() |> # count of pixels in each stand-year-PWG 
      right_join(harvestEvents.df, by=c('Year', 'Stand')) |>
      mutate(PWG = as.factor(PWG),
             Harvested_Mg_Stand_PWG = BiomassHarvestedMg * n/HarvestedSites * 0.81,  # total harvested in a stand times the fraction in the current PWG times the area of a pixel
             Harvested_HA_PWG = n * 0.81) |>  # calc the Ha of the stand falling within a PWG
      filter(Harvested_Mg_Stand_PWG > 0) |>  # we want to 
      group_by(Year, Species, PWG) |>  # consolidate stands by PWG
      summarize(Harvested_Mg = sum(Harvested_Mg_Stand_PWG),  # this one's just the total biomass for the species for the PWG
                Harvested_Ha = sum(n)*0.81,  # this should only be grabbing stands with the species present
                Treatments = n())
  }
} else {
  harvest.biomass.raster.df <- expand.grid('Species'="TotalBiomass",'Eco'=c('12', '13', '14', '15', '20', '30', '40', '50'),'Year'=0:simLength, 'HarvestedRaster_Mg'=0)
  harvest.biomass.df <- expand.grid('Species'=c("AbieAmab","AbieGran","AbieLasi","LariOcci","PiceEnge","PinuCont",
                                                "PinuMont","PinuPond","PseuMenz","ThujPlic","TsugHete","TsugMert","TotalBiomass"),'PWG'=c('12', '13', '14', '15', '20', '30', '40', '50'),'Year'=0:simLength, 'Harvested_Mg'=0, 'Harvested_Ha' = 0, Treatments = 0)
}

gc()
biomass.dynamics.df <- expand.grid('Species'=c("AbieAmab","AbieGran","AbieLasi","LariOcci","PiceEnge","PinuCont",  # ensure one row per species x PWG x Year combination
                                               "PinuMont","PinuPond","PseuMenz","ThujPlic","TsugHete","TsugMert","TotalBiomass"),
                                   'PWG'=c('12', '13', '14', '15', '20', '30', '40', '50'), 
                                   'Year'=0:simLength) |>
  left_join(ecos.area.df) |>
  left_join(biomass.df) |>
  left_join(bioKilled.df) |>
  left_join(consumption.df) |>
  left_join(finefuels.df) |>
  left_join(harvest.biomass.raster.df) |>
  left_join(harvest.biomass.df) |>
  arrange(Species, Year, PWG) |>
  mutate(  # calculate summaries by ActiveSites, PWG, Burned area, cut area
    Biomass_mean_MgHaActive = Biomass_sum_Mg/(ActiveSites*0.81),
    Biomass_mean_MgHaEco = Biomass_sum_Mg/PWG_area_Ha,
    BiomassKilledByFire_MgHaBurned = BiomassKilledByFire_Mg/Burned_Ha,
    BiomassKilledByRxFire_MgHaBurned = BiomassKilledByRxFire_Mg/Burned_Ha,
    BiomassKilledByFire_MgHaEco = BiomassKilledByFire_Mg/PWG_area_Ha,
    BiomassKilledByRxFire_MgHaEco = BiomassKilledByRxFire_Mg/PWG_area_Ha,
    BiomassKilledByFire_MgHa = BiomassKilledByFire_Mg/(ActiveSites*0.81),
    BiomassKilledByRxFire_MgHa = BiomassKilledByRxFire_Mg/(ActiveSites*0.81),
    FineFuels_mean_MgHaEco = FineFuelsMg/PWG_area_Ha,
    Harvested_MgHaEco = Harvested_Mg/PWG_area_Ha,
    Harvested_MgHa = Harvested_Mg/(ActiveSites*0.81),  # out of the area where the species is present
    Harvested_MgHaCut = Harvested_Mg/Harvested_Ha,  # calculate Mg/HA within sites where species is present!
    Eco = PWG,
    Biomass_SimAreaAverage_MgHa = Biomass_sum_Mg / study.area.size.Ha,
    Harvested_SimAreaAverage_MgHa = Harvested_Mg / study.area.size.Ha,
    Killed_by_fire_SimAreaAverage_MgHa = BiomassKilledByFire_Mg / study.area.size.Ha,
    Species = as.factor(Species)
  ) |>
  group_by(PWG, Species) |>
  mutate(  # smooth NA biomass values
    Biomass_sum_Mg = zoo::na.approx(Biomass_sum_Mg, na.rm=F),
    Biomass_mean_MgHaActive = zoo::na.approx(Biomass_mean_MgHaActive, na.rm=F),
    Biomass_mean_MgHaEco = zoo::na.approx(Biomass_mean_MgHaEco, na.rm=F),
    Biomass_SimAreaAverage_MgHa = zoo::na.approx(Biomass_SimAreaAverage_MgHa, na.rm=F),
    PWG_Name = factor(PWG, levels = c(12, 13, 14, 15, 20, 30, 40, 50), labels = ecos2)
  ) |> ungroup()
  

write.csv(biomass.dynamics.df,file.path(landisOutputDir,'Biomass_Annual_Dynamics.csv'),row.names=F)
cat(' ... Complete! \n')
gc()

#-----------------------------------------------------------------------------------------------------------------------
### Plot all time steps for one species: ----
plotAllYrs <- function(species){
  r <- rast(file.path(biomassOutput, paste0(species, "-yr-biomass.tif"))) |>
    select(contains(paste0("-", as.character(seq(0, simLength, 20)), '-')))
  r <- ifel(is.na(pwg.r), NA, r)
  rmax <- global(r, fun='max', na.rm=T) |> max(na.rm=T)
  names(r) <- paste("Year", seq(0, simLength, 20), species)
  
  plot(r, col=biomassCols, range = c(0, rmax))
  #pngOut(plot(r, col=biomassCols, range = c(0, rmax), legend = F), file.path(landisOutputDir,paste0('biomass_', species, '_all_years.png')),height=9,width=11,res=600,units='in')
}

#-----------------------------------------------------------------------------------------------------------------------
### Plot all species at one time step: ----
plotAllSpp <- function(yr){
  r <- rast(file.path(biomassOutput, paste0(species.subset, '-yr-biomass.tif'))) |>  # one mega-stack with biomass of all listed species
    select(contains(paste0(yr, '-biomass')))
  r <- ifel(is.na(pwg.r), NA, r)
  rmax <- global(r, fun='max', na.rm=T) |> max(na.rm=T)
  pngOut(plot(r, col=biomassCols, range = c(0, rmax), legend = F, maxnl= 20), file.path(landisOutputDir,paste0('biomass_year_', yr, '_all_species.png')),height=9,width=11,res=600,units='in')
}
# for (yr in c(0, 20, 40, 60, 80, 100)){
#   if(yr > simLength){next}
#   print(paste("Plotting biomass for all species year", yr))
#   plotAllSpp(yr)
# }

cat(paste('\n\nBiomass routine complete.', Sys.time(), '\n'), file = outFile, append = T)

