#-----------------------------------------------------------------------------------------------------------------------
### Harvest GIFs: ----
## Define prescriptions. *NOTE: These must be in the same order as the prescriptions are listed in the ext_BiomassHarvest.txt file!
## *** THIS WOULD BE A GREAT PLACE TO READ IN THE ext_BiomassHarvest.txt file from the Input File Archive and extract treatment types from there.
cat('\n      -----------------------------------------------
     ---> Processing harvest outputs...
    -----------------------------------------------\n')

harvestEvents.df <- harvestEvents.df |>  # pivot into more usable long form
  select(!contains(c("Grass_Forb", "Nfixer", "NonFxr"))) |>
  mutate(BiomassHarvestedMg_TotalBiomass = MgBiomassRemoved,
         Year = as.numeric(Time)) |>  # rename this for pivot longer to work in the next step
  pivot_longer(starts_with(c("CohortsHarvested_", "BiomassHarvestedMg_")), names_to = c("Metric", "Species"), names_sep = "_", values_to = "val") |>
  pivot_wider(names_from = "Metric", values_from = "val") |>
  mutate(Prescription = gsub(' ','', Prescription),
         HarvestedHA = HarvestedSites * 0.81) |>
  left_join(merch_partition.df) |>
  mutate(HarvestedMerchMg = Merch_frac * BiomassHarvestedMg,
         HarvestedResidueMg = (1-Merch_frac) * BiomassHarvestedMg)

### Add snags onto harvest events: -----
##### Grab standing dead and patches, filter patches to those corresponding to salvage events, tally biomass of snags and add to 


### Process LanzaTech dataframe: -----
if(file.exists(file.path(LANDIS.EXTENT, "Ancillary_data", paste0("BiomassCollectionHexes_", LANDIS.EXTENT, ".gpkg"))) &
   !file.exists(file.path(landisOutputDir, paste0('Merch_biomass_hexes', basename(landisOutputDir), '.csv')))
   ){
  hexes.shp <- vect(file.path(LANDIS.EXTENT, "Ancillary_data", paste0("BiomassCollectionHexes_", LANDIS.EXTENT, ".gpkg")))

  
  df <- harvestEvents.df |>
    filter(Species != "TotalBiomass") |>
    filter(HarvestedSites > 0,
           Prescription%in%c('MaxBiomass_Valley',	'MaxBiomass_Mesic',	'MaxBiomass_Xeric',	'SalvageLogging',	'CT_dry',	'CT_moist',	'CC_cold')) |>  # only interested in managed forest thinning
    group_by(Year, Prescription, Stand, EventID, HarvestedSites) |>
    summarise(HarvestedMerchMg = sum(HarvestedMerchMg, na.rm = T),  # sum harvested merch and residue for all species in event
              HarvestedResidueMg = sum(HarvestedResidueMg, na.rm = T)) |> ungroup()  # convert from total in the harvest event to amount in each pixel, since eventsdf rows mapped to pixels WRONG harvest sites != px


  ### Create map of merch biomass at harvest site
  cat('\nCalculating merchantable biomass in collection hexagons...\n')
  merch.hexes.df <- c(pwg.r, standIDmaps.r) |> as.data.frame(na.rm=T, xy = T) |>  # one pixel per row, columns are years
    filter(!is.na(PWG)) |>
    pivot_longer(cols=names(standIDmaps.r), names_to=c("drop1", "drop2", 'Year'), names_sep = '-', values_to='Stand') |>  # one row per pixel-year
    select(!c(drop1, drop2)) |>
    mutate(Year = as.numeric(Year)) |>
    group_by(Stand, Year) |> mutate(Px_count = n()) |> ungroup() |>  # calculate number of pixels in stand to divide out total biomass extracted from stand
    left_join(df, by=c('Year', 'Stand')) |>
    mutate(HarvestedMerchMgPix = HarvestedMerchMg/Px_count) |>
    mutate(HarvestedMerchMgPix = replace_na(HarvestedMerchMgPix, 0))|>
    select(x, y, Year, HarvestedMerchMgPix) |>
    pivot_wider(names_from = Year, values_from = HarvestedMerchMgPix, names_prefix = "MerchMg-") |>
    rast(type = "xyz", extent = ext(pwg.r), crs = crs(pwg.r)) |>
    terra::extract(hexes.shp, fun = 'sum', bind = T)

  
  
  write.csv(as.data.frame(merch.hexes.df), file.path(landisOutputDir, paste0('Merch_biomass_hexes', basename(landisOutputDir), '.csv')))
}

### Estimate salvage yield: -----
# Biomass available from HS fire, HS

# areas that burned a year or two prior
sev_prev.r <- c(zero.r[[1]], severityStackSmoothedClassified.r[[1:(simLength-1)]])
sev_prev2.r <- c(zero.r[[1]], zero.r[[1]], severityStackSmoothedClassified.r[[1:(simLength-2)]])
harvest_zones.r <- rast(file.path(modelDir, LANDIS.EXTENT, paste0("ext_BiomassHarvestMgmt_", LANDIS.EXTENT, ".tif")))

available_burned.r <- ifel((sev_prev.r >= 2 | sev_prev2.r >= 2) & is.na(severityStackSmoothedClassified.r) & (harvest_zones.r %in% c(1, 2, 3)), 1, 0)
plot(available_burned.r)

# Calculate pre-fire biomass
# to get potential yield, for each pixel, we want to get mean harvest yield from stands of the same pwg and stand age????

# thinning_removals.r <- ifel(harvestPrescripts.r %in% c(12,13,14), biomassRemoved.r, 0)  # get removals only from regular thinning operations
# names(thinning_removals.r) <- paste0("yield-", 1:100)

# mean_yield.df <- c(pwg.r, thinning_removals.r, )


# get average yield by PWG and stand age?

# the apply to pixels marked as salvage logged

# and write a csv?



cat('\n      -----------------------------------------------
     ---> Harvest figs complete!
    ===============================================\n')
cat(paste('\n\nHarvest routine complete.', Sys.time(), '\n'), file = outFile, append = T)

