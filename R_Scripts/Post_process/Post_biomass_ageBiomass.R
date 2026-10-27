

if(!dir.exists(file.path(landisOutputDir, 'ageBiomassSecondary'))) {
  dir.create(file.path(landisOutputDir, 'ageBiomassSecondary'))
} 

 
ageBiomassStacks <- dir(file.path(landisOutputDir, "ageBiomassOutput"))
allSpp <- str_extract(ageBiomassStacks, "^(.+)-(.+)-yr.tif", group = 1) |> unique() 
treeSpp <- allSpp[!allSpp%in%c("Nfixer_Resprt","NonFxr_Resprt","NonFxr_Seed","Grass_Forb","TotalBiomass")]
treeStacks <- ageBiomassStacks[str_detect(ageBiomassStacks, paste(treeSpp, collapse = "|"))]

biomass.df <- data.frame()

if (!file.exists(file.path(landisOutputDir, "ageBiomassSecondary", "biomass_by_pwg.csv"))){
  for (spp in treeSpp){
    print(spp)
    
    sppStacks <- ageBiomassStacks[str_detect(ageBiomassStacks, spp)]
    
    spp.biomass.r <- rast(file.path(landisOutputDir, "ageBiomassOutput", sppStacks))
    
    # spp.biomass.r <- ageBiomass.sds 
    
    summary.df <- zonal(spp.biomass.r, pwg.r, fun = 'sum', na.rm = T) |>
      pivot_longer(starts_with(spp), names_to = c("Species", "Cohort", "Year"), names_sep = "-", values_to = "Biomass_gm2") |>
      filter(!PWG%in%c(10,11,99)) |>
      mutate(Year = as.integer(Year), Biomass_MG = Biomass_gm2 * 0.01 * 0.81)
   
    biomass.df <- biomass.df |> bind_rows(summary.df) 
    
  }
  write.csv(biomass.df, file.path(landisOutputDir, "ageBiomassSecondary", "biomass_by_pwg.csv"))
} else {
  biomass.df <- read.csv(file.path(landisOutputDir, "ageBiomassSecondary", "biomass_by_pwg.csv"))
}




df <- biomass.df |>
  filter(Year == 0) |>
  mutate(CohortAge = as.numeric(str_extract(Cohort, "\\d+"))) |>
  group_by(Species) |>
  summarise(
    Biomass_MG = sum(Biomass_MG),
    # AgeMean = weighted.mean(CohortAge, w = Biomass_MG)
  ) |>
  arrange(Biomass_MG)

ggplot(data = biomass.df, aes(x = Species, y = Biomass_MG)) + geom_col() + ggtitle("Wenatchee-Entiat Year 0 Total Biomass by species.")  +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


if (!file.exists(file.path(landisOutputDir, "ageBiomassSecondary", "TotalBiomass.tif"))){
  TotalBiomass.r <- zero.r
  
  for (stack in ageBiomassStacks) {
    r <- rast(file.path(landisOutputDir, "ageBiomassOutput", stack))
    # r <- ifel(is.na(r), 0, r)
    
    TotalBiomass.r <- TotalBiomass.r + r
  }
  
  writeRaster(TotalBiomass.r, file.path(landisOutputDir, "ageBiomassSecondary", "TotalBiomass.tif"))
} else {
  TotalBiomass.r <- rast(file.path(landisOutputDir, "ageBiomassSecondary", "TotalBiomass.tif"))
}


plot(TotalBiomass.r)


#

# Tally biomass under 40 years

if (!file.exists(file.path(landisOutputDir, "ageBiomassSecondary", "TotalYoungTreeBiomass.tif"))){
  TotalYoungTreeBiomass.r <- zero.r
  
  for (stack in ageBiomassStacks) {
    if (str_detect(stack, "age0-") | str_detect(stack, "age10-")  | str_detect(stack, "age20-")  | str_detect(stack, "age30-")){
      print(stack)
      r <- rast(file.path(landisOutputDir, "ageBiomassOutput", stack))
      # r <- ifel(is.na(r), 0, r)
      
      TotalYoungTreeBiomass.r <- TotalYoungTreeBiomass.r + r
    }
  }
  
  writeRaster(TotalYoungTreeBiomass.r, file.path(landisOutputDir, "ageBiomassSecondary", "TotalYoungTreeBiomass.tif"))
} else {
  TotalYoungTreeBiomass.r <- rast(file.path(landisOutputDir, "ageBiomassSecondary", "TotalYoungTreeBiomass.tif"))
}


plot(TotalYoungTreeBiomass.r)

# Now detect decreases and overlay with fire footprints

YoungTreeLoss.r <- TotalYoungTreeBiomass.r[[2:simLength]] - TotalYoungTreeBiomass.r[[1:(simLength - 1)]]
YoungTreeLoss.r <- ifel(YoungTreeLoss.r > 0, 0, YoungTreeLoss.r)
plot(YoungTreeLoss.r[[11]])
plot(severityStackSmoothedClassified.r[[11]])

YoungTreeLowModFireMortality.r <- ifel(severityStackSmoothedClassified.r %in% c(1, 2, 3), YoungTreeLoss.r, 0)

YoungTreeLowModFireMortality.df <- zonal(YoungTreeLowModFireMortality.r, pwg.r, fun = 'sum') |>
  pivot_longer(cols = starts_with("fire-dnbr-"), names_to = "Year", values_to = "Total_Mortality", names_prefix = "fire-dnbr-") |>
  mutate(Year = as.numeric(Year), Total_Mortality_MG = Total_Mortality * -1 * 0.01 * 0.81) |>
  group_by(PWG) |>
  arrange(Year) |>
  mutate(Cumulative_Mortality_MG = cumsum(Total_Mortality_MG))

p <- ggplot(data = YoungTreeLowModFireMortality.df, aes(x = Year, y = Cumulative_Mortality_MG)) + geom_line() + facet_wrap(~PWG)
p

# TotalBiomass.r <- app(ageBiomass.sds, fun = "sum", na.rm = T)
# plot(TotalBiomass.r)

### Dominant species map
### Sum all cohorts by species, then find code of greatest by biomass

totalBiomassSpp.r <- list()

for (spp in treeSpp) {
  sppStacks <- ageBiomassStacks[str_detect(ageBiomassStacks, spp)]
  spp.biomass.r <- rast(file.path(landisOutputDir, "ageBiomassOutput", sppStacks))
  
  totalBiomassSpp.r[[spp]] <- sum(spp.biomass.r)
}


### Mean age dominant species



### Mean age all species (weighted by biomass)
# group and sum by cohort, then do weighted mean, exclusing grasses/shrubs

meanAge.r <- list()

for (yr in 0:simLength){
  
  yrCohort <- rast(file.path(ageBiomassOutput, treeStacks)) |>
    select(contains(paste0('-', yr)))
  
  yrCohort.df <- as.data.frame(yrCohort, xy = T) |>
    pivot_longer(starts_with(spp), names_to = c("Species", "Cohort", "Year"), names_sep = "-", values_to = "Biomass_gm2")
    
  
  # for (spp in treeSpp) {
  #   
  #   
  #   
  # }
}


### Filter biomass by in road network
mgmt.r <- rast(file.path(modelDir, LANDIS.EXTENT, paste0("ext_BiomassHarvestMgmt_", LANDIS.EXTENT, ".tif")))
mgt_forest.r <- ifel(mgmt.r <=3, 1, 0) 

roads.no.wild.r<-rast(file.path(dataDir,'PWG',paste0("roads_noWild_",LANDIS.EXTENT,"_45m.tif"))) |>
  buffer(183)  # 800 feet yarding distance
roads_mgmt.r <- ifel(mgt_forest.r == 1 & roads.no.wild.r, 1, 0)
# mgmt_access.r <- roads_mgmt.r + mgt_forest.r

fire_in_mgmt_footprint.r <- terra::mask(severityStackSmoothedClassified.r, roads_mgmt.r)
plot(fire_in_mgmt_footprint.r)


tree_biomass.r <- rast(file.path(landisOutputDir, "ageBiomassOutput", treeStacks))
tree_biomass_mgmt.r <- ifel(roads_mgmt.r == 1, tree_biomass.r, NA)
names(tree_biomass_mgmt.r) <- names(tree_biomass.r)


# zones.r <- ifel(mgmt_access.r == 2, mtbs_mgmt.r + 1, (mtbs_mgmt.r * -1) - 1)  # if the area is managed but unreachable by road, invert the severity scale
# zones.r <- ifel(mgmt_access.r == 0, NA, zones.r)

summary.df <- data.frame()

for (yr in 2:40){
  print(yr)
  yr_biomass.r <- tree_biomass.r |> select(ends_with(paste0("-", yr)))
  last_yr_biomass.r <- tree_biomass.r |> select(ends_with(paste0("-", yr-1)))
  
  biomass_delta.r <- last_yr_biomass.r - yr_biomass.r
  # biomass_delta.r <- ifel()
  
  yr_fire.r <- fire_in_mgmt_footprint.r[[yr]]
  
  # yr_biomass_footprint.r <- ifel(!is.na(yr_fire.r), yr_biomass.r, 0)
  # names(yr_biomass_footprint.r) <- names(yr_biomass.r)
  
  year_fire_summary.df <- zonal(yr_biomass.r, yr_fire.r, fun = "sum", na.rm = T) |>
    rename("Severity" = paste0("fire-dnbr-", yr)) |>
    pivot_longer(ends_with(paste0("-", yr)), names_to = c("Species", "Cohort", "Year"), names_sep = "-", values_to = "Biomass_gm2") |>
    mutate(Age = str_replace(Cohort, "age", "") |> as.numeric(), BiomassMG = Biomass_gm2 * 0.01 * 0.81, Year = as.numeric(Year)) |>
    mutate(Partition = case_when(
      Species == "HardWood" & Age == 10 ~ "Saplings & Hardwoods",
      Species == "Hardwood" ~ NA,
      Age <= 30 ~ "Saplings & Hardwoods",
      Age <= 120 ~ "Mature",
      .default = NA
    )) |>
    filter_out(is.na(Partition)) |>
    mutate(
      MerchMG = ifelse(Partition == "Mature", BiomassMG * 0.55, 0),
      ChipMG = ifelse(Partition == "Mature", BiomassMG * 0.45, BiomassMG),
      Status = "Live"
    )
  
  year_mortality_summary.df <- zonal(biomass_delta.r, yr_fire.r, fun = "sum", na.rm = T) |>
    rename("Severity" = paste0("fire-dnbr-", yr)) |>
    pivot_longer(ends_with(paste0("-", yr-1)), names_to = c("Species", "Cohort", "Year"), names_sep = "-", values_to = "Biomass_gm2") |>
    mutate(Age = str_replace(Cohort, "age", "") |> as.numeric(), BiomassMG = Biomass_gm2 * 0.01 * 0.81, Year = as.numeric(Year)) |>
    mutate(Partition = case_when(
      Species == "HardWood" & Age == 10 ~ "Saplings & Hardwoods",
      Species == "Hardwood" ~ NA,
      Age <= 30 ~ "Saplings & Hardwoods",
      Age <= 120 ~ "Mature",
      .default = NA
    )) |>
    filter_out(is.na(Partition)) |>
    mutate(
      MerchMG = ifelse(Partition == "Mature", BiomassMG * 0.55, 0),
      ChipMG = ifelse(Partition == "Mature", BiomassMG * 0.45, BiomassMG),
      Status = "Dead"
    )
  
  summary.df <- summary.df |> bind_rows(year_fire_summary.df, year_mortality_summary.df)
  
}

df <- summary.df |> 
  mutate(Year = as.numeric(Year)) |>
  group_by(Partition, Year, Severity, Status) |>
  summarise(ChipMG = sum(ChipMG), MerchMG = sum(MerchMG)) |>
  ungroup() |>
  pivot_longer(cols = c(ChipMG, MerchMG), names_to = "Product", values_to = "BiomassMG") |>
  group_by(Product, Severity, Status) |>
  arrange(Year) |>
  mutate(BiomassMG_cumulative = cumsum(BiomassMG)) 

ggplot(data = df, aes(x = Year, y = BiomassMG_cumulative, color = Product, linetype = Status)) + geom_line() + facet_wrap(~Severity) + ylab("Cumulative Biomass (MG)")
















