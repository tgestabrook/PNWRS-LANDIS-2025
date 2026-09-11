

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

YoungTreeLoss.r <- TotalYoungTreeBiomass.r[2:simLength] - TotalYoungTreeBiomass.r[[1:(simLength - 1)]]
YoungTreeLoss.r <- ifel(YoungTreeLoss.r > 0, 0, YoungTreeLoss.r)
plot(YoungTreeLoss.r[[7]])
plot(severityStackSmoothedClassified.r[[7]])

YoungTreeLowModFireMortality.r <- ifel(severityStackSmoothedClassified.r %in% c(1, 2, 3), YoungTreeLoss.r, 0)

YoungTreeLowModFireMortality.df <- zonal(YoungTreeLowModFireMortality.r, pwg.r, fun = 'sum')

p <- ggplot(data = YoungTreeLowModFireMortality.df) + geom_line() + facet_wrap(~PWG)


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








