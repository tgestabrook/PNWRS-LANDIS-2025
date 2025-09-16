library(tidyverse)
library(cffdrs)

ignLog.df <- read.csv("F:\\2025_Q3_Scenarios\\TripodNoSupp\\LANDIS_Sim_Tripod_baseclim_Sc99_20250912_1113\\socialclimatefire-ignitions-log.csv")

ignCounts.df <- ignLog.df |>
  mutate(IgnitionType = str_remove(IgnitionType, " ")) |>
  group_by(SimulationYear, IgnitionType) |>
  summarise(IgnitionCount = sum(ActualNumberIgnitions)) |>
  pivot_wider(names_from = "IgnitionType", values_from = "IgnitionCount", names_prefix = "Ignitions")


par(mfrow = c(1,2))
hist(ignCounts.df$IgnitionsLightning, breaks = c(0, 12, 25, 37, 50, 62, 75, 87), labels = T)


hist(ignCounts.df$IgnitionsAccidental, breaks = c(0, 12, 25, 37, 50, 62, 75, 87), labels = T)


clim.dat<-read.csv(file.path(dataDir,paste0('Climate_and_FWI_', LANDIS.EXTENT ,'.csv'))) # Note: If you change this to Wen_Ent, you may get different coefficients

aoi_ext = ext(project(rast(file.path("Tripod", "ECOREGIONS_Tripod.tif")), 'EPSG:4326'))
lat = unname(0.5*(aoi_ext$ymin + aoi_ext$ymax))
lon = unname(0.5*(aoi_ext$xmin + aoi_ext$xmax))

fwi.df<-data.frame('lat'=lat,'long'=lon,'yr' = clim.dat$yr,'mon' = clim.dat$mon,'day' = clim.dat$day,
                   'temp' = clim.dat$tasmax,'rh' = clim.dat$rhsmin,'ws' = clim.dat$WndSpd * 3.6,'prec' = clim.dat$pr)

fwi.df<-fwi(fwi.df)

clim.dat$FWI <- fwi.df$FWI
