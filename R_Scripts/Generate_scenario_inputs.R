
library(terra)
library(tidyverse)
library(tidyterra)
library(pscl)


bigDataDir<-'F:/LANDIS_Input_Data_Prep/BigData'
dataDir<-'F:/LANDIS_Input_Data_Prep/Data'
modelDir<-'F:/V8_Models'

#LANDIS.EXTENT<-'WenEnt'
#TreatPatch.shp <- vect(file.path(dataDir,'PWG',"TreatPatch_WenEnt.shp"))  # Treatment patches - combine watershed (HUC), topo-asp (combined topography and n-s aspect) land use with 4 ha mmu

LANDIS.EXTENT<-'Tripod'

#-----------------------------------------------------------------------------------------------------------------------
## Generate study area inputs 
dir.create(LANDIS.EXTENT)
dir.create(file.path(LANDIS.EXTENT, 'Climate'))
dir.create(file.path(LANDIS.EXTENT, 'NECN_input_maps'))
dir.create(file.path(LANDIS.EXTENT, 'SCRAPPLE_input_maps'))
dir.create(file.path(LANDIS.EXTENT, 'zClimate_Library'))
dir.create(file.path(LANDIS.EXTENT, 'MTBS_and_FOD_Fires', LANDIS.EXTENT))

### Dissolve Ecoregion shapefile to PWG level: ----
ecos.shp <- terra::aggregate(TreatPatch.shp, by='PWG', fun=mean, dissolve=T) |>
  mutate(PWG = ifelse(PWG == 99, 0, PWG)) |>
  mutate(Area = round(terra::expanse(., unit = 'ha'),0)) |>
  tidyterra::select(PWG, Area)

writeVector(ecos.shp,file.path(dataDir,'PWG', paste0('PWG_', LANDIS.EXTENT, '.gpkg')),overwrite = T)

if (  # if any of the input layers are unavailable
  !file.exists(file.path(dataDir, paste0("wildlands_5000m_buffer_",LANDIS.EXTENT,".tif"))) ||
  !file.exists(file.path(dataDir, paste0("wildlands_1610m_inner_buffer_",LANDIS.EXTENT,".tif")))
) {
  source("Input_data_prep/Generate_study_area_maps.R")
} else {
  pwg.r <- rast(file.path(dataDir,paste0('PWG/PWG_', LANDIS.EXTENT ,'.tif')))
  lua.r <- rast(file.path(dataDir,'PWG',paste0("LUA_", LANDIS.EXTENT ,".tif")))
  #wildlands.buffer <- rast(file.path(dataDir,paste0("wildlands_5000m_buffer_",LANDIS.EXTENT,".tif")))  
  wildlands.inner.buffer <- rast(file.path(dataDir,paste0("wildlands_1610m_inner_buffer_",LANDIS.EXTENT,".tif")))  
}


#-----------------------------------------------------------------------------------------------------------------------
## Initial communities from FIA 


#-----------------------------------------------------------------------------------------------------------------------
## Fit shade parameters


#-----------------------------------------------------------------------------------------------------------------------
## Climate inputs 


#-----------------------------------------------------------------------------------------------------------------------
## Soil inputs for NECN 


#-----------------------------------------------------------------------------------------------------------------------
## Harvest MGMT areas & stands 


#-----------------------------------------------------------------------------------------------------------------------
## SCRAPPLE ignition model fits 
fires.r <- rast(file.path(dataDir,paste0("Wildfires_1878_2019_", LANDIS.EXTENT ,".tif")))  # created in 0_StudyAreaMasks[...].R
ignitions.df <- vect(file.path(bigDataDir,"Fire_Ignitions_1992_2015_RDS-2013-0009/Fires_1992-2015_WA/Fires_1992-2015_WA.shp")) |>
  project(crs(pwg.r)) |>
  crop(as.polygons(pwg.r)) |>  # crop down to study AOI, probably a better way to do this
  mutate(yr = FIRE_YEAR, j_day = DISCOVERY1, IgnitionType = ifelse(STAT_CAU_1 == "Lightning", "Lightning", "Accidental")) |>
  group_by(yr, j_day, IgnitionType) |>  # group by year, julian day, and ignition type
  summarise(IgnitionCount = n()) |>
  as.data.frame() |>
  pivot_wider(names_from = "IgnitionType", values_from = "IgnitionCount", names_prefix = "Ignitions")


### Calculate FWI for fitting ignition model FIX FWI windspeed issue with km/hr vs m/s
#clim.dat<-read.csv(file.path(dataDir,paste0('Climate_and_FWI_', LANDIS.EXTENT ,'.csv'))) # Note: If you change this to Wen_Ent, you may get different coefficients

fires.by.fwi <- clim.dat |>
  left_join(ignitions.df, by=c('yr','j_day')) |>
  mutate(IgnitionsLightning = replace_na(IgnitionsLightning, 0),
         IgnitionsAccidental = replace_na(IgnitionsAccidental, 0)) |>
  filter(yr %in% 1992:2015)

# annualsummary.df <- fires.by.fwi |> group_by(yr) |>
#   summarise(IgnitionsAccidental = sum(IgnitionsAccidental), IgnitionsLightning = sum(IgnitionsLightning))
# hist(annualsummary.df$IgnitionsLightning)

zeroInf_lm_L <- zeroinfl(data=fires.by.fwi, IgnitionsLightning~FWI,dist="poisson")
summary(zeroInf_lm_L)


#-----------------------------------------------------------------------------------------------------------------------
## SCRAPPLE suppression maps 
Suppression.df <- read.csv(file.path(dataDir, "Suppression_by_LUA.csv")) |>
  pivot_longer(starts_with(LANDIS.EXTENT), names_to = "IgnType", values_to = "Suppression", names_prefix = paste0(LANDIS.EXTENT, '.')) |>
  rename(is = LUA, becomes = Suppression)

## Lightning: 
NatSuprEff.r <- ifel(wildlands.inner.buffer == 1, 13, lua.r) |> 
  classify(rcl = Suppression.df |> filter(IgnType == 'Nat') |> select(is, becomes))

AccSuprEff.r <- ifel(wildlands.inner.buffer == 1, 13, lua.r) |> 
  classify(rcl = Suppression.df |> filter(IgnType == 'Acc') |> select(is, becomes))

RxSuprEff.r <- ifel(wildlands.inner.buffer == 1, 13, lua.r) |> 
  classify(rcl = Suppression.df |> filter(IgnType == 'Rx') |> select(is, becomes))

writeRaster(NatSuprEff.r, file.path(LANDIS.EXTENT, "SCRAPPLE_input_maps",paste0('NatSuprEff','.tif')),overwrite=T,datatype='INT1U')
writeRaster(AccSuprEff.r, file.path(LANDIS.EXTENT, "SCRAPPLE_input_maps",paste0('AccSuprEff','.tif')),overwrite=T,datatype='INT1U')
writeRaster(RxSuprEff.r, file.path(LANDIS.EXTENT, "SCRAPPLE_input_maps",paste0('RxSuprEff','.tif')),overwrite=T,datatype='INT1U')

#-----------------------------------------------------------------------------------------------------------------------
## Generate plots







