library(terra)
library(tidyverse)
library(tidyterra)

# set tmpdir to prevent running out of space
Sys.setenv(TMPDIR = "F:/R_TEMP")
terraOptions(tempdir = "F:/R_TEMP")

bigDataDir<-'F:/LANDIS_Input_Data_Prep/BigData'
dataDir<-'F:/LANDIS_Input_Data_Prep/Data'
modelDir<-'F:/V8_Models'

LANDIS.EXTENT <- "WenEnt"

aoi.shp <- vect(file.path(dataDir, paste0("Outline_", LANDIS.EXTENT, ".gpkg")))

### PWG: -----
### Clip master ecoregion raster to AOI: ----
if(file.exists(file.path(dataDir,'PWG',paste0("PWG_",LANDIS.EXTENT,".tif")))) {
  pwg.r <- rast(file.path(dataDir,'PWG',paste0("PWG_",LANDIS.EXTENT,".tif")))
} else {
  if (LANDIS.EXTENT %in% c('OkaMet', 'WenEnt', 'WenEntOkaMet')){# Use treatment patch area as a mask for pwg raster
    pwg.r <- crop(ecos.r,TreatPatch.shp, mask = T)  |>
      crop(TreatPatch.shp, mask = T)
  } else{
    pwg.r <- rasterize(TreatPatch.shp,ecos.r,field='PWG') |>
      crop(TreatPatch.shp, mask = T)
  }
  plot(pwg.r)
  
  writeRaster(pwg.r,file.path(dataDir,'PWG',paste0("PWG_",LANDIS.EXTENT,".tif")),overwrite=T)
}

### Land use: -----

### DEM: -----

### PatchID: -----

### Fine Fuels: -----
fuelmap.r <- rast(file.path(bigDataDir, "FuelMap2022", "FuelMap2022.tif"))
aoi_project.shp <- aoi.shp |> project(crs(fuelmap.r))
fuelmap.r <- fuelmap.r |>
  crop(aoi_project.shp) |>
  droplevels()
activeCat(fuelmap.r) <- "LITTER_CAR"


fuelmap.r <- fuelmap.r |>
  as.numeric() |>
  select("LITTER_CAR") |>
  project(pwg.r, method = "bilinear")

fuelmap.r <- ifel(is.na(fuelmap.r), 0, fuelmap.r * 0.11)   ## convert pounds per acre to g/m^2

writeRaster(fuelmap.r, file.path(modelDir, LANDIS.EXTENT, "NECN_input_maps", "FuelMap_Surface_Litter.tif"), overwrite = T)


### Wildlands buffers: -----
# if(!exists('wildlands.buffer')){
#   wildlands<-lua.r
#   wildlands[wildlands==12]<-1
#   wildlands[wildlands!=1]<-NA
#   
#   cat('Started at',as.character(Sys.time()),'\n')
#   wildlands.buffer<-buffer(wildlands,width=5000,doEdge=T) # This takes 13 minutes!
#   cat('Finished at',as.character(Sys.time()))
#   
#   plot(wildlands.buffer)
#   plot(wildlands,col='white',add=T)
#   
#   writeRaster(wildlands.buffer,file.path(dataDir,paste0("wildlands_5000m_buffer_",LANDIS.EXTENT,".tif")),overwrite=T)
# }
# 
# wildlands.inner.buffer <- rast(file.path(dataDir,paste0("wildlands_1610m_inner_buffer_",LANDIS.EXTENT,".tif")))  
# if(!exists('wildlands.inner.buffer')){
#   wildlands<-lua.r
#   wildlands[is.na(wildlands)]<-1
#   wildlands[wildlands!=12]<-1
#   wildlands[wildlands==12]<-NA
#   
#   cat('Started at',as.character(Sys.time()),'\n')
#   wildlands.inner.buffer<-buffer(wildlands,width=1610,doEdge=T) # 1 mile buffer. This takes ~5 minutes!
#   cat('Finished at',as.character(Sys.time()))
#   
#   wildlands.inner.buffer[is.na(lua.r)]<-NA
#   wildlands.inner.buffer[lua.r!=12]<-NA
#   
#   plot(lua.r)
#   plot(wildlands.inner.buffer,col='red',add=T)
#   
#   writeRaster(wildlands.inner.buffer,file.path(dataDir,paste0("wildlands_1610m_inner_buffer_",LANDIS.EXTENT,".tif")),overwrite=T)
# }








































