library(terra)
library(tidyverse)
library(tidyterra)

bigDataDir <- 'F:/LANDIS_Input_Data_Prep/BigData' # Location of WA_FIA data and Riley dataset (RDS-2019-0026)
#dataDir<-'C:/Users/tuckf/Tuckers_Data/R/ORISE_R/LANDIS_R/Data' # Location of species codes, PWG raster, study area raster
dataDir <- 'F:/LANDIS_Input_Data_Prep/Data' # Location of species codes, PWG raster, study area raster

AOI <- rbind(
  vect(file.path(dataDir, "Outline_WenEnt.gpkg")),
  vect(file.path(dataDir, "Outline_OkaMet.gpkg"))
)

### Join winddir to ecoregion map???


### Get fuels from LANDFIRE
landfire_fuel.r <- rast(file.path(bigDataDir, "LANDFIRE_Fuels", "Tif", "us_105flm.tif"))
AOI.albers <- AOI |> project(crs(landfire_fuel.r)) |> buffer(30000)
landfire_fuel.r <- landfire_fuel.r |> crop(ext(AOI.albers))
activeCat(landfire_fuel.r) <- "LITTER"

litter.r <- catalyze(landfire_fuel.r) |> select("LITTER") |> project(crs(AOI), method = "bilinear") |> crop(ext(AOI))
plot(litter.r)

### convert from tons/acre to grams/sq meter, multiply by 247.11
fine_fuels.r <- litter.r * 247.11
plot(fine_fuels.r)

### cap at current max, 1500
fine_fuels.r <- ifel(fine_fuels.r > 1500, 1500, fine_fuels.r) |> terra::aggregate(fact = 3)
plot(fine_fuels.r); plot(AOI, add = T)


### perimeters from GEOMAC -- robbins uses centroid movement for spread
geomac_2000 <- vect(file.path(bigDataDir, "GEOMAC_perimeters", "Historic_Geomac_Perimeters_2000_-1118491216123023496.gpkg")) |> project(crs(AOI)) |> crop(ext(AOI))
geomac_2001 <- vect(file.path(bigDataDir, "GEOMAC_perimeters", "Historic_Geomac_Perimeters_2001_3482145466592074540.gpkg")) |> project(crs(AOI)) |> crop(ext(AOI))
geomac_2002 <- vect(file.path(bigDataDir, "GEOMAC_perimeters", "Historic_Geomac_Perimeters_2002_-6932490535327812361.gpkg")) |> project(crs(AOI)) |> crop(ext(AOI))
geomac_2003 <- vect(file.path(bigDataDir, "GEOMAC_perimeters", "Historic_Perimeters_2003.geojson")) |> project(crs(AOI)) |> crop(ext(AOI))
geomac_2004 <- vect(file.path(bigDataDir, "GEOMAC_perimeters", "Historic_Geomac_Perimeters_2004_-5890390374840480055.gpkg")) |> project(crs(AOI)) |> crop(ext(AOI))

geomac_all.sf <- rbind(geomac_2000, geomac_2001, geomac_2002, geomac_2003, geomac_2004)
geomac_all.df <- geomac_all.sf |> as.data.frame()

plot(AOI); plot(geomac_all, add = T)
