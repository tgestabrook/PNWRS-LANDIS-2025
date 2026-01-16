### This script takes in DHSVM outputs

### merge wenatchee and entiat outputs by scenario

### 

HUC10.all.sf <- vect(file.path(dataDir, "PWG", "HUC10_Wen_Ent_Oka_Met.shp"))

ent.mask.r <- rast(file.path(dataDir, "DHSVMBasinMasks", "entiat_mask.asc"))
crs(ent.mask.r) <- crs(HUC10.all.sf)

wen.mask.r <- rast(file.path(dataDir, "DHSVMBasinMasks", "wena_mask.asc"))
crs(wen.mask.r) <- crs(HUC10.all.sf)

oka.mask.r <- rast(file.path(dataDir, "DHSVMBasinMasks", "okan_mask.asc")) 
crs(oka.mask.r) <- crs(HUC10.all.sf)

met.mask.r <- rast(file.path(dataDir, "DHSVMBasinMasks", "methow_mask.asc")) 
crs(met.mask.r) <- crs(HUC10.all.sf)


if (LANDIS.EXTENT == "WenEnt"){
  dhsvm_sim.prefix <- "wenent"
  subbasin_folders <- c("Wenatchee", "Entiat")
  subbasin_masks <- list(
    "Wenatchee" = wen.mask.r,
    "Entiat" = ent.mask.r
  )
}

dhsvm_output_scenarios <- dir(file.path(DHSVM_dir, subbasin_folders[[1]]))

for (maptype in c("MaxSwe_", "MaxSweDate_", "MeltOutDate_")){
  for (scenario in dhsvm_output_scenarios){
    scenario_shortened <- str_remove(scenario, dhsvm_sim.prefix)  # get scenarioname without subbasin name
    
    ### merge mean annual flow dataframes
    merged_annual_mean.df <- foreach(subbasin_folder = subbasin_folders, .combine = dplyr::left_join, .maxcombine = 2) %do% {
      df <- read_csv(file.path(DHSVM_dir, subbasin_folder, scenario, paste0(scenario, ".meter.annual_mean.csv"))) |> rename("Year" = "...1")
    }
    write_csv(merged_annual_mean.df, file.path(DHSVM_dir, LANDIS.EXTENT, paste0("MeanAnnualFlow_", scenario_shortened, ".csv")))
      
    if(!file.exists(file.path(DHSVM_dir, LANDIS.EXTENT, paste0(maptype, scenario_shortened, '.tif')))){
      combined_stack <- foreach(subbasin_folder = subbasin_folders, .combine = terra::merge, .maxcombine = 2) %do% {
        loose_files <- dir(file.path(DHSVM_dir, subbasin_folder, scenario)) |> 
          (\(x) x[startsWith(x, maptype)])() 
        s <- rast(file.path(DHSVM_dir, subbasin_folder, scenario, loose_files)) |>
          mask(subbasin_masks[[subbasin_folder]], maskvalue = 0)
      }
      crs(combined_stack) <- crs(HUC10.all.sf)
      writeRaster(combined_stack, file.path(DHSVM_dir, LANDIS.EXTENT, paste0(maptype, scenario_shortened, '.tif')))
    }
  }
}


# test1 <- terra::merge(ifel(ent.mask.r == 1, 1, NA), wen.mask.r)
# plot(test1)
# 
# 
# 
# 
# subdir_files <- dir(file.path())
# maps <- subdir_files[grepl(paste0('^', name_prefix), subdir_files)]  # get files in the directory that match the output map type
# 
# maps <- maps[order(as.numeric(str_extract(maps, "(\\d+)")))]  # sort by year
# map_paths <- file.path(landisOutputDir, subdir, maps)
# maprs <- rast(map_paths)
# 
# names(maprs) <- str_replace(maps, '.tif', '') |>
#   str_replace('.img', '') |>
#   str_replace('.asc', '')
# 
# if(flip_rasters){maprs <- flip(maprs)}  # flip if upside-down
# 
# crs(maprs) <- crs(ecos.r)  # update CRS and extent
# ext(maprs) <- ext(ecos.r)
# 
