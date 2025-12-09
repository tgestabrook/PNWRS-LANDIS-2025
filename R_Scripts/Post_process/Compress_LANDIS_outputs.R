cat(paste('Compressing', landisOutputDir, '.............'), file = outFile, append = T)
print(paste('Compressing', landisOutputDir, '.............'))  # print in R console as well

if (file.exists(file.path(landisOutputDir, 'Climate-spinup-input-log.csv'))){
  file.remove(file.path(landisOutputDir, 'Climate-spinup-input-log.csv'))  # remove unused climate spinup log
}

### Load ecoregions: ----
eco_files<-dir(file.path(landisOutputDir,'Input_file_archive'))[grepl('.tif',dir(file.path(landisOutputDir,'Input_file_archive')))&
                                                             grepl('ECOREGIONS',dir(file.path(landisOutputDir,'Input_file_archive')))]
if(length(eco_files)==1) {
  ecos.r<-rast(file.path(landisOutputDir,'Input_file_archive',eco_files))
} else {stop("FIX ECOREGIONS in input file archive")}
ecos.r[ecos.r==0]<-NA

### Check if LANDIS outputs are flipped: ----
test_r <- rast(file.path(landisOutputDir, 'biomassOutput', 'TotalBiomass-0-biomass.tif')); crs(test_r) <- crs(ecos.r); ext(test_r) <- ext(ecos.r)
overlay_normal <- ifel(is.na(ecos.r)&test_r!=0, 1, 0)
overlay_flip <- ifel(is.na(ecos.r)&flip(test_r)!=0, 1, 0)
if(sum(values(overlay_normal, na.rm=T))>sum(values(overlay_flip, na.rm=T))){flip_rasters<-T}else{flip_rasters<-F}

cat('###################################################################################################################################
 Post-processing LANDIS-II outputs. Assigning CRS, applying LZW compression to .tiff files, and converting .img files to GeoTiff.
###################################################################################################################################\n\n', file = outFile, append = T)

get_maps <- function(subdir, name_prefix){
  subdir_files <- dir(file.path(landisOutputDir, subdir))
  maps <- subdir_files[grepl(name_prefix, subdir_files)]  # get files in the directory that match the output map type
  
  maps <- maps[order(as.numeric(str_extract(maps, "(\\d+)")))]  # sort by year
  map_paths <- file.path(landisOutputDir, subdir, maps)
  maprs <- rast(map_paths)

  names(maprs) <- str_replace(maps, '.tif', '') |>
    str_replace('.img', '')
  
  if(flip_rasters){maprs <- flip(maprs)}  # flip if upside-down
  
  crs(maprs) <- crs(ecos.r)  # update CRS and extent
  ext(maprs) <- ext(ecos.r)
  
  return(maprs)
}

for(folder in c('biomassOutput','ageOutput','Harvest', 'NECN','social-climate-fire', 'MagicHarvest')){
  cat(paste0('\n', folder, '...'))
  
  ### Remove tif.aux.xml
  auxxml <- files <- list.files(path = file.path(landisOutputDir,folder), pattern = "\\.aux.xml$", full.names = TRUE)
  file.remove(auxxml)
  
  files<-dir(file.path(landisOutputDir,folder))
  if(length(files)==0) next
  if(folder == "MagicHarvest" & !"R_log_MH.txt"%in%files){next}
  
  ### Get the unique map types: ----
  mapTypes <- str_replace(files, '(\\d+)', '(\\\\d+)') |>  # replace year numbers with generic number matching string for use later
    unique()  # get the unique map types
  
  if(folder == "MagicHarvest"){mapTypes <- c("MH_mgmt_areas_(\\d+).tif", "MH_stands_(\\d+).tif")}
  
  for(mapType in mapTypes){
    outName <- str_replace(mapType, '\\(\\\\d\\+\\)', 'yr') |> str_replace('.img', '.tif')
    if (file.exists(file.path(landisOutputDir, folder, outName))){next}
    
    cat(paste0(mapType, '...'))
    oldMaps <- dir(file.path(landisOutputDir, folder))[grepl(mapType, dir(file.path(landisOutputDir, folder)))]  # get the names of the maps to delete after loading
    stack <- get_maps(folder, mapType)
    
    writeRaster(stack, file.path(landisOutputDir, folder, outName))
    file.remove(file.path(landisOutputDir, folder, oldMaps))  # remove the old loose map files
  }
  
}

gc()

cat(paste('\n\nCompression routine complete.', Sys.time(), '\n'), file = outFile, append = T)


























