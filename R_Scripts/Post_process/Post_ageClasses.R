########################################################################################################################-
#### Calculate the dominant three species and mean ages thereof from LANDIS biomass & age output. ######################-
#-----------------------------------------------------------------------------------------------------------------------#

MedAgeAllspp.stack <- rast(file.path(ageOutput, dir(ageOutput)[grepl("yr-MED", dir(ageOutput))]))
MaxAgeAllspp.stack <- rast(file.path(ageOutput, dir(ageOutput)[grepl("yr-MAX", dir(ageOutput))]))

if (!file.exists(file.path(ageOutput, 'MeanAge_AllSpp-yr.tif'))){
  cat("\n\nCalculating annual mean & max age...")
  grouping_index <- names(MedAgeAllspp.stack) |> str_extract("\\d+") |> as.integer() 
  MeanAge.stack <- MedAgeAllspp.stack |> tapp(index = grouping_index, fun = "mean", na.rm = T)
  writeRaster(MeanAge.stack,file.path(ageOutput, 'MeanAge_AllSpp-yr.tif'),overwrite=T)
  
  grouping_index <- names(MaxAgeAllspp.stack) |> str_extract("\\d+") |> as.integer() 
  MaxAge.stack <- MaxAgeAllspp.stack |> tapp(index = grouping_index, fun = "max", na.rm = T)
  writeRaster(MeanAge.stack,file.path(ageOutput, 'MaxAge_AllSpp-yr.tif'),overwrite=T)
}
 
### CORE LOOP: -----------------------------------------------------------------
if (
  file.exists(file.path(landisOutputDir, 'ageOutput','MeanAge_DominantSpecies-yr.tif')) & 
  file.exists(file.path(landisOutputDir, 'ageOutput','MeanAge_TopThreeSpecies-yr.tif')) & 
  file.exists(file.path(landisOutputDir, 'ageOutput','DominantSpecies-yr.tif')) & 
  file.exists(file.path(landisOutputDir, 'ageOutput','DominantSpeciesTwo-yr.tif')) &
  file.exists(file.path(landisOutputDir, 'ageOutput','DominantSpeciesThree-yr.tif'))
) {
  cat("Ages already processed") ### Do nothing
} else {
  cat('\n\n----------------------------------------------------------------------------------\nLooping through years to calculate dominant species & age...\n----------------------------------------------------------------------------------\n')
  
  n_cores <- detectCores()
  cluster <- makeCluster(min(n_cores-1, 4))
  
  registerDoParallel(cluster)
  
  start.time <- Sys.time()
  
  foreach (yr = unique(yrs),  .packages = c("terra", "tidyverse", "tidyterra"), .export = c("get_ages_of_top3_biomass"), .verbose = T, .inorder=F, .final = function(x) NULL) %dopar% {
    if(
      file.exists(file.path(landisOutputDir, 'ageOutput',paste0('MeanAge_DominantSpecies-', yr, '.tif'))) &
      file.exists(file.path(landisOutputDir, 'ageOutput',paste0('MeanAge_TopThreeSpecies-', yr, '.tif'))) &
      file.exists(file.path(landisOutputDir, 'ageOutput',paste0('DominantSpecies-', yr, '.tif'))) &
      file.exists(file.path(landisOutputDir, 'ageOutput',paste0('DominantSpeciesTwo-', yr, '.tif'))) &
      file.exists(file.path(landisOutputDir, 'ageOutput',paste0('DominantSpeciesThree-', yr, '.tif')))
    ) {
      cat(paste0("Age rasters for year ", yr, "already processed."))
    } else {
      biomass.trees <- rast(file.path(biomassOutput, dir(biomassOutput)[grepl("yr-biomass", dir(biomassOutput))])) |> 
        select(!starts_with(c("Nfixer_Resprt","NonFxr_Resprt","NonFxr_Seed","Grass_Forb","TotalBiomass"))) |>
        select(contains(paste0('-', yr, '-')))
      
      med.age<-rast(file.path(ageOutput, dir(ageOutput)[grepl("yr-MED", dir(ageOutput))])) |> 
        select(contains(paste0('-', yr, '-'))) |> 
        select(!starts_with(c("Nfixer_Resprt","NonFxr_Resprt","NonFxr_Seed","Grass_Forb","TotalBiomass")))
      
      # start.time <- Sys.time()
      top3sp.r <- app(biomass.trees, order, decreasing = T)[[1:3]]
      names(top3sp.r) <- c("BiomassRank1", "BiomassRank2", "BiomassRank3")
      # print(Sys.time() - start.time)
      
      # start.time <- Sys.time()
      age.top.3.dom.r <- app(c(top3sp.r, med.age), get_ages_of_top3_biomass)
      # print(Sys.time() - start.time)
      
      cat('\n-> Calculating mean age of dominant 3 species per site')
      mean.age.top3.dom.r <- mean(age.top.3.dom.r, na.rm = T) |> round(0)
      mean.age.domSpp.r <- age.top.3.dom.r[[1]] |> round(0)
      
      # dominant.spp
      cat('\n-> Generating dominant species raster')
      dominant.spp <- top3sp.r[[1]]
      dominant.spp2 <- top3sp.r[[2]]
      dominant.spp3 <- top3sp.r[[3]]
      
      if (!file.exists(file.path(ageOutput, 'MeanAge_DominantSpecies-yr.tif'))){
        writeRaster(mean.age.domSpp.r,file.path(ageOutput, paste0('MeanAge_DominantSpecies-',yr,'.tif')),overwrite=T)
        writeRaster(mean.age.top3.dom.r,file.path(ageOutput, paste0('MeanAge_TopThreeSpecies-',yr,'.tif')),overwrite=T)
        writeRaster(dominant.spp,file.path(ageOutput, paste0('DominantSpecies-',yr,'.tif')),overwrite=T)
        writeRaster(dominant.spp2,file.path(ageOutput, paste0('DominantSpeciesTwo-',yr,'.tif')),overwrite=T)
        writeRaster(dominant.spp3,file.path(ageOutput, paste0('DominantSpeciesThree-',yr,'.tif')),overwrite=T)
      }
      rm(list = c("mean.age.domSpp.r", "mean.age.top3.dom.r", "dominant.spp", "dominant.spp2", "dominant.spp3", "age.top.3.dom.r", "biomass.trees", "med.age", "top3sp.r"))
      gc()
    }
  }
  
  stopImplicitCluster()
  print(Sys.time() - start.time)
  
  gc()
}

#### Replace loose rasters with stacks: ----
flip_rasters <- FALSE
for(folder in c('ageOutput')){
  cat(paste0('\nCreating stacks...', folder, '...'))
  
  ### Remove tif.aux.xml
  auxxml <- files <- list.files(path = file.path(landisOutputDir,folder), pattern = "\\.aux.xml$", full.names = TRUE)
  file.remove(auxxml)
  
  files<-dir(file.path(landisOutputDir,folder))
  if(length(files)==0) next
  
  ### Get the unique map types: ----
  mapTypes <- str_replace(files, '(\\d+)', '(\\\\d+)') |>  # replace year numbers with generic number matching string for use later
    unique()  # get the unique map types
  
  for(mapType in mapTypes){
    outName <- str_replace(mapType, '\\(\\\\d\\+\\)', 'yr') |> str_replace('.img', '.tif')
    if (file.exists(file.path(landisOutputDir, folder, outName))){next}
    
    cat(paste0(mapType, '...'))
    oldMaps <- dir(file.path(landisOutputDir, folder))[grepl(paste0("^", mapType), dir(file.path(landisOutputDir, folder)))]  # get the names of the maps to delete after loading
    stack <- get_maps(folder, mapType)
    
    writeRaster(stack, file.path(landisOutputDir, folder, outName))
    file.remove(file.path(landisOutputDir, folder, oldMaps))  # remove the old loose map files
  }
  
}

gc()





