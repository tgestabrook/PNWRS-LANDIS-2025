########################################################################################################################-
#### Interpolate non-yearly LANDIS outputs to annual timesteps #########################################################-
#-----------------------------------------------------------------------------------------------------------------------#
gc()

start_time <- Sys.time()

n_cores <- detectCores()
cluster <- makeCluster(min(n_cores-1, 4))

registerDoParallel(cluster)

for (folder in c("ageOutput", "biomassOutput", "ageBiomassOutput", "NECN")){
  cat(paste0('\nInterpolating raster stacks in ', folder))
  if (!file.exists(file.path(landisOutputDir, folder))) {next}

  files <- dir(file.path(landisOutputDir,folder))
  files <- files[grepl(".tif", files)]
  
  gc()

  foreach (stack = files, .packages = c("terra", "stringr"), .inorder = F) %dopar% {
    s <- rast(file.path(landisOutputDir, folder, stack))
    
    if (nlyr(s) %in% c(1, 2, 3, 4)){# if there is already a layer for each year, or if it's a single layer, or 3 layers in the case of mean age of top 3 species
    } else if (nlyr(s) %in% c(simLength, simLength+1)) {
      if (folder%in%c('biomassOutput', 'ageOutput', 'ageBiomassOutput') & "FLT4S"%in%datatype(s)){
        writeRaster(as.int(s), file.path(landisOutputDir, folder, stack), overwrite=T, datatype="INT4S")
      }
    } else if (nlyr(s) > simLength+1) {
      warning(paste("Raster stack", stack, "has too many layers!"))
      
    } else {

      if ((folder == 'NECN') & (names(s)[1] != str_replace(stack, 'yr', '0') |> str_replace(".tif", ''))){  # grab year zero NECN from single-year simulation
        y0 <- rast(file.path(dataDir,'NECN_Outputs_Yr_0', LANDIS.EXTENT, str_replace(stack, 'yr', '1')))
        names(y0) <- str_replace(stack, 'yr', '0') |> str_replace(".tif", '')
        s <- c(y0, s)
      }
      
      gc()
      s <- interpolateRaster(s)
      
      if (folder == 'biomassOutput' | folder == "ageBiomassOutput"){
        dtype = "INT4S"  # turn biomass and age rasters into integers to make some processing faster
      } else if (folder == "ageOutput") {
        dtype = "INT2S"
      } else {dtype = "FLT4S"}
      
      writeRaster(s, file.path(landisOutputDir, folder, stack), overwrite=T, datatype = dtype)
      # cat("...done!")
    }
  }
}

stopImplicitCluster()
gc()

print(Sys.time() - start_time)







# 
# for (rname in dir(file.path(dataDir, 'NECN_Outputs_Yr_0', LANDIS.EXTENT))) {
#   r <- rast(file.path(dataDir, 'NECN_Outputs_Yr_0', LANDIS.EXTENT, rname))
#   
#   crs(r) <- crs(ecos.r)
#   ext(r) <- ext(ecos.r)
#   
#   rname_out <- str_replace(rname, "-2", "-1")
#   writeRaster(r, file.path(dataDir, 'NECN_Outputs_Yr_0', LANDIS.EXTENT, rname_out))
#   
# }


























