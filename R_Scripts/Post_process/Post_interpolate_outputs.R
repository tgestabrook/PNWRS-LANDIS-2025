start_time <- Sys.time()

n_cores <- detectCores()
cluster <- makeCluster(min(n_cores-1, 4))

registerDoParallel(cluster)

for (folder in c("ageOutput", "biomassOutput", "NECN")){
  cat(paste0('\nInterpolating raster stacks in ', folder))

  files <- dir(file.path(landisOutputDir,folder))
  files <- files[grepl(".tif", files)]

  foreach (stack = files, .packages = c("terra", "stringr"), .inorder = F) %dopar% {
    s <- rast(file.path(landisOutputDir, folder, stack))
    
    if (nlyr(s) %in% c(1, 2, 3, 4)){# if there is already a layer for each year, or if it's a single layer, or 3 layers in the case of mean age of top 3 species
    } else if (nlyr(s) %in% c(simLength, simLength+1)) {
      if (folder%in%c('biomassOutput', 'ageOutput') & "FLT4S"%in%datatype(s)){
        writeRaster(as.int(s), file.path(landisOutputDir, folder, stack), overwrite=T, datatype="INT4S")
      }
    } else if (nlyr(s) > simLength+1) {
      warning(paste("Raster stack", stack, "has too many layers!"))
      
    } else {

      if (folder == 'NECN'){  # grab year zero NECN from single-year simulation
        y0 <- rast(file.path(dataDir,'NECN_Outputs_Yr_0', LANDIS.EXTENT, str_replace(stack, 'yr', '1')))
        names(y0) <- str_replace(stack, 'yr', '0') |> str_replace(".tif", '')
        s <- c(y0, s)
      }

      s <- interpolateRaster(s)
      
      if (folder%in%c('biomassOutput', 'ageOutput')){
        dtype = "INT4S"  # turn biomass and age rasters into integers to make some processing faster
      } else {dtype = "FLT4S"}
      
      writeRaster(s, file.path(landisOutputDir, folder, stack), overwrite=T, datatype = dtype)
      cat("...done!")
    }
  }
}

stopImplicitCluster()
gc()

print(Sys.time() - start_time)




