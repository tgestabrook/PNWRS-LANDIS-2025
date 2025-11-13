start_time <- Sys.time()

n_cores <- detectCores()
cluster <- makeCluster(min(n_cores-1, 2))

registerDoParallel(cluster)

for (folder in c("ageOutput", "biomassOutput", "NECN")){
  cat(paste0('\nInterpolating raster stacks in ', folder))

  files <- dir(file.path(landisOutputDir,folder))
  files <- files[grepl(".tif", files)]

  foreach (stack = files, .packages = c("terra", "stringr")) %dopar% {
    s <- rast(file.path(landisOutputDir, folder, stack))
    if (nlyr(s) %in% c(simLength, simLength+1, 1, 2, 3, 4)){# if there is already a layer for each year, or if it's a single layer, or 3 layers in the case of mean age of top 3 species I guess
      message(paste0('\n -  Skipping ', stack, ' due to layer count.'))
    } else {
      message(paste0('\n -  Interpolating ', stack))

      if (folder == 'NECN'){  # grab year zero NECN
        y0 <- rast(file.path(dataDir,'NECN_Outputs_Yr_0', LANDIS.EXTENT, str_replace(stack, 'yr', '1')))
        names(y0) <- str_replace(stack, 'yr', '0') |> str_replace(".tif", '')
        s <- c(y0, s)
      }

      s <- interpolateRaster(s)
      writeRaster(s, file.path(landisOutputDir, folder, stack), overwrite=T)
      cat("...done!")
    }
  }
}

stopImplicitCluster()
#rm(s)
gc()

print(paste("Parallel interpolation took", Sys.time() - start_time))


# 
# 
# start_time <- Sys.time()
# for (folder in c("ageOutput", "biomassOutput", "NECN")){
#   cat(paste0('\nInterpolating raster stacks in ', folder))
# 
#   files <- dir(file.path(landisOutputDir,folder))
#   files <- files[grepl(".tif", files)]
# 
#   for (stack in files) {
#     s <- rast(file.path(landisOutputDir, folder, stack))
#     if (nlyr(s) %in% c(simLength, simLength+1, 1, 2, 3, 4)){# if there is already a layer for each year, or if it's a single layer, or 3 layers in the case of mean age of top 3 species I guess
#       message(paste0('\n -  Skipping ', stack, ' due to layer count.'))
#     } else {
#       message(paste0('\n -  Interpolating ', stack))
# 
#       if (folder == 'NECN'){  # grab year zero NECN
#         y0 <- rast(file.path(dataDir,'NECN_Outputs_Yr_0', LANDIS.EXTENT, str_replace(stack, 'yr', '1')))
#         names(y0) <- str_replace(stack, 'yr', '0') |> str_replace(".tif", '')
#         s <- c(y0, s)
#       }
# 
#       s <- interpolateRaster(s)
#       writeRaster(s, file.path(landisOutputDir, folder, stack), overwrite=T)
#       cat("...done!")
#     }
#   }
# }
# print(paste("Regular interpolation took", Sys.time() - start_time))
# 
# 


