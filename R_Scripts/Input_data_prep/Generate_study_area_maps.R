
### PWG: -----

### Land use: -----

### DEM: -----

### PatchID: -----



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








































