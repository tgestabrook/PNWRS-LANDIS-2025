########################################################################################################################-
########################################################################################################################-
########################################################################################################################-
###                 Process LANDIS-II Output Maps, Generate Human-readible Maps and Figs                          ######-
###                              USDA Forest Service PNWRS LANDIS-II MODEL                                        ######-
#-----------------------------------------------------------------------------------------------------------------------#
###   EXTENT: Wenatchee, Entiat, Okanogan, and Methow sub-basins                                                  ######-
###   PROJECT: BIOMASS Phase 3 for Pacific Northwest Research Station                                             ######-    
###   DATE: March 2021                                                                                            ######-   
#-----------------------------------------------------------------------------------------------------------------------#
###   Code developed by Tucker Furniss                                                                            ######-
###     Contact: tucker.furniss@usda.gov; tucker.furniss@gmail.com;                                               ######-
#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
### Set directories and load packages: ----
if(!suppressWarnings(suppressMessages(require(terra)))) install.packages('terra',repos="https://cloud.r-project.org")
suppressWarnings(suppressMessages(library(terra)))
#if(!suppressWarnings(suppressMessages(require(sp)))) install.packages('sp',repos="https://cloud.r-project.org")
#suppressWarnings(suppressMessages(library(sp)))

#-----------------------------------------------------------------------------------------------------------------------
Dir<-getwd() # Location of LANDIS-II Model

#-----------------------------------------------------------------------------------------------------------------------
### Write LANDIS-II extension information: ----
cat('\n###########################################################################################################
-----------------------------------\n   LANDIS-II v7 Extension summary\n-----------------------------------
############################################################################################################\n')
extensions<-dir(file.path(R.home(),'../../LANDIS-II-v7/extensions'))
extensions<-extensions[grepl('.dll',extensions)]

## Improved version:
# First, find correct files:
for(i in c('NECN','SCRAPPLE','BiomassHarvest','Climate')){
  if(length(extensions[grepl(i,extensions)])>1){
    
    versions<-as.numeric(gsub("[^\\d]+", "", extensions[grepl(i,extensions)], perl=TRUE))
    versions<-versions[substr(as.character(versions),1,1)==max(substr(as.character(versions),1,1))] # Drop old versions (e.g., v32 would override v4, but we want to keep 4)
    versions<-max(versions) # Now take max
    
    focal.extensions<-extensions[grepl(i,extensions)][grepl(versions,as.numeric(gsub("[^\\d]+", "", extensions[grepl(i,extensions)], perl=TRUE)))]
    
    extensions<-extensions[!grepl(i,extensions)|grepl(extensions[grepl(focal.extensions,extensions)],extensions)]
  }
    
}

#-----------------------------------------------------------------------------------------------------------------------
### Load ecoregions: ----
ecos<-dir(file.path('Input_file_archive'))[grepl('.tif',dir(file.path('Input_file_archive')))&grepl('ECOREGIONS',dir(file.path('Input_file_archive')))]
if(length(ecos)==0) {
  ecos<-dir('../')[grepl('.tif',dir('../'))&grepl('ECOREGIONS',dir('../'))]
  ecos<-rast(file.path('../',ecos))
} else ecos<-rast(file.path('Input_file_archive',ecos))
ecos[ecos==0]<-NA
#-----------------------------------------------------------------------------------------------------------------------
### Check if LANDIS outputs are flipped
test_r <- rast(file.path(Dir, 'biomassOutput', 'TotalBiomass-0-biomass.tif')); crs(test_r) <- crs(ecos); ext(test_r) <- ext(ecos)
overlay_normal <- ifel(is.na(ecos)&test_r!=0, 1, 0)
overlay_flip <- ifel(is.na(ecos)&flip(test_r)!=0, 1, 0)

if(sum(values(overlay_normal, na.rm=T))>sum(values(overlay_flip, na.rm=T))){flip_rasters<-T}else{flip_rasters<-F}

#-----------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------------
cat('###################################################################################################################################
 Post-processing LANDIS-II outputs. Assigning CRS, applying LZW compression to .tiff files, and converting .img files to GeoTiff.
###################################################################################################################################\n\n')
### Load all the geotiffs, compress using LZW lossless compression, and assign CRS! ----
for(folder in c('biomassOutput','ageOutput','Harvest')){
  tifs<-dir(file.path(Dir,folder))
  if(length(tifs)==0) next

  tifs<-tifs[grepl('.tif',tifs)]
  cat('Defining CRS and applying lossless LZW compression to',folder,'output maps...\n')
  for(i in tifs){
    r<-rast(file.path(Dir,folder,i))
    if(flip_rasters){r <- flip(r)}
    
    # if(!is.na(crs(r))) next
    # cat(i,', ')

    crs(r)<-crs(ecos)
    ext(r)<-ext(ecos)

    r[is.na(r)|r<0]<-0
    r[is.na(ecos)]<-NA

    writeRaster(r,file.path(Dir,folder,i),overwrite=T)
  }
}

### Now convert .img files to geotiffs, compress using LZW lossless compression, and assign CRS! ----
if(dir.exists(file.path(Dir,'social-climate-fire'))) file.rename(file.path(Dir,'social-climate-fire'),file.path(Dir,'scrapple-fire'))

for(folder in c('NECN','scrapple-fire')){  # do this for both the scrapple-fire and NECN folders
  imgs<-dir(file.path(Dir,folder))
  imgs<-imgs[grepl('.img',imgs)]
  if(length(imgs)==0) next
  
  cat('Defining CRS and applying lossless LZW compression to',folder,'output maps...\n')
  for(i in imgs){
    # cat(i,', ')
    r<-rast(file.path(Dir,folder,i))
    if(flip_rasters){r <- flip(r)}
    
    crs(r)<-crs(ecos)
    ext(r)<-ext(ecos)
    
    r[is.na(ecos)]<-NA
    
    i2 <- gsub('.img', '.tif', i)
    writeRaster(r,file.path(Dir,folder,i2),overwrite=T)
    
    file.remove(file.path(Dir,folder,i))
  }
}

cat('\n\n###################################################################################################################################
 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~          Post-processing COMPLETE!        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###################################################################################################################################\n')

#-----------------------------------------------------------------------------------------------------------------------
########################################################################################################################-
########################################################################################################################-
########################################################################################################################-