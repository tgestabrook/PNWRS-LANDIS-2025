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
# if(!suppressWarnings(suppressMessages(require(rgdal)))) install.packages('rgdal',repos="https://cloud.r-project.org")
# suppressWarnings(suppressMessages(library(rgdal)))
# if(!suppressWarnings(suppressMessages(require(terra)))) install.packages('terra')
# suppressWarnings(suppressMessages(library(terra)))
# if(!suppressWarnings(suppressMessages(require(rgeos)))) install.packages('rgeos',repos="https://cloud.r-project.org")
# suppressWarnings(suppressMessages(library(rgeos)))
if(!suppressWarnings(suppressMessages(require(sp)))) install.packages('sp',repos="https://cloud.r-project.org")
suppressWarnings(suppressMessages(library(sp)))

### To prevent the creation of .tif.aux.xml files:
#invisible(rgdal::setCPLConfigOption("GDAL_PAM_ENABLED", "FALSE"))

#-----------------------------------------------------------------------------------------------------------------------
Dir<-getwd() # Location of LANDIS-II Model

#-----------------------------------------------------------------------------------------------------------------------
### Write LANDIS-II extension information: ----
cat('\n###########################################################################################################
-----------------------------------\n   LANDIS-II v7 Extension summary\n-----------------------------------
############################################################################################################\n')
extensions<-dir(file.path(R.home(),'../../LANDIS-II-v7/extensions'))
extensions<-extensions[grepl('.dll',extensions)]

## Old hack:
# necn.ext<-max(ext[grepl('NECN',ext)])
# cat('NECN:',substr(necn.ext,nchar(necn.ext)-5,nchar(necn.ext)-4),'\n')

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

#necn.ext<-system(paste0("powershell (Get-Command ",file.path(R.home(),'../../LANDIS-II-v7/extensions',ext[grepl('NECN',ext)]),").FileVersionInfo.FileVersion"),intern=T)
#fire.ext<-system(paste0("powershell (Get-Command ",file.path(R.home(),'../../LANDIS-II-v7/extensions',ext[grepl('SCRAPPLE',ext)]),").FileVersionInfo.FileVersion"),intern=T)
#harvest.ext<-system(paste0("powershell (Get-Command ",file.path(R.home(),'../../LANDIS-II-v7/extensions',ext[grepl('BiomassHarvest',ext)]),").FileVersionInfo.FileVersion"),intern=T)
#product version for climate library:
#climate.ext<-system(paste0("powershell (Get-Command ",file.path(R.home(),'../../LANDIS-II-v7/extensions',ext[grepl('Climate',ext)]),").FileVersionInfo.ProductVersion"),intern=T)

#cat(paste0('NECN: v',necn.ext,'\n'))
#cat(paste0('SCRAPPLE: v',fire.ext,'\n'))
#cat(paste0('Biomass Harvest: v',harvest.ext,'\n'))
#cat(paste0('Climate Library: v',climate.ext,'\n\n'))

# cat('working dir:',getwd(),'\n')
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

# cat('Moving IC output files to a sub-folder...\n')
# 
# if(T %in% grepl('community-input-file',dir())){
#   dir.create('IC_Output')
#   for(i in dir()[grepl('community-input-file',dir())|grepl('output-community',dir())]){
#     invisible(file.copy(i,file.path('IC_Output',i)))
#     invisible(file.remove(i))
#   }
# }
# 
# toRemove<-dir('IC_Output')[grepl('.csv',dir('IC_Output')) | 
#                             grepl('.img',dir('IC_Output')) | 
#                             grepl('.txt',dir('IC_Output'))]
# toKeep<-toRemove[#grepl('50.csv',toRemove)|
#                    grepl('-30.img',toRemove)|grepl('-0.img',toRemove)|
#                    grepl('-0.txt',toRemove)|grepl('-20.txt',toRemove)|
#                    grepl('-30.txt',toRemove)|grepl('-40.txt',toRemove)|
#                    grepl('-50.txt',toRemove)|grepl('-80.txt',toRemove)|grepl('-100.txt',toRemove)]
# toRemove<-toRemove[!toRemove%in%toKeep]
# file.remove(file.path('IC_Output',toRemove))
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
### Convert IC_Output .img to .tifs: ----
imgs<-dir(file.path(Dir,'IC_Output'))
imgs<-imgs[grepl('.img',imgs)]
for(i in imgs){
  r<-rast(file.path(Dir,'IC_Output',i))  
  if(flip_rasters){r <- flip(r)}
  
  crs(r)<-crs(ecos)
  ext(r)<-ext(ecos)
  r[is.na(r)]<-0
  
  i2 <- gsub('.img', '.tif', i)
  writeRaster(r,file.path(Dir,'IC_Output',i2), datatype = "INT4S",overwrite=T)
  
  file.remove(file.path(Dir,'IC_Output',i))
}

### Fix IC_Output files. Remove duplicate ages. LANDIS code is so F'ing buggy! ----
cat('Fixing IC_Output files. Remove duplicate ages. LANDIS code is so Fing buggy! Checking text file:')

txt<-dir(file.path(Dir,'IC_Output'))
txt<-txt[grepl('.txt',txt)]
for(i in txt){
  cat('\n     - ',i,'...')
  
  ic<-read.delim(file.path(Dir,'IC_Output',i),skip=2,header=F)
  
  temp<-ic
  
  split<-strsplit(ic[,1]," ")
  
  age.biomass<-lapply(split,FUN=function(x)
    if(x[1]=='MapCode') return(x) else return(paste(x[seq(2,length(x),2)],x[seq(3,length(x),2)],sep=' ')))
  biomass<-lapply(split,FUN=function(x)
    if(x[1]=='MapCode') return(x) else return(x[seq(3,length(x),2)]))
  ages<-lapply(split,FUN=function(x) 
    if(x[1]=='MapCode') return(x) else return(x[seq(2,length(x),2)]))
  
  temp$length<-unlist(lapply(ages,FUN=function(x) return(length((x)))))
  temp$length.unique<-unlist(lapply(ages,FUN=function(x) return(length(unique(x)))))
  temp$age.biomass<-unlist(lapply(age.biomass,FUN=function(x) return(paste(unique(x),sep=' ',collapse=" "))))
  # temp$biomass<-unlist(lapply(biomass,FUN=function(x) return(paste(unique(x),sep=' ',collapse=" "))))
  
  # temp$ages<-unlist(lapply(ages,FUN=function(x) return(paste(unique(x),sep=' ',collapse=" "))))
  temp$duplicated.age<-unlist(lapply(ages,FUN=function(x) return(paste(x[duplicated(x)],sep=' ',collapse=" "))))
  
  temp[temp$length!=temp$length.unique,]
  
  if(nrow(temp[temp$length!=temp$length.unique,])>0){
    cat('Removing',nrow(temp[temp$length!=temp$length.unique,]),'duplicate cohorts created by buggy InitialCommunity Output code.')
    
    for(j in row.names(temp[temp$length!=temp$length.unique,])){
      sp<-unlist(strsplit(temp[j,'V1']," "))[1]
      age.biomass.unique<-unlist(strsplit(temp[j,'age.biomass'],temp[j,'duplicated.age']))
      age.biomass.unique<-paste(sp,age.biomass.unique[1],temp[j,'duplicated.age'],age.biomass.unique[length(age.biomass.unique)])
      temp[j,'V1']<-age.biomass.unique
    }
    ic[row.names(ic)%in%(row.names(temp[temp$length!=temp$length.unique,])),"V1"]<-temp[temp$length!=temp$length.unique,"V1"]
  }
  
  other.error.rows<-row.names( temp[(substr(temp$age.biomass,nchar(temp$age.biomass),nchar(temp$age.biomass))!=")" & !grepl('MapCode',temp$age.biomass))|
                                      grepl("[0-9] [0-9]",temp$age.biomass),])
  
  if(nrow(temp[temp$length!=temp$length.unique,])>0 | length(other.error.rows)>0){
    
    ic<-data.frame(ic[!row.names(ic) %in% other.error.rows,])
    
    colnames(ic)<-'LandisData "Initial Communities"'
    
    write.table(ic,file=file.path(Dir,'IC_Output',i),row.names=F,quote = F)
  }
}

cat('\n\n###################################################################################################################################
 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~          Post-processing COMPLETE!        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###################################################################################################################################\n')

#-----------------------------------------------------------------------------------------------------------------------
########################################################################################################################-
########################################################################################################################-
########################################################################################################################-