########################################################################################################################-
########################################################################################################################-
########################################################################################################################-
###                    Generate DHSVM-classified Vegetation Maps from LANDIS-II Outputs                          ######-
###                              and Generate Decision Support Tool layers                                       ######-
#----------------------------------------------------------------------------------------------------------------------#
###   EXTENT: Wenatchee, Entiat, Okanogan, and Methow sub-basins                                                 ######-
###   PROJECT: BIOMASS Phase 3 for Pacific Northwest Research Station                                            ######-    
###   DATE: December 2025                                                                                       ######-   
#----------------------------------------------------------------------------------------------------------------------#
###   Code developed by Tucker Furniss                                                                           ######-
###     Contact: tucker.furniss@usda.gov; tucker.furniss@gmail.com;                                              ######-
###   Modified by Thomas Estabrook; thomas.estabrook@usda.gov
#-----------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------#

wDir <- file.path('F:', "LANDIS_Input_Data_Prep", LANDIS.EXTENT)


#----------------------------------------------------------------------------------------------------------------------
### Trim output to wenatchee - entiat without 5-km buffer? ----
MASK = T

### Run DHSVM! ----

cat('\n\n  ***************************************************************************************************
Generating DHSVM output maps for',gsub(dirToProcess,"",landisOutputDir),'
***************************************************************************************************\n\n')
if(length(dir(landisOutputDir))==0) stop("LANDIS output folder is empty! Check file path...")
#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
#####  ---------------    Set initial variables    ------------- #####

names(yrs) <- as.numeric(yrs) + 2020

## Create output folder: ----
if(!dir.exists(file.path(landisOutputDir, 'DHSVM'))) dir.create(file.path(landisOutputDir, 'DHSVM'))


### Load LANDFIRE raster and definitions: ----
if(ext(landfire.r)!=ext(pwg.r)) stop('Extent of pwg.r does not match extent of landfire.r. This will cause major issues.')

#-----------------------------------------------------------------------------------------------------------------------
### Define LANDFIRE non-forest classes: ----
lf.codes.present<-unique(values(landfire.r))
landfire.codes[landfire.codes$VALUE%in%lf.codes.present,c('VALUE','EVT_GP_N','EVT_PHYS','EVT_LF')]

landfire.codes[landfire.codes$VALUE%in%lf.codes.present&grepl('agricultur',landfire.codes$EVT_GP_N,T),]

lf.ag.dry<-landfireReclassFUN(codes=c('Agricultural-Close Grown Crop','Agricultural-Fallow/Idle Cropland'))
lf.ag.moist<-landfireReclassFUN(codes=c('Agricultural-Wheat','Agricultural-Pasture and Hayland'))
lf.ag.vineyard<-landfireReclassFUN(codes='Agricultural-Vineyard')
lf.ag.orchard<-landfireReclassFUN(codes='Agricultural-Orchard')
# plot(lf.ag.orchard)

lf.urban<-landfireReclassFUN(codes=c('Developed-High Intensity','Developed-Low Intensity','Developed-Medium Intensity','Developed-Roads',
                                     'Developed-Upland Evergreen Forest'))
# plot(lf.urban)

lf.snowIce<-landfireReclassFUN(codes=c('Snow-Ice'))
# plot(lf.snowIce)

lf.barren<-landfireReclassFUN(codes=c('Sparse Vegetation'))
# plot(lf.barren)

lf.water<-landfireReclassFUN(codes=c('Open Water'))

lf.shrub<-landfireReclassFUN(codes=c('Deciduous Shrubland','Big Sagebrush Shrubland and Steppe','Transitional Shrub Vegetation',
                                     'Introduced Upland Vegetation-Shrub','Desert Scrub'))

lf.grass<-landfireReclassFUN(codes=c('Grassland','Transitional Herbaceous Vegetation','Grassland and Steppe',
                                     'Introduced Annual and Biennial Forbland','Introduced Annual Grassland',
                                     'Alpine Dwarf-Shrubland, Fell-field and Meadow'))

lf.nonForest.mask<-landfireReclassFUN(codes=c('Agricultural-Close Grown Crop','Agricultural-Fallow/Idle Cropland',
                                              'Agricultural-Wheat','Agricultural-Pasture and Hayland',
                                              'Agricultural-Vineyard','Agricultural-Orchard',
                                              'Developed-High Intensity','Developed-Low Intensity','Developed-Medium Intensity',
                                              'Developed-Upland Evergreen Forest','Developed-Roads',
                                              'Snow-Ice','Open Water','Sparse Vegetation'))

### Place extra DHSVM categories 161-173 so we don't have to recalc with every timestep!
extra_dhsvm.r <- rast(pwg.r, vals = NA)
### TODO shrubs and grass
### For ag land, urban, snow, water, overwrite LANDIS outputs based on Landfire EVT
extra_dhsvm.r <- ifel(pwg.r %in% 10:50 & lf.ag.dry, 165, extra_dhsvm.r)  # ag dry, 165
extra_dhsvm.r <- ifel(pwg.r %in% 10:50 & lf.ag.moist, 166, extra_dhsvm.r)  # ag moist, 166
extra_dhsvm.r <- ifel(pwg.r %in% 10:50 & lf.ag.vineyard, 167, extra_dhsvm.r)  # ag vineyard, 167
extra_dhsvm.r <- ifel(pwg.r %in% 10:50 & lf.ag.orchard, 168, extra_dhsvm.r)  # ag vineyard, 168
extra_dhsvm.r <- ifel(pwg.r %in% 10:50 & !is.na(lf.urban), 173, extra_dhsvm.r)  # urban, 173
extra_dhsvm.r <- ifel(pwg.r %in% 10:50 & !is.na(lf.snowIce), 171, extra_dhsvm.r)  # snow ice 171
extra_dhsvm.r <- ifel(pwg.r %in% c(10) & !is.na(lf.water), 170, extra_dhsvm.r)  # water 170

#-----------------------------------------------------------------------------------------------------------------------
### DHSVM Veg Reclass data frame (1-173): ----
ht.df <- data.frame(
  from = c( 0, 5, 10, 25),
  to = c(5, 10, 25, 115),
  becomes = c(2.5, 7.5, 17.5, 37.5)
)
fc.df <- data.frame(
  from = c(0, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80),
  to = c(0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 1),
  becomes = c(0.1, 0.25, 0.35, 0.45, 0.55, 0.65, 0.75, 0.9)
)

rcl.df<-expand.grid(
  fc = fc.df$becomes,
  ht = ht.df$becomes,
  pwg.name = c('dry', 'moist', 'cool', 'cold', 'deciduous')
) |>
  mutate(pwg = factor(pwg.name,labels=c('c(12,13,20)','30','c(40,50)','15','14'))) |>
  mutate(pwg = as.character(pwg),
         DHSVM.class = row_number())

rcl.df<-rbind(rcl.df,
              data.frame('fc'=0,'ht'=1.5,'pwg.name'='high shrub','pwg'=13,'DHSVM.class'=161),
              data.frame('fc'=0,'ht'=0.5,'pwg.name'='low shrub','pwg'=13,'DHSVM.class'=162),
              data.frame('fc'=0,'ht'=0.8,'pwg.name'='high herb','pwg'=12,'DHSVM.class'=163),
              data.frame('fc'=0,'ht'=0.3,'pwg.name'='low herb','pwg'=12,'DHSVM.class'=164),
              ## NOTE: 165-168 have the same veg parameters in DHSVM, but height varies
              data.frame('fc'=0,'ht'=0.5,'pwg.name'='agricultural dry','pwg'="10:50",'DHSVM.class'=165),
              data.frame('fc'=0,'ht'=0.7,'pwg.name'='agricultural moist','pwg'="10:50",'DHSVM.class'=166),
              data.frame('fc'=0,'ht'=1.5,'pwg.name'='agricultural vineyard','pwg'="10:50",'DHSVM.class'=167),
              data.frame('fc'=0,'ht'=4.5,'pwg.name'='agricultural orchard','pwg'="10:50",'DHSVM.class'=168),
              ## NOTE: 169-173 have the same veg parameters in DHSVM
              data.frame('fc'=0,'ht'=0,'pwg.name'='rock','pwg'="12:50",'DHSVM.class'=169),
              data.frame('fc'=0,'ht'=0,'pwg.name'='water','pwg'="10",'DHSVM.class'=170),
              data.frame('fc'=0,'ht'=0,'pwg.name'='snow and ice','pwg'="10:50",'DHSVM.class'=171),
              data.frame('fc'=0,'ht'=0,'pwg.name'='barren','pwg'="11", 'DHSVM.class'=172),
              data.frame('fc'=0,'ht'=0,'pwg.name'='urban','pwg'="10:50",'DHSVM.class'=173))

# Ok this is stupid but will save a ton of time
expander.df <- data.frame("pwg" = NULL, "pwg.code" = NULL)
for(pwg in unique(rcl.df$pwg)){
  to_append <- data.frame("pwg" = pwg, "pwg.code" = eval(parse(text=as.character(pwg))))
  
  expander.df <- bind_rows(expander.df, to_append)
}
expander.df <- expander.df |> filter(pwg.code %in% c(10, 11, 12, 13, 14, 15, 20, 30, 40, 50))
rcl.expanded.df <- rcl.df |> left_join(expander.df)  # use for joining to raster stack


#-----------------------------------------------------------------------------------------------------------------------
### Fractional Coverage model: ----
### Fractional Coverage model: ----
load(file.path(LANDIS.EXTENT, 'Ancillary_data', 'Fc_AgeBiomassSppPWG_glm.Rdata'))
predict(fc.glm,newdata=data.frame('Mean.Age'=100,'Biomass.sum.gm2'=seq(1000,2000,100),'PWG'=40,'Elevation'=1000),type='response')

### Height model: ----
load(file.path(LANDIS.EXTENT, 'Ancillary_data', 'Ht_AgeBiomassSppPWG_glm.Rdata'))
predict(ht.glm,newdata=data.frame('Age'=seq(1,100,20),'Biomass_gm2'=1000,'Species'='PinuPond','PWG'=40),type='response')


#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
#### Loop through years to generate HT, FC, Mean Age, Dom Spp, LAI, and DHSVM maps at succession timesteps: ----
## If maps aren't present, re-run. If maps are present, this step can be skipped by setting RERUN.DHSVM.HEIGHT_FC_and_LAI_MAPS to FALSE
if(FALSE %in% c(paste0('Veg_Height-m_yr-',yrs,'.tif') %in% dir(file.path(landisOutputDir,'DHSVM'))) | 
   FALSE %in% c(paste0('MeanAge_AllSpp_yr',yrs,'.tif') %in% dir(file.path(landisOutputDir,'ageOutput'))))
  simOpts$RERUN.DHSVM.HEIGHT_FC_and_LAI_MAPS<-T 

LAI.stack <- rast(file.path(necnOutput, "LAI-yr.tif"))
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

mean.age.domSpp.stack.r <- rast(file.path(ageOutput, paste0('MeanAge_DominantSpecies-yr.tif')))
mean.age.top3.dom.stack.r <- rast(file.path(ageOutput, paste0('MeanAge_TopThreeSpecies-yr.tif')))
dominant.spp.stack.r<- rast(file.path(ageOutput, paste0('DominantSpecies-yr.tif')))
dominant.spp2.stack.r<- rast(file.path(ageOutput, paste0('DominantSpeciesTwo-yr.tif')))
dominant.spp3.stack.r<- rast(file.path(ageOutput, paste0('DominantSpeciesThree-yr.tif')))

cat('\n\n----------------------------------------------------------------------------------\nLooping through years...\n----------------------------------------------------------------------------------\n')

for(yr in unique(yrs)){
  cat(paste0('\n------------\nYear: ',yr,'\n------------\n'))

  if (
      file.exists(file.path(landisOutputDir, 'DHSVM',paste0('Veg_Height-m_yr-',yr,'.tif'))) &
      file.exists(file.path(landisOutputDir, 'DHSVM',paste0('Veg_FracCov_yr-',yr,'.tif'))) &
      file.exists(file.path(landisOutputDir, 'DHSVM',paste0('Veg_LAI_yr-',yr,'.tif'))) 
      ) {
    
  } else {
    cat('-> Loading Total LAI and Biomass maps...\n')
    ### LAI: ----
    lai.r<-LAI.stack |> select(ends_with(paste0('-', yr)))
    
    ### Biomass: ----
    total.biomass.r<-totalBiomass_stack.r |> select(contains(paste0('-', yr, '-')))
    
    ## Set LAI to 0.1 for cells with non-zero biomass but LAI == 0:
    lai.r <- ifel(lai.r==0&!is.na(total.biomass.r)&total.biomass.r>0, 0.1, lai.r)
    
    ### Age: ----
    cat('-> Loading Species Age and Biomass maps')
    
    biomass.trees<-BiomassTrees.stack |> select(contains(paste0('-', yr, '-')))
    
    mean.age.top3.dom.r <- mean.age.top3.dom.stack.r[[yr]]
    top3sp.r <- c(
      dominant.spp.stack.r[[yr]],
      dominant.spp2.stack.r[[yr]],
      dominant.spp3.stack.r[[yr]]
    )
    
    #---------------------------------------------#      
    ### Generate Rasters for average FC and Height per cell: ----
    cat('\n-> Generating rasters for FC and Height...\n')
    start.time <- Sys.time()
    fc.r <- c(pwg.r, dem.r, total.biomass.r, mean.age.top3.dom.r) |>
      as.data.frame(xy=T) |>
      filter(!is.na(PWG)) |>
      rename(Biomass.sum.gm2 = names(total.biomass.r)[1], Mean.Age = names(mean.age.top3.dom.r)[1]) |>
      mutate(
        PWG = as.factor(ifelse(PWG <= 11, 12, PWG)),  # The bareground PWG level (11) is absent from fc.glm model... model FC as if these were in PWG = 12 (grassland)
        Elevation = round(Elevation, -1)
      ) |>
      mutate(Pred = predict(fc.glm, pick(Mean.Age, Biomass.sum.gm2, PWG, Elevation))) |>
      mutate(Pred = ifelse(Pred > 1, 1, ifelse(Pred < 0, 0, Pred))) |>  # ensure values stay between 0 and 1
      mutate(fc = ifelse(is.na(Pred) & Biomass.sum.gm2 > 0, 0.1, Pred)) |>  # if grasses where pred is NA but there is biomass, set the value to 0.1
      select(x, y, fc) |>
      rast(type = 'xyz', crs = crs(pwg.r), ext = ext(pwg.r))
    
    ht.r <- c(pwg.r, total.biomass.r, mean.age.top3.dom.r, top3sp.r) |>
      as.data.frame(xy=T) |>
      filter(!is.na(PWG)) |>
      mutate(
        PWG = as.factor(ifelse(!PWG%in%levels(ht.glm@frame$PWG), 50, PWG))  # The bareground PWG level (11) is absent from fc.glm model... model FC as if these were in PWG = 50 (cold-dry conifer)
      ) |>
      left_join(data.frame("BiomassRank1" = 1:length(names(biomass.trees)), "Species" = str_split_i(names(biomass.trees), "-", 1))) |>  # join the df with name of species with highest biomass
      rename(Age = names(mean.age.top3.dom.r)[1], Biomass_gm2 = names(total.biomass.r)[1]) |>
      mutate(Pred = predict(ht.glm, pick(Age, Biomass_gm2, PWG, Species), allow.new.levels = T)) |>
      mutate(Pred = ifelse(Pred < 0, 0, Pred)) |>
      mutate(ht = ifelse((is.na(Pred) | Pred <= 1) & Biomass_gm2 > 0, 1, Pred)) |> # if biomass > 0 but the prediction is 0 or NA, set height to 1.
      select(x, y, ht) |>
      rast(type = 'xyz', crs = crs(pwg.r), ext = ext(pwg.r))
    # plot(ht.r)
    print(Sys.time() - start.time)
    
    
    ### Trim rasters to study area: ----
    if(MASK == T){
      cat('\n-> Trimming rasters to study area')
      for(r.name in c('ht.r','fc.r','lai.r','total.biomass.r')){
        r<-eval(parse(text=r.name))
        r <- ifel(is.na(pwg.noBuffer.r), NA, r)
        assign(r.name,r)
      }
    }
    #---------------------------------------------#   
    ### View result: ----
    # if(as.numeric(yr) %in% c(0,20,40,60,80,100)){
    #   dhsvmCols<-colorRampPalette(c('saddlebrown','goldenrod1','darkslategray','darkgreen','seagreen4',
    #                                 'turquoise4','dodgerblue4','midnightblue','navy','orchid4','palegoldenrod','wheat'))(49)
    #   
    #   tiff(file.path(landisOutputDir,'DHSVM',paste0('Summary_fig_yr-',yr,'.tif')),width=6.5,height=9,res=300,units='in',compression='lzw')
    #   
    #   par(mfrow=c(2,2),oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(1,0.01,0),tck=-0.002,ps=10,cex=1)
    #   plot.new()
    #   plot.window(xlim=ext(pwg.r)[1:2], ylim=ext(pwg.r)[3:4],xaxs="i",yaxs="i",asp=1)
    #   plot(pwg.r,col='black',legend=F,add=T)
    #   plot(ht.r,col=rev(hcl.colors(length(ht.df$to),palette = 'ag_GrnYl')),breaks=c(0,ht.df$to),legend=F,xaxt='n',yaxt='n',add=T)
    #   plot(dem.r,col=demCols(50),add=T,alpha=0.15,legend=F)
    #   plot(hillshade.r,col=colorRampPalette(c('black','grey20','white'))(100),add=T,alpha=0.15,legend=F);box(lwd=3)
    #   mtext(paste0('Height (',2020+as.numeric(yr),')'),font=2,line=-1,adj=0.99,cex=1)
    #   
    #   plot.new()
    #   plot.window(xlim=ext(pwg.r)[1:2], ylim=ext(pwg.r)[3:4],xaxs="i",yaxs="i",asp=1)
    #   plot(pwg.r,col='black',legend=F,add=T)
    #   plot(fc.r,col=rev(hcl.colors(length(fc.df$to),palette = 'ag_GrnYl')),breaks=c(0,fc.df$to),legend=F,xaxt='n',yaxt='n',add=T)
    #   plot(dem.r,col=demCols(50),add=T,alpha=0.15,legend=F)
    #   plot(hillshade.r,col=colorRampPalette(c('black','grey20','white'))(100),add=T,alpha=0.15,legend=F);box(lwd=3)
    #   mtext(paste0('Fractional cover (',2020+as.numeric(yr),')'),font=2,line=-1,adj=0.99,cex=1)
    #   
    #   plot.new()
    #   plot.window(xlim=ext(pwg.r)[1:2], ylim=ext(pwg.r)[3:4],xaxs="i",yaxs="i",asp=1)
    #   plot(pwg.r,col='black',legend=F,add=T)
    #   plot(total.biomass.r,col=rev(hcl.colors(6,palette = 'Viridis')),breaks=c(1,5000,10000,20000,40000,100000),legend=F,xaxt='n',yaxt='n',add=T)
    #   plot(dem.r,col=demCols(50),add=T,alpha=0.15,legend=F)
    #   plot(hillshade.r,col=colorRampPalette(c('black','grey20','white'))(100),add=T,alpha=0.15,legend=F);box(lwd=3)
    #   mtext(paste0('Total biomass (',2020+as.numeric(yr),')'),font=2,line=-1,adj=0.99,cex=1)
    #   
    #   dev.off()
    #   
    #   gc()
    # }
    #---------------------------------------------#   
    ### WRITE RASTERS: ----
    for(r.name in c('ht.r','fc.r','lai.r','total.biomass.r')){
      r<-eval(parse(text=r.name))
      r <- ifel(is.na(r), 0, round(r, 2))
      assign(r.name,r)
    }
    
    writeRaster(ht.r,file.path(landisOutputDir, 'DHSVM',paste0('Veg_Height-m_yr-',yr,'.tif')),overwrite=T)
    writeRaster(fc.r,file.path(landisOutputDir, 'DHSVM',paste0('Veg_FracCov_yr-',yr,'.tif')),overwrite=T)
    writeRaster(lai.r,file.path(landisOutputDir, 'DHSVM',paste0('Veg_LAI_yr-',yr,'.tif')),overwrite=T)
    
    ## Save Age Rasters:
    for(r.name in c(#'mean.age.r','max.age.r',
      'mean.age.domSpp.r',
      'mean.age.top3.dom.r','dominant.spp','dominant.spp2')){
      r<-eval(parse(text=r.name))
      r<-round(r,0)
      assign(r.name,r)
    }
    
    if (!file.exists(file.path(ageOutput, 'MeanAge_DominantSpecies-yr.tif'))){
      writeRaster(mean.age.domSpp.r,file.path(ageOutput, paste0('MeanAge_DominantSpecies-',yr,'.tif')),overwrite=T)
      writeRaster(mean.age.top3.dom.r,file.path(ageOutput, paste0('MeanAge_TopThreeSpecies-',yr,'.tif')),overwrite=T)
      writeRaster(dominant.spp,file.path(ageOutput, paste0('DominantSpecies-',yr,'.tif')),overwrite=T)
      writeRaster(dominant.spp2,file.path(ageOutput, paste0('DominantSpeciesTwo-',yr,'.tif')),overwrite=T)
    }
    
    
    cat('----> Complete! <----\n')
  }
  
} # END YEAR LOOP
gc()

  
#### Start generating DHSVM raster: ----
cat('\n\n----------------------------------------------------------------------------------\n Looping through years for DHSVM...\n----------------------------------------------------------------------------------\n')

for(yr in unique(yrs)){
  cat(paste0('\n------------\nYear: ',yr,'\n------------\n'))

  ht.r <- rast(file.path(landisOutputDir, 'DHSVM',paste0('Veg_Height-m_yr-',yr,'.tif')))
  fc.r <- rast(file.path(landisOutputDir, 'DHSVM',paste0('Veg_FracCov_yr-',yr,'.tif')))
  lai.r<- rast(file.path(landisOutputDir, 'DHSVM',paste0('Veg_LAI_yr-',yr,'.tif')))
  
  ht.r <- ifel(pwg.r<12 | is.na(pwg.r), NA, ht.r)  # Mask out inactive areas
  fc.r <- ifel(pwg.r<12 | is.na(pwg.r), NA, fc.r)
  lai.r <- ifel(pwg.r<12 | is.na(pwg.r), NA, lai.r)
  
  fc.r.classed <- classify(fc.r, rcl = fc.df, include.lowest = TRUE)
  ht.r.classed <- classify(ht.r, rcl = ht.df, include.lowest = TRUE)
  
  names(fc.r.classed) <- 'fc'
  names(ht.r.classed) <- 'ht'
  
  dhsvm.r <- c(fc.r.classed, ht.r.classed, pwg.r) |>
    as.data.frame(xy = T) |>
    rename('pwg.code' = 'PWG') |>
    left_join(rcl.expanded.df |> filter(DHSVM.class <= 160)) |>  # tree classes are exclusive, so we can use a table join
    select(x, y, DHSVM.class) |>
    rast(type = 'xyz', crs = crs(pwg.r), ext = ext(pwg.r))
  
  unique(values(dhsvm.r))

  ## Assign non-forest classes using LANDFIRE layers: ----
  for(i in 161:164){
    ## Classify herb and shrub using approximate height
    if(grepl("shrub",rcl.df[i,'pwg.name'])|grepl("herb",rcl.df[i,'pwg.name'])){
      dhsvm.r[pwg.r %in% eval(parse(text=as.character(rcl.df[i,'pwg']))) & 
                ht.r.classed<=rcl.df[i,'ht']] <- i
    }
    if(grepl("herb",rcl.df[i,'pwg.name'])){
      dhsvm.r[is.na(dhsvm.r) & 
                lf.grass] <- i
    }
    if(grepl("shrub",rcl.df[i,'pwg.name'])){
      dhsvm.r[is.na(dhsvm.r) & 
                lf.shrub] <- i
    }
  }
  # ## Classify Agricultural and Urban land using LANDFIRE EVT, overwriting LANDIS pixels in specified land use & pwg:
  dhsvm.r <- ifel(!is.na(extra_dhsvm.r), extra_dhsvm.r, dhsvm.r)
  
  ### Classify barren
  dhsvm.r <- ifel(pwg.r == 11 & (is.na(ht.r.classed) | is.na(fc.r.classed) | !is.na(lf.barren)) & is.na(dhsvm.r), 172, dhsvm.r)  # barren
  dhsvm.r <- ifel(pwg.r %in% 12:50 & (is.na(ht.r.classed) | is.na(fc.r.classed) | !is.na(lf.barren)) & is.na(dhsvm.r), 169, dhsvm.r)  # rock

  dhsvm.r <- ifel(is.na(dhsvm.r)&!is.na(pwg.r), 164, dhsvm.r) # Low herb class for any remaining uncategorized pixels.

  ## Mask out inactive areas: ----
  dhsvm.r <- ifel(is.na(ht.r) & is.na(fc.r), NA, dhsvm.r)
  
  ### View result: ----
  # if(yr %in% c(0,20,40,60,80,100)){
  #   
  #   ## Plot:
  #   tiff(file.path(landisOutputDir,'DHSVM',paste0('Summary_fig_yr-',yr,'.tif')),width=9.75,height=9,res=300,units='in',compression='lzw')
  #   
  #   par(mfrow=c(2,3),oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(1,0.01,0),tck=-0.002,ps=10,cex=1)
  #   
  #   plot.new()
  #   plot.window(xlim=ext(pwg.r)[1:2], ylim=ext(pwg.r)[3:4],xaxs="i",yaxs="i",asp=1)
  #   plot(pwg.r,col='black',legend=F,add=T)
  #   plot(totalBiomass_stack.r |> select(contains(paste0('-', yr, '-'))),col=rev(hcl.colors(6,palette = 'Viridis')),breaks=c(1,5000,10000,20000,40000,100000),legend=F,xaxt='n',yaxt='n',add=T)
  #   plot(dem.r,col=demCols(50),add=T,alpha=0.15,legend=F)
  #   plot(hillshade.r,col=colorRampPalette(c('black','grey20','white'))(100),add=T,alpha=0.15,legend=F);box(lwd=3)
  #   mtext(paste0('Total biomass (',2020+as.numeric(yr),')'),font=2,line=-1,adj=0.99,cex=1)
  #   
  #   plot.new()
  #   plot.window(xlim=ext(pwg.r)[1:2], ylim=ext(pwg.r)[3:4],xaxs="i",yaxs="i",asp=1)
  #   plot(pwg.r,col='black',legend=F,add=T)
  #   plot(mean.age.top3.dom.r,col=rev(hcl.colors(7,palette = 'Viridis')),breaks=c(1,20,40,80,120,160,300),legend=F,xaxt='n',yaxt='n',add=T)
  #   plot(dem.r,col=demCols(50),add=T,alpha=0.15,legend=F)
  #   plot(hillshade.r,col=colorRampPalette(c('black','grey20','white'))(100),add=T,alpha=0.15,legend=F);box(lwd=3)
  #   mtext(paste0('Age (',2020+as.numeric(yr),')'),font=2,line=-1,adj=0.99,cex=1)
  #   
  #   plot.new()
  #   plot.window(xlim=ext(pwg.r)[1:2], ylim=ext(pwg.r)[3:4],xaxs="i",yaxs="i",asp=1)
  #   plot(pwg.r,col='black',legend=F,add=T)
  #   plot(lai.r,col=rev(hcl.colors(6,palette = 'Viridis')),breaks=c(1,2,4,6,8,10),legend=F,xaxt='n',yaxt='n',add=T)
  #   plot(dem.r,col=demCols(50),add=T,alpha=0.15,legend=F)
  #   plot(hillshade.r,col=colorRampPalette(c('black','grey20','white'))(100),add=T,alpha=0.15,legend=F);box(lwd=3)
  #   mtext(paste0('LAI (',2020+as.numeric(yr),')'),font=2,line=-1,adj=0.99,cex=1)
  #   
  #   plot.new()
  #   plot.window(xlim=ext(pwg.r)[1:2], ylim=ext(pwg.r)[3:4],xaxs="i",yaxs="i",asp=1)
  #   plot(pwg.r,col='black',legend=F,add=T)
  #   plot(ht.r,col=rev(hcl.colors(length(ht.df$to),palette = 'ag_GrnYl')),breaks=c(0,ht.df$to),legend=F,xaxt='n',yaxt='n',add=T)
  #   plot(dem.r,col=demCols(50),add=T,alpha=0.15,legend=F)
  #   plot(hillshade.r,col=colorRampPalette(c('black','grey20','white'))(100),add=T,alpha=0.15,legend=F);box(lwd=3)
  #   mtext(paste0('Height (',2020+as.numeric(yr),')'),font=2,line=-1,adj=0.99,cex=1)
  #   
  #   plot.new()
  #   plot.window(xlim=ext(pwg.r)[1:2], ylim=ext(pwg.r)[3:4],xaxs="i",yaxs="i",asp=1)
  #   plot(pwg.r,col='black',legend=F,add=T)
  #   plot(fc.r,col=rev(hcl.colors(length(fc.df$to),palette = 'ag_GrnYl')),breaks=c(0,fc.df$to),legend=F,xaxt='n',yaxt='n',add=T)
  #   plot(dem.r,col=demCols(50),add=T,alpha=0.15,legend=F)
  #   plot(hillshade.r,col=colorRampPalette(c('black','grey20','white'))(100),add=T,alpha=0.15,legend=F);box(lwd=3)
  #   mtext(paste0('Fractional cover (',2020+as.numeric(yr),')'),font=2,line=-1,adj=0.99,cex=1)
  #   
  #   plot.new()
  #   plot.window(xlim=ext(pwg.r)[1:2], ylim=ext(pwg.r)[3:4],xaxs="i",yaxs="i",asp=1)
  #   plot(pwg.r,col='black',legend=F,add=T)
  #   plot(dhsvm.r,col=dhsvmCols,breaks=seq(1,195,4),legend=F,xaxt='n',yaxt='n',add=T)
  #   plot(dem.r,col=demCols(50),add=T,alpha=0.15,legend=F)
  #   plot(hillshade.r,col=colorRampPalette(c('black','grey20','white'))(100),add=T,alpha=0.15,legend=F);box(lwd=3)
  #   mtext(paste0('DHSVM class (',2020+as.numeric(yr),')'),font=2,line=-1,adj=0.99,cex=1)
  #   
  #   dev.off()
  # }
  
  ### Output DHSVM map: ----
  dhsvm.r <- ifel(is.na(dhsvm.r), 0, dhsvm.r)
  writeRaster(dhsvm.r,file.path(landisOutputDir, 'DHSVM',paste0('DHSVM_yr-',yr,'.tif')),overwrite=T)
  
  
  rm(ht.r,fc.r,lai.r, dhsvm.r)
} # END YEAR LOOP

cat("DHSVM RASTER GENERATION FINISHED")
#-----------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------------
#### Read in all years, replace NA with -9999, and replace 0 with 0.1: ----
cat('\n\n-----------------------------------------------------------------------
          Replacing 0 with 0.1 and NA with -9999\n-----------------------------------------------------------------------\n')
#------------------------------------------------------#
for (yr in unique(yrs)){
  cat(yr,', ',sep='')
  
  ### Load interpolated ht and fc maps: ----
  ht.r <- rast(file.path(landisOutputDir, 'DHSVM',paste0('Veg_Height-m_yr-',yr,'.tif')))
  fc.r <- rast(file.path(landisOutputDir, 'DHSVM',paste0('Veg_FracCov_yr-',yr,'.tif')))
  lai.r<- rast(file.path(landisOutputDir, 'DHSVM',paste0('Veg_LAI_yr-',yr,'.tif')))
  
  ht.r <- ifel(ht.r == 0, 0.1, ifel(pwg.r<12 | is.na(pwg.r) | is.na(pwg.noBuffer.r), -9999, ht.r))
  fc.r <- ifel(fc.r == 0, 0.01, ifel(pwg.r<12 | is.na(pwg.r) | is.na(pwg.noBuffer.r), -9999, fc.r))
  lai.r <- ifel(lai.r == 0, 0.1, ifel(pwg.r<12 | is.na(pwg.r) | is.na(pwg.noBuffer.r), -9999, lai.r))
  
  names(ht.r) <- ""
  
  ### Output DHSVM map: ----
  writeRaster(ht.r,file.path(landisOutputDir, 'DHSVM',paste0('Veg_Height-m_yr-',yr,'.tif')),overwrite=T)
  writeRaster(fc.r,file.path(landisOutputDir, 'DHSVM',paste0('Veg_FracCov_yr-',yr,'.tif')),overwrite=T)
  writeRaster(lai.r,file.path(landisOutputDir, 'DHSVM',paste0('Veg_LAI_yr-',yr,'.tif')),overwrite=T)
  
} # Close year loop

gc()

#-----------------------------------------------------------------------------------------------------------------------



#-----------------------------------------------------------------------------------------------------------------------

cat('\n\n
===================================================================================================
***************************************************************************************************
                               Finished generating DHSVM output maps.             
---------------------------------------------------------------------------------------------------
***************************************************************************************************
===================================================================================================\n\n')

