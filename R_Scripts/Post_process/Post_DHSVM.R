#----------------------------------------------------------------------------------------------------------------------
#######################################################################################################################
####                                 NOW LOOP TO RUN ALL LANDIS OUTPUT FOLDERS:                                     ####
#######################################################################################################################
#----------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------

if(file.exists(file.path(landisOutputDir,'DHSVM','DHSVM.tif')) & SimOpts$RERUN.DHSVM.MAPS == F){
  cat('\nDHSVM outputs already exist for',gsub(dirToProcess,"",landisOutputDir),'. Skipping to next sim...')
  next
}

cat('\n\n  ***************************************************************************************************
Generating DHSVM output maps for',gsub(dirToProcess,"",landisOutputDir),'
***************************************************************************************************\n\n')
## Create output folder: ----
if(!dir.exists(file.path(landisOutputDir, 'DHSVM'))) dir.create(file.path(landisOutputDir, 'DHSVM'))

### Define LANDFIRE non-forest classes: ----
lf.ag.dry<-landfireReclassFUN(codes=c('Agricultural-Close Grown Crop','Agricultural-Fallow/Idle Cropland'))
lf.ag.moist<-landfireReclassFUN(codes=c('Agricultural-Wheat','Agricultural-Pasture and Hayland'))
lf.ag.vineyard<-landfireReclassFUN(codes='Agricultural-Vineyard')
lf.ag.orchard<-landfireReclassFUN(codes='Agricultural-Orchard')

lf.urban<-landfireReclassFUN(codes=c('Developed-High Intensity','Developed-Low Intensity','Developed-Medium Intensity','Developed-Roads',
                                     'Developed-Upland Evergreen Forest'))

lf.snowIce<-landfireReclassFUN(codes=c('Snow-Ice'))
lf.barren<-landfireReclassFUN(codes=c('Sparse Vegetation'))
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

########################################################################################################################
#-----------------------------------------------------------------------------------------------------------------------
#####  ---------------    DHSVM    --------------------------------------------------------------------------------- #####
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
#-----------------------------------------------------------------------------------------------------------------------
### Fractional Coverage model: ----
load(file.path(LANDIS.EXTENT, 'Ancillary_data', 'Fc_AgeBiomassSppPWG_glm.Rdata'))
predict(fc.glm,newdata=data.frame('Mean.Age'=100,'Biomass.sum.gm2'=seq(1000,2000,100),'PWG'=40,'Elevation'=1000),type='response')

### Height model: ----
load(file.path(LANDIS.EXTENT, 'Ancillary_data', 'Ht_AgeBiomassSppPWG_glm.Rdata'))
predict(ht.glm,newdata=data.frame('Age'=seq(1,100,20),'Biomass_gm2'=1000,'Species'='PinuPond','PWG'=40),type='response')

#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
#### Generate HT, FC, Mean Age, Dom Spp, LAI, and DHSVM maps at succession timesteps: ----
LAIStack.r <- c(
  rast(file.path(dataDir,'NECN_Outputs_Yr_0', LANDIS.EXTENT, 'LAI-1.tif')),  # load year zero LAI input
  rast(file.path(necnOutput, "LAI-yr.tif"))
)
names(LAIStack.r)[1] <- "LAI-0"
LAIStack.r <- LAIStack.r |>
  as.data.frame(xy = T) |>
  pivot_longer(contains("LAI"), names_to = c("drop", "Year"), names_sep = '-', values_to = "LAI") |>
  mutate(Year = as.numeric(Year)) |>
  complete(Year = 0:simLength, x, y) |>  # add rows for missing year x pixel combinations
  arrange(Year, x, y) |>  # get the columns in the right order
  group_by(x, y) |>  # group by pixel and interpolate between years
  mutate(z = zoo::na.approx(LAI, na.rm = F)) |>
  ungroup() |>
  mutate(l = paste0("LAI-", Year)) |>  # generate a layer name for the output raster stack
  select(x, y, l, z) |>
  rast(type = 'xylz', crs = crs(pwg.r), ext = ext(pwg.r))
plot(LAIStack.r)

### Age: ----
med.age.r <- rast(file.path(ageOutput, dir(ageOutput)[grepl("MED", dir(ageOutput))]))
max.age.r <- rast(file.path(ageOutput, dir(ageOutput)[grepl("MAX", dir(ageOutput))]))

biomass.trees.r <- biomassStack.r |> select(!starts_with(c("Nfixer_Resprt","NonFxr_Resprt","NonFxr_Seed","Grass_Forb","TotalBiomass")))
biomass.notTrees.r <- biomassStack.r |> select(starts_with(c("Nfixer_Resprt","NonFxr_Resprt","NonFxr_Seed","Grass_Forb")))
biomass.all.r <- biomassStack.r |> select(!starts_with("TotalBiomass"))

biomass.years <- names(biomass.all.r) |> str_extract("-\\d+-") |> unique()

gc()
cat('\n-> Identifying top 3 dominant species per site per year\n')

top3sp.df <- data.frame("x" = NULL, 'y' = NULL, "Year" = NULL, "Rank" = NULL, "Species" = NULL)
for (yr in biomass.years){
  cat(yr)
  biomass.trees.yr.r <- biomass.trees.r |> select(contains(yr)) #|> app(rank)
  
  # sp1.df <- which.lyr(biomass.trees.r == nlyr(biomass.trees.yr.r))
  # sp2.df <- which.lyr(biomass.trees.r == nlyr(biomass.trees.yr.r)-1)
  # sp3.df <- which.lyr(biomass.trees.r == nlyr(biomass.trees.yr.r)-2)
  
  top3sp.yr.df <- c(pwg.r, biomass.trees.yr.r) |>
    as.data.frame(xy=T) |>
    filter(!is.na(PWG)) |>  # save some memory by dropping cells outside study area
    pivot_longer(contains("biomass"), names_to = c("Species", "Year", "drop"), names_sep = '-', values_to = "Biomass_gm2", values_drop_na = T) |>
    select(!drop) |>
    filter(Biomass_gm2 > 0) |>  # don't bother ranking species-pixels with 0 biomass
    mutate(Year = as.numeric(Year)) |>
    group_by(x, y, Year) |>  # we want the top three species per pixel, so group by pixel and year
    mutate(Rank = rank(-Biomass_gm2),
           PWG = as.factor(PWG)) |> # grab the top 3 species in each pixel-year
    ungroup() |>
    filter(Rank <= 3) |>
    select(x, y, Year, Rank, Species)  # only keep pixel location, species, and rank to make joining easier later
  
  top3sp.df <- bind_rows(top3sp.yr.df)
}

# top3sp.df <- c(pwg.r, biomass.trees.r) |>
#   as.data.frame(xy=T) |>
#   filter(!is.na(PWG)) |>  # save some memory by dropping cells outside study area
#   pivot_longer(contains("biomass"), names_to = c("Species", "Year", "drop"), names_sep = '-', values_to = "Biomass_gm2", values_drop_na = T) |>
#   select(!drop) |>
#   filter(Biomass_gm2 > 0) |>  # don't bother ranking species-pixels with 0 biomass
#   mutate(Year = as.numeric(Year)) |>
#   group_by(x, y, Year) |>  # we want the top three species per pixel, so group by pixel and year
#   mutate(Rank = rank(-Biomass_gm2),
#          PWG = as.factor(PWG)) |> # grab the top 3 species in each pixel-year
#   ungroup() |>
#   filter(Rank <= 3) |>
#   select(x, y, Year, Rank, Species)  # only keep pixel location, species, and rank to make joining easier later

cat('\n-> Calculating mean median age for top 3 dominant species per site per year')
mean.age.top3.dom.df <- c(med.age.r) |>
  as.data.frame(xy=T) |>
  pivot_longer(contains("MED"), names_to = c("Species", "Year", "drop"), names_sep = '-', values_to = 'Med.Age') |>
  mutate(Year = as.numeric(Year)) |>
  inner_join(top3sp.df) |>
  group_by(x, y, Year) |>
  summarise(Mean.Age = mean(Med.Age)) |>
  ungroup()

gc()
### Generate Rasters for average FC and Height per cell: ----
cat('\n-> Generating rasters for FC and Height...\n')
fc.r <- c(pwg.r, dem.r, totalBiomass_stack.r) |>
  as.data.frame(xy=T) |>
  filter(!is.na(PWG)) |>
  pivot_longer(contains("TotalBiomass"), names_to = c("drop", "Year", "drop2"), names_sep = '-', values_to = 'Biomass.sum.gm2') |>
  select(!starts_with("drop")) |>
  mutate(
    Year = as.numeric(Year),
    PWG = as.factor(ifelse(PWG <= 11, 12, PWG)),  # The bareground PWG level (11) is absent from fc.glm model... model FC as if these were in PWG = 12 (grassland)
    Elevation = round(Elevation, -1)
  ) |>
  left_join(mean.age.top3.dom.df) |>  # join mean age for top 3 species by x, y, Year
  mutate(Pred = predict(fc.glm, pick(Mean.Age, Biomass.sum.gm2, PWG, Elevation))) |>
  mutate(Pred = ifelse(Pred > 1, 1, ifelse(Pred < 0, 0, Pred))) |>  # ensure values stay between 0 and 1
  mutate(Pred = ifelse(is.na(Pred) & Biomass.sum.gm2 > 0, 0.1, Pred)) |>  # if grasses where pred is NA but there is biomass, set the value to 0.1
  complete(Year = 0:simLength, x, y) |>  # add rows for missing year x pixel combinations
  arrange(Year, x, y) |>  # get the columns in the right order
  group_by(x, y) |>  # group by pixel and interpolate between years
  mutate(z = zoo::na.approx(Pred, na.rm = F)) |>
  ungroup() |>
  mutate(l = paste0("FC-", Year)) |>  # generate a layer name for the output raster stack
  select(x, y, l, z) |>
  rast(type = 'xylz', crs = crs(pwg.r), ext = ext(pwg.r))

plot(fc.r)

gc()
ht.r <- c(pwg.r, totalBiomass_stack.r) |>
  as.data.frame(xy=T) |>
  filter(!is.na(PWG)) |>
  pivot_longer(contains("TotalBiomass"), names_to = c("drop", "Year", "drop2"), names_sep = '-', values_to = 'Biomass_gm2') |>
  select(!starts_with("drop")) |>
  mutate(
    Year = as.numeric(Year),
    PWG = as.factor(ifelse(!PWG%in%levels(ht.glm@frame$PWG), 50, PWG))  # The bareground PWG level (11) is absent from fc.glm model... model FC as if these were in PWG = 50 (cold-dry conifer)
  ) |>
  left_join(top3sp.df) |>  # join the df with top species by biomass but only keep top-ranked
  filter(Rank == 1) |>
  left_join(mean.age.top3.dom.df) |>  # join mean age for top 3 species by x, y, Year
  rename(Age = Mean.Age) |>
  mutate(Pred = predict(ht.glm, pick(Age, Biomass_gm2, PWG, Species))) |>
  mutate(Pred = ifelse(Pred < 0, 0, Pred)) |>
  mutate(Pred = ifelse((is.na(Pred) | Pred <= 1) & Biomass_gm2 > 0, 1, Pred)) |> # if biomass > 0 but the prediction is 0 or NA, set height to 1.
  complete(Year = 0:simLength, x, y) |>  # add rows for missing year x pixel combinations
  arrange(Year, x, y) |>  # get the columns in the right order
  group_by(x, y) |>  # group by pixel and interpolate between years
  mutate(z = zoo::na.approx(Pred, na.rm = F)) |>
  ungroup() |>
  mutate(l = paste0("HT-", Year)) |>
  select(x, y, l, z) |>
  rast(type = 'xylz', crs = crs(pwg.r), ext = ext(pwg.r))
plot(ht.r)

writeRaster(fc.r, file.path(landisOutputDir, 'DHSVM', "Veg_FracCov.tif"), overwrite = T)
writeRaster(ht.r, file.path(landisOutputDir, 'DHSVM', "Veg_Height-m.tif"), overwrite = T)

# reclassify to bins for DHSVM code assignment
fc.reclassed.r <- fc.r |> classify(fc.df)  
ht.reclassed.r <- ht.r |> classify(ht.df)





if(RERUN.DHSVM.HEIGHT_FC_and_LAI_MAPS==T){
  cat('\n\n----------------------------------------------------------------------------------
Looping through years...\n----------------------------------------------------------------------------------\n')
  for(yr in unique(yrs)){
    cat(paste0('\n------------\nYear: ',yr,'\n------------\n'))
    
    ### FIX LAI. NECN v6.8 has a bug in LAI code: ----
    ##   This bug makes LAI calculation cumulative for all trees in a cell rather than making LAI diminish as the cell fills up.
    ##   The result is that some cells have unreasonably high LAI.
    ##   To fix, scale LAI values to some theoretical maximum. Let's use 10.
    ## Set reclass parameters:
    # maxLAI<-10 # This prduces max LAI values ~8 (asymptote reached only if input values are very large)
    # maxLAI.t0<-max(values(lai.0), na.rm=T)
    # if(maxLAI.t0>maxLAI){
    #   inflection<-5
    #   x=0:maxLAI.t0
    #   
    #   ## Reclass function:
    #   # lai.df<-data.frame('is'=x,'becomes'=seq(0,maxLAI,length.out=length(x))) # Linear function
    #   lai.df<-data.frame('is'=x,'becomes'= round(maxLAI * x/(inflection + x),1)) # Saturation function
    #   
    #   plot(lai.df$is,lai.df$becomes,pch=21,bg=rev(hcl.colors(nrow(lai.df),palette = 'ag_GrnYl')),ylim=c(0,maxLAI))
    #   lines(lai.df$is,lai.df$becomes,col=grey(0.2,0.1));lines(0:maxLAI.t0,0:maxLAI.t0,col=grey(0.5,0.5),lty='dotted');abline(h=maxLAI,col='darkred')
    #   
    #   ## Reclassify:
    #   
    #   lai.r[lai.r>maxLAI.t0]<-maxLAI.t0
    #   if(length(lai.r[!lai.r%in%lai.df$is & !is.na(lai.r)])>0){
    #     lai.r<-round(lai.r,0)
    #     warning('Raw LAI raster has non-integer values. Rounding to integers for reclassification.')
    #   }
    #   lai.r<-classify(lai.r,rcl=lai.df)
    #   
    # }

    
    cat('\n-> Identifying top 3 dominant species per site')
    
    ## Dominant species by biomass: ----
    dominant.spp<-which.max(biomass.trees) # How to identify the dominant species per cell???
    dominant.spp.lookup<-1:length(names(biomass.trees))
    names(dominant.spp.lookup)<-names(biomass.trees)
    
    ## Second and third dominant by biomass: ----
    ##  Biomass values, ordered (layer 1 is highest biomass, layer 2 is second highest, etc.)
    dom.spp.ordered<-app(biomass.trees, fun=function(x,na.rm) x[order(x,decreasing=T)])  # function used to be calc
    
    dominant.spp2<-dominant.spp
    dominant.spp3<-dominant.spp
    values(dominant.spp2)<-0
    values(dominant.spp3)<-0
    
    for(sp in names(dominant.spp.lookup)){
      cat('.')
      ## Create a mask for where each species is within the top 3 per site:
      # Second highest biomass:
      temp<-biomass.trees[[sp]]==dom.spp.ordered[[2]] # Create mask
      temp[temp!=1|is.na(temp)]<-0
      temp[temp==1]<-dominant.spp.lookup[sp] # Assign species index number to mask
      # For ties, pick either the new species being added or the species already chosen (random choice):
      if(sample(c(T,F),1))
        temp[dominant.spp2!=0]<-0 else
          dominant.spp2[temp!=0]<-0
      
      dominant.spp2<-dominant.spp2+temp
      if(max(values(dominant.spp2))>nlyr(biomass.trees)) stop('Species index exceeds the number of species in the biomass raster stack. This may be due to ties where two species are the second most abundant.')
      # cat(paste0(max(values(dominant.spp2)),', '))
      
      # Third highest biomass:
      temp<-biomass.trees[[sp]]==dom.spp.ordered[[3]]
      temp[temp!=1|is.na(temp)]<-0
      temp[temp==1]<-dominant.spp.lookup[sp]
      # For ties, pick one at random:
      if(sample(c(T,F),1))
        temp[dominant.spp3!=0]<-0 else
          dominant.spp3[temp!=0]<-0
      
      dominant.spp3<-dominant.spp3+temp
      if(max(values(dominant.spp3))>nlyr(biomass.trees)) stop('Species index exceeds the number of species in the biomass raster stack')
      # cat(paste0(max(values(dominant.spp3)),', '))
    }
    
    ## Max age of any species: ----
    max.age.r<-max(max.age,na.rm=T)
    plot(max.age.r)
    
    ## Max biomass of any species:
    max.biomass.r<-max(biomass.trees,na.rm=T)
    plot(total.biomass.r)
    plot(max.biomass.r)
    
    ## Mean age of dominant species per site: ----
    mean.age.domSpp.r<-max.age.r
    values(mean.age.domSpp.r)<-0
    for(sp in names(dominant.spp.lookup)){
      sp.dom.r<-dominant.spp
      sp.num<-dominant.spp.lookup[sp]
      sp.mask<-sp.dom.r==sp.num
      sp.mask[sp.mask==0]<-NA
      # plot(sp.mask)
      
      
      sp.med.age<-mask(med.age[[sp]],sp.mask)
      sp.med.age[is.na(sp.med.age)]<-0
      # plot(sp.med.age)
      
      mean.age.domSpp.r<-mean.age.domSpp.r+sp.med.age
    }
    plot(mean.age.domSpp.r)
    
    ## Mean age of dominant 3 species per site: ----
    cat('\n-> Calculating mean age of dominant 3 species per site')
    age.top3.dom.Spp<-list()
    for(sp in names(dominant.spp.lookup)){
      cat('.')
      # cat(paste0(sp,', '))
      sp.num<-dominant.spp.lookup[sp]
      sp.mask<-dominant.spp==sp.num
      sp.mask[sp.mask==0]<-NA
      # plot(sp.mask)
      dom.sp.med.age<-mask(med.age[[sp]],sp.mask)
      plot(dom.sp.med.age,col='red',main=sp,legend=F)
      dom.sp.med.age[is.na(dom.sp.med.age)]<-0
      # plot(sp.med.age)
      
      sp.mask<-dominant.spp2==sp.num
      sp.mask[sp.mask==0]<-NA
      # plot(sp.mask)
      dom2.sp.med.age<-mask(med.age[[sp]],sp.mask)
      plot(dom2.sp.med.age,add=T,col='gold',legend=F)
      dom2.sp.med.age[is.na(dom2.sp.med.age)]<-0
      
      sp.mask<-dominant.spp3==sp.num
      sp.mask[sp.mask==0]<-NA
      # plot(sp.mask)
      dom3.sp.med.age<-mask(med.age[[sp]],sp.mask)
      plot(dom3.sp.med.age,add=T,col='wheat',legend=F)
      dom3.sp.med.age[is.na(dom3.sp.med.age)]<-0
      
      sp.med.age<-dom.sp.med.age+dom2.sp.med.age+dom3.sp.med.age
      
      sp.med.age[sp.med.age==0]<-NA
      age.top3.dom.Spp[[sp]]<-sp.med.age
    }
    age.top3.dom.Spp<-rast(age.top3.dom.Spp)
    # It works! See:
    med.age[604,400]
    age.top3.dom.Spp[604,400]
    mean.age.top3.dom.r<-mean(age.top3.dom.Spp,na.rm=T)
    plot(mean.age.top3.dom.r)
    
    # Compare to the overall mean median age (this method is higher if there are younger cohorts present that weight down the med age, lower if the dominant cohort is very old):
    mean.age.r<-mean(med.age,na.rm=T)
    plot(mean.age.r)
    plot(mean.age.domSpp.r-mean.age.r,col=colorRampPalette(c('red','gold','grey80','blue','green','darkgreen'))(50))
    #---------------------------------------------#      
    # ### Generate Rasters for average FC and Height per cell: ----
    # cat('\n-> Generating rasters for FC and Height...\n')
    # 
    # ## Create data frames based on raster values
    # fc.pred.df<-data.frame('Mean.Age'=unname(values(mean.age.top3.dom.r)),'Biomass.sum.gm2'=unname(values(total.biomass.r)),
    #                        'PWG'=unname(values(pwg.r)),'Elevation'=round(unname(values(dem.r)),-1),'Pred'=NA)
    # ## The bareground PWG level (11) is absent from fc.glm model... model FC as if these were in PWG = 12 (grassland)
    # fc.pred.df[!fc.pred.df$PWG%in%levels(fc.glm@frame$PWG)&!is.na(fc.pred.df$PWG),'PWG']<-12
    # 
    ## Create data frames based on raster values
    ht.pred.df<-data.frame('Age'=unname(values(mean.age.top3.dom.r)),'Biomass_gm2'=unname(values(total.biomass.r)),
                           'Species'=names(dominant.spp.lookup[values(dominant.spp)]),'PWG'=unname(values(pwg.r)),'Pred'=NA)
    ## For cells with non-zero biomass with species = NA, fill in with grass:
    ht.pred.df[is.na(ht.pred.df$Species) & !is.na(ht.pred.df$Biomass_gm2),'Age']<-1
    ## The bareground PWG level (11) is absent from fc.glm model... model FC as if these were in PWG = 50 (cold-dry conifer)
    ht.pred.df[!ht.pred.df$PWG%in%levels(fc.glm@frame$PWG)&!is.na(ht.pred.df$PWG),'PWG']<-50
    
    #---------------------------------------------#      
    # ## Generate fractional cover predicted values
    # fc.pred.df[!is.na(fc.pred.df$Mean.Age),'Pred'] <- 
    #   predict(fc.glm,newdata=fc.pred.df[!is.na(fc.pred.df$Mean.Age),],type='response')
    # 
    ## Genearte height predicted values
    ht.pred.df[!is.na(ht.pred.df$Age) & ht.pred.df$Species%in%levels(ht.glm@flist$Species),'Pred'] <- 
      predict(ht.glm,newdata=ht.pred.df[!is.na(ht.pred.df$Age) & ht.pred.df$Species%in%levels(ht.glm@flist$Species),],type='response')
    # Generic species form for species absent from the model factor levels:
    ht.pred.df[!is.na(ht.pred.df$Age) & !ht.pred.df$Species%in%levels(ht.glm@flist$Species) & !is.na(ht.pred.df$Species),'Pred'] <- 
      predict(ht.glm,newdata=ht.pred.df[!is.na(ht.pred.df$Age) & !ht.pred.df$Species%in%levels(ht.glm@flist$Species) & !is.na(ht.pred.df$Species),],type='response',re.form=~(1 + Age | PWG))
    
    # For cells with non-zero biomass with species = NA, give FC layer low grass cover:
    fc.pred.df[is.na(fc.pred.df$Pred) & !is.na(ht.pred.df$Pred),'Pred']<-0.1
    #---------------------------------------------#      
    ## Convert FC to 0 to 1 scale:
    if(mean(fc.pred.df$Pred,na.rm=T)>1)
      fc.pred.df$Pred<-fc.pred.df$Pred / 100
    ## Limit FC to 100:
    fc.pred.df[fc.pred.df$Pred>1&!is.na(fc.pred.df$Pred),'Pred']<-1
    # Fix negative values
    ht.pred.df[ht.pred.df$Pred<0&!is.na(ht.pred.df$Pred),'Pred']<-0
    fc.pred.df[fc.pred.df$Pred<0&!is.na(fc.pred.df$Pred),'Pred']<-0
    
    # Create blank rasters
    fc.r<-pwg.r
    values(fc.r)<-NA
    ht.r<-fc.r
    
    # Assign predicted values
    values(fc.r)<-fc.pred.df$Pred
    values(ht.r)<-ht.pred.df$Pred
    
    plot(fc.r)
    plot(ht.r)
    
    # Reclass raster to DHSVM Canopy cover class
    fc.r.classed <- classify(fc.r, rcl = fc.df, include.lowest = TRUE)
    ht.r.classed <- classify(ht.r, rcl = ht.df, include.lowest = TRUE)
    
    #---------------------------------------------#      
    ## Some cells have LAI = NA but non-zero biomass:
    biomass.trees[(is.na(lai.r)|lai.r==0)&!is.na(total.biomass.r)]
    plot(total.biomass.r,col='black',legend=F)
    plot(lai.r,add=T)
    
    temp<-total.biomass.r
    temp[(is.na(lai.r)|lai.r==0)&!is.na(temp)]<-9999
    temp[temp!=9999]<-NA
    plot(temp,col='red',add=T)
    
    ## Set LAI to 0.1 for cells with non-zero biomass but LAI == 0:
    lai.r[lai.r==0&!is.na(total.biomass.r)&total.biomass.r>0]<-0.1
    
    #---------------------------------------------#      
    #### CREATE DHSVM RASTER BASED ON HEIGHT, FC, and PWG: #### ----
    ##   *** NOTE: if INTERPOLATE.DHSVM == T, we'll have to re-do this based on interpolated height and FC maps. Skip this here to save time.
    if(INTERPOLATE.DHSVM==F){
      dhsvm.r<-pwg.r
      values(dhsvm.r)<-NA
      
      # First, assign forested classes
      cat('-> Creating DHSVM raster based on LANDIS-II veg outputs...\n     - Assigning class: ')
      for(i in 1:nrow(rcl.df)){
        cat(paste0(i,', '))
        dhsvm.r[fc.r.classed==rcl.df[i,'fc'] & 
                  ht.r.classed==rcl.df[i,'ht'] & 
                  pwg.r %in% eval(parse(text=as.character(rcl.df[i,'pwg'])))] <- 
          rcl.df[i,'DHSVM.class']
      }
      unique(values(dhsvm.r))
      plot(dhsvm.r)
      
      # Next, assign non-forest classes
      # Use LANDFIRE EVT to overwrite LANDIS-II produced pixels that should be static (e.g., Agriculture, Urban, rock)
      for(i in 161:173){
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
        ## Classify Agricultural and Urban land using LANDFIRE EVT:
        #   This will only overwrite LANDIS-II pixels within the PWGs specified in rcl.df.
        if(grepl("agricultural dry",rcl.df[i,'pwg.name'])){
          dhsvm.r[pwg.r %in% eval(parse(text=as.character(rcl.df[i,'pwg']))) & 
                    lf.ag.dry ] <- i
        }
        if(grepl("agricultural moist",rcl.df[i,'pwg.name'])){
          dhsvm.r[pwg.r %in% eval(parse(text=as.character(rcl.df[i,'pwg']))) & 
                    lf.ag.moist ] <- i
        }
        if(grepl("agricultural vineyard",rcl.df[i,'pwg.name'])){
          dhsvm.r[pwg.r %in% eval(parse(text=as.character(rcl.df[i,'pwg']))) & 
                    lf.ag.vineyard] <- i
        }
        if(grepl("agricultural orchard",rcl.df[i,'pwg.name'])){
          dhsvm.r[pwg.r %in% eval(parse(text=as.character(rcl.df[i,'pwg']))) & 
                    lf.ag.orchard ] <- i
        }
        if(grepl("urban",rcl.df[i,'pwg.name'])){
          dhsvm.r[pwg.r %in% eval(parse(text=as.character(rcl.df[i,'pwg']))) & 
                    !is.na(lf.urban)] <- i
        }
        if(grepl("snow",rcl.df[i,'pwg.name'])){
          dhsvm.r[pwg.r %in% eval(parse(text=as.character(rcl.df[i,'pwg']))) & 
                    !is.na(lf.snowIce)] <- i
        }
        if(grepl("water",rcl.df[i,'pwg.name'])){
          dhsvm.r[pwg.r %in% eval(parse(text=as.character(rcl.df[i,'pwg']))) & 
                    !is.na(lf.water)] <- i
        }
        if(grepl("barren",rcl.df[i,'pwg.name'])){
          dhsvm.r[pwg.r %in% eval(parse(text=as.character(rcl.df[i,'pwg']))) & 
                    is.na(dhsvm.r) &
                    !is.na(lf.barren)] <- i
        }
        
        ## Use PWG layer to classify a few remaining empty pixels:
        if(grepl("barren",rcl.df[i,'pwg.name'])|grepl("rock",rcl.df[i,'pwg.name'])){
          dhsvm.r[pwg.r %in% eval(parse(text=as.character(rcl.df[i,'pwg']))) &
                    (is.na(ht.r.classed) | is.na(fc.r.classed)) & 
                    is.na(dhsvm.r)] <- i
        }
      }
      par(mfrow=c(1,2))
      plot(dhsvm.r,col=hcl.colors(20))
      plot(total.biomass.r,col=hcl.colors(10,palette = 'ag_GrnYl'),legend=F,xaxt='n',yaxt='n')
      
      total.biomass.r[is.na(dhsvm.r)&!is.na(total.biomass.r)]
      ht.r.classed[is.na(dhsvm.r)&!is.na(total.biomass.r)]
      fc.r.classed[is.na(dhsvm.r)&!is.na(total.biomass.r)]
      pwg.r[is.na(dhsvm.r)&!is.na(total.biomass.r)]
      landfire.r[is.na(dhsvm.r)&!is.na(total.biomass.r)]
      
      # View landfire codes for areas that don't have a dhsvm veg class yet:
      r<-landfire.r
      r[!is.na(dhsvm.r)]<-NA
      
      t<-setNames(aggregate(values(r),by=list(values(r)),FUN=NROW),c('VALUE','Count'))
      t<-merge(t,landfire.codes[landfire.codes$VALUE%in%t$VALUE,c('VALUE','EVT_GP_N')],by='VALUE')
      t<-t[order(t$Count),]
      t
      
      # plot(pwg.r,col='black')
      # plot(r,col='red',add=T)
      
      ## Looks like very few pixels are missing DHSVM class! Great!
      dhsvm.r[is.na(dhsvm.r)&!is.na(pwg.r)]<-164 # Low herb class for any remaining uncategorized pixels.
      # assign(paste0('DHSVM.',yr),dhsvm.r)
    } else {
      dhsvm.r<-pwg.r
      values(dhsvm.r)<-0
    }
    #---------------------------------------------#   
    ### Trim rasters to study area: ----
    if(MASK == T){
      for(r.name in c('dhsvm.r','ht.r','fc.r','lai.r','total.biomass.r')){
        r<-eval(parse(text=r.name))
        r[is.na(pwg.noBuffer.r)]<-NA
        
        assign(r.name,r)
      }
      plot(total.biomass.r,col=hcl.colors(173))
    }
    #---------------------------------------------#   
    ### View result: ----
    if(yr %in% c(0,20,40,60,80,100)){
      dhsvmCols<-colorRampPalette(c('saddlebrown','goldenrod1','darkslategray','darkgreen','seagreen4',
                                    'turquoise4','dodgerblue4','midnightblue','navy','orchid4','palegoldenrod','wheat'))(49)
      
      tiff(file.path(landisOutputDir,'DHSVM',paste0('Summary_fig_yr-',yr,'.tif')),width=6.5,height=9,res=300,units='in',compression='lzw')
      
      par(mfrow=c(2,2),oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(1,0.01,0),tck=-0.002,ps=10,cex=1)
      plot.new()
      plot.window(xlim=ext(pwg.r)[1:2], ylim=ext(pwg.r)[3:4],xaxs="i",yaxs="i",asp=1)
      plot(pwg.r,col='black',legend=F,add=T)
      plot(ht.r,col=rev(hcl.colors(length(ht.df$to),palette = 'ag_GrnYl')),breaks=c(0,ht.df$to),legend=F,xaxt='n',yaxt='n',add=T)
      plot(dem.r,col=demCols,add=T,alpha=0.15,legend=F)
      plot(hillshade.r,col=colorRampPalette(c('black','grey20','white'))(100),add=T,alpha=0.15,legend=F);box(lwd=3)
      mtext(paste0('Height (',2020+as.numeric(yr),')'),font=2,line=-1,adj=0.99,cex=1)
      
      plot.new()
      plot.window(xlim=ext(pwg.r)[1:2], ylim=ext(pwg.r)[3:4],xaxs="i",yaxs="i",asp=1)
      plot(pwg.r,col='black',legend=F,add=T)
      plot(fc.r,col=rev(hcl.colors(length(fc.df$to),palette = 'ag_GrnYl')),breaks=c(0,fc.df$to),legend=F,xaxt='n',yaxt='n',add=T)
      plot(dem.r,col=demCols,add=T,alpha=0.15,legend=F)
      plot(hillshade.r,col=colorRampPalette(c('black','grey20','white'))(100),add=T,alpha=0.15,legend=F);box(lwd=3)
      mtext(paste0('Fractional cover (',2020+as.numeric(yr),')'),font=2,line=-1,adj=0.99,cex=1)
      
      plot.new()
      plot.window(xlim=ext(pwg.r)[1:2], ylim=ext(pwg.r)[3:4],xaxs="i",yaxs="i",asp=1)
      plot(pwg.r,col='black',legend=F,add=T)
      plot(total.biomass.r,col=rev(hcl.colors(6,palette = 'Viridis')),breaks=c(1,5000,10000,20000,40000,100000),legend=F,xaxt='n',yaxt='n',add=T)
      plot(dem.r,col=demCols,add=T,alpha=0.15,legend=F)
      plot(hillshade.r,col=colorRampPalette(c('black','grey20','white'))(100),add=T,alpha=0.15,legend=F);box(lwd=3)
      mtext(paste0('Total biomass (',2020+as.numeric(yr),')'),font=2,line=-1,adj=0.99,cex=1)
      
      plot.new()
      plot.window(xlim=ext(pwg.r)[1:2], ylim=ext(pwg.r)[3:4],xaxs="i",yaxs="i",asp=1)
      plot(pwg.r,col='black',legend=F,add=T)
      # plot(dhsvm.r,col=rev(hcl.colors(173,palette = 'Zissou 1')),legend=F,xaxt='n',yaxt='n',add=T)
      plot(dhsvm.r,col=dhsvmCols,breaks=seq(1,195,4),legend=F,xaxt='n',yaxt='n',add=T)
      plot(dem.r,col=demCols,add=T,alpha=0.15,legend=F)
      plot(hillshade.r,col=colorRampPalette(c('black','grey20','white'))(100),add=T,alpha=0.15,legend=F);box(lwd=3)
      mtext(paste0('DHSVM class (',2020+as.numeric(yr),')'),font=2,line=-1,adj=0.99,cex=1)
      
      dev.off()
    }
    #---------------------------------------------#   
    ### WRITE RASTERS: ----
    for(r.name in c('dhsvm.r','ht.r','fc.r','lai.r','total.biomass.r')){
      r<-eval(parse(text=r.name))
      r<-round(r,2)
      r[is.na(r)]<-0
      assign(r.name,r)
    }
    
    writeRaster(dhsvm.r,file.path(landisOutputDir, 'DHSVM',paste0('DHSVM_yr-',yr,'.tif')),overwrite=T)
    writeRaster(ht.r,file.path(landisOutputDir, 'DHSVM',paste0('Veg_Height-m_yr-',yr,'.tif')),overwrite=T)
    writeRaster(fc.r,file.path(landisOutputDir, 'DHSVM',paste0('Veg_FracCov_yr-',yr,'.tif')),overwrite=T)
    writeRaster(lai.r,file.path(landisOutputDir, 'DHSVM',paste0('Veg_LAI_yr-',yr,'.tif')),overwrite=T)
    
    ## Save Age Rasters:
    for(r.name in c('mean.age.r','max.age.r','mean.age.domSpp.r','mean.age.top3.dom.r','dominant.spp','dominant.spp2')){
      r<-eval(parse(text=r.name))
      r<-round(r,0)
      assign(r.name,r)
    }
    writeRaster(mean.age.r,file.path(ageDir, paste0('MeanAge_AllSpp_yr-',landis.yr,'.tif')),overwrite=T)
    writeRaster(max.age.r,file.path(ageDir, paste0('MaxAge_AllSpp_yr-',landis.yr,'.tif')),overwrite=T)
    writeRaster(mean.age.domSpp.r,file.path(ageDir, paste0('MeanAge_DominantSpecies_yr-',landis.yr,'.tif')),overwrite=T)
    writeRaster(mean.age.top3.dom.r,file.path(ageDir, paste0('MeanAge_Top3Species_yr-',landis.yr,'.tif')),overwrite=T)
    writeRaster(dominant.spp,file.path(ageDir, paste0('DominantSpecies-',landis.yr,'.tif')),overwrite=T)
    writeRaster(dominant.spp2,file.path(ageDir, paste0('DominantSpecies2-',landis.yr,'.tif')),overwrite=T)
    
    cat('----> Complete! <----\n')
    
    rm(dhsvm.r,ht.r,fc.r,ht.r.classed,fc.r.classed,lai.r)
  } # END YEAR LOOP
} # Finished creating Ht, FC, Mean Age, and Dom Spp maps at the succession timesteps.
#-----------------------------------------------------------------------------------------------------------------------






#-----------------------------------------------------------------------------------------------------------------------
#### Interpolate HT and FC maps, create DHSVM maps based on interpolated maps: ----
if(INTERPOLATE.DHSVM==T){
  cat('\n\n-----------------------------------------------------------------------
          Interpolating from',as.numeric(yrs[2]) - as.numeric(yrs[1]),'to',increment,'years.\n-----------------------------------------------------------------------\n')
  
  #------------------------------------------------------#
  ### Create empty raster: ----
  NA.r<-pwg.r
  NA.r[is.na(NA.r)]<-0
  NA.r[NA.r!=0]<-NA
  plot(NA.r,xaxt='n',yaxt='n')
  
  
  #------------------------------------------------------#
  ### Loop through years and interpolate FC and HT maps to the desired increment: ----
  for(yr in unique(yrs)){
    cat(paste0('\n-------------------\nYear: ',yr,'\n-------------------\n'))
    
    #------------------------------------------------------#
    cat('-> Loading Veg_FracCov, Veg_Height, Veg_LAI, and DHSVM maps...\n')
    
    VegHT.r <- rast(file.path(landisOutputDir, 'DHSVM',paste0('Veg_Height-m_yr-',yr,'.tif'))); names(VegHT.r) <- paste0('Veg_Height-m_yr-',yr)
    VegFC.r <- rast(file.path(landisOutputDir, 'DHSVM',paste0('Veg_FracCov_yr-',yr,'.tif'))); names(VegFC.r) <- paste0('Veg_FracCov_yr-',yr)
    VegLAI.r<- rast(file.path(landisOutputDir, 'DHSVM',paste0('Veg_LAI_yr-',yr,'.tif'))); names(VegLAI.r) <- paste0('Veg_LAI_yr-',yr)
    # DHSVM.r <- raster(file.path(landisOutputDir, 'DHSVM',paste0('DHSVM_yr-',yr,'.tif')))
    
    #------------------------------------------------------#
    ## If it's the first year, skip to next: ----
    if(yr == yrs[1]) {
      VegHT.prev<-VegHT.r
      VegFC.prev<-VegFC.r
      VegLAI.prev<-VegLAI.r
      # DHSVM.prev<-DHSVM.r
      prev.yr=yr
      next
    } 
    #------------------------------------------------------#
    ## Determine number of years to interpolate: ----
    succession.timestep <- as.numeric(yr) - as.numeric(prev.yr)
    yrs.needed <- seq(as.numeric(prev.yr)+increment,as.numeric(yr)-increment,increment)
    yrs.needed
    cat('-> Interpolating from year',prev.yr,'to',yr,'...\n')
    
    ## Create empty raster stack ----
    for(i in yrs.needed){
      if(i==yrs.needed[1]) empty.s<-NA.r else
        empty.s<-c(empty.s,NA.r)
    }
    names(empty.s)<-yrs.needed
    
    
    #------------------------------------------------------#
    ## Interpolate: ----
    cat('-> Layers: ')
    start.time<-Sys.time()
    
    for(layer in c('VegHT','VegFC','VegLAI')){
      cat(layer,', ',sep='')
      layer.r<-eval(parse(text=paste0(layer,'.r')))
      layer.prev<-eval(parse(text=paste0(layer,'.prev')))
      
      ## Old method. Way too slow.
      # start.time<-Sys.time()
      # s<-stack(layer.prev,empty.s,layer.r)
      # layer.interp<-approxNA(s,method="linear",yleft=0,yright=0)
      # names(layer.interp)<-gsub('.biomass','',names(layer.interp))
      # names(layer.interp)<-paste0(gsub('X',paste0(sp,'-'),names(layer.interp)),'-biomass')
      # cat('Time diff:',round(difftime(Sys.time(),start.time,units='secs'),0),'seconds')
      
      ## New method. Much faster.
      ## Create vectors for time 1 and time 2 values:
      t1<-layer.prev[is.na(NA.r)]
      t2<-layer.r[is.na(NA.r)]
      ## Calculate distance:
      delta <- t2-t1
      ## Interpolate: 
      s<-empty.s
      for(i in 1:length(yrs.needed)){
        s[[i]][is.na(s[[i]])] <- t1 + i * (delta / succession.timestep)
      }
      ## Join with t1 and t2 maps:
      layer.interp<-c(layer.prev,s,layer.r)
      ## Round layers:
      layer.interp<-round(layer.interp,2)
      ## Rename:
      names(layer.interp)<-paste0(gsub("[0-9]","",names(layer.prev)),
                                  seq(as.numeric(gsub(".*?([0-9]+).*", "\\1", names(layer.prev))),
                                      as.numeric(gsub(".*?([0-9]+).*", "\\1", names(layer.r))),
                                      increment))
      ## Write rasters:
      for(i in 1:nlyr(layer.interp)){
        if(names(layer.interp[[i]]) %in% c(names(layer.prev),names(layer.r))) next
        writeRaster(layer.interp[[i]],file.path(landisOutputDir, 'DHSVM',paste0(paste0(gsub("[.]","-",names(layer.interp[[i]]))),'.tif')),overwrite=T)
      }
    }
    
    # cat('\n -Time diff:',round(difftime(Sys.time(),start.time,units='secs'),0),'seconds')
    #------------------------------------------------------#
    ## Save current year maps as previous year maps for next loop level: ----
    VegHT.prev<-VegHT.r
    VegFC.prev<-VegFC.r
    VegLAI.prev<-VegLAI.r
    # DHSVM.prev<-DHSVM.r
    prev.yr<-yr
  } # Close year loop for interpolation
  
  #------------------------------------------------------#
  ##  Validate results: ----
  # cat('Validating interpolation results...')
  # #------------------------------------------------------#
  # pixel.values<-data.frame('year'=seq(0,max(as.numeric(yrs)),increment))
  # 
  # ## Fractional cover
  # dev.new()
  # par(mfrow=c(1,2),oma=rep(0,4),mar=rep(0,4))
  # r<-raster(file.path(landisOutputDir,'DHSVM',paste0('Veg_FracCov_yr-0.tif')))
  # plot.new();plot.window(xlim=ext(r)[1:2], ylim=ext(r)[3:4],xaxs="i",yaxs="i",asp=1)
  # plot(pwg.r,col='black',add=T,legend=F)
  # mtext('FracCov',font=2,line=-1.75,adj=0.99,cex=1)
  # 
  # pixels<-sample(1:ncell(r),100)
  # pixels<-pixels[!is.na(r[pixels])]
  # 
  # for(i in seq(0,max(as.numeric(yrs)),increment)){
  #   cat(i,', ',sep='')
  #   r<-raster(file.path(landisOutputDir,'DHSVM',paste0('Veg_FracCov_yr-',i,'.tif')))
  #   r[is.na(pwg.r)]<-NA
  #   
  #   r[1:2,1:2]<-c(1,1,0,0) # For consistent colors
  #   
  #   pixel.values[i+1,2:(length(pixels)+1)]<-r[pixels]
  #   
  #   plot(r,col=c('#7f7f7f',hcl.colors(10)),legend=F,add=T)
  #   mtext(2020+i-1,font=1,line=-1,adj=0.99,cex=1,col = 'white')
  #   mtext(2020+i,font=1,line=-1,adj=0.99,cex=1,col = 'black')
  # }
  # # par(mfrow=c(1,1),oma=c(2,2,0,0),mar=rep(1,4))
  # plot(pixel.values$year,pixel.values[,2],type='l',col='blue',xlab='Year',ylab='FracCov',ylim=c(0,1))
  # for(i in 2:ncol(pixel.values)){
  #   lines(pixel.values$year,pixel.values[,i],col=i)
  # }
  # dev.off()
  #------------------------------------------------------#
  
  #### CREATE DHSVM RASTER BASED ON INTERPOLATED MAPS OF HEIGHT AND FC: ----
  cat('\n\n-----------------------------------------------------------\n-> Creating DHSVM raster based on LANDIS-II veg outputs...\n-----------------------------------------------------------
      - Year: ')
  for(yr in seq(0,max(as.numeric(unique(yrs))),increment)){
    cat(paste0(' ',yr))
    dhsvm.r<-pwg.r
    values(dhsvm.r)<-NA
    
    ### Load interpolated ht and fc maps: ----
    ht.r <- rast(file.path(landisOutputDir, 'DHSVM',paste0('Veg_Height-m_yr-',yr,'.tif')))
    fc.r <- rast(file.path(landisOutputDir, 'DHSVM',paste0('Veg_FracCov_yr-',yr,'.tif')))
    lai.r<- rast(file.path(landisOutputDir, 'DHSVM',paste0('Veg_LAI_yr-',yr,'.tif')))
    
    ht.r[pwg.r<12 | is.na(pwg.r)]<-NA # Mask out inactive areas
    fc.r[pwg.r<12 | is.na(pwg.r)]<-NA # Mask out inactive areas
    lai.r[pwg.r<12 | is.na(pwg.r)]<-NA # Mask out inactive areas
    
    fc.r.classed <- classify(fc.r, rcl = fc.df, include.lowest = TRUE)
    ht.r.classed <- classify(ht.r, rcl = ht.df, include.lowest = TRUE)
    
    ## Assign forested classes: ----
    for(i in 1:nrow(rcl.df)){
      if(i %in% seq(1,170,40)) cat('.')
      # cat(paste0(i,', '))
      dhsvm.r[fc.r.classed==rcl.df[i,'fc'] & 
                ht.r.classed==rcl.df[i,'ht'] & 
                pwg.r %in% eval(parse(text=as.character(rcl.df[i,'pwg'])))] <- 
        rcl.df[i,'DHSVM.class']
    }
    unique(values(dhsvm.r))
    plot(dhsvm.r)
    
    ## Assign non-forest classes using LANDFIRE layers: ----
    for(i in 161:173){
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
      ## Classify Agricultural and Urban land using LANDFIRE EVT:
      #   This will only overwrite LANDIS-II pixels within the PWGs specified in rcl.df.
      if(grepl("agricultural dry",rcl.df[i,'pwg.name'])){
        dhsvm.r[pwg.r %in% eval(parse(text=as.character(rcl.df[i,'pwg']))) & 
                  lf.ag.dry ] <- i
      }
      if(grepl("agricultural moist",rcl.df[i,'pwg.name'])){
        dhsvm.r[pwg.r %in% eval(parse(text=as.character(rcl.df[i,'pwg']))) & 
                  lf.ag.moist ] <- i
      }
      if(grepl("agricultural vineyard",rcl.df[i,'pwg.name'])){
        dhsvm.r[pwg.r %in% eval(parse(text=as.character(rcl.df[i,'pwg']))) & 
                  lf.ag.vineyard] <- i
      }
      if(grepl("agricultural orchard",rcl.df[i,'pwg.name'])){
        dhsvm.r[pwg.r %in% eval(parse(text=as.character(rcl.df[i,'pwg']))) & 
                  lf.ag.orchard ] <- i
      }
      if(grepl("urban",rcl.df[i,'pwg.name'])){
        dhsvm.r[pwg.r %in% eval(parse(text=as.character(rcl.df[i,'pwg']))) & 
                  !is.na(lf.urban)] <- i
      }
      if(grepl("snow",rcl.df[i,'pwg.name'])){
        dhsvm.r[pwg.r %in% eval(parse(text=as.character(rcl.df[i,'pwg']))) & 
                  !is.na(lf.snowIce)] <- i
      }
      if(grepl("water",rcl.df[i,'pwg.name'])){
        dhsvm.r[pwg.r %in% eval(parse(text=as.character(rcl.df[i,'pwg']))) & 
                  !is.na(lf.water)] <- i
      }
      if(grepl("barren",rcl.df[i,'pwg.name'])){
        dhsvm.r[pwg.r %in% eval(parse(text=as.character(rcl.df[i,'pwg']))) & 
                  is.na(dhsvm.r) &
                  !is.na(lf.barren)] <- i
      }
      
      ## Use PWG layer to classify a few remaining empty pixels:
      if(grepl("barren",rcl.df[i,'pwg.name'])|grepl("rock",rcl.df[i,'pwg.name'])){
        dhsvm.r[pwg.r %in% eval(parse(text=as.character(rcl.df[i,'pwg']))) &
                  (is.na(ht.r.classed) | is.na(fc.r.classed)) & 
                  is.na(dhsvm.r)] <- i
      }
    }
    # Low herb class for any remaining uncategorized pixels.
    dhsvm.r[is.na(dhsvm.r)&!is.na(pwg.r)]<-164 # Low herb class for any remaining uncategorized pixels.
    
    ## Mask out inactive areas: ----
    dhsvm.r[is.na(ht.r) & is.na(fc.r)]<-NA
    # dhsvm.r[is.na(fc.r)]<-NA
    
    ### TRIM RASTERS TO STUDY AREA: ----
    if(MASK == T){
      dhsvm.r[is.na(pwg.noBuffer.r)]<-NA
      ht.r[is.na(pwg.noBuffer.r)]<-NA
      fc.r[is.na(pwg.noBuffer.r)]<-NA
      lai.r[is.na(pwg.noBuffer.r)]<-NA
    }
    
    ### View result: ----
    if(yr %in% c(0,20,40,60,80,100)){
      ## Load total.biomass raster.
      if(yr %in% yrs.mgmtEra) {
        total.biomass.r <- makeRaster(paste0('TotalBiomass-',yr,'-biomass.tif'),biomassOutput)
      } else total.biomass.r <- makeRaster(paste0('TotalBiomass-',yr - RunOut.start.year,'-biomass.tif'),biomassOutput.RunOut)
      
      # total.biomass.r[is.na(ht.r) & is.na(fc.r)]<-NA # Mask out inactive areas
      
      ## Set NA for inactive areas:
      ht.r[is.na(total.biomass.r)]<-NA
      fc.r[is.na(total.biomass.r)]<-NA
      lai.r[is.na(total.biomass.r)]<-NA
      
      ## Set colors:
      dhsvmCols<-colorRampPalette(c('saddlebrown','goldenrod1','darkslategray','darkgreen','seagreen4',
                                    'turquoise4','dodgerblue4','midnightblue','navy','orchid4','palegoldenrod','wheat'))(49)
      # plot(dhsvm.r,col=dhsvmCols,breaks=seq(1,195,4),legend=F,xaxt='n',yaxt='n')
      
      ## Plot:
      tiff(file.path(landisOutputDir,'DHSVM',paste0('Summary_fig_yr-',yr,'.tif')),width=9.75,height=9,res=300,units='in',compression='lzw')
      
      par(mfrow=c(2,3),oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(1,0.01,0),tck=-0.002,ps=10,cex=1)
      
      plot.new()
      plot.window(xlim=ext(pwg.r)[1:2], ylim=ext(pwg.r)[3:4],xaxs="i",yaxs="i",asp=1)
      plot(pwg.r,col='black',legend=F,add=T)
      plot(total.biomass.r,col=rev(hcl.colors(6,palette = 'Viridis')),breaks=c(1,5000,10000,20000,40000,100000),legend=F,xaxt='n',yaxt='n',add=T)
      plot(dem.r,col=demCols,add=T,alpha=0.15,legend=F)
      plot(hillshade.r,col=colorRampPalette(c('black','grey20','white'))(100),add=T,alpha=0.15,legend=F);box(lwd=3)
      mtext(paste0('Total biomass (',2020+as.numeric(yr),')'),font=2,line=-1,adj=0.99,cex=1)
      
      plot.new()
      plot.window(xlim=ext(pwg.r)[1:2], ylim=ext(pwg.r)[3:4],xaxs="i",yaxs="i",asp=1)
      plot(pwg.r,col='black',legend=F,add=T)
      plot(mean.age.top3.dom.r,col=rev(hcl.colors(7,palette = 'Viridis')),breaks=c(1,20,40,80,120,160,300),legend=F,xaxt='n',yaxt='n',add=T)
      plot(dem.r,col=demCols,add=T,alpha=0.15,legend=F)
      plot(hillshade.r,col=colorRampPalette(c('black','grey20','white'))(100),add=T,alpha=0.15,legend=F);box(lwd=3)
      mtext(paste0('Age (',2020+as.numeric(yr),')'),font=2,line=-1,adj=0.99,cex=1)
      
      plot.new()
      plot.window(xlim=ext(pwg.r)[1:2], ylim=ext(pwg.r)[3:4],xaxs="i",yaxs="i",asp=1)
      plot(pwg.r,col='black',legend=F,add=T)
      plot(lai.r,col=rev(hcl.colors(6,palette = 'Viridis')),breaks=c(1,2,4,6,8,10),legend=F,xaxt='n',yaxt='n',add=T)
      plot(dem.r,col=demCols,add=T,alpha=0.15,legend=F)
      plot(hillshade.r,col=colorRampPalette(c('black','grey20','white'))(100),add=T,alpha=0.15,legend=F);box(lwd=3)
      mtext(paste0('LAI (',2020+as.numeric(yr),')'),font=2,line=-1,adj=0.99,cex=1)
      
      plot.new()
      plot.window(xlim=ext(pwg.r)[1:2], ylim=ext(pwg.r)[3:4],xaxs="i",yaxs="i",asp=1)
      plot(pwg.r,col='black',legend=F,add=T)
      plot(ht.r,col=rev(hcl.colors(length(ht.df$to),palette = 'ag_GrnYl')),breaks=c(0,ht.df$to),legend=F,xaxt='n',yaxt='n',add=T)
      plot(dem.r,col=demCols,add=T,alpha=0.15,legend=F)
      plot(hillshade.r,col=colorRampPalette(c('black','grey20','white'))(100),add=T,alpha=0.15,legend=F);box(lwd=3)
      mtext(paste0('Height (',2020+as.numeric(yr),')'),font=2,line=-1,adj=0.99,cex=1)
      
      plot.new()
      plot.window(xlim=ext(pwg.r)[1:2], ylim=ext(pwg.r)[3:4],xaxs="i",yaxs="i",asp=1)
      plot(pwg.r,col='black',legend=F,add=T)
      plot(fc.r,col=rev(hcl.colors(length(fc.df$to),palette = 'ag_GrnYl')),breaks=c(0,fc.df$to),legend=F,xaxt='n',yaxt='n',add=T)
      plot(dem.r,col=demCols,add=T,alpha=0.15,legend=F)
      plot(hillshade.r,col=colorRampPalette(c('black','grey20','white'))(100),add=T,alpha=0.15,legend=F);box(lwd=3)
      mtext(paste0('Fractional cover (',2020+as.numeric(yr),')'),font=2,line=-1,adj=0.99,cex=1)
      
      plot.new()
      plot.window(xlim=ext(pwg.r)[1:2], ylim=ext(pwg.r)[3:4],xaxs="i",yaxs="i",asp=1)
      plot(pwg.r,col='black',legend=F,add=T)
      # plot(dhsvm.r,col=rev(hcl.colors(173,palette = 'Zissou 1')),legend=F,xaxt='n',yaxt='n',add=T)
      plot(dhsvm.r,col=dhsvmCols,breaks=seq(1,195,4),legend=F,xaxt='n',yaxt='n',add=T)
      plot(dem.r,col=demCols,add=T,alpha=0.15,legend=F)
      plot(hillshade.r,col=colorRampPalette(c('black','grey20','white'))(100),add=T,alpha=0.15,legend=F);box(lwd=3)
      mtext(paste0('DHSVM class (',2020+as.numeric(yr),')'),font=2,line=-1,adj=0.99,cex=1)
      
      dev.off()
    }
    
    ### Output DHSVM map: ----
    dhsvm.r[is.na(dhsvm.r)]<-0
    writeRaster(dhsvm.r,file.path(landisOutputDir, 'DHSVM',paste0('DHSVM_yr-',yr,'.tif')),overwrite=T)
    
    
  } # Close DHSVM year loop
  #---------------------------------------------#   
  
  #------------------------------------------------------#
  #------------------------------------------------------
} # Finished creating DHSVM rasters based on interplated HT and FC maps.
#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
#### Read in all years, replace NA with -9999, and replace 0 with 0.1: ----
cat('\n\n-----------------------------------------------------------------------
          Replacing 0 with 0.1 and NA with -9999\n-----------------------------------------------------------------------\n')
#------------------------------------------------------#
cat('\n- Year: ')
for(yr in seq(0,max(as.numeric(unique(yrs))),increment)){
  cat(yr,', ',sep='')
  
  ### Load interpolated ht and fc maps: ----
  ht.r <- rast(file.path(landisOutputDir, 'DHSVM',paste0('Veg_Height-m_yr-',yr,'.tif')))
  fc.r <- rast(file.path(landisOutputDir, 'DHSVM',paste0('Veg_FracCov_yr-',yr,'.tif')))
  lai.r<- rast(file.path(landisOutputDir, 'DHSVM',paste0('Veg_LAI_yr-',yr,'.tif')))
  
  ht.r[ht.r==0]<-0.1
  fc.r[fc.r==0]<-0.01
  lai.r[lai.r==0]<-0.1
  
  ht.r[pwg.r<12 | is.na(pwg.r) | is.na(pwg.noBuffer.r)]  <- -9999
  fc.r[pwg.r<12 | is.na(pwg.r) | is.na(pwg.noBuffer.r)]  <- -9999 
  lai.r[pwg.r<12 | is.na(pwg.r) | is.na(pwg.noBuffer.r)] <- -9999 
  
  ### Output DHSVM map: ----
  writeRaster(ht.r,file.path(landisOutputDir, 'DHSVM',paste0('Veg_Height-m_yr-',yr,'.tif')),overwrite=T)
  writeRaster(fc.r,file.path(landisOutputDir, 'DHSVM',paste0('Veg_FracCov_yr-',yr,'.tif')),overwrite=T)
  writeRaster(lai.r,file.path(landisOutputDir, 'DHSVM',paste0('Veg_LAI_yr-',yr,'.tif')),overwrite=T)
  
} # Close year loop
#---------------------------------------------#   
#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
#### Zip results: ----
scenarioName = gsub(dirToProcess,"",landisOutputDir)
scenarioName = gsub("[_]|LANDIS_Sim_Wen|/","",scenarioName)
scenarioName = gsub("NECN","100yr",scenarioName)
#scenarioName = gsub("NECN","30yr",scenarioName)
scenarioName = gsub("SCRAPPLE","WILDFIRE",scenarioName)
scenarioName = gsub("HARVEST","_HARVEST",scenarioName)
scenarioName = gsub("NECN100","NO_FIRE",scenarioName)
scenarioName = gsub("NECN","NO_FIRE",scenarioName)
scenarioName = paste(substr(scenarioName,1,nchar(scenarioName)-12),substr(scenarioName,nchar(scenarioName)-11,nchar(scenarioName)-4),sep='_')
scenarioName

if(OVERWRITE.ZIP.FILES==T |
   !file.exists(paste0(dirToProcess,'/DHSVM_Inputs_',scenarioName,'.zip'))){
  cat('\n***  ZIPPING DHSVM output maps for',landisOutputDir,'  ***\n')
  zipr(paste0(dirToProcess,'/DHSVM_Inputs_',scenarioName,'.zip'),files = file.path(landisOutputDir,'DHSVM',dir(file.path(landisOutputDir,'DHSVM'))))
}
gc()
#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------

rm(list=ls()[grepl('Veg',ls())|grepl('lf.',ls())|grepl('dom',ls())])
cat('\n\n
===================================================================================================
***************************************************************************************************
                             Finished generating DHSVM output maps.             
---------------------------------------------------------------------------------------------------
***************************************************************************************************
===================================================================================================\n\n')

#----------------------------------------------------------------------------------------------------------------------
#######################################################################################################################
#----------------------------------------------------------------------------------------------------------------------
gc()
if(RUN.DST.MAPS==T){
source('R4_Scenarios_DSTs/R4_Run_DST_Maps.R')
}

#----------------------------------------------------------------------------------------------------------------------
#######################################################################################################################
#----------------------------------------------------------------------------------------------------------------------
### TRIM OUTPUT RASTERS TO HUC EXTENT: ----
# TODO: build into dst

#----------------------------------------------------------------------------------------------------------------------
#######################################################################################################################
#----------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------
#######################################################################################################################
#######################################################################################################################
#----------------------------------------------------------------------------------------------------------------------
### END ########################################################################################################### ---- 
cat(paste('\n\nDHSVM routine complete.', Sys.time(), '\n'), file = outFile, append = T)
stop('\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n
                                                      ####################################################
                                                            =========================================
                                                              -------------------------------------
                                                              
                                                           Code ran successfully! No errors detected.
                                                                  
                                                              -------------------------------------
                                                          *********************************************
                                                     ######################################################\n',call.=F)

#----------------------------------------------------------------------------------------------------------------------
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################

