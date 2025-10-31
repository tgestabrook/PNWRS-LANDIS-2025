########################################################################################################################-
########################################################################################################################-
########################################################################################################################-
###                    Generate DHSVM-classified Vegetation Maps from LANDIS-II Outputs                          ######-
###                              and Generate Decision Support Tool layers                                       ######-
#----------------------------------------------------------------------------------------------------------------------#
###   EXTENT: Wenatchee, Entiat, Okanogan, and Methow sub-basins                                                 ######-
###   PROJECT: BIOMASS Phase 3 for Pacific Northwest Research Station                                            ######-    
###   DATE: August 2022                                                                                       ######-   
#----------------------------------------------------------------------------------------------------------------------#
###   Code developed by Tucker Furniss                                                                           ######-
###     Contact: tucker.furniss@usda.gov; tucker.furniss@gmail.com;                                              ######-
#-----------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------#
###   DEPENDENCIES: None. Does not rely on 3_Process_LANDIS_Outputs.R script. The Biomass_Annual_Dynamics.csv file is loaded, but not used.
###                 However, Section 2 (DST) requires Section 1 (DHSVM) to be run first (generates maps of dominant species).
#-----------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------#

wDir <- file.path('F:', "LANDIS_Input_Data_Prep", LANDIS.EXTENT)


#----------------------------------------------------------------------------------------------------------------------
### Trim output to wenatchee - entiat without 5-km buffer? ----
MASK = T

### Interpolate DHSVM maps to a finer temporal resolution? ----
INTERPOLATE.DHSVM = T

#----------------------------------------------------------------------------------------------------------------------
### Trim output to a HUC10 or HUC12? ----
# TRIM.TO.HUC = c(170200110105,170200110103,170200110111,170200110110,170200110109,170200110108,170200110107,170200110106,170200110101)
# TRIM.TO.HUC = 1702001102 # Nason creek study domain.
TRIM.TO.HUC = F
if(length(unique(nchar(TRIM.TO.HUC)))>1) stop('TRIM.TO.HUC values must be a vector of HUC codes AT THE SAME SCALE. 
                                              You can use HUC12, HUC11, or HUC10, but you cant mix them.')
#----------------------------------------------------------------------------------------------------------------------
### Define subset of years to run: ----
# yrs.subset = c(0,50,100) # Set to NULL to run all years
yrs.subset = NULL # Set to NULL to run all years
if(!is.null(yrs.subset) & INTERPOLATE.DHSVM == T) {
  yrs.subset = NULL 
  warning('INTERPOLATE set to TRUE but you attempted to use yrs.subset. Must use all available yrs if you want to interpolate to a finer temporal resolution.')
}

#----------------------------------------------------------------------------------------------------------------------
#######################################################################################################################
####                                   Load base data and some handy functions                                      ####
#----------------------------------------------------------------------------------------------------------------------

active.ecos<-ecos.txt[ecos.txt$V1=='yes','V3']
active.pwgs<-unique(substr(ecos.txt[ecos.txt$V1=='yes','V2'],1,2))

#-------------------------------------------------------------------
### Suppress .tif.aux.xml files: ----
## To prevent the creation of .tif.aux.xml files: 
#invisible(rgdal::setCPLConfigOption("GDAL_PAM_ENABLED", "FALSE"))

### Set color palettes: ----
sppCols<-colorRampPalette(c('darkslategray','goldenrod1','lightcoral','tomato3','wheat','firebrick4','orchid4','dodgerblue',
                            'turquoise','forestgreen','darkgreen','olivedrab1','aquamarine4','dodgerblue4'))(100)
sppColsRandom<-sample(sppCols,35,replace=F)

fireCols<-colorRampPalette(c('grey20','darkslategray','goldenrod1','tomato2','firebrick4'))(10)
# plot(1:length(fireCols),rep(0,length(fireCols)),col=fireCols,pch=15,cex=5)



### Function to round up or down to nearest value of roundTo: ----
roundFUN <- function(x, roundTo, dir = 0) {
  if(dir == 1) {  ##ROUND UP
    x + (roundTo - x %% roundTo)
  } else {
    if(dir == 0) {  ##ROUND DOWN
      x - (x %% roundTo)
    }
  }
}
roundFUN(1:15,5)

raster2csv<-function(r, agg.r = pwg.r, na.value = NA){
  if(!is.na(na.value)){r[r==na.value]<-NA}
  
  r <- ifel(is.na(pwg.r), NA, r)
  
  df<-zonal(r,agg.r,fun=function(x,...) {return(data.frame('area.ha'=length(x) * 0.81,'occupied.area.ha'=length(x[!is.na(x)]) * 0.81,
                                                           'sum'=sum(x,na.rm=T),'mean'=mean(x,na.rm=T),'sd'=sd(x,na.rm=T)))})
  df<-cbind(as.data.frame(df[,1]), as.data.frame(df[,2]))
  
  df<-setNames(as.data.frame(df),c('zone','area.ha','occupied.area.ha','sum','mean','sd'))
  df$area.ha <- unlist(df$area.ha)
  df$occupied.area.ha <- unlist(df$occupied.area.ha)
  df$sum <- unlist(df$sum)
  df$mean <- unlist(df$mean)
  df$sd <- unlist(df$sd)
  
  
  df<-df[order(df$zone),]
  return(df)
}
#----------------------------------------------------------------------------------------------------------------------
#######################################################################################################################
####                                 NOW LOOP TO RUN ALL LANDIS OUTPUT FOLDERS:                                     ####
#######################################################################################################################
#----------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------
### Run DHSVM! ----
if(simOpts$RUN.DHSVM.MAPS==T){
  if(file.exists(file.path(landisOutputDir,'DHSVM','DHSVM_yr-100.tif')) & simOpts$RERUN.DHSVM.MAPS == F){
    cat('\nDHSVM outputs already exist for',gsub(dirToProcess,"",landisOutputDir),'. Skipping to next sim...')
    next
  }
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
  
  ########################################################################################################################
  #-----------------------------------------------------------------------------------------------------------------------
  #####  ---------------    DHSVM    --------------------------------------------------------------------------------- #####
  #-----------------------------------------------------------------------------------------------------------------------
  ### Load LANDFIRE raster and definitions: ----
  if(ext(landfire.r)!=ext(pwg.r)) stop('Extent of pwg.r does not match extent of landfire.r. This will cause major issues.')

  #-----------------------------------------------------------------------------------------------------------------------
  ### Define LANDFIRE non-forest classes: ----
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
  load(file.path(wDir,'Fc_AgeBiomassSppPWG_glm.Rdata'))
  predict(fc.glm,newdata=data.frame('Mean.Age'=100,'Biomass.sum.gm2'=seq(1000,2000,100),'PWG'=40,'Elevation'=1000),type='response')
  
  ### Height model: ----
  load(file.path(dataDir,'Ht_AgeBiomassSppPWG_glm.Rdata'))
  predict(ht.glm,newdata=data.frame('Age'=seq(1,100,20),'Biomass_gm2'=1000,'Species'='PinuPond','PWG'=40),type='response')
  
  #-----------------------------------------------------------------------------------------------------------------------
  #-----------------------------------------------------------------------------------------------------------------------
  #### Loop through years to generate HT, FC, Mean Age, Dom Spp, LAI, and DHSVM maps at succession timesteps: ----
  ## If maps aren't present, re-run. If maps are present, this step can be skipped by setting RERUN.DHSVM.HEIGHT_FC_and_LAI_MAPS to FALSE
  if(FALSE %in% c(paste0('Veg_Height-m_yr-',yrs,'.tif') %in% dir(file.path(landisOutputDir,'DHSVM'))) | 
     FALSE %in% c(paste0('MeanAge_AllSpp_yr',yrs,'.tif') %in% dir(file.path(landisOutputDir,'ageOutput'))))
    simOpts$RERUN.DHSVM.HEIGHT_FC_and_LAI_MAPS<-T 
  
  LAI.stack <- rast(file.path(necnOutput, "LAI-yr.tif"))
  MedAge.stack <- rast(file.path(ageOutput, dir(ageOutput)[grepl("yr-MED", dir(ageOutput))]))
  MaxAge.stack <- rast(file.path(ageOutput, dir(ageOutput)[grepl("yr-MAX", dir(ageOutput))]))
  BiomassTrees.stack <- biomassStack.r |> select(!starts_with(c("Nfixer_Resprt","NonFxr_Resprt","NonFxr_Seed","Grass_Forb","TotalBiomass")))
  BiomassNotTrees.stack <- biomassStack.r |> select(starts_with(c("Nfixer_Resprt","NonFxr_Resprt","NonFxr_Seed","Grass_Forb")))
  BiomassAll.stack <- biomassStack.r |> select(!starts_with("TotalBiomass"))
  
  if(simOpts$RERUN.DHSVM.HEIGHT_FC_and_LAI_MAPS==T){
    cat('\n\n----------------------------------------------------------------------------------
Looping through years...\n----------------------------------------------------------------------------------\n')
    for(yr in unique(yrs)){
      cat(paste0('\n------------\nYear: ',yr,'\n------------\n'))

      # biomassMapFiles<-biomassMaps
      # ageMapFiles<-ageMaps
      # laiMapFiles<-laiMaps
      
      cat('-> Loading Total LAI and Biomass maps...\n')
      ### LAI: ----
      ##   For Year 0, you must generate a YR-0 LAI map by running LANDIS with 1-yr succession time steps and no fire and no harvest. Save this file to the data directory. 
      if(as.numeric(yr)==0){
        lai.r<-rast(file.path(dataDir,'NECN_Outputs_Yr_0', LANDIS.EXTENT, 'LAI-1.tif'))
        lai.0<-lai.r
      } else lai.r<-LAI.stack |> select(ends_with(paste0('-', yr)))
      
      ### FIX LAI. NECN v6.8 has a bug in LAI code: ----
      ##   This bug makes LAI calculation cumulative for all trees in a cell rather than making LAI diminish as the cell fills up.
      ##   The result is that some cells have unreasonably high LAI.
      ##   To fix, scale LAI values to some theoretical maximum. Let's use 10.
      ## Set reclass parameters:
      maxLAI<-10 # This prduces max LAI values ~8 (asymptote reached only if input values are very large)
      maxLAI.t0<-max(values(lai.0), na.rm=T)
      if(maxLAI.t0>maxLAI){
        inflection<-5
        x=0:maxLAI.t0
        
        ## Reclass function:
        # lai.df<-data.frame('is'=x,'becomes'=seq(0,maxLAI,length.out=length(x))) # Linear function
        lai.df<-data.frame('is'=x,'becomes'= round(maxLAI * x/(inflection + x),1)) # Saturation function
        
        plot(lai.df$is,lai.df$becomes,pch=21,bg=rev(hcl.colors(nrow(lai.df),palette = 'ag_GrnYl')),ylim=c(0,maxLAI))
        lines(lai.df$is,lai.df$becomes,col=grey(0.2,0.1));lines(0:maxLAI.t0,0:maxLAI.t0,col=grey(0.5,0.5),lty='dotted');abline(h=maxLAI,col='darkred')
        
        ## Reclassify:
        
        lai.r[lai.r>maxLAI.t0]<-maxLAI.t0
        if(length(lai.r[!lai.r%in%lai.df$is & !is.na(lai.r)])>0){
          lai.r<-round(lai.r,0)
          warning('Raw LAI raster has non-integer values. Rounding to integers for reclassification.')
        }
        lai.r<-classify(lai.r,rcl=lai.df)
        
      }
      
      ## Some cells have LAI = NA but non-zero biomass:
      ## Set LAI to 0.1 for cells with non-zero biomass but LAI == 0:
      lai.r <- ifel(lai.r==0&!is.na(total.biomass.r)&total.biomass.r>0, 0.1, lai.r)
      
      ### Biomass: ----
      total.biomass.r<-totalBiomass_stack.r |> select(contains(paste0('-', yr, '-')))
      
      ### Age: ----
      cat('-> Loading Species Age and Biomass maps')
      med.age<-MedAge.stack |> select(contains(paste0('-', yr, '-')))
      max.age<-MaxAge.stack |> select(contains(paste0('-', yr, '-')))
      biomass.trees<-BiomassTrees.stack |> select(contains(paste0('-', yr, '-')))
      biomass.notTree<-BiomassNotTrees.stack |> select(contains(paste0('-', yr, '-')))
      biomass.all<-BiomassAll.stack |> select(contains(paste0('-', yr, '-')))
      
      dominant.spp.lookup<-1:length(names(biomass.trees))
      names(dominant.spp.lookup)<-str_split_i(names(biomass.trees), "-", 1)
      
      cat('\n-> Identifying top 3 dominant species per site')
      top3sp.df <- c(pwg.r, biomass.trees) |>
        as.data.frame(xy=T) |>
        filter(!is.na(PWG)) |>  # save some memory by dropping cells outside study area
        pivot_longer(contains("biomass"), names_to = c("Species", "Year", "drop"), names_sep = '-', values_to = "Biomass_gm2", values_drop_na = T) |>
        select(!c(drop, Year)) |>
        filter(Biomass_gm2 > 0) |>  # don't bother ranking species-pixels with 0 biomass
        group_by(x, y) |>  # we want the top three species per pixel, so group by pixel and year
        mutate(Rank = rank(-Biomass_gm2),
               PWG = as.factor(PWG)) |> # grab the top 3 species in each pixel-year
        ungroup() |>
        filter(Rank <= 3) |>
        select(x, y, Rank, Species)  # only keep pixel location, species, and rank to make joining easier later
      
      topsp.df <- top3sp.df |> 
        filter(Rank == 1)
      
      # mean.age.r
      mean.age.r<-ifel(is.na(pwg.r), NA, mean(med.age,na.rm=T))
      
      # max.age.r
      max.age.r<-ifel(is.na(pwg.r), NA, max(max.age,na.rm=T))
      
      # mean.age.domSpp.r
      mean.age.domSpp.r <- c(med.age) |>
        as.data.frame(xy=T) |>
        pivot_longer(contains("MED"), names_to = c("Species", "Year", "drop"), names_sep = '-', values_to = 'Med.Age') |>
        select(!c(drop, Year)) |>
        inner_join(topsp.df) |>
        # group_by(x, y) |>
        # summarise(Mean.Age = mean(Med.Age)) |>
        # ungroup() |>
        rast(type = "xyz", crs = crs(pwg.r), ext = ext(pwg.r))
      
      # mean.age.top3.dom.r
      cat('\n-> Calculating mean age of dominant 3 species per site')
      mean.age.top3.dom.r <- c(med.age) |>
        as.data.frame(xy=T) |>
        pivot_longer(contains("MED"), names_to = c("Species", "Year", "drop"), names_sep = '-', values_to = 'Med.Age') |>
        select(!c(drop, Year)) |>
        inner_join(top3sp.df) |>
        group_by(x, y) |>
        summarise(Mean.Age = mean(Med.Age)) |>
        ungroup() |>
        rast(type = "xyz", crs = crs(pwg.r), ext = ext(pwg.r))
      
      
      # dominant.spp
      dominant.spp <- top3sp.df |>
        filter(Rank == 1) |>
        left_join(data.frame("Species" = names(dominant.spp.lookup), "spcode" = dominant.spp.lookup)) |>  # get numerical species code
        select(x, y, spcode) |>
        rast(type = "xyz", crs = crs(pwg.r), ext = ext(pwg.r))
      
      # dominant.spp2
      dominant.spp2 <- top3sp.df |>
        filter(Rank == 2) |>
        left_join(data.frame("Species" = names(dominant.spp.lookup), "spcode" = dominant.spp.lookup)) |>  # get numerical species code
        select(x, y, spcode) |>
        rast(type = "xyz", crs = crs(pwg.r), ext = ext(pwg.r))
      
      # 
      # 
      # ## Dominant species by biomass: ----
      # dominant.spp<-which.max(biomass.trees) # How to identify the dominant species per cell???
      # dominant.spp.lookup<-1:length(names(biomass.trees))
      # names(dominant.spp.lookup)<-str_split_i(names(biomass.trees), "-", 1)
      # 
      # ## Second and third dominant by biomass: ----
      # ##  Biomass values, ordered (layer 1 is highest biomass, layer 2 is second highest, etc.)
      # dom.spp.ordered<-app(biomass.trees, fun=function(x,na.rm) x[order(x,decreasing=T)])  # function used to be calc
      # 
      # dominant.spp2<-rast(dominant.spp, vals = 0)  # initialize second and third place dominant species rasters
      # dominant.spp3<-rast(dominant.spp, vals = 0)
      # 
      # for(sp in sample(names(dominant.spp.lookup))){  # sample names to randomize in case of ties, species going first wins
      #   cat('.')
      #   ## Create a mask for where each species is within the top 3 per site:
      #   # Second highest biomass:
      #   spbiomass.r <- biomass.trees |> select(contains(sp))
      #   dominant.spp2<-ifel(spbiomass.r == dom.spp.ordered[[2]] & dominant.spp2 == 0,dominant.spp.lookup[sp], dominant.spp2)
      #   
      #   dominant.spp3<-ifel(spbiomass.r == dom.spp.ordered[[3]] & dominant.spp3 == 0, dominant.spp.lookup[sp], dominant.spp3)
      # }
      # ## View where AbieAmab is the 1st, 2nd, and 3rd dominant species by biomass
      # # plot(dominant.spp==3,col=c('white','darkred'))
      # # plot(dominant.spp2==3,col=c(NA,'orange'),add=T)
      # # plot(dominant.spp3==3,col=c(NA,'gold'),add=T)
      # 
      # ## Max age of any species: ----
      # max.age.r<-max(max.age,na.rm=T)
      # # plot(max.age.r)
      # 
      # ## Mean age of dominant species per site: ----
      # mean.age.domSpp.r<- rast(max.age.r, vals = 0)
      # 
      # for(sp in sample(str_split_i(names(dominant.spp.lookup), "-", 1))){
      #   sp.num<-dominant.spp.lookup[sp]
      #   sp.mask<-ifel(dominant.spp == sp.num, 1, NA) #sp.dom.r==sp.num
      # 
      #   sp.med.age<-mask(med.age |> select(contains(sp)),sp.mask)
      #   sp.med.age<-ifel(is.na(sp.med.age), 0, sp.med.age)
      # 
      #   mean.age.domSpp.r<-mean.age.domSpp.r+sp.med.age
      # }
      # # plot(mean.age.domSpp.r)
      # 
      # ## Mean age of dominant 3 species per site: ----
      # cat('\n-> Calculating mean age of dominant 3 species per site')
      # age.top3.dom.Spp<-list()
      # for(sp in names(dominant.spp.lookup)){
      #   cat('.')
      #   # cat(paste0(sp,', '))
      #   sp.num<-dominant.spp.lookup[sp]
      #   sp.med.age <- med.age |> select(contains(sp))  # grab median age raster for this species
      #   
      #   sp.med.age <- ifel(dominant.spp==sp.num | dominant.spp2==sp.num | dominant.spp3==sp.num, sp.med.age, NA)
      #   
      #   # sp.med.age<-ifel(sp.med.age==0, NA, sp.med.age)
      #   age.top3.dom.Spp[[sp]]<-sp.med.age
      # }
      # age.top3.dom.Spp<-rast(age.top3.dom.Spp)
      # # It works! See:
      # # med.age[694,400]
      # # age.top3.dom.Spp[694,400]
      # mean.age.top3.dom.r<-mean(age.top3.dom.Spp,na.rm=T)
      # mean.age.top3.dom.r<-ifel(is.na(pwg.r), NA, mean.age.top3.dom.r)
      # # plot(mean.age.top3.dom.r)
      # 
      # # Compare to the overall mean median age (this method is higher if there are younger cohorts present that weight down the med age, lower if the dominant cohort is very old):
      # mean.age.r<-mean(med.age,na.rm=T)
      # mean.age.r<-ifel(is.na(pwg.r), NA, mean.age.r)
      # plot(mean.age.r)
      # plot(mean.age.domSpp.r-mean.age.r,col=colorRampPalette(c('red','gold','grey80','blue','green','darkgreen'))(50))
      #---------------------------------------------#      
      ### Generate Rasters for average FC and Height per cell: ----
      cat('\n-> Generating rasters for FC and Height...\n')
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
        mutate(Pred = ifelse(is.na(Pred) & Biomass.sum.gm2 > 0, 0.1, Pred)) |>  # if grasses where pred is NA but there is biomass, set the value to 0.1
        select(x, y, Pred) |>
        rast(type = 'xyz', crs = crs(pwg.r), ext = ext(pwg.r))
      
      plot(fc.r)
      
      gc()
      ht.r <- c(pwg.r, total.biomass.r, mean.age.top3.dom.r) |>
        as.data.frame(xy=T) |>
        filter(!is.na(PWG)) |>
        mutate(
          PWG = as.factor(ifelse(!PWG%in%levels(ht.glm@frame$PWG), 50, PWG))  # The bareground PWG level (11) is absent from fc.glm model... model FC as if these were in PWG = 50 (cold-dry conifer)
        ) |>
        left_join(top3sp.df |> filter(Rank == 1)) |>  # join the df with top species by biomass but only keep top-ranked
        rename(Age = names(mean.age.top3.dom.r)[1], Biomass_gm2 = names(total.biomass.r)[1]) |>
        mutate(Pred = predict(ht.glm, pick(Age, Biomass_gm2, PWG, Species))) |>
        mutate(Pred = ifelse(Pred < 0, 0, Pred)) |>
        mutate(Pred = ifelse((is.na(Pred) | Pred <= 1) & Biomass_gm2 > 0, 1, Pred)) |> # if biomass > 0 but the prediction is 0 or NA, set height to 1.
        select(x, y, Pred) |>
        rast(type = 'xyz', crs = crs(pwg.r), ext = ext(pwg.r))
      plot(ht.r)
      
      # 
      # 
      # 
      # 
      # 
      # 
      # ## Create data frames based on raster values
      # fc.pred.df<-data.frame('Mean.Age'=unname(values(mean.age.top3.dom.r)),'Biomass.sum.gm2'=unname(values(total.biomass.r)),
      #                        'PWG'=unname(values(pwg.r)),'Elevation'=round(unname(values(dem.r)),-1),'Pred'=NA)
      # ## The bareground PWG level (11) is absent from fc.glm model... model FC as if these were in PWG = 12 (grassland)
      # fc.pred.df[!fc.pred.df$PWG%in%levels(fc.glm@frame$PWG)&!is.na(fc.pred.df$PWG),'PWG']<-12
      # 
      # ## Create data frames based on raster values
      # ht.pred.df<-data.frame('Age'=unname(values(mean.age.top3.dom.r)),'Biomass_gm2'=unname(values(total.biomass.r)),
      #                        'Species'=names(dominant.spp.lookup[values(dominant.spp)]),'PWG'=unname(values(pwg.r)),'Pred'=NA)
      # ## For cells with non-zero biomass with species = NA, fill in with grass:
      # ht.pred.df[is.na(ht.pred.df$Species) & !is.na(ht.pred.df$Biomass_gm2),'Age']<-1
      # ## The bareground PWG level (11) is absent from fc.glm model... model FC as if these were in PWG = 50 (cold-dry conifer)
      # ht.pred.df[!ht.pred.df$PWG%in%levels(fc.glm@frame$PWG)&!is.na(ht.pred.df$PWG),'PWG']<-50
      # 
      # ## A handful of cells are PWG NA but have tree data...
      # if (nrow(fc.pred.df[!is.na(fc.pred.df$Mean.Age)&is.na(fc.pred.df$PWG),])>0){
      #   print(paste(nrow(fc.pred.df[!is.na(fc.pred.df$Mean.Age)&is.na(fc.pred.df$PWG),]), 'pixels with tree data but NA pwg.'))
      #   fc.pred.df[!is.na(fc.pred.df$Mean.Age)&is.na(fc.pred.df$PWG),'PWG'] <- 50
      # }
      # 
      # if (nrow(ht.pred.df[!is.na(ht.pred.df$Age)&is.na(ht.pred.df$PWG),])>0){
      #   print(paste(nrow(ht.pred.df[!is.na(ht.pred.df$Age)&is.na(ht.pred.df$PWG),]), 'pixels with tree data but NA pwg.'))
      #   ht.pred.df[!is.na(ht.pred.df$Age)&is.na(ht.pred.df$PWG),'PWG'] <- 50
      # }
      # 
      # #---------------------------------------------#      
      # ## Generate fractional cover predicted values
      # fc.pred.df[!is.na(fc.pred.df$Mean.Age),'Pred'] <- 
      #   predict(fc.glm,newdata=fc.pred.df[!is.na(fc.pred.df$Mean.Age),],type='response')
      # 
      # ## Genearte height predicted values
      # ht.pred.df[!is.na(ht.pred.df$Age) & ht.pred.df$Species%in%levels(ht.glm@flist$Species),'Pred'] <- 
      #   predict(ht.glm,newdata=ht.pred.df[!is.na(ht.pred.df$Age) & ht.pred.df$Species%in%levels(ht.glm@flist$Species),],type='response')
      # # Generic species form for species absent from the model factor levels:
      # ht.pred.df[!is.na(ht.pred.df$Age) & !ht.pred.df$Species%in%levels(ht.glm@flist$Species) & !is.na(ht.pred.df$Species),'Pred'] <- 
      #   predict(ht.glm,newdata=ht.pred.df[!is.na(ht.pred.df$Age) & !ht.pred.df$Species%in%levels(ht.glm@flist$Species) & !is.na(ht.pred.df$Species),],type='response',re.form=~(1 + Age | PWG))
      # 
      # # For cells with non-zero biomass with species = NA, give FC layer low grass cover:
      # fc.pred.df[is.na(fc.pred.df$Pred) & !is.na(ht.pred.df$Pred),'Pred']<-0.1
      # #---------------------------------------------#      
      # ## Convert FC to 0 to 1 scale:
      # if(mean(fc.pred.df$Pred,na.rm=T)>1)
      #   fc.pred.df$Pred<-fc.pred.df$Pred / 100
      # ## Limit FC to 100:
      # fc.pred.df[fc.pred.df$Pred>1&!is.na(fc.pred.df$Pred),'Pred']<-1
      # # Fix negative values
      # ht.pred.df[ht.pred.df$Pred<0&!is.na(ht.pred.df$Pred),'Pred']<-0
      # fc.pred.df[fc.pred.df$Pred<0&!is.na(fc.pred.df$Pred),'Pred']<-0
      # 
      # # Create blank rasters
      # fc.r<-pwg.r
      # values(fc.r)<-NA
      # ht.r<-fc.r
      # 
      # # Assign predicted values
      # values(fc.r)<-fc.pred.df$Pred
      # values(ht.r)<-ht.pred.df$Pred
      
      # plot(fc.r)
      # plot(ht.r)
      # assign(paste0('FC.',yr),fc.r)
      # assign(paste0('HT.',yr),ht.r)
      
      #---------------------------------------------#      
      
      # fc.r<-eval(parse(text=paste0('FC.',yr)))
      # ht.r<-eval(parse(text=paste0('HT.',yr)))
      
      # Reclass raster to DHSVM Canopy cover class
      fc.r.classed <- classify(fc.r, rcl = fc.df, include.lowest = TRUE)
      ht.r.classed <- classify(ht.r, rcl = ht.df, include.lowest = TRUE)
      
      #---------------------------------------------#      

      
      #---------------------------------------------#      
      #### CREATE DHSVM RASTER BASED ON HEIGHT, FC, and PWG: #### ----
      ##   *** NOTE: if INTERPOLATE.DHSVM == T, we'll have to re-do this based on interpolated height and FC maps. Skip this here to save time.
      #---------------------------------------------#   
      ### Trim rasters to study area: ----
      if(MASK == T){
        for(r.name in c('ht.r','fc.r','lai.r','total.biomass.r')){
          r<-eval(parse(text=r.name))
          r <- ifel(is.na(pwg.noBuffer.r), NA, r)
          # r[is.na(pwg.noBuffer.r)]<-NA
          
          assign(r.name,r)
        }
        plot(total.biomass.r,col=hcl.colors(173))
      }
      #---------------------------------------------#   
      ### View result: ----
      if(as.numeric(yr) %in% c(0,20,40,60,80,100)){
        dhsvmCols<-colorRampPalette(c('saddlebrown','goldenrod1','darkslategray','darkgreen','seagreen4',
                                      'turquoise4','dodgerblue4','midnightblue','navy','orchid4','palegoldenrod','wheat'))(49)
        
        tiff(file.path(landisOutputDir,'DHSVM',paste0('Summary_fig_yr-',yr,'.tif')),width=6.5,height=9,res=300,units='in',compression='lzw')
        
        par(mfrow=c(2,2),oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(1,0.01,0),tck=-0.002,ps=10,cex=1)
        plot.new()
        plot.window(xlim=ext(pwg.r)[1:2], ylim=ext(pwg.r)[3:4],xaxs="i",yaxs="i",asp=1)
        plot(pwg.r,col='black',legend=F,add=T)
        plot(ht.r,col=rev(hcl.colors(length(ht.df$to),palette = 'ag_GrnYl')),breaks=c(0,ht.df$to),legend=F,xaxt='n',yaxt='n',add=T)
        plot(dem.r,col=demCols(50),add=T,alpha=0.15,legend=F)
        plot(hillshade.r,col=colorRampPalette(c('black','grey20','white'))(100),add=T,alpha=0.15,legend=F);box(lwd=3)
        mtext(paste0('Height (',2020+as.numeric(yr),')'),font=2,line=-1,adj=0.99,cex=1)
        
        plot.new()
        plot.window(xlim=ext(pwg.r)[1:2], ylim=ext(pwg.r)[3:4],xaxs="i",yaxs="i",asp=1)
        plot(pwg.r,col='black',legend=F,add=T)
        plot(fc.r,col=rev(hcl.colors(length(fc.df$to),palette = 'ag_GrnYl')),breaks=c(0,fc.df$to),legend=F,xaxt='n',yaxt='n',add=T)
        plot(dem.r,col=demCols(50),add=T,alpha=0.15,legend=F)
        plot(hillshade.r,col=colorRampPalette(c('black','grey20','white'))(100),add=T,alpha=0.15,legend=F);box(lwd=3)
        mtext(paste0('Fractional cover (',2020+as.numeric(yr),')'),font=2,line=-1,adj=0.99,cex=1)
        
        plot.new()
        plot.window(xlim=ext(pwg.r)[1:2], ylim=ext(pwg.r)[3:4],xaxs="i",yaxs="i",asp=1)
        plot(pwg.r,col='black',legend=F,add=T)
        plot(total.biomass.r,col=rev(hcl.colors(6,palette = 'Viridis')),breaks=c(1,5000,10000,20000,40000,100000),legend=F,xaxt='n',yaxt='n',add=T)
        plot(dem.r,col=demCols(50),add=T,alpha=0.15,legend=F)
        plot(hillshade.r,col=colorRampPalette(c('black','grey20','white'))(100),add=T,alpha=0.15,legend=F);box(lwd=3)
        mtext(paste0('Total biomass (',2020+as.numeric(yr),')'),font=2,line=-1,adj=0.99,cex=1)
        
        dev.off()
      }
      #---------------------------------------------#   
      ### WRITE RASTERS: ----
      for(r.name in c('ht.r','fc.r','lai.r','total.biomass.r')){
        r<-eval(parse(text=r.name))
        r<-round(r,2)
        r[is.na(r)]<-0
        assign(r.name,r)
      }
      
      # writeRaster(dhsvm.r,file.path(landisOutputDir, 'DHSVM',paste0('DHSVM_yr-',yr,'.tif')),overwrite=T)
      writeRaster(ht.r,file.path(landisOutputDir, 'DHSVM',paste0('Veg_Height-m_yr-',yr,'.tif')),overwrite=T)
      writeRaster(fc.r,file.path(landisOutputDir, 'DHSVM',paste0('Veg_FracCov_yr-',yr,'.tif')),overwrite=T)
      writeRaster(lai.r,file.path(landisOutputDir, 'DHSVM',paste0('Veg_LAI_yr-',yr,'.tif')),overwrite=T)
      
      ## Save Age Rasters:
      for(r.name in c('mean.age.r','max.age.r','mean.age.domSpp.r','mean.age.top3.dom.r','dominant.spp','dominant.spp2')){
        r<-eval(parse(text=r.name))
        r<-round(r,0)
        assign(r.name,r)
      }
      writeRaster(mean.age.r,file.path(ageOutput, paste0('MeanAge_AllSpp_yr-',yr,'.tif')),overwrite=T)
      writeRaster(max.age.r,file.path(ageOutput, paste0('MaxAge_AllSpp_yr-',yr,'.tif')),overwrite=T)
      writeRaster(mean.age.domSpp.r,file.path(ageOutput, paste0('MeanAge_DominantSpecies_yr-',yr,'.tif')),overwrite=T)
      writeRaster(mean.age.top3.dom.r,file.path(ageOutput, paste0('MeanAge_Top3Species_yr-',yr,'.tif')),overwrite=T)
      writeRaster(dominant.spp,file.path(ageOutput, paste0('DominantSpecies-',yr,'.tif')),overwrite=T)
      writeRaster(dominant.spp2,file.path(ageOutput, paste0('DominantSpecies2-',yr,'.tif')),overwrite=T)
      
      cat('----> Complete! <----\n')
      
      rm(ht.r,fc.r,ht.r.classed,fc.r.classed,lai.r)
    } # END YEAR LOOP
  } # Finished creating Ht, FC, Mean Age, and Dom Spp maps at the succession timesteps.
  #-----------------------------------------------------------------------------------------------------------------------
  #-----------------------------------------------------------------------------------------------------------------------
  #### Interpolate HT and FC maps, create DHSVM maps based on interpolated maps: ----
  if(INTERPOLATE.DHSVM==T){
    cat('\n\n-----------------------------------------------------------------------
            Interpolating from',as.numeric(yrs[2]) - as.numeric(yrs[1]),'to',simOpts$increment,'years.\n-----------------------------------------------------------------------\n')
    
    #------------------------------------------------------#
    ### Create empty raster: ----
    NA.r <- ifel(is.na(pwg.r), 0, NA)

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
      yrs.needed <- seq(as.numeric(prev.yr)+simOpts$increment,as.numeric(yr)-simOpts$increment,simOpts$increment)
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
        t1 <- ifel(is.na(NA.r), layer.prev, NA)
        t2 <- ifel(is.na(NA.r), layer.r, NA)
        
        # t1<-layer.prev[is.na(NA.r)]
        # t2<-layer.r[is.na(NA.r)]
        ## Calculate distance:
        delta <- t2-t1
        ## Interpolate: 
        s<-empty.s
        for(i in 1:length(yrs.needed)){
          # s[[i]][is.na(s[[i]])] <- t1 + i * (delta / succession.timestep)
          s[[i]] <- ifel(is.na(s[[i]]), t1 + i * (delta / succession.timestep))
        }
        ## Join with t1 and t2 maps:
        layer.interp<-c(layer.prev,s,layer.r)
        ## Round layers:
        layer.interp<-round(layer.interp,2)
        ## Rename:
        names(layer.interp)<-paste0(gsub("[0-9]","",names(layer.prev)),
                                    seq(as.numeric(gsub(".*?([0-9]+).*", "\\1", names(layer.prev))),
                                        as.numeric(gsub(".*?([0-9]+).*", "\\1", names(layer.r))),
                                        simOpts$increment))
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
    for(yr in seq(0,max(as.numeric(unique(yrs))),simOpts$increment)){
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
        plot(dem.r,col=demCols(50),add=T,alpha=0.15,legend=F)
        plot(hillshade.r,col=colorRampPalette(c('black','grey20','white'))(100),add=T,alpha=0.15,legend=F);box(lwd=3)
        mtext(paste0('Total biomass (',2020+as.numeric(yr),')'),font=2,line=-1,adj=0.99,cex=1)
        
        plot.new()
        plot.window(xlim=ext(pwg.r)[1:2], ylim=ext(pwg.r)[3:4],xaxs="i",yaxs="i",asp=1)
        plot(pwg.r,col='black',legend=F,add=T)
        plot(mean.age.top3.dom.r,col=rev(hcl.colors(7,palette = 'Viridis')),breaks=c(1,20,40,80,120,160,300),legend=F,xaxt='n',yaxt='n',add=T)
        plot(dem.r,col=demCols(50),add=T,alpha=0.15,legend=F)
        plot(hillshade.r,col=colorRampPalette(c('black','grey20','white'))(100),add=T,alpha=0.15,legend=F);box(lwd=3)
        mtext(paste0('Age (',2020+as.numeric(yr),')'),font=2,line=-1,adj=0.99,cex=1)
        
        plot.new()
        plot.window(xlim=ext(pwg.r)[1:2], ylim=ext(pwg.r)[3:4],xaxs="i",yaxs="i",asp=1)
        plot(pwg.r,col='black',legend=F,add=T)
        plot(lai.r,col=rev(hcl.colors(6,palette = 'Viridis')),breaks=c(1,2,4,6,8,10),legend=F,xaxt='n',yaxt='n',add=T)
        plot(dem.r,col=demCols(50),add=T,alpha=0.15,legend=F)
        plot(hillshade.r,col=colorRampPalette(c('black','grey20','white'))(100),add=T,alpha=0.15,legend=F);box(lwd=3)
        mtext(paste0('LAI (',2020+as.numeric(yr),')'),font=2,line=-1,adj=0.99,cex=1)
        
        plot.new()
        plot.window(xlim=ext(pwg.r)[1:2], ylim=ext(pwg.r)[3:4],xaxs="i",yaxs="i",asp=1)
        plot(pwg.r,col='black',legend=F,add=T)
        plot(ht.r,col=rev(hcl.colors(length(ht.df$to),palette = 'ag_GrnYl')),breaks=c(0,ht.df$to),legend=F,xaxt='n',yaxt='n',add=T)
        plot(dem.r,col=demCols(50),add=T,alpha=0.15,legend=F)
        plot(hillshade.r,col=colorRampPalette(c('black','grey20','white'))(100),add=T,alpha=0.15,legend=F);box(lwd=3)
        mtext(paste0('Height (',2020+as.numeric(yr),')'),font=2,line=-1,adj=0.99,cex=1)
        
        plot.new()
        plot.window(xlim=ext(pwg.r)[1:2], ylim=ext(pwg.r)[3:4],xaxs="i",yaxs="i",asp=1)
        plot(pwg.r,col='black',legend=F,add=T)
        plot(fc.r,col=rev(hcl.colors(length(fc.df$to),palette = 'ag_GrnYl')),breaks=c(0,fc.df$to),legend=F,xaxt='n',yaxt='n',add=T)
        plot(dem.r,col=demCols(50),add=T,alpha=0.15,legend=F)
        plot(hillshade.r,col=colorRampPalette(c('black','grey20','white'))(100),add=T,alpha=0.15,legend=F);box(lwd=3)
        mtext(paste0('Fractional cover (',2020+as.numeric(yr),')'),font=2,line=-1,adj=0.99,cex=1)
        
        plot.new()
        plot.window(xlim=ext(pwg.r)[1:2], ylim=ext(pwg.r)[3:4],xaxs="i",yaxs="i",asp=1)
        plot(pwg.r,col='black',legend=F,add=T)
        # plot(dhsvm.r,col=rev(hcl.colors(173,palette = 'Zissou 1')),legend=F,xaxt='n',yaxt='n',add=T)
        plot(dhsvm.r,col=dhsvmCols,breaks=seq(1,195,4),legend=F,xaxt='n',yaxt='n',add=T)
        plot(dem.r,col=demCols(50),add=T,alpha=0.15,legend=F)
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
  for(yr in seq(0,max(as.numeric(unique(yrs))),simOpts$increment)){
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
  
  if(simOpts$OVERWRITE.ZIP.FILES==T |
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
}
#----------------------------------------------------------------------------------------------------------------------
#######################################################################################################################
#----------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------
#######################################################################################################################
#----------------------------------------------------------------------------------------------------------------------
### TRIM OUTPUT RASTERS TO HUC EXTENT: ----
##  If TRIM.TO.HUC contains HUCs to trim to, loop through rasters and trim to focal HUCs.
# if(TRIM.TO.HUC[1]!=F){
#   for(landisOutputDir in RunsToProcess){
#     if(!dir.exists(file.path(landisOutputDir,'SubExtents',TRIM.TO.HUC[1],'DHSVM'))) dir.create(file.path(landisOutputDir,'SubExtents',TRIM.TO.HUC[1],'DHSVM'))
#     cat('\nTRIMMING RESULTS TO SMALLER OUTPUT EXTENT...\n\n')
#     focalHUC.nchar<-max(nchar(TRIM.TO.HUC))
#     cat(' ---> Trimming to HUC',focalHUC.nchar,'level. Focal HUCs:',TRIM.TO.HUC)
#     
#     focalHUC.sf<-HUC12.sf[substr(HUC12.sf$HUC12,1,focalHUC.nchar)%in%TRIM.TO.HUC,]
#     plot(HUC12.sf$geometry)
#     plot(focalHUC.sf$geometry,add=T,col='red')
#     
#     focalHUC.r <- mask(pwg.r,focalHUC.sf,updatevalue=NA)
#     # plot(pwg.r,col='black',yaxt='n',xaxt='n',legend=F)
#     # plot(pwg.noBuffer.r,col=demCols,add=T,legend=F)
#     # plot(focalHUC.r,col=hcl.colors(20),add=T)
#     plot(focalHUC.r)
#     
#     ## This loop assumes that the rasters are in your R environment.
#     # for(r.name in c( 'smoke.r',  'dhsvm.r','ht.r','fc.r','lai.r','total.biomass.r')){
#     #   r<-eval(parse(text=r.name))
#     #   r[is.na(focalHUC.r)]<-NA
#     #   # r[is.na(r)]<-0
#     #   
#     #   ## Trim raster? r<-trim(r)
#     #   assign(paste0('focalHUC.',r.name),r)
#     # }
#     # plot(focalHUC.dhsvm.r,col=hcl.colors(173))
#     
#     # ## Write rasters:
#     # writeRaster(focalHUC.dhsvm.r,file.path(landisOutputDir,'SubExtents',TRIM.TO.HUC[1],paste0('smoke_yr-',yr,'.tif')),'GTiff',overwrite=T)
#     # 
#     # writeRaster(focalHUC.dhsvm.r,file.path(landisOutputDir,'SubExtents',TRIM.TO.HUC[1],paste0('DHSVM_yr-',yr,'.tif')),'GTiff',overwrite=T)
#     # writeRaster(focalHUC.ht.r,file.path(landisOutputDir,'SubExtents',TRIM.TO.HUC[1],paste0('Veg_Height-m_yr-',yr,'.tif')),'GTiff',overwrite=T)
#     # writeRaster(focalHUC.fc.r,file.path(landisOutputDir,'SubExtents',TRIM.TO.HUC[1],paste0('Veg_FracCov_yr-',yr,'.tif')),'GTiff',overwrite=T)
#     # writeRaster(focalHUC.lai.r,file.path(landisOutputDir,'SubExtents',TRIM.TO.HUC[1],paste0('Veg_LAI_yr-',yr,'.tif')),'GTiff',overwrite=T)
#     # 
#     # rm(  smoke.r,  focalHUC.dhsvm.r,focalHUC.ht.r,focalHUC.fc.r,focalHUC.lai.r)
#     
#     
#     ## Instead, read in rasters from DHSVM folder:
#     r.to.trim<-dir(file.path(landisOutputDir,'DHSVM'))
#     for(r.name in r.to.trim){
#       r<-rast(file.path(landisOutputDir,'DHSVM',r.name))
#       r[is.na(focalHUC.r)]<-NA
#       # r[is.na(r)]<-0
#       
#       ## Trim raster? r<-trim(r)
#       writeRaster(r,file.path(landisOutputDir,'SubExtents',TRIM.TO.HUC[1],'DHSVM',r.name),'GTiff',overwrite=T)
#     }
#     # Check:
#     r<-rast(file.path(landisOutputDir,'SubExtents',TRIM.TO.HUC[1],"DHSVM_yr-30.tif"))
#     plot(focalHUC.dhsvm.r,col=hcl.colors(173))
#     
#   }
# }