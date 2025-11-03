#----------------------------------------------------------------------------------------------------------------------
#######################################################################################################################
#----------------------------------------------------------------------------------------------------------------------
### Run DST Maps! ----
##   Create 1 table per scenario containing all DST metrics.

for(landisOutputDir in RunsToProcess){
  if(file.exists(file.path(landisOutputDir,'DST','DST_Metrics_by_HUC12.csv')) & RERUN.DST.MAPS == F){
    cat('\nDST outputs already exist for',gsub(dirToProcess,"",landisOutputDir),'. Skipping to next sim...')
    next
  }
  cat('\n\n***************************************************************************************************
  Generating DST output maps for',gsub(dirToProcess,"",landisOutputDir),'             
  ***************************************************************************************************\n')
  #-----------------------------------------------------------------------------------------------------------------------
  #-----------------------------------------------------------------------------------------------------------------------
  #####  ---------------    Set initial variables    ------------- #####
  ### Create empty rasters: ----
  NA.r<-pwg.r
  NA.r[is.na(NA.r)]<-0
  NA.r[NA.r!=0]<-NA
  
  zero.r<-pwg.r
  zero.r[!is.na(zero.r)]<-0
  # plot(NA.r,xaxt='n',yaxt='n')
  
  ### Define file suffix (different for single runs vs. averaged scenarios: ----
  if(grepl('Average',landisOutputDir)){
    suffix <- "_MEAN.tif"
  } else suffix <- ".tif"
  
  ## Initiate blank map of active cells:
  active.r<-pwg.r
  active.r[active.r<12]<-NA # inactive = NA
  active.r[active.r>=12]<-0 # active = 0
  
  ### Define raster file paths: ----
  biomassOutput <- file.path(landisOutputDir, 'biomassOutput')
  ageOutput <- file.path(landisOutputDir, 'ageOutput')
  fireOutput <- file.path(landisOutputDir, 'scrapple-fire')
  harvestOutput <- file.path(landisOutputDir, 'Harvest')
  necnOutput <- file.path(landisOutputDir, 'NECN')
  
  ### Load list of output map names: ----
  biomassMaps<-dir(biomassOutput)[grepl('.tif',dir(biomassOutput))]
  ageMaps<-dir(ageOutput)[grepl('.tif',dir(ageOutput))]
  laiMaps<-dir(necnOutput)[grepl('LAI',dir(necnOutput))&grepl('.tif',dir(necnOutput))]
  fireMaps<-dir(fireOutput)[grepl('.tif',dir(fireOutput))]
  harvestMaps<-dir(harvestOutput)[grepl('.tif',dir(harvestOutput))]
  necnMaps<-dir(necnOutput)[grepl('.tif',dir(necnOutput))]
  
  ### List all species and years: ----
  unlisted<-unlist(strsplit(biomassMaps,"-"))
  spp<-unique(unlisted[seq(1,length(unlisted),3)])
  yrs<-unique(unlisted[seq(2,length(unlisted),3)])
  yrs<-yrs[order(as.numeric(yrs))]
  names(yrs) <- as.numeric(yrs) + 2020
  
  ### Create Output folder: ----
  if(!dir.exists(file.path(landisOutputDir, 'DST'))) {
    dir.create(file.path(landisOutputDir, 'DST'))
  } 
  
  ### Create Output Table: ----
  dst.pwg.df<-expand.grid('Year'=1:max(as.numeric(yrs)),'PWG'=names(ecos)) %>%
    arrange(Year, PWG)
  
  dst.huc12.df<-expand.grid('Year'=1:max(as.numeric(yrs)),'HUC12'=unique(HUC12.r[!is.na(HUC12.r)])) %>%
    arrange(Year, HUC12)
  
  ### If interpolated maps are not present set RERUN.DST.INTERPOLATION to TRUE: ----
  if(FALSE %in% c(paste0('totalC-',1:max(as.numeric(yrs)),'.tif') %in% dir(file.path(landisOutputDir,'DST','totalC'))))
    RERUN.DST.INTERPOLATION <- T 
  
  #-----------------------------------------------------------------------------------------------------------------------
  ### Percent Slope: ----
  slope.percent.r<-tan(terrain(dem.r,'slope',unit='radians'))*100
  
  #-----------------------------------------------------#
  ### Load Distance to Roads raster: ----
  if(file.exists(file.path(dataDir,'PWG',paste0("road_dist_",LANDIS.EXTENT,".tif"))) & file.exists(file.path(dataDir,'PWG',paste0("road_noWild_dist_",LANDIS.EXTENT,".tif")))){
    roads.r<-rast(file.path(dataDir,'PWG',paste0("roads_",LANDIS.EXTENT,"_45m.tif")))
    road.dist.r<-rast(file.path(dataDir,'PWG',paste0("road_dist_",LANDIS.EXTENT,".tif")))
    roads.no.wild.r<-rast(file.path(dataDir,'PWG',paste0("roads_noWild_",LANDIS.EXTENT,"_45m.tif")))
    road.no.wild.dist.r<-rast(file.path(dataDir,'PWG',paste0("road_noWild_dist_",LANDIS.EXTENT,".tif")))
  } else {stop('MISSING ROAD MAPS: Rerun in 0_Study_area_masks_and_raw_layer_prep.R')  }
  par(mfrow=c(1,2))
  plot(road.dist.r,col=rev(hcl.colors(20)))
  plot(road.no.wild.dist.r,col=rev(hcl.colors(20)))
  
  #-----------------------------------------------------#
  ### Define biomass harvest costs and value: ----
  
  ## Sources:
  # - https://www.fpl.fs.fed.us/documnts/fplrp/fpl_rp701.pdf
  # - http://www.globalwood.org/tech/tech_wood_weights.htm
  # - https://extension.psu.edu/calculating-the-green-weight-of-wood-species
  
  ## Units: 
  # 1000 board feet = 17.5 short tons (air dried softwood logs), 20.0 short tons (air dried hardwood logs), ~2000 lbs kiln dried lumber (softwood).
  # 1 cord = 2.65 "green tons"
  # 1000 board feet of doug fir = 3200 lbs green, 2850 lbs air dry, 8700 lbs in 12" logs. http://www.globalwood.org/tech/tech_wood_weights.htm
  # 1000 board feet of ponderosa = 3750 lbs green, 2350 lbs air dry, 11,300 lbs in 12" logs.
  # 1000 board feet = 83.33 cubic feet = 2.36 m^3
  # 1 Megagram = 2204.62 lbs
  # 2204.62 lbs / (3500 lbs per 1k bd.ft.) = 630 board feet
  # 600 bd ft ~ 1800 lbs ~ 0.8 Mg.
  ### 1 Megagram ~ 588 bd ft. (ponderosa) to 689 bd ft. (doug fir) ### (total, including slash and non-merch)
  ### Assuming 12" logs, ratio of merch weight/log weight weight is 3200/8700 (37%) for doug fir and 3750/11300 (33%) for ponderosa.
  ### So 1 Mg is 2204.6 lbs, that produces ~800 lbs merch and 1400 lbs non-merch, which is ~ 230 bd ft. merch and 0.7 tons chip.
  
  #-----------------------------------------------------#
  ##  Revenue: ---
  # Forest service stumpage prices: See table 21 in https://www.fpl.fs.fed.us/documnts/fplrp/fpl_rp701.pdf
  # - East side Doug-fir stumpage: $125 per 1000 bd ft.
  # - Ponderosa stumpage: $77 per 1000 bd ft.
  # - Alder stumpage: $98 per 1000 bd ft.
  
  # DNR mill delivered prices: https://www.dnr.wa.gov/publications/psl_ts_may22_logprices.pdf
  # These prices are higher because mill delivered prices include "tumpage prices + cutting + skidding + loading + hauling + other fixed costs
  # - Doug-fir: $420/1000 bd ft.
  # - Ponderosa: $300/1000 bd ft.
  
  #### Rough estimates of $ per 1k bd ft Merch ###
  # $110 / 1000 board feet stumpage
  # $450 / 1000 board feet mill delivered
  # >$4000-14000 per 1000 board feet milled lumber value
  Revenue.dollars.per.1k.BdFt.Merch <- 4000
  # Convert to $/Mg merch using 650 bd ft/Mg.  
  Revenue.dollars.per.Mg.Merch <- Revenue.dollars.per.1k.BdFt.Merch * 650/1000
  
  #### $ per Mg Chip ###
  # Convert to $/Mg chip using 2204.6 lbs/Mg.
  Revenue.dollars.per.1ton.Chip <- 125 # mill delivered price of total fiber (residual and whole log chip): https://www.forest2market.com/blog/northwest-wood-chip-prices-level-off
  Revenue.dollars.per.Mg.Chip <- Revenue.dollars.per.1ton.Chip  * (2204.6/2000)
  
  ## Value of 1 Mg:
  Revenue.dollars.per.Mg.Merch
  Revenue.dollars.per.Mg.Chip
  
  ##  Costs: ---
  # Note: these values are all relative, not based on actual dollars.
  New.Roads.dollars.per.mile<-10000
  
  Harvest.dollars.per.Ha<-100
  # Harvest.dollars.per.Stand<-1000
  Harvest.dollars.per.Mg.per.meter<-1
  
  Access.dollars.per.Km<-100
  
  Harvest.dollars.per.Mg.Merch<-50
  Harvest.dollars.per.Mg.Chip<-50
  
  cable.yarding.multiplier<-0.5 # Cable yarding or tethered yarding system? Check with John Bailey at OSU
  
  RxFire.ActiveForest.cost.USD.per.Ha <- 300 * 0.404686 # $300/ac to ha
  RxFire.Wildlands.cost.USD.per.Ha <- 150 * 0.404686 # $150/ac to ha
  
  #-------------------------------------------------------#
  
  ### Define Proportion Merchentable timber by prescription ID: ----
  ##  Note: This is proportion of removed cohorts that have any merchantable value. Merchentable % is further reduced
  ##   using the 35% multiplier, meaning for every 1 Mg merchentable biomass, only 35% gets turned into actual lumber.
  ##   So if 100 Mg is removed and this table says it is 60% merchentable, we will assume 60 Mg merchentable biomass, 
  ##   of which only 35% (21 Mg) gets turned into actual board feet. The rest becomes chip. So Chip weight = 40 Mg + 0.65*60 = 79 Mg.
  ##   35% is based on a 12" tree producing 120 board feet, which will weigh about 420 lbs (green). A 12" pine tree weighs 1500-2000 lbs. So 35% is really optimistic. 
  prescriptId.df<-data.frame('Prescription'=c('Valley','Mesic','Xeric','Industrial','Pre-commercial Thin', 'WA_DNR'),
                             'RasterID'=c(1,2,3,4,5,7,8,9,10,11,12,13,14,15),
                             'ProportionMerchCohorts'=c(0.8,0.8,0.6,0.9,0.0, 0.9),
                             'PercentMerch'=0.35)
  #-----------------------------------------------------------------------------------------------------------------------
  #### Load LANDIS output .CSV files and fire summary rasters: ----
  ## SCRAPPLE EVENTS LOG:
  if(file.exists(file.path(landisOutputDir,'scrapple-events-log.csv'))){ 
    fire.df<-read.csv(file.path(landisOutputDir,'scrapple-events-log.csv')) %>%
      rename(Year = SimulationYear) %>%
      mutate(IgnitionType = gsub(" ","",IgnitionType)) %>%  # Fix fire.df ignition type column
      mutate(IgnitionType = ifelse(IgnitionType=="Rx", "Prescribed"))
  } else {
      # If scenario has no fire at all, remove fire.df
      warning('No fire activity for ',gsub(dirToProcess,'',landisOutputDir))
      if(exists('fire.df')) rm(fire.df)
  }
  ## HARVEST EVENTS LOG:
  if(file.exists(file.path(landisOutputDir,'biomass-harvest-summary-log.csv'))){ 
    harvestSum.df<-read.csv(file.path(landisOutputDir,'biomass-harvest-summary-log.csv')) %>%
      rename(Time = Year)
    harvestEvents.df<-read.csv(file.path(landisOutputDir,'biomass-harvest-event-log.csv')) %>%
      rename(Time = Year)
  }
  
  # # Fix column names, remove RunOut year 0, and re-label RunOut years:
  # if(exists('fire.df')){
  #   colnames(fire.df)[colnames(fire.df)=='SimulationYear']<-'Year'
  #   if(!'Era'%in%colnames(fire.df)) fire.df$Era<-'Mgmt'
  #   fire.df<-fire.df[!(fire.df$Year==0 & fire.df$Era=='RunOut'),]
  #   fire.df[fire.df$Era=='RunOut','Year'] <-  fire.df[fire.df$Era=='RunOut','Year'] + RunOut.start.year
  #   ### 
  #   fire.df$IgnitionType<-gsub(" ","",fire.df$IgnitionType)
  #   fire.df[fire.df$IgnitionType=='Rx','IgnitionType']<-'Prescribed'
  #   if(nrow(fire.df)==0) stop('No rows in fire.df. Must be an error!')
  # }
  # if(exists('harvestEvents.df')){
  #   colnames(harvestSum.df)[colnames(harvestSum.df)=='Time']<-'Year'
  #   colnames(harvestEvents.df)[colnames(harvestEvents.df)=='Time']<-'Year'
  # }
  # 
  
  #-----------------------------------------------------------------------------------------------------------------------
  #### Generate Mean Age and Dominant Species Maps for each Succession timestep: ----
  ###  *** NOTE: This is for some sims when generating DHSVM maps ***
  temp.file<-file.path(ageOutput, paste0('DominantSpecies2-',max(as.numeric(yrs)),'.tif'))
  if(!file.exists(temp.file)){
    cat('-> Calculating Mean Age and Dominant Species by Biomass maps...')
    
    for(yr in unique(yrs)){
      cat(paste0('\n ------------\n Year: ',yr,'\n ------------\n'))
      ## Identify correct directories: ----
      biomassDir<-biomassOutput
      ageDir<-ageOutput
      
      biomassMapFiles<-biomassMaps
      ageMapFiles<-ageMaps
      
      landis.yr <- yr 
      ## Load Biomass: ----
      total.biomass.r<-makeRaster(biomassMapFiles[grepl(paste0('TotalBiomass-',landis.yr,"-biomass",suffix),biomassMapFiles)],biomassDir)
      
      ## Load Age: ----
      cat('   - Loading Species Age and Biomass maps')
      med.age<-list()
      max.age<-list()
      biomass.all<-list()
      for(sp in spp){
        # if(sp %in% c("Nfixer_Resprt","NonFxr_Resprt","NonFxr_Seed","Grass_Forb","TotalBiomass")) next
        # cat(paste0(sp,', '))
        cat('.')
        
        if(file.exists(file.path(ageDir,paste0(sp,'-',landis.yr,'-MED',suffix))))
          med.age[[sp]]<-makeRaster(paste0(sp,'-',landis.yr,'-MED',suffix),ageDir) # Median age per species
        if(file.exists(file.path(ageDir,paste0(sp,'-',landis.yr,'-MAX',suffix))))
          max.age[[sp]]<-makeRaster(paste0(sp,'-',landis.yr,'-MAX',suffix),ageDir) # Max age
        if(file.exists(file.path(biomassDir,paste0(sp,'-',landis.yr,'-biomass',suffix))))
          biomass.all[[sp]]<-makeRaster(paste0(sp,'-',landis.yr,'-biomass',suffix),biomassDir) # Biomass
        
      }
      med.age<-rast(med.age)
      max.age<-rast(max.age)
      biomass.trees<-rast(biomass.all[!names(biomass.all)%in%c("Nfixer_Resprt","NonFxr_Resprt","NonFxr_Seed","Grass_Forb","TotalBiomass")])
      biomass.notTree<-rast(biomass.all[c("Nfixer_Resprt","NonFxr_Resprt","NonFxr_Seed","Grass_Forb")])
      biomass.all<-rast(biomass.all[names(biomass.all)!="TotalBiomass"])
      
      cat('\n   - Identifying top 3 dominant species per site')
      
      ## Dominant species by biomass: ----
      dominant.spp<-which.max(biomass.trees) # How to identify the dominant species per cell???
      dominant.spp.lookup<-1:length(names(biomass.trees))
      names(dominant.spp.lookup)<-names(biomass.trees)
      
      ## Second and third dominant by biomass: ----
      ##  Biomass values, ordered (layer 1 is highest biomass, layer 2 is second highest, etc.)
      dom.spp.ordered<-calc(biomass.trees, fun=function(x,na.rm) x[order(x,decreasing=T)])
      
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
      ## View where AbieAmab is the 1st, 2nd, and 3rd dominant species by biomass
      # par(mfrow=c(4,4),oma=rep(0,4),mar=rep(0,4))
      plot(dominant.spp==1,col=c('white','darkred'),xaxt='n',yaxt='n',legend=F)
      plot(dominant.spp2==1,col=c(NA,'orange'),add=T)
      plot(dominant.spp3==1,col=c(NA,'gold'),add=T)
      
      ## Max age of any species: ----
      max.age.r<-max(max.age,na.rm=T)
      # plot(max.age.r)
      
      ## Max biomass of any species: ----
      max.biomass.r<-max(biomass.trees,na.rm=T)
      # plot(total.biomass.r)
      # plot(max.biomass.r)
      
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
      # plot(mean.age.domSpp.r)
      
      ## Mean age of dominant 3 species per site: ----
      cat('\n   - Calculating mean age of dominant 3 species per site')
      age.top3.dom.Spp<-list()
      for(sp in names(dominant.spp.lookup)){
        cat('.')
        # cat(paste0(sp,', '))
        sp.num<-dominant.spp.lookup[sp]
        sp.mask<-dominant.spp==sp.num
        sp.mask[sp.mask==0]<-NA
        # plot(sp.mask)
        dom.sp.med.age<-mask(med.age[[sp]],sp.mask)
        # plot(dom.sp.med.age,col='red',main=sp,legend=F)
        dom.sp.med.age[is.na(dom.sp.med.age)]<-0
        # plot(sp.med.age)
        
        sp.mask<-dominant.spp2==sp.num
        sp.mask[sp.mask==0]<-NA
        # plot(sp.mask)
        dom2.sp.med.age<-mask(med.age[[sp]],sp.mask)
        # plot(dom2.sp.med.age,add=T,col='gold',legend=F)
        dom2.sp.med.age[is.na(dom2.sp.med.age)]<-0
        
        sp.mask<-dominant.spp3==sp.num
        sp.mask[sp.mask==0]<-NA
        # plot(sp.mask)
        dom3.sp.med.age<-mask(med.age[[sp]],sp.mask)
        # plot(dom3.sp.med.age,add=T,col='wheat',legend=F)
        dom3.sp.med.age[is.na(dom3.sp.med.age)]<-0
        
        sp.med.age<-dom.sp.med.age+dom2.sp.med.age+dom3.sp.med.age
        
        sp.med.age[sp.med.age==0]<-NA
        age.top3.dom.Spp[[sp]]<-sp.med.age
      }
      age.top3.dom.Spp<-rast(age.top3.dom.Spp)
      # It works! See:
      med.age[604,400]
      age.top3.dom.Spp[604,400]
      mean.age.top3.dom.r<-rast(age.top3.dom.Spp,na.rm=T)
      # plot(mean.age.top3.dom.r)
      
      # Compare to the overall mean median age (this method is higher if there are younger cohorts present that weight down the med age, lower if the dominant cohort is very old):
      mean.age.r<-mean(med.age,na.rm=T)
      # plot(mean.age.r)
      # plot(mean.age.domSpp.r-mean.age.r,col=colorRampPalette(c('red','gold','grey80','blue','green','darkgreen'))(50))
      #---------------------------------------------#      
      ## Round rastsers:
      for(r.name in c('mean.age.r','max.age.r','mean.age.domSpp.r','mean.age.top3.dom.r','dominant.spp','dominant.spp2')){
        r<-eval(parse(text=r.name))
        r<-round(r,0)
        assign(r.name,r)
      }
      
      ## Save Age Rasters:
      writeRaster(mean.age.r,file.path(ageDir, paste0('MeanAge_AllSpp_yr-',landis.yr,'.tif')),overwrite=T)
      writeRaster(max.age.r,file.path(ageDir, paste0('MaxAge_AllSpp_yr-',landis.yr,'.tif')),overwrite=T)
      writeRaster(mean.age.domSpp.r,file.path(ageDir, paste0('MeanAge_DominantSpecies_yr-',landis.yr,'.tif')),overwrite=T)
      writeRaster(mean.age.top3.dom.r,file.path(ageDir, paste0('MeanAge_Top3Species_yr-',landis.yr,'.tif')),overwrite=T)
      writeRaster(dominant.spp,file.path(ageDir, paste0('DominantSpecies-',landis.yr,'.tif')),overwrite=T)
      writeRaster(dominant.spp2,file.path(ageDir, paste0('DominantSpecies2-',landis.yr,'.tif')),overwrite=T)
      
    }
    ageMaps<-dir(ageOutput)[grepl('.tif',dir(ageOutput))]
  }
  #-----------------------------------------------------------------------------------------------------------------------
  #### Interpolate Biomass, NECN, and AGE maps (final DST maps produced at 1-yr increments): ----
  if(RERUN.DST.INTERPOLATION==T){
    increment = 1
    cat('\n-> Interpolating Biomass and NECN maps from',as.numeric(yrs[2]) - as.numeric(yrs[1]),'to',increment,'years...')
    
    #------------------------------------------------------#
    ### Loop through years and interpolate to 1-yr increments: ----
    cat('\n------------')
    for(yr in unique(yrs)){
      cat(paste0('\nYear: ',yr,'\n'))
      
      ## Define directory depending on active year:
      biomassDir<-biomassOutput
      ageDir<-ageOutput
      necnDir<-necnOutput
      fireDir<-fireOutput
      harvestDir<-harvestOutput
      
      biomassMapFiles<-biomassMaps
      ageMapFiles<-ageMaps
      necnMapFiles<-necnMaps
      # fireMapFiles<-fireMaps
      # harvestMapFiles<-harvestMaps
      
      landis.yr <- yr 

      #------------------------------------------------------#
      # cat('-> Loading Total Biomass, Age, Soil C, ANPP, NEE, TotalC, DeadWoodBiomass, Mean Age, and Dominant Species maps...\n')
      
      ## For biomass and age maps, outputs are generated for year 0.
      total.biomass.r<-makeRaster(biomassMapFiles[grepl(paste0('TotalBiomass-',landis.yr,"-biomass",suffix),biomassMapFiles)],biomassDir)
      
      mean.age.r<-makeRaster(ageMapFiles[grepl(paste0('MeanAge_AllSpp_yr-',landis.yr,suffix),ageMapFiles)],ageDir)
      max.age.r<-makeRaster(ageMapFiles[grepl(paste0('MaxAge_AllSpp_yr-',landis.yr,suffix),ageMapFiles)],ageDir)
      mean.age.domSpp.r<-makeRaster(ageMapFiles[grepl(paste0('MeanAge_DominantSpecies_yr-',landis.yr,suffix),ageMapFiles)],ageDir)
      mean.age.top3.dom.r<-makeRaster(ageMapFiles[grepl(paste0('MeanAge_Top3Species_yr-',landis.yr,suffix),ageMapFiles)],ageDir)
      dominant.spp.r<-makeRaster(ageMapFiles[grepl(paste0('DominantSpecies-',landis.yr,suffix),ageMapFiles)],ageDir)
      dominant.spp2.r<-makeRaster(ageMapFiles[grepl(paste0('DominantSpecies2-',landis.yr,suffix),ageMapFiles)],ageDir)
      
      ##   For Year 0, you must generate a YR-0 NECN maps by running LANDIS with 1-yr succession time steps and no fire and no harvest. Save this folder to the data directory. 
      if(yr==0){
        soilC.r<-makeRaster('SOMTC-1.tif',file.path(dataDir,'NECN_Outputs_Yr_0',LANDIS.EXTENT))
        npp.r<-makeRaster('AG_NPP-1.tif',file.path(dataDir,'NECN_Outputs_Yr_0',LANDIS.EXTENT))
        nee.r<-makeRaster('ANEE-1.tif',file.path(dataDir,'NECN_Outputs_Yr_0',LANDIS.EXTENT))
        totalC.r<-makeRaster('TotalC-1.tif',file.path(dataDir,'NECN_Outputs_Yr_0',LANDIS.EXTENT))
        deadWoodBiomass.r<-makeRaster('DeadWoodBiomass-1.tif',file.path(dataDir,'NECN_Outputs_Yr_0',LANDIS.EXTENT))
      } else {
        soilC.r<-makeRaster(necnMapFiles[grepl(paste0('SOMTC-',landis.yr,suffix),necnMapFiles)],necnDir)
        npp.r<-makeRaster(necnMapFiles[grepl(paste0('AG_NPP-',landis.yr,suffix),necnMapFiles)],necnDir)
        nee.r<-makeRaster(necnMapFiles[grepl(paste0('ANEE-',landis.yr,suffix),necnMapFiles)],necnDir)
        totalC.r<-makeRaster(necnMapFiles[grepl(paste0('TotalC-',landis.yr,suffix),necnMapFiles)],necnDir)
        deadWoodBiomass.r<-makeRaster(necnMapFiles[grepl(paste0('DeadWoodBiomass-',landis.yr,suffix),necnMapFiles)],necnDir)
      }
      #------------------------------------------------------#
      ## If it's the first year, skip to next: ----
      if(yr == yrs[1]) {
        total.biomass.prev<-total.biomass.r
        mean.age.prev<-mean.age.r
        max.age.prev<-max.age.r
        mean.age.domSpp.prev<-mean.age.domSpp.r
        mean.age.top3.dom.prev<-mean.age.top3.dom.r
        dominant.spp.prev<-dominant.spp.r
        dominant.spp2.prev<-dominant.spp2.r
        soilC.prev<-soilC.r
        npp.prev<-npp.r
        nee.prev<-nee.r
        totalC.prev<-totalC.r
        deadWoodBiomass.prev<-deadWoodBiomass.r
        
        prev.yr=yr
        
        cat('------------')
        next
      } 
      #------------------------------------------------------#
      ## Determine number of years to interpolate: ----
      succession.timestep <- as.numeric(yr) - as.numeric(prev.yr)
      yrs.needed <- seq(as.numeric(prev.yr)+increment,as.numeric(yr)-increment,increment)
      yrs.needed
      # cat('-> Interpolating from year',prev.yr,'to',yr,'... ')
      
      ## Create empty raster stack ----
      for(i in yrs.needed){
        if(i==yrs.needed[1]) empty.s<-NA.r else
          empty.s<-c(empty.s,NA.r)
      }
      names(empty.s)<-yrs.needed
      
      #------------------------------------------------------#
      ## Interpolate: ----
      # cat('Layer: ')
      start.time<-Sys.time()
      
      for(layer in c('total.biomass','mean.age','max.age','mean.age.domSpp','mean.age.top3.dom','dominant.spp','dominant.spp2',
                     'soilC','npp','nee','totalC','deadWoodBiomass')){
        # cat(layer,', ',sep='')
        cat('-')
        layer.r<-eval(parse(text=paste0(layer,'.r')))
        layer.prev<-eval(parse(text=paste0(layer,'.prev')))
        
        names(layer.r)<-paste(layer,yr,sep='-')
        names(layer.prev)<-paste(layer,prev.yr,sep='-')
        
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
          s[[i]][NA.r==0]<-NA # Set NA for areas outside of study domain
        }
        
        # For discrete variables, replace interpolated values with previous timestep or next timestep.
        if(layer %in% c('dominant.spp','dominant.spp2')){
          for(i in 1:length(yrs.needed)){
            if(i %in% 1:(length(yrs.needed)/2)){ # First half of the timestep.
              s[[i]][is.na(NA.r)] <- t1 
            } else s[[i]][is.na(NA.r)] <- t2 # Second half of the timestep.
          }
        }
        ## Join with t1 and t2 maps:
        layer.interp<-c(layer.prev,s,layer.r)
        
        ## Round layers:
        layer.interp<-round(layer.interp,1)
        
        ## Rename: # This method causes problems if there is a number in the layer name (e.g., mean.age.top3.dom)
        # names(layer.interp)<-paste0(gsub("[0-9]","",names(layer.prev)),
        #                             seq(as.numeric(gsub(".*?([0-9]+).*", "\\1", names(layer.prev))),
        #                                 as.numeric(gsub(".*?([0-9]+).*", "\\1", names(layer.r))),
        #                                 increment))
        ## Rename: # This is far simpler and more reliable. Don't worry, yr and prev.yr were used to name layer.r and layer.prev anyways.
        names(layer.interp)<-paste(layer,seq(prev.yr,yr,increment),sep='.')
        
        ## Write rasters:
        for(i in 1:nlyr(layer.interp)){
          out.name<-gsub("[.]","-",layer)
          # if(names(layer.interp[[i]]) %in% c(names(layer.prev),names(layer.r))) next
          if(!dir.exists(file.path(landisOutputDir, 'DST',out.name))) dir.create(file.path(landisOutputDir, 'DST',out.name))
          writeRaster(layer.interp[[i]],file.path(landisOutputDir, 'DST',out.name,paste0(gsub("[.]","-",names(layer.interp[[i]])),'.tif')),overwrite=T)
        }
      }
      
      # cat('\n -Time diff:',round(difftime(Sys.time(),start.time,units='secs'),0),'seconds')
      #------------------------------------------------------#
      ## Save current year maps as previous year maps for next loop level: ----
      total.biomass.prev<-total.biomass.r
      mean.age.prev<-mean.age.r
      max.age.prev<-max.age.r
      mean.age.domSpp.prev<-mean.age.domSpp.r
      mean.age.top3.dom.prev<-mean.age.top3.dom.r
      dominant.spp.prev<-dominant.spp.r
      dominant.spp2.prev<-dominant.spp2.r
      soilC.prev<-soilC.r
      npp.prev<-npp.r
      nee.prev<-nee.r
      totalC.prev<-totalC.r
      deadWoodBiomass.prev<-deadWoodBiomass.r
      
      prev.yr<-yr
    } # End year loop for interpolation.
    
  } # Finished interpolating
  #------------------------------------------------------#
  #-----------------------------------------------------------------------------------------------------------------------
  ## Load interpolated maps from disk: ----
  cat('\n\n-> Loading interpolated raster stacks: ')
  for(l in c('total.biomass','mean.age','max.age','mean.age.domSpp','mean.age.top3.dom','dominant.spp','dominant.spp2',
             'soilC','npp','nee','totalC','deadWoodBiomass')){
    cat(l,', ',sep='')
    out.name<-gsub("[.]","-",l)
    r<-rast(file.path(landisOutputDir, 'DST',out.name,paste0(out.name,'-',0:max(as.numeric(yrs)),'.tif')))
    names(r)<-paste0(l,'.',0:max(as.numeric(yrs)))
    assign(l,r)
  }
  #------------------------------------------------------#
  ## Validate interpolated maps: ----
  if(FALSE & RERUN.DST.INTERPOLATION==T){
    
    cat('\n\n-----------------------------------------------------------------------
            Validating interpolation results...\n-----------------------------------------------------------------------\n')
    
    #------------------------------------------------------#
    pixel.values<-data.frame('year'=seq(0,max(as.numeric(yrs)),increment))
    
    dev.new()
    par(mfrow=c(1,2),oma=rep(0,4),mar=rep(0,4))
    r<-total.biomass[[paste0('total.biomass.',0)]]
    plot.new();plot.window(xlim=ext(r)[1:2], ylim=ext(r)[3:4],xaxs="i",yaxs="i",asp=1)
    plot(pwg.r,col='black',add=T,legend=F)
    mtext('Total biomass',font=2,line=-1.75,adj=0.99,cex=1)
    
    pixels<-sample(1:ncell(r),100)
    pixels<-pixels[!is.na(r[pixels])]
    
    for(i in seq(0,max(as.numeric(yrs)),increment)){
      cat(i,', ',sep='')
      r<-total.biomass[[paste0('total.biomass.',i)]]
      r[is.na(pwg.r)]<-NA
      
      r[1:2,1:2]<-c(1,1,0,0) # For consistent colors
      
      pixel.values[i+1,2:(length(pixels)+1)]<-r[pixels]
      
      plot(r,col=c('#7f7f7f',hcl.colors(10)),legend=F,add=T)
      mtext(2020+i-1,font=1,line=-1,adj=0.99,cex=1,col = 'white')
      mtext(2020+i,font=2,line=-1,adj=0.99,cex=1,col = 'black')
    }
    # par(mfrow=c(1,1),oma=c(2,2,0,0),mar=rep(1,4))
    plot(pixel.values$year,pixel.values[,2],type='l',col='blue',xlab='Year',ylab='Total Biomass',ylim=c(0,80000))
    for(i in 2:ncol(pixel.values)){
      lines(pixel.values$year,pixel.values[,i],col=i)
    }
  }
  #------------------------------------------------------#
  ## Set years to annual increments: ----
  succession.timesteps<-as.numeric(yrs)
  
  yrs.mgmtEra<-1:max(as.numeric(yrs.mgmtEra))
  
  yrs<-1:max(as.numeric(yrs))
  names(yrs) <- yrs + 2020
  
  #-----------------------------------------------------------------------------------------------------------------------
  ########################################################################################################################
  #-----------------------------------------------------------------------------------------------------------------------
  #####  ---------------    DST INPUT MAPS and SUMMARY TABLES    ----------------------------------------------------- ----
  #-----------------------------------------------------------------------------------------------------------------------
  #-----------------------------------------------------------------------------------------------------------------------
  #### YEAR LOOP TO GENERATE DST SUMMARY METRICS: ----
  cat('\n\n***********************************************************************************
   LOOPING THROUGH YEARS TO GENERATE DST SUMMARY METRICS...\n----------------------------------------------------------------------------------\n')
  
  for(yr in unique(yrs)){
    cat(paste0('\n------------\nYear: ',yr,'\n------------\n'))
    ### Define map directory and set landis.yr (adjust for RunOut): ----
    biomassDir<-biomassOutput
    ageDir<-ageOutput
    necnDir<-necnOutput
    fireDir<-fireOutput
    harvestDir<-harvestOutput
    
    landis.yr <- yr 
    
    prev.succession.timestep <- succession.timesteps[which.min(abs(succession.timesteps[succession.timesteps<=yr]-yr))]
   
    #-----------------------------------------------------------------------------------------------------------------------
    ### Load Interpolated LANDIS-II Output maps for Focal Year from R Environment: ----
    cat('Loading Interpolated rasters for the focal year...\n')
    rm(list=ls()[grepl('focal.yr.',ls())])
    
    for(l in c('total.biomass','mean.age','max.age','mean.age.domSpp','mean.age.top3.dom','dominant.spp','dominant.spp2',
               'soilC','npp','nee','totalC','deadWoodBiomass')){
      s<-eval(parse(text=l))
      r<-s[[paste0(l,'.',yr)]]
      assign(paste0('focal.yr.',l),r)
    }
    ### Load focal year harvest maps: ----
    if(file.exists(file.path(harvestDir,paste0('biomass-removed-',landis.yr,'.tif')))){
      focal.yr.harvest.r<-makeRaster(paste0('biomass-removed-',landis.yr,'.tif'),harvestDir) # g/m2
      focal.yr.harvestPrescript.r<-makeRaster(paste0('biomass-harvest-prescripts-',landis.yr,'.tif'),harvestDir)
      focal.yr.harvestID.r<-classify(standID.r,rcl=data.frame('is'=harvestEvents.df$Stand,'becomes'=harvestEvents.df$EventID))
      focal.yr.harvestID.r[!focal.yr.harvestID.r%in%harvestEvents.df$EventID]<-NA
      focal.yr.harvestSizeHa.r<-classify(focal.yr.harvestID.r,rcl=data.frame('is'=harvestEvents.df$EventID,'becomes'=harvestEvents.df$HarvestedSites * 0.81))
      focal.yr.harvestSizeHa.r[is.na(focal.yr.harvest.r)]<-NA
      ## Harvest biomass g/m2 maps output by LANDIS. Can't get Mg per species.
      focal.yr.harvestMg.r<-focal.yr.harvest.r * 0.01 * 0.81 # Convert g/m2 to mg/ha to Mg. Same as below method but cant get individual species MgBiomassRemoved.
      ## To get biomass harvested per species, Use MgBiomassRemoved per harvest event from harvest summary table instead. Divide by number of cells in harvest event (harvestSizeHa / 0.81) to get Mg per cell.
      focal.yr.harvestMg.r<-classify(focal.yr.harvestID.r,rcl=data.frame('is'=harvestEvents.df$EventID,'becomes'=harvestEvents.df$MgBiomassRemoved)) / (focal.yr.harvestSizeHa.r / 0.81)
      focal.yr.harvestMg.r[is.na(focal.yr.harvest.r)]<-NA # NA for cells that are in a selected stand that weren't harvested.
      focal.yr.harvestMgHa.r<-focal.yr.harvestMg.r / 0.81 # Divide by cell size to get Mg per Ha per pixel
      
      focal.yr.harvestMerchProp.r<-classify(focal.yr.harvestPrescript.r,rcl=data.frame('from'=prescriptId.df$RasterID,'to'=prescriptId.df$ProportionMerchCohorts * prescriptId.df$PercentMerch))
      focal.yr.harvestMerchProp.r[is.na(focal.yr.harvest.r)]<-NA
      
      # Verify:
      harvestEvents.df[harvestEvents.df$EventID==92,c('NumberOfSites', 'HarvestedSites', 'MgBiomassRemoved', 'MgBioRemovedPerDamagedHa')]
      focal.yr.harvest.r[focal.yr.harvestID.r==92] * 0.01 * 0.81 # Mg removed per cell, but not possible to partition it by Species
      focal.yr.harvestMg.r[focal.yr.harvestID.r==92] # Mg removed per species, averaged across the entire patch that was cut.
      focal.yr.harvestSizeHa.r[focal.yr.harvestID.r==92]
      focal.yr.harvestMgHa.r[focal.yr.harvestID.r==92] # Mg/ha, which is slightly higher because each cell is 0.81 ha.
      sum(focal.yr.harvestMg.r[focal.yr.harvestID.r==92],na.rm=T)
      mean(focal.yr.harvestMgHa.r[focal.yr.harvestID.r==92],na.rm=T) * harvestEvents.df[harvestEvents.df$EventID==92,c('HarvestedSites')] * 0.81
      
      if(exists('biomass.df')){
        ## Numbers from focal.yr.harvestMg.r (and Biomass Annual Dynamics CSV file) are slightly off (<1%) due to rounding. Not a big deal.
        biomass.df.AllSp[biomass.df.AllSp$Year==yr,c('PWG','Harvested_Mg','HarvestedRaster_Mg','Harvested_Ha')]
        colSums(harvestSum.df[harvestSum.df$Year==yr,c('TotalBiomassHarvested','HarvestedSites')])
        
        biomass.df.Sp[biomass.df.Sp$Year==yr & biomass.df.Sp$Species=='PseuMenz',c('PWG','Harvested_Mg','Harvested_MgHa','Harvested_MgHaCut','Harvested_MgHaEco')]
        sum(harvestSum.df[harvestSum.df$Year==yr,]$BiomassHarvestedMg_PseuMenz)
      }
    } else {
      focal.yr.harvest.r <- zero.r
      focal.yr.harvestID.r <- zero.r
      focal.yr.harvestSizeHa.r <- zero.r
      focal.yr.harvestMg.r <- zero.r
    }
    ### Load focal year fire data frame: ----
    if(exists('fire.df')){
      focal.yr.fire.df<-fire.df[fire.df$Year==yr,]
    }
    ### Load focal year fire maps: ----
    if(length(dir(fireDir))>0){
      if(nrow(focal.yr.fire.df)==0) stop('fire.df has no rows for year: ',yr,'. This is probably an error!')
      focal.yr.flamingCons.r<-makeRaster(paste0('flaming-consumptions-',landis.yr,'.tif'),fireDir)
      focal.yr.smolderCons.r<-makeRaster(paste0('smolder-consumption-',landis.yr,'.tif'),fireDir)
      focal.yr.fineFuels.r<-makeRaster(paste0('fine-fuels-',landis.yr,'.tif'),fireDir)
      focal.yr.siteMortality.r<-makeRaster(paste0('fire-dnbr-',landis.yr,'.tif'),fireDir)
      focal.yr.siteMortality.r[focal.yr.siteMortality.r==1]<-0 # Value of 1 indicates no fire. Silly.
      focal.yr.fireID.r<-makeRaster(paste0('event-ID-',landis.yr,'.tif'),fireDir)
      focal.yr.mort.r<-classify(focal.yr.fireID.r,rcl=data.frame('is'=focal.yr.fire.df$EventID,'becomes'=focal.yr.fire.df$TotalBiomassMortality))
      focal.yr.fireSizeHa.r<-classify(focal.yr.fireID.r,rcl=data.frame('is'=focal.yr.fire.df$EventID,'becomes'=focal.yr.fire.df$TotalSitesBurned * 0.81))
      focal.yr.RxFireSizeHa.r<-classify(focal.yr.fireID.r,rcl=data.frame('is'=focal.yr.fire.df$EventID,'becomes'=focal.yr.fire.df$TotalSitesBurned * 0.81))
      focal.yr.RxFireSizeHa.r[!is.na(focal.yr.fireID.r) & focal.yr.fireID.r%in%focal.yr.fire.df[focal.yr.fire.df$IgnitionType!='Prescribed','EventID']]<-0
      focal.yr.mort.Mg.r<-focal.yr.mort.r * 0.01 * 0.81 / (focal.yr.fireSizeHa.r/0.81) # Convert g/m2 to mg/ha to Mg, divide by fire size (in # of cells) to get Mg per pixel. If you divide by fire size in # of Ha, you get mort in Mg/Ha not Mg/cell
      # Classify severity:
      severity.thresholds<-c(100,200,300,400,2001);names(severity.thresholds)<-c('Unburned','Low','Moderate','High','max')
      severity.reclass.df<-data.frame('from'=c(severity.thresholds[1:4]),'to'=c(severity.thresholds[2:5]),'becomes'=c(1,2,3,4))
      focal.yr.sev.r<-classify(focal.yr.siteMortality.r,rcl=severity.reclass.df,include.lowest=T)
    } else {
      focal.yr.flamingCons.r<-zero.r;   focal.yr.smolderCons.r<-zero.r;  focal.yr.fineFuels.r<-zero.r;
      focal.yr.siteMortality.r<-zero.r; focal.yr.fireID.r<-zero.r;       focal.yr.mort.r<-zero.r;
      focal.yr.fireSizeHa.r<-zero.r;    focal.yr.mort.Mg.r<-zero.r;      
      focal.yr.sev.r<-zero.r;           focal.yr.RxFireSizeHa.r<-zero.r
    }
    
    ### View tables and test functions: ----
    
    # head(biomass.df.AllSp)
    # head(biomass.df.Sp)
    # head(severity.df)
    # head(firePatch.df)
    # head(focal.yr.fire.df)
    # 
    # head(harvestSum.df)
    # head(harvestEvents.df)
    # 
    # fireSummary.s
    # fireSummary.s.RunOut
    
    # raster2csv(r=focal.yr.mort.r)
    # plot(csv2raster(biomass.df.AllSp,map='PWG',value='Biomass_mean_MgHaEco',years=c(0,10,20,30)))
    # plot(csv2raster(focal.yr.fire.df,map='EventID',value='TotalBiomassMortality',r=focal.yr.fireID.r,years=NULL))
    
    ### Create empty output vector: ----
    outputs<-c()
    ########################################################################################################################
    ##############  SEVEN PILLARS OF RESILIENCE  ###########################################################################
    #-----------------------------------------------------------------------------------------------------------------------
    #-----------------------------------------------------------------------------------------------------------------------
    ### SUSTAINABLE BIOMASS: ----
    cat('-> Calculating Sustainable biomass...\n')
    #-------------------------------------------------------------------------------------------#
    ### Timber harvest ###
    #-------------------------------------------------------#
    if(max(values(focal.yr.harvest.r), na.rm = T)>0){
      ## Merch biomass:
      merchMg.r<-focal.yr.harvestMg.r * focal.yr.harvestMerchProp.r
      ## Chip biomass: 
      chipMg.r<-focal.yr.harvestMg.r * (1-focal.yr.harvestMerchProp.r)
      
    } else {
      merchMg.r<-zero.r 
      chipMg.r<-zero.r 
    }
    
    #-------------------------------------------------------------------------------------------#
    ### Percent area harvested ###
    #-------------------------------------------------------#
    area.treated.r<-focal.yr.harvestID.r
    area.treated.r[area.treated.r!=0 & !is.na(area.treated.r)]<-0.81
    #-------------------------------------------------------------------------------------------#
    ### Sustainability ###
    #-------------------------------------------------------#
    ## Diversity of forest products: 
    
    
    #-------------------------------------------------------#
    ## Delta biomass since Time 0:
    total.biomass.yr0<-total.biomass[['total.biomass.0']]
    
    delta.biomass.r <- focal.yr.total.biomass - total.biomass.yr0
    #-------------------------------------------------------------------------------------------#
    ### --- Add outputs to list --- ###
    #-------------------------------------------------------#
    new.outputs<-c('merchMg.r','chipMg.r','area.treated.r','delta.biomass.r')
    names(new.outputs)<-c('Harvested_Merch_Mg','Harvested_Chip_Mg','Treated_Ha','Delta_Biomass_Mg')
    outputs<-c(outputs,new.outputs)
    #-------------------------------------------------------------------------------------------#
    
    #-----------------------------------------------------------------------------------------------------------------------
    ### ECONOMICS: ----
    cat('-> Calculating Economics...\n')
    #-------------------------------------------------------------------------------------------#
    ### Timber operations ###
    #-------------------------------------------------------#
    if(max(values(focal.yr.harvest.r), na.rm=T)>0){
      #-------------------------------------------------------#
      ### Revenue ###
      
      ## Merch revenue:
      merchRevenue.r<-merchMg.r * Revenue.dollars.per.Mg.Merch
      
      ## Chip revenue: 
      chipRevenue.r<-chipMg.r * Revenue.dollars.per.Mg.Chip
      
      #-------------------------------------------------------#
      ### Costs ###
      
      harvestCost.r <- 
        (Harvest.dollars.per.Mg.Merch * merchMg.r + Harvest.dollars.per.Mg.Chip * chipMg.r) * # harvesting costs
        (1 + slope.percent.r/100) + # slope multiplier
        (road.no.wild.dist.r * Harvest.dollars.per.Mg.per.meter * focal.yr.harvestMg.r) + # distance to road multiplier per Mg
        (road.no.wild.dist.r/1000 * Access.dollars.per.Km) +  # distance to road multiplier per harvest event
        (Harvest.dollars.per.Ha * focal.yr.harvestSizeHa.r)   # harvest size multiplier
      
      
      
      
    } else {
      merchRevenue.r<-zero.r 
      chipRevenue.r<-zero.r 
      harvestCost.r<-zero.r
    }
    
    #-------------------------------------------------------------------------------------------#
    ## Other MGMT costs: 
    #-------------------------------------------------------#
    if(exists('fire.df')){
      ## Rx fire cost: 
      rxCost.r<-focal.yr.RxFireSizeHa.r 
      rxCost.r[lua.r!=12] <- rxCost.r[lua.r!=12] * RxFire.ActiveForest.cost.USD.per.Ha
      rxCost.r[lua.r==12] <- rxCost.r[lua.r==12] * RxFire.Wildlands.cost.USD.per.Ha
      
      
      #-------------------------------------------------------#
      ## Economic exposure / risk: 
      
      #-------------------------------------------------------#
      ## Fire suppression costs: 
      suppressionEffort.r <- classify(focal.yr.fireID.r,rcl=data.frame(focal.yr.fire.df$EventID,focal.yr.fire.df$MeanSuppressionEffectiveness))
      suppressionEffort.r[!is.na(focal.yr.fireID.r) & is.na(suppressionEffort.r)]<-0
      suppressionCost.r <- suppressionEffort.r * 19
      ## NOTE: 10-year average suppression costs are 550 $/ha, 
      #        according to https://www.nifc.gov/fire-information/statistics/suppression-costs. 
      #        What suppression cost multiplier gives us an average cost of 550 $/ha??
      #        Using the formula: mean(values(suppressionEffort.r) * x  / 0.81) = 550, we can calculate a value of x=19 gives a mean suppression cost of $550/ha.
      #-------------------------------------------------------#
      
    } else {
      rxCost.r<-zero.r 
      suppressionEffort.r<-zero.r 
      suppressionCost.r<-zero.r
    }
    #-------------------------------------------------------------------------------------------#
    ### --- Add outputs to list --- ###
    #-------------------------------------------------------#
    new.outputs<-c('merchRevenue.r','chipRevenue.r','harvestCost.r','rxCost.r','suppressionCost.r')
    names(new.outputs)<-c('Revenue_Merch_USD','Revenue_Chip_USD','Harvest_Cost_USD','Rx_Fire_Cost_USD','Fire_Suppression_Cost_USD')
    outputs<-c(outputs,new.outputs)
    #-------------------------------------------------------------------------------------------#
    
    #-----------------------------------------------------------------------------------------------------------------------
    ### CARBON: ----
    cat('-> Calculating Carbon dynamics...\n')
    
    ## Notes: 
    # TotalC = SOMTC + LeafC + FRootC + WoodC + CRootC + SurfaceDeadWood + SoilDeadWood (source: NECN source code)
    # However, NECN does not output maps for any of these metrics. 
    # TotalC INCLUDES LIVE BIOMASS. We can re-create TotalC using the NECN-succession-log. 
    # We can match the TotalC map value by summing the following fields in the NECN-succession-log:
    # C_LiveLeaf + C_LiveFRoot + C_LiveWood + C_LiveCRoot + C_DeadWood + C_DeadCRoot + SOMTC
    # TotalC does not include C_DeadLeaf_Struc, C_DeadLeaf_Meta, C_DeadFRoot_Struc, C_DeadFRoot_Meta 
    
    # REMOVING 1000 FROM NEE MAPS! See https://usermanual.wiki/Pdf/LANDISII20Net20Ecosystem20CN20Succession20v4220User20Guide.678432021/help'
    
    ## For checking carbon numbers, load ecos.r and mask one ecoregion.
    mask<-ecos.r
    mask[mask==30119]<-1
    mask[mask!=1]<-0
    
    mean(focal.yr.total.biomass[mask],na.rm=T) # NECN-succession-log: AGB = 33351.9
    mean(focal.yr.soilC[mask],na.rm=T) # NECN-succession-log: SOMTC = 6263.9
    mean(focal.yr.totalC[mask],na.rm=T) # NECN-succession-log: sum of C_LiveLeaf, C_LiveFRoot, C_LiveWood, C_LiveCRoot, C_DeadWood, C_DeadCRoot, SOMTC = 30056.7
    
    #-------------------------------------------------------------------------------------------#
    ### Carbon pools (Mg C) ###
    #-------------------------------------------------------#
    ## Aboveground live biomass: 
    AG.LiveBiomass.Mg.r <- focal.yr.total.biomass * 0.01 * 0.81 # g/m2 to Mg/ha to Mg per cell.
    
    AG.LiveC.MgC.r <- AG.LiveBiomass.Mg.r * 0.47
    
    #-------------------------------------------------------#
    ## Soil carbon: 
    SoilC.Mg.r <- focal.yr.soilC * 0.01 * 0.81 # g/m2 to Mg/ha to Mg C per cell.
    
    #-------------------------------------------------------#
    ## Deadwood carbon: 
    AG.DeadC.MgC.r <- focal.yr.deadWoodBiomass * 0.5 * 0.01 * 0.81 # Biomass to C, then g/m2 to Mg/ha to Mg C per cell.
    
    #-------------------------------------------------------#
    ## Belowground carbon:
    
    #   We need to back calculate this using the available maps (because belowground carbon is not output).
    #   For ecoregion 30119, mean totalC = 30056.7 g m-2. This can be broken down with:
    #   totalC = focal.yr.total.biomass * 0.47 + # 15688. Aboveground live. * Biomass Output maps
    #          focal.yr.soilC + # 6263. * NECN Output maps
    #          focal.yr.deadWoodBiomass * 0.5 + # 2022. * NECN Output maps
    #          focal.yr.belowground.live.biomass * 0.47 + # 590 + 4466.4. Fine root + coarse root. *** Found only in succession log. 
    #          focal.yr.deadRootC # 1050. Dead Coarse roots. *** Found only in NECN-succession-log.
    
    ## So, Belowground Root Carbon (live + dead) can be calculated as totalC - AGB - SoilC - SurfaceDeadWood.
    BG.LiveDeadC.MgC.r <- (focal.yr.totalC - 
                             (focal.yr.total.biomass * 0.47 + 
                                focal.yr.soilC + 
                                focal.yr.deadWoodBiomass * 0.5)) * 0.01 * 0.81
    
    ## On average, belowground dead is about 25% and belowground live is about 75% of total belowground carbon.
    #  I calculated average percent using C_LiveFRoot, C_LiveCRoot, and C_DeadCRoot columns in NECN-succesion-log. Mean = 25%, median = 21%. 
    # e.g., for ecoregion 30119, deadRootC = 1050, live fine roots + live coarse roots = 5056, total belowground C = 6091.
    BG.LiveRootC.MgC.r <- BG.LiveDeadC.MgC.r * 0.79
    BG.DeadRootC.MgC.r <- BG.LiveDeadC.MgC.r * 0.21
    
    #-------------------------------------------------------#
    ## Carbon stability (vulnerability based on future sims): 
    
    #-------------------------------------------------------------------------------------------#
    ### Carbon fluxes (Mg C yr-1) ###
    #-------------------------------------------------------#
    ### These have been verified with the NECN-succession-log. 
    ## Net C flux: 
    NEE.MgC.per.yr <- (focal.yr.nee - 1000) * 0.01 * 0.81
    
    ## Net C sequestration: 
    NPP.MgC.per.yr <- focal.yr.npp * 0.01 * 0.81
    
    #-------------------------------------------------------#
    ## Turnover rate ?
    C.turnover.years <- (AG.LiveC.MgC.r + AG.DeadC.MgC.r + SoilC.Mg.r + BG.LiveRootC.MgC.r + BG.DeadRootC.MgC.r) / NPP.MgC.per.yr
    
    #-------------------------------------------------------#
    ## Longevity of lost C?
    
    #-------------------------------------------------------------------------------------------#
    ### --- Add outputs to list --- ###
    #-------------------------------------------------------#
    new.outputs<-c('AG.LiveBiomass.Mg.r','AG.LiveC.MgC.r','AG.DeadC.MgC.r','SoilC.Mg.r','BG.LiveRootC.MgC.r','BG.DeadRootC.MgC.r',
                   'NEE.MgC.per.yr','NPP.MgC.per.yr')
    names(new.outputs)<-gsub('[.]','_',new.outputs)
    names(new.outputs)<-gsub('_r','',names(new.outputs))
    outputs<-c(outputs,new.outputs)
    #-------------------------------------------------------------------------------------------#
    
    
    #-----------------------------------------------------------------------------------------------------------------------
    ### WATER: ----
    cat('-> Calculating Water dynamics...\n')
    #-------------------------------------------------------------------------------------------#
    ### Streamflow ###
    #-------------------------------------------------------#
    ## Peak late-season flow (m):
    # if(grepl('Nason',landisOutputDir)){
    #   dhsvmDir<-'Nason_Creek_DHSVM_Outputs'
    # } else {
    #   dhsvmDir<-'Phase_3_DHSVM_Outputs'
    # }
    # scenarioName = gsub(dirToProcess,"",landisOutputDir)
    # scenarioName = gsub("[_]|LANDIS_Sim_Wen|/|BaseClim|[0-9]","",scenarioName)
    # scenarioName = gsub("NECNSCRAPPLE","WILDFIRE",scenarioName)
    # scenarioName = gsub("HARVEST","_HARVEST",scenarioName)
    # scenarioName = gsub("NECN","NO_FIRE",scenarioName)
    # scenarioName
    # 
    # r<-raster(file.path(landisOutputDir,'../',dhsvmDir,'Rasters',scenarioName,paste0('MaxSwe_',names(yrs)[yr],'.tif')))
    # c('MaxSwe','MaxSweDate','MeltOutDate')
    #-------------------------------------------------------#
    ## Mean annual flow (m): 
    
    #-------------------------------------------------------#
    ## Peak late season flow date:
    
    #-------------------------------------------------------#
    ## Range in peak flow date (across drainages, years, or sims)
    
    #-------------------------------------------------------------------------------------------#
    ### Snowpack ###
    #-------------------------------------------------------#
    ## Peak SWE (cm H20/m2): 
    
    #-------------------------------------------------------#
    ## Peak SWE date: 
    
    #-------------------------------------------------------#
    ## Range in peak SWE:
    
    #-----------------------------------------------------------------------------------------------------------------------
    ### FIRE: ----
    if(exists('fire.df')){
      cat('-> Calculating Fire dynamics...\n')
      #-------------------------------------------------------------------------------------------#
      ### Fire activity ### ---
      #-------------------------------------------------------#
      ## High severity burn area (ha):
      highSevFire.Ha.r<-focal.yr.fireSizeHa.r
      highSevFire.Ha.r[focal.yr.sev.r<4]<-0
      highSevFire.Ha.r[highSevFire.Ha.r>1]<-0.81
      
      ## Low severity burn area (ha):
      lowSevFire.Ha.r<-focal.yr.fireSizeHa.r
      lowSevFire.Ha.r[focal.yr.sev.r>2]<-0
      lowSevFire.Ha.r[lowSevFire.Ha.r>1]<-0.81
      
      ## Rx fire burn area (ha):
      RxFireHa.r<-focal.yr.RxFireSizeHa.r
      RxFireHa.r[RxFireHa.r>0]<-0.81
      
      #-------------------------------------------------------#
      ## Severity classes smoothed: 
      cat('   - Smoothing fire severity patches...\n')
      # Na for single burn sites:
      focal.yr.sev.r.smoothed<-focal.yr.sev.r
      focal.yr.sev.r.smoothed[focal.yr.fireSizeHa.r<2]<-NA
      # Fill in NA pixels within each fire with the median of a 3x3 window if NA
      fill.na <- function(x, i=5) {
        if( is.na(x)[i] ) {
          return(median(x, na.rm=TRUE))
        } else {
          return( round(x[i],0) )
        }
      }
      focal.yr.sev.r.smoothed <- focal(focal.yr.sev.r.smoothed, w = matrix(1,3,3), fun = median, pad = TRUE, na.rm = T )
      #-------------------------------------------------------#
      ## Average severity (dNBR): 
      focal.yr.siteMortality.r
      
      ## Mg killed:
      focal.yr.mort.Mg.r
      
      ## Percent mortality:
      cat('   - Calculating percent mortality for each fire...\n')
      if(nrow(focal.yr.fire.df)>0){
        t<-data.frame('ID'=focal.yr.fire.df$EventID,'Available_biomass_Mg'=NA,'Killed_biomass_Mg'=NA)
        prev.timestep.biomass<-makeRaster(paste0("TotalBiomass-",prev.succession.timestep,'-biomass.tif'),biomassDir)
        for(id in focal.yr.fire.df$EventID){
          # cat(paste0(id,', '))
          t[t$ID==id,'Available_biomass_Mg']<-sum(prev.timestep.biomass[focal.yr.fireID.r==id] * 0.01 * 0.81,na.rm=T)
          t[t$ID==id,'Killed_biomass_Mg']<-sum(focal.yr.mort.Mg.r[focal.yr.fireID.r==id],na.rm=T)
        }
        t$PercentMort<-t$Killed_biomass_Mg / t$Available_biomass_Mg
        t$PercentMort<-round(t$PercentMort,3); t[t$PercentMort>1 & !is.na(t$PercentMort),'PercentMort']<-1
        
        focal.yr.mort.percent.r<-classify(focal.yr.fireID.r,rcl=data.frame(t$ID,t$PercentMort))
        plot(focal.yr.mort.percent.r)
      } else focal.yr.mort.percent.r<-zero.r
      #-------------------------------------------------------#
      ## Area burned in WUI (ha):
      focal.yr.areaBurnedNonWilderness.Ha.r<-focal.yr.fireSizeHa.r
      focal.yr.areaBurnedNonWilderness.Ha.r[lua.r==12]<-NA
      
      #-------------------------------------------------------#
      ## Shape of fire size distribution (maybe use multiple sims?):
      
      #-------------------------------------------------------#
      ## Fire risk??
      
      #-------------------------------------------------------------------------------------------#
      ### Smoke ### ---
      #-------------------------------------------------------#
      ## Things to consider: Fire size, fire severity, biomass available, biomass consumed, burn day, 
      
      ## Emission factors and Modified Combustion Efficiency from Urbanski 2014. 
      #   For Wildfire northwest conifer forest: MCE = 0.88, Pm2.5 EF = 23.2 g kg-1. 
      #   For Rx fire  northwest conifer forest: MCE = 0.91, Pm2.5 EF = 17.6 g kg-1. 
      #   For temperate forest duff: MCE = 0.75, Pm2.5 EF = 50 g kg-1
      #   For stumps and logs, MCE = 0.80, Pm2.5 EF = 33 g kg-1. 
      
      ## Emission factors from Prichard et al. 2020:
      #   Pm2.5 = 19.8 g kg-1, SD = 16.2, min = 1.1, max = 89.95.
      #   Pm10 = 8.7 g kg-1, SD = 3.2, Min = 3.38, max = 12.9
      
      emissions.Kg.r <- (focal.yr.flamingCons.r + focal.yr.smolderCons.r) * 0.01 * 0.81 * 1000 # g/m2 to Mg/ha to Mg to kg
      emissions.pm2.5.Kg.r <- emissions.Kg.r * 20 / 1000
      emissions.pm10.Kg.r <- emissions.Kg.r * 8.7 / 1000
      
      
      #-------------------------------------------------------#
      ## Timing of smoke emissions ---
      
      # Use a logistic function to estimate fire duration based on fire size:
      x=seq(0,100000,1000)
      sizeToDurationFUN=function(x) {
        k=0.00008 # steepness
        x0=40000 # midpoint
        ymax=20 # max duration
        adj=0.5 # adjust so min fire duration is 1
        return(round(ymax/(1 + exp(-k*(x-x0))) + adj,0))
      }
      plot(x, sizeToDurationFUN(x),type='l',xlab='fire size',ylab='duration',ylim=c(0,20));abline(h=1,col='red',lty='dashed')
      
      # Create fire start day raster:
      focal.yr.fireDay.r<-classify(focal.yr.fireID.r,rcl=data.frame('is'=focal.yr.fire.df$EventID,'becomes'=focal.yr.fire.df$InitialDayOfYear))
      # New raster for fires that burn more than 1 cell. Doesn't eliminate that many fires since ignitions are related to FWI.
      focal.yr.firesGt1.r<-focal.yr.fireDay.r;focal.yr.firesGt1.r[focal.yr.fireSizeHa.r<1]<-NA
      
      # Create fire duration raster:
      focal.yr.fireDuration.r<-round(sizeToDurationFUN(focal.yr.fireSizeHa.r),0)
      focal.yr.fireDuration.r[focal.yr.fireDuration.r==0]<-1
      hist(unique(values(focal.yr.fireDuration.r)))
      
      
      ## Create data frame for smoke emissions per fire event and assign emissions evenly across multi-day fires:
      cat('   - Summarizing emissions per day...\n')
      if(nrow(focal.yr.fire.df)>0){
        smoke.df<-data.frame('Day'=focal.yr.fire.df$InitialDayOfYear,'EventID'=focal.yr.fire.df$EventID,
                             'FireSizeHa'=focal.yr.fire.df$TotalSitesBurned*0.81,'Duration'=sizeToDurationFUN(focal.yr.fire.df$TotalSitesBurned*0.81))
        for(i in 1:365){
          smoke.df[,ncol(smoke.df)+1]<-NA
          colnames(smoke.df)[ncol(smoke.df)]<-paste0('Day_',i)
        }
        for(id in smoke.df$EventID){
          # cat(paste0(id,', '))
          
          flameCons<-focal.yr.flamingCons.r[focal.yr.fireID.r==id]
          smolderCons<-focal.yr.smolderCons.r[focal.yr.fireID.r==id]
          flameCons[is.na(flameCons)]<-0 # A few small fires have NA for flaming consumption. Not sure why, I checked the source code and couldn't figure it out. One example is a fire with dNBR = 2000 and it only burned one site. Probably no big deal to just ignore this issue.
          smolderCons[is.na(smolderCons)]<-0 # As far as I can tell, smoldering consumption is never NA. But set to 0 just in case.
          
          fireDuration<-smoke.df[smoke.df$EventID==id,'Duration']
          fireDay<-smoke.df[smoke.df$EventID==id,'Day']
          
          totalConsumption_Kg <- (flameCons + smolderCons) * 0.01 * 0.81 * 1000 # g/m2 to Mg/ha to Mg to kg
          totalConsumptionAveraged <- totalConsumption_Kg/fireDuration
          
          smoke.df[smoke.df$EventID==id,paste0('Day_',1:365)] <- 0
          smoke.df[smoke.df$EventID==id,paste0('Day_',fireDay:(fireDay+fireDuration-1))] <- round(sum(totalConsumptionAveraged),1)
        }
        smoke.df$TotalEmissions.perFire<-rowSums(smoke.df[,paste0('Day_',1:365)],na.rm=T)
        
        # Summarize total emissions per day for all fires actively burning
        emissions.df<-data.frame('Day'=1:365,'TotalEmissions.perDay'=colSums(smoke.df[,paste0('Day_',1:365)],na.rm=T))
        emissions.df$Emissions_Pm25_Kg <- round(emissions.df$TotalEmissions.perDay * 20 / 1000,1) # Emission factor 20 g / kg
        emissions.df$Emissions_Pm10_Kg <- round(emissions.df$TotalEmissions.perDay * 8.7 / 1000,1)
        
        emissions.df
        
        # View result
        par(mfrow=c(1,2),oma=c(0,0,0,0),mar=c(2,2,0,0),mgp=c(1,0.2,0),tck=-0.01,ps=10,cex=1,cex.axis=0.8)
        plot(smoke.df$Day,smoke.df$FireSizeHa,xlab='Julian Day',ylab='Number of ignitions',pch=21,bg=alpha('darkred',0.2),xlim=c(1,365))
        plot(emissions.df$Day,emissions.df$Emissions_Pm25_Kg,pch=21,bg=alpha('blue',0.5),ylab='Emissions (Kg)',xlab='Julian Day')
        points(emissions.df$Day,emissions.df$Emissions_Pm10_Kg,pch=21,bg=alpha('orchid',0.5))
        legend('topleft',legend=c('Pm 2.5','Pm 10'),pt.bg=c(alpha('blue',0.5),alpha('orchid',0.5)),inset=0.02,pch=21)
        
        # Calculate emissions quantile, merge to smoke.df:
        emissions.df[emissions.df$TotalEmissions.perDay==0,'TotalEmissions.perDay']<-NA
        emissions.df$Quantile<-round(percent_rank(emissions.df$TotalEmissions.perDay),3)
        emissions.df[is.na(emissions.df$Quantile),'Quantile']<-0
        emissions.df$Quantile.smoothed<-rollmean(emissions.df$Quantile,14,align = 'left',fill = 0)
        
        # Merge to smoke.df to assign quantile to each fire event ID:
        t<-merge(smoke.df,emissions.df,by='Day',all.y=T)
        t<-t[,c('Day','EventID','Duration','FireSizeHa','TotalEmissions.perFire','TotalEmissions.perDay','Quantile','Quantile.smoothed')]
        t[is.na(t$TotalEmissions.perDay),'TotalEmissions.perDay']<-0
        
        t$Emissions_Pm25_Kg <- round(t$TotalEmissions.perFire * 20 / 1000,1) # Emission factor 20 g / kg
        t$Emissions_Pm10_Kg <- round(t$TotalEmissions.perFire * 8.7 / 1000,1)
        
        emissions.df<-t
        
        lines(emissions.df$Day,emissions.df$Quantile.smoothed*max(emissions.df$Emissions_Pm25_Kg,na.rm=T),col='darkred',lty='dotdash')
        
        emissions.timing.r<-classify(focal.yr.fireID.r,rcl=data.frame('from'=emissions.df$EventID,'to'=emissions.df$Quantile.smoothed))
        
      } else emissions.timing.r <- zero.r
      
      #-------------------------------------------------------#
      ## Distance to high severity patch edge:
      high.sev.r<-focal.yr.sev.r.smoothed
      high.sev.r[high.sev.r!=4 | is.na(high.sev.r)]<-0 # 0 for anything other than high severity
      high.sev.r[high.sev.r==4]<-1 # 1 for high severity
      high.sev.r[pwg.r %in% c(10:11) | is.na(pwg.r)]<-NA # NA for inactive areas
      high.sev.r[focal.yr.sev.r.smoothed==0]<-NA # NA for unburned areas
      plot(high.sev.r,col=hcl.colors(100,'Temps'))
      
      cat('  - Distance to high severity patch edge (smoothed!)... ')
      high.sev.dist.r <- distance(high.sev.r, target=0)

      high.sev.dist.r[pwg.r %in% c(10:11) | is.na(pwg.r)]<-0 # Set non-forest zones = 0
      high.sev.dist.r[focal.yr.sev.r.smoothed==0]<-0 # Set unburned areas = 0
      plot(high.sev.dist.r,col=hcl.colors(100,'Temps'))
    } else {
      focal.yr.siteMortality.r<-zero.r
      highSevFire.Ha.r<-zero.r
      lowSevFire.Ha.r<-zero.r
      RxFireHa.r<-zero.r
      focal.yr.mort.Mg.r<-zero.r
      focal.yr.mort.percent.r<-zero.r
      focal.yr.areaBurnedNonWilderness.Ha.r<-zero.r
      focal.yr.sev.r<-zero.r
      emissions.pm2.5.Kg.r<-zero.r
      emissions.pm10.Kg.r<-zero.r
      emissions.timing.r<-zero.r
      high.sev.dist.r<-zero.r
    }
    
    #-------------------------------------------------------------------------------------------#
    ### --- Add outputs to list --- ### ---
    #-------------------------------------------------------#
    new.outputs<-c('focal.yr.siteMortality.r','highSevFire.Ha.r','lowSevFire.Ha.r','RxFireHa.r','focal.yr.mort.Mg.r','focal.yr.mort.percent.r',
                   'focal.yr.areaBurnedNonWilderness.Ha.r','focal.yr.sev.r',
                   'emissions.pm2.5.Kg.r','emissions.pm10.Kg.r','emissions.timing.r','high.sev.dist.r')
    names(new.outputs)<-c('Fire_dNBR','Fire_High_Sev_Area_Ha','Fire_Low_Sev_Area_Ha','Fire_Rx_Area_Ha','Fire_Mortality_Mg','Fire_Mortality_Percent',
                          'Fire_Area_Burned_nonWilderness_Ha','Fire_Severity_Class',
                          'Smoke_Emissions_Kg_pm25','Smoke_Emissions_Kg_pm10','Smoke_Timing_quantile','Distance_from_high_severity_m')
    outputs<-c(outputs,new.outputs)
    
    #-----------------------------------------------------------------------------------------------------------------------
    ### FOREST HEALTH: ----
    cat('-> Calculating Forest Health...\n')
    #-------------------------------------------------------------------------------------------#
    ## Forest age:
    #-------------------------------------------------------#
    focal.yr.max.age
    focal.yr.mean.age.domSpp
    focal.yr.mean.age.top3.dom
    
    # Forests where top 3 dominant cohorts by biomass are older than 120 years
    age.gt.120.r<-focal.yr.mean.age.top3.dom
    age.gt.120.r[age.gt.120.r<120]<-NA
    age.gt.120.r[age.gt.120.r>=120]<-1
    
    # Forest patches of old forest
    age.gt.120.patches.r<-patches(age.gt.120.r,directions=8)
    rcl.df<-data.frame(freq(age.gt.120.patches.r))
    rcl.df$area <- rcl.df$count * 0.81
    age.gt.120.patch.size.r<-classify(age.gt.120.patches.r,rcl=data.frame('is'=rcl.df$value,'becomes'=rcl.df$area))
    age.gt.120.patch.size.r[is.na(age.gt.120.r)]<-NA
    age.gt.120.patch.size.r[is.na(age.gt.120.patch.size.r) & !is.na(focal.yr.total.biomass)]<-0
    plot(age.gt.120.patch.size.r,col=hcl.colors(100,'Temps'))
    
    age.gt.120.patch.gt.10ha.r<-age.gt.120.patch.size.r
    age.gt.120.patch.gt.10ha.r[age.gt.120.patch.gt.10ha.r < 10]<-NA # Drop patches less than 10-ha
    age.gt.120.patch.gt.10ha.r[!is.na(age.gt.120.patch.gt.10ha.r)]<-1
    
    ## Forest patch size distribution:
    forest.patch.size.df<-rcl.df
    forest.patch.size.df$area_bin<-NA
    forest.patch.size.df[forest.patch.size.df$area<10,'area_bin']<-'<10_ha'
    forest.patch.size.df[forest.patch.size.df$area<30&is.na(forest.patch.size.df$area_bin),'area_bin']<-'10-30_ha'
    forest.patch.size.df[forest.patch.size.df$area<100&is.na(forest.patch.size.df$area_bin),'area_bin']<-'30-100_ha'
    forest.patch.size.df[forest.patch.size.df$area<300&is.na(forest.patch.size.df$area_bin),'area_bin']<-'100-300_ha'
    forest.patch.size.df[forest.patch.size.df$area<1000&is.na(forest.patch.size.df$area_bin),'area_bin']<-'300-1000_ha'
    forest.patch.size.df[forest.patch.size.df$area>=1000&is.na(forest.patch.size.df$area_bin),'area_bin']<-'>1000_ha'
    
    forest.patch.size.df<-setNames(aggregate(forest.patch.size.df$count,by=list(forest.patch.size.df$area_bin),FUN=NROW),c('Size_class','Count'))
    forest.patch.size.df
    
    
    ## Area of mature forest patches per HUC12:    
    huc12.matureForestPatchArea.r<-HUC12.r
    huc12.matureForestPatchArea.r[is.na(age.gt.120.patch.gt.10ha.r)]<-NA
    area.df<-data.frame(freq(huc12.matureForestPatchArea.r))
    area.df<-area.df[!is.na(area.df$value),]
    rcl.df<-data.frame('is'=area.df$value,'becomes'=area.df$count * 0.81)
    huc12.matureForestPatchArea.r<-classify(huc12.matureForestPatchArea.r,rcl=rcl.df)
    
    plot(huc12.matureForestPatchArea.r,col=hcl.colors(100,'Temps'))
    #-------------------------------------------------------#
    ## Mature forest cover % per HUC:
    forest.r<-focal.yr.mean.age.top3.dom
    forest.r[forest.r<50]<-0
    forest.r[!is.na(forest.r) & forest.r>0]<-1
    forest.r[is.na(forest.r) & !is.na(pwg.r) & pwg.r>11]<-0
    
    nonForest.r<-forest.r * -1 + 1
    
    plot(forest.r)
    
    ## Summarize per HUC12:    
    huc12.forest.percent.r<-HUC12.r
    huc12.forest.percent.r[is.na(forest.r)|forest.r==0]<-NA
    forested.area<-data.frame(freq(huc12.forest.percent.r))
    total.area<-data.frame(freq(HUC12.r))
    total.area<-total.area[total.area$value%in%forested.area$value,]
    rcl.df<-data.frame('is'=forested.area$value,'becomes'=forested.area$count/total.area$count)
    rcl.df[is.na(rcl.df$is),'becomes']<-NA
    
    huc12.forest.percent.r<-classify(huc12.forest.percent.r,rcl=rcl.df)
    plot(huc12.forest.percent.r,col=hcl.colors(100,'Temps'))
    
    #-------------------------------------------------------#
    ## Distance to seed source:
    # NA stays NA, 1 becomes distance 0, 0s become distance to nearest 1.
    seed.source.r<-focal.yr.mean.age.top3.dom
    seed.source.r[seed.source.r<20 | is.na(seed.source.r)]<-0 # 0 for stand initiation
    seed.source.r[seed.source.r>=20]<-1 # 1 for seed sources
    seed.source.r[pwg.r %in% c(10:15) | is.na(pwg.r)]<-0 # NA for non-forest zones
    plot(seed.source.r,col=hcl.colors(100,'Temps'))
    
    nonForest.r<-seed.source.r * -1 + 1
    plot(nonForest.r)
    
    start.time<-Sys.time()
    cat('  - Distance to seed source... ')
    seed.source.dist.r<-distance(seed.source.r, target=0)
    cat('Time difference with terra:',round(difftime(Sys.time(),start.time,units='secs'),0),'seconds')
    
    
    seed.source.dist.r[pwg.r %in% c(10:15) | is.na(pwg.r)]<-NA # Reset NA for non-forest zones
    # if(grepl('Nason',landisOutputDir)) seed.source.dist.r[is.na(focal.yr.total.biomass)]<-NA # Set NA for inactive areas if running Nason Creek
    plot(seed.source.dist.r,col=hcl.colors(100,'Temps'))
    
    
    ## Using raster package. Much slower (takes ~10 min):
    # start.time<-Sys.time()
    # cat('  - Distance to seed source... ') # This takes 12 minutes... Maybe find a faster way?
    # seed.source.dist.r<-focal.yr.mean.age.top3.dom
    # seed.source.dist.r[seed.source.dist.r<20]<-NA # NA for potential forest sites in stand initiation phase
    # seed.source.dist.r[seed.source.dist.r>0]<-1 # 1 for seed sources
    # seed.source.dist.r[pwg.r %in% c(10:15) | is.na(pwg.r)]<-0 # 0 for non-forest zones
    # plot(seed.source.dist.r,col=hcl.colors(100,'Temps'))
    # 
    # seed.source.dist.r<-distance(seed.source.dist.r)
    # seed.source.dist.r[pwg.r %in% c(10:15) | is.na(pwg.r)]<-NA # Reset NA for non-forest zones
    # plot(seed.source.dist.r,col=hcl.colors(100,'Temps'))
    # cat('Time difference with raster package:',round(difftime(Sys.time(),start.time,units='secs'),0),'seconds')
    
    #-------------------------------------------------------#
    ## Cover type: 
    focal.yr.dominant.spp
    
    dominant.spp.lookup<-1:length(spp[!spp%in%c("Nfixer_Resprt","NonFxr_Resprt","NonFxr_Seed","Grass_Forb","TotalBiomass")])
    names(dominant.spp.lookup)<-spp[!spp%in%c("Nfixer_Resprt","NonFxr_Resprt","NonFxr_Seed","Grass_Forb","TotalBiomass")]
    
    if(length(dominant.spp.lookup)!=max(values(focal.yr.dominant.spp), na.rm=T)) stop('Values in focal.yr.dominant.spp do not match values in dominant.spp.lookup table!')
    
    coverClass.rcl<-data.frame('is'=dominant.spp.lookup,'becomes'=NA)
    coverClass.rcl[names(dominant.spp.lookup) %in% c('AcerMacr','AlnuRubr','BetuOcci','BetuPapy','CornNutt',
                                                     'FraxLati','PopuBals','PopuTrem','PrunEmar','QuerGarr'),'becomes']<-14
    coverClass.rcl[names(dominant.spp.lookup) %in% c('PinuPond','PinuMont','LariOcci','JuniOcci','JuniScop'),'becomes']<-20
    coverClass.rcl[names(dominant.spp.lookup) %in% c('AbieAmab','AbieGran','AbieProc','ChamNoot','PseuMenz','TaxuBrev','ThujPlic','TsugHete','TsugMert'),'becomes']<-30
    coverClass.rcl[names(dominant.spp.lookup) %in% c('AbieLasi','PiceEnge','PinuCont'),'becomes']<-40
    coverClass.rcl[names(dominant.spp.lookup) %in% c('LariLyal','PinuAlbi'),'becomes']<-50
    coverClass.rcl
    
    # Dominant cover
    cover.type.r<-classify(focal.yr.dominant.spp,rcl=coverClass.rcl)
    # Secondary cover
    cover.type2.r<-classify(focal.yr.dominant.spp2,rcl=coverClass.rcl)
    ### IN MOIST FORESTS, RE-CLASSIFY COVER TYPE BASED ON SECOND DOMINANT SPECIES:
    # We do this because douglas fir dominates by biomass so it makes what is actually dry and cold forest appear to be moist mixed conifer.
    cover.type.r[cover.type.r==30 & cover.type2.r>0 & !is.na(cover.type2.r)]<-cover.type2.r[cover.type.r==30 & cover.type2.r>0 & !is.na(cover.type2.r)]
    # 0 for non-forest sites:
    cover.type.r[is.na(focal.yr.total.biomass) | focal.yr.total.biomass==0 | is.na(cover.type.r)]<-0
    
    # View result:
    plot(cover.type.r,col=hcl.colors(100,'Temps'))
    
    if(length(unique(values(cover.type.r)))>length(unique(values(pwg.r)))) 
      stop('Erronious cover types found. Check the focal.yr.dominant.spp raster. Interpolation may have messed it up.')
    
    #-------------------------------------------------------#
    ## Biomass of mesic sp. on xeric sites and cold species on warm sites:
    ##  Note: This goes in forest health topic, but is calculated here because we need cover type raster.
    cover.type.r[cover.type.r==30 & pwg.r==20]
    
    # delta.biomass.r <- focal.yr.total.biomass - total.biomass.yr0
    delta.1yr.biomass.r <- focal.yr.total.biomass - total.biomass[[paste0('total.biomass.',yr-1)]]
    
    cover.type.r[cover.type.r==30 & pwg.r==20]
    delta.1yr.biomass.r[cover.type.r==30 & pwg.r==20]
    
    delta.offsite.biomass.r<-zero.r
    delta.offsite.biomass.r[cover.type.r==30 & pwg.r==20] <- delta.1yr.biomass.r[cover.type.r==30 & pwg.r==20] # moist mixed conifer on dry sites
    delta.offsite.biomass.r[cover.type.r==50 & pwg.r==20] <- delta.1yr.biomass.r[cover.type.r==50 & pwg.r==20] # cold-dry conifer on dry sites
    delta.offsite.biomass.r[cover.type.r==40 & pwg.r==20] <- delta.1yr.biomass.r[cover.type.r==40 & pwg.r==20] # cold-moist conifer on dry sites
    delta.offsite.biomass.r[cover.type.r==40 & pwg.r==30] <- delta.1yr.biomass.r[cover.type.r==40 & pwg.r==30] # cold-moist conifer on moist sites
    delta.offsite.biomass.r[cover.type.r==50 & pwg.r==30] <- delta.1yr.biomass.r[cover.type.r==50 & pwg.r==30] # cold-dry conifer on moist sites
    # delta.offsite.biomass.r[cover.type.r==30 & pwg.r==40] <- delta.1yr.biomass.r[cover.type.r==30 & pwg.r==40] # moist conifer on cold-moist sites
    delta.offsite.biomass.r[cover.type.r==40 & pwg.r==50] <- delta.1yr.biomass.r[cover.type.r==40 & pwg.r==50] # cold-moist conifer on cold-dry sites
    delta.offsite.biomass.r[delta.offsite.biomass.r==0]<-NA
    plot(cover.type.r)
    plot(delta.offsite.biomass.r,add=T,col=hcl.colors(100,'Temps'))
    
    
    #-------------------------------------------------------#
    ## Heterogeneity:
    age.class.r<-roundFUN(focal.yr.mean.age.top3.dom, 40)
    
    age.classes<-unique(age.class.r[age.class.r>0 & !is.na(age.class.r)])
    cover.types<-unique(cover.type.r[cover.type.r>0 & !is.na(cover.type.r)])
    
    # # Identify unique patches:
    # patches<-focal.yr.total.biomass
    # patches[age.class.r==0 | is.na(age.class.r)]<-NA
    # patches[cover.type.r==0 | is.na(cover.type.r)]<-NA
    # counter=1
    # for(a in age.classes){
    #   for(c in cover.types){
    #     # Identify homogenous forests
    #     patches[age.class.r==a & cover.type.r==c]<-#counter
    #       counter=counter+1
    #   }
    # }
    # plot(patches,col=hcl.colors(100,'Temps'))
    
    ## Calculate area of homogenous patches
    cat('  - Area of homogenous patches... ')
    patches.r<-zero.r
    for(a in age.classes){
      for(c in cover.types){
        # Identify homogenous forests
        r<-zero.r
        r[age.class.r==a & cover.type.r==c]<-1
        # Identify patches.r
        r2<-patches(r) 
        # Calculate area
        rcl.df<-data.frame(freq(r2))
        rcl.df$ha<-rcl.df$count * 0.81
        r3<-classify(r2,rcl=data.frame('is'=rcl.df$value,'becomes'=rcl.df$ha))
        r3[is.na(r2)]<-0
        patches.r<-patches.r+r3
      }
    }
    plot(patches.r,col=hcl.colors(100,'Temps'))
    
    
    #-------------------------------------------------------------------------------------------#
    ### --- Add outputs to list --- ###
    #-------------------------------------------------------#
    new.outputs<-c('age.gt.120.r','seed.source.r',
                   'huc12.matureForestPatchArea.r','huc12.forest.percent.r',
                   'seed.source.dist.r','delta.offsite.biomass.r','patches.r')
    names(new.outputs)<-c('Old_Forest','Seed_Source',
                          'Area_of_mature_forest_patches_120yo_over_10_ha','Percent_Forest_gt50yr',
                          'Seed_Source_Distance_m','Delta_Offsite_Biomass_Mg','Area_of_Homogeneous_Forest_ha')
    outputs<-c(outputs,new.outputs)
    
    #-------------------------------------------------------------------------------------------#
    
    #-----------------------------------------------------------------------------------------------------------------------
    ### LANDSCAPE INTEGRITY: ----
    cat('\n-> Calculating Landscape integrity...\n')
    #-------------------------------------------------------------------------------------------#
    ### Stability ###
    #-------------------------------------------------------#
    
    #-------------------------------------------------------#
    ## HUC12-level structural diversity (mean # of structure classes):
    age.class.r<-roundFUN(focal.yr.mean.age.top3.dom, 40)
    
    ## Summarize per HUC12:    
    huc12.age.diversity.r<-HUC12.r
    huc12.age.diversity.r[!is.na(huc12.age.diversity.r)]<-0 #length(unique(age.class.r[!is.na(HUC12.r)]))
    for(huc in unique(values(HUC12.r))){
      if(is.na(huc)) next
      # Isolate structure classes within the focal huc
      df<-data.frame('str.class'=unname(age.class.r[HUC12.r==huc & pwg.r>12])) # Only consider vegetation PWGs
      if(length(df[!is.na(df)])==0) next
      # cat(huc,' ')
      
      # Total count of structure classes
      freq<-aggregate(df$str.class,by=list(df$str.class),FUN=NROW)
      huc12.age.diversity.r[HUC12.r==huc]<-nrow(freq)
      
      # Count of structure classes that make up >5% of the area
      freq.over.5percent<-freq[freq$x>(0.05*sum(freq$x)),'Group.1']
      huc12.age.diversity.r[HUC12.r==huc]<-length(freq.over.5percent)
    }
    plot(huc12.age.diversity.r,col=hcl.colors(100,'Temps'))
    
    #-------------------------------------------------------#
    ## HUC12-level compositional diversity (mean # of PWG cover types):
    
    ## Summarize per HUC12:    
    huc12.diversity.r<-HUC12.r
    huc12.diversity.r[!is.na(huc12.diversity.r)]<-length(unique(pwg.r[!is.na(HUC12.r)]))
    for(huc in unique(values(HUC12.r))){
      if(is.na(huc)) next
      # Select cover classes within the focal huc
      df<-data.frame('dom.spp'=unname(cover.type.r[HUC12.r==huc & pwg.r>12])) # Only consider vegetation PWGs
      if(length(df[!is.na(df)])==0) next
      freq<-aggregate(df$dom.spp,by=list(df$dom.spp),FUN=NROW)
      
      # Count of cover type classes that make up >5% of the area
      freq.over.5percent<-freq[freq$x>(0.05*sum(freq$x)),'Group.1']
      huc12.diversity.r[HUC12.r==huc]<-length(freq.over.5percent)     
      
      # Count of cover type classes that make up >10% of the area
      # freq.over.10percent<-freq[freq$x>(0.1*sum(freq$x)),'Group.1']
      # huc12.diversity.r[HUC12.r==huc]<-length(freq.over.10percent)
      
      ## Total number of PWG
      # huc12.diversity.r[HUC12.r==huc]<-length(unique(pwg.r[HUC12.r==huc]))
    }
    plot(huc12.diversity.r,col=hcl.colors(100,'Temps'))
    
    
    #-------------------------------------------------------#
    ## Dispersion of forest throughout the landscape (percent of landscape >1km from a mature forest patch):
    # Forest patches of old forest
    mature.patch.dist.r<-patches(age.gt.120.r,directions=8)
    # NA stays NA, 1 becomes distance 0, 0s become distance to nearest 1.
    mature.patch.dist.r[mature.patch.dist.r==0 | is.na(mature.patch.dist.r)]<-0 # 0 for stand initiation
    mature.patch.dist.r[mature.patch.dist.r>0]<-1 # 1 for seed sources
    mature.patch.dist.r[pwg.r %in% c(10:15) | is.na(pwg.r)]<-0 # NA for non-forest zones
    plot(mature.patch.dist.r,col=hcl.colors(100,'Temps'))
    
    cat('  - Distance to mature forest... ')
    mature.patch.dist.r <- distance(mature.patch.dist.r, target=0)
    
    mature.patch.dist.r[pwg.r %in% c(10:12) | is.na(pwg.r)]<-NA # Reset NA for non-forest zones
    plot(mature.patch.dist.r,col=hcl.colors(100,'Temps'))
    
    #-------------------------------------------------------------------------------------------#
    ### Type change ###
    #-------------------------------------------------------#
    ## Percent area that converts from forest PWG to non-forest PWG:
    
    ## Set stand initation phase to 0
    cover.type.r2<-cover.type.r
    cover.type.r2[focal.yr.mean.age.top3.dom<20]<-0
    
    ## Save initial cover type at year 1:
    if(yr==1){
      dominant.spp.yr1.r<-dominant.spp[['dominant.spp.1']]
      cover.type.yr1.r<-cover.type.r
      cover.type.yr1.r2<-cover.type.r2
    }
    
    # 0 = no change, forest. NA = no change, non-forest. -1 = forest to non-forest. 1 = non-forest to forest.
    type.change.r<-cover.type.r2
    type.change.r[cover.type.yr1.r2==0 & cover.type.r2==0]<-NA
    type.change.r[cover.type.yr1.r2!=0 & cover.type.r2!=0]<-0
    type.change.r[cover.type.yr1.r2!=0 & cover.type.r2==0]<- -1
    type.change.r[cover.type.yr1.r2==0 & cover.type.r2!=0]<- 1
    if(max(values(type.change.r), na.rm=T)>1) stop('Maximum value for type.change.r should be 1, but it is higher. Please resolve.')
    
    plot(type.change.r,col=hcl.colors(100,'Temps'))
    
    #-------------------------------------------------------#
    ## Persistence as initial forest type (% of pixels that stay as initial forest type):
    comp.change.r<-cover.type.r2
    comp.change.r[cover.type.r2==cover.type.yr1.r2]<-0
    comp.change.r[cover.type.r2!=cover.type.yr1.r2]<-1
    if(max(values(comp.change.r),na.rm=T)>1) stop('Maximum value for comp.change.r should be 1, but it is higher. Please resolve.')
    plot(comp.change.r,col=hcl.colors(100,'Temps'))
    
    
    
    #-------------------------------------------------------#
    ## Fractal Dimension:
    # library(landscapemetrics)
    # lsm_l_frac_mn(age.gt.120.patches.r)
    # 
    #-------------------------------------------------------#
    ## Beetle risk?:
    
    #-------------------------------------------------------#
    ## Climate risk?
    
    
    #-------------------------------------------------------------------------------------------#
    ### Habitat quality ###
    #-------------------------------------------------------#
    ## Lynx habitat quality:
    
    #-------------------------------------------------------#
    ## Owl habitat quality:
    
    #-------------------------------------------------------#
    ## Fine-scale structural diversity:
    
    #-------------------------------------------------------#
    ## Meso-scale structural diversity:
    
    #-------------------------------------------------------#
    ## Biodiversity?
    
    
    #-------------------------------------------------------------------------------------------#
    ### --- Add outputs to list --- ###
    #-------------------------------------------------------#
    new.outputs<-c('huc12.age.diversity.r','huc12.diversity.r','mature.patch.dist.r',
                   'type.change.r','comp.change.r')
    names(new.outputs)<-c('N_age_classes_gt5percentArea_in_HUC12','N_spp_gt5percentArea_in_HUC12','Distance_to_old_forest_m',
                          'Type_Conversion','Cover_Type_Change')
    outputs<-c(outputs,new.outputs)
    #-------------------------------------------------------------------------------------------#
    
    #-----------------------------------------------------------------------------------------------------------------------
    ########################################################################################################################
    #-----------------------------------------------------------------------------------------------------------------------
    ### TRIM RASTERS TO STUDY AREA: ----
    cat('---------------------------------\n-> Trimming to study area...')
    for(r.name in outputs){
      r<-eval(parse(text=r.name))
      
      r[is.na(r)]<-0 # Zeros for active cells with NAs
      r[is.na(pwg.r)|pwg.r<12]<-NA # NAs for inactive cells
      
      if(MASK == T)
        r[is.na(pwg.noBuffer.r)]<-NA # NAs for the 5-km study area buffer
      
      assign(r.name,r)
    }
    
    #-----------------------------------------------------------------------------------------------------------------------
    ### WRITE OUTPUT RASTERS: ----
    # if(yr %in% YEARS.TO.OUTPUT){
    cat('\n-> Writing output rasters...')
    for(r.name in outputs){
      r<-eval(parse(text=r.name)) # Load layer
      
      r<-round(r,2)
      
      out.name<-names(outputs[outputs==r.name]) # Load out name
      
      names(r)<-r.name
      
      ## Write raster...
      writeRaster(r,file.path(landisOutputDir,'DST',paste0(out.name,'_yr',yr,'.tif')),overwrite=T)
    }
    # }
    #-----------------------------------------------------------------------------------------------------------------------
    ### SUMMARIZE TABLUAR OUTPUTS: ----
    
    if(SUMMARIZE.BY.PWG.and.HUC){
      
      ##   Summarize by PWG: ----
      cat('\n-> Summarizing output rasters by PWG...')
      t<-raster2csv(pwg.r)
      dst.pwg.df[dst.pwg.df$Year==yr,'PWG.check']<-t$zone
      dst.pwg.df[dst.pwg.df$Year==yr,'Area_Ha']<-t$area.ha
      dst.pwg.df[dst.pwg.df$Year==yr,'Occupied_Area_Ha']<-t$occupied.area.ha
      
      for(r.name in outputs){
        r<-eval(parse(text=r.name)) # Load layer
        
        out.name<-names(outputs[outputs==r.name]) # Load name
        # cat(paste0(out.name,', '))
        
        # Summarize by PWG
        t<-raster2csv(r)
        t$Mean.per.Ha<-t$sum/t$occupied.area.ha
        t$SD.per.Ha<-t$sd/0.81
        for(c in c('mean','sd','Mean.per.Ha','SD.per.Ha')){
          t[is.na(t[,c]),c]<-0
        }
        
        dst.pwg.df[dst.pwg.df$Year==yr,paste(out.name,c('Mean.per.cell','Mean.per.Ha','Sum','SD'),sep='.')]<-t[,c('mean','Mean.per.Ha','sum','SD.per.Ha')]
      }
      
      ##   Summarize by HUC12: ----
      cat('\n-> Summarizing output rasters by HUC12...\n')
      
      t<-raster2csv(pwg.r,agg.r=HUC12.r)
      dst.huc12.df[dst.huc12.df$Year==yr,'HUC12.check']<-t$zone
      dst.huc12.df[dst.huc12.df$Year==yr,'Area_Ha']<-t$area.ha
      dst.huc12.df[dst.huc12.df$Year==yr,'Occupied_Area_Ha']<-t$occupied.area.ha
      
      for(r.name in outputs){
        r<-eval(parse(text=r.name)) # Load layer
        r[is.na(r)]<-0 # Zeros for active cells with NAs
        r[is.na(pwg.r)|pwg.r<12]<-NA # NAs for inactive cells
        
        out.name<-names(outputs[outputs==r.name]) # Load name
        # cat(paste0(out.name,', '))
        
        # Summarize by HUC12
        t<-raster2csv(r,agg.r=HUC12.r)
        t$Mean.per.Ha<-t$sum/t$occupied.area.ha
        t$SD.per.Ha<-t$sd/0.81
        for(c in c('mean','sd','Mean.per.Ha','SD.per.Ha')){
          t[is.na(t[,c]),c]<-0
        }
        
        dst.huc12.df[dst.huc12.df$Year==yr,paste(out.name,c('Mean.per.cell','Mean.per.Ha','Sum','SD'),sep='.')]<-t[,c('mean','Mean.per.Ha','sum','SD.per.Ha')]
      }
    }
    #-----------------------------------------------------------------------------------------------------------------------
  } # END YEAR LOOP
  
  ### WRITE .CSV FILES: ----
  if(SUMMARIZE.BY.PWG.and.HUC) {
    write.csv(dst.pwg.df,file=file.path(landisOutputDir,'DST','DST_Metrics_by_PWG.csv'),row.names = F)
    write.csv(dst.huc12.df,file=file.path(landisOutputDir,'DST','DST_Metrics_by_HUC12.csv'),row.names = F)
  }
  gc()
  #-----------------------------------------------------------------------------------------------------------------------
  ########################################################################################################################
  #-----------------------------------------------------------------------------------------------------------------------
  #-----------------------------------------------------------------------------------------------------------------------
  #### Zip results: ----
  # scenarioName = gsub(dirToProcess,"",landisOutputDir)
  # scenarioName = gsub("[0-9]|[_]|LANDIS_Sim_Wen|/|BaseClim","",scenarioName)
  # scenarioName = gsub("NECNSCRAPPLE","WILDFIRE",scenarioName)
  # scenarioName = gsub("HARVEST","_HARVEST",scenarioName)
  # scenarioName = gsub("NECN","NO_FIRE",scenarioName)
  # scenarioName
  # 
  # if(OVERWRITE.ZIP.FILES==T | 
  #    !file.exists(paste0(dirToProcess,'/DST_Inputs_',scenarioName,'.zip'))){
  #   cat('\n***  ZIPPING DHSVM output maps for',landisOutputDir,'  ***\n')
  #   # Zip helper maps:
  #   zipr(paste0(landisOutputDir,'/Interpolated_maps_for_DST_metrics_',scenarioName,'.zip'),files = list.dirs(file.path(landisOutputDir,'DST',dir(file.path(landisOutputDir,'DST')))))
  #   
  #   # Zip maps and .CSV files: 
  #   mapsToZip<-dir(file.path(landisOutputDir,'DST'))[grepl("[.]",dir(file.path(landisOutputDir,'DST')))]
  #   zipr(paste0(dirToProcess,'/DST_Inputs_',scenarioName,'.zip'),files = file.path(landisOutputDir,'DST',mapsToZip))
  # }
  #-----------------------------------------------------------------------------------------------------------------------
  #-----------------------------------------------------------------------------------------------------------------------
}
cat('\n\n
===================================================================================================
***************************************************************************************************
                               Finished generating DST output maps.             
---------------------------------------------------------------------------------------------------
***************************************************************************************************
===================================================================================================\n\n')

