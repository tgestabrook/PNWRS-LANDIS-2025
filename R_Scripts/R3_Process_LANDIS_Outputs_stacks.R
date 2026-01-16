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
###   Original code developed by Tucker Furniss                                                                   ######-
###     Contact: tucker.furniss@usda.gov; tucker.furniss@gmail.com;                                               ######-
###   Revised by Thomas Estabrook                                                                                 ######-
###     Contact: thomas.estabrook@usda.gov; tgestab@umich.edu                                                     ######-
#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------

library(terra)
library(tidyverse)
library(tidyterra)
library(magick)
library(landscapemetrics)
library(gridExtra)
library(scales)
library(zoo)
library(zip)
library(hues)
library(foreach)
library(doParallel)
source("./R_Scripts/Post_process/Post_functions.R") # load custom functions

# set tmpdir to prevent running out of space
Sys.setenv(TMPDIR = "F:/R_TEMP")
terraOptions(tempdir = "F:/R_TEMP")

LANDIS.EXTENT<-'WenEnt'
# Dir <- file.path('F:/2025_Q4_Scenarios', "FEMC")
Dir <- file.path('F:/2025_Q4_Scenarios', LANDIS.EXTENT)

bigDataDir<-'F:/LANDIS_Input_Data_Prep/BigData'
dataDir<-'F:/LANDIS_Input_Data_Prep/Data'
modelDir<-'F:/V8_Models'

MTBS_dir <- file.path(dataDir,'MTBS_and_FOD_Fires', LANDIS.EXTENT)
DHSVM_dir <- if (dir.exists(file.path(Dir, '..', "DHSVM_Outputs"))) {file.path(Dir, '..', "DHSVM_Outputs")} else {NULL}

#### SET LANDIS-II DIRECTORY TO PROCESS: ----
dirToProcess <- file.path(Dir)
#-----------------------------------------------------------------------------------------------------------------------
## List all sims in the directory: ----
landisRuns <- file.path(dirToProcess,dir(dirToProcess)[grepl('Sim',dir(dirToProcess)) & !grepl('.zip', dir(dirToProcess))])

### Define post-processing options:
simOpts <- list(
  rerunBiomassAnnualDynamics = F,
  rerunHarvest = T,
  rerunFires = F,
  remakeGifs = F,
  RUN.DHSVM.MAPS = F,
  RERUN.DHSVM.MAPS = F,
  RERUN.DHSVM.HEIGHT_FC_and_LAI_MAPS = F,
  SUMMARIZE.BY.PWG.and.HUC = T,
  OVERWRITE.ZIP.FILES = F,
  increment = 1, # Define Desired Interval to interpolate DHSVM maps to
  RUN.DST.MAPS = F,
  RERUN.DST.MAPS = F,
  base.year = 2020,
  max.fine.fuels = 2000,  # max fine fuels for gif -- should be same as SCF parameter
  DST.comparison.yr = 50,  # year to compare treatments in spatial DST
  compare.version = F
)

#-----------------------------------------------------------------------------------------------------------------------
### Things in common to all runs: ---
#### Set fire severity thresholds: ----
severity.thresholds<-c(5, 41,176,376,2001);names(severity.thresholds)<-c('Unburned','Low','Moderate','High','max') # Miller & Thode 2007
severity.reclass.df<-data.frame('from'=c(-100, severity.thresholds[1:4]),'to'=c(severity.thresholds[1:5]),'becomes'=c(NA,1,2,3,4))

#### Set color palettes: ----
demCols<-colorRampPalette(c('black','white'))  # for plotting DEM
biomassCols<-hcl.colors(50,'Greens',rev=T)  # for plotting biomass
severityColsClassified<-colorRampPalette(c('darkgreen','darkseagreen','goldenrod1','firebrick4'))(4)
harvestColsClassified<-data.frame('value'=1:14, 
                                  'color'=c('red', # Old valley 2
                                            'red',  # Old mesic 3
                                            'red',  # Old xeric 4
                                            'tomato4',  # Industrial 5 
                                            'red',  # Old pre-commercial 6
                                            'orchid1',  # Salvage 7
                                            'slateblue2', # WA_DNR 8
                                            'lightgoldenrod',  # PCT dry 9
                                            'olivedrab2',  # PCT moist 10
                                            'dodgerblue',  # PCT cold 11
                                            'goldenrod',  # CT dry 12 
                                            'olivedrab4',  # CT moist 13
                                            'dodgerblue4',  # CC Cold 14
                                            'dodgerblue3'  #WC Cold 15
                                  ),
                                  'id'=1:14,
                                  'Treatment'=c('MBValley', 'MBMesic', 'MBXeric', 'Industrial', 'PCTgeneric', 'Salvage','WA DNR', 'PCTdry', 'PCTmoist', 'PCTcold', 'CTdry', 'CTmoist', 'CCcold', 'WCcold'))
## Set colors:
dhsvmCols<-colorRampPalette(c('saddlebrown','goldenrod1','darkslategray','darkgreen','seagreen4',
                              'turquoise4','dodgerblue4','midnightblue','navy','orchid4','palegoldenrod','wheat'))(49)
options(scipen=999)

#### Define ecoregions: ----
ecos<-c('Water','Bareground','Grassland','Shrubland','Hardwood','Alpine meadow','Dry mixed conifer','Moist mixed conifer','Cold-moist conifer','Cold-dry conifer')
names(ecos)<-c(10,11,12,13,14,15,20,30,40,50)
ecos2 <- ecos[3:10]  # for plotting
pwg.r <-  rast(file.path(dataDir, "PWG", paste0("PWG_", LANDIS.EXTENT, ".tif")))
pwg.r <- ifel(pwg.r == 0, NA, pwg.r)
names(pwg.r) <- "PWG"

active.r <- ifel(pwg.r < 12, NA, 0)# inactive = NA, active = 0

### Load LUA raster: ----
lua.r<-rast(file.path(dataDir,'PWG',paste0('LUA_',LANDIS.EXTENT,'.tif')))
### Define LUA codes: 
luas<-c('Water','Federal-Active Mgmt','Federal Wildlands','State-Active Mgmt','State/Local Park','NGO-Conservation',
        'Private-Undeveloped/Ag.','Private Industrial Forest','Urban/Rural',
        'Tribal-Undeveloped/Ag.','Tribal Industrial Forest','Tribal Urban/Rural','Other Govt. Lands')
names(luas)<-c(10,11,12,18,20,21,22,24,26,27,29,31,32)


### Load Study Area mask without 5-km buffer: ----
pwg.noBuffer.r<-rast(file.path(dataDir,"PWG",paste0('PWG_noBuffer_',LANDIS.EXTENT,'.tif')))

### Load HUC x PWG ecoregions: ----
PWGxHUC10.sf<-vect(file.path(dataDir,'PWG',paste0('PWGxHUC_',LANDIS.EXTENT,'.shp')))

### Map of cold vs. warm forests: ----
PWG_reclass.r <- ifel(pwg.r %in% c(20, 30), 30, ifel(pwg.r %in% c(40, 50), 50, NA)) 
forest_type_area.df <- as.data.frame(PWG_reclass.r) |> group_by(PWG) |> summarise(forest_area = 0.81*dplyr::n()) |> mutate(PWG = as.factor(PWG))

annual_sev.df <- data.frame('year' = integer(0), 'PWG'=character(0), 'sev_class'=character(0), 'count' = integer(0))

### Compile MTBS record for study area
for (i in 1984:2019){
  cat(paste0(i,'...'))
  fire_sev <- rast(file.path(MTBS_dir, paste0('Observed_fires_', i, '.tif'))) 
  fire_sev[fire_sev<min(severity.reclass.df$from)]<-NA
  
  fire_sev <- fire_sev |> 
    terra::classify(severity.reclass.df, include.lowest = T) 
  # plot(fire_sev)
  
  stack <- c(PWG_reclass.r, 'dnbr'= fire_sev)
  
  yr_df <- as.data.frame(stack) |> 
    filter(!is.na(PWG)) |> 
    mutate(sev_class = cut(dnbr, c(0, 1, 2, 3, 4), c('UB', 'Low', 'Moderate', 'High'))) |>
    group_by(PWG, sev_class) |> summarise(count = dplyr::n()) |>
    mutate(PWG = as.factor(PWG), year = i, sev_class = as.character(sev_class)) |> filter(!is.na(sev_class))
  
  #hist(yr_df$dnbr)
  annual_sev.df <- dplyr::bind_rows(annual_sev.df, yr_df)
}

#### Load elevation and hillshade: ----
dem.r<-rast(file.path(dataDir,paste0("DEM_90m_", LANDIS.EXTENT, ".tif")))
names(dem.r) <- "Elevation"
hillshade.r<-rast(file.path(dataDir, paste0("Hillshade_", LANDIS.EXTENT, ".tif")))
slope.percent.r<-tan(terrain(dem.r,'slope',unit='radians'))*100

### Load HUC rasters: ----
## Shapefile:
HUC12.sf<-vect(file.path(dataDir,'PWG',paste0('HUC12_',LANDIS.EXTENT,'.shp'))) |> mutate(HUC12.num = as.numeric(HUC12))
HUC10.sf<-vect(file.path(dataDir,'PWG',paste0('HUC10_',LANDIS.EXTENT,'.shp'))) |> mutate(HUC10.num = as.numeric(HUC10))

## Raster: 
HUC12.r<-rasterize(HUC12.sf,pwg.r,field='HUC12.num')
HUC10.r<-rasterize(HUC10.sf,pwg.r,field='HUC10.num')

#### Read in FVS 100-yr simulation for comparison: ----
if(file.exists(file.path(dataDir, paste0('FVS_100yr_sim_biomass_summary_', LANDIS.EXTENT,'.csv')))){
  fvsSimToPlot<-read.csv(file.path(dataDir, paste0('FVS_100yr_sim_biomass_summary_', LANDIS.EXTENT,'.csv'))) |> mutate(PWG = as.character(Eco)) |>
    left_join(data.frame("PWG" = names(ecos), PWG_Name = ecos))
}else {fvsSimToPlot<-read.csv(file.path(dataDir,'FVS_100yr_sim_biomass_summary.csv')) |> mutate(PWG = as.character(Eco)) |>
  left_join(data.frame("PWG" = names(ecos), PWG_Name = ecos))}

#### Load latin names: ----
species.codes<-read.csv(file.path(dataDir,"Species_code_crosswalk.csv"))[,c('SpecCode','SpeciesLatin')]
latin<-species.codes$SpeciesLatin; names(latin)<-species.codes$SpecCode

#### Define functional groups: ----
funcgroups = list(
  conifers=c("AbieAmab","AbieGran","AbieLasi","AbieProc","ChamNoot","JuniOcci","JuniScop","LariLyal","LariOcci",
            "PiceEnge","PinuAlbi","PinuCont","PinuMont","PinuPond","PseuMenz","TaxuBrev","ThujPlic","TsugHete","TsugMert"),
  conifers.mesic=c("AbieAmab","AbieProc","ChamNoot","PiceEnge","PseuMenz","TaxuBrev","ThujPlic","TsugHete"),
  conifers.xeric=c("AbieGran","JuniOcci","JuniScop","LariOcci","PinuMont","PinuPond"),
  conifers.subalpine=c("AbieLasi","LariLyal","PiceEnge","PinuAlbi","PinuCont","TsugMert"),
  hardwoods=c("AcerMacr","AlnuRubr","BetuOcci","BetuPapy","CornNutt","FraxLati","PopuBals","PopuTrem", "PrunEmar","QuerGarr"),
  shrubs=c("Nfixer_Resprt","NonFxr_Resprt","NonFxr_Seed"),
  grass="Grass_Forb"
)


#### List all species: ----
species.subset<-c('PseuMenz','TsugMert','AbieLasi','PiceEnge','PinuAlbi',
                  'PinuPond','PinuMont','PinuCont','LariOcci','AbieGran',
                  'ThujPlic','TsugHete','AbieAmab','AlnuRubr','PopuBals',
                  'Nfixer_Resprt','NonFxr_Resprt','NonFxr_Seed','Grass_Forb','TotalBiomass')



#### Calculate Study area size: ----
study.area.size.Ha<-length(pwg.r[pwg.r%in%12:50&!is.na(pwg.r)]) * res(pwg.r)[1] * res(pwg.r)[2] / 10000

### Load Distance to Roads raster: ----
if(file.exists(file.path(dataDir,'PWG',paste0("road_dist_",LANDIS.EXTENT,".tif"))) & file.exists(file.path(dataDir,'PWG',paste0("road_noWild_dist_",LANDIS.EXTENT,".tif")))){
  roads.r<-rast(file.path(dataDir,'PWG',paste0("roads_",LANDIS.EXTENT,"_45m.tif")))
  road.dist.r<-rast(file.path(dataDir,'PWG',paste0("road_dist_",LANDIS.EXTENT,".tif")))
  roads.no.wild.r<-rast(file.path(dataDir,'PWG',paste0("roads_noWild_",LANDIS.EXTENT,"_45m.tif")))
  road.no.wild.dist.r<-rast(file.path(dataDir,'PWG',paste0("road_noWild_dist_",LANDIS.EXTENT,".tif")))
} else {stop('MISSING ROAD MAPS: Rerun in 0_Study_area_masks_and_raw_layer_prep.R')  }


### Load LANDFIRE raster and definitions: ----
landfire.r<-rast(file.path(dataDir,paste0("LANDFIREv2.0_EVT_",LANDIS.EXTENT,".tif")))
landfire.codes<-read.csv(file.path(dataDir,'LF16_EVT_200.csv')) |>
  filter(VALUE%in%unique(values(landfire.r))) |>
  select(VALUE, EVT_GP_N, EVT_PHYS, EVT_LF)
lf.codes.present<-unique(values(landfire.r))

if(ext(landfire.r)!=ext(pwg.r)) stop('Extent of pwg.r does not match extent of landfire.r. This will cause major issues.')

#### GGplot theme: ----
theme_set(theme_classic()+theme(panel.background = element_rect(color='black',fill=NA,linewidth=0.75),
                                panel.border = element_rect(color='black',fill=NA,linewidth=0.75),
                                plot.margin = margin(0,0,0,0),
                                axis.line = element_line(color='black',linewidth=0.25),
                                axis.text=element_text(size=10,color='black'),axis.title=element_text(size=10,color='black'),
                                axis.ticks = element_line(color='black',linewidth=0.4),axis.ticks.length = unit(0.05,'cm'),
                                legend.margin = margin(1,1,1,1),legend.box.margin = margin(2,2,2,2),
                                legend.text=element_text(size=8,color='black'),legend.title=element_text(size=10,color='black'),
                                legend.box.background = element_rect(colour='grey20',fill='white'),legend.background = element_blank(),
                                legend.key.size=unit(0.4,'cm')))

#### If testing on single run, run this line and then the code inside the for loop below.
landisOutputDir <- landisRuns[34]

#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
### Core post-processing loop: ----
for(landisOutputDir in prioritize_uncompressed_runs(landisRuns)){
  if(length(dir(landisOutputDir))==0) stop("LANDIS output folder is empty! Check file path...")
  
  #----------------------------------------------------------------------#
  cat('\n\n\n***********************************************************************************************\n-----------------------------------------------------------------------------------------------
    Post-processing:',gsub(dirToProcess,'',landisOutputDir),
      '\n-----------------------------------------------------------------------------------------------\n***********************************************************************************************\n\n')
  
  ### Timing
  outFile <- file.path(landisOutputDir, 'post-processing-log.txt')
  cat(paste('Start time:', Sys.time(), '\n\n'), file=outFile)
  

  if(length(list.files(landisOutputDir, pattern = '\\.img$', ignore.case = T, recursive = T))>0){
    source("./R_Scripts/Post_process/Compress_LANDIS_outputs.R")  # Compress and make stacks if raw LANDIS outputs
  }
  
  ### Load Ecoregions raster
  eco_files<-dir(file.path(landisOutputDir,'Input_file_archive'))[grepl('.tif',dir(file.path(landisOutputDir,'Input_file_archive')))&
                                                               grepl('ECOREGIONS',dir(file.path(landisOutputDir,'Input_file_archive')))]
  if(length(eco_files)==1) {
    ecos.r<-rast(file.path(landisOutputDir,'Input_file_archive',eco_files))
  } else {stop("FIX ECOREGIONS in input file archive")}
  ecos.r <- ifel(ecos.r == 0, NA, ecos.r)
  names(ecos.r) <- "PWG"
  
  ## Ecoregions text file
  if(file.exists(file.path(landisOutputDir,'Input_file_archive',paste0('ECOREGIONS.txt')))){
    ecos.txt<-read.table(file.path(landisOutputDir,'Input_file_archive','Ecoregions.txt'),skip = 5)
  } else if (file.exists(file.path(landisOutputDir,'Input_file_archive',paste0('ECOREGIONS_', LANDIS.EXTENT, '.txt')))){
    ecos.txt<-read.table(file.path(landisOutputDir,'Input_file_archive',paste0('ECOREGIONS_', LANDIS.EXTENT, '.txt')),skip = 5)
  } else {stop("FIX ECOREGIONS TEXT FILE in input file archive")}
  
  ### Load IC: ----
  if(file.exists(file.path(landisOutputDir,'Input_file_archive',paste0("INITIAL_COMMUNITIES_",LANDIS.EXTENT,".tif")))) {
    IC.r<-rast(file.path(landisOutputDir,'Input_file_archive',paste0("INITIAL_COMMUNITIES_",LANDIS.EXTENT,".tif")))
  } else {
    IC.r<-dir(file.path(landisOutputDir,'Input_file_archive'))[grepl('.tif',dir(file.path(landisOutputDir,'Input_file_archive')))&
                                                                 grepl('communit',dir(file.path(landisOutputDir,'Input_file_archive')))]
  }
  
  ### Load file paths and list species: ----
  # Define Map Folders: ----
  biomassOutput <- file.path(landisOutputDir, 'biomassOutput')
  ageOutput <- file.path(landisOutputDir, 'ageOutput')
  fireOutput <- file.path(landisOutputDir, 'social-climate-fire')
  harvestOutput <- file.path(landisOutputDir, 'Harvest')
  necnOutput <- file.path(landisOutputDir, "NECN")
  MHOutput <- file.path(landisOutputDir, 'MagicHarvest')
  
  totalBiomass_stack.r <- rast(file.path(biomassOutput, "TotalBiomass-yr-biomass.tif"))
  simLength <- totalBiomass_stack.r |> names() |> str_extract("\\d+") |> as.integer() |> max() 
  
  ### Interpolate rasters
  if (nlyr(totalBiomass_stack.r)<simLength){
    source("./R_Scripts/Post_process/Post_interpolate_outputs.R")
  }
  
  ## Generate lists of output map names: ----
  biomassMaps<-dir(biomassOutput)[grepl('.tif',dir(biomassOutput))]
  ageMaps<-dir(ageOutput)[grepl('MED.tif',dir(ageOutput))]
  fireMaps<-dir(fireOutput)[grepl('.tif',dir(fireOutput))]
  harvestMaps<-dir(harvestOutput)[grepl('.tif',dir(harvestOutput))]
  
  totalBiomass_stack.r <- rast(file.path(biomassOutput, "TotalBiomass-yr-biomass.tif"))
  biomassStack.r <- rast(file.path(biomassOutput, dir(biomassOutput)[grepl("yr-biomass", dir(biomassOutput))]))  # one mega-stack with biomass of all species
  
  LAI.stack <- rast(file.path(necnOutput, "LAI-yr.tif"))
  MedAgeAllspp.stack <- rast(file.path(ageOutput, dir(ageOutput)[grepl("yr-MED", dir(ageOutput))]))
  MaxAgeAllspp.stack <- rast(file.path(ageOutput, dir(ageOutput)[grepl("yr-MAX", dir(ageOutput))]))
  BiomassTrees.stack <- biomassStack.r |> select(!starts_with(c("Nfixer_Resprt","NonFxr_Resprt","NonFxr_Seed","Grass_Forb","TotalBiomass"))) 
  
  ## List all years: ----
  yrs <- totalBiomass_stack.r |> names() |> str_extract("\\d+") |> as.integer()
  
  ### Crunch out age stacks 
  source("./R_Scripts/Post_process/Post_ageClasses.R")

  #### Fires #### ----
  if(dir.exists(fireOutput)){
    cat(paste('\nFire routine starting...', Sys.time(), '\n'), file = outFile, append = T)
    source('./R_Scripts/Post_process/Post_fires.R')
  
    gc()  # free up memory
  }
  
  #### Harvest #### ----
  if(dir.exists(harvestOutput)){
    ### Load harvest maps: ----
    cat('\nLoading harvest data...\n')
    harvestPrescripts.r <- rast(file.path(harvestOutput, "biomass-harvest-prescripts-yr.tif"))
    biomassRemoved.r <- rast(file.path(harvestOutput, "biomass-removed-yr.tif"))
    standIDmaps.r <- rast(file.path(MHOutput, "MH_stands_yr.tif"))
    names(standIDmaps.r) <- str_replace(names(standIDmaps.r), '.tif', '') |>
      str_replace_all('_', '-')
    
    ### Load dataframe with merchantable fraction by species and harvest type
    merch_partition.df <- read.csv(file.path(modelDir, 'Shared_inputs', 'Harvest_merch_table.csv')) |>  # table specifying merch/nonmerch breakdown
      pivot_longer(cols = !starts_with("Species"), names_to = "Prescription", values_to = "Merch_frac")
    
    ### Load harvest logs
    harvestEvents.df <- read.csv(file.path(landisOutputDir, "biomass-harvest-event-log.csv"))
    harvestSum.df<-read.csv(file.path(landisOutputDir,'biomass-harvest-summary-log.csv')) |>
      rename('Year' = 'Time')
    
    if(nrow(harvestEvents.df>0)){
      source('./R_Scripts/Post_process/Post_harvest.R')
    } else {
      rm(harvestEvents.df)  # remove so as not to break downstream code using harvestevents to plot outputs
    }
    
    gc()  # free up memory
  }
  
  #### Biomass #### ----
  source('./R_Scripts/Post_process/Post_biomass.R')
  gc()

  # if (!file.exists(file.path(landisOutputDir, "Fire_Severity_fine_fuels.gif"))|
  #     simOpts$remakeGifs){  # separate out GIFs since they're now the slowest to generate
  #   source('./R_Scripts/Post_process/Make_gifs.R')
  # }
  
  #### DHSVM #### ----
  if((simOpts$RUN.DHSVM.MAPS&(!file.exists(file.path(landisOutputDir,'DHSVM','DHSVM_yr-100.tif'))))|simOpts$RERUN.DHSVM.MAPS){
    source('./R_Scripts/Post_process/Post_DHSVM_stopgap.R')
  }
  
  #### Zip results: ----
  scenarioName <- landisOutputDir |> 
    str_replace(dirToProcess, "") |>
    str_replace_all("/", "") |>
    str_replace("Sim.{6}", "DHSVM")
  
  if(simOpts$OVERWRITE.ZIP.FILES==T |
     !file.exists(paste0(dirToProcess,'/',scenarioName,'.zip'))){
    cat('\n***  ZIPPING DHSVM output maps for',landisOutputDir,'  ***\n')
    zipr(paste0(dirToProcess,'/',scenarioName,'.zip'),files = file.path(landisOutputDir,'DHSVM',dir(file.path(landisOutputDir,'DHSVM'))))
  }
  
  # stop()
  # 
  if(file.exists(file.path(landisOutputDir,'DST','DST_Metrics_by_HUC12.csv')) & simOpts$RERUN.DST.MAPS == F){
    cat('\nDST outputs already exist for',gsub(dirToProcess,"",landisOutputDir),'. Skipping to next sim...')
  } else {
    source('./R_Scripts/Post_process/Generate_DST_Maps.R')
  }
  
  if(!file.exists(file.path(landisOutputDir, "Diagnostics.html"))){
    rmarkdown::render(input = "./R_Scripts/Post_process/Sim_diagnostics.Rmd", 
                    output_format = "html_document",
                    output_file = file.path(landisOutputDir, "Diagnostics.html"), )
  }
  
  
  cat('\n\n###################################################################################################################################
      ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~          Post-processing COMPLETE!        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ###################################################################################################################################\n', file = outFile, append = T)
  cat('\n\n###################################################################################################################################
      ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~          Post-processing COMPLETE!        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ###################################################################################################################################\n')
  
  rm(list=ls()[!ls()%in%c('Dir',
                          'LANDIS.EXTENT',
                          'bigDataDir',
                          'dataDir',
                          'modelDir',
                          'MTBS_dir',
                          'DHSVM_dir',
                          'dirToProcess',
                          'landisRuns',
                          'simOpts',
                          'demCols',
                          'biomassCols',
                          'severityColsClassified',
                          'harvestColsClassified',
                          'dhsvmCols',
                          'ecos',
                          'ecos2',
                          'pwg.r',
                          'lua.r',
                          'pwg.noBuffer.r',
                          'PWGxHUC10.sf',
                          'road.no.wild.dist.r',
                          'dem.r',
                          'hillshade.r',
                          'slope.percent.r',
                          'HUC12.sf',
                          'HUC12.r',
                          'HUC10.r',
                          'HUC10.sf',
                          'fvsSimToPlot',
                          'species.codes',
                          'latin',
                          'funcgroups',
                          'species.subset',
                          'base.year',
                          'study.area.size.Ha',
                          'severity.thresholds',
                          'severity.reclass.df',
                          'max.fine.fuels',
                          'landfire.r',
                          'landfire.codes',
                          'lf.codes.present',
                          'emp.fire.dnbr',
                          'PWG_reclass.r',
                          'forest_type_area.df',
                          'annual_sev.df',
                          'pngOut',
                          'landfireReclassFUN', 
                          'sizeToDurationFUN',
                          'get_maps',
                          'get_ages_of_top3_biomass',
                          'read_and_label',
                          'compute_deviation',
                          'writeOutputRasts',
                          'raster2csv',
                          'trim_to_study_area',
                          'interpolateRaster')])  # clear out everything not on this list
  gc()
  
}

#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
### Scenario comparison: ----
cat('\n\n###################################################################################################################################
      ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~          Starting scenario comparison        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ###################################################################################################################################\n')

if(!file.exists(file.path(dirToProcess, 'Scenario_name_map.csv'))){
  cat('\n\nScenario csv not found, Aborting...')
  stop()}  # stop post-processing if a scenario map csv hasn't been placed in the directory

climate_colors <- c('palegreen', 'skyblue', 'goldenrod', 'darkorange', 'darkred')
names(climate_colors) <- c("Historical", "Base climate", 'RCP 4.5', 'RCP 6.5', 'RCP 8.5')

scenario_map.df <- read.csv(file.path(dirToProcess, 'Scenario_name_map.csv')) |>
  mutate(Folder_name = str_to_lower((Folder_name))) 

# put together dataframe of runs
sims.df <- data.frame('Run'=basename(landisRuns)) |>  # each folder gets a row
  mutate(date = str_extract(Run, '[0-9]{8}_[0-9]{4}'), Folder_name = gsub('_[0-9]{8}_[0-9]{4}', '', Run), # make a column of date info for each run and strip if from scenario name
         model_version = substr(Run, 4, 9)) |>   # get git commit id, useful for comparisons across model versions
  mutate(Folder_name = str_replace(Folder_name, model_version, '')) |>
  mutate(Folder_name = str_replace(Folder_name, paste0("Sim_", LANDIS.EXTENT, "_"), '')) |> # strip away standard sim prefix 
  mutate(Folder_name = str_to_lower(Folder_name)) |>
  left_join(scenario_map.df)  |> 
  filter(!is.na(Mgt_scenario)) |>
  mutate(DST_prepared = ifelse(file.exists(file.path(landisRuns, 'DST', 'DST_Metrics_by_PWG.csv')), T, F)) |>  # check if the DST input map script has been run
  mutate(Post_processed = ifelse(file.exists(file.path(landisRuns, 'Biomass_Annual_Dynamics.csv')), T, F))  # this is the last essential file generated in the post-processing script, so if it exists, the run has been fully processed

# if there are different model versions present, merge version into scenario so that we can compare versions
if(length(unique(sims.df$model_version))>1 & simOpts$compare.version == T){
  sims.df <- sims.df |>
    mutate(Mgt_scenario = paste(Mgt_scenario, model_version))
}

rmarkdown::render(input = "./R_Scripts/Post_process/Scenario_comparison.Rmd", 
                  output_format = "html_document",
                  output_file = file.path(dirToProcess, "Scenario_comparisons.html"), 
                  params = list(
                    scenario_column = "Mgt_scenario",
                    scenario_column2 = "Climate",
                    reference_scenario1 = "BAU wildfire",
                    reference_scenario2 = "Base climate"
                    )
                  )
