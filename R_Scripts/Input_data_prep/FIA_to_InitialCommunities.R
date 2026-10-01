########################################################################################################################-
########################################################################################################################-
########################################################################################################################-
###                   GENERATE INITIAL COMMUNITY INPUT MAP AND COHORT TREE LISTS                                  ######-
###                              USDA Forest Service PNWRS LANDIS-II MODEL                                        ######-
#-----------------------------------------------------------------------------------------------------------------------#
###   EXTENT: Wenatchee, Entiat, Okanogan, and Methow sub-basins                                                  ######-
###   PROJECT: BIOMASS Phase 3 for Pacific Northwest Research Station                                             ######-    
###   DATE: Nov. 2021 revised June 2023                                                                           ######-   
#-----------------------------------------------------------------------------------------------------------------------#
###   Code developed by Tucker Furniss                                                                            ######-
###     Contact: tucker.furniss@usda.gov; tucker.furniss@gmail.com;                                               ######-
###   Adapted from code written by Charles Maxwell, edited by Vivian Griffey                                      ######-
###   Edited further by Thomas Estabrook                                                                          ######-
#-----------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------#
###   This script takes the Riley raster, Riley tree list, and FIA tree lists as inputs, and will output the 
#       InitialCommunity.tif map and InitialCommunity.txt file required for LANDIS-II NECN and BIOMASS extensions.
#     The FIA tables are required because the Riley tree list associated with the Riley raster does not contain
#       tree ages or biomass, both of which are required for LANDIS. This script will create a DBH ~ AGE model to
#       estimate age for trees that were not cored (only some trees are cored in each FIA plot), ensure each tree
#       has an age, group by age and summarize the species, ages, and biomass present in each pixel.
#-----------------------------------------------------------------------------------------------------------------------
########################################################################################################################
#-----------------------------------------------------------------------------------------------------------------------
### Load packages: ----
library(tidyverse)
# library(dplyr)
# library(purrr)
# library(tidyr)
# library(broom)
# library(raster)
# library(sf)
# library(akima)
library(readxl)
library(colorspace)
library(terra)
library(tidyterra)

Sys.setenv(TMPDIR = "F:/R_TEMP")
terraOptions(tempdir = "F:/R_TEMP")
#-----------------------------------------------------------------------------------------------------------------------
### Set data directory: ----
# Dir<-'C:/Users/tuckf/Tuckers_Data/R/ORISE_R/LANDIS_R' # Location of R script
Dir <- 'F:/LANDIS_Input_Data_Prep'

# bigDataDir<-'D:/Data' # Location of WA_FIA data and Riley dataset (RDS-2019-0026)
bigDataDir <- 'F:/LANDIS_Input_Data_Prep/BigData' # Location of WA_FIA data and Riley dataset (RDS-2019-0026)

# dataDir<-'C:/Users/tuckf/Tuckers_Data/R/ORISE_R/LANDIS_R/Data' # Location of species codes, PWG raster, study area raster
dataDir <- 'F:/LANDIS_Input_Data_Prep/Data' # Location of species codes, PWG raster, study area raster

### Set area of interest
LANDIS.EXTENT <- 'OkaMet'  # name for saving files, etc.
wdir <- file.path('F:/LANDIS_Input_Data_Prep', LANDIS.EXTENT)

# LANDIS.EXTENT <- 'Oka'
# wdir <- 'D:/LANDIS_Input_Data_Prep/Oka'

# LANDIS.EXTENT <- 'Met'
# wdir <- 'D:/LANDIS_Input_Data_Prep/Met'

# LANDIS.EXTENT <- 'WenEnt'
# wdir <- 'D:/LANDIS_Input_Data_Prep/WenEnt'
# LANDIS.EXTENT <- 'WenEnt_OkaMet'
# LANDIS.EXTENT <- 'Tripod'
# wdir <- 'F:/LANDIS_Input_Data_Prep/Tripod_test_output'

## Function to roundup ages to nearest X: ----
roundUp <- function(x,to=10){
  to*(x%/%to + as.logical(x%%to))
}

#-----------------------------------------------------------------------------------------------------------------------
########################################################################################################################
#-----------------------------------------------------------------------------------------------------------------------
### Load data: ----
### Load Pathway Group raster. This will become the LANDIS ecoregion map: ----
ecos.r <- rast(file.path(dataDir,'PWG',paste0("PWG_",LANDIS.EXTENT,".tif")))  ## Ecoregion map

### Load LUA codes: ----
lua.r <- rast(file.path(dataDir,paste0("PWG/LUA_",LANDIS.EXTENT,".tif"))) 
lua.r[ecos.r==0|is.na(ecos.r)]<-0

### Species code crosswalk (FIA, FVS, and LANDIS species codes): ----
species.codes <- read.csv(file.path(dataDir,"Species_code_crosswalk.csv"))
shrub.codes <- read_csv(file.path(dataDir,"Shrub_code_crosswalk.csv")) 

### grab area outline
aoi.sf <- vect(file.path(dataDir, paste0("Outline_", LANDIS.EXTENT, ".gpkg")))

### Longevity (overwrite default values with values in SPECIES_MASTER.XLSX file): ----
species.master <- read_excel(file.path(dataDir,"Species_master.xlsx"),skip=4)[,c('Spp','Name','Longevity')] |>
  filter(!is.na(Spp)) |>
  dplyr::select(Name,Longevity) 

species.codes <- species.master |>
  full_join(species.codes,by=c('Name'='SpecCode')) |>
  mutate(Longevity.x = as.numeric(Longevity.x),
         Longevity = coalesce(Longevity.x, Longevity.y)) |>
  rename('SpecCode' = 'Name') |>
  select(!c(Longevity.x, Longevity.y)) |>
  arrange(SpecCode)

## lookup table linking FIA species codes and names
fia.spp.codes <- read_csv(file.path(bigDataDir,"FIA","FIADB_REFERENCE/REF_SPECIES.csv"), guess_max = 100000) |>
  dplyr::rename(FIA_Sp_Num = SPCD) |>
  dplyr::rename(SPEC = SPECIES_SYMBOL) |>
  dplyr::select(FIA_Sp_Num, COMMON_NAME, GENUS, SPECIES, SPEC)

#-----------------------------------------------------------------------------------------------------------------------
### Riley tree list: ----
riley_tree2022.df <- read.csv(file.path(bigDataDir, "TreeMap2022", "TreeMap2022_CONUS_Tree_Table.csv"))

#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
### Load FIA tree data: ----
##   FIA data source: https://apps.fs.usda.gov/fia/datamart/CSV/datamart_csv.html
##  Load FIA data function: ---
loadFIA<-function(state,year = NA){
  fiadir<-file.path(bigDataDir,"FIA",paste0(state,'_FIA'))

  plots <- read_csv(file.path(fiadir,paste0(state,"_PLOT.csv")), guess_max = 1000) |>
    dplyr::select(CN,LAT,LON,ELEV) |>
    dplyr::rename(PLT_CN=CN) 
  
  plotsnap <- read_csv(file.path(fiadir,paste0(state,"_PLOTSNAP.csv")), guess_max = 1000) |>
    rename(PLT_CN = CN) |>
    select(PLT_CN, ECOSUBCD) |>
    group_by(PLT_CN) |>
    slice_head(n=1)
  
  plots <- plots |>
    left_join(plotsnap) |>
    filter(!is.na(ECOSUBCD))
  
  trees_full <- read_csv(file.path(fiadir,paste0(state,"_TREE.csv")), guess_max = 3000) 
  trees_full <- trees_full |>
    dplyr::select(CN, PLT_CN, INVYR, SUBP, TREE, STATUSCD, SPCD, BHAGE, TOTAGE, DIA, HT, ACTUALHT, CPOSCD, CLIGHTCD, CARBON_AG, TPA_UNADJ) |>
    dplyr::rename(TRE_CN=CN) |>
    dplyr::rename(FIA_Sp_Num = SPCD) |>
    filter(!is.na(TRE_CN)) |>
    filter(!is.na(DIA)) |>
    right_join(plots,by="PLT_CN") |>
    #  left_join(biomass,by="TRE_CN") |>
    # left_join(plp,by="PLT_CN") |>
    left_join(fia.spp.codes,by=c("FIA_Sp_Num")) |>
    filter(!is.na(TRE_CN)) |>
    left_join(species.codes, by = c("SPEC" = "VEG_SPCD")) |>
    #  filter(!is.na(SpecCode)) |>
    # filter(STATUSCD == 1) |> # Select live trees only. See page ~72 in FIA database user guide. 
    mutate(SPEC = as.factor(SPEC))
  
  # Fill in missing TPA and Carbon values with minimum values:
  trees_full[trees_full$CARBON_AG==0|is.na(trees_full$CARBON_AG),'CARBON_AG']<-0.81
  trees_full[trees_full$TPA_UNADJ==0|is.na(trees_full$TPA_UNADJ),'TPA_UNADJ']<-0.99
  
  # Add biomass field
  trees_full <-  trees_full |>
    mutate(AG_biomass_gm2 = round(CARBON_AG * 2 * TPA_UNADJ * 0.1121,0)) # Multiply carbon by 2 to approximate biomass weight. Units are g / m2.
  
  if(!is.na(year))
    trees_full <- trees_full |> filter(INVYR > year)
  
  trees_full$STATE<-state
  
  return(trees_full)
}


## Load data for plots from ID, MT, and OR: ----
wa_trees<-loadFIA('WA',2000)
id_trees<-loadFIA('ID',2000)
or_trees<-loadFIA('OR',2000)
mt_trees<-loadFIA('MT',2000)


full_fia.df<-rbind(wa_trees,or_trees,mt_trees,id_trees)

#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
### Load Riley raster (circa 2016). This will be used as the initial communities map: ----
### If necessary, crop the full, original Riley raster to create TreeMap2016_WenEntOkaMet.tif: --
if(!file.exists(file.path(dataDir,paste0("TreeMap2022_",LANDIS.EXTENT,"_30m.tif")))){
  if(askYesNo('Riley raster for the study area not found. Recreate from full Riley dataset?')){
    Riley_raster_full <- rast(file.path(bigDataDir,"TreeMap2022","TreeMap2022_CONUS.tif"))  ## National Riley tree list
    
    ## This is much faster using terra package.
    temp<-project(ecos.r,crs(Riley_raster_full),method='near') # Create temp raster for croping full Riley raster
    Riley_raster<-terra::crop(Riley_raster_full,temp) |> # crop full Riley raster
      project(crs(ecos.r), method = 'near') |> # Project cropped Riley raster -- do not use ecos as template because we want to resample after we eliminate out-of-state plots.
      # resample(ecos.r, method = "near") |>
      crop(ext(ecos.r)) |> # Crop again to study area
      droplevels()  # eliminate unused raster attribute categories
    
    # Riley_raster <- ifel(is.na(ecos.r), NA, Riley_raster)
    Riley_raster <- mask(Riley_raster, aoi.sf)

    plot(ecos.r,col='black',legend=F,xaxt='n',yaxt='n')
    plot(Riley_raster,add=T)
    
    writeRaster(Riley_raster,file.path(dataDir,paste0("TreeMap2022_",LANDIS.EXTENT,"_30m.tif")),overwrite=T)
    rm(Riley_raster_full, temp)
  }
} else Riley_raster <- rast(file.path(dataDir,paste0("TreeMap2022_",LANDIS.EXTENT,"_30m.tif")))  ##Riley tree list, trimmed to study area

par(mfrow=c(1,1))
plot(ecos.r,col='black',legend=F,xaxt='n',yaxt='n')
plot(Riley_raster,add=T)

#-----------------------------------------------------------------------------------------------------------------------
## Record unique plot CN codes for each plot in the Riley raster: ----
riley_tree.df_aoi<-riley_tree2022.df |>
  filter(TM_ID %in% unique(cats(Riley_raster)[[1]]$TM_ID))
head(riley_tree.df_aoi)

#-----------------------------------------------------------------------------------------------------------------------
### Subset riley tree list for plots in the PNW and trees with valid DBH: ----
## NOTE: State ID is not found in the tree list associated with TreeMap circa 2016. Use FIA database and TreeMap circa 2014 to fill in state ID:
##  Extract state id from fia database:
plot_state.df<-full_fia.df |> select(PLT_CN,STATE,INVYR) |> unique()

head(plot_state.df)


## Add state column to Riley Tree List: ----
riley_tree.df_aoi <- riley_tree.df_aoi |> left_join(plot_state.df, by = "PLT_CN") |>
  mutate(STATE = replace_na(STATE, "z-OTHER"))

plot_state.df <- riley_tree.df_aoi |>
  select("TM_ID", "PLT_CN", "STATE", "INVYR") |> unique()
  


### How many of the plots in the Riley tree list are from each year? 
# Full FIA dataset has plots dating back to 2001, but the Riley tree list only uses plots from 2006-2016.
plot_year_summary.df <- riley_tree.df_aoi |> 
  group_by(INVYR) |> tally()

### How many of the plots in the Riley tree list are from each state? 
plot_state_summary.df <- riley_tree.df_aoi |>
  group_by(STATE) |> tally()

# out of region plots:
t<-riley_tree.df_aoi |> filter(!STATE %in% c('WA','ID','OR','MT'))

# Percent of total 
length(unique(t$PLT_CN)) / length(unique(riley_tree.df_aoi$PLT_CN)) # ~15% for Wen, 18% for OkaMet, 21% for Tripod!
# View the species composition in these plots:
t[sample(1:nrow(t),100),]


#-----------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------------
### Replace foreign pixels with nearby pixels that correspond to FIA plots that are in WA, OR, ID, or MT: ----
##  If this was done previously, load saved raster: 
if(file.exists(file.path(dataDir,paste0("TreeMap2022_NoExotics_",LANDIS.EXTENT,"_90m.tif")))){
  Riley_raster <- rast(file.path(dataDir,paste0("TreeMap2022_NoExotics_",LANDIS.EXTENT,"_90m.tif")))  ##Riley tree list, trimmed to study area and foreign pixels replaced.
} else {
  warning('Modified TreeMap 2022 not found. The following code will recreate the modified TreeMap from the raw TreeMap circa 2022, cropped to study domain.')
  
  # temp <- unique(values(Riley_raster))
  # temp <- riley_tree.df_aoi[riley_tree.df_aoi$tm_id %in% temp,]
  # source_states<-setNames(aggregate(temp$CN,by=list(temp$STATE),FUN=function(x) length(unique(x))),c('State','N.plots'))
  # source_states
  
  
  
  ##  See how these foreign plots are distributed across the landscape:
  # out.of.state.tm_id<-riley_tree.df_WenEntOkaMet[!riley_tree.df_WenEntOkaMet$STATE=='WA','tm_id']
  # out.of.pnw.tm_id<-riley_tree.df_WenEntOkaMet[!riley_tree.df_WenEntOkaMet$STATE %in% c('WA','OR','ID','MT'),'tm_id']
  # foreign1<-Riley_raster
  # foreign1[foreign1 %in% out.of.state.tm_id]<-(-9999)
  # foreign1[foreign1>=0]<-NA
  # plot(foreign1,col='orchid4',legend=F)
  
  # Read in the full Riley tree list to see where they're from:
  # plot_state.sum.df<-setNames(aggregate(plot_state.df$tm_id,by=list(plot_state.df$STATE),FUN=NROW),c('State','N.plots'))
  # plot_state.sum.df$Num<-1:nrow(plot_state.sum.df)
  # plot_state.sum.df
  
  ### Visualize origin states of riley plots
  origin.r <- Riley_raster
  
  newcat <- cats(origin.r)[[1]] |> left_join(plot_state.df) |> 
    mutate(Origin = ifelse(STATE%in%c('OR','ID','MT'), "PNW", ifelse(STATE == "WA", "WA", "Foreign"))) |> select("TM_ID", "Origin")
  
  origin.r <- origin.r |> addCats(newcat)
  activeCat(origin.r) <- "Origin"
  
  plot(origin.r)
  
  #------------------------------------------------------------------------------------------------------------------------#
  ## Replace foreign pixels with nearby pixels that correspond to FIA plots that are in WA, OR, ID, or MT:
  
  exotics <- plot_state.df |> filter(STATE == "z-OTHER") |> select("TM_ID") |> pull() 
  
  activeCat(Riley_raster) <- "TM_ID"
  
  no_exotic.r <- ifel(Riley_raster > 0 , as.numeric(Riley_raster), NA)
  no_exotic.r <- ifel(is.na(no_exotic.r), 0, no_exotic.r)
  no_exotic.r <- ifel(Riley_raster %in% exotics, NA, no_exotic.r)
  plot(no_exotic.r)
  
  # levels(no_exotic.r) <- bind_rows(levels(no_exotic.r), data.frame("Value" = 0, "TM_ID" = 0))
  
  focal_fun <- function(m) {
    m[m==0] <- NA
    m <- m[!is.na(m)]
    if(is.null(m)) {return(NA)}
    
    # return(raster::modal(m, na.rm = T))
    um <- unique(m)
    um[which.max(tabulate(match(m, um)))]
  }
  
  empties <- sum(values(is.na(no_exotic.r))) +1
  win = 5
  
  while (sum(values(is.na(no_exotic.r))) > 300 & win < 16) {
    if (sum(values(is.na(no_exotic.r))) == empties) {win <- win + 2; print("Widening focal window.")}  # widen window if it stops shrinking
  
    
    empties <- sum(values(is.na(no_exotic.r)))
    print(paste(empties, "empty pixels remaining."))
    
    no_exotic.r <- terra::focal(no_exotic.r, w=win, fun = focal_fun, na.policy = "only")
    plot(no_exotic.r)
  }
  
  no_exotic.r <- ifel(is.na(no_exotic.r), 0, no_exotic.r)
  
  ### Resample TreeMap to 90-m: ----
  Riley_no_exotics_90m<-project(no_exotic.r,ecos.r,method='near')
  Riley_no_exotics_90m <- ifel(is.na(ecos.r), 0, Riley_no_exotics_90m)
  
  ## Save modified Riley raster: 
  writeRaster(no_exotic.r,file.path(dataDir,paste0("TreeMap2022_NoExotics_",LANDIS.EXTENT,"_30m.tif")))#,overwrite=T)
  writeRaster(Riley_no_exotics_90m,file.path(dataDir,paste0("TreeMap2022_NoExotics_",LANDIS.EXTENT,"_90m.tif")),overwrite=T)
  
  Riley_raster <- Riley_no_exotics_90m
  
}

### pare down treelist to just those in the modified map
riley_tree.df <- riley_tree.df_aoi |> filter(TM_ID %in% values(Riley_raster))

### are any plots in the riley tree list but not in the FIA database

riley_tree.df |> filter(!PLT_CN%in%full_fia.df$PLT_CN)

### Drop trees from Riley tree list without DBH. These are probably seedlings?

riley_tree.df |> filter(is.na(DIA)|DIA == 0)

##   BHAGE is breast height age
##   TOTAGE is Total age. The age of a live tree derived either from counting tree rings from an increment core sample extracted 
#     at the base of a tree where diameter is measured at root collar (DRC), or for small saplings (1.0 to 2.9 inches diameter 
#     at breast height) by counting all branch whorls, or by adding a species-dependent number of years to breast height age.
## It appears that for conifers, 8 years is the mean difference between BHAGE and TOTAGE. Use this for the post-2005 plots.
## Create merged AGE column to reflect either TOTAGE or BHAGE adjusted for breast height:
full_fia.df <- full_fia.df |>
  mutate(BHAGE_adj = BHAGE + 8) |>
  mutate(AGE = coalesce(TOTAGE, BHAGE_adj)) |>
  # mutate(AGE_circa_2020 = AGE + (2020 - INVYR)) |>
  mutate(PLT_CN = as.factor(PLT_CN))


#-----------------------------------------------------------------------------------------------------------------------
# ### Adjust age for 2020: ----
# ##   BHAGE is breast height age
# ##   TOTAGE is Total age. The age of a live tree derived either from counting tree rings from an increment core sample extracted 
# #     at the base of a tree where diameter is measured at root collar (DRC), or for small saplings (1.0 to 2.9 inches diameter 
# #     at breast height) by counting all branch whorls, or by adding a species-dependent number of years to breast height age.
# cat('Pre-2005 tree list:',nrow(wa_trees.pre.2005),'trees (total),',nrow(wa_trees.pre.2005[!is.na(wa_trees.pre.2005$BHAGE),]),'with ages')
# cat('Post-2005 tree list:',nrow(wa_trees),'trees (total),',nrow(wa_trees[!is.na(wa_trees$BHAGE),]),'with ages')
# 
# ## For whatever reason, pre-2005 trees have both BHAGE and TOTAGE, but post-2005 trees only have BHAGE:
# summary(wa_trees[,c('BHAGE','TOTAGE')])
# summary(wa_trees[!is.na(wa_trees$BHAGE),'INVYR'])
# summary(wa_trees[!is.na(wa_trees$TOTAGE),'INVYR'])
# 
# wa_trees[!is.na(wa_trees$BHAGE)&!is.na(wa_trees$TOTAGE),] # Trees that have both BHAGE and TOTAGE post 2005
# wa_trees.pre.2005[!is.na(wa_trees.pre.2005$BHAGE)&!is.na(wa_trees.pre.2005$TOTAGE),] # Many trees from pre-2005 have both BHAGE and TOTAGE
# 
# t<-aggregate(wa_trees.pre.2005$TOTAGE-wa_trees.pre.2005$BHAGE,by=list(wa_trees.pre.2005$SPEC),FUN=function(x) 
#   return(c('Min'=round(min(x,na.rm=T),0),'Max'=round(max(x,na.rm=T),0),'Mean'=round(mean(x,na.rm=T),0))))
# t<-t[!is.na(t$x[,3]),]
# t
# 
# ## It appears that for conifers, 8 years is the mean difference between BHAGE and TOTAGE. Use this for the post-2005 plots.
# ## Create merged AGE column to reflect either TOTAGE or BHAGE adjusted for breast height:
# full_fia.df <- full_fia.df |>
#   mutate(BHAGE_adj = BHAGE + 8) |>
#   mutate(AGE = coalesce(TOTAGE, BHAGE_adj)) |>
#   mutate(AGE_circa_2020 = AGE + (2020 - INVYR)) |>
#   mutate(PLT_CN = as.factor(PLT_CN))
# head(cbind(full_fia.df[!is.na(full_fia.df$AGE),c('BHAGE','TOTAGE','AGE','AGE_circa_2020','INVYR')]),20)

#-----------------------------------------------------------------------------------------------------------------------
### Add PWG to full_fia.df: ----

## Create pwg lookup table:
pwg_cn.lookup<-as.data.frame(c(ecos.r, Riley_raster))
colnames(pwg_cn.lookup) <- c('PWG', 'TM_ID')
pwg_cn.lookup <- pwg_cn.lookup |>
  group_by(PWG, TM_ID) |> tally()|>
  group_by(TM_ID) |>
  slice_max(n, with_ties = F, n = 1) |> ungroup() |> unique() # Select most common pwg per CN


pwg_cn.lookup




## Create ECOSUBCD lookup table:
# ecosubcd_cn.lookup<-full_fia.df |>
#   select(ECOSUBCD, PLT_CN) |> unique() |>
#   mutate(PLT_CN = as.factor(PLT_CN),
#          ECOSUBCD.num = as.numeric(factor(ECOSUBCD)))
#   

 

#ecosubcd_cn.lookup$ECOSUBCD.num<-as.numeric(factor(ecosubcd_cn.lookup$ECOSUBCD))

## Merge to plot_state lookup table:
# plot_state.df <- unique(riley_tree.df[,c('CN','STATE','INVYR')]) |> mutate(CN = as.factor(CN)) |>
#   rename(PLT_CN = CN) 
# 
# plot_state.df <- plot_state.df |>
#   left_join(pwg_cn.lookup, by = c('PLT_CN' = 'CN')) |>
#   left_join(ecosubcd_cn.lookup, by = 'PLT_CN') 

## Merge PWG to fia_tree.df
# full_fia.df <- full_fia.df |> left_join(plot_state.df[,c('PLT_CN','PWG')], by = 'PLT_CN') |> unique()

#full_fia.df<-unique(merge(full_fia.df,plot_state.df[,c('PLT_CN','PWG')],by='PLT_CN',all.x=T))  

## Reclass Riley_raster to produce map of FIA ECOSUBCD:
# r<-classify(Riley_raster,rcl=plot_state.df[,c('PLT_CN','ECOSUBCD.num')])
# r <- subst(Riley_raster, from = plot_state.df$PLT_CN, to = plot_state.df$ECOSUBCD.num)
# 
# par(mfrow=c(1,2),oma=rep(0,4),mar=rep(0,4))
# plot(r)
# plot(ecos.r)

#-----------------------------------------------------------------------------------------------------------------------
## Evaluate the relative abundance of FIA plots in the study domain: ---- 
t2<- as.data.frame(Riley_raster) |> filter(!is.na(TM_ID)) |> 
  group_by(TM_ID) |> summarise(count = n()) |> ungroup() |>
  mutate(percent = count / sum(count)) |>
  arrange(-count)
# t2<-aggregate(t,by=list(t),FUN=NROW)
# t2<-t2[order(t2$x,decreasing=TRUE),]
# t2$percent<-t2$x / sum(t2$x)

cat('* There are ',nrow(t2),' unique FIA plots in the Riley raster for the study domain.\n',sep='')
cat('* The most abundant 10 plots (',round(10/nrow(t2)*100,1),'% of all plots) comprise ',round(sum(t2[1:10,'percent'])*100,0),'% of the pixels on the landscape.\n',sep='')
cat('* The most abundant 1% of plots comprise ',round(sum(t2[1:round(nrow(t2)/100),'percent'])*100,0),'% of the pixels on the landscape.\n',sep='')
cat('* The most abundant 100 plots (',round(100/nrow(t2)*100,1),'% of all plots) comprise ',round(sum(t2[1:100,'percent'])*100,0),'% of the pixels on the landscape.\n',sep='')
cat('* The most abundant 1000 plots (',round(1000/nrow(t2)*100,1),'% of all plots) comprise ',round(sum(t2[1:1000,'percent'])*100,0),'% of the pixels on the landscape.\n',sep='')


#-----------------------------------------------------------------------------------------------------------------------
########################################################################################################################
########################################################################################################################
### WRITE LANDIS-II INITIAL COMMUNITY INPUT MAPS: ----

## INITIAL COMMUNITY RASTER: ----
## For pixels that Riley_raster is NA (or 0) but ecos.r indicates the cell should be active (not 0, water, or bareground), assign code 99.
# Code 99 will be assigned to an initial community of grass/forbs. Without this step, LANDIS-II has a big increase in vegetation at the first time step.
# Most of these pixels are grasslands (pwg=12) or alpine meadows (pwg=15):
Riley_raster <- ifel(Riley_raster == 0 & (ecos.r > 11) & !is.na(ecos.r), 99, Riley_raster)

## Write raster:
writeRaster(Riley_raster,file.path(wdir,paste0("INITIAL_COMMUNITIES_",LANDIS.EXTENT,".tif")), datatype = "INT4S", overwrite=T)
writeRaster(Riley_raster,file.path(LANDIS.EXTENT,paste0("INITIAL_COMMUNITIES_2022_",LANDIS.EXTENT,".tif")), datatype = "INT4S", overwrite=T)

########################################################################################################################
########################################################################################################################
#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
### Impute missing age data: ----


##  Charles's method: estimate age using log(diameter) and species  
#m0 <- lm(log(AGE_circa_2020) ~ log(DIA) * FIA_Sp_Num , data = full_fia.df, na.action = na.exclude) # does a terrible job. Well yeah, no shit. You're using species code as a numeric predictor??? If we're going to use this method, we should use a glmm and species should be a categorical random effect. 
#summary(m0)
# actual <- full_fia.df$AGE_circa_2020[!is.na(full_fia.df$AGE_circa_2020)]
# plot(actual, exp(m0$fitted.values))

## A more appropriate model? Let's use DBH & elevation, with species as a blocking factor: ----
# m2 <- lmer(AGE_circa_2020 ~ poly(DIA,2,raw=T) + (1|FIA_Sp_Num), data = full_fia.df, na.action = na.exclude)
# summary(m2)

# Simplification using lm:
# m2 <- lm(AGE_circa_2020 ~ poly(DIA,2,raw=T)  + ELEV * LAT + factor(ECOSUBCD) + factor(SPEC), data = full_fia.df, na.action = na.exclude) # Full model with elevation, latitude, ecoregion, and species
# summary(m2)

# m2 <- lm(AGE_circa_2020 ~ poly(DIA,2,raw=T)  + factor(ECOSUBCD) + factor(SPEC), data = full_fia.df, na.action = na.exclude) # Use ecoregion as a proxy for elevation, having elevation and latitude in the model results with negative ages for trees at low elevations and low latitudes!
# summary(m2)
# 
# # Use PWG rather than ECOSUBCD
# m2 <- lm(AGE_circa_2020 ~ poly(DIA,2,raw=T)  + factor(PWG) + factor(SPEC), data = full_fia.df, na.action = na.exclude) 
# summary(m2) 
# 
# # It helps a bit to use crown position as well! But, this measurement only exists for 15,000 / 960,000 rows. Ditch it.
# m2 <- lm(AGE_circa_2020 ~ poly(DIA,2,raw=T)  + CLIGHTCD + factor(ECOSUBCD) + factor(SPEC), data = full_fia.df, na.action = na.exclude) 
# summary(m2)
# 
# Back to using ECOSUBCD:
m2 <- lm(AGE ~ poly(DIA,2,raw=T) + factor(ECOSUBCD) + factor(SPEC), data = full_fia.df, na.action = na.exclude)
summary(m2)


unique(full_fia.df$ECOSUBCD)
## View model response: ----
elevs<-c(2000,4000,6000)
# sp='PIPO'
temp<-c()
pred<-c()
for(sp in c('PIPO','PSME','TSHE','ABAM','THPL','PICO')){
  temp<-rbind(temp,full_fia.df[full_fia.df$SPEC==sp&!is.na(full_fia.df$AGE),])
  pred<-rbind(pred,
              data.frame('SPEC'=sp,'DIA'=0:100,'ELEV'=elevs[1],'LAT'=47,'ECOSUBCD'='M242De','CLIGHTCD'=2),
              data.frame('SPEC'=sp,'DIA'=0:100,'ELEV'=elevs[2],'LAT'=47,'ECOSUBCD'='M242De','CLIGHTCD'=2),
              data.frame('SPEC'=sp,'DIA'=0:100,'ELEV'=elevs[3],'LAT'=47,'ECOSUBCD'='M242De','CLIGHTCD'=2))
  pred<-rbind(pred,
              data.frame('SPEC'=sp,'DIA'=0:100,'ELEV'=elevs[1],'LAT'=47,'ECOSUBCD'='M242Db','CLIGHTCD'=2),
              data.frame('SPEC'=sp,'DIA'=0:100,'ELEV'=elevs[1],'LAT'=47,'ECOSUBCD'='242Ae','CLIGHTCD'=2),
              data.frame('SPEC'=sp,'DIA'=0:100,'ELEV'=elevs[1],'LAT'=47,'ECOSUBCD'='M332Gl','CLIGHTCD'=2))
}
pred$AGE<-predict(m2,newdata=pred)

ggplot(temp,aes(x=DIA,y=AGE,colour=LAT,fill=ELEV)) + theme_classic() + geom_point(shape=21,alpha=0.4,colour='black') + 
  facet_wrap(~SPEC) + 
  # scale_color_gradient2(low='gold',mid='darkgreen',high='orchid4',midpoint=47.2,name='Latitude') +
  scale_fill_gradient2(low='darkred',mid='gold',high='skyblue',midpoint=3000,name='Elevation') + 
  geom_line(data=pred[pred$ELEV==elevs[1]&pred$ECOSUBCD=='M242De',],colour='darkred',linewidth=0.5) +
  geom_line(data=pred[pred$ELEV==elevs[2]&pred$ECOSUBCD=='M242De',],colour='gold',linewidth=0.5) +
  geom_line(data=pred[pred$ELEV==elevs[3]&pred$ECOSUBCD=='M242De',],colour='skyblue',linewidth=0.5) +
  geom_line(data=pred[pred$ECOSUBCD=='M242Db',],colour='black',linetype='dashed',linewidth=0.5) +
  geom_line(data=pred[pred$ECOSUBCD=='242Ae',],colour='black',linetype='dotted',linewidth=0.5) +
  geom_line(data=pred[pred$ECOSUBCD=='M332Gl',],colour='black',linetype='dotdash',linewidth=0.5) +
  xlim(0,80)+ylim(0,1000)+xlab('Diameter (inches)')+ylab('Age (years)') +
  theme(strip.background = element_blank(),strip.text = element_text(face='bold'), panel.border = element_rect(fill=NA),
        legend.key.width = unit(0.5,'cm'),legend.text = element_text(size=8))
dev.print(tiff,file=file.path(wdir,'dbh_age_regression_by_sp_2022.tiff'),width=6.5,height=5,res=600,units='in',compression='lzw')

pred2<-pred[pred$ELEV==elevs[1],]
pred2$AGE<-predict(m2,newdata=pred2)

ggplot(temp,aes(x=DIA,y=AGE,colour=SPEC,fill=SPEC)) + theme_classic() + 
  geom_point(alpha=0.5,shape=21,colour='black') + 
  scale_color_brewer(palette='Dark2',name='Species',guide='none') + 
  scale_fill_brewer(palette='Dark2',name='Species') + 
  geom_line(data=pred2,size=0.75,aes(linetype=ECOSUBCD)) +
  xlim(0,100)+ylim(0,1200)+xlab('Diameter (inches)')+ylab('Age (years)') +
  guides(fill=guide_legend(override.aes=list(alpha=1,linewidth=3)))+
  theme(strip.background = element_blank(),strip.text = element_text(face='bold'), panel.border = element_rect(fill=NA),
        legend.key.width = unit(0.5,'cm'),legend.text = element_text(size=8))
dev.print(tiff,file=file.path(wdir,'dbh_age_regression_all_sp_2022.tiff'),width=6.5,height=5,res=600,units='in',compression='lzw')

#-----------------------------------------------------------------------------------------------------------------------
### Group some species: ----
#     No ages exist for the following species: 2TB, 2TE, ACGL, ARME, CELE3, MAFU, PRAV, PRVI, ROPS, SAAL2
#     so we can't estimate their age using DBH. Drop or group all of these, or it will break the predict() function.

spp.grouper<-function(df,spp,new.spp,columns=c('FIA_Sp_Num','SPEC','SpecCode','Longevity')){
  if(NA%in%df[df$SPEC==new.spp,columns][1,]) stop('No species code "',new.spp,'" in data frame!')
  df[df$SPEC==spp,columns]<-df[df$SPEC==new.spp,columns][1,]
  return(df)
}
full_fia.df_raw_spp<-full_fia.df
full_fia.df <- full_fia.df |>
  spp.grouper(spp='ABCO',new.spp='ABGR') |>
  spp.grouper(spp='ACGL',new.spp='ACMA3') |>
  spp.grouper(spp='ARME',new.spp='ACMA3') |>
  spp.grouper(spp='ABSH',new.spp='ABPR') |>
  spp.grouper(spp='ALRH2',new.spp='ALRU2') |>
  spp.grouper(spp='CADE27',new.spp='CHNO') |>
  spp.grouper(spp='CHLA',new.spp='THPL') |>
  spp.grouper(spp='MAFU',new.spp='PREM') |>
  spp.grouper(spp='PRVI',new.spp='PREM') |>
  spp.grouper(spp='PRAV',new.spp='PREM') |>
  spp.grouper(spp='PRVI',new.spp='PREM') |>
  spp.grouper(spp='PRUNU',new.spp='PREM') |>
  spp.grouper(spp='UMCA',new.spp='PREM') |>
  spp.grouper(spp='2TE',new.spp='PSME') |>
  spp.grouper(spp='QUKE',new.spp='QUGA4') |>
  spp.grouper(spp='QUCH2',new.spp='QUGA4') |>
  spp.grouper(spp='LIDE3',new.spp='QUGA4') |>
  spp.grouper(spp='PIFL2',new.spp='PIAL') |>
  spp.grouper(spp='PIJE',new.spp='PIPO') |>
  spp.grouper(spp='PILA',new.spp='PIMO3') |>
  spp.grouper(spp='PISI',new.spp='PIEN') |>
  spp.grouper(spp='CELE3',new.spp='PREM') |>
  spp.grouper(spp='CHCHC4',new.spp='PREM') |>
  spp.grouper(spp='PIAT',new.spp='PICO') |>
  spp.grouper(spp='JUOS',new.spp='JUOC') |>
  spp.grouper(spp='ABMA',new.spp='ABPR') |>
  spp.grouper(spp='SESE3',new.spp='THPL') |>
  spp.grouper(spp='PIBR',new.spp='PIEN') |>
  spp.grouper(spp='SEGI2',new.spp='ABAM') |>
  spp.grouper(spp='CHNO', new.spp='THPL') |>
  spp.grouper(spp='ACMA3', new.spp='POTR5') |>  # reclass hardwoods to POTR to later reclass to special hardwood funcgroup
  spp.grouper(spp='ALRU2', new.spp='POTR5') |>
  spp.grouper(spp='BEOC2', new.spp='POTR5') |>
  spp.grouper(spp='BEPA', new.spp='POTR5') |>
  spp.grouper(spp='CONU4', new.spp='POTR5') |>
  spp.grouper(spp='FRLA', new.spp='POTR5') |>
  spp.grouper(spp='POBAT', new.spp='POTR5') |>
  spp.grouper(spp='PREM', new.spp='POTR5') |>
  spp.grouper(spp='QUGA4', new.spp='POTR5') |>
  group_by(SPEC) |>
  filter(n() >= 50) |> ## Drop species with <50 individuals
  ungroup() |>
  filter(!is.na(SpecCode))

## Reclass ABGR to PSME for OkaMet area: ----
if(LANDIS.EXTENT %in% c('OkaMet', 'Tripod', 'Oka', 'Met')){
  summary(full_fia.df[full_fia.df$SpecCode == 'AbieGran',])
  
  full_fia.df <- full_fia.df |>
    spp.grouper(spp='ABGR',new.spp='PSME')
}

# full_fia.df_raw_spp$SPEC[!full_fia.df_raw_spp$SPEC%in%full_fia.df$SPEC] |> unique()


## Drop rows (2000 / 970,000) that don't have species (mostly random willow sp.):
### I have 1,199,000 rows at this point... maybe different FIADB downloads?
# full_fia.df <- full_fia.df[!is.na(full_fia.df$SpecCode),]

## Are there any remaining species that aren't in the master species table?: ----
if(nrow(full_fia.df[!full_fia.df$SpecCode %in% species.master$Name,])>0) {
  stop('Found species in full_fia.df that are not in master species table. Re-assign these species or include in master species table.')
  unique(full_fia.df[!full_fia.df$SpecCode %in% species.master$Name,c('SPEC','COMMON_NAME')]) # Problem species
}

#-----------------------------------------------------------------------------------------------------------------------
##  Split data frame into trees with ages and trees without: ----
full_fia.df <- full_fia.df |>
  mutate(ECOSUBCD = ECOSUBCD |> replace_values(
    "342Bi" ~ "342Bj"
  ))

trees_missing <- subset(full_fia.df, is.na(AGE))
trees_notmissing <- subset(full_fia.df, !is.na(AGE))

if (length(unique(trees_notmissing$ECOSUBCD)) != length(unique(full_fia.df$ECOSUBCD))){
  stop("MISSING FACTOR LEVEL, FIX MANUALLY")
}

### Create final model: ----
age.lm <- lm(AGE ~ poly(DIA,2,raw=T) + factor(ECOSUBCD) + factor(SPEC), data = full_fia.df, na.action = na.exclude) # Same as m2 model, above.


##  Predict age for missing trees: ----
trees_missing$AGE <- as.numeric(round(predict.lm(age.lm, trees_missing, type='response'),0)) # Estimate age with new model

# Note which trees ages were modeled, and which they were measured
trees_missing$AGE_METHOD<-'Modeled'
trees_notmissing$AGE_METHOD<-'Cored'

### Rejoin: ----
full_fia.df <- rbind(trees_missing, trees_notmissing) |>
  mutate(AGE = AGE |> replace_when(AGE < 5 ~ 0)) |>
  mutate(AGE = roundUp(AGE, to = 5))

##  This model occasionally predicts negative ages. Fix these: ----
# full_fia.df[full_fia.df$AGE_circa_2020<5,'AGE_circa_2020']<-0
##  Round ages up to match with SUCCESSION extension timestep (currently 5 years): ----
# full_fia.df$AGE_circa_2020 <- roundUp(full_fia.df$AGE_circa_2020,to = 5)

#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
########################################################################################################################
#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------

### Subset FIA db for plots in the study area: ----
### Drop dead trees from both data sets: ----
fia_trees.df <- full_fia.df |>
  filter(PLT_CN %in% riley_tree.df$PLT_CN, STATUSCD == 1)

riley_tree.df <- riley_tree.df |>
  filter(STATUSCD == 1) |>
  mutate(PLT_CN = as.factor(PLT_CN))

# fia_trees.df is FIA data for WA state (plus some FIA plots from ID, OR, and MT that were present in Riley dataset), modified by the above code.
# riley_tree.df is the tree table associated with the Riley dataset. This is based on FIA data, but isn't the complete FIA dataset.
full_fia.df[full_fia.df$STATUSCD==2,]

#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
### JOIN FIA tree data to spatial Riley tree data: ----
## Try out a join and look for missing plots: ----
# names(fia_trees.df)[names(fia_trees.df) == 'PLT_CN'] <- 'CN'
names(riley_tree.df)[names(riley_tree.df) == 'SPCD'] <- 'Orig_Sp_Num'

#   Join plots from Riley dataset and FIA that are in the study area. No plots should be missing.
#   Note - not joining by species because we grouped some species in the fia_trees.df dataframe, but the true species 
#        is still present in the Riley tree list. If we group by species, these will be left out. 
#        Let's just trust the join by plot, sub-plot, tree number, and DBH.
# a <- riley_tree.df |> select(CN, SUBP, TREE, DIA) |> mutate(src = 'RILEY') 
# nrow(unique(a))/nrow(a)
# b <- fia_trees.df |> select(CN, SUBP, TREE, DIA) |> mutate(src = 'FIA')
# nrow(unique(b))/nrow(b)
# t <- a |> left_join(b,by = c("CN", "SUBP", "TREE", "DIA"))
# t2 <- b |> left_join(a, by = c("CN", "SUBP", "TREE", "DIA"))

nrow(unique(riley_tree.df))/nrow(riley_tree.df)  # ok, still 1

# riley_tree.df <- riley_tree.df |>
#   group_by(CN, SUBP, TREE, DIA) |>
#   slice_max(INVYR, n=1) |> ungroup()

t <- riley_tree.df |> left_join(fia_trees.df, by = c("PLT_CN", "SUBP", "TREE", "DIA")) 
t2 <- fia_trees.df |> left_join(riley_tree.df, by = c("PLT_CN", "SUBP", "TREE", "DIA"))

fia.only<-t[is.na(t$PWG),]
riley.fia.only<-t[is.na(t$TPA_UNADJ.x),]

nrow(t) # trees full join
nrow(fia.only) # Trees among WA FIA that aren't in Riley FIA.
nrow(riley.fia.only) # Trees among Riley FIA that aren't in WA FIA. These are a problem.
#unique(formatC(riley.fia.only[,'CN'],digits=16))

if(nrow(riley.fia.only) > 0){stop("There's a tree in Riley not in WA FIA. Uncomment the code to investigate.")}
# A tree missing from FIA. Looks like it is missing because SPECIES is rare.
# riley.fia.only[riley.fia.only$CN==188765687020004,]
# riley.fia.only[riley.fia.only$CN==174763305020004,]
# riley_tree.df[riley_tree.df$CN==174763305020004&riley_tree.df$SUBP==1&riley_tree.df$TREE==103,c("CN", "SUBP", "TREE", "Orig_Sp_Num", "DIA", "INVYR")]
# data.frame(fia_trees.df[fia_trees.df$CN==174763305020004&fia_trees.df$SUBP==1&fia_trees.df$TREE==103,c("CN", "SUBP","TREE","SpecCode", "FIA_Sp_Num", "DIA", "INVYR")])
# fia.spp.codes[fia.spp.codes$FIA_Sp_Num%in%riley.fia.only$Orig_Sp_Num,]
# nrow(riley.fia.only) # So, we're missing ~100 trees that are rare species, and a few trees with corrected DBHs. Not a big deal, the full merged dataset is 104345 trees.
# nrow(t) 


######################################################-
#-----------------------------------------------------------------------------------------------------------------------

### JOIN FIA tree data to spatial Riley tree data: ----
landis_treelist.df <- riley_tree.df |> 
  inner_join(fia_trees.df, by = c("PLT_CN", "SUBP", "TREE", "DIA")) |> # Join Riley tree list and full FIA dataset
  dplyr::select(TM_ID, PLT_CN, SUBP, TREE, BHAGE,AGE,DIA,HT.y,ACTUALHT.y,CARBON_AG,
                AG_biomass_gm2,TPA_UNADJ.y,ELEV,SpecCode,LAT,LON,SpeciesLatin,Longevity,STATE.y,INVYR.x,AGE_METHOD) |>
  rename(INVYR = INVYR.x) |> rename(TPA_UNADJ = TPA_UNADJ.y) |>
  rename(ACTUALHT = ACTUALHT.y) |> rename(HT = HT.y) |>
  rename(STATE = STATE.y) 

if(nrow(t) - nrow(landis_treelist.df) > 1000) stop('Somehow we lost more than 1000 trees when joining Riley tree list with FIA. This might indicate a big problem.')

# Use an inner_join to drop ~100 trees that aren't in the fia_trees.df data frame,
#  mostly because they are rare out-of-state species and were dropped from fia_trees
#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
### Load FIA shrub data: ----
loadFIA.shrub<-function(state){
  dir<-file.path(bigDataDir,"FIA",paste0(state,'_FIA'))
  
  shrubs_raw <- read_csv(file.path(dir,paste0(state,"_P2VEG_SUBPLOT_SPP.csv",sep=""))) 
  
  shrubs <- shrubs_raw |>
    filter(PLT_CN %in% landis_treelist.df$PLT_CN) |> # Select only Riley CNs
    dplyr::select(CN,PLT_CN,SUBP,INVYR,CONDID,VEG_FLDSPCD,VEG_SPCD,GROWTH_HABIT_CD,LAYER,COVER_PCT) |>
    filter(GROWTH_HABIT_CD %in% c('SH','SS','GR','FB')) |> # Only shrubs and subshrubs, forbes and grasses
    left_join(shrub.codes, by = "VEG_SPCD") |>
    filter(!is.na(VEG_SPCD)) |>
    mutate(growth_layer = case_when(LAYER == 1 ~ 0.25,
                                    LAYER == 2 ~ 0.5,
                                    LAYER == 3 ~ 0.75,
                                    LAYER == 4 ~ 1)) |>
    mutate(biomass = growth_layer * COVER_PCT * 20) |>
    mutate(age = roundUp((COVER_PCT * 0.6))) |>
    mutate(STATE = state)
  
  return(shrubs)
}

shrubs <- rbind(loadFIA.shrub('WA'),
                loadFIA.shrub('ID'),
                loadFIA.shrub('OR'),
                loadFIA.shrub('MT')) |>
  mutate(PLT_CN = as.factor(PLT_CN)) |>
  mutate(Type = Type |> replace_when(
    GROWTH_HABIT_CD%in%c("GR", "FB") ~ "Grass_Forb",
    is.na(Type) ~ "NonFxr_Resprt"
  )) |>
  mutate( ## Reduce grass biomass because it's way too high.
    biomass = biomass |> replace_when(
      Type == "Grass_Forb" ~ biomass * 0.2
    ),
    age = age |> replace_when( ## Reduce grass age because it's way too high.
      Type == "Grass_Forb" ~ age * 0.1
    )
  )
 
# shrubs[shrubs$GROWTH_HABIT_CD%in%c('GR','FB'),'Type']<-'Grass_Forb'
# shrubs[is.na(shrubs$Type),'Type']<-'NonFxr_Resprt'
# shrubs$CN<-as.factor(shrubs$PLT_CN)

## Reduce grass biomass because it's way too high.
# shrubs[shrubs$Type=='Grass_Forb','biomass']<-shrubs[shrubs$Type=='Grass_Forb','biomass'] * 0.2
# 
# ## Reduce grass age because it's way too high.
# shrubs[shrubs$Type=='Grass_Forb','biomass']<-shrubs[shrubs$Type=='Grass_Forb','age'] * 0.1

# View estimated age and biomass
par(mfrow=c(1,2),oma=c(4,4,4,4))
plot(shrubs$COVER_PCT,shrubs$age,xlab='Cover (%)',ylab='Estimated age',pch=19)
plot(shrubs$COVER_PCT,shrubs$biomass,col=topo.colors(4)[shrubs$growth_layer*4],xlab='Cover (%)',ylab='Estimated biomass',pch=19)

# View relative abundances
t <- shrubs |>
  group_by(VEG_SPCD, GROWTH_HABIT_CD, Type) |> tally() |>
  left_join(shrub.codes |> select(VEG_SPCD, Scientific)) |>
  arrange(-n)
head(t)

# 
# t<-setNames(aggregate(shrubs[,c('VEG_SPCD')],by=list(shrubs$VEG_SPCD, shrubs$GROWTH_HABIT_CD,shrubs$Type),FUN=NROW),c('VEG_SPCD','GROWTH_HABIT_CD','Type','count'))
# t<-merge(t,shrub.codes[,c('VEG_SPCD','Scientific')],by='VEG_SPCD',all.x=T)
# head(t[order(t$count,decreasing=T),],50)

# Plots that are missing from shrub dataframe
# For most of these, this is because they have no shrubs in the shrub table
# unique(formatC(cn.df[!cn.df%in%shrubs$CN],digits=16)) # Plots that are missing from shrub data.frame
#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
### Join FIA shrub data to FIA trees: ----
shrubs<-shrubs |> left_join(unique(riley_tree.df[,c('TM_ID','PLT_CN')]),by='PLT_CN') |>
  filter(!is.na(Type))

shrubs_to_merge<-data.frame(shrubs[,c('TM_ID','PLT_CN')],'SUBP'=shrubs$SUBP, 'PWG'=NA,'TREE'=NA,
                            'BHAGE'=shrubs$age,'AGE'=shrubs$age,'DIA'=NA,'HT'=NA,'ACTUALHT'=NA,
                            'CARBON_AG'=shrubs$biomass,'AG_biomass_gm2'=shrubs$biomass,'TPA_UNADJ'=NA,'ELEV'=NA,
                            'SpecCode'=shrubs$Type,'LAT'=NA,'LON'=NA,'SpeciesLatin'=shrubs$Type,
                            'Longevity'=shrubs |> left_join(species.codes[,c('SpecCode','Longevity')],by=c('Type'='SpecCode')) |> dplyr::select(Longevity),
                            'STATE'=shrubs$STATE,'INVYR'=shrubs$INVYR,'AGE_METHOD'=NA)
shrubs_to_merge

landis_tree_shrub.df <- bind_rows(landis_treelist.df,shrubs_to_merge)
str(landis_tree_shrub.df)

## View relative tree and shrub biomass: 
t <- landis_tree_shrub.df |>
  group_by(SpecCode, PLT_CN, SUBP) |>
  summarise(Biomass = sum(AG_biomass_gm2))

ggplot(t)+geom_bar(aes(x=SpecCode,y=Biomass),stat='identity')+theme_classic() + theme(axis.text.x=element_text(angle=90))
ggplot(landis_tree_shrub.df)+geom_point(aes(x=AGE,y=AG_biomass_gm2,col=SpecCode))+theme_classic()


# temp<-landis_tree_shrub.df
# temp$veg_group<-'conifer'
# temp[temp$SpecCode%in%c('BetuPapy','BetuOcci','AlnuRub','PopuBals','PopuTrem','AcerMacr','QuerGarr','FraxLati','PrunEmar'),'veg_group']<-'hardwood'
# temp[temp$SpecCode%in%c('Nfixer_Resprt','NonFxr_Resprt','NonFxr_Seed'),'veg_group']<-'shrub'
# temp[temp$SpecCode%in%c('Grass_Forb'),'veg_group']<-'grasses'
# 
# t<-setNames(aggregate(temp$AG_biomass_gm2,by=list(temp$CN,temp$SUBP,temp$veg_group),FUN=sum),c('CN','SUBP','Veg_type','Biomass'))
# t$Veg_type<-factor(t$Veg_type,levels=c('conifer','hardwood','shrub','grasses'))
# t<-t[order(t$CN),]
# 
# theme_set(theme_classic()+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank()))
# ggplot(t)+geom_point(aes(x=factor(CN),y=Biomass,col=Veg_type))
# ggplot(t[1:5000,])+geom_bar(aes(x=factor(CN),y=Biomass,fill=Veg_type),stat='identity',position='stack')+ylim(0,20000)+scale_fill_discrete_sequential('Viridis',rev=F)

# Plot cumulative % tree biomass:
# t <- temp |> group_by(CN) |> mutate(percent.biomass = AG_biomass_gm2/sum(AG_biomass_gm2)) |>
#   ungroup() |>
#   group_by(CN, veg_group) |>
#   summarise(percent.biomass = sum(percent.biomass)) |>
#   mutate(CN = as.factor(CN)) |>
#   unique() |>
#   left_join(plot_state.df[,c('PLT_CN','PWG')],by=c("CN" = "PLT_CN")) |>
#   arrange(percent.biomass) |>
#   mutate(rank = row_number())
# t[t$CN==12268178010690,c('CN','TREE','veg_group','SpecCode','percent.biomass')]
# t2<-setNames(aggregate(t$percent.biomass,by=list(t$CN,t$veg_group),FUN=sum),c('CN','veg_group','percent.biomass'))
# t2[t2$CN==12268178010690,c('CN','veg_group','percent.biomass')]
# t2<-unique(t2 |> mutate(CN = as.factor(CN)) |> left_join(plot_state.df[,c('PLT_CN','PWG')],by=c("CN" = "PLT_CN")))

# t2<-t2[order(t2$percent.biomass,decreasing=F),]
# t2$rank<-1:nrow(t2)

# ggplot(t) + geom_point(aes(x=rank,y=percent.biomass,colour=veg_group)) +
#   scale_y_continuous(name='Proportion of plot biomass',limits=c(0,1))

# t2<-t[sample(1:nrow(t),nrow(t),replace=F),]
# t2$rank<-1:nrow(t2)
# 
# ggplot(t2) + geom_point(aes(x=rank,y=percent.biomass,colour=veg_group)) +
#   scale_y_continuous(name='Proportion of plot biomass',limits=c(0,1))
# 
# library(data.table)
# t3<-dcast(as.data.table(t2), CN + PWG ~ veg_group, value.var = c("percent.biomass"),fun.aggregate = sum)
# t3<-data.frame(t3)
# t3[t3$CN==12268178010690,]
# 
# t3<-t3[order(t3$conifer,decreasing=F),]
# t3$rank<-1:nrow(t3)
# 
# ggplot(t3) + geom_point(aes(x=rank,y=conifer),col='darkgreen') +
#   geom_point(aes(x=rank,y=hardwood),col='blue') +
#   geom_point(aes(x=rank,y=shrub),col='tomato4') +
#   geom_point(aes(x=rank,y=grasses),col='wheat') + 
#   scale_y_continuous(name='Proportion of plot biomass',limits=c(0,1))
# 
# dev.print(tiff,file=file.path(wdir,'Biomass_relative_proportion.tiff'),width=3,height=3,res=600,units='in',compression='lzw')

#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
## If any plots have multiple censuses, pick the most recent census: ----
t <- landis_tree_shrub.df |> group_by(PLT_CN, STATE) |>
  summarise(Obs = length(unique(INVYR)))

# t<-aggregate(landis_tree_shrub.df$INVYR,by=list(landis_tree_shrub.df$CN,landis_tree_shrub.df$STATE),FUN=function(x) length(unique(x)))
if(nrow(t[t$Obs>1,])>0) {
  t<-t[t$x>1,]
  print(formatC(t$Group.1,digits=15))
  landis_tree_shrub.df[landis_tree_shrub.df$CN %in% t$Group.1,]
  
  # landis_tree_shrub.df <- landis_tree_shrub.df[!(landis_tree_shrub.df$CN=='22827651010497' & landis_tree_shrub.df$INVYR == 2006),]
  # landis_tree_shrub.df <- landis_tree_shrub.df[!(landis_tree_shrub.df$CN=='22827758010497' & landis_tree_shrub.df$INVYR == 2006),]
  warning("Dropped ",nrow(t)," plots containing multiple INVYRs in the Riley Dataset causing duplicate rows.")
}
#-----------------------------------------------------------------------------------------------------------------------



#-----------------------------------------------------------------------------------------------------------------------
## Reduce ages for trees that are beyond their longevity: ----
landis_tree_shrub.df <- landis_tree_shrub.df |>
  mutate(AGE_limit_by_longevity = ifelse(AGE > Longevity & !is.na(Longevity), Longevity, AGE)) |>
  filter(!is.na(AGE)) |>
  filter(AGE>0) ## Drop trees with age = 0: ----

landis_tree_shrub.df[landis_tree_shrub.df$AGE>landis_tree_shrub.df$Longevity
                     ,c('DIA','SpecCode','AGE','AGE_limit_by_longevity','Longevity')]
#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
## Write tree list: ----
write.csv(landis_tree_shrub.df, file.path(wdir,paste0(LANDIS.EXTENT,"_tree_list_2022.csv")),row.names=F) 
#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
## Aggregate by cohort: ----
unique_cohort.df <- landis_tree_shrub.df |>
  select(TM_ID, SpecCode, AGE_limit_by_longevity, AG_biomass_gm2) |>
  mutate(SpecCode = SpecCode |> replace_values(
    "PopuTrem" ~ "HardWood",
  )) |>
  group_by(TM_ID, SpecCode, AGE_limit_by_longevity) |>
  summarise(AG_biomass_gm2 = sum(AG_biomass_gm2)) |>
  na.omit() |>
  rename(MapCode = TM_ID, SpeciesName = SpecCode, CohortAge = AGE_limit_by_longevity, CohortBiomass = AG_biomass_gm2) |>
  bind_rows(data.frame("MapCode"=99, "SpeciesName"='Grass_Forb', "CohortAge"=2, "CohortBiomass"=250)) |>
  mutate(
    WoodBiomass = CohortBiomass * 0.9,
    LeafBiomass = CohortBiomass * 0.1
  )

# colnames(unique_cohort.df) <- c("MapCode", "SpeciesName", "CohortAge", "CohortBiomass")

## Create a dummy row for map id = 99: ----
#   These pixels have no value in the Riley raster, but not bare ground or water. These are primarily grass pixels.
# grass_plot<-data.frame("MapCode"=99, "SpeciesName"='Grass_Forb', "CohortAge"=2, "CohortBiomass"=250)

# unique_cohort.df<-bind_rows(unique_cohort.df,grass_plot) 

#-----------------------------------------------------------------------------------------------------------------------
### Write cohorts to csv: ----
write.csv(unique_cohort.df, file.path(wdir,paste0(LANDIS.EXTENT,"_cohorts2022.csv")),row.names=F) 
write.csv(unique_cohort.df, file.path(LANDIS.EXTENT, paste0("INITIAL_COMMUNITIES_2022_", LANDIS.EXTENT,".csv")),row.names=F) 

outputs <- unique_cohort.df |>
  group_by(SpeciesName) |>
  summarise(Totalbiomass = sum(CohortBiomass))
data.frame(outputs)

outputs2 <- read.csv(file.path(wdir,paste0(LANDIS.EXTENT,"_cohorts.csv"))) |>
  group_by(SpeciesName) |>
  summarise(Totalbiomass = sum(CohortBiomass))
data.frame(outputs2)

biom2 <- outputs |>
  mutate(SpeciesName = fct_reorder(SpeciesName, desc(Totalbiomass))) |>
  ggplot(aes(x = SpeciesName, y = Totalbiomass)) + 
  geom_col() +
  theme(axis.text.x = element_text(angle = 90))
plot(biom2)
png(file.path(wdir,paste0(LANDIS.EXTENT,"_biomass_by_sp_2022.png")), width=5, height = 3, units='in', res=300)
plot(biom2)
dev.off()

# Check for erroneously high biomass (can result from double counting trees because of an incorrect table join)
t<-data.frame(unique_cohort.df |>
                group_by(MapCode) |>
                summarise(Biomass_Mg = sum(CohortBiomass) * 0.01))
if(nrow(t[t$Biomass_Mg>1000,])>1) stop('Some plots have biomass greater than 1,000 Mg/ha. That seems wrong. Are you double counting trees? Run: landis_tree_shrub.df[landis_tree_shrub.df$tm_id%in%t[t$Biomass_Mg>1000,]$MapCode,]')
#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
### Summarize species abundance and max ages for log file: ----
landis_tree_shrub.df <- landis_tree_shrub.df |>
  mutate(Age_Measured = ifelse(AGE_METHOD == 'Modeled', NA, AGE))

t <- landis_tree_shrub.df |>
  group_by(SpecCode) |>
  summarise(Count = n(), 
            Total.biomass = round(sum(AG_biomass_gm2), 0),
            N.plots = length(unique(PLT_CN)),
            Mean.age = round(mean(Age_Measured, na.rm = T),0),
            Max.90th.age = quantile(Age_Measured, 0.9, na.rm = T),
            Max.99th.age = quantile(Age_Measured, 0.99, na.rm = T),
            Max.age = max(Age_Measured, na.rm = T), 
            Max.age.limited = max(AGE_limit_by_longevity)) |>
  mutate(Mean.biomass.per.plot = round(Total.biomass / N.plots,0)) |>
  arrange(SpecCode)

# landis_tree_shrub.df$Age_Measured<-landis_tree_shrub.df$AGE_circa_2020
# landis_tree_shrub.df[landis_tree_shrub.df$AGE_METHOD == 'Modeled'&!is.na(landis_tree_shrub.df$AGE_METHOD),'Age_Measured']<-NA

# t<-setNames(aggregate(landis_tree_shrub.df$SpecCode,by=list(landis_tree_shrub.df$SpecCode),FUN=NROW),c('SpecCode','Count'))
# t$Total.biomass<-round(aggregate(landis_tree_shrub.df$AG_biomass_gm2,by=list(landis_tree_shrub.df$SpecCode),FUN=sum)$x,0)
# t$N.plots<-aggregate(landis_tree_shrub.df$CN,by=list(landis_tree_shrub.df$SpecCode),FUN=function(x) return(length(unique(x))))$x
# t$Mean.biomass.per.plot<-round(t$Total.biomass/t$N.plots,0)
# t$Mean.age<- round(aggregate(landis_tree_shrub.df$Age_Measured,by=list(landis_tree_shrub.df$SpecCode),FUN=function(x) return(mean(x,na.rm=T)))$x,0)
# t$Max.90th.age <-  aggregate(landis_tree_shrub.df$Age_Measured,by=list(landis_tree_shrub.df$SpecCode),FUN=function(x) return(quantile(x,0.9,na.rm=T)))$x
# t$Max.99th.age <-  aggregate(landis_tree_shrub.df$Age_Measured,by=list(landis_tree_shrub.df$SpecCode),FUN=function(x) return(quantile(x,0.99,na.rm=T)))$x
# t$Max.age      <-  aggregate(landis_tree_shrub.df$Age_Measured,by=list(landis_tree_shrub.df$SpecCode),FUN=function(x) return(max(x,na.rm=T)))$x
# t$Max.age.limited<-aggregate(landis_tree_shrub.df$AGE_limit_by_longevity,by=list(landis_tree_shrub.df$SpecCode),FUN=max)$x
# t<-t[order(t$SpecCode),]


t2 <- unique_cohort.df |>
  group_by(MapCode, SpeciesName) |>
  summarise(Biomass.sum = sum(CohortBiomass)) |>
  group_by(SpeciesName) |>
  summarise(Median.biomass = quantile(Biomass.sum, 0.5),
            Max.99th.biomass = quantile(Biomass.sum, 0.99),
            Max.biomass = max(Biomass.sum, na.rm = T)) |>
  rename(SpecCode = SpeciesName)

# t2<-aggregate(unique_cohort.df$CohortBiomass,by=list(unique_cohort.df$MapCode,unique_cohort.df$SpeciesName),FUN=sum)
# t3<-aggregate(t2$x,by=list(t2$Group.2),FUN=function(x) 
#   return(c('quantile'=quantile(x,0.5),'quantile'=quantile(x,0.99),'max'=max(x,na.rm=T))))
# t3<-data.frame('SpecCode'=t3$Group.1,'Median.biomass'=t3$x[,1],'Max.99th.biomass'=t3$x[,2],'Max.biomass'=t3$x[,3])
#t4<-merge(t,t3,by='SpecCode')
t4<- t |>
  left_join(t2, by = "SpecCode")


### Write species abundance summary to CSV: 
write.csv(t4,file=file.path(wdir,"Species_abundance_age_summary_2022.csv"),row.names=F)


#-----------------------------------------------------------------------------------------------------------------------
########################################################################################################################
#####################################################    END    ##################################################   ---- 
stop('\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n
                                                      ####################################################
                                                            =========================================
                                                              -------------------------------------
                                                              
                                                           Code ran successfully! No errors detected.
                                                                  
                                                              -------------------------------------
                                                          *********************************************
                                                     ######################################################\n',call.=F)

#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
########################################################################################################################
########################################################################################################################
########################################################################################################################
########################################################################################################################


















