suppressWarnings(suppressMessages(library(terra)))
suppressWarnings(suppressMessages(library(tidyverse)))
suppressWarnings(suppressMessages(library(tidyterra)))

#setwd('C:\\LANDIS_Local\\OkaMet_6.8\\LANDIS_MHtest_20240630_0806')
#setwd('F:\\LANDIS Runs\\OkaMet_LANDIS_model\\LANDIS_MHtest_20240703_1559')
#timestep <- 79
#scenario_rx_name <- 'Rx_zone_mgd_forest.tif'; thin_cooldown <- 20
#fireDir <- './scrapple-fire'

flipIfNeeded <- function(r){
  r2 <- r  # do test not on raw data
  crs(r2) <- crs(ecos.r); ext(r2) <- ext(ecos.r)
  overlay_normal <- ifel(ecos.r==0&r2!=0, 1, 0)  # make a raster of cells where
  overlay_flip <- ifel(ecos.r==0&flip(r2)!=0, 1, 0)
  if (sum(values(overlay_normal, na.rm=T)) > sum(values(overlay_flip, na.rm=T))) {
    return(flip(r2))
  } else {
    return(r2)
  }
}


# Load magic harvest command inputs
# 1. timestep (always first argument)
# 2. scenario name
# 3. thinning cooldown
# 4. magic harvest file name
# 5. Landis extent
args <- commandArgs(trailingOnly = TRUE)
print("Input args:")
print(args)

timestep <- as.numeric(args[1]); print(paste('Timestep:', timestep))
last_year <- timestep - 1
scenario_rx_name <- args[2]
mh_file_name <- args[4]

ecos_file <- dir("./Input_file_archive")[grepl("ECOREGIONS", dir("./Input_file_archive"))][[1]]
LANDIS.EXTENT <- str_replace(ecos_file, 'ECOREGIONS_', '') %>% str_replace('.txt', '') %>% str_replace('.tif', '')  # EXTRACT LANDIS EXTENT from ECOREGIONS

fireDir <- './social-climate-fire'
harvestDir <- './Harvest'

sink(file = file.path('./MagicHarvest', paste0('R_log_MH.txt')), append=T)  # write output to a text file for troubleshooting

print('Loading ecoregions...')
ecos.r <- rast(file.path('./Input_file_archive', paste0('ECOREGIONS_',LANDIS.EXTENT,'.tif')))
print('Loading base mgmt. zones...')
base_mgmt.r <- rast(file.path('./Input_file_archive', paste0('ext_BiomassHarvestMgmt_',LANDIS.EXTENT,'.tif')))
base_stands.r <- rast(file.path('./Input_file_archive', paste0('ext_BiomassHarvestStands_',LANDIS.EXTENT,'.tif')))
mgmt.r <- rast(file.path('./MagicHarvest', paste0('MH_mgmt_areas.tif'))) + 0
#stands.r <- rast(file.path('./MagicHarvest', paste0('MH_stands.tif'))) + 0

severity.thresholds <- c(5, 41, 176, 376, 2001); names(severity.thresholds) <- c('Unburned', 'Low', 'Moderate', 'High', 'max')
severity.reclass.df <- data.frame('from'=c(severity.thresholds[1:4]), 'to'=c(severity.thresholds[2:5]), 'becomes'=c(1,2,3,4))

fire_cooldown <- 7  # possibly set to 6 or 7
thin_cooldown <- as.numeric(args[3])  # 20 for 5% or 50 for 2%
cold_cooldown <- ifelse(thin_cooldown<=30, thin_cooldown+5, thin_cooldown)

max_annual_thin <- 1/thin_cooldown*sum(values(ifel(base_mgmt.r%in%c(1,2,3), 1, NA)), na.rm=T)  # should be 2 or 5 % of managed forest area
max_annual_ind <- 0.05*sum(values(ifel(base_mgmt.r==4, 1, NA)), na.rm=T)
max_annual_DNR <- 0.05*sum(values(ifel(base_mgmt.r==7, 1, NA)), na.rm=T)
max_annual_salvage <- 0.1*sum(values(ifel(base_mgmt.r<9, 1, NA)), na.rm=T)  # should be 2 or 5 % of managed forest area

################################################################################
####### Initialize rasters #####################################################
################################################################################
if(timestep == 1){
  if(!dir.exists('./MagicHarvest')){dir.create('./MagicHarvest')}  # setup directory
  
  # copy stand age from input file archive
  stand_age.r <- rast(file.path('./Input_file_archive', paste0('MH_Stand_age', LANDIS.EXTENT, '.tif')))
  writeRaster(stand_age.r, file.path('./MagicHarvest', 'stand_age.tif'))
  
  # copy rx dynamic
  rx_dynamic.r <- rast(file.path('..', LANDIS.EXTENT, 'SCRAPPLE_input_maps', scenario_rx_name))
  writeRaster(rx_dynamic.r, file.path('./MagicHarvest', 'Rx_base.tif'))
  
  # fire cooldown 
  fire_cooldown.r <- rast(nrows=nrow(ecos.r), ncols=ncol(ecos.r), extent=ext(ecos.r), crs=crs(ecos.r), vals=0)
  fire_cooldown.r[] <- 0
  writeRaster(fire_cooldown.r, file.path('./MagicHarvest', 'fire_cooldown.tif'))
  
  thinning_cooldown.r <- rast(nrows=nrow(ecos.r), ncols=ncol(ecos.r), extent=ext(ecos.r), crs=crs(ecos.r), vals=0)
  thinning_cooldown.r[] <- 0
  writeRaster(thinning_cooldown.r, file.path('./MagicHarvest', 'thinning_cooldown.tif'))
}

################################################################################
####### Load this year's rasters ###############################################
################################################################################
# Load last year's disturbances
if (timestep == 1){
  last_year_fire_severity.r <- rast(nrows=nrow(ecos.r), ncols=ncol(ecos.r), extent=ext(ecos.r), crs=crs(ecos.r), vals=0)
  last_year_harvest_prescrip.r <- rast(nrows=nrow(ecos.r), ncols=ncol(ecos.r), extent=ext(ecos.r), crs=crs(ecos.r), vals=0)
} else {
  last_year_fire_severity.r <- flipIfNeeded(rast(file.path(fireDir, paste0('fire-dnbr-',last_year,'.img')))) + 0 
  last_year_fire_severity.r <- last_year_fire_severity.r %>%
    ifel(.>1, ., NA) %>%
    focal(w = matrix(1,3,3), fun = median, na.rm=T, pad = TRUE, na.policy = 'only') %>%
    classify(rcl=severity.reclass.df,include.lowest=T)
  crs(last_year_fire_severity.r) <- crs(ecos.r)
  ext(last_year_fire_severity.r) <- ext(ecos.r)
  plot(last_year_fire_severity.r)
  
  last_year_harvest_prescrip.r <- flipIfNeeded(rast(file.path(harvestDir, paste0('biomass-harvest-prescripts-',last_year,'.tif')))) + 0
  crs(last_year_harvest_prescrip.r) <- crs(ecos.r)
  ext(last_year_harvest_prescrip.r) <- ext(ecos.r)
}

# Load cooldowns
fire_cooldown.r <- rast(file.path('./MagicHarvest', 'fire_cooldown.tif')) + 0
thinning_cooldown.r <- rast(file.path('./MagicHarvest', 'thinning_cooldown.tif')) + 0

# Load stand age
stand_age.r <- rast(file.path('./MagicHarvest', paste0('stand_age.tif'))) + 0

################################################################################
####### Update tracking rasters ################################################
################################################################################
# Calculate stand age
print('Calculating stand age...')
stand_age.r <- stand_age.r + 1  # age up the stands by 1
stand_age.r[last_year_fire_severity.r==4] <- 0  # if a severe fire burned last year, the stand is now a year old
stand_age.r[last_year_harvest_prescrip.r%in%c(5,8)] <- 1  # if a stand was clearcut, it's now one -- this is to bypass the salvage allocation

writeRaster(stand_age.r, file.path('./MagicHarvest', 'stand_age.tif'), overwrite=T)

print('Updating fire cooldown...')
fire_cooldown.r <- fire_cooldown.r - 1  # increment down active counters
fire_cooldown.r[fire_cooldown.r<fire_cooldown*-1] <- fire_cooldown*-1  # actually, we want lagged areas to be prioritized
fire_cooldown.r[last_year_fire_severity.r>0] <- fire_cooldown  # if it burned last year, we don't need to Rx burn it, so set the timer to max
fire_cooldown.r[last_year_harvest_prescrip.r>1] <- fire_cooldown  # if it was thinned or harvested, fuels were reduced through slash burning so don't burn again
fire_cooldown.r[last_year_fire_severity.r>2] <- 5
writeRaster(fire_cooldown.r, file.path('./MagicHarvest', 'fire_cooldown.tif'), overwrite=T)

print('Updating thinning cooldown...')
thinning_cooldown.r <- thinning_cooldown.r -1
thinning_cooldown.r[thinning_cooldown.r<0] <- 0
thinning_cooldown.r[last_year_harvest_prescrip.r%in%c(9,10,12,13)] <- thin_cooldown  # if it was harvested, set the cooldown to the specified interval
thinning_cooldown.r[last_year_harvest_prescrip.r%in%c(11,14,15)] <- cold_cooldown
#thinning_cooldown.r[last_year_harvest_prescrip.r==6] <- 20  # but if PC thin, max cooldown is 20? 10?
writeRaster(thinning_cooldown.r, file.path('./MagicHarvest', 'thinning_cooldown.tif'), overwrite=T)

################################################################################
####### Generate harvest and fire allocations ##################################
################################################################################
print('Generating new harvest and fire allocations...')
# salvage: age 1-2, in a managed or industrial forest
eligible_salvage.r <- rast(nrows=nrow(ecos.r), ncols=ncol(ecos.r), extent=ext(ecos.r), crs=crs(ecos.r), vals=0)
eligible_salvage.r[stand_age.r<2&base_mgmt.r<9] <- 1
eligible_salvage.r[last_year_harvest_prescrip.r==7] <- 0  # if it was salvaged last year, don't salvage it again!
eligible_salvage.r[last_year_harvest_prescrip.r%in%c(5,8)] <- 0  # if it was clearcut last year, don't salvage it!

# pc thin: age 20-40, in a managed or industrial forest
# eligible_pcthin.r <- rast(nrows=nrow(ecos.r), ncols=ncol(ecos.r), extent=ext(ecos.r), crs=crs(ecos.r), vals=0)
# eligible_pcthin.r[stand_age.r>=20&stand_age.r<40&base_mgmt.r<9&thinning_cooldown.r==0] <- 1

# thin: age 20+, thin cooldown = 0 -- harvest module will take care of PC vs C thinning
eligible_thin.r <- rast(nrows=nrow(ecos.r), ncols=ncol(ecos.r), extent=ext(ecos.r), crs=crs(ecos.r), vals=0)
eligible_thin.r[stand_age.r>=20&base_mgmt.r%in%c(1,2,3)&thinning_cooldown.r==0] <- 1

# industrial harvest: age 35+
# eligible_industrial.r <- rast(nrows=nrow(ecos.r), ncols=ncol(ecos.r), extent=ext(ecos.r), crs=crs(ecos.r), vals=0)
# eligible_industrial.r[stand_age.r>=35&mgmt.r%in%c(1,2,3)&thinning_cooldown.r==0] <- 1

### Regenerate stands by grouping
print('Generating new patches...')
new_patches.r <- stand_age.r
new_patches.r[!is.na(new_patches.r)]<-as.numeric(paste0(base_mgmt.r[!is.na(new_patches.r)],stand_age.r[!is.na(new_patches.r)]))

new_patches.r <- new_patches.r %>% as.polygons() %>%
  disagg() %>%
  mutate(ID = row_number()) %>%
  mutate(ID = sample(ID, length(ID))) %>%  # shuffle ID numbers for better visualization later
  rasterize(new_patches.r, 'ID')
plot(new_patches.r)

new_patches.r[is.na(new_patches.r)] <- 0

plot(new_patches.r)
plot(base_stands.r)
length(unique(values(base_stands.r)))
length(unique(values(new_patches.r)))
writeRaster(new_patches.r, file.path('./MagicHarvest', paste0('MH_stands_',timestep,'.tif')), datatype = 'INT4S')

harvest_input <- read_lines(file = file.path('.', 'Input_file_archive', mh_file_name))
harvest_input[which(grepl('ManagementAreas', harvest_input))] <- paste0('ManagementAreas', '    ', './MagicHarvest/MH_mgmt_areas_', timestep, '.tif')
harvest_input[which(grepl('Stands', harvest_input))] <- paste0('Stands', '    ', './MagicHarvest/MH_stands_', timestep, '.tif')
write_lines(harvest_input, file.path('.', 'Input_file_archive', mh_file_name))

################################################################################
####### generate new management zones
################################################################################
### select patches to thin from managed forests
print('Generating management zones...')
elig_stack <- c(new_patches.r, eligible_salvage.r, eligible_thin.r, base_mgmt.r)
names(elig_stack) <- c('Patch', 'Salvage_elig', 'Thin_elig', 'Mgmt_zone')
elig.df <- as.data.frame(elig_stack) %>% 
  filter(Patch!=0) %>% 
  group_by(Patch) %>% 
  summarise(Salvage_elig = mean(Salvage_elig),  # these should be integers if all works correctly
            #PCthin_elig = mean(PCthin_elig),
            Thin_elig = mean(Thin_elig),
            Mgmt_zone = mean(Mgmt_zone),
            Pixels = n())

#### while total area is < target, pick a stand and add it to the list
##### Salvage
salvageable_stands.df <- elig.df %>% filter(Salvage_elig>.75) %>%
  sample_n(nrow(.)) %>%  # shuffle the dataframe
  mutate(Area_cumsum = cumsum(Pixels)) %>%  # get cumulative area
  filter(Area_cumsum <= max_annual_salvage)  # and cutoff once the area crosses the annual limit!

##### Thinning
thinnable_stands.df <- elig.df %>% filter(Thin_elig>.75) %>%
  sample_n(nrow(.)) %>%  # shuffle the dataframe
  mutate(Area_cumsum = cumsum(Pixels)) %>%  # get cumulative area
  filter(Area_cumsum <= max_annual_thin)  # and cutoff once the area crosses the annual limit!


new_zones.r <- rast(nrows=nrow(ecos.r), ncols=ncol(ecos.r), extent=ext(ecos.r), crs=crs(ecos.r), vals=9)
new_zones.r[base_mgmt.r==4] <- 4  # use base harvest to allocate industrial and WA dnr area
new_zones.r[base_mgmt.r==7] <- 7
#new_zones.r[eligible_pcthin.r==1] <- 5  # assign pcthin
if(nrow(thinnable_stands.df)>0){new_zones.r[new_patches.r%in%thinnable_stands.df$Patch] <- base_mgmt.r[new_patches.r%in%thinnable_stands.df$Patch]}
if(nrow(salvageable_stands.df)>0){new_zones.r[new_patches.r%in%salvageable_stands.df$Patch] <- 6}

plot(new_zones.r)
writeRaster(as.int(new_zones.r), file.path('./MagicHarvest', paste0('MH_mgmt_areas_', timestep, '.tif')), datatype = 'INT1U', overwrite=T)

################################################################################
##### rx burn: no treatment planned, rx timer = 0, in Rx zone (managed forest or managed+wildlands)
################################################################################
print('Generating new Rx allocation...')
rx_zone.r <- rast(file.path('./MagicHarvest', 'Rx_base.tif'))
rx_stands.r <- rast(file.path('..', paste0('ext_BiomassHarvestStands_', LANDIS.EXTENT,'.tif')))  # use a different, fixed set of stands since SCRAPPLE doesn't reload with each timestep
topo_asp.r <- rast(file.path('..', 'TopoAsp_', LANDIS.EXTENT,'.tif'))
#file.path('./MagicHarvest', 'TopoAsp_Patches.tif') <- rast(file.path('./MagicHarvest', 'TopoAsp_Patches.tif'))
max_annual_Rx <- sum(values(ifel(rx_zone.r>0, 1, NA)), na.rm=T)/fire_cooldown  # get a maximum burnable area value that keeps a steady cycle of burning

## first, get which small patches are eligible to burn
rx_elig_stack <- c(rx_stands.r, rx_zone.r, fire_cooldown.r, topo_asp.r)
names(rx_elig_stack) <- c('Patch', 'Rx_zone', 'Rx_cooldown', 'TopoAsp')
rx_elig.df <- as.data.frame(rx_elig_stack) %>% 
  filter(Patch!=0, 
         Rx_zone != 0) %>% 
  group_by(Patch) %>%
  summarise(Mean_cooldown = mean(Rx_cooldown),
            Pixels = n(),
            TopoAsp = mean(TopoAsp)) %>%
  filter(Mean_cooldown <= 0.33) # we want the stand to on average be due for another burn. I think setting to 0.33 is best so that a stand isn't made ineligible if less than a third has burned 

# Now we have a set of eligible patches.
print(paste('Number of Rx eligible patches:', nrow(rx_elig.df)))

# Delete ineligible patches from the topo_asp layer
topo_asp.r[!rx_stands.r%in%rx_elig.df$Patch] <- NA
topo_asp.r[rx_zone.r==0] <- NA  # also remove areas outside the Rx eligible zone

# Turn this edited topoasp layer into discrete patch polygons
topo_asp_patches.r <- topo_asp.r %>% as.polygons() %>%
  disagg() %>%
  mutate(ID = row_number()) %>%
  mutate(ID = sample(ID, length(ID))) %>%  # shuffle ID numbers for better visualization later
  rasterize(new_patches.r, 'ID')

topo_asp_patches_stack <- c(topo_asp_patches.r, rx_stands.r)
names(topo_asp_patches_stack) <- c('TopoAspID', 'Patch')

topo_asp_selected.df <- topo_asp_patches_stack %>% 
  as.data.frame() %>%
  group_by(TopoAspID) %>%
  summarise(Pixels=n()) %>%
  sample_n(nrow(.)) %>%
  mutate(Area_cumsum=cumsum(Pixels)) %>%
  filter(Area_cumsum<=max_annual_Rx)

print(paste('Max annual rx:', max_annual_Rx, 'Area selected to burn in big patches:',max(topo_asp_selected.df$Area_cumsum)))

new_rx.r <- rast(nrows=nrow(ecos.r), ncols=ncol(ecos.r), extent=ext(ecos.r), crs=crs(ecos.r), vals=0)
# new_rx.r[fire_cooldown.r == 0] <- -1  # we want to burn everywhere eligible
# new_rx.r[rx_zone==0] <- 0  # but not if its outside the potential zones
# new_rx.r[new_zones.r%in%c(1,2,3,4,5,6,7,8)] <- 0  # and not if there's already thinning planned
# #new_rx.r[stand_age.r < 20] <- 0  # and not if the stand age is too young

#new_rx.r <- -1*fire_cooldown.r  # prioritize areas that are overdue for burning

#new_rx.r[rx_stands.r%in%rx_elig.df$Patch] <- 1
new_rx.r[topo_asp_patches.r%in%topo_asp_selected.df$TopoAspID] <- 1
new_rx.r[rx_zone.r==0] <- 0  # but not if its outside the potential zones
new_rx.r[new_rx.r<0] <- 0  # if fire cooldown active, set odds to zero

writeRaster(new_rx.r, file.path('./MagicHarvest', 'RxIgnProb_dynamic.tif'), overwrite=T)

################################################################################
####### Make some plots ########################################################
################################################################################
mgmtCols<-data.frame('value'=c(1,2,3,4,6,7,9), 
                     'color'=c('goldenrod',  # Dry 1
                             'olivedrab4',  # Moist 2
                             'dodgerblue4',  # Cold 3
                             'tomato4',  # Industrial 4
                             'orchid1',  # Salvage 6
                             'slateblue2',   # WA DNR 7
                             'lightgrey'  # no treatment 9
                             ),
                     'id'=c(1,2,3,4,6,7,9),
                     'Zone'=c('Dry', 'Moist', 'Cold', 'Industrial', 'Salvage', 'WA DNR', 'None'))

# prescripts<-c('None', 'Valley','Mesic','Xeric','Industrial','Pre-commercial','Salvage', 'WA_DNR', 'PCT Dry', 'PCT Moist', 'PCT Cold', 'CT Dry', 'CT Moist', 'CT Cold')
# names(prescripts)<-1:length(prescripts)

harvestColsClassified<-data.frame('value'=1:14, 
                                  'color'=c('lightgrey', # none 1
                                            'red', # Old valley 2
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
                                            'dodgerblue4'  # CT Cold 14
                                             ),
                                  'id'=1:14,
                                  'Treatment'=c('None', 'MBValley', 'MBMesic', 'MBXeric', 'Industrial', 'PCTgeneric', 'Salvage','WA DNR', 'PCTdry', 'PCTmoist', 'PCTcold', 'CTdry', 'CTmoist', 'CTcold'))

severityColsClassified <- colorRampPalette(c('darkgreen', 'darkseagreen', 'goldenrod1', 'firebrick4'))(4)

patchCols <- colorRampPalette(c("#000000","#FFFF00","#1CE6FF","#FF34FF","#FF4A46",
                                "#008941","#006FA6","#A30059","#FFDBE5","#7A4900",
                                "#0000A6","#63FFAC","#B79762","#004D43","#8FB0FF",
                                "#997D87","#5A0007","#809693","#FEFFE6","#1B4400",
                                "#4FC601","#3B5DFF","#4A3B53","#FF2F80","#61615A",
                                "#BA0900","#6B7900","#00C2A0","#FFAA92","#FF90C9",
                                "#B903AA","#D16100","#DDEFFF","#000035","#7B4F4B",
                                "#A1C299","#300018","#0AA6D8","#013349","#00846F",
                                "#372101","#FFB500","#C2FFED","#A079BF","#CC0744",
                                "#C0B9B2","#C2FF99","#001E09","#00489C","#6F0062",
                                "#0CBD66","#EEC3FF","#456D75","#B77B68","#7A87A1",
                                "#788D66","#885578","#FAD09F","#FF8A9A","#D157A0",
                                "#BEC459","#456648","#0086ED","#886F4C","#34362D",
                                "#B4A8BD","#00A6AA","#452C2C","#636375","#A3C8C9",
                                "#FF913F","#938A81","#575329","#00FECF","#B05B6F",
                                "#8CD0FF","#3B9700","#04F757","#C8A1A1","#1E6E00",
                                "#7900D7","#A77500","#6367A9","#A05837","#6B002C",
                                "#772600","#D790FF","#9B9700","#549E79","#FFF69F",
                                "#201625","#72418F","#BC23FF","#99ADC0","#3A2465",
                                "#922329","#5B4534","#FDE8DC","#404E55","#0089A3",
                                "#CB7E98","#A4E804","#324E72","#6A3A4C","#83AB58",
                                "#001C1E","#D1F7CE","#004B28","#C8D0F6","#A3A489",
                                "#806C66","#222800","#BF5650","#E83000","#66796D",
                                "#DA007C","#FF1A59","#8ADBB4","#1E0200","#5B4E51",
                                "#C895C5","#320033","#FF6832","#66E1D3","#CFCDAC",
                                "#D0AC94","#7ED379","#012C58","#7A7BFF","#D68E01",
                                "#353339","#78AFA1","#FEB2C6","#75797C","#837393",
                                "#943A4D","#B5F4FF","#D2DCD5","#9556BD","#6A714A",
                                "#001325","#02525F","#0AA3F7","#E98176","#DBD5DD",
                                "#5EBCD1","#3D4F44","#7E6405","#02684E","#962B75",
                                "#8D8546","#9695C5","#E773CE","#D86A78","#3E89BE",
                                "#CA834E","#518A87","#5B113C","#55813B","#E704C4",
                                "#00005F","#A97399","#4B8160","#59738A","#FF5DA7",
                                "#F7C9BF","#643127","#513A01","#6B94AA","#51A058",
                                "#A45B02","#1D1702","#E20027","#E7AB63","#4C6001",
                                "#9C6966","#64547B","#97979E","#006A66","#391406",
                                "#F4D749","#0045D2","#006C31","#DDB6D0","#7C6571",
                                "#9FB2A4","#00D891","#15A08A","#BC65E9","#FFFFFE",
                                "#C6DC99","#203B3C","#671190","#6B3A64","#F5E1FF",
                                "#FFA0F2","#CCAA35","#374527","#8BB400","#797868",
                                "#C6005A","#3B000A","#C86240","#29607C","#402334",
                                "#7D5A44","#CCB87C","#B88183","#AA5199","#B5D6C3",
                                "#A38469","#9F94F0","#A74571","#B894A6","#71BB8C",
                                "#00B433","#789EC9","#6D80BA","#953F00","#5EFF03",
                                "#E4FFFC","#1BE177","#BCB1E5","#76912F","#003109",
                                "#0060CD","#D20096","#895563","#29201D","#5B3213",
                                "#A76F42","#89412E","#1A3A2A","#494B5A","#A88C85",
                                "#F4ABAA","#A3F3AB","#00C6C8","#EA8B66","#958A9F",
                                "#BDC9D2","#9FA064","#BE4700","#658188","#83A485",
                                "#453C23","#47675D","#3A3F00","#061203","#DFFB71",
                                "#868E7E","#98D058","#6C8F7D","#D7BFC2","#3C3E6E",
                                "#D83D66","#2F5D9B","#6C5E46","#D25B88","#5B656C",
                                "#00B57F","#545C46","#866097","#365D25","#252F99",
                                "#00CCFF","#674E60","#FC009C","#92896B"))(length(unique(values(new_patches.r))))

eligible_thin.r[ecos.r==0]<-NA
eligible_salvage.r[ecos.r==0]<-NA
#last_year_harvest_prescrip.r[1:10,1:130] <- rep(2:14, 100)
last_year_harvest_prescrip.r[last_year_harvest_prescrip.r==0]<-NA
last_year_harvest_prescrip.r <- terra::as.factor(last_year_harvest_prescrip.r)
levels(last_year_harvest_prescrip.r) <- harvestColsClassified[, c(3, 4)]

new_zones.r[is.na(last_year_harvest_prescrip.r)] <- NA
new_zones.r <- terra::as.factor(new_zones.r)
levels(new_zones.r) <- mgmtCols[, c(3, 4)]

last_year_fire_severity.r[1:4] <- c(1, 2, 3, 4)

fire_cooldown.r[ecos.r==0] <- NA
fire_cooldown.r[rx_zone.r==0] <- NA
fire_cooldown.r[1:fire_cooldown] <- 1:fire_cooldown
thinning_cooldown.r[ecos.r==0] <- NA
thinning_cooldown.r[1:cold_cooldown] <- 1:cold_cooldown
#new_zones.r[1:10,1:70] <- rep(c(1,2,3,4,6,7,9), 100)

### Load ignition type and fine fuels
if(timestep>1){
  ignition_type.r <- rast(file.path(fireDir, paste0('ignition-type-', last_year, '.img'))) %>% flipIfNeeded() %>% ifel(.>0, ., NA) %>% terra::as.factor()
  crs(ignition_type.r) <- crs(ecos.r)
  ext(ignition_type.r) <- ext(ecos.r)
  levels(ignition_type.r) <- data.frame('id'=c(1,2,3,4), 'ignition'=c('None', 'Acc.', 'Lgt.', 'Rx'))
  ignition_type.r[ecos.r==0] <- NA
  
  fine_fuels.r <- rast(file.path(fireDir, paste0('fine-fuels-', last_year, '.img'))) %>% flipIfNeeded()
  crs(fine_fuels.r) <- crs(ecos.r)
  ext(fine_fuels.r) <- ext(ecos.r)
  fine_fuels.r[ecos.r==0] <- NA
} else{
  ignition_type.r <- rast(nrows=nrow(ecos.r), ncols=ncol(ecos.r), extent=ext(ecos.r), crs=crs(ecos.r), vals=0)
  fine_fuels.r <- rast(nrows=nrow(ecos.r), ncols=ncol(ecos.r), extent=ext(ecos.r), crs=crs(ecos.r), vals=0)
}


### Make plot
png(filename=file.path('./MagicHarvest', paste0('stand_age_', timestep, '.png')), width = 2800, height = 2000)
par(mfrow = c(3,4))
plot(last_year_harvest_prescrip.r, type='classes', col=harvestColsClassified[,2], main="Last year's harvests", all_levels=T)
plot(as.int(last_year_fire_severity.r),col=severityColsClassified, main="Last year's fires")
plot(stand_age.r, main='Stand Age', range=c(0,250))
plot(new_patches.r, col=patchCols, main='New patches')
plot(fire_cooldown.r, type='continuous', col=colorRampPalette(c('red', 'white', 'forestgreen'))(15), range=c(-7,fire_cooldown), main="Fire cooldown")
plot(thinning_cooldown.r, type='continuous', range=c(0,cold_cooldown), main="Thinning cooldown")
plot(new_zones.r, col=mgmtCols[,2], type='classes', main="Planned harvest", all_levels=T)
plot(new_rx.r, main="Planned Rx")
plot(eligible_thin.r, main='Eligible to thin')
#plot(eligible_pcthin.r, main='Eligible to PC thin')
plot(eligible_salvage.r, main='Eligible to salvage')
plot(ignition_type.r, col=c('lightgray', 'darkorange', 'goldenrod', 'turquoise3'), main="Last year ignition types", all_levels=T)
plot((fine_fuels.r/30), main="Last year fine fuel %", col=colorRampPalette(c('cornsilk', 'burlywood', 'darkolivegreen1', 'darkolivegreen1', 
                                                                            'darkolivegreen3', 'darkolivegreen3', 'darkolivegreen', 'darkolivegreen'))(100), range=c(0,100))
dev.off()

rm(list=ls())
gc()

print("Script ran successfully!")












