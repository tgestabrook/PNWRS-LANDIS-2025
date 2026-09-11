library(tidyverse)
library(terra)
library(tidyterra)

Sys.setenv(TMPDIR = "F:/R_TEMP")
terraOptions(tempdir = "F:/R_TEMP")

dataDir <- 'F:/LANDIS_Input_Data_Prep/Data'
bigDataDir <- 'F:/LANDIS_Input_Data_Prep/BigData' # Location of WA_FIA data and Riley dataset (RDS-2019-0026)
outDir <- file.path("F:", "Other Projects", "SmokeTradeoffs")


aoi.sf <- vect(file.path(dataDir, "OWNF_Boundary.shp"))

### Riley tree list: ----
riley_tree2022.df <- read.csv(file.path(bigDataDir, "TreeMap2022", "TreeMap2022_CONUS_Tree_Table.csv"))

### FIA species codes
species.codes <- read.csv(file.path(dataDir,"Species_code_crosswalk.csv"))
fia.spp.codes <- read_csv(file.path(bigDataDir,"FIA","FIADB_REFERENCE/REF_SPECIES.csv"), guess_max = 100000) |>
  dplyr::rename(FIA_Sp_Num = SPCD) |>
  dplyr::rename(SPEC = SPECIES_SYMBOL) |>
  dplyr::select(FIA_Sp_Num, COMMON_NAME, GENUS, SPECIES, SPEC)

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
    left_join(fia.spp.codes,by=c("FIA_Sp_Num")) |>
    filter(!is.na(TRE_CN)) |>
    left_join(species.codes, by = c("SPEC" = "VEG_SPCD")) |>
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
wa_trees<-loadFIA('WA',1980)
id_trees<-loadFIA('ID',1980)
or_trees<-loadFIA('OR',1980)
mt_trees<-loadFIA('MT',1980)


full_fia.df<-rbind(wa_trees,or_trees,mt_trees,id_trees)

#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------

## Crop Riley raster
# Riley_raster_full <- rast(file.path(bigDataDir,"TreeMap2022","TreeMap2022_CONUS.tif")) # National Riley tree list
TreeMap_full <- rast(file.path(bigDataDir,"TreeMap2022","TreeMap2022_CONUS.tif")) 
aoi.sf <- project(aoi.sf, crs(TreeMap_full))
aoi_buff.sf <- aoi.sf |> buffer(90)  # buffer so that a few extra pixels are considered when using focal window to impute out-of-state plots

TreeMap.r <- TreeMap_full |> crop(aoi_buff.sf) |>
  mask(aoi_buff.sf) #|>
  # droplevels()  # eliminate attribute table rows for plots not in the AOI.

plot(TreeMap.r)
polys(aoi.sf, col = 'red', alpha = 0.5)

### the fuelmap plots should be a subset of plots from riley treelist
plot_state.df<-full_fia.df |> select(PLT_CN,STATE,INVYR) |> unique()

tm_cn_crosswalk.df <- riley_tree2022.df |>
  group_by(TM_ID, PLT_CN) |> tally() |> left_join(plot_state.df)

tm_ids_treemap <- unique(values(TreeMap.r))
tm_cn_aoi_treemap.df <- tm_cn_crosswalk.df |>
  filter(TM_ID %in% tm_ids_treemap)

tm_cn_aoi_treemap.df |> group_by(STATE) |> tally()

activeCat(TreeMap.r) <- "TM_ID"
freq(TreeMap.r) |> left_join(tm_cn_aoi_treemap.df, by = c("value" = "TM_ID")) |>
  group_by(STATE) |> summarise(Pix_count = sum(count))



# 
# plot(Riley_raster)
# polys(aoi.sf, col = 'red', alpha = 0.5)
# 
# ## Record unique plot CN codes for each plot in the Riley raster: ----
# riley_tree.df_aoi<-riley_tree2022.df |>
#   filter(TM_ID %in% unique(cats(Riley_raster)[[1]]$TM_ID))
# head(riley_tree.df_aoi)
# 
# ## Add state column to Riley Tree List: ----
# plot_state.df<-full_fia.df |> select(PLT_CN,STATE,INVYR) |> unique()
# 
# head(plot_state.df)
# 
# riley_tree.df_aoi <- riley_tree.df_aoi |> left_join(plot_state.df, by = "PLT_CN") |>
#   mutate(STATE = replace_na(STATE, "z-OTHER"))
# 
# plot_state.df <- riley_tree.df_aoi |> # now trim the plots by state list to just those in AOI
#   select("TM_ID", "PLT_CN", "STATE", "INVYR") |> unique()
# 
# 
# 
# ### How many of the plots in the Riley tree list are from each year? 
# # Full FIA dataset has plots dating back to 2001, but the Riley tree list only uses plots from 2006-2016.
# riley_tree.df_aoi |> 
#   group_by(INVYR) |> tally()
# 
# ### How many of the plots in the Riley tree list are from each state? 
# riley_tree.df_aoi |>
#   group_by(STATE) |> tally()
# 
# # out of region plots:
# t<-riley_tree.df_aoi |> filter(!STATE %in% c('WA','ID','OR','MT'))
# # Percent of total 
# length(unique(t$PLT_CN)) / length(unique(riley_tree.df_aoi$PLT_CN)) # ~15% for Wen, 18% for OkaMet, 21% for Tripod!
# # View the species composition in these plots:
# t[sample(1:nrow(t),100),]
# 
# 
# ### Visualize origin states of riley plots
# origin.r <- Riley_raster
# 
# newcat <- cats(origin.r)[[1]] |> 
#   left_join(plot_state.df) |> 
#   mutate(Origin = ifelse(STATE%in%c('OR','ID','MT'), "PNW", ifelse(STATE == "WA", "WA", "Foreign"))) |> 
#   select("TM_ID", "Origin")
# 
# origin.r <- origin.r |> addCats(newcat)
# activeCat(origin.r) <- "Origin"
# 
# plot(origin.r)
# 
# 
# exotics <- plot_state.df |> filter(STATE == "z-OTHER") |> select("TM_ID") |> pull() 
# 
# activeCat(Riley_raster) <- "TM_ID"
# 
# no_exotic.r <- ifel(Riley_raster > 0 , as.numeric(Riley_raster), NA)
# no_exotic.r <- ifel(is.na(no_exotic.r), 0, no_exotic.r)
# no_exotic.r <- ifel(Riley_raster %in% exotics, NA, no_exotic.r)
# plot(no_exotic.r)


#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------

## Crop Riley raster
# Riley_raster_full <- rast(file.path(bigDataDir,"TreeMap2022","TreeMap2022_CONUS.tif")) # National Riley tree list
Riley_raster_full <- rast(file.path(bigDataDir, "FuelMap2022", "FuelMap2022.tif")) # fuelmap
if (length(cats(Riley_raster_full)) == 8){
  Riley_raster_full <- Riley_raster_full[[1]]
}
aoi.sf <- project(aoi.sf, crs(Riley_raster_full)) 
aoi_buff.sf <- aoi.sf |> buffer(90)  # buffer so that a few extra pixels are considered when using focal window to impute out-of-state plots

Riley_raster <- Riley_raster_full |> crop(aoi_buff.sf) |>
  mask(aoi_buff.sf) #|> 
# droplevels()  # eliminate attribute table rows for plots not in the AOI.

plot(Riley_raster)
polys(aoi.sf, col = 'red', alpha = 0.5)

### the fuelmap plots should be a subset of plots from riley treelist
plot_state.df<-full_fia.df |> select(PLT_CN,STATE,INVYR) |> unique()

tm_cn_crosswalk.df <- riley_tree2022.df |>
  group_by(TM_ID, PLT_CN) |> tally() |> left_join(plot_state.df)

tm_ids <- unique(values(Riley_raster))
tm_cn_aoi.df <- tm_cn_crosswalk.df |>
  filter(TM_ID %in% tm_ids)

tm_cn_aoi.df |> group_by(STATE) |> tally()

freq(Riley_raster) |> left_join(tm_cn_aoi.df, by = c("value" = "TM_ID")) |>
  group_by(STATE) |> summarise(Pix_count = sum(count))



activeCat(TreeMap.r) <- "TM_ID"
TreeMap_tmid.r <- as.numeric(TreeMap.r)
activeCat(Riley_raster) <- "TM_ID"
FuelMap_tmid.r <- as.numeric(Riley_raster)

different_plots.r <- ifel(TreeMap_tmid.r != FuelMap_tmid.r, FuelMap_tmid.r, NA)
plot(different_plots.r)
polys(aoi.sf)

dp2.r <- ifel(TreeMap_tmid.r != FuelMap_tmid.r, 1, 0)
plot(dp2.r)
freq(dp2.r)


### Visualize origin states of riley plots
origin.r <- Riley_raster

newcat <- cats(origin.r)[[1]] |> 
  left_join(plot_state.df) |> 
  mutate(Origin = ifelse(STATE%in%c('OR','ID','MT'), "PNW", ifelse(STATE == "WA", "WA", "Foreign"))) |> 
  select("TM_ID", "Origin")

origin.r <- origin.r |> addCats(newcat)
activeCat(origin.r) <- "Origin"

plot(origin.r)


exotics <- tm_cn_aoi.df |> filter(is.na(STATE)) |> select("TM_ID") |> pull() 

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

while (sum(values(is.na(no_exotic.r))) > 500 & win < 16) {
  if (sum(values(is.na(no_exotic.r))) == empties) {win <- win + 2; print("Widening focal window.")}  # widen window if it stops shrinking
  
  
  empties <- sum(values(is.na(no_exotic.r)))
  print(paste(empties, "empty pixels remaining."))
  
  no_exotic.r <- terra::focal(no_exotic.r, w=win, fun = focal_fun, na.policy = "only")
  plot(no_exotic.r)
}

no_exotic.r <- ifel(is.na(no_exotic.r), 0, no_exotic.r)

Riley_raster <- no_exotic.r |>
  mask(aoi.sf)

# writeRaster(Riley_raster, file.path(outDir, "TreeMap2022_OWNF_pnw_plots_only.tif"))
writeRaster(Riley_raster, file.path(outDir, "FuelMap2022_OWNF_pnw_plots_only.tif"))


### pare down treelist to just those in the modified map
riley_tree.df <- riley_tree.df_aoi |> filter(TM_ID %in% values(Riley_raster))

### are any plots in the riley tree list but not in the FIA database
riley_tree.df |> filter(!PLT_CN%in%full_fia.df$PLT_CN)

### Drop trees from Riley tree list without DBH. These are probably seedlings?
riley_tree.df |> filter(is.na(DIA)|DIA == 0)

write.csv(riley_tree.df, file.path(outDir, "TreeMap2022_OWNF_treelist_pnw_plots_only.csv"))

################################################################################
##### Additional Analysis ######################################################
################################################################################

# Riley_raster <- rast(file.path(outDir, "TreeMap2022_OWNF_pnw_plots_only.tif"))
Riley_raster <- rast(file.path(outDir, "FuelMap2022_OWNF_pnw_plots_only.tif"))
riley_tree.df <- read.csv(file.path(outDir, "TreeMap2022_OWNF_treelist_pnw_plots_only.csv"))
 

### Load all requested variables
vars_list <- read_lines(file.path(outDir, "VarList_VF_2.txt"),skip_empty_rows = T)
vars.df <- data.frame("fia_table.var" = vars_list) |> mutate(
  fia_table = str_split_i(fia_table.var, "\\.", 1),
  var = str_split_i(fia_table.var, "\\.", 2),
)

cn_tmid_crosswalk.df <- riley_tree.df |>
  select(PLT_CN, TM_ID) |>
  unique()

plot_freq.df <- freq(Riley_raster) |> 
  rename(TM_ID = value, OWNF_count = count) |>
  left_join(cn_tmid_crosswalk.df) |>
  filter(TM_ID > 0) |>
  select(!layer)

for (fia_tab in unique(vars.df$fia_table)){
  
  if (fia_tab %in% c("POP_STRATUM", "POP_STRATUM_ASSGN", "REF_SPECIES", "REF_PLANT_DICTIONARY", 
                     "REF_FOREST_TYPE", "REF_FOREST_TYPE_GROUP", "REF_HABTYP_DESCRIPTION", "REF_HABTYP_PUBLICATION", 
                     "REF_DAMAGE_AGENT")){next}
  
  table.vars <- vars.df |>
    filter(fia_table == fia_tab) |>
    select(var) |> pull()

  table.vars = c("PLT_CN", table.vars)

  if (file.exists(file.path(bigDataDir, "FIA", paste0("WA_FIA/", "WA_", fia_tab, ".csv")))){
    states <- c("WA", "OR", "MT", "ID")
    fia_full.df <- file.path(bigDataDir, "FIA", paste0(states, "_FIA/", states, "_", fia_tab, ".csv")) |>
      map_df(~read_csv(., col_types = "cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc")) |>
      readr::type_convert() 
  } else {
    fia_full.df <- read.csv(file.path(bigDataDir, "FIA", "PNWRS_CAORWA", paste0("PNWRS_CAORWACAORWA_", fia_tab, ".csv")))
  }
  
  
  
  if(fia_tab == "PLOT"){
    pop_stratum.df <- file.path(bigDataDir, "FIA", paste0(states, "_FIA/", states, "_POP_STRATUM.csv")) |>
      map_df(~read_csv(.)) |>
      mutate(STRATUM_CN = CN) |>
      select(vars.df |>
               filter(fia_table == "POP_STRATUM") |>
               select(var) |> pull())
    
    pop_stratum_assgn.df <- file.path(bigDataDir, "FIA", paste0(states, "_FIA/", states, "_POP_PLOT_STRATUM_ASSGN.csv")) |>
      map_df(~read_csv(.)) |>
      select(STRATUM_CN, PLT_CN)
    
    table.vars <- c(table.vars, vars.df |> filter(fia_table == "POP_STRATUM") |> select(var) |> pull())
    
    fia_full.df <- fia_full.df |>
      mutate(PLT_CN = CN) |>
      left_join(pop_stratum_assgn.df) |>
      left_join(pop_stratum.df)
  } else if (fia_tab %in% c("PLOTGEOM", "PLOTSNAP")){
    fia_full.df <- fia_full.df |>
      mutate(PLT_CN = CN)
  }
  
  fia_full.df <- fia_full.df |>
    select(all_of(table.vars)) |>
    inner_join(plot_freq.df)  # right join filters out plots not in riley dataset
  
  print(paste("DF for", fia_tab, "has", nrow(fia_full.df), "rows."))
  
  write_csv(fia_full.df, file.path(outDir, paste0("OWNF_", fia_tab, ".csv")))
}



df <- plot_freq.df |>
  arrange(-OWNF_count) |>
  mutate(rank = row_number()) |>
  mutate(pct = rank / max(rank) * 100) |>
  mutate(cumulative_pix = cumsum(OWNF_count)) |>
  mutate(pct_landscape = cumulative_pix / max(cumulative_pix) * 100)

ggplot(df, aes(x = rank, y = pct_landscape)) + geom_point(col = "seagreen") + xlab("Number of plots") + ylab("Percentage of OWNF") + lims(y = c(0,100))







#####
pop_stratum.df <- file.path(bigDataDir, "FIA", paste0(states, "_FIA/", states, "_POP_STRATUM.csv")) |>
  map_df(~read_csv(.)) |>
  mutate(STRATUM_CN = CN) |>
  select(!c(CN, CREATED_DATE, MODIFIED_DATE))

pop_stratum_assgn.df <- file.path(bigDataDir, "FIA", paste0(states, "_FIA/", states, "_POP_PLOT_STRATUM_ASSGN.csv")) |>
  map_df(~read_csv(.)) 

df <- pop_stratum.df |> 
  inner_join(pop_stratum_assgn.df) |>
  inner_join(plot_freq.df)

write_csv(df, file.path(outDir, "OWNF_STRATUM_PLT_MAP.csv"))  








