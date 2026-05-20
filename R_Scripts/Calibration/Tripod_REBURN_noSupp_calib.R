# library(terra)
# library(tidyverse)
# library(tidyterra)
# 
# REBURNDir <- file.path("F:", "Tripod_Sim01d1_Thomas", "FireSeverity_Annual")
# landisOutputDir <- landisRuns[1]
# 
# test <- rast(file.path(REBURNDir, "Severity_0303.tif")) |> as.int()
# 
# ReburnFires <- dir(REBURNDir)[grepl(".tif$", dir(REBURNDir))]
# 
# # REBURNStack <- as.int(rast(file.path(REBURNDir, ReburnFires)))
# 
# REBURN.df <- data.frame(
#   "Year" = NULL,
#   "AreaHigh" = NULL,
#   "AreaMod" = NULL,
#   "AreaUBLow" = NULL
# )
# 
# for (tif in ReburnFires){
#   print(tif)
#   r <- rast(file.path(REBURNDir, tif)) |>
#     as.int()
#   
#   ext(r) <- ext(pwg.r)
#   freqs <- freq(r)
#   
#   as <- ifelse(3 %in% freqs$value, freqs |> filter(value == 3) |> pull(count), 0)
#   am <- ifelse(2 %in% freqs$value, freqs |> filter(value == 2) |> pull(count), 0)
#   al <- ifelse(1 %in% freqs$value, freqs |> filter(value == 1) |> pull(count), 0)
#   au <- ifelse(0 %in% freqs$value, freqs |> filter(value == 0) |> pull(count), 0)
#   
#   df <- data.frame(
#     "Year" = str_remove(str_remove(tif, "Severity_"), ".tif"),
#     "AreaHigh" = as,
#     "AreaMod" = am,
#     "AreaLow" = al,
#     "AreaUB" = au
#   )
#   
#   REBURN.df <- bind_rows(REBURN.df, df)
# }
# 
# REBURN.df <- REBURN.df |>
#   mutate(Source = "REBURN")
# 
# 
# severityStack.r <- rast(file.path(landisOutputDir, 'social-climate-fire', 'fire-dnbr-yr.tif'))
# severityStackClassified.r <- ifel(severityStack.r<=1, NA, severityStack.r) |>  # have to class 0 as NA so that median window only operates on unburned cells
#   focal(w = 3, fun = median, na.rm=T, pad = TRUE, na.policy = 'only') |>
#   classify(rcl=severity.reclass.df,include.lowest=T)
# 
# LANDIS.df <- severityStackClassified.r |>
#   as.data.frame() |>
#   pivot_longer(cols = everything(), names_to = "Year", values_to = "SeverityCode") |>
#   filter(!is.na(SeverityCode)) |>
#   left_join(data.frame("SeverityCode" = c(1,2,3,4), "Severity" = c("UB", "Low", "Mod", "High"))) |>
#   mutate(Severity = factor(Severity, levels = c("UB", "Low", "Mod", "High"), labels = c("UB", "Low", "Mod", "High"))) |>
#   group_by(Year, Severity) |>
#   summarise(Area = n()) |>
#   pivot_wider(names_from = "Severity", values_from = "Area", names_prefix = "Area", names_sep = "", values_fill = 0, names_expand = T) |>
#   mutate(Source = "LANDIS")
# 
# 
# df <- bind_rows(REBURN.df, LANDIS.df) |>
#   mutate(AreaUBLow = AreaUB + AreaLow) |>
#   select(!c(AreaUB, AreaLow)) |>
#   pivot_longer(cols = starts_with("Area"), names_to = "Severity", names_prefix = "Area", values_to = "Area")
# 
# ggplot(df, aes(x = log(Area), colour = Source)) + geom_density() + facet_wrap(~Severity)
# 
# 
# ### Make map of # times burned at each severity level
# 
# #### Low
# LANDISlow.r <- ifel(severityStackClassified.r %in% c(1,2), 1, 0) |>
#   sum()
# 
# REBURNlow.r <- rast(file.path(REBURNDir, '..', "SevLow_Count.tif"))
# 
# par(mfrow = c(1, 2))
# plot(LANDISlow.r/nlyr(severityStack.r), range = c(0, 0.5))
# plot(REBURNlow.r[[1]]/nrow(REBURN.df), range = c(0, 0.5))
# 
# #### Mod
# LANDISmod.r <- ifel(severityStackClassified.r %in% c(3), 1, 0) |>
#   sum()
# 
# REBURNmod.r <- rast(file.path(REBURNDir, '..', "SevMod_Count.tif"))
# 
# par(mfrow = c(1, 2))
# plot(LANDISmod.r/nlyr(severityStack.r), range = c(0, 0.15))
# plot(REBURNmod.r[[1]]/nrow(REBURN.df), range = c(0, 0.15))
# 
# #### High
# LANDIShigh.r <- ifel(severityStackClassified.r %in% c(4), 1, 0) |>
#   sum()
# 
# REBURNhigh.r <- rast(file.path(REBURNDir, '..', "SevHigh_Count.tif"))
# 
# par(mfrow = c(1, 2))
# plot(LANDIShigh.r/nlyr(severityStack.r), range = c(0, 0.1))
# plot(REBURNhigh.r[[1]]/nrow(REBURN.df), range = c(0, 0.1))
# 
# ### FRI
# #### Low
# LANDISFRIlow.r <- nlyr(severityStack.r)/LANDISlow.r
# plot(LANDISFRIlow.r, range = c(1, 100), col = rainbow(50))
# 
# REBURNFRIlow.r <- nrow(REBURN.df)/REBURNlow.r[[1]]
# plot(REBURNFRIlow.r, range = c(1, 100), col = rainbow(50))

################################################################################
### From the REBURN data let’s start with event size distributions, L, M, and H severity patch size distributions, area burned, and area burned by severity class in each PWG
library(terra)
library(tidyverse)
library(tidyterra)
library(landscapemetrics)

REBURNDir <- file.path("F:", "Tripod_Sim01d1_Thomas", "FireSeverity_Annual")
LANDIS.EXTENT<-'Tripod'
Dir <- file.path('F:/2025_Q4_Scenarios', LANDIS.EXTENT)

bigDataDir<-'F:/LANDIS_Input_Data_Prep/BigData'
dataDir<-'F:/LANDIS_Input_Data_Prep/Data'
modelDir<-'F:/V8_Models'

test <- rast(file.path(REBURNDir, "Severity_0303.tif")) |> as.int()

MTBS_dir <- file.path(dataDir,'MTBS_and_FOD_Fires', LANDIS.EXTENT)

pwg.r <-  rast(file.path(dataDir, "PWG", paste0("PWG_", LANDIS.EXTENT, ".tif")))
zonemap.df <- data.frame("PWG" = unlist(unique(pwg.r)), "zone" = 1:length(unlist(unique(pwg.r))))  # Needed for stupid freq function implementation

#### Save a dataframe with event size distribution
firePerims.shp <- vect(file.path(REBURNDir, '..', "FirePerimeters.gpkg")) |> project(crs(pwg.r))

plot(pwg.r)
plot(firePerims.shp[1000], add = T)

# evsizes.df <- data.frame("EventSize" = expanse(firePerims.shp, unit = "ha"))
# hist(log10(evsizes.df$EventSize))
# 
# write.csv(evsizes.df, file.path(REBURNDir, "..", "REBURN_event_sizes.csv"))
# 
# #### Save a dataframe with L, M, H, patch size distributions
# ReburnFires <- dir(REBURNDir)[grepl(".tif$", dir(REBURNDir))]
# # REBURNStack.r <- as.int(rast(file.path(REBURNDir, ReburnFires)))
# # plot(REBURNStack.r)
# 
# reburn.patch.df <- data.frame()
# sev.pwg.df <- data.frame()
# 
# for (tif in ReburnFires){
#   print(tif)
#   r <- rast(file.path(REBURNDir, tif)) |> project(crs(pwg.r), method = "near") |>
#     as.int() |> resample(pwg.r, method = 'near')
# 
#   # ext(r) <- ext(pwg.r)
#   
#   yr.patch.df <- lsm_p_area(r) |> mutate(layer = tif)
#   
#   reburn.patch.df <- bind_rows(reburn.patch.df, yr.patch.df)
#   
#   yr.sev.pwg.df <- freq(r, zones = pwg.r) |> left_join(zonemap.df)  # severity count by PWG by year
#   sev.pwg.df <- sev.pwg.df |> bind_rows(yr.sev.pwg.df)
# }
# 
# reburn.patch.df <- reburn.patch.df |>
#   rename(Severity = class) |>
#   pivot_wider(names_from = "metric", values_from = "value") |>
#   mutate(Severity = as.factor(Severity + 1),
#          layer = as.numeric(str_extract(layer, "(\\d+)")))
# 
# # ggplot(data = reburn.patch.df |> filter(class != 9), aes(x = log(value))) + geom_histogram() + facet_wrap(~class)
# write.csv(reburn.patch.df, file.path(REBURNDir, "..", "REBURN_patch_sizes.csv"))
# 
# 
# #### Save a datagrame with distribution of annual area burned
# annual.area.df <- reburn.patch.df |> group_by(layer) |> summarise(area = sum(area))
# # hist(annual.area.df$Area)
# 
# #### Save a dataframe with annual area burned by severity class per PWG
# sev.pwg.df <- sev.pwg.df |>
#   rename(Severity = value) |>
#   mutate(
#     Severity = as.factor(Severity + 1),
#     Area = count * 0.81,
#     Arealog10 = log10(Area)
#     )
# 
# 
# ggplot(data = sev.pwg.df |> filter(Severity != 9, PWG > 12), aes(y = Arealog10, x = as.factor(PWG), fill = Severity)) + 
#   geom_boxplot() + 
#   # facet_wrap(~PWG) + 
#   scale_fill_manual(values = c('darkgreen','darkseagreen','goldenrod1','firebrick4')) +
#   ggtitle("Patch size distribution (log-transformed) by PWG for REBURN simulation") +
#   xlab("PWG") + ylab("Patch size, log10 transformed")
# 
# write.csv(sev.pwg.df, file.path(REBURNDir, "..", "REBURN_pwg_burned_area.csv"))
# 
# 
# ### Initial plots
# # From the REBURN data let’s start with event size distributions, L, M, and H severity patch size distributions, area burned, and area burned by severity class in each PWG
# ## event size dist
# ggplot(data = evsizes.df, aes(x = log10(EventSize))) + geom_histogram(fill = "gray", color = "black") + ggtitle("REBURN Event Size Distribution") +
#   xlab("Event Size (log10 transformed)") + ylab("Event count")
# 
# ## L, M, H patch size dists
# ggplot(data = reburn.patch.df |> filter(Severity != 9), aes(x = log10(area), fill = Severity)) + geom_histogram() + facet_wrap(~Severity) +
#   scale_fill_manual(values = c('darkgreen','darkseagreen','goldenrod1','firebrick4')) +
#   ggtitle("Patch size distributions (log10 transformed)") + 
#   xlab("Patch size (log10 transformed)") + ylab("Number of patches")
# 
# 
# ## Annual area burned
# ggplot(data = annual.area.df, aes(x = log10(area))) + geom_histogram() +
#   ggtitle("Annual burned area (log10 transformed)")
# 
# ## Area burned by severity class in each pwg
# ggplot(data = sev.pwg.df |> filter(Severity != 9, PWG > 12), aes(y = Arealog10, x = as.factor(PWG), fill = Severity)) + 
#   geom_boxplot() + 
#   # facet_wrap(~PWG) + 
#   scale_fill_manual(values = c('darkgreen','darkseagreen','goldenrod1','firebrick4')) +
#   ggtitle("Annual area burned (log-transformed) by Severity & PWG for REBURN simulation") +
#   xlab("PWG") + ylab("Annual area burned, log10 transformed")

### make mtbs event sizes, patch sizes, burned area by pwg
# mtbs.dir<-'F:/LANDIS_Input_Data_Prep/BigData/MTBS_1984_2019_WA_State'

obs.ignitions.df<-vect(file.path(bigDataDir,"Fire_Ignitions_1992_2015_RDS-2013-0009/Fires_1992-2015_WA/Fires_1992-2015_WA.shp")) |>
  project(crs(pwg.r)) |> 
  crop(extend(ext(pwg.r),c(10000, 10000))) |> 
  as.data.frame() |>
  select(c('SOURCE_R_1','FIRE_NAME','DISCOVERY_','FIRE_YEAR','FIRE_SIZE','STAT_CAU_1','MTBS_ID')) |>
  rename(
    'IgnitionType' = 'STAT_CAU_1',
    'SIZE_ac_from_igntion_df' = 'FIRE_SIZE',
    'Event_ID' = 'MTBS_ID'
  ) |>
  mutate(SIZE_ha_from_igntion_df = SIZE_ac_from_igntion_df*0.4046864) |>  # round size for binned counts
  filter(SIZE_ac_from_igntion_df<1000) |>  ## Drop big fires, as these should all be duplicates of MTBS:
  mutate(
    EventSize = SIZE_ha_from_igntion_df, 
    Source = "MTBS"
  ) |>
  filter(is.na(Event_ID)) |>
  select(FIRE_YEAR, EventSize, Source, Event_ID)


evsizes.mtbs.df <- read.csv(file.path(bigDataDir, 'MTBS_1984_2019_WA_State', 'MTBS_Raster_Summary_Stats_Tripod.csv')) |>
  mutate(EventSize = Area_ha, Source = "MTBS") |>
  select(FIRE_YEAR, Event_ID, EventSize, Source) |>
  bind_rows(obs.ignitions.df)
  

mtbs.severity.r <- rast(file.path(dataDir, "MTBS_and_FOD_Fires", LANDIS.EXTENT, paste0("Observed_fires_", 1984:2019, ".tif")))
mtbs.severity.r <- ifel(mtbs.severity.r < 5, NA, mtbs.severity.r) |>
  terra::classify(rcl=severity.reclass.df,include.lowest=T)

mtbs.patch.df  <- lsm_p_area(mtbs.severity.r) |>
  rename(Severity = class) |>
  pivot_wider(names_from = "metric", values_from = "value") |>
  mutate(Severity = as.factor(Severity), Source = "MTBS") |>
  filter(!is.na(Severity))

sev.pwg.mtbs.df <- freq(mtbs.severity.r, zones = pwg.r) |> left_join(zonemap.df) |>
  rename(Severity = value) |>
  mutate(
    Severity = as.factor(Severity),
    Area = count * 0.81,
    Arealog10 = log10(Area),
    Source = "MTBS"
  )  






################################################################################
### Comparison plots
evsizes.df <- read.csv(file.path(REBURNDir, "..", "REBURN_event_sizes.csv")) 
reburn.patch.df <- read.csv(file.path(REBURNDir, "..", "REBURN_patch_sizes.csv"))|>
  mutate(Severity = as.factor(Severity))
sev.pwg.df <- read.csv(file.path(REBURNDir, "..", "REBURN_pwg_burned_area.csv"))|>
  filter(Severity != 9) |>
  mutate(Severity = as.factor(Severity+1))

#### Event size distribution
df <- evsizes.df |> 
  mutate(Source = "REBURN") |>
  bind_rows(test_landis$evs.df |> mutate(Source = "LANDIS")) |>
  bind_rows(evsizes.mtbs.df) |> filter(EventSize > 10)

ggplot(data = df, aes(x = log10(EventSize), fill = Source, color = Source)) + geom_density(alpha = 0.25) + ggtitle("Event Size Distribution") +
  xlab("Event Size (log10 transformed)") + ylab("Event count") #+ facet_wrap(~Source)


# fire.size.classes.log10<-data.frame('Fire_size'=c('0-1','1-10','10-100','100-1000','1000-10000','10000+'),
#                                     'min'=c(0,1,10,100,1000,10000),
                                    # 'max'=c(1,10,100,1000,10000,100000))

simlength.df <- data.frame(
  "Source" = c("REBURN", "LANDIS", "MTBS", "REBURN", "LANDIS", "MTBS"),
  "SimLen" = c(3000, max(unique(test_landis$sev.pwg.df$layer)), 35, 3000, max(unique(test_landis$sev.pwg.df$layer)), 24),  # MTBS is technically mixed with 24 years of FOD data, 
  "Size" = c("Big", "Big", "Big", "Small", "Small", "Small")
)

df <- evsizes.df |> 
  mutate(Source = "REBURN") |>
  bind_rows(test_landis$evs.df |> mutate(Source = "LANDIS")) |>
  bind_rows(evsizes.mtbs.df) |> filter(EventSize > 10) |>
  mutate(Size = ifelse(EventSize > 600, "Big", "Small")) |>
  left_join(simlength.df) |> 
  mutate(bin.log10 = cut(EventSize, breaks = c(0, 1, 10, 100, 1000, 10000, 100000))) |>
  group_by(Source, bin.log10) |>
  summarise(Freq = n()/mean(SimLen))


ggplot(data = df, aes(x = bin.log10, y=Freq, fill = Source)) + geom_col(position = "dodge")


#### Patch size distribution
df <- reburn.patch.df |> 
  mutate(Source = "REBURN") |>  # severity scale is offset by 1 for REBURN
  bind_rows(test_landis$ptch.df |> mutate(Source = "LANDIS")) |>
  bind_rows(mtbs.patch.df) |>
  select(Severity, area, layer, Source) |> 
  mutate(patchID = row_number()) |>
  pivot_wider(names_from = "Severity", values_from = "area", names_prefix = "Sev", values_fill = 0) |>
  mutate(SevHigh = Sev4, SevMod = Sev3, SevLow = Sev1+Sev2) |>
  select(!c(Sev1, Sev2, Sev3, Sev4, Sev10)) |>
  pivot_longer(cols = c(SevHigh, SevMod, SevLow), names_to = "Severity", names_prefix = "Sev", values_to = "area") |>
  mutate(Severity = factor(Severity, levels = c("Low", "Mod", "High")))

ggplot(data = df |> filter(Severity != 10), aes(x = log10(area), fill = Source)) + geom_density(alpha = 0.25) + facet_wrap(~Severity) +
  # scale_fill_manual(values = c('darkgreen','darkseagreen','goldenrod1','firebrick4')) +
  ggtitle("Patch size distributions (log10 transformed)") + 
  xlab("Patch size (log10 transformed)") + ylab("Number of patches") + scale_y_continuous(limits = c(0, 3))

#### Annual area burned
df <- df |> group_by(layer, Source) |>
  summarise(area = sum(area))

ggplot(data = df, aes(x = log10(area), fill = Source)) + geom_density(alpha = 0.25) +
  ggtitle("Annual burned area (log10 transformed)") #+ 
  # facet_wrap(~Source)

#### Annual area burned by severity
df <- sev.pwg.df |> 
  mutate(Source = "REBURN") |>
  bind_rows(test_landis$sev.pwg.df |> mutate(Source = "LANDIS")) |>
  bind_rows(sev.pwg.mtbs.df) |>
  filter(PWG != 99) |>
  pivot_wider(names_from = "Severity", values_from = "Area", names_prefix = "Sev", values_fill = 0) |>
  mutate(SevHigh = Sev4, SevMod = Sev3, SevLow = Sev1+Sev2) |>
  select(!c(Sev1, Sev2, Sev3, Sev4)) |>
  pivot_longer(cols = c(SevHigh, SevMod, SevLow), names_to = "Severity", names_prefix = "Sev", values_to = "area") |>
  mutate(Severity = factor(Severity, levels = c("Low", "Mod", "High")))

ggplot(data = df |> filter(Severity != 10, PWG > 12), aes(y = log10(area), x = as.factor(PWG), fill = Severity)) + 
  geom_boxplot() + 
  facet_wrap(~Source) + 
  scale_fill_manual(values = c('darkseagreen','goldenrod1','firebrick4')) +
  ggtitle("Annual area burned (log-transformed) by Severity & PWG for REBURN simulation") +
  xlab("PWG") + ylab("Annual area burned, log10 transformed")

#### FRI
reburn.fri.low.r <- 3300 / rast(file.path(REBURNDir, '..', "SevLow_Count.tif"))[[1]]
reburn.fri.low.r <- reburn.fri.low.r |> project(crs(pwg.r), method = "bilinear") |> resample(pwg.r, method = 'bilinear')

reburn.fri.mod.r <- 3300 / rast(file.path(REBURNDir, '..', "SevMod_Count.tif"))[[1]]
reburn.fri.mod.r <- reburn.fri.mod.r |> project(crs(pwg.r), method = "bilinear") |> resample(pwg.r, method = 'bilinear')

reburn.fri.high.r <- 3300 / rast(file.path(REBURNDir, '..', "SevHigh_Count.tif"))[[1]]
reburn.fri.high.r <- reburn.fri.high.r |> project(crs(pwg.r), method = "bilinear") |> resample(pwg.r, method = 'bilinear')

friComp.r <- c(reburn.fri.low.r, reburn.fri.mod.r, reburn.fri.high.r, test_landis$fri.low.r, test_landis$fri.mod.r, test_landis$fri.high.r)
names(friComp.r) <- c("REBURN FRI Low", "REBURN FRI Mod", "REBURN FRI High", "LANDIS FRI Low", "LANDIS FRI Mod", "LANDIS FRI High")

plot(friComp.r, range = c(0, 500), type = "continuous", fill_range = T)

# #### Low
# LANDISlow.r <- ifel(severityStackClassified.r %in% c(1,2), 1, 0) |>
#   sum()
# 
# REBURNlow.r <- rast(file.path(REBURNDir, '..', "SevLow_Count.tif"))
# 
# par(mfrow = c(1, 2))
# plot(LANDISlow.r/nlyr(severityStack.r), range = c(0, 0.5))
# plot(REBURNlow.r[[1]]/nrow(REBURN.df), range = c(0, 0.5))
# 
# #### Mod
# LANDISmod.r <- ifel(severityStackClassified.r %in% c(3), 1, 0) |>
#   sum()
# 
# REBURNmod.r <- rast(file.path(REBURNDir, '..', "SevMod_Count.tif"))
# 
# par(mfrow = c(1, 2))
# plot(LANDISmod.r/nlyr(severityStack.r), range = c(0, 0.15))
# plot(REBURNmod.r[[1]]/nrow(REBURN.df), range = c(0, 0.15))
# 
# #### High
# LANDIShigh.r <- ifel(severityStackClassified.r %in% c(4), 1, 0) |>
#   sum()
# 
# REBURNhigh.r <- rast(file.path(REBURNDir, '..', "SevHigh_Count.tif"))
# 
# par(mfrow = c(1, 2))
# plot(LANDIShigh.r/nlyr(severityStack.r), range = c(0, 0.1))
# plot(REBURNhigh.r[[1]]/nrow(REBURN.df), range = c(0, 0.1))
# 
# ### FRI
# #### Low
# LANDISFRIlow.r <- nlyr(severityStack.r)/LANDISlow.r
# plot(LANDISFRIlow.r, range = c(1, 100), col = rainbow(50))
# 
# REBURNFRIlow.r <- nrow(REBURN.df)/REBURNlow.r[[1]]
# plot(REBURNFRIlow.r, range = c(1, 100), col = rainbow(50))


