library(terra)
library(tidyverse)
library(tidyterra)
library(cffdrs)

dataDir<-'F:/LANDIS_Input_Data_Prep/Data'

v7_evlog <- read.csv("F:\\2025_Q3_Scenarios\\FuelFix5\\Sim690d8a_WenEnt_baseclim_Sc14_20250821_0005\\scrapple-events-log.csv")
hist(v7_evlog$MeanFWI)
v8_evlog <- read.csv("F:\\2025_Q3_Scenarios\\TEST_WenEnt\\LANDIS_Sim_WenEnt_baseclim_Sc2_20250821_1329\\socialclimatefire-events-log.csv")
hist(v8_evlog$MeanFWI)

v8_input_log <- read.csv("F:\\V8_Models\\LANDIS_Sim_Tripod_baseclim_Sc0_20250904_1554\\Climate-future-monthly-input-log.csv")|>
  mutate(EcoregionName = str_remove_all(EcoregionName, ' ')) |>
  filter(EcoregionName == 'eco12.170200062')

v8_climate_inputs <- read.csv(file.path("Tripod", 'zClimate_Library', "CLIMATE_2020-2120_baseline_Tripod.csv")) |> select(Year, Month, Day, Variable, eco12.170200062) |>
  filter(Year %in% v8_input_log$CalendarYear) |>
  pivot_wider(names_from = "Variable", values_from = "eco12.170200062") |>
  mutate(windspeed = windspeed)  #m/s to km/h
  

v7_input_log <- read.csv(file.path("F:\\LANDIS Runs\\Tripod_LANDIS_model\\LANDIS_MHTest_20240604_1814\\Climate-future-input-log.csv")) |>
  filter(Year <= 2024) |>
  mutate(EcoregionName = str_remove_all(EcoregionName, ' ')) |>
  filter(EcoregionName == 'eco12.170200062') |>
  mutate(Date = ymd("2020-01-01") + days(Timestep + (Year-2020)*365)) |>
  mutate(yr = year(Date),
         mon = month(Date),
         day = day(Date))

v7_input_log_monthly <- v7_input_log |>
  mutate(month = cut(Timestep, breaks = c(-1, 31,  59,  90,  120, 151,  181, 212, 243, 273, 304, 334, 366), labels = 0:11)) |>
  group_by(Year, month) |>
  summarise(ppt = mean(ppt), FWI = mean(FWI), BUI = mean(BuildUpIndex), DC = mean(DroughtCode), DMC = mean(DuffMoistureCode), FFMC = mean(FineFuelMoistureCode))


v8_input_log_daily <- read.csv("F:\\V8_Models\\LANDIS_Sim_Tripod_baseclim_Sc0_20250904_1554\\Climate-future-daily-input-log.csv") |>
  mutate(EcoregionName = str_remove_all(EcoregionName, ' ')) |>
  filter(EcoregionName == 'eco12.170200062') |>
  mutate(Date = ymd("2020-01-01") + days(Day + (Year-1)*365)) |>
  mutate(yr = year(Date),
         mon = month(Date),
         day = day(Date))

v8_input_log_daily2 <- read.csv("F:\\V8_Models\\LANDIS_Sim_Tripod_baseclim_Sc0_20250910_1817\\Climate-future-daily-input-log.csv") |>
  mutate(EcoregionName = str_remove_all(EcoregionName, ' ')) |>
  filter(EcoregionName == 'eco12.170200062') |>
  mutate(Date = ymd("2020-01-01") + days(Day + (Year-1)*365)) |>
  mutate(yr = year(Date),
         mon = month(Date),
         day = day(Date))

compare_hists <- function(v7var, v8var, inputvar = NULL){
  v7data <- v7_input_log |>
    select(all_of(v7var)) |>
    rename("Var" = v7var) |>
    mutate(Source = 'v7')
  
  v8data <- v8_input_log |>
    select(all_of(v8var)) |>
    rename("Var" = v8var) |>
    mutate(Source = 'v8')
  
  if(!is.null(inputvar)){
    v8indata <- v8_climate_inputs |>
      select(all_of(inputvar)) |>
      rename("Var" = inputvar) |>
      mutate(Source = 'Input to v8')
  } else {
    v8indata <- data.frame()
  }
  
  
  df <- bind_rows(v7data, v8data, v8indata)
  
  ggplot(data = df, aes(x = Var, fill = Source)) + geom_density(alpha = 0.4) + ggtitle(v8var)
}


compare_hists('ppt', 'Precip', 'precip')
compare_hists('min_airtemp', 'MinTemp', 'mintemp')
compare_hists('windspeed','WindSpeed', 'windspeed')
compare_hists('min_relativehumidity', 'MinRH', 'minrh')
compare_hists('max_relativehumidity', 'MaxRH', 'maxrh')
compare_hists("DuffMoistureCode", "DuffMoistureCode", NULL)
compare_hists("DroughtCode", "DroughtCode", NULL)
compare_hists("BuildUpIndex", "BuildUpIndex", NULL)
compare_hists("FineFuelMoistureCode", "FineFuelMoistureCode", NULL)
compare_hists("FWI", "FireWeatherindex", NULL)

par(mfrow = c(2, 1))
hist(v8_input_log$FireWeatherindex)
hist(v7_input_log$FWI)

hist(v8_input_log$Precip)
hist(v7_input_log$ppt)

hist(v8_input_log$MinTemp)
hist(v7_input_log$min_airtemp)

hist(v8_input_log$FireWeatherindex)
hist(v7_input_log$FWI)

hist(v8_input_log$FireWeatherindex)
hist(v7_input_log$FWI)


v7_calculated_fwi <- read.csv(file.path(dataDir,paste0('Climate_and_FWI_','WenEnt','.csv')))

test<-v8_climate_inputs |>
  group_by(Month, Year) |>
  summarise(precip = sum(precip)) |>
  arrange(Year, Month)


hist <- read.csv(file.path(Dir, 'Tripod' ,paste0('climvars_hist_', 'Tripod', '.csv'))) |> filter(statistic=='MEAN') |> select(!X) |>
  select(DateTime, variable, X12.170200062) |>
  pivot_wider(names_from = variable, values_from = X12.170200062) |>
  separate(DateTime, into = c('Year', 'Month', 'Day'), sep='-')

# ### see if converting ppt throws it off
# aoi_ext = ext(project(ecos.r, 'EPSG:4326'))
# lat = unname(0.5*(aoi_ext$ymin + aoi_ext$ymax))
# lon = unname(0.5*(aoi_ext$xmin + aoi_ext$xmax))
# testfwi.df<-data.frame('lat'=lat,'long'=lon,'yr' = v7_calculated_fwi$yr,'mon' = v7_calculated_fwi$mon,'day' = v7_calculated_fwi$day,
#                    'temp' = v7_calculated_fwi$tasmax,'rh' = v7_calculated_fwi$rhsmin,'ws' = v7_calculated_fwi$WndSpd,'prec' = v7_calculated_fwi$pr/10)
# 
# testfwi.df<-fwi(testfwi.df)
# 
# 
# hist(testfwi.df$FWI)
# 
# df <- rbind(
#   data.frame("FWI" = v7_calculated_fwi$FWI, "Source" = "Standard inputs"),
#   data.frame("FWI" = testfwi.df$FWI, "Source" = "Precip to cm")
# )
# 
# ggplot(data = df, aes(x = FWI, fill = Source)) + geom_density()

### try calculating FWI from v8 input data
aoi_ext = ext(project(rast(file.path("Tripod", "ECOREGIONS_Tripod.tif")), 'EPSG:4326'))
lat = unname(0.5*(aoi_ext$ymin + aoi_ext$ymax))
lon = unname(0.5*(aoi_ext$xmin + aoi_ext$xmax))
testfwi1.df <- fwi(data.frame('lat'=lat,'long'=lon,'yr' = v8_input_log$Year,'mon' = v8_input_log$Month,'day' = v8_climate_inputs$Day,
                        'temp' = v8_climate_inputs$maxtemp,'rh' = v8_climate_inputs$minrh,'ws' = v8_climate_inputs$windspeed*3.6,'prec' = v8_climate_inputs$precip)) |>
  group_by(YR, MON) |> summarise(FWI = mean(FWI), FFMC = mean(FFMC), DMC = mean(DMC), DC = mean(DC), BUI = mean(BUI)) |>
  mutate(Source = 'wind km/s, precip cm')
testfwi2.df <- fwi(data.frame('lat'=lat,'long'=lon,'yr' = v8_input_log$Year,'mon' = v8_input_log$Month,'day' = v8_climate_inputs$Day,
                              'temp' = v8_climate_inputs$maxtemp,'rh' = v8_climate_inputs$minrh,'ws' = v8_climate_inputs$windspeed*3.6,'prec' = v8_climate_inputs$precip*10))|>
  group_by(YR, MON) |> summarise(FWI = mean(FWI), FFMC = mean(FFMC), DMC = mean(DMC), DC = mean(DC), BUI = mean(BUI)) |>
  mutate(Source = 'wind km/s, precip mm (correct FWI inputs)')
testfwi3.df <- fwi(data.frame('lat'=lat,'long'=lon,'yr' = v8_input_log$Year,'mon' = v8_input_log$Month,'day' = v8_climate_inputs$Day,
                              'temp' = v8_climate_inputs$maxtemp,'rh' = v8_climate_inputs$minrh,'ws' = v8_climate_inputs$windspeed,'prec' = v8_climate_inputs$precip*10))|>
  group_by(YR, MON) |> summarise(FWI = mean(FWI), FFMC = mean(FFMC), DMC = mean(DMC), DC = mean(DC), BUI = mean(BUI)) |>
  mutate(Source = 'winspdeed m/s, precip mm')
testfwi4.df <- fwi(data.frame('lat'=lat,'long'=lon,'yr' = v8_input_log$Year,'mon' = v8_input_log$Month,'day' = v8_climate_inputs$Day,
                              'temp' = (v8_climate_inputs$maxtemp+v8_climate_inputs$mintemp)/2,'rh' = (v8_climate_inputs$minrh+v8_climate_inputs$minrh)/2,'ws' = v8_climate_inputs$windspeed*3.6,'prec' = v8_climate_inputs$precip*10))|>
  group_by(YR, MON) |> summarise(FWI = mean(FWI), FFMC = mean(FFMC), DMC = mean(DMC), DC = mean(DC), BUI = mean(BUI)) |>
  mutate(Source = 'wind km/s, precip mm (correct FWI inputs), avgs')



df <- bind_rows(testfwi1.df, 
                testfwi2.df,
                testfwi3.df,
                testfwi4.df,
                data.frame(
                  YR = v8_input_log$Year + 2019,
                  MON = v8_input_log$Month + 1,
                  FWI = v8_input_log$FireWeatherindex,
                  Source = 'LANDIS v8 climate log'
                ),
                data.frame(
                  YR = v7_input_log_monthly$Year,
                  MON = as.numeric(as.character(v7_input_log_monthly$month)) + 1,
                  FWI = v7_input_log_monthly$FWI,
                  Source = 'LANDIS v7 climate log'
                )
                ) |>
  mutate(Simulation_month = MON + (YR - 2020)*12)

ggplot(data = df, aes(x = Simulation_month, color = Source, y = FWI)) + geom_line()

df2 <- bind_rows(#testfwi1.df, 
                 testfwi2.df,
                 #testfwi3.df,
                 testfwi4.df,
                 data.frame(
                   YR = v7_input_log_monthly$Year,
                   MON = as.numeric(as.character(v7_input_log_monthly$month)) + 1,
                   FWI = v7_input_log_monthly$FWI,
                   FFMC = v7_input_log_monthly$FFMC,
                   DMC = v7_input_log_monthly$DMC,
                   DC = v7_input_log_monthly$DC,
                   BUI = v7_input_log_monthly$BUI,
                   Source = 'LANDIS v7 log'
                 ),
                 data.frame(
                   YR = v8_input_log$Year + 2019,
                   MON = v8_input_log$Month + 1,
                   FWI = v8_input_log$FireWeatherindex,
                   FFMC = v8_input_log$FineFuelMoistureCode,
                   DMC = v8_input_log$DuffMoistureCode,
                   DC = v8_input_log$DroughtCode,
                   BUI = v8_input_log$BuildUpIndex,
                   Source = 'LANDIS v8 log'
                 )) |>
  pivot_longer(c(FWI, FFMC, DMC, DC, BUI), names_to = "Index", values_to = "Val") |>
  mutate(Simulation_month = MON + (YR - 2020)*12) 

ggplot(data = df2, aes(x = Simulation_month, color = Source, y = Val, linetype = Source)) + geom_line() + facet_wrap(~Index, scales = 'free')


# lasttest <- fwi(
#   data.frame(
#     'lat'=lat,'long'=lon,'yr' = v8_input_log$Year,'mon' = v8_input_log$Month,
#     'temp' = v8_input_log$MaxTemp,'rh' = v8_input_log$MinRH,'ws' = v8_input_log$WindSpeed,'prec' = v8_input_log$Precip*10
#   ), batch = F
# )


test <- fwi(
  data.frame('lat'=lat,'long'=lon,'yr' = v8_input_log_daily$yr,'mon' = v8_input_log_daily$mon, 'day' = v8_input_log_daily$day,
             'temp' = v8_input_log_daily$MaxTemp,'rh' = v8_input_log_daily$MinRH,'ws' = v8_input_log_daily$WindSpeed,'prec' = v8_input_log_daily$Precip*10),
  init = data.frame("ffmc" = 85, 'dmc' = 20, 'dc' = 100, 'lat' = lat)
) |>
  mutate(Date = ymd(paste(YR, MON, DAY, sep = "-")))

hist(test$FWI)

test2 <- fwi(
  data.frame('lat'=lat,'long'=lon,'yr' = v7_input_log$yr,'mon' = v7_input_log$mon, 'day' = v7_input_log$day,
             'temp' = v7_input_log$max_airtemp,'rh' = v7_input_log$min_relativehumidity,'ws' = v7_input_log$windspeed,'prec' = v7_input_log$ppt*10),
  init = data.frame("ffmc" = 85, 'dmc' = 20, 'dc' = 100, 'lat' = lat)
)|>
  mutate(Date = ymd(paste(YR, MON, DAY, sep = "-")))

df <- bind_rows(
  data.frame(
    Source = "LANDIS v8",
    FWI = v8_input_log_daily$FireWeatherindex,
    Date = v8_input_log_daily$Date,
    FFMC = v8_input_log_daily$FineFuelMoistureCode,
    DMC = v8_input_log_daily$DuffMoistureCode,
    DC = v8_input_log_daily$DroughtCode,
    BUI = v8_input_log_daily$BuildUpIndex
  ),
  data.frame(
    Source = "LANDIS v8 fix",
    FWI = v8_input_log_daily2$FireWeatherindex,
    Date = v8_input_log_daily2$Date,
    FFMC = v8_input_log_daily2$FineFuelMoistureCode,
    DMC = v8_input_log_daily2$DuffMoistureCode,
    DC = v8_input_log_daily2$DroughtCode,
    BUI = v8_input_log_daily2$BuildUpIndex
  ),
  data.frame(
    Source = "LANDIS v7",
    FWI = v7_input_log$FWI,
    Date = v7_input_log$Date,
    FFMC = v7_input_log$FineFuelMoistureCode,
    DMC = v7_input_log$DuffMoistureCode,
    DC = v7_input_log$DroughtCode,
    BUI = v7_input_log$BuildUpIndex
  ),
  data.frame(
    Source = "R",
    FWI = test$FWI,
    Date = test$Date,
    FFMC = test$FFMC,
    DMC = test$DMC,
    DC = test$DC,
    BUI = test$BUI
  ),
  # data.frame(
  #   Source = "R on v7 inputs",
  #   FWI = test2$FWI,
  #   Date = test2$Date,
  #   FFMC = test2$FFMC,
  #   DMC = test2$DMC,
  #   DC = test2$DC,
  #   BUI = test2$BUI
  # )
) |>
  pivot_longer(c(FWI, FFMC, DMC, DC, BUI), names_to = "Index", values_to = "Val") |>
  filter(Index %in% c("FWI", "FFMC"))

ggplot(data = df, aes(x = Date, y = Val, color = Source)) + geom_line() + facet_wrap(~Index, scales = "free")


### Reduce spinup logs
for (LANDIS.EXTENT in c("Tripod", "WenEnt", "OkaMet")){
  spinup_clim <- file.path(LANDIS.EXTENT, 'zClimate_Library', paste0("CLIMATE_1980-2019_baseline_", LANDIS.EXTENT, '.csv'))
  
  
  
}





































