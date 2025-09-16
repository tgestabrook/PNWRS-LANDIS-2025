library(terra)
library(tidyverse)
library(tidyterra)

REBURNDir <- file.path("F:", "Tripod_Sim01d1_Thomas", "FireSeverity_Annual")
landisOutputDir <- landisRuns[1]

test <- rast(file.path(REBURNDir, "Severity_0303.tif")) |> as.int()

ReburnFires <- dir(REBURNDir)[grepl(".tif$", dir(REBURNDir))]

# REBURNStack <- as.int(rast(file.path(REBURNDir, ReburnFires)))

REBURN.df <- data.frame(
  "Year" = NULL,
  "AreaHigh" = NULL,
  "AreaMod" = NULL,
  "AreaUBLow" = NULL
)

for (tif in ReburnFires){
  print(tif)
  r <- rast(file.path(REBURNDir, tif)) |>
    as.int()
  
  ext(r) <- ext(pwg.r)
  freqs <- freq(r)
  
  as <- ifelse(3 %in% freqs$value, freqs |> filter(value == 3) |> pull(count), 0)
  am <- ifelse(2 %in% freqs$value, freqs |> filter(value == 2) |> pull(count), 0)
  al <- ifelse(1 %in% freqs$value, freqs |> filter(value == 1) |> pull(count), 0)
  au <- ifelse(0 %in% freqs$value, freqs |> filter(value == 0) |> pull(count), 0)
  
  df <- data.frame(
    "Year" = str_remove(str_remove(tif, "Severity_"), ".tif"),
    "AreaHigh" = as,
    "AreaMod" = am,
    "AreaLow" = al,
    "AreaUB" = au
  )
  
  REBURN.df <- bind_rows(REBURN.df, df)
}

REBURN.df <- REBURN.df |>
  mutate(Source = "REBURN")


severityStack.r <- rast(file.path(landisOutputDir, 'social-climate-fire', 'fire-dnbr-yr.tif'))
severityStackClassified.r <- ifel(severityStack.r<=1, NA, severityStack.r) |>  # have to class 0 as NA so that median window only operates on unburned cells
  focal(w = 3, fun = median, na.rm=T, pad = TRUE, na.policy = 'only') |>
  classify(rcl=severity.reclass.df,include.lowest=T)

LANDIS.df <- severityStackClassified.r |>
  as.data.frame() |>
  pivot_longer(cols = everything(), names_to = "Year", values_to = "SeverityCode") |>
  filter(!is.na(SeverityCode)) |>
  left_join(data.frame("SeverityCode" = c(1,2,3,4), "Severity" = c("UB", "Low", "Mod", "High"))) |>
  mutate(Severity = factor(Severity, levels = c("UB", "Low", "Mod", "High"), labels = c("UB", "Low", "Mod", "High"))) |>
  group_by(Year, Severity) |>
  summarise(Area = n()) |>
  pivot_wider(names_from = "Severity", values_from = "Area", names_prefix = "Area", names_sep = "", values_fill = 0, names_expand = T) |>
  mutate(Source = "LANDIS")


df <- bind_rows(REBURN.df, LANDIS.df) |>
  mutate(AreaUBLow = AreaUB + AreaLow) |>
  select(!c(AreaUB, AreaLow)) |>
  pivot_longer(cols = starts_with("Area"), names_to = "Severity", names_prefix = "Area", values_to = "Area")

ggplot(df, aes(x = log(Area), colour = Source)) + geom_density() + facet_wrap(~Severity)


### Make map of # times burned at each severity level

#### Low
LANDISlow.r <- ifel(severityStackClassified.r %in% c(1,2), 1, 0) |>
  sum()

REBURNlow.r <- rast(file.path(REBURNDir, '..', "SevLow_Count.tif"))

par(mfrow = c(1, 2))
plot(LANDISlow.r/nlyr(severityStack.r), range = c(0, 0.5))
plot(REBURNlow.r[[1]]/nrow(REBURN.df), range = c(0, 0.5))

#### Mod
LANDISmod.r <- ifel(severityStackClassified.r %in% c(3), 1, 0) |>
  sum()

REBURNmod.r <- rast(file.path(REBURNDir, '..', "SevMod_Count.tif"))

par(mfrow = c(1, 2))
plot(LANDISmod.r/nlyr(severityStack.r), range = c(0, 0.15))
plot(REBURNmod.r[[1]]/nrow(REBURN.df), range = c(0, 0.15))

#### High
LANDIShigh.r <- ifel(severityStackClassified.r %in% c(4), 1, 0) |>
  sum()

REBURNhigh.r <- rast(file.path(REBURNDir, '..', "SevHigh_Count.tif"))

par(mfrow = c(1, 2))
plot(LANDIShigh.r/nlyr(severityStack.r), range = c(0, 0.1))
plot(REBURNhigh.r[[1]]/nrow(REBURN.df), range = c(0, 0.1))

### FRI
#### Low
LANDISFRIlow.r <- nlyr(severityStack.r)/LANDISlow.r
plot(LANDISFRIlow.r, range = c(1, 100), col = rainbow(50))

REBURNFRIlow.r <- nrow(REBURN.df)/REBURNlow.r[[1]]
plot(REBURNFRIlow.r, range = c(1, 100), col = rainbow(50))










