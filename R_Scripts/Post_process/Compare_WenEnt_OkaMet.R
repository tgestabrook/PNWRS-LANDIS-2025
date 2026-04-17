library(terra)
library(tidyverse)
library(tidyterra)
library(hues)
library(zoo)
source("./R_Scripts/Post_process/Post_functions.R") # load custom functions

dataDir<-'F:/LANDIS_Input_Data_Prep/Data'
modelDir<-'F:/V8_Models'

AOI_WenEnt.sf <- vect(file.path(dataDir,'PWG',paste0('HUC12_nobuffer_WenEnt.gpkg'))) |> mutate(HUC12.num = as.numeric(HUC12))
AOI_OkaMet.sf <- vect(file.path(dataDir,'PWG',paste0('HUC12_nobuffer_OkaMet.gpkg'))) |> mutate(HUC12.num = as.numeric(HUC12))
AOI_area <- terra::union(AOI_WenEnt.sf, AOI_OkaMet.sf) |> terra::aggregate() |> terra::disagg() |> expanse(unit = "ha")

FigDir <- file.path("F:", "2025_Q4_Scenarios", "White Paper Figures")

# combined_runs.df <- data.frame()

dirToProcess <- file.path('F:/2025_Q4_Scenarios', c("WenEnt", "OkaMet"))
landisRuns <- c(file.path(dirToProcess[1],dir(dirToProcess[1])[grepl('Sim',dir(dirToProcess[1])) & !grepl('.zip', dir(dirToProcess[1]))]),
                file.path(dirToProcess[2],dir(dirToProcess[2])[grepl('Sim',dir(dirToProcess[2])) & !grepl('.zip', dir(dirToProcess[2]))]))

scenario_map.df <- read.csv(file.path(dirToProcess[1], 'Scenario_name_map.csv')) |>
  mutate(Folder_name = str_to_lower((Folder_name))) 

sims.df <- data.frame("Path" = landisRuns, 'Run'=basename(landisRuns)) |>  # each folder gets a row
  mutate(date = str_extract(Run, '[0-9]{8}_[0-9]{4}'), Folder_name = gsub('_[0-9]{8}_[0-9]{4}', '', Run), # make a column of date info for each run and strip if from scenario name
         model_version = substr(Run, 4, 9),
         Extent = ifelse(grepl("OkaMet", Folder_name), "Okanogan-Methow", "Wenatchee-Entiat")) |>   # get git commit id, useful for comparisons across model versions
  mutate(Folder_name = str_replace(Folder_name, model_version, '')) |>
  mutate(Folder_name = str_replace(Folder_name, paste0("Sim_", "WenEnt", "_"), '')) |> # strip away standard sim prefix 
  mutate(Folder_name = str_replace(Folder_name, paste0("Sim_", "OkaMet", "_"), '')) |> # strip away standard sim prefix
  mutate(Folder_name = str_to_lower(Folder_name)) |>
  left_join(scenario_map.df)  |> 
  filter(!is.na(Mgt_scenario)) |>
  mutate(DST_prepared = ifelse(file.exists(file.path(landisRuns, 'DST', 'DST_Metrics_by_PWG.csv')), T, F)) |>  # check if the DST input map script has been run
  mutate(Post_processed = ifelse(file.exists(file.path(landisRuns, 'Biomass_Annual_Dynamics.csv')), T, F))  # this is the last essential file generated in the post-processing script, so if it exists, the run has been fully processed

params = list(
  scenario_column = "Mgt_scenario",
  scenario_column2 = "Climate",
  reference_scenario1 = "BAU wildfire",
  reference_scenario2 = "Base climate"
)

sims.plot.df <- sims.df |>  # add the information from scenario_map
  mutate(Scenario = get(params$scenario_column),
         Scenario2 = get(params$scenario_column2)) |>  
  mutate(Scenario = factor(Scenario, levels=unique(Scenario)[base::order(unique(Scenario))]),
         Scenario2 = factor(Scenario2, levels=unique(Scenario2)))

scenario_colors <- iwanthue(length(unique(sims.plot.df$Scenario)), 0, 360, 36, 180, 13, 73)
names(scenario_colors) <- levels(sims.plot.df$Scenario)

read_and_label2 <- function(file){
  if(file.exists(file.path(file))){
    
    if (grepl("DST", dirname(file))){
      df <- read.csv(file.path(file)) |>
        mutate(Path = dirname(dirname(file)))
    } else{
      df <- read.csv(file.path(file)) |>
        mutate(Path = dirname(file))
    }
    
    return(df)
  } else{
    warning(paste("Missing file!", file))
    return(data.frame())
  }
}


# dst.pwg.all.df <- lapply(file.path(sims.df$Path,'DST', 'DST_Metrics_by_PWG.csv'), read_and_label2) |> bind_rows() |>
#   left_join(sims.plot.df)

dst.huc12.all.df <- lapply(file.path(sims.df$Path,'DST', 'DST_Metrics_by_HUC12.csv'), read_and_label2) |> bind_rows() |>
  left_join(sims.plot.df) |> filter(HUC12.num > 0) 

dst.fire.all.df <- lapply(file.path(sims.df$Path,'Fire_severity_df.csv'), read_and_label2) |> bind_rows() |>
  left_join(sims.plot.df) |>
  pivot_longer(c(PercentUnburned, PercentLow, PercentModerate, PercentHigh), names_to = "Severity", names_prefix = "Percent", values_to = "Percent") |>
  mutate(Area_ha = FIRE_SIZE * (Percent/100),
         Severity = ifelse(Severity == "Unburned", "Low", Severity)) |>
  mutate(Severity = factor(Severity, levels = c("Low", "Moderate", "High", labels = c("Low Severity", "Moderate Severity", "High Severity")))) 

dst.biomass.all.df <-  lapply(file.path(sims.df$Path,'Biomass_Annual_Dynamics.csv'), read_and_label2) |> bind_rows() |>
  left_join(sims.plot.df)





dst.structure.df<-read.csv(file.path(modelDir, 'Shared_inputs', 'DST_Structure.csv')) |>
  filter(!is.na(Premise)) |>
  mutate(Topic = str_replace_all(Topic, ' ', '.'),
         Direction = ifelse(grepl('Less',Premise), -1, 1), Metric = Field_name)  # if premise includes the word "less" direction is -1

fire_topics <- dst.structure.df |> filter(Topic == "Wildfire") |> select(Field_name) |> pull()

# for (DST_metric in unique(dst.huc12.all.df$Metric)){
#   gc()
#   structure_info <- dst.structure.df |> filter(Field_name == DST_metric)
#   cat(paste0('\n', DST_metric))
#   
#   dst_output_time_series(metricName = DST_metric, areaSummary = structure_info$AreaSummary, metricLabel = structure_info$Units, cumulative = structure_info$Cumulative, Direction = structure_info$Direction)
# }


###### Specific plots

### Cumulative non-rx area burned Wen Ent
df <- dst.fire.all.df |>
  filter(IgnitionType!="Prescribed", Extent == "Wenatchee-Entiat") |>
  group_by(Run, SimulationYear, Extent, Scenario, Scenario2, Severity) |>
  summarise(Area_total = sum(Area_ha)) |>  # total annual area in each severity class
  ungroup() |>
  group_by(Run, Extent, Scenario, Scenario2, Severity) |>
  mutate(Area_cumulative = cumsum(Area_total)/404.6) |>  # cumulative area for each year to 1000s of acres
  ungroup() |> 
  group_by(Scenario, Scenario2, Extent, SimulationYear, Severity) |>
  summarise(Max = max(Area_cumulative), Min = min(Area_cumulative), Mean = mean(Area_cumulative)) |>
  compute_deviation(Sc = params$reference_scenario1, Sc2 = params$reference_scenario2)

p <- ggplot(data = df, aes(x = SimulationYear, y = Mean, color = Scenario, fill = Scenario, linetype = Scenario)) +
  geom_line() +
  geom_ribbon(aes(ymin=Min,ymax=Max),alpha=0.2, linewidth = NA) +
  scale_color_manual(values = scenario_colors) + scale_fill_manual(values = scenario_colors) +
  facet_grid(Scenario2~Severity) +
  ylab("Area (1000s of Acres)") +
  theme(legend.position = "inside", legend.position.inside = c(.84, .87))

p
pngOut(p, file.path(FigDir, "Cumulative_fire_WenEnt.png"), width = 8, height = 6)  

### Cumulative non-rx area burned Oka Met
df <- dst.fire.all.df |>
  filter(IgnitionType!="Prescribed", Extent == "Okanogan-Methow") |>
  group_by(Run, SimulationYear, Extent, Scenario, Scenario2, Severity) |>
  summarise(Area_total = sum(Area_ha)) |>  # total annual area in each severity class
  ungroup() |>
  group_by(Run, Extent, Scenario, Scenario2, Severity) |>
  mutate(Area_cumulative = cumsum(Area_total)/404.6) |>  # cumulative area for each year to 1000s of acres
  ungroup() |> 
  group_by(Scenario, Scenario2, Extent, SimulationYear, Severity) |>
  summarise(Max = max(Area_cumulative), Min = min(Area_cumulative), Mean = mean(Area_cumulative)) |>
  compute_deviation(Sc = params$reference_scenario1, Sc2 = params$reference_scenario2)

p <- ggplot(data = df, aes(x = SimulationYear, y = Mean, color = Scenario, fill = Scenario, linetype = Scenario)) +
  geom_line() +
  geom_ribbon(aes(ymin=Min,ymax=Max),alpha=0.2, linewidth = NA) +
  scale_color_manual(values = scenario_colors) + scale_fill_manual(values = scenario_colors) +
  facet_grid(Scenario2~Severity) +
  ylab("Area (1000s of Acres)") +
  theme(legend.position = "inside", legend.position.inside = c(.84, .87))

p
pngOut(p, file.path(FigDir, "Cumulative_fire_OkaMet.png"), width = 8, height = 6)  

### Overall wildfire decision score
landscape.mean.df <- dst.huc12.all.df |>
  filter(Metric %in% fire_topics) |>
  group_by(Extent, Year, Scenario, Scenario2, Run, Metric) |>  # calculate full-landscape score from HUC12
  summarise(Landscape_value = mean(mean, na.rm = T)) |> ungroup()

baseline.df <- landscape.mean.df |>
  filter(Scenario == "BAU wildfire") |>
  group_by(Year, Extent, Scenario, Scenario2, Metric) |>
  summarise(bMean = mean(Landscape_value, na.rm = T)) |> ungroup() |>
  select(Year, Extent, Scenario2, Metric, bMean)

df <- landscape.mean.df |>
  # left_join(baseline.df) |>
  # mutate(Landscape_value = Landscape_value - bMean) |>
  inner_join(dst.structure.df) |>
  group_by(Extent, Metric) |>
  mutate(SOE = departureFUN(Landscape_value) * Direction) |> ungroup() |>
  group_by(Year, Extent, Scenario, Scenario2, Run) |>  # average soe of topic areas
  mutate(SOE = mean(SOE)) |> ungroup() |>
  group_by(Extent, Scenario, Scenario2, Run) |>
  mutate(SOE = cumsum(SOE)) |>
  group_by(Year, Extent, Scenario, Scenario2) |>
  summarise(
    Mean = mean(SOE),
    Max = max(SOE),
    Min = min(SOE)
  ) |> ungroup() |>
  compute_deviation(Sc = params$reference_scenario1, Sc2 = NULL)

# df <- dst.huc12.all.df |>
#   filter(Metric %in% fire_topics) |>
#   group_by(Extent, Year, Scenario, Scenario2, Run, Metric) |>  # calculate full-landscape score
#   summarise(Landscape_value = mean(mean, na.rm = T)) |> ungroup() |>
#   group_by(Metric, Extent) |> 
#   mutate(SOE = departureFUN(Landscape_value)) |> ungroup() |>
#   group_by(Extent, Scenario, Scenario2, Metric) |>
#   arrange(Year) |>
#   mutate(SOE = cumsum(SOE)) |> ungroup() |>
#   group_by(Year, Extent, Scenario, Scenario2) |>
#   summarise(
#     Mean = mean(SOE),
#     Max = max(SOE),
#     Min = min(SOE)
#   ) |> ungroup()  |>
#   compute_deviation(Sc = params$reference_scenario1, Sc2 = NULL)

p <- ggplot(data = df, aes(x = Year, y = Mean, color = Scenario, fill = Scenario, linetype = Scenario)) +
  geom_line() +
  geom_ribbon(aes(ymin=Min,ymax=Max),alpha=0.1, linewidth = NA) +
  scale_color_manual(values = scenario_colors) + scale_fill_manual(values = scenario_colors) +
  facet_grid(Scenario2~Extent) +
  ylab("Strength of Evidence") +
  theme(legend.position = "inside", legend.position.inside = c(.18, .5))

p

pngOut(p, file.path(FigDir, "Wildfire_decision_score.png"), width = 8, height = 6)  

### Biomass deviation WenEnt
df <- dst.biomass.all.df |>
  filter(Species == "TotalBiomass",
         Extent == "Wenatchee-Entiat",
         PWG %in% c(20, 30, 40, 50)) |>
  group_by(Year, PWG_Name, Scenario, Scenario2) |>
  summarise(
    Mean = mean(Biomass_sum_Mg)/1000,
    Max = max(Biomass_sum_Mg)/1000,
    Min = min(Biomass_sum_Mg)/1000
  ) |>
  compute_deviation(Sc = params$reference_scenario1, Sc2 = params$reference_scenario2)

p<-ggplot(data = df, aes(x = Year, fill = Scenario, color = Scenario, fill = Scenario, linetype = Scenario)) + 
  geom_line(aes(y = Mean)) +
  geom_ribbon(aes(ymin=Min,ymax=Max),colour=NA,alpha=0.1,size=0.25) +
  scale_fill_manual(values = scenario_colors) + scale_color_manual(values = scenario_colors) +
  facet_grid(Scenario2~PWG_Name) +
  xlab("Simulation Year") + ylab(expression(paste('Biomass (1000s of Mg)'))) +
  theme(legend.position = "inside", legend.position.inside = c(.84, .57))

p
pngOut(p, file.path(FigDir, "Biomass_Deviation_WenEnt.png"), width = 8, height = 6)  

### Biomass deviation OkaMet
df <- dst.biomass.all.df |>
  filter(Species == "TotalBiomass",
         Extent == "Okanogan-Methow",
         PWG %in% c(20, 30, 40, 50)) |>
  group_by(Year, PWG_Name, Scenario, Scenario2) |>
  summarise(
    Mean = mean(Biomass_sum_Mg)/1000,
    Max = max(Biomass_sum_Mg)/1000,
    Min = min(Biomass_sum_Mg)/1000
  ) |>
  compute_deviation(Sc = params$reference_scenario1, Sc2 = params$reference_scenario2)

p<-ggplot(data = df, aes(x = Year, fill = Scenario, color = Scenario, fill = Scenario, linetype = Scenario)) + 
  geom_line(aes(y = Mean)) +
  geom_ribbon(aes(ymin=Min,ymax=Max),colour=NA,alpha=0.1,size=0.25) +
  scale_fill_manual(values = scenario_colors) + scale_color_manual(values = scenario_colors) +
  facet_grid(Scenario2~PWG_Name) +
  xlab("Simulation Year") + ylab(expression(paste('Biomass (1000s of Mg)'))) +
  theme(legend.position = "inside", legend.position.inside = c(.84, .57))

p
pngOut(p, file.path(FigDir, "Biomass_Deviation_OkaMet.png"), width = 8, height = 6)  

### Fine fuel loading both AOIS (summed)
df <- dst.biomass.all.df |>
  filter(Species == "TotalBiomass", 
         PWG %in% c(20, 30, 40, 50)) |>
  group_by(Year, PWG_Name, Scenario, Scenario2, Run) |>
  summarize(FineFuels_mean_MgHaEco = sum(FineFuels_mean_MgHaEco * PWG_area_Ha)/sum(PWG_area_Ha)) |> ungroup() |>
  group_by(PWG_Name, Scenario, Scenario2, Run) |>
  mutate(FineFuels_mean_MgHaEco = rollmean(FineFuels_mean_MgHaEco, k = 5, fill = NA)) |> ungroup() |>
  group_by(Year, PWG_Name, Scenario, Scenario2) |>
  summarize(
    Max = max(FineFuels_mean_MgHaEco),
    Min = min(FineFuels_mean_MgHaEco),
    Mean = mean(FineFuels_mean_MgHaEco)
  )

p <- ggplot(data = df, aes(x = Year, fill = Scenario, color = Scenario, fill = Scenario, linetype = Scenario)) + 
  geom_line(aes(y = Mean)) +
  geom_ribbon(aes(ymin=Min,ymax=Max),alpha=0.1, linewidth = NA) +
  scale_fill_manual(values = scenario_colors) + scale_color_manual(values = scenario_colors) +
  facet_grid(Scenario2~PWG_Name) +
  xlab("Simulation Year") + ylab(expression(paste('Fine Fuels (Mg  ',ha^-1,')'))) +
  theme(legend.position = "inside", legend.position.inside = c(.84, .4))
p
pngOut(p, file.path(FigDir, "Fine_fuels_whole_landscape.png"), width = 8, height = 6)  


### Cumulative net revenue both AOIS (faceted)
df <- dst.huc12.all.df |>
  filter(Metric == "Net_Revenue_USD") |>
  mutate(sum = replace_na(sum, 0)) |>
  group_by(Year, Run, Extent, Scenario, Scenario2) |>
  summarize(NetRevenue = sum(sum)) |> ungroup() |>
  group_by(Extent, Run, Scenario, Scenario2) |>
  arrange(Year) |>
  mutate(NetRevenue = cumsum(NetRevenue)) |> ungroup() |>
  group_by(Year, Extent, Scenario, Scenario2) |>
  summarize(
    Max = max(NetRevenue)/1000000,
    Min = min(NetRevenue)/1000000,
    Mean = mean(NetRevenue)/1000000
  )
  
p <- ggplot(data = df, aes(x = Year, fill = Scenario, color = Scenario, fill = Scenario, linetype = Scenario)) + 
  geom_line(aes(y = Mean)) +
  geom_ribbon(aes(ymin=Min,ymax=Max),alpha=0.1, linewidth = NA) +
  scale_fill_manual(values = scenario_colors) + scale_color_manual(values = scenario_colors) +
  facet_grid(Scenario2~Extent) +
  xlab("Simulation Year") + ylab(expression(paste('Cumulative Net Revenue (Millions USD)'))) +
  theme(legend.position = "inside", legend.position.inside = c(.17, .4))
p
pngOut(p, file.path(FigDir, "Net_revenue_whole_landscape.png"), width = 8, height = 6)   

### Cumulative merchantable biomass both AOIS
df <- dst.huc12.all.df |>
  filter(Metric == "Harvested_Merch_Mg", !Scenario %in% c("BAU wildfire", "Mgd. forest Rx")) |>
  mutate(sum = replace_na(sum, 0)) |>
  group_by(Year, Run, Extent, Scenario, Scenario2) |>
  summarize(Harvested_Merch_Mg = sum(sum)) |> ungroup() |>
  group_by(Extent, Run, Scenario, Scenario2) |>
  arrange(Year) |>
  mutate(Harvested_Merch_Mg = cumsum(Harvested_Merch_Mg)) |> ungroup() |>
  group_by(Year, Extent, Scenario, Scenario2) |>
  summarize(
    Max = max(Harvested_Merch_Mg)/1000,
    Min = min(Harvested_Merch_Mg)/1000,
    Mean = mean(Harvested_Merch_Mg)/1000
  )

p <- ggplot(data = df, aes(x = Year, fill = Scenario, color = Scenario, fill = Scenario, linetype = Scenario)) + 
  geom_line(aes(y = Mean)) +
  geom_ribbon(aes(ymin=Min,ymax=Max),alpha=0.1, linewidth = NA) +
  scale_fill_manual(values = scenario_colors) + scale_color_manual(values = scenario_colors) +
  facet_grid(Scenario2~Extent) +
  xlab("Simulation Year") + ylab(expression(paste('Merchantable Biomass (1,000s Mg)'))) +
  theme(legend.position = "inside", legend.position.inside = c(.17, .4))
p
pngOut(p, file.path(FigDir, "Harvested_merch_whole_landscape.png"), width = 8, height = 6)   

### Overall forest health decision score











## Decision score for everything
# First, calc difference from BAU wildfire
baseline.df <- dst.huc12.all.df |>
  filter(Scenario == "BAU wildfire") |>
  mutate(Year = ifelse(Year > 2000, Year - 2020, Year)) |>
  # mutate(mean = replace_na(mean, 0)) |>
  group_by(Year, Extent, Scenario, Scenario2, Metric, HUC12.num) |>
  summarise(bMean = mean(mean, na.rm = T)) |> ungroup() |>
  mutate(bMean = replace_na(bMean, 0)) |>
  select(Year, Extent, Scenario2, Metric, bMean, HUC12.num)

test <- baseline.df |>
  group_by(Year, Scenario2, Metric) |>
  summarise(bMean = mean(bMean, na.rm = T))

ggplot(data = test, aes(x = Year, y = bMean, linetype = Scenario2)) + geom_line() + facet_wrap(~Metric, scales = "free")

HUC12.area.df <- terra::union(AOI_WenEnt.sf, AOI_OkaMet.sf) |>
  as.data.frame() |>
  select(HUC12.num, Area)

dst.df <- dst.huc12.all.df |>
  mutate(Year = ifelse(Year > 2000, Year - 2020, Year)) |>
  mutate(mean = replace_na(mean, 0)) |>
  select(!c(X, sum, Path, model_version, Post_processed)) |>
  left_join(baseline.df) |>
  mutate(delta = mean - bMean) |>  # calculate delta as the difference between the mean value in HUC12 vs. mean mean value in HUC12 from all BAU scenarios
  group_by(Metric, Extent, Scenario2) |>  # score is calculated for all runs in 
  mutate(soe = departureFUN(delta)) |> ungroup() |>
  rename(Field_name = Metric) |>
  inner_join(dst.structure.df) |>
  mutate(soe = Direction * soe) |>  # modify SOE by premise
  inner_join(HUC12.area.df) |>
  group_by(Year, Extent, Scenario, Scenario2, Run, Field_name, Metric, Topic) |>  # calculate a full-landscape score for each simulation
  summarise(soe = weighted.mean(soe, Area, na.rm = T)) |> ungroup()  # at this point, we should have the landscape mean for each metric


  # group_by(Year, Extent, Scenario, Scenario2, Field_name, Metric, Topic) |>  # now, get the max, mean, and min for all the runs within one scenario
  # summarise(Max = max(soe),
  #           Mean = mean(soe),
  #           Min = min(soe))


dst.topics.df <- dst.df |>
  group_by(Year, Extent, Scenario, Scenario2, Topic, Run) |>  # get mean value of all soe scores in a topic
  summarise(soe = mean(soe, na.rm = T)) |> ungroup() |>
  group_by(Extent, Scenario, Scenario2, Topic, Run) |>
  arrange(Year) |>
  mutate(soe = rollmean(soe, k = 5, fill = NA)) |> ungroup() |>
  group_by(Year, Extent, Scenario, Scenario2, Topic) |>  # now, get the max, mean, and min for all the runs within one scenario
  summarise(Max = max(soe, na.rm = T),
            Mean = mean(soe, na.rm = T),
            Min = min(soe, na.rm = T))

p <- ggplot(data = dst.topics.df, aes(x = Year, y = Mean, color = Scenario, fill = Scenario, linetype = Extent)) +
  geom_line() +
  geom_ribbon(aes(ymin=Min,ymax=Max),alpha=0.1, linewidth = NA) +
  scale_color_manual(values = scenario_colors) + scale_fill_manual(values = scenario_colors) +
  facet_grid(Scenario2~Topic) +
  ylab("Strength of Evidence") +
  theme(legend.position = "inside", legend.position.inside = c(.18, .5))

p

pngOut(p, file.path(FigDir, "decision_score.png"), width = 8, height = 6)  






















