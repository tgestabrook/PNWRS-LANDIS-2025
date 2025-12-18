
### function to calculate SOE score: range from -1 to +1, cutoff and ramp between 10th and 90th percentile OF YEAR ZERO 
calculate_SOE <- function()





















#-----------------------------------------------------------------------------------------------------------------------
######  LOAD DATA LAYERS  ###### ----

### LOAD DST STRUCTURE (defined in DST_Structure.csv): ---- 
dst.structure<-read.csv(file.path(dataDir,'DST_Structure.csv')) |>
  filter(!is.na(Premise)) |>
  mutate(Topic = str_replace_all(Topic, ' ', '.'),
         Direction = ifelse(grepl('Less',Premise), -1, 1))  # if premise includes the word "less" direction is -1



### Load PWG & HUC12 summary csvs

### Also integrate water outputs

### Group by scenario, year, PWG/HUC12, metric and average

### Calculate SOE based on departure from BAU Wildfire

### Join DST structure, adjust based on direction, group by topic area, scenario, year, PWG/HUC12, calculate topic score










































