#### Write PNG function: ----
pngOut <- function(p, file, width, height, res=600, units="in"){
  png(file=file,width=width,height=height,res=res,units=units)
  print(p)
  dev.off()
}

### Function to turn loose rasters into a stack: ----
get_maps <- function(subdir, name_prefix){
  subdir_files <- dir(file.path(landisOutputDir, subdir))
  maps <- subdir_files[grepl(paste0('^', name_prefix), subdir_files)]  # get files in the directory that match the output map type
  
  maps <- maps[order(as.numeric(str_extract(maps, "(\\d+)")))]  # sort by year
  map_paths <- file.path(landisOutputDir, subdir, maps)
  maprs <- rast(map_paths)
  
  names(maprs) <- str_replace(maps, '.tif', '') |>
    str_replace('.img', '') |>
    str_replace('.asc', '')
  
  if(flip_rasters){maprs <- flip(maprs)}  # flip if upside-down
  
  crs(maprs) <- crs(ecos.r)  # update CRS and extent
  ext(maprs) <- ext(ecos.r)
  
  return(maprs)
}

### Function to determine post-processing order: ----
.check_for_filetype <- Vectorize(function(dir_path, extension){
  if (substr(extension, 1, 1) != ".") {
    extension <- paste0(".", extension)
  }
  
  files <- list.files(path = dir_path, pattern = paste0("\\", extension, "$"), ignore.case = TRUE, recursive = T)
  
  return(length(files) > 0)
})


prioritize_uncompressed_runs <- function(runlist, shuffle = T){
  uncompressed_runs <- runlist[.check_for_filetype(runlist, ".img")]
  
  compressed_runs <- runlist[!(runlist%in%uncompressed_runs)]
  
  if (shuffle) {
    return(
      c(sample(uncompressed_runs), sample(compressed_runs))
    )
  } else {
    return(
      c(uncompressed_runs, compressed_runs)
    )
  }
}

### Function to create landfire mask for name of veg group or list of codes: ----
landfireReclassFUN<-function(r=landfire.r,codes){
  if(is.character(codes)){
    codes.num<-landfire.codes %>% filter(EVT_PHYS %in% codes | EVT_GP_N %in% codes) |> select(VALUE) |> pull()
  } else codes.num<-codes
  
  if (sum(codes.num%in%values(r)) == 0){return(rast(r, vals = NA))}
  
  r2 <- ifel(r%in%codes.num, r, NA)
  
  return(r2)
}

### Function to interpolate a continuous raster stack: ----
interpolateRaster <- function(r){
  prefix <- str_extract(names(r)[1], "^[^0-9]*")  # grab all text before numerals appear
  suffix <- str_extract(names(r)[1], "[^0-9]*$")
  
  data.yrs <- names(r) |> str_extract("\\d+") |> as.integer()
  missing.yrs <- seq(0, simLength)[!seq(0, simLength) %in% data.yrs]
  
  missing.lyrs <- rast(r, vals = NA, nlyrs = length(missing.yrs))
  names(missing.lyrs) <- paste0(prefix, missing.yrs, suffix)
  
  r_interpolated <- c(r, missing.lyrs) 
  r_interpolated <- r_interpolated[[paste0(prefix, 0:simLength, suffix)]]  # this will sort the stack into the correct order
  r_interpolated <- approximate(r_interpolated, method = 'linear', rule = 2)
  
  return(r_interpolated)
}

### Function to turn fire size into duration: ----
sizeToDurationFUN=function(x) {
  k=0.00008 # steepness
  x0=40000 # midpoint
  ymax=20 # max duration
  adj=0.5 # adjust so min fire duration is 1
  return(round(ymax/(1 + exp(-k*(x-x0))) + adj,0))
}

### Function to summarize metrics by area unit
raster2csv<-function(r, agg.r = pwg.r){
  # if(is.na(na.value)){
  #   r[is.na(pwg.r) | is.na(r)] <- NA 
  # } else {
  #   r[is.na(pwg.r) | r==na.value] <- NA
  # }
  
  names(r) <- as.character(1:nlyr(r))
  
  sum.df <- zonal(r, agg.r, fun = "sum", wide = F, na.rm = T) |> mutate(metric = "sum")
  
  mean.df <- zonal(r, agg.r, fun = "mean", wide = F, na.rm = T) |> mutate(metric = "mean")
  
  # sd.df <- zonal(r, agg.r, fun = sd, wide = T, na.rm = T) |> pivot_longer(names(r), names_to = "layer", values_to = "value") |> mutate(metric = "sd", layer = as.numeric(layer))
  
  df <- bind_rows(sum.df, mean.df
                  # , sd.df
                  ) |> mutate(Year = layer-1) |> pivot_wider(names_from = 'metric', values_from = 'value') |>
    select(!layer)
  
  return(df)
}

get_ages_of_top3_biomass <- function(pixstack){ # assumes that species are in same order, should be true
  indices <- pixstack[1:3]  # first three layers are indicies of 1st place, 2nd place, 3rd place
  agepix <- pixstack[4:length(pixstack)]  # remaining layers have age data, select those corresponding to top biomass
  
  return(agepix[indices])
}

# function to load an output df and 
read_and_label <- function(file){
  if(file.exists(file.path(dirToProcess, file))){
    run <- str_split(file, '/')[[1]][1]
    
    df <- read.csv(file.path(dirToProcess, file)) |>
      mutate(Run = run)
    
    return(df)
  } else{
    warning(paste("Missing file!", file))
    return(data.frame())
  }
}

# function to trip maps to active sites in study area: ----
trim_to_study_area <- function(r, MASK = T){
  
  if (MASK){
    r <- ifel(is.na(r), 0, ifel(is.na(pwg.noBuffer.r)|pwg.r<12, NA, r)) # Zeros for active cells with NAs, NAs for inactive cells
  } else {
    r <- ifel(is.na(r), 0, ifel(is.na(pwg.r)|pwg.r<12, NA, r))
  }
  
  return(r)
}

### Function to write a batch of output rasters for DST maps: -----
writeOutputRasts <- function(outputs, destDir){
  cat('\n-> Writing output rasters...\n')
  for(r.name in outputs){
    cat(paste0("Writing ", r.name, " "))
    r<-eval(parse(text=r.name)) |> trim_to_study_area() # Load layer
    
    # r <- ifel(is.na(r), 0, ifel(is.na(pwg.noBuffer.r)|pwg.r<12, NA, r)) 
    # 
    # if(MASK == T)
    #   r <- ifel(is.na(pwg.noBuffer.r), NA, r) # NAs for the 5-km study area buffer
    # 
    out.name<-names(outputs[outputs==r.name]) # Load out name
    
    if (nlyr(r) == length(yrs)) {
      names(r)<-paste0(r.name, "-", yrs)
    } else if (nlyr(r) == length(yrs) - 1) {  # add a zero raster for year zero
      r <- c(zero.r[[1]], r)
      names(r)<-paste0(r.name, "-", yrs)
    } else {
      stop(paste0("PROBLEM, TOO FEW LAYERS IN ", r.name))
    }
    
    ## Write raster...
    writeRaster(r,file.path(landisOutputDir,destDir,paste0(out.name,'.tif')),overwrite=T)
    gc()
  }
  rm(list = outputs)
  # tmpFiles(current = T, orphan = T, remove = T)
  gc()
  cat('\n')
}



compute_deviation <- function(df, Sc = "BAU wildfire", Sc2 = "Base climate"){
  baseline_vals <- df |>  # create a dataframe with the Max, Min, Mean values of whatever variable was summarized for the baseline scenario
    ungroup() |>
    filter(Scenario == Sc,
           Scenario2 == Sc2) |>
    mutate(bMax = Max, bMin = Min, bMean = Mean) |>
    select(!c(Scenario, Scenario2, Max, Min, Mean))
  
  if(nrow(baseline_vals)==0){return(df)}
  
  outdf <- df |>  # join the baseline data and mutate so that the baseline vals per year, PWG, etc. are subtracted from those for each scenario
    left_join(baseline_vals) |>
    mutate(
      Mean = Mean - bMean,
      Max = Max - bMean,
      Min = Min - bMean
    )
  return(outdf)
}

# calculate summary, average across repeat sims, make plot
dst_output_time_series <- function(metricName, areaSummary, metricLabel = metricname, cumulative = F, plotScale = 1, mapYear = 50){  # area summary is sum or mean or sd
  
  df <- dst.huc12.all.df |>
    filter(Metric == metricName) |>
    mutate(areaSummary = replace_na(get(areaSummary), 0))

  
  df <- df |> 
    group_by(Year, HUC12.num, Scenario, Scenario2) |>  # first summarize mean, max, min values for all HUC12 units across scenario type
    summarise(
      Mean = mean(areaSummary, na.rm = T),
      Max = max(areaSummary, na.rm = T),
      Min = min(areaSummary, na.rm = T)
    ) |> ungroup()
  
  if(areaSummary == "sum"){
    full.df <- df |>
      group_by(Year, Scenario, Scenario2) |>  # summarise mean of all HUC12
      summarise(
        Mean = sum(Mean),
        Max = sum(Max),
        Min = sum(Min)
      )
  } else if (areaSummary == "mean"){
    full.df <- df |>
      group_by(Year, Scenario, Scenario2) |>  # summarise mean of all HUC12
      summarise(
        Mean = mean(Mean),
        Max = mean(Max),
        Min = mean(Min)
      )
  }
  
  
  
  if (cumulative) {
    full.df <- full.df |>
      group_by(Scenario, Scenario2)
  }
  
  if (plotScale != 1){
    metricLabel = paste0(plotScale, "s ", metricLabel)
  }
  
  p<-ggplot(data = full.df, aes(x = Year, fill = Scenario, color = Scenario, fill = Scenario)) +
    geom_line(aes(y = Mean/plotScale)) +
    geom_ribbon(aes(ymin=Min/plotScale,ymax=Max/plotScale),colour=NA,alpha=0.2,size=0.25) +
    scale_fill_manual(values = scenario_colors) + scale_color_manual(values = scenario_colors) +
    facet_grid(Scenario2~.) +
    xlab("Simulation Year") + ylab(metricLabel) 
  
  
  pngOut(p, file.path(dirToProcess, 'DST_figures', paste0(metricName, "_scenariocomp.png")), width = 6, height = 4)
  
  ### Now for the map component
  if (cumulative){  # if we already did cumsum, just grab year 50
    map.df <- df |>
      filter(Year == mapYear) 
  } else {  # otherwise, take mean or sum of 1-50
    map.df <- df |> 
      filter(Year <= mapYear) |>
      group_by(HUC12.num, Scenario, Scenario2) |>
      summarise(Mean = mean(Mean))  # average the mean scenario value per HUC12 across the first 50 years
  }
  
  shp <- HUC12.sf |> left_join(df)
  
  p2 <- ggplot(data = shp, aes(fill = Mean)) + geom_spatvector(color = NA) + scale_fill_viridis_c() + facet_grid(Scenario2~Scenario)
  
  pngOut(p2, file.path(dirToProcess, 'DST_figures', paste0(metricName, "_mapcomp.png")), width = 6, height = 4)
}
# 
# dst_plot_huc12 <- function(metric_name, summary_fun, year){
#   df <- dst.huc12.all.df |>
#     filter(Year <= 50, "Fire_High_Sev_Area_Ha") |>
#     group_by(Year, HUC12.num, Run, Scenario, Scenario2) |>
#     summarize(Cumulative_Fire_High_Sev_Area_Ha = sum(Fire_High_Sev_Area_Ha)) |>
#     
#     
#     
#   shp <- HUC12.shp |> left_join(df)
#   
# }






















