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
    str_replace('.img', '')
  
  if(flip_rasters){maprs <- flip(maprs)}  # flip if upside-down
  
  crs(maprs) <- crs(ecos.r)  # update CRS and extent
  ext(maprs) <- ext(ecos.r)
  
  return(maprs)
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
raster2csv<-function(r, agg.r = pwg.r, na.value = NA){
  if(is.na(na.value)){
    r[is.na(pwg.r) | is.na(r)] <- NA 
  } else {
    r[is.na(pwg.r) | r==na.value] <- NA
  }
  
  sum.df <- zonal(r, agg.r, fun = "sum", wide = F) |> mutate(metric = "sum")
  
  mean.df <- zonal(r, agg.r, fun = "mean", wide = F) |> mutate(metric = "mean")
  
  sd.df <- zonal(r, agg.r, fun = "sd", wide = F) |> mutate(metric = "sd")
  
  df <- bind_rows(sum.df, mean.df, sd.df)
  
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
  } else{
    next
  }
  return(df)
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