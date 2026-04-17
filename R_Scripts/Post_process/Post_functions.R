#### Write PNG function: ----
pngOut <- function(p, file, width, height, res=600, units="in"){
  png(file=file,width=width,height=height,res=res,units=units)
  print(p)
  dev.off()
}

### Function to turn loose rasters into a stack: ----
get_maps <- function(subdir, name_prefix, target.crs, target.ext){
  subdir_files <- dir(file.path(landisOutputDir, subdir))
  maps <- subdir_files[grepl(paste0('^', name_prefix), subdir_files)]  # get files in the directory that match the output map type
  
  maps <- maps[order(as.numeric(str_extract(maps, "(\\d+)")))]  # sort by year
  map_paths <- file.path(landisOutputDir, subdir, maps)
  maprs <- rast(map_paths)
  
  names(maprs) <- str_replace(maps, '.tif', '') |>
    str_replace('.img', '') |>
    str_replace('.asc', '')
  
  if(flip_rasters){maprs <- flip(maprs)}  # flip if upside-down
  
  crs(maprs) <- target.crs  # update CRS and extent
  ext(maprs) <- target.ext
  
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

flipifneeded <- function(r){
  test <- ifel(is.na(ecos.r)&(test_r!=0&!is.na(test_r)), 1, 0)
  if (sum(values(test), na.rm = T) > 0) {
    r <- flip(r)
  } 
  return(r)
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
  # indices <- pixstack[1:3]  # first three layers are indicies of 1st place, 2nd place, 3rd place
  # agepix <- pixstack[4:length(pixstack)]  # remaining layers have age data, select those corresponding to top biomass
  # 
  # return(agepix[indices])
  return(pixstack[4:length(pixstack)][pixstack[1:3]])
}

# function to load an output df and 
read_and_label <- function(file, dirToProcess){
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
  
  if (is.null(Sc2)){
    baseline_vals <- df |> as.data.frame() |>
      ungroup() |>
      filter(Scenario == Sc) |>
      mutate(bMax = Max, bMin = Min, bMean = Mean) |>
      select(!c(Scenario, Max, Min, Mean))  # we want scenario 2 (climate) in the join criteria so we can normalize to BAU under RCP8.5
  } else {
    baseline_vals <- df |> as.data.frame() |>  # create a dataframe with the Max, Min, Mean values of whatever variable was summarized for the baseline scenario
      ungroup() |>
      filter(Scenario == Sc,
             Scenario2 == Sc2) |>
      mutate(bMax = Max, bMin = Min, bMean = Mean) |>
      select(!c(Scenario, Scenario2, Max, Min, Mean))  # we need to join based on year and huc12, including scenarios will result in NA for non-reference scenarios
  }
  
  
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
dst_output_time_series <- function(metricName, areaSummary, metricLabel = metricname, cumulative = F, plotScale = 1, mapYear = c(25, 50, 75, 100), Direction = 1){  # area summary is sum or mean or sd
  
  df <- dst.huc12.all.df |>
    filter(Metric == metricName) |>
    mutate(areaSummary = replace_na(get(areaSummary), 0),
           Year = ifelse(Year > 2000, Year - 2020, Year))
  
  if (cumulative) {
    df <- df |>
      group_by(Scenario, Scenario2, Run, HUC12.num) |>
      arrange(Year) |> mutate(areaSummary = cumsum(areaSummary)) |> ungroup()
  }
  
  if(areaSummary == "sum"){
    
    full.df <-  df |> 
      group_by(Year, HUC12.num, Scenario, Scenario2) |>  # get the average of all repeat runs for each scenario and HUC12
      summarise(
        Mean = mean(areaSummary, na.rm = T),
        Max = max(areaSummary, na.rm = T),
        Min = min(areaSummary, na.rm = T)
      ) |> ungroup() |>
      group_by(Year, Scenario, Scenario2) |>  # calculate sum of all huc12
      summarise(
        Mean = sum(Mean),
        Max = sum(Max),
        Min = sum(Min)
      )
    
    join.df <- df |> # get baseline vals for computing deviation
      group_by(Year, Run, Scenario, Scenario2) |>  # sum of area
      summarise(areaSummary = sum(areaSummary)) |> ungroup() |>
      group_by(Year, Scenario, Scenario2) |>   # mean of scenario
      summarise(
        bMean = mean(areaSummary, na.rm = T)
      ) |> ungroup() |>
      filter(Scenario == "BAU wildfire") |>
      select(Scenario2, Year, bMean)
    
    soe.df <- df |>
      group_by(Year, Run, Scenario, Scenario2) |> 
      summarise(areaSummary = sum(areaSummary)) |> ungroup() |>  # sum of area for each run
      left_join(join.df) |>
      mutate(areaSummary = areaSummary - bMean) |>
      mutate(SOE = departureFUN(areaSummary, center = NA)  * Direction) |>
      group_by(Year, Scenario, Scenario2) |>  # get the average of all repeat runs for each scenario and HUC12
      summarise(
        Mean = mean(SOE, na.rm = T),
        Max = max(SOE, na.rm = T),
        Min = min(SOE, na.rm = T)
      ) |> ungroup() 
      
  } else if (areaSummary == "mean"){
    full.df <- df  |> 
      group_by(Year, HUC12.num, Scenario, Scenario2) |> # get the average of all repeat runs for each scenario and HUC12
      summarise(
        Mean = mean(areaSummary, na.rm = T),
        Max = max(areaSummary, na.rm = T),
        Min = min(areaSummary, na.rm = T)
      ) |> ungroup() |>
      group_by(Year, Scenario, Scenario2) |>  # calculate mean of all HUC12
      summarise(
        Mean = mean(Mean),
        Max = mean(Max),
        Min = mean(Min)
      )
    
    join.df <- df |> # get baseline vals for computing deviation
      group_by(Year, Scenario, Scenario2) |> 
      summarise(
        bMean = mean(areaSummary, na.rm = T)
      ) |> ungroup() |>
      filter(Scenario == "BAU wildfire") |>
      select(Scenario2, Year, bMean)
    
    soe.df <- df  |> 
      left_join(join.df) |>
      mutate(areaSummary = areaSummary - bMean) |>
      mutate(SOE = departureFUN(areaSummary, center = NA) * Direction) |>
      group_by(Year, Run, Scenario, Scenario2) |> # get the landscape average for each run
      summarise(
        Mean = mean(SOE, na.rm = T)
      ) |> ungroup() |>
      group_by(Year, Scenario, Scenario2) |>  # calculate mean of all HUC12
      summarise(
        Mean = mean(Mean),
        Max = max(Mean),
        Min = min(Mean)
      )
  }
  
  
  if (plotScale != 1){
    metricLabel = paste0(plotScale, "s ", metricLabel)
  }
  
  p<-ggplot(data = full.df, aes(x = Year, fill = Scenario, color = Scenario, fill = Scenario, linetype = Scenario)) +
    geom_line(aes(y = Mean/plotScale)) +
    geom_ribbon(aes(ymin=Min/plotScale,ymax=Max/plotScale),colour=NA,alpha=0.2,size=0.25) +
    scale_fill_manual(values = scenario_colors) + scale_color_manual(values = scenario_colors) +
    facet_grid(Scenario2~.) +
    xlab("Simulation Year") + ylab(metricLabel) 
  
  
  pngOut(p, file.path(dirToProcess, 'DST_figures', paste0(metricName, "_scenariocomp.png")), width = 10, height = 8)
  
  
  delta.df <- full.df |>
    compute_deviation(Sc2 = NULL)
  
  p2<-ggplot(data = delta.df, aes(x = Year, fill = Scenario, color = Scenario, fill = Scenario, linetype = Scenario)) +
    geom_line(aes(y = Mean/plotScale)) +
    geom_ribbon(aes(ymin=Min/plotScale,ymax=Max/plotScale),colour=NA,alpha=0.2,size=0.25) +
    scale_fill_manual(values = scenario_colors) + scale_color_manual(values = scenario_colors) +
    facet_grid(Scenario2~.) +
    xlab("Simulation Year") + ylab(metricLabel) 
  
  
  pngOut(p2, file.path(dirToProcess, 'DST_figures', paste0(metricName, "_delta_scenariocomp.png")), width = 10, height = 8)
  

  p3<-ggplot(data = soe.df, aes(x = Year, fill = Scenario, color = Scenario, fill = Scenario, linetype = Scenario)) +
    geom_line(aes(y = Mean)) +
    geom_ribbon(aes(ymin=Min,ymax=Max),colour=NA,alpha=0.2,size=0.25) +
    scale_fill_manual(values = scenario_colors) + scale_color_manual(values = scenario_colors) +
    facet_grid(Scenario2~.) +
    xlab("Simulation Year") + ylab("Strength of Evidence Score") 
  
  
  pngOut(p3, file.path(dirToProcess, 'DST_figures', paste0(metricName, "_delta_soe_scenariocomp.png")), width = 10, height = 8)  
  
  ### Now for the map component
  for (my in mapYear){
    cat(paste("Generating map for year", my, "..."))
    
    if (cumulative){  # if we already did cumsum, just grab year 50
      map.df <- df |>
        filter(Year == my) |>  # get the average of all repeat runs for each scenario and HUC12
        group_by(HUC12.num, Scenario, Scenario2) |>
        summarise(
          Mean = mean(areaSummary, na.rm = T),
          Max = max(areaSummary, na.rm = T),
          Min = min(areaSummary, na.rm = T)
        ) |> ungroup()
    } else {  # otherwise, take mean of 1-50
      map.df <- df |> 
        filter(Year <= my) |>
        group_by(HUC12.num, Scenario, Scenario2) |>  # here, we average the value of the metric over the first x years
        summarise(
          Mean = mean(areaSummary, na.rm = T),
          Max = max(areaSummary, na.rm = T),
          Min = min(areaSummary, na.rm = T)
        ) |> ungroup()
    }
    
    shp <- HUC12_nobuffer.sf |> right_join(map.df)
    
    p4 <- ggplot(data = shp, aes(fill = Mean)) + geom_spatvector(color = NA) + scale_fill_viridis_c() + facet_grid(Scenario2~Scenario, labeller = label_wrap_gen(width = 20)) + 
      theme(axis.ticks = element_blank(), axis.text = element_blank())
    
    figwidth = 2 * length(unique(shp$Scenario)) + 1
    
    pngOut(p4, file.path(dirToProcess, 'DST_figures', paste0(metricName, "_yr", my, "_mapcomp.png")), width = figwidth, height = 8)
    
    
    ### Also do a relative version
    shp.relative <- shp |>
      compute_deviation(Sc2 = NULL)
    
    if (Direction == 1){
      lowcol <- 'red'
      highcol <- 'forestgreen'
    } else{
      lowcol <- 'forestgreen'
      highcol <- 'red'
    }
    
    
    p5 <- ggplot(data = shp.relative, aes(fill = Mean)) + geom_spatvector(color = NA) + 
      scale_fill_gradient2(low = "red", mid = "gray90", high = "forestgreen", midpoint = 0, limits = c(-max(abs(shp.relative$Mean)), max(abs(shp.relative$Mean)))) + 
      facet_grid(Scenario2~Scenario, labeller = label_wrap_gen(width = 20)) + 
      theme(axis.ticks = element_blank(), axis.text = element_blank())
    
    pngOut(p5, file.path(dirToProcess, 'DST_figures', paste0(metricName, "_yr", my, "_relative_mapcomp.png")), width = figwidth, height = 8)
    
  }
  

}

### Function to calculate strength-of-evidence scores (modified version of Nick's departureFUN): ----
departureFUN<-function(x, q.breaks = c(0.1,0.9), breaks = NA, scores = c(-1, 1), center = 0, plotit = F) {
  
  if(is.na(breaks[1])){
    x.nonZero.nonNA<-x[!is.na(x) & x != 0]
    if(length(x.nonZero.nonNA)==0) x.nonZero.nonNA<-0
    breaks<-quantile(x.nonZero.nonNA,q.breaks)
    # quantile(x.nonZero.nonNA,q.breaks[1]),quantile(x.nonZero.nonNA,q.breaks[2]))
  } else {
    warning('Using specified breakpoints rather than quantile breakpoints.')
  }
  
  if(!is.na(center) & length(breaks)>2) stop('Cannot use "center" argument with more than 2 breakpoints. To use a more complicated ramp function, set center=NA and use breaks=c(1,2,3,4),scores=c(-1,0,0.5,1).')
  
  if(!is.na(center)){ 
    ## TJF wrote this code on 9/29/2022 to allow us to center strength-of-evidence scores using the "center" argument. 
    ##  Center represents the input value that will map to a y value of 0.
    ## For more complicated ramp functions, use the breaks arguement and set q.breaks=NA. 
    ## Breaks = breakpoints in input units (not quantiles), and scores must be a vector of the same length.
    if(breaks[1]<center & breaks[2]>center){
      breaks<-c(breaks[1],center,breaks[2])
    } else if(breaks[2]<center){
      breaks<-c(breaks[1],center,center)
      warning('Center value is larger than the right break. Using center value as right break point.')
    } else if(breaks[1]>=center){ # NOTE: updated this on Jan. 20 to be >= rather than >. 
      breaks<-c(center,center,breaks[2])
      warning('Center value is smaller than the left break. Using center value as left break point.')
    }
    scores<-c(scores[1],0,scores[2])
    
  }
  
  
  soe <- rep(NA, length(x))
  
  brks <- as.numeric(breaks)
  tmp <- findInterval(x = x, vec = brks)
  
  w <- which((tmp != 0) & (!is.na(x)) & (tmp != length(scores)))
  
  mn0 <- brks[tmp[w]]
  mx0 <- brks[tmp[w] + 1]
  mn1 <- scores[tmp[w]]
  mx1 <- scores[tmp[w] + 1]
  
  
  soe[w] <- (-(((-mx0 * mn1) + (mn0 * mx1))/(-mn0 + mx0))) + (((-mn1 + mx1) * x[w])/(-mn0 + mx0))
  
  soe[which(tmp == 0)] <- head(scores, 1)
  soe[which(tmp == length(scores))] <- tail(scores, 1)
  
  
  if(plotit){
    x<-x[!is.na(x)]
    xlim <- range(c(breaks, x))
    # x11()
    plot(0, xlim = c(xlim[1], xlim[2]), ylim = c(-1, 1), xlab = "", ylab = "", type = 'n')
    
    plot_x <- range(breaks)
    plot_y <- scores
    
    if(min(x) < min(plot_x)){
      plot_x[1] <- min(x)
      plot_y <- c(scores[1], scores)
    }
    
    
    if(max(x) > max(plot_x)){
      plot_x[2] <- max(x)
      plot_y <- c(plot_y, tail(scores, 1))
    }
    
    
    plot_x <- c(plot_x, breaks)
    plot_x <- sort(unique(plot_x))
    
    lines(plot_x, plot_y)
    
    sapply(1:length(x), function(i) lines(x = c(min(plot_x), x[i]), y = c(soe[i], soe[i]), lty = 3))
    sapply(1:length(x), function(i) lines(x = c(x[i], x[i]), y = c(-1, soe[i]), lty = 3))
    
    text(x = x, y = rep(-1, length(x)), labels = sprintf("%.02f", x), cex = 0.7, pos = 4)
    text(x = min(plot_x), y = soe, labels = sprintf("%.02f", soe), cex = 0.7, pos = 3)
    
    points(breaks, scores, pch = 21, bg = 'black', col = 'black', cex = 1.2)
    points(x, soe[!is.na(soe)], cex = 1.1, pch = 21, bg = "gold3", col = 'gold3')
  }
  rm(list=c('x','mn0','mx0','mn1','mx1','w','brks','tmp'))
  return(soe)
}


