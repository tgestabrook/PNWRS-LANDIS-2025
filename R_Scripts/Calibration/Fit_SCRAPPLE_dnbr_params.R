library(tidyverse)
library(terra)
library(tidyterra)
library(gridExtra)

landisOutputDir <- file.path("F:\\2026_Q4_Scenarios\\WenEnt\\Sim231bbf_WenEnt_baseclim_Sc2_20261029_2352")
LANDIS.EXTENT <- "WenEnt"

bigDataDir<-'F:/LANDIS_Input_Data_Prep/BigData'
dataDir<-'F:/LANDIS_Input_Data_Prep/Data'
wDir <- file.path(landisOutputDir)

pwg.r <-  rast(file.path(dataDir, "PWG", paste0("PWG_", LANDIS.EXTENT, ".tif")))
names(pwg.r) <- 'PWG'


siteSeverityFUN = function(B0=SiteMortalityB0, B1=SiteMortalityB1, B2=SiteMortalityB2, B3=SiteMortalityB3,
                           B4=SiteMortalityB4, B5=SiteMortalityB5, B6=SiteMortalityB6,
                           CLAY = 0.10, ET = 430, WS = 20, CWD = 540, FINES = 0.2, LADDER = 1500, variable=NA,...){
  
  if(!is.na(variable))  {
    new.dat<-eval(parse(text=variable),envir=.GlobalEnv) # Look for variable in global environment (outside of function)
    assign(variable,new.dat) # Overwrite variable name within function environment
  }
  dNBR <- 1 / (B0 + B1*CLAY + B2*ET + B3*WS + B4*CWD + B5*FINES + B6*LADDER)
  dNBR[!is.finite(dNBR)]<-1000
  dNBR[dNBR<0]<-1000
  dNBR[dNBR>1000]<-1000
  return(dNBR)
}

mtbs.df <- data.frame()
for(mtbs_file in dir(file.path(dataDir, 'MTBS_and_FOD_Fires', LANDIS.EXTENT))[grepl('Observed_fires_', dir(file.path(dataDir, 'MTBS_and_FOD_Fires', LANDIS.EXTENT)))]){
  print(mtbs_file)
  mtbs.r <- rast(file.path(dataDir, 'MTBS_and_FOD_Fires', LANDIS.EXTENT, mtbs_file))
  stack <- c(mtbs.r, pwg.r)
  names(stack) <- c('DNBR','PWG')
  mtbs.df <- bind_rows(mtbs.df, as.data.frame(stack, na.rm=T))
}

mtbs.df <- mtbs.df |> 
  filter(!PWG%in%c(10,11)) |>
  mutate(source='MTBS', PWG = as.factor(PWG)) |>
  mutate(DNBR = ifelse(DNBR > 1000, 1000, DNBR)) |>  # cap DNBR range at 1000 as past that it's 100% mortality anyways
  # mutate(Severity = cut(DNBR, breaks=c(0,41,176,366,750,2001), labels=c('Unburned', 'Low', 'Moderate', 'High', 'Extreme')))
  mutate(Severity = cut(DNBR, breaks=c(0,176,366,750,2001), labels=c('Unburned-Low', 'Moderate', 'High', 'Extreme')))

  
### Load predictors
soilclay.df <- rast(file.path("F:", "V8_Models", LANDIS.EXTENT, 'NECN_input_maps', 'SoilPercentClay.tif')) |>
  as.data.frame(xy = T) |>
  rename(Clay = SoilPercentClay)
PET.df <- rast(file.path(landisOutputDir, "social-climate-fire", "PET-yr.tif")) |>
  as.data.frame(xy = T) |>
  pivot_longer(cols = starts_with("PET"), names_to = "Year", values_to = "PET", names_prefix = "PET-")
EWS.df <- rast(file.path(landisOutputDir, "social-climate-fire", "EWS-yr.tif")) |>
  as.data.frame(xy = T) |>
  pivot_longer(cols = starts_with("EWS"), names_to = "Year", values_to = "EWS", names_prefix = "EWS-")
CWD.df <- rast(file.path(landisOutputDir, "social-climate-fire", "CWD-yr.tif")) |>
  as.data.frame(xy = T) |>
  pivot_longer(cols = starts_with("CWD"), names_to = "Year", values_to = "CWD", names_prefix = "CWD-")
Fines.df <- rast(file.path(landisOutputDir, "social-climate-fire", "fine-fuels-yr.tif")) |>
  as.data.frame(xy = T) |>
  pivot_longer(cols = starts_with("fine-fuels"), names_to = "Year", values_to = "Fines", names_prefix = "fine-fuels-") |>
  mutate(Year = as.character(as.numeric(Year) + 1))  # WE NEED THE PREVIOUS YEAR'S FINE FUELS!!!!!!
Ladder.df <- rast(file.path(landisOutputDir, "social-climate-fire", "Ladders-yr.tif")) |>
  as.data.frame(xy = T) |>
  pivot_longer(cols = starts_with("Ladders"), names_to = "Year", values_to = "Ladders", names_prefix = "Ladders-")

### Load response
Severity.df <- rast(file.path(landisOutputDir, "social-climate-fire", "fire-dnbr-yr.tif")) |>
  as.data.frame(xy = T) |>
  pivot_longer(cols = starts_with("fire-dnbr"), names_to = "Year", values_to = "dNBR", names_prefix = "fire-dnbr-")

landis.df <- pwg.r |> as.data.frame(xy = T, na.rm = T) |>
  filter(!PWG%in%c(10, 11)) |>
  filter(!is.na(PWG)) |>
  left_join(soilclay.df) |>
  left_join(PET.df) |>
  left_join(EWS.df) |>
  left_join(CWD.df) |>
  left_join(Fines.df) |>
  left_join(Ladder.df) |>
  left_join(Severity.df) |>
  filter(dNBR > 1) |>
  group_by(PWG) |>
  # sample_frac(0.33) |>
  ungroup()

landis_light.df <- landis.df |>  # light version to maybe run faster?
  select(PWG, Clay, PET, EWS, CWD, Fines, Ladders, dNBR) |>
  group_by(PWG) |>
  sample_frac(0.33) |>
  ungroup()

n_mtbs_obs <- nrow(mtbs.df)
# pdistr_mtbs <- mtbs.df |>
#   select(Severity, PWG) |>
#   group_by(Severity, PWG, .drop = F) |>
#   summarise(n = n()) |>
#   group_by(PWG, .drop = F) |> mutate(p = n/n_mtbs_obs) |>
#   select(PWG, Severity, p) |>
#   pivot_wider(values_from = p, names_from = Severity)

# calculate_xs_dist <- function(PWGin, pdistr=pdistr_mtbs){
#   xdistr <- dnbr_vals.df |> filter(PWG%in%PWGin) |> select(Severity) |> group_by(Severity, .drop=F) |> summarise(count = n())
# 
#   if(length(xdistr)==0|is.null(xdistr)){return(NA)}
# 
#   #pdistr <- dnbr_vals.df %>% filter(source=='MTBS', PWG%in%PWGin) %>% select(Severity) %>% group_by(Severity, .drop=F) %>% summarise(n = n()) %>% mutate(p = n/sum(n))
#   pdistr <- pdistr |> filter(PWG%in%PWGin)
# 
# 
#   chisq_result <- chisq.test(x=xdistr$count, p=pdistr$p)
#   if(!is.finite(chisq_result)|is.na(chisq_result)|is.nan(chisq_result)){
#     print(paste("Infinite chisq result for:", PWGin))
#     chisq_result <- 99999999999}
#   return(chisq_result$statistic)
# }

# calculate_xs_dist(40)

# brute force
B0_range <- seq(0.006, 0.009, 0.00025)  # intercept
B1_range <- 0.04 #seq(0.04, 0.07, 0.01)  # update: removing negative clay values option
B2_range <- seq(-0.00000, -0.00005, -0.00001)  # PET
B3_range <- seq(-0.00003, -0.00015, -0.00001)  # EWS
B4_range <- -0.00005 #seq(-0.00003, -0.00012, -0.00001)  # CWD
B5_range <- seq(-0.0001, -0.0015, -0.00005)  # FF
B6_range <- seq(-0.0000002, -0.0000006, -0.0000001)  # Ladders
FF_range <- 1750 #seq(1750, 2500, 250)  # MaxFF

gc()
param_combos <- expand.grid(B0_range, B1_range, B2_range, B3_range, B4_range, B5_range, B6_range, FF_range) |>
  sample_frac()  #randomize order
gc()

# names(param_combos) <- c("Intercept", "Clay", "PET", "EWS", "CWD", "FF", "Lad", "MaxFF")

param_combos$xdist_12 <- NA
param_combos$xdist_20 <- NA
param_combos$xdist_30 <- NA
param_combos$xdist_40 <- NA
param_combos$xdist_50 <- NA
param_combos$xdist_forest <- NA

.calculate_ks <- function(PWG_val, df){
  landispwg.df <- df |>
    filter(PWG == PWG_val)
  mtbspwg.df <- mtbs.df |> 
    filter(PWG == PWG_val)
  
  return(ks.test(x = landispwg.df$DNBR, y = mtbspwg.df$DNBR)$statistic)
}

.plot_params <- function(param_cs, final_ks = 0, df = landis.df){
  dvals.df <- df |>
    mutate(DNBR_new = mapply(siteSeverityFUN, param_cs$Var1, param_cs$Var2, param_cs$Var3, param_cs$Var4,
                             param_cs$Var5, param_cs$Var6, param_cs$Var7,
                             Clay, PET, EWS, CWD, pmin(Fines/param_cs$Var8, 1), Ladders),
           source = "LANDIS") |>
    select(DNBR_new, PWG, source) |> 
    mutate(DNBR=DNBR_new, PWG = as.factor(PWG)) |>
    mutate(DNBR = ifelse(DNBR > 1000, 1000, DNBR)) |>
    bind_rows(mtbs.df %>% select(DNBR, PWG, source)) |>
    # mutate(Severity = cut(DNBR, breaks=c(0,41,176,366,750,2001), labels=c('Unburned', 'Low', 'Moderate', 'High', 'Extreme')))
    mutate(Severity = cut(DNBR, breaks=c(0,41,176,366,750,2001), labels=c('Unburned', 'Low', 'Moderate', 'High', 'Extreme')))
  
  print(quantile(dvals.df$DNBR))
  print(head(dvals.df))
  
  p1 <- ggplot(dvals.df%>%filter(!PWG%in%c(14,15)), aes(x=DNBR, fill=source)) + geom_density(alpha=0.5) + facet_wrap(~PWG, nrow=1) +
    ggtitle(paste(round(final_ks, 4), '--', paste(paste0(c("Int.", "Clay", "PET", "EWS", "CWD", "FF", "LF", "MaxFF"), ": ", param_cs[1, 1:8]), collapse=' / ')))  +
    scale_fill_manual(values=c('grey30', 'grey90')) +
    annotate('rect', xmin=0, xmax=41, ymin=-Inf, ymax=0, alpha=.75, fill='darkgreen') +
    annotate('rect', xmin=42, xmax=176, ymin=-Inf, ymax=0, alpha=.75, fill='darkseagreen') +
    annotate('rect', xmin=177, xmax=366, ymin=-Inf, ymax=0, alpha=.75, fill='goldenrod1') +
    annotate('rect', xmin=367, xmax=750, ymin=-Inf, ymax=0, alpha=.75, fill='firebrick4') +
    annotate('rect', xmin=751, xmax=1000, ymin=-Inf, ymax=0, alpha=.75, fill='purple')+ geom_vline(xintercept=c(41,176,366,750), linetype='dotted') + theme_minimal()
  p2 <- ggplot(dvals.df%>%filter(!PWG%in%c(14,15))%>%group_by(PWG, Severity, source)%>%summarise(n=n())%>%group_by(PWG,source)%>%mutate(p=n/sum(n)), aes(x=Severity, y=p, fill=source, color=Severity)) +
    geom_bar(stat='identity', position = 'dodge', linewidth=2) + facet_wrap(~PWG, nrow=1) +
    scale_color_manual(values=c('darkgreen','darkseagreen','goldenrod1','firebrick4', 'purple')) +
    scale_fill_manual(values=c('grey30', 'grey90'))
  print(grid.arrange(p1, p2))
}


# test <- landis.df |>
#   mutate(DNBR_test = mapply(siteSeverityFUN, .008, .04, -0.000001, -0.000029, 0, -0.005, -0.00000012, Clay, PET, EWS, CWD, pmin(Fines/1750, 1), Ladders)) |>
#   mutate(DNBR_trunc = ifelse(dNBR > 1000, 1000, dNBR)) 

# hist(test$DNBR_test)
# 
# param_cs <- data.frame(Var1 = .0065, Var2 = .04, Var3 = -.0000001, Var4 = -0.000029, Var5 = 0, Var6 = 0, Var7 = -0.00000012, Var8 = 1750)[1,]
# test2 <- landis.df |>
#   mutate(DNBR_new = mapply(siteSeverityFUN, param_cs$Var1, param_cs$Var2, param_cs$Var3, param_cs$Var4,
#                            param_cs$Var5, param_cs$Var6, param_cs$Var7,
#                            Clay, PET, EWS, CWD, min(Fines/param_cs$Var8, 1), Ladders),
#          source = "LANDIS") |>
#   select(DNBR_new, PWG, source) |> 
#   mutate(DNBR=DNBR_new, PWG = as.factor(PWG)) |>
#   mutate(DNBR = ifelse(DNBR > 1000, 1000, DNBR)) |>
#   bind_rows(mtbs.df %>% select(DNBR, PWG, source)) |>
#   # mutate(Severity = cut(DNBR, breaks=c(0,41,176,366,750,2001), labels=c('Unburned', 'Low', 'Moderate', 'High', 'Extreme')))
#   mutate(Severity = cut(DNBR, breaks=c(0,41,176,366,750,2001), labels=c('Unburned', 'Low', 'Moderate', 'High', 'Extreme')))
# 
# hist(test2$DNBR_new)


.plot_params(data.frame(Var1 = .00675, Var2 = .04, Var3 = -.00005, Var4 = -.00005, Var5 = -.00008, Var6 = -.015, Var7 = -.0000004, Var8 = 1750)[1,], 9)
# .plot_params(data.frame(Var1 = 20, Var2 = .04, Var3 = -.0000001, Var4 = -0.000029, Var5 = 0, Var6 = 0, Var7 = -0.00000012, Var8 = 1750)[1,], 9)

.plot_params(param_combos[1, 1:8])

tot=nrow(param_combos)
best_dist <- 1
iter = 1
for(combo_id in sample(1:nrow(param_combos), nrow(param_combos))){
  print(paste(combo_id, '-- iteration', iter, 'out of', tot))
  if (!is.na(param_combos[combo_id, "xdist_forest"])){next}

  dnbr_vals.df <- landis_light.df |>
    mutate(DNBR = mapply(siteSeverityFUN, param_combos$Var1[combo_id], param_combos$Var2[combo_id], param_combos$Var3[combo_id], param_combos$Var4[combo_id],
                                                                 param_combos$Var5[combo_id], param_combos$Var6[combo_id], param_combos$Var7[combo_id],
                                                                 Clay, PET, EWS, CWD, pmin(1, Fines/param_combos$Var8[combo_id]), Ladders)) |> 
    select(DNBR, PWG) #|>
    # mutate(DNBR=DNBR_new) |>
    # mutate(DNBR = ifelse(DNBR > 1000, 1000, DNBR)) |>
    # mutate(Severity = cut(DNBR, breaks=c(0,41,176,366,750,2001), labels=c('Unburned', 'Low', 'Moderate', 'High', 'Extreme')))
    # mutate(Severity = cut(DNBR, breaks=c(0,176,366,750,2001), labels=c('Unburned-Low', 'Moderate', 'High', 'Extreme')))
  
  # xdistr_landis <- dnbr_vals.df |>
  #   select(Severity, PWG) |>
  #   group_by(Severity, PWG, .drop = F) |>
  #   summarise(n = n(), .groups = "drop_last") |>
  #   select(PWG, Severity, n) |>
  #   pivot_wider(values_from = n, names_from = Severity, values_fill = 0) |>
  #   filter(!is.na(PWG))
  
  
  #xdist <- chisq.test(x = xdistr_landis[,2:5], p = pdistr_mtbs[,2:5], simulate.p.value = T)$statistic
  # dvals20.df <- dnbr_vals.df |> filter(PWG == 20)
  # dvals30.df <- dnbr_vals.df |> filter(PWG == 30)
  # dvals40.df <- dnbr_vals.df |> filter(PWG == 40)
  # dvals50.df <- dnbr_vals.df |> filter(PWG == 50)
  # 
  ks12 <- .calculate_ks(12, dnbr_vals.df)
  ks13 <- .calculate_ks(13, dnbr_vals.df)
  ks20 <- .calculate_ks(20, dnbr_vals.df)
  ks30 <- .calculate_ks(30, dnbr_vals.df)
  ks40 <- .calculate_ks(40, dnbr_vals.df)
  ks50 <- .calculate_ks(50, dnbr_vals.df)
  
  final_ks <- ((ks12 + ks13)/2 + ks20 + ks30 + ks40 + ks50)/5
  
  # xdist <- ks.test(x = dnbr_vals.df$DNBR, y = mtbs.df$DNBR)$statistic
  
  if(is.nan(final_ks)){
    final_ks <- 1
    iter <- iter+1
    next
  }
  
  param_combos[combo_id, 'xdist_12'] <- (ks12 + ks13)/2
  param_combos[combo_id, 'xdist_20'] <- ks20
  param_combos[combo_id, 'xdist_30'] <- ks30
  param_combos[combo_id, 'xdist_40'] <- ks40
  param_combos[combo_id, 'xdist_50'] <- ks50
  param_combos[combo_id, 'xdist_forest'] <- final_ks
  
  
  if (final_ks<best_dist){
    best_dist <- final_ks
    print(paste('new best:', final_ks, 'with params', paste(param_combos[combo_id,1:8], collapse='; ')))
    
    .plot_params(param_combos[combo_id, 1:8], best_dist)
  }
  
  # param_combos[combo_id, 'xdist_50'] <- xdist50
  iter <- iter+1
  
  if(iter%%5000==0){
    gc()
    write.csv(param_combos%>%filter(!is.na(xdist_forest)), file.path(wDir, 'fire_calib_param_tests.csv'))
  }
}

which.min(param_combos$xdist_forest)
param_combos[which.min(param_combos$xdist_forest),]

print(paste('final best:', param_combos[which.min(param_combos$dist_forest),'dist_forest'], 'with params', paste(param_combos[which.min(param_combos$dist_forest),1:8], collapse='/')))

### Plot to see basic patterns
tested_combos <- read.csv(file.path(wDir, 'fire_calib_param_tests.csv')) |> filter(!is.na(xdist_forest)) |>
  mutate(final_metric = xdist_forest) |>
  rename('Intercept'=Var1, 'Clay'=Var2, 'PrET'=Var3, 'EWS'=Var4, 'CWD'=Var5, 'FF'=Var6, 'LF'=Var7, 'MaxFF'=Var8) |>
  mutate(best=as.factor(ifelse(final_metric<0.15, 1, 0)))

quantile(tested_combos$xdist_forest)

tested_combos_long <- tested_combos |>
  pivot_longer(cols=c('Intercept', 'Clay', 'PrET', 'EWS', 'CWD', 'FF', 'LF', 'MaxFF'), names_to = 'Parameter', values_to = 'ParameterValue') %>%
  mutate(ParameterValueFac=as.factor(ParameterValue))

ggplot(data=tested_combos_long%>%filter(final_metric>0.5)) + geom_violin(aes(x=ParameterValueFac, y=final_metric)) + facet_wrap(~Parameter, scales='free_x')
ggplot(data=tested_combos_long%>%filter(final_metric<0.25)) + geom_violin(aes(x=ParameterValueFac, y=final_metric)) + facet_wrap(~Parameter, scales='free_x')
ggplot(data=tested_combos_long, aes(x = best, y = ParameterValue)) + geom_jitter() + facet_wrap(~Parameter, scales='free_y')

df <- tested_combos |>
  group_by(MaxFF, FF) |>
  summarize(performance = mean(final_metric), perf2 = median(final_metric), obs = n())
ggplot(data = df, aes(x = MaxFF, y = FF, size = 1-perf2, color = obs)) + geom_point() 

df <- tested_combos_long |>
  group_by(ParameterValueFac, Parameter) |>
  summarize(median_perf = median(final_metric))
ggplot(data = df, aes(x = ParameterValueFac, y = median_perf)) + geom_col() + facet_wrap(~Parameter, scales="free_x")

# pairs_df <- tested_combos_long |>
#   filter_out(Parameter == "MaxFF") |>
#   mutate(ParVal = paste(Parameter, ParameterValue))
# 
pairs <- expand.grid(c('Intercept', 'Clay', 'PrET', 'EWS', 'CWD', 'FF', 'LF'), c('Intercept', 'Clay', 'PrET', 'EWS', 'CWD', 'FF', 'LF')) |>
  filter_out(Var1 == Var2)

df <- tested_combos |>
  group_by(EWS, CWD) |>
  summarize(performance = mean(final_metric), perf2 = median(final_metric), obs = n())
ggplot(data = df, aes(x = EWS, y = CWD, size = perf2, color = obs)) + geom_point() 



# GGally::ggpairs(tested_combos, columns = c('Intercept', 'Clay', 'PrET', 'EWS', 'CWD', 'FF', 'LF', 'MaxFF'), aes(color=best, alpha=0.001))
# 
# ### Make some final plots ######################################################
# ### Overlay WenEnt, Prev calib, empirical, and new calib
# # WenEnt calib
# combo_id <- which.min(param_combos$xdist_forest)
# dnbr_vals.df <- evlog.df %>%
#   rowwise() %>%
#   mutate(DNBR_WE = siteSeverityFUN(B0=0.01, B1=0, B2=-0.000004, B3=-0.000035,
#                                    B4=-0.000005, B5=-0.003, B6=-0.00000025,
#                                    CLAY = MeanClay, ET = MeanPET, WS = MeanEffectiveWindSpeed, CWD = MeanWD, FINES = FineFuels/5000, LADDER = MeanLadderFuels)) %>%
#   mutate(DNBR_Old = siteSeverityFUN(B0=0.0105, B1=0, B2=-0.000003, B3=-0.00009,
#                                     B4=-0.0000045, B5=-0.0015, B6=-0.00000055,
#                                     CLAY = MeanClay, ET = MeanPET, WS = MeanEffectiveWindSpeed, CWD = MeanWD, FINES = FineFuels/3000, LADDER = MeanLadderFuels)) %>%
#   mutate(DNBR_New = siteSeverityFUN(B0=param_combos$Var1[combo_id], B1=param_combos$Var2[combo_id], B2=param_combos$Var3[combo_id], B3=param_combos$Var4[combo_id],
#                                     B4=param_combos$Var5[combo_id], B5=param_combos$Var6[combo_id], B6=param_combos$Var7[combo_id],
#                                     CLAY = MeanClay, ET = MeanPET, WS = MeanEffectiveWindSpeed, CWD = MeanWD, FINES = FineFuels/param_combos$Var8[combo_id], LADDER = MeanLadderFuels)) %>%
#   ungroup() %>% 
#   select(DNBR_New, DNBR_WE, DNBR_Old, PWG, source) %>% 
#   #mutate(DNBR=DNBR_new) %>%
#   bind_rows(mtbs.df %>% select(DNBR, PWG, source) %>% mutate(DNBR_New=DNBR, DNBR_WE=DNBR, DNBR_Old=DNBR)) %>%
#   #mutate(Severity = cut(DNBR, breaks=c(0,200,300,400,2001), labels=c('Unburned', 'Low', 'Moderate', 'High')))
#   mutate(Severity_WE = cut(DNBR_WE, breaks=c(0,41,176,366,750,2001), labels=c('Unburned (0-41)', 'Low (42-176)', 'Moderate (177-366)', 'High (367-750)', 'Extreme (751+)')),
#          Severity_Old = cut(DNBR_Old, breaks=c(0,41,176,366,750,2001), labels=c('Unburned (0-41)', 'Low (42-176)', 'Moderate (177-366)', 'High (367-750)', 'Extreme (751+)')),
#          Severity_New = cut(DNBR_New, breaks=c(0,41,176,366,750,2001), labels=c('Unburned (0-41)', 'Low (42-176)', 'Moderate (177-366)', 'High (367-750)', 'Extreme (751+)')))
# 
# p1 <- ggplot(dnbr_vals.df%>%filter(!PWG%in%c(14,15)), aes(x=DNBR_WE, fill=source)) + geom_density(alpha=0.5) + facet_wrap(~PWG, nrow=1) + ggtitle('WenEnt parameters')+ geom_vline(xintercept=c(41,176,366,750), linetype='dotted') + 
#   scale_fill_manual(values=c('grey30', 'grey90')) + 
#   annotate('rect', xmin=0, xmax=41, ymin=-Inf, ymax=0, alpha=.75, fill='darkgreen') + 
#   annotate('rect', xmin=42, xmax=176, ymin=-Inf, ymax=0, alpha=.75, fill='darkseagreen') + 
#   annotate('rect', xmin=177, xmax=366, ymin=-Inf, ymax=0, alpha=.75, fill='goldenrod1') + 
#   annotate('rect', xmin=367, xmax=750, ymin=-Inf, ymax=0, alpha=.75, fill='firebrick4') + 
#   annotate('rect', xmin=751, xmax=2000, ymin=-Inf, ymax=0, alpha=.75, fill='purple')+ geom_vline(xintercept=c(41,176,366,750), linetype='dotted') + theme(legend.position="none")
# p2 <- ggplot(dnbr_vals.df%>%filter(!PWG%in%c(14,15))%>%group_by(PWG, Severity_WE, source)%>%summarise(n=n())%>%group_by(PWG,source)%>%mutate(p=n/sum(n)), aes(x=Severity_WE, y=p, fill=source, color=Severity_WE)) + 
#   geom_bar(stat='identity', position = 'dodge', linewidth=2) + facet_wrap(~PWG, nrow=1) + 
#   #scale_color_manual(values=c('darkgreen','darkseagreen','goldenrod1','firebrick4')) + 
#   scale_color_manual(values=c('darkgreen','darkseagreen','goldenrod1','firebrick4', 'purple')) + 
#   scale_fill_manual(values=c('grey40', 'grey80')) +
#   scale_x_discrete(guide = guide_axis(n.dodge = 2))
# p3 <- ggplot(dnbr_vals.df%>%filter(!PWG%in%c(14,15)), aes(x=DNBR_Old, fill=source)) + geom_density(alpha=0.5) + facet_wrap(~PWG, nrow=1) + ggtitle('Old OkaMet parameters')+ geom_vline(xintercept=c(41,176,366,750), linetype='dotted') + 
#   scale_fill_manual(values=c('grey30', 'grey90')) + 
#   annotate('rect', xmin=0, xmax=41, ymin=-Inf, ymax=0, alpha=.75, fill='darkgreen') + 
#   annotate('rect', xmin=42, xmax=176, ymin=-Inf, ymax=0, alpha=.75, fill='darkseagreen') + 
#   annotate('rect', xmin=177, xmax=366, ymin=-Inf, ymax=0, alpha=.75, fill='goldenrod1') + 
#   annotate('rect', xmin=367, xmax=750, ymin=-Inf, ymax=0, alpha=.75, fill='firebrick4') + 
#   annotate('rect', xmin=751, xmax=2000, ymin=-Inf, ymax=0, alpha=.75, fill='purple')+ geom_vline(xintercept=c(41,176,366,750), linetype='dotted') + theme(legend.position="none")
# p4 <- ggplot(dnbr_vals.df%>%filter(!PWG%in%c(14,15))%>%group_by(PWG, Severity_Old, source)%>%summarise(n=n())%>%group_by(PWG,source)%>%mutate(p=n/sum(n)), aes(x=Severity_Old, y=p, fill=source, color=Severity_Old)) + 
#   geom_bar(stat='identity', position = 'dodge', linewidth=2) + facet_wrap(~PWG, nrow=1) + 
#   #scale_color_manual(values=c('darkgreen','darkseagreen','goldenrod1','firebrick4')) + 
#   scale_color_manual(values=c('darkgreen','darkseagreen','goldenrod1','firebrick4', 'purple')) + 
#   scale_fill_manual(values=c('grey40', 'grey80')) +
#   scale_x_discrete(guide = guide_axis(n.dodge = 2))
# p5 <- ggplot(dnbr_vals.df%>%filter(!PWG%in%c(14,15)), aes(x=DNBR_New, fill=source)) + geom_density(alpha=0.5) + facet_wrap(~PWG, nrow=1) + ggtitle('New OkaMet parameters')+ geom_vline(xintercept=c(41,176,366,750), linetype='dotted') + 
#   scale_fill_manual(values=c('grey30', 'grey90')) + 
#   annotate('rect', xmin=0, xmax=41, ymin=-Inf, ymax=0, alpha=.75, fill='darkgreen') + 
#   annotate('rect', xmin=42, xmax=176, ymin=-Inf, ymax=0, alpha=.75, fill='darkseagreen') + 
#   annotate('rect', xmin=177, xmax=366, ymin=-Inf, ymax=0, alpha=.75, fill='goldenrod1') + 
#   annotate('rect', xmin=367, xmax=750, ymin=-Inf, ymax=0, alpha=.75, fill='firebrick4') + 
#   annotate('rect', xmin=751, xmax=2000, ymin=-Inf, ymax=0, alpha=.75, fill='purple')+ geom_vline(xintercept=c(41,176,366,750), linetype='dotted') + theme(legend.position="none")
# p6 <- ggplot(dnbr_vals.df%>%filter(!PWG%in%c(14,15))%>%group_by(PWG, Severity_New, source)%>%summarise(n=n())%>%group_by(PWG,source)%>%mutate(p=n/sum(n)), aes(x=Severity_New, y=p, fill=source, color=Severity_New)) + 
#   geom_bar(stat='identity', position = 'dodge', linewidth=2) + facet_wrap(~PWG, nrow=1) + 
#   #scale_color_manual(values=c('darkgreen','darkseagreen','goldenrod1','firebrick4')) + 
#   scale_color_manual(values=c('darkgreen','darkseagreen','goldenrod1','firebrick4', 'purple')) + 
#   scale_fill_manual(values=c('grey40', 'grey80')) +
#   scale_x_discrete(guide = guide_axis(n.dodge = 2))
# print(grid.arrange(p1, p2, p3, p4, p5, p6, nrow=6)) 
# 
# print(grid.arrange(p1, p2, nrow=2)) 
# print(grid.arrange(p3, p4, nrow=2)) 
# print(grid.arrange(p5, p6, nrow=2)) 
# 
# 
# 
# 
# 
# 

B0_range <- seq(0.006, 0.0075, 0.00025)  # intercept
B1_range <- 0.04 #seq(0.04, 0.07, 0.01)  # update: removing negative clay values option
B2_range <- seq(-0.00004, -0.00008, -0.00001)  # PET
B3_range <- seq(-0.00008, -0.00015, -0.00001)  # EWS
B4_range <- seq(-0.00006, -0.00012, -0.00001)  # CWD
B5_range <- seq(-0.0005, -0.001, -0.001)  # FF
B6_range <- seq(-0.0000002, -0.0000006, -0.0000001)  # Ladders
FF_range <- 1750 #seq(1750, 2500, 250)  # MaxFF




### Bayesian optimization
library(rBayesianOptimization)



xdist_fun <- function(#B0,
                      B2, B3, 
                      # B4, 
                      B5#, B6
                      ){
  dnbr_vals.df <- landis_light.df |>
    mutate(DNBR = mapply(siteSeverityFUN, 0.00675, 0.04, B2, B3,
                         -0.00005, B5, -0.0000004,
                         Clay, PET, EWS, CWD, pmin(1, Fines/1750), Ladders)) |> 
    select(DNBR, PWG) #|>
  
  ks12 <- .calculate_ks(12, dnbr_vals.df)
  ks13 <- .calculate_ks(13, dnbr_vals.df)
  ks20 <- .calculate_ks(20, dnbr_vals.df)
  ks30 <- .calculate_ks(30, dnbr_vals.df)
  ks40 <- .calculate_ks(40, dnbr_vals.df)
  ks50 <- .calculate_ks(50, dnbr_vals.df)
  
  xdist <- ((ks12 + ks13)/2 + ks20 + ks30 + ks40 + ks50)/5
  # xdist <- ks.test(x = dnbr_vals.df$DNBR, y = mtbs.df$DNBR)$statistic
  
  if(is.nan(xdist)){
    xdist <- 1
  }
  return(list('Score' = 1-xdist, 'Pred' = 0))
}

xdist_fun(-0.000001, -0.000029, -0.005)

pre_search <- tested_combos |>
  rename(B0 = Intercept, B2 = PrET, B3 = EWS, B4 = CWD, B5 = FF, B6 = LF, Value = final_metric) |> 
  mutate(Value = 1-Value) |>
  select(B0, B2, B3, B4, B5, B6, Value) |>
  slice_sample(n = 1000)

DNBR_search <- BayesianOptimization(
  FUN = xdist_fun,
  bounds = list(
    # B0 = c(0.007, 0.011),
    B2 = c( -0.0001, 0),
    B3 = c(-0.0002, 0),
    # B4 = c(-0.0002, 0),
    B5 = c( -0.02, -0.001)#,
    # B6 = c( -0.000001, -0.0000001)
  ), 
  # init_grid_dt = pre_search,
  init_points = 200,
  n_iter = 25
)


names(DNBR_search$History)

df <- DNBR_search$History |> pivot_longer(c(B0, B2, B3, B5), names_to = "Param", values_to = "Val")

ggplot(df, aes(x = Val, y = Value)) + geom_point() + facet_wrap(~Param, scales = "free_x")

ggplot(DNBR_search$History, aes(x = B3, y = B5, size = Value)) + geom_point()








