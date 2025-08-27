library(tidyverse)
library(terra)
library(tidyterra)
library(gridExtra)

landisOutputDir <- file.path("F:\\2025_Q3_Scenarios\\TEST_WenEnt\\LANDIS_Sim_WenEnt_baseclim_Sc2_20250806_1530")
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
  dNBR[!is.finite(dNBR)]<-2000
  dNBR[dNBR<0]<-2000
  dNBR[dNBR>2000]<-2000
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
  mutate(DNBR = ifelse(DNBR > 1000, 1000, DNBR)) |>  # cap DNBR range at 1000 as bast that it's 100% mortality anyways
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
  pivot_longer(cols = starts_with("fine-fuels"), names_to = "Year", values_to = "Fines", names_prefix = "fine-fuels-")
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
  sample_frac(0.33) |>
  ungroup()

n_mtbs_obs <- nrow(mtbs.df)
pdistr_mtbs <- mtbs.df |>
  select(Severity, PWG) |>
  group_by(Severity, PWG, .drop = F) |>
  summarise(n = n()) |>
  group_by(PWG, .drop = F) |> mutate(p = n/n_mtbs_obs) |>
  select(PWG, Severity, p) |>
  pivot_wider(values_from = p, names_from = Severity)

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
B0_range <- seq(0.004, 0.008, 0.00025)  # intercept
B1_range <- seq(0.00, 0.03, 0.01)  # update: removing negative clay values option
B2_range <- seq(-0.00002, -0.00008, -0.00001)  # PET
B3_range <- seq(-0.00005, -0.0001, -0.00001)  # EWS
B4_range <- seq(-0.00001, -0.00008, -0.00001)  # CWD
B5_range <- seq(-0.005, -0.035, -0.005)  # FF
B6_range <- seq(-0.0000001, -0.0000015, -0.0000001)  # Ladders
FF_range <- seq(1500, 3000, 250)  # MaxFF

gc()
param_combos <- expand.grid(B0_range, B1_range, B2_range, B3_range, B4_range, B5_range, B6_range, FF_range) |>
  sample_frac()  #randomize order
gc()

param_combos$xdist_20 <- NA
param_combos$xdist_30 <- NA
param_combos$xdist_40 <- NA
param_combos$xdist_50 <- NA
param_combos$xdist_forest <- NA

tot=nrow(param_combos)
best_dist <- 99999999999999999
iter = 1
for(combo_id in sample(1:nrow(param_combos), nrow(param_combos))){
  print(paste(combo_id, '-- iteration', iter, 'out of', tot))

  dnbr_vals.df <- landis.df |>
    mutate(DNBR_new = mapply(siteSeverityFUN, param_combos$Var1[combo_id], param_combos$Var2[combo_id], param_combos$Var3[combo_id], param_combos$Var4[combo_id],
                                                                 param_combos$Var5[combo_id], param_combos$Var6[combo_id], param_combos$Var7[combo_id],
                                                                 Clay, PET, EWS, CWD, Fines/param_combos$Var8[combo_id], Ladders)) |> 
    select(DNBR_new, PWG) |>
    mutate(DNBR=DNBR_new) |>
    mutate(DNBR = ifelse(DNBR > 1000, 1000, DNBR)) |>
    # mutate(Severity = cut(DNBR, breaks=c(0,41,176,366,750,2001), labels=c('Unburned', 'Low', 'Moderate', 'High', 'Extreme')))
    mutate(Severity = cut(DNBR, breaks=c(0,176,366,750,2001), labels=c('Unburned-Low', 'Moderate', 'High', 'Extreme')))
  
  xdistr_landis <- dnbr_vals.df |>
    select(Severity, PWG) |>
    group_by(Severity, PWG, .drop = F) |>
    summarise(n = n()) |>
    select(PWG, Severity, n) |>
    pivot_wider(values_from = n, names_from = Severity, values_fill = 0) |>
    filter(!is.na(PWG))
  
  
  #xdist <- chisq.test(x = xdistr_landis[,2:5], p = pdistr_mtbs[,2:5], simulate.p.value = T)$statistic
  xdist <- ks.test(x = dnbr_vals.df$DNBR, y = mtbs.df$DNBR)$statistic
  
  if(is.nan(xdist)){
    xdist <- 99999999999999999
    iter <- iter+1
    next
  }
  
  
  if (xdist<best_dist){
    best_dist <- xdist
    print(paste('new best:', xdist, 'with params', paste(param_combos[combo_id,1:8], collapse='; ')))
    
    dnbr_vals.df <- landis.df |>
      mutate(DNBR_new = mapply(siteSeverityFUN, param_combos$Var1[combo_id], param_combos$Var2[combo_id], param_combos$Var3[combo_id], param_combos$Var4[combo_id],
                               param_combos$Var5[combo_id], param_combos$Var6[combo_id], param_combos$Var7[combo_id],
                               Clay, PET, EWS, CWD, Fines/param_combos$Var8[combo_id], Ladders),
             source = "LANDIS") |>
      select(DNBR_new, PWG, source) |> 
      mutate(DNBR=DNBR_new, PWG = as.factor(PWG)) |>
      mutate(DNBR = ifelse(DNBR > 1000, 1000, DNBR)) |>
      bind_rows(mtbs.df %>% select(DNBR, PWG, source)) |>
      # mutate(Severity = cut(DNBR, breaks=c(0,41,176,366,750,2001), labels=c('Unburned', 'Low', 'Moderate', 'High', 'Extreme')))
      mutate(Severity = cut(DNBR, breaks=c(0,41,176,366,750,2001), labels=c('Unburned', 'Low', 'Moderate', 'High', 'Extreme')))
    
    p1 <- ggplot(dnbr_vals.df%>%filter(!PWG%in%c(14,15)), aes(x=DNBR, fill=source)) + geom_density(alpha=0.5) + facet_wrap(~PWG, nrow=1) +
      ggtitle(paste(as.integer(xdist), '--', paste(param_combos[combo_id,1:8], collapse='/')))  +
      scale_fill_manual(values=c('grey30', 'grey90')) +
      annotate('rect', xmin=0, xmax=41, ymin=-Inf, ymax=0, alpha=.75, fill='darkgreen') +
      annotate('rect', xmin=42, xmax=176, ymin=-Inf, ymax=0, alpha=.75, fill='darkseagreen') +
      annotate('rect', xmin=177, xmax=366, ymin=-Inf, ymax=0, alpha=.75, fill='goldenrod1') +
      annotate('rect', xmin=367, xmax=750, ymin=-Inf, ymax=0, alpha=.75, fill='firebrick4') +
      annotate('rect', xmin=751, xmax=1000, ymin=-Inf, ymax=0, alpha=.75, fill='purple')+ geom_vline(xintercept=c(41,176,366,750), linetype='dotted') + theme_minimal()
    p2 <- ggplot(dnbr_vals.df%>%filter(!PWG%in%c(14,15))%>%group_by(PWG, Severity, source)%>%summarise(n=n())%>%group_by(PWG,source)%>%mutate(p=n/sum(n)), aes(x=Severity, y=p, fill=source, color=Severity)) +
      geom_bar(stat='identity', position = 'dodge', linewidth=2) + facet_wrap(~PWG, nrow=1) +
      scale_color_manual(values=c('darkgreen','darkseagreen','goldenrod1','firebrick4', 'purple')) +
      scale_fill_manual(values=c('grey30', 'grey90'))
    print(grid.arrange(p1, p2))
  }
  
  # param_combos[combo_id, 'xdist_50'] <- xdist50
  param_combos[combo_id, 'xdist_forest'] <- xdist
  iter <- iter+1
  
  if(iter%%5000==0){
    gc()
    write.csv(param_combos%>%filter(!is.na(xdist)), file.path(wDir, 'fire_calib_param_tests.csv'))
  }
}

which.min(param_combos$xdist_forest)
param_combos[which.min(param_combos$xdist_forest),]

print(paste('final best:', param_combos[which.min(param_combos$dist_forest),'dist_forest'], 'with params', paste(param_combos[which.min(param_combos$dist_forest),1:8], collapse='/')))

### Plot to see basic patterns
tested_combos <- param_combos %>% filter(!is.na(xdist_forest)) %>%
  mutate(final_metric = xdist_forest) %>%
  rename('Intercept'=Var1, 'Clay'=Var2, 'PrET'=Var3, 'EWS'=Var4, 'CWD'=Var5, 'FF'=Var6, 'LF'=Var7, 'MaxFF'=Var8) %>%
  mutate(best=as.factor(ifelse(final_metric<8000, 1, 0)))

quantile(tested_combos$xdist_forest)

tested_combos_long <- tested_combos %>%
  pivot_longer(cols=c('Intercept', 'Clay', 'PrET', 'EWS', 'CWD', 'FF', 'LF', 'MaxFF'), names_to = 'Parameter', values_to = 'ParameterValue') %>%
  mutate(ParameterValueFac=as.factor(ParameterValue))

ggplot(data=tested_combos_long%>%filter(final_metric<17000)) + geom_violin(aes(x=ParameterValueFac, y=final_metric)) + facet_wrap(~Parameter, scales='free_x')

ggpairs(tested_combos, columns = c('Intercept', 'Clay', 'PrET', 'EWS', 'CWD', 'FF', 'LF', 'MaxFF'), aes(color=best, alpha=0.001))
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


















