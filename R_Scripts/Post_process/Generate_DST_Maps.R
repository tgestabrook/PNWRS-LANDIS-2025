if(file.exists(file.path(landisOutputDir,'DST','DST_Metrics_by_HUC12.csv')) & RERUN.DST.MAPS == F){
  cat('\nDST outputs already exist for',gsub(dirToProcess,"",landisOutputDir),'. Skipping to next sim...')
  next
}
cat('\n\n***************************************************************************************************
Generating DST output maps for',gsub(dirToProcess,"",landisOutputDir),'             
***************************************************************************************************\n')

### Create Output folder: ----
if(!dir.exists(file.path(landisOutputDir, 'DST'))) {
  dir.create(file.path(landisOutputDir, 'DST'))
} 

### Create Output Table: ----
dst.pwg.df<-expand.grid('Year'=1:max(as.numeric(yrs)),'PWG'=names(ecos)) %>%
  arrange(Year, PWG)

dst.huc12.df<-expand.grid('Year'=1:max(as.numeric(yrs)),'HUC12'=unique(HUC12.r[!is.na(HUC12.r)])) %>%
  arrange(Year, HUC12)

#-----------------------------------------------------#
### Define biomass harvest costs and value: ----

## Sources:
# - https://www.fpl.fs.fed.us/documnts/fplrp/fpl_rp701.pdf
# - http://www.globalwood.org/tech/tech_wood_weights.htm
# - https://extension.psu.edu/calculating-the-green-weight-of-wood-species

## Units: 
# 1000 board feet = 17.5 short tons (air dried softwood logs), 20.0 short tons (air dried hardwood logs), ~2000 lbs kiln dried lumber (softwood).
# 1 cord = 2.65 "green tons"
# 1000 board feet of doug fir = 3200 lbs green, 2850 lbs air dry, 8700 lbs in 12" logs. http://www.globalwood.org/tech/tech_wood_weights.htm
# 1000 board feet of ponderosa = 3750 lbs green, 2350 lbs air dry, 11,300 lbs in 12" logs.
# 1000 board feet = 83.33 cubic feet = 2.36 m^3
# 1 Megagram = 2204.62 lbs
# 2204.62 lbs / (3500 lbs per 1k bd.ft.) = 630 board feet
# 600 bd ft ~ 1800 lbs ~ 0.8 Mg.
### 1 Megagram ~ 588 bd ft. (ponderosa) to 689 bd ft. (doug fir) ### (total, including slash and non-merch)
### Assuming 12" logs, ratio of merch weight/log weight weight is 3200/8700 (37%) for doug fir and 3750/11300 (33%) for ponderosa.
### So 1 Mg is 2204.6 lbs, that produces ~800 lbs merch and 1400 lbs non-merch, which is ~ 230 bd ft. merch and 0.7 tons chip.

#-----------------------------------------------------#
##  Revenue: ---
# Forest service stumpage prices: See table 21 in https://www.fpl.fs.fed.us/documnts/fplrp/fpl_rp701.pdf
# - East side Doug-fir stumpage: $125 per 1000 bd ft.
# - Ponderosa stumpage: $77 per 1000 bd ft.
# - Alder stumpage: $98 per 1000 bd ft.

# DNR mill delivered prices: https://www.dnr.wa.gov/publications/psl_ts_may22_logprices.pdf
# These prices are higher because mill delivered prices include "tumpage prices + cutting + skidding + loading + hauling + other fixed costs
# - Doug-fir: $420/1000 bd ft.
# - Ponderosa: $300/1000 bd ft.

#### Rough estimates of $ per 1k bd ft Merch ###
# $110 / 1000 board feet stumpage
# $450 / 1000 board feet mill delivered
# >$4000-14000 per 1000 board feet milled lumber value
Revenue.dollars.per.1k.BdFt.Merch <- 4000
# Convert to $/Mg merch using 650 bd ft/Mg.  
Revenue.dollars.per.Mg.Merch <- Revenue.dollars.per.1k.BdFt.Merch * 650/1000

#### $ per Mg Chip ###
# Convert to $/Mg chip using 2204.6 lbs/Mg.
Revenue.dollars.per.1ton.Chip <- 125 # mill delivered price of total fiber (residual and whole log chip): https://www.forest2market.com/blog/northwest-wood-chip-prices-level-off
Revenue.dollars.per.Mg.Chip <- Revenue.dollars.per.1ton.Chip  * (2204.6/2000)

##  Costs: ---
# Note: these values are all relative, not based on actual dollars.
New.Roads.dollars.per.mile<-10000

Harvest.dollars.per.Ha<-100
# Harvest.dollars.per.Stand<-1000
Harvest.dollars.per.Mg.per.meter<-1

Access.dollars.per.Km<-100

Harvest.dollars.per.Mg.Merch<-50
Harvest.dollars.per.Mg.Chip<-50

cable.yarding.multiplier<-0.5 # Cable yarding or tethered yarding system? Check with John Bailey at OSU

RxFire.ActiveForest.cost.USD.per.Ha <- 300 * 0.404686 # $300/ac to ha
RxFire.Wildlands.cost.USD.per.Ha <- 150 * 0.404686 # $150/ac to ha

#-------------------------------------------------------#

### Define Proportion Merchentable timber by prescription ID: ----
##  Note: This is proportion of removed cohorts that have any merchantable value. Merchentable % is further reduced
##   using the 35% multiplier, meaning for every 1 Mg merchentable biomass, only 35% gets turned into actual lumber.
##   So if 100 Mg is removed and this table says it is 60% merchentable, we will assume 60 Mg merchentable biomass, 
##   of which only 35% (21 Mg) gets turned into actual board feet. The rest becomes chip. So Chip weight = 40 Mg + 0.65*60 = 79 Mg.
##   35% is based on a 12" tree producing 120 board feet, which will weigh about 420 lbs (green). A 12" pine tree weighs 1500-2000 lbs. So 35% is really optimistic. 
prescriptId.df<-data.frame('Prescription'=c('MBValley', 'MBMesic', 'MBXeric', 'Industrial', 'PCTgeneric', 'Salvage','WA DNR', 'PCTdry', 'PCTmoist', 'PCTcold', 'CTdry', 'CTmoist', 'CCcold', 'WCcold'),
                           'RasterID'=2:15,  # raster value of 1 means an active site that wasn't harvested
                           'ProportionMerchCohorts'=c(0.8,0.8,0.6,0.9,0.0, 0.9, 0.9, 0.0, 0.0, 0.0, 0.6, 0.8, 0.8, 0.0),
                           'PercentMerch'=0.35)


### SUSTAINABLE BIOMASS: -------------------------------------------------------
cat('-> Calculating Biomass dynamics...\n')
harvestMerchProp.r<-classify(harvestPrescripts.r,rcl=data.frame('from'=prescriptId.df$RasterID,'to'=prescriptId.df$ProportionMerchCohorts * prescriptId.df$PercentMerch))
harvestedMG.r <- biomassRemoved.r * 0.01 * 0.81  # Convert g/m2 to mg/ha to Mg.


### merchMG.r
merchMg.r <- harvestMerchProp.r * harvestedMG.r


### chipMG.r
chipMg.r <- (1-harvestedMerchProp.r) * harvestedMG.r

### area.treated.r
area.treated.r <- ifel(harvestPrescripts.r > 1, 0.81, 0)  # if there is a prescription, assign cell area, otherwise zero

### delta.biomass.r
delta.biomass.r <- totalBiomass_stack.r - totalBiomass_stack.r[[1]]  # subtract year zero biomass from stack

new.outputs<-c('merchMg.r','chipMg.r','area.treated.r','delta.biomass.r')
names(new.outputs)<-c('Harvested_Merch_Mg','Harvested_Chip_Mg','Treated_Ha','Delta_Biomass_Mg')
outputs<-c(outputs,new.outputs)

### ECONOMICS: -----------------------------------------------------------------
cat('-> Calculating Economics\n')
### merchRevenue.r
merchRevenue.r<-merchMg.r * Revenue.dollars.per.Mg.Merch

### chipRevenue.r
chipRevenue.r<-chipMg.r * Revenue.dollars.per.Mg.Chip

### harvestCost.r
harvestCost.r <- 
  ((Harvest.dollars.per.Mg.Merch * merchMg.r) + (Harvest.dollars.per.Mg.Chip * chipMg.r)) * # harvesting costs
  (1 + slope.percent.r/100) + # slope multiplier
  (road.no.wild.dist.r * Harvest.dollars.per.Mg.per.meter * harvestedMg.r) + # distance to road multiplier per Mg
  (road.no.wild.dist.r/1000 * Access.dollars.per.Km) #+  # distance to road multiplier per harvest event
  (Harvest.dollars.per.Ha * area.treated.r)   # harvest size multiplier

### rxCost.r
RxFireHa.r <- ifel(ignitionTypeStack.r == 4, 0.81, 0)  # RX = 4 on ignition type maps
rxCost.r <- ifel(lua.r == 12, RxFireHa.r * RxFire.Wildlands.cost.USD.per.Ha, RxFireHa.r * RxFire.ActiveForest.cost.USD.per.Ha)  # different cost for wildland and managed

### suppressionCost.r
## NOTE: 10-year average suppression costs are 550 $/ha, 
#        according to https://www.nifc.gov/fire-information/statistics/suppression-costs. 
#        What suppression cost multiplier gives us an average cost of 550 $/ha??
#        Using the formula: mean(values(suppressionEffort.r) * x  / 0.81) = 550, we can calculate a value of x=19 gives a mean suppression cost of $550/ha.

suppressionEffort.r <- classify(fireIdStack.r, rcl=data.frame(fire.df$EventID,fire.df$MeanSuppressionEffectiveness))
suppressionEffort.r <- ifel(!is.na(fireIDStack.r) & is.na(suppressionEffort.r), 0, suppressionEffort.r)  # if there's a pixel where there was fire but no assigned suppressioneffort, set to zero
suppressionCost.r <- suppressionEffort.r * 19

new.outputs<-c('merchRevenue.r','chipRevenue.r','harvestCost.r','rxCost.r','suppressionCost.r')
names(new.outputs)<-c('Revenue_Merch_USD','Revenue_Chip_USD','Harvest_Cost_USD','Rx_Fire_Cost_USD','Fire_Suppression_Cost_USD')
outputs<-c(outputs,new.outputs)

### CARBON: --------------------------------------------------------------------
cat('-> Calculating Carbon dynamics...\n')
## Notes: 
# TotalC = SOMTC + LeafC + FRootC + WoodC + CRootC + SurfaceDeadWood + SoilDeadWood (source: NECN source code)
# However, NECN does not output maps for any of these metrics. 
# TotalC INCLUDES LIVE BIOMASS. We can re-create TotalC using the NECN-succession-log. 
# We can match the TotalC map value by summing the following fields in the NECN-succession-log:
# C_LiveLeaf + C_LiveFRoot + C_LiveWood + C_LiveCRoot + C_DeadWood + C_DeadCRoot + SOMTC
# TotalC does not include C_DeadLeaf_Struc, C_DeadLeaf_Meta, C_DeadFRoot_Struc, C_DeadFRoot_Meta 

# REMOVING 1000 FROM NEE MAPS! See https://usermanual.wiki/Pdf/LANDISII20Net20Ecosystem20CN20Succession20v4220User20Guide.678432021/help'
totalC.Mg.r <- rast(file.path(necnOutput, "TotalC-yr.tif"))  * 0.01 * 0.81


### AG.LiveBiomass.Mg.r -- aboveground live biomass
AG.LiveBiomass.Mg.r <- totalBiomass_stack.r * 0.01 * 0.81 # g/m2 to Mg/ha to Mg per cell.

### AG.LiveC.MgC.r -- aboveground carbon
AG.LiveC.MgC.r <- AG.LiveBiomass.Mg.r * 0.47

### SoilC.Mg.r -- soil carbon
SoilC.Mg.r <- rast(file.path(necnOutput, "SOMTC-yr.tif")) * 0.01 * 0.81 # g/m2 to Mg/ha to Mg C per cell.

### AG.DeadC.MgC.r -- deadwood carbon
AG.DeadC.MgC.r <- rast(file.path(necnOutput, "DeadWoodBiomass-yr.tif")) * 0.5 * 0.01 * 0.81 # Biomass to C, then g/m2 to Mg/ha to Mg C per cell.


BG.LiveDeadC.MgC.r <- (totalC.Mg.r - 
                         (AG.LiveC.MgC.r + 
                            SoilC.Mg.r + 
                            AG.DeadC.MgC.r * 0.5)) 

## On average, belowground dead is about 25% and belowground live is about 75% of total belowground carbon.
#  I calculated average percent using C_LiveFRoot, C_LiveCRoot, and C_DeadCRoot columns in NECN-succesion-log. Mean = 25%, median = 21%. 
# e.g., for ecoregion 30119, deadRootC = 1050, live fine roots + live coarse roots = 5056, total belowground C = 6091.
BG.LiveRootC.MgC.r <- BG.LiveDeadC.MgC.r * 0.79
BG.DeadRootC.MgC.r <- BG.LiveDeadC.MgC.r * 0.21

### NEE.MgC.per.yr -- Net C flux
NEE.MgC.per.yr <- (rast(file.path(necnOutput, "ANEE-yr.tif")) - 1000) * 0.01 * 0.81

### NEE.MgC.per.yr -- Net C sequestration
NPP.MgC.per.yr <- rast(file.path(necnOutput, "AG_NPP-yr.tif")) * 0.01 * 0.81

### C.turnover.years -- C turnover rate
C.turnover.years <- (AG.LiveC.MgC.r + AG.DeadC.MgC.r + SoilC.Mg.r + BG.LiveRootC.MgC.r + BG.DeadRootC.MgC.r) / NPP.MgC.per.yr

new.outputs<-c('AG.LiveBiomass.Mg.r','AG.LiveC.MgC.r','AG.DeadC.MgC.r','SoilC.Mg.r','BG.LiveRootC.MgC.r','BG.DeadRootC.MgC.r',
               'NEE.MgC.per.yr','NPP.MgC.per.yr')
names(new.outputs)<-gsub('[.]','_',new.outputs)
names(new.outputs)<-gsub('_r','',names(new.outputs))
outputs<-c(outputs,new.outputs)

### WATER: ---------------------------------------------------------------------



### FIRE: ----------------------------------------------------------------------
cat('-> Calculating Fire dynamics...\n')

### highSevFire.Ha.r
highSevFire.Ha.r <- ifel(severityStackSmoothedClassified.r == 4, 0.81, 0)

### lowSevFire.Ha.r
lowSevFire.Ha.r <- ifel(severityStackSmoothedClassified.r < 4, 0.81, 0)

### fireDMBR.r (formerly siteMortality)
fireDNBR.r <- ifel(severityStack.r == 1, 0, severityStack.r)  # reclass pixel value 1 (didn't burn but could) to 0

### mort.MG.r 
mort.Mg.r <- classify(fireIdStack.r, rcl=data.frame('is'=focal.yr.fire.df$EventID, 'becomes'=focal.yr.fire.df$TotalBiomassMortality / focal.yr.fire.df$TotalSitesBurned)) * 0.01 * 0.81

### mort.percent.r
mort.percent.r <- ifel(totalBiomass_stack.r[2:nlyr(totalBiomass_stack.r)] > 0, mort.Mg.r / totalBiomass_stack.r[2:nlyr(totalBiomass_stack.r)], 0)  # look at available biomass from the year before the fire
mort.percent.r <- ifel(mort.percent.r > 1, 1, mort.percent.r)  # still problem that because mortality is averaged, there will be some cells above 100%, but I think it's fine

### focal.yr.areaBurnedNonWilderness.Ha.r
focal.yr.areaBurnedNonWilderness.Ha.r <- ifel(!is.na(severityStackSmoothedClassified.r) & lua.r != 12, 0.81, 0)

### high.sev.dist.r -- distance to high severity patch edge
high.sev.dist.r <- distance(ifel(severityStackSmoothedClassified.r == 4, 1, 0), target = 0)
high.sev.dist.r <- ifel(pwg.r %in% c(10, 11) | is.na(pwg.r), 0, high.sev.dist.r)
high.sev.dist.r <- ifel(severityStackSmoothedClassified.r == 0, 0, high.sev.dist.r)


###surfaceFuels.r
surfaceFuels.r <- fineFuelStack.r

new.outputs<-c('fireDNBR.r','highSevFire.Ha.r','lowSevFire.Ha.r','RxFireHa.r','mort.Mg.r','mort.percent.r',
               'areaBurnedNonWilderness.Ha.r','severityStackSmoothedClassified.r',
               'high.sev.dist.r', 'surfaceFuels.r')
names(new.outputs)<-c('Fire_dNBR','Fire_High_Sev_Area_Ha','Fire_Low_Sev_Area_Ha','Fire_Rx_Area_Ha','Fire_Mortality_Mg','Fire_Mortality_Percent',
                      'Fire_Area_Burned_nonWilderness_Ha','Fire_Severity_Class',
                      'Distance_from_high_severity_m', 'Surface_Fuel_Mg')
outputs<-c(outputs,new.outputs)

### SMOKE: ---------------------------------------------------------------------
## Things to consider: Fire size, fire severity, biomass available, biomass consumed, burn day, 

## Emission factors and Modified Combustion Efficiency from Urbanski 2014. 
#   For Wildfire northwest conifer forest: MCE = 0.88, Pm2.5 EF = 23.2 g kg-1. 
#   For Rx fire  northwest conifer forest: MCE = 0.91, Pm2.5 EF = 17.6 g kg-1. 
#   For temperate forest duff: MCE = 0.75, Pm2.5 EF = 50 g kg-1
#   For stumps and logs, MCE = 0.80, Pm2.5 EF = 33 g kg-1. 

## Emission factors from Prichard et al. 2020:
#   Pm2.5 = 19.8 g kg-1, SD = 16.2, min = 1.1, max = 89.95.
#   Pm10 = 8.7 g kg-1, SD = 3.2, Min = 3.38, max = 12.9

emissions.Kg.r <- (flamingConsumptionStack.r + smolderConsumptionStack.r) * 0.01 * 0.81 * 1000 # g/m2 to Mg/ha to Mg to kg
emissions.pm2.5.Kg.r <- emissions.Kg.r * 19.8 / 1000
emissions.pm10.Kg.r <- emissions.Kg.r * 8.7 / 1000

### emissions.timing.r

## Create data frame for smoke emissions per fire event and assign emissions evenly across multi-day fires:
cat('   - Summarizing emissions per day...\n')

emissions.timing.r <- list()
for (yr in 1:simLength){
  cat(paste0('...', yr))
  focal.yr.fire.df <- fire.df |> filter(Year == yr)
  focal.yr.flamingCons.r <- flamingConsumptionStack.r[[yr]]
  focal.yr.smolderCons.r <- smolderConsumptionStack.r[[yr]]
  focal.yr.fireID.r <- fireIdStack.r[[yr]]
  
  if(nrow(focal.yr.fire.df)>0){
    smoke.df<-data.frame('Day'=focal.yr.fire.df$InitialDayOfYear,'EventID'=focal.yr.fire.df$EventID,
                         'FireSizeHa'=focal.yr.fire.df$TotalSitesBurned*0.81,'Duration'=sizeToDurationFUN(focal.yr.fire.df$TotalSitesBurned*0.81))
    for(i in 1:365){
      smoke.df[,ncol(smoke.df)+1]<-NA
      colnames(smoke.df)[ncol(smoke.df)]<-paste0('Day_',i)
    }
    
    for(id in smoke.df$EventID){
      # cat(paste0(id,', '))
      
      flameCons<-focal.yr.flamingCons.r[focal.yr.fireID.r==id]
      smolderCons<-focal.yr.smolderCons.r[focal.yr.fireID.r==id]
      flameCons[is.na(flameCons)]<-0 # A few small fires have NA for flaming consumption. Not sure why, I checked the source code and couldn't figure it out. One example is a fire with dNBR = 2000 and it only burned one site. Probably no big deal to just ignore this issue.
      smolderCons[is.na(smolderCons)]<-0 # As far as I can tell, smoldering consumption is never NA. But set to 0 just in case.
      
      fireDuration<-smoke.df |> filter(EventID == id) |> select("Duration") |> pull() # [smoke.df$EventID==id,'Duration']
      fireDay<-smoke.df |> filter(EventID == id) |> select("Duration") |> pull()  # [smoke.df$EventID==id,'Day']
      
      totalConsumption_Kg <- (flameCons + smolderCons) * 0.01 * 0.81 * 1000 # g/m2 to Mg/ha to Mg to kg
      totalConsumptionAveraged <- totalConsumption_Kg/fireDuration
      
      smoke.df[smoke.df$EventID==id,paste0('Day_',1:365)] <- 0
      smoke.df[smoke.df$EventID==id,paste0('Day_',fireDay:(fireDay+fireDuration-1))] <- round(sum(totalConsumptionAveraged),1)
    }
    smoke.df$TotalEmissions.perFire<-rowSums(smoke.df[,paste0('Day_',1:365)],na.rm=T)

    # Summarize total emissions per day for all fires actively burning
    emissions.df<-data.frame('Day'=1:365,'TotalEmissions.perDay'=colSums(smoke.df[,paste0('Day_',1:365)],na.rm=T)) |>
      mutate(Emissions_Pm25_Kg = round(TotalEmissions.perDay * 20 / 1000,1),# Emission factor 20 g / kg
             Emissions_Pm10_Kg = round(TotalEmissions.perDay * 8.7 / 1000,1)) |>
      mutate(TotalEmissions.perDay = ifelse(TotalEmissions.perDay == 0, NA, TotalEmissions.perDay)) |>
      mutate(Quantile = round(percent_rank(TotalEmissions.perDay),3))
    
    emissions.df
    
    # View result
    par(mfrow=c(1,2),oma=c(0,0,0,0),mar=c(2,2,0,0),mgp=c(1,0.2,0),tck=-0.01,ps=10,cex=1,cex.axis=0.8)
    plot(smoke.df$Day,smoke.df$FireSizeHa,xlab='Julian Day',ylab='Number of ignitions',pch=21,bg=alpha('darkred',0.2),xlim=c(1,365))
    plot(emissions.df$Day,emissions.df$Emissions_Pm25_Kg,pch=21,bg=alpha('blue',0.5),ylab='Emissions (Kg)',xlab='Julian Day')
    points(emissions.df$Day,emissions.df$Emissions_Pm10_Kg,pch=21,bg=alpha('orchid',0.5))
    legend('topleft',legend=c('Pm 2.5','Pm 10'),pt.bg=c(alpha('blue',0.5),alpha('orchid',0.5)),inset=0.02,pch=21)
    
    # Calculate emissions quantile, merge to smoke.df:
    # emissions.df[emissions.df$TotalEmissions.perDay==0,'TotalEmissions.perDay']<-NA
    emissions.df$Quantile<-round(percent_rank(emissions.df$TotalEmissions.perDay),3)
    emissions.df[is.na(emissions.df$Quantile),'Quantile']<-0
    emissions.df$Quantile.smoothed<-rollmean(emissions.df$Quantile,14,align = 'left',fill = 0)
    
    # Merge to smoke.df to assign quantile to each fire event ID:
    t<-merge(smoke.df,emissions.df,by='Day',all.y=T)
    t<-t[,c('Day','EventID','Duration','FireSizeHa','TotalEmissions.perFire','TotalEmissions.perDay','Quantile','Quantile.smoothed')]
    t[is.na(t$TotalEmissions.perDay),'TotalEmissions.perDay']<-0
    
    t$Emissions_Pm25_Kg <- round(t$TotalEmissions.perFire * 20 / 1000,1) # Emission factor 20 g / kg
    t$Emissions_Pm10_Kg <- round(t$TotalEmissions.perFire * 8.7 / 1000,1)
    
    emissions.df<-t
    
    lines(emissions.df$Day,emissions.df$Quantile.smoothed*max(emissions.df$Emissions_Pm25_Kg,na.rm=T),col='darkred',lty='dotdash')
    
    emissions.timing.yr.r<-classify(focal.yr.fireID.r,rcl=data.frame('from'=emissions.df$EventID,'to'=emissions.df$Quantile.smoothed))
    
  } else emissions.timing.r <- zero.r
  
  emissions.timing.r[yr] <- emissions.timing.yr.r
}

emissions.timing.r <- rast(emissions.timing.r)
names(emissions.timing.r) <- paste0("Emissions-timing-", 1:simLength)

new.outputs<-c('emissions.pm2.5.Kg.r','emissions.pm10.Kg.r','emissions.timing.r')
names(new.outputs)<-c('Smoke_Emissions_Kg_pm25','Smoke_Emissions_Kg_pm10','Smoke_Timing_quantile')
outputs<-c(outputs,new.outputs)

### FOREST HEALTH: -------------------------------------------------------------
cat('-> Calculating Forest Health...\n')

mean.age.top3.dom.r <- rast(ageOutput, "MeanAge_Top3Species-yr.tif")

### age.gt.120.r -- Forests where top 3 dominant cohorts by biomass are older than 120 years
age.gt.120.r <- ifel(mean.age.top3.dom.r > 120, 1, 0)

### seed.source.r -- Distance to seed source:
seed.source.r <- ifel(focal.yr.mean.age.top3.dom < 20|is.na(focal.yr.mean.age.top3.dom), 0, 1)
seed.source.r <- ifel(pwg.r %in% c(10:15) | is.na(pwg.r), 0, seed.source.r)

seed.source.dist.r<-distance(seed.source.r, target=0)
seed.source.dist.r <- ifel(pwg.r %in% c(10:15) | is.na(pwg.r), NA, seed.source.dist.r)


### delta.offsite.biomass.r -- Drought succeptible biomass
focal.yr.dominant.spp <- rast(ageOutput, "DominantSpecies-yr.tif")
focal.yr.dominant.spp <- rast(ageOutput, "DominantSpecies2-yr.tif")

spp <- BiomassTrees.stack |> select(contains("-0")) |> names()
dominant.spp.lookup<-1:length(spp[!spp%in%c("Nfixer_Resprt","NonFxr_Resprt","NonFxr_Seed","Grass_Forb","TotalBiomass")])
names(dominant.spp.lookup)<-spp[!spp%in%c("Nfixer_Resprt","NonFxr_Resprt","NonFxr_Seed","Grass_Forb","TotalBiomass")]

coverClass.rcl<-data.frame('is'=dominant.spp.lookup,'becomes'=NA)
coverClass.rcl[names(dominant.spp.lookup) %in% c('AcerMacr','AlnuRubr','BetuOcci','BetuPapy','CornNutt',
                                                 'FraxLati','PopuBals','PopuTrem','PrunEmar','QuerGarr'),'becomes']<-14
coverClass.rcl[names(dominant.spp.lookup) %in% c('PinuPond','PinuMont','LariOcci','JuniOcci','JuniScop'),'becomes']<-20
coverClass.rcl[names(dominant.spp.lookup) %in% c('AbieAmab','AbieGran','AbieProc','ChamNoot','PseuMenz','TaxuBrev','ThujPlic','TsugHete','TsugMert'),'becomes']<-30
coverClass.rcl[names(dominant.spp.lookup) %in% c('AbieLasi','PiceEnge','PinuCont'),'becomes']<-40
coverClass.rcl[names(dominant.spp.lookup) %in% c('LariLyal','PinuAlbi'),'becomes']<-50
coverClass.rcl

# Dominant cover
cover.type.r<-classify(focal.yr.dominant.spp,rcl=coverClass.rcl)
# Secondary cover
cover.type2.r<-classify(focal.yr.dominant.spp2,rcl=coverClass.rcl)

### IN MOIST FORESTS, RE-CLASSIFY COVER TYPE BASED ON SECOND DOMINANT SPECIES:
# We do this because douglas fir dominates by biomass so it makes what is actually dry and cold forest appear to be moist mixed conifer.
cover.type.r <- ifel(cover.type.r==30 & cover.type2.r>0 & !is.na(cover.type2.r), cover.type2.r, cover.type.r)

# 0 for non-forest sites:
cover.type.r <- ifel(is.na(focal.yr.total.biomass) | focal.yr.total.biomass==0 | is.na(cover.type.r), 0, cover.type.r)

# View result:
plot(cover.type.r,col=hcl.colors(100,'Temps'))

if(length(unique(values(cover.type.r)))>length(unique(values(pwg.r)))) 
  stop('Erronious cover types found. Check the focal.yr.dominant.spp raster. Interpolation may have messed it up.')

#-------------------------------------------------------#
## Biomass of mesic sp. on xeric sites and cold species on warm sites:
delta.1yr.biomass.r <- totalBiomass_stack.r - c(zero.r, totalBiomass_stack.r[1:(simLength - 1)])  # get the 1-year change at each timestep
# delta.1yr.biomass.r <- focal.yr.total.biomass - total.biomass[[paste0('total.biomass.',yr-1)]]

# cover.type.r[cover.type.r==30 & pwg.r==20]
# delta.1yr.biomass.r[cover.type.r==30 & pwg.r==20]
delta.offsite.biomass.r <- ifel((cover.type.r==30 & pwg.r==20)|  # moist mixed conifer on dry sites
                                  (cover.type.r==50 & pwg.r==20)|  # cold-dry conifer on dry sites
                                  (cover.type.r==40 & pwg.r==20)|  # cold-moist conifer on dry sites
                                  (cover.type.r==40 & pwg.r==30)|  # cold-moist conifer on moist sites
                                  (cover.type.r==50 & pwg.r==30)|  # cold-dry conifer on moist sites
                                  (cover.type.r==40 & pwg.r==50), delta.1yr.biomass.r, NA)  # cold-moist conifer on cold-dry sites
                                  
### patches.r -- area of homogenous patches
age.class.r<-round(mean.age.top3.dom, 40)

age.classes<-unique(age.class.r[age.class.r>0 & !is.na(age.class.r)])
cover.types<-unique(cover.type.r[cover.type.r>0 & !is.na(cover.type.r)])

cat('  - Area of homogenous patches... ')
patches.r<-zero.r
for(a in age.classes){
  for(c in cover.types){
    # Identify homogenous forests
    r <- ifel(age.class.r == a & cover.type.r == c, 1, 0)

    # Identify patches.r
    r2<-patches(r) 
    
    r3 <- zonal(r2, rast(zero.r, vals = 0.81), fun = "sum", as.raster = T)
    r3 <- ifel(is.na(r3), 0)
    patches.r<-patches.r+r3
    
    
    # Calculate area
    # rcl.df<-data.frame(freq(r2))
    # rcl.df$ha<-rcl.df$count * 0.81
    # r3<-classify(r2,rcl=data.frame('is'=rcl.df$value,'becomes'=rcl.df$ha))
    # r3[is.na(r2)]<-0
    # patches.r<-patches.r+r3
  }
}
plot(patches.r,col=hcl.colors(100,'Temps'))

### mature.patch.dist.r -- Dispersion of forest throughout the landscape (percent of landscape >1km from a mature forest patch):

mature.patch.dist.r<-patches(age.gt.120.r,directions=8)
mature.patch.dist.r <- ifel((mature.patch.dist.r == 0 | is.na(mature.patch.dist.r)) | pwg.r %in% c(10:15), 0, 1) |>
  distance(target=0)
mature.patch.dist.r <- ifel(pwg.r %in% c(10:12) | is.na(pwg.r), NA, mature.patch.dist.r)
plot(mature.patch.dist.r,col=hcl.colors(100,'Temps'))


### type.change.r -- Percent area that converts from forest PWG to non-forest PWG:
cover.type2.r <- ifel(focal.yr.mean.age.top3.dom < 20, 0, cover.type.r) ## Set stand initation phase to 0

type.change.r <- ifel(cover.type2.r[[1]] == 0 & cover.type2.r == 0, NA,  # non-forest to non-forest, non change
                      ifel(cover.type2.r[[1]] != 0 & cover.type2.r != 0, 0,  #forest to forest, no change
                            ifel(cover.type2.r[[1]] != 0 & cover.type2.r == 0, -1, 1)))  # forest to non-forest (-1) or non-forest to forest (1)
if(max(values(type.change.r), na.rm=T)>1) stop('Maximum value for type.change.r should be 1, but it is higher. Please resolve.')


### comp.change.r -- Persistence as initial forest type (% of pixels that stay as initial forest type):
comp.change.r <- ifel(cover.type2.r == cover.type2.r[[1]], 0, 1)

new.outputs<-c('age.gt.120.r','seed.source.r',
               'seed.source.dist.r','delta.offsite.biomass.r','patches.r')
names(new.outputs)<-c('Old_Forest','Seed_Source',
                      'Seed_Source_Distance_m','Delta_Offsite_Biomass_Mg','Area_of_Homogeneous_Forest_ha')
outputs<-c(outputs,new.outputs)
#-------------------------------------------------------#
### Write Rasters: ----
### TRIM RASTERS TO STUDY AREA: ----
cat('---------------------------------\n-> Trimming to study area...')
for(r.name in outputs){
  r<-eval(parse(text=r.name))
  
  r[is.na(r)]<-0 # Zeros for active cells with NAs
  r[is.na(pwg.r)|pwg.r<12]<-NA # NAs for inactive cells
  
  if(MASK == T)
    r[is.na(pwg.noBuffer.r)]<-NA # NAs for the 5-km study area buffer
  
  assign(r.name,r)
}

#-----------------------------------------------------------------------------------------------------------------------
### WRITE OUTPUT RASTERS: ----
# if(yr %in% YEARS.TO.OUTPUT){
cat('\n-> Writing output rasters...')
for(r.name in outputs){
  r<-eval(parse(text=r.name)) # Load layer
  
  r<-round(r,2)
  
  out.name<-names(outputs[outputs==r.name]) # Load out name
  
  names(r)<-r.name
  
  ## Write raster...
  writeRaster(r,file.path(landisOutputDir,'DST',paste0(out.name,'.tif')),overwrite=T)
}

### Calculate Metrics












