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

### SUSTAINABLE BIOMASS: ----
harvestMerchProp.r<-classify(harvestPrescripts.r,rcl=data.frame('from'=prescriptId.df$RasterID,'to'=prescriptId.df$ProportionMerchCohorts * prescriptId.df$PercentMerch))
harvestedMG.r <- biomassRemoved.r * 0.01 * 0.81  # Convert g/m2 to mg/ha to Mg.

### merchMG.r
merchMG.r <- harvestMerchProp.r * harvestedMG.r


### chipMG.r
chipMG.r <- (1-harvestedMerchProp.r) * harvestedMG.r

### area.treated.r
area.treated.r <- ifel(harvestPrescripts.r > 1, 0.81, 0)  # if there is a prescription, assign cell area, otherwise zero

### delta.biomass.r
delta.biomass.r <- totalBiomass_stack.r - totalBiomass_stack.r[[1]]  # subtract year zero biomass from stack


### ECONOMICS: ----
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
  #(Harvest.dollars.per.Ha * harvestSizeHa.r)   # harvest size multiplier

### rxCost.r


### suppressionCost.r































