########################################################################################################################-
########################################################################################################################-
########################################################################################################################-
###                 FIT MODELS TO RELATE AGE AND BIOMASS TO HEIGHT AND FRACTIONAL COVER                           ######-
###                              USDA Forest Service PNWRS LANDIS-II MODEL                                        ######-
#-----------------------------------------------------------------------------------------------------------------------#
###   EXTENT: Wenatchee, Entiat, Okanogan, and Methow sub-basins                                                  ######-
###   PROJECT: BIOMASS Phase 3 for Pacific Northwest Research Station                                             ######-    
###   DATE: April 2022                                                                                            ######-   
#-----------------------------------------------------------------------------------------------------------------------#
###   Code developed by Tucker Furniss                                                                            ######-
###     Contact: tucker.furniss@usda.gov; tucker.furniss@gmail.com;                                               ######-
#-----------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------#
###   This script will use LANDFIRE and FIA data to fit models that relate age and biomass to estimate fractional coverage
###     and height, required inputs for dynamic vegetation in DHSVM.
#-----------------------------------------------------------------------------------------------------------------------
########################################################################################################################
#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
### Load packages: ----
library(tidyverse)
library(terra)
library(lme4)
#-----------------------------------------------------------------------------------------------------------------------
### Set data directory: ----
Dir<-'F:/LANDIS_Input_Data_Prep' # Location of R script
# Dir<-'C:/Users/TuckerFurniss/Desktop/LANDIS_R' # Location of R script
bigDataDir<-'F:/LANDIS_Input_Data_Prep/BigData' # Location of WA_FIA data and Riley dataset (RDS-2019-0026)
dataDir<-file.path(Dir,'Data') # Location of species codes, PWG raster, study area raster

LANDIS.EXTENT <- 'Tripod'

wdir <- file.path(Dir,LANDIS.EXTENT) # Location of output directory for LANDIS-II input files

#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
### Read plot list: ----
Riley_plot_list_full <- read.csv(file.path(bigDataDir,"TreeMap2016_RDS-2021-0074_Data","TreeMap2016_attribute_table.csv"))

### Read tree list: ----
fia.df.raw<-read.csv(file.path(wdir, paste0(LANDIS.EXTENT, '_tree_list.csv'))) 

head(fia.df.raw)

### Species code crosswalk (FIA, FVS, and LANDIS species codes): ----
species.codes <- read.csv(file.path(dataDir,"Species_code_crosswalk.csv"))

### Load raster data: ----
ecos.r <- rast(file.path(dataDir,"PWG",paste0("PWG_",LANDIS.EXTENT,".tif")))

## Pathway Group raster
#pwg.r <- rast(file.path(dataDir,"PWG/PWG_Wen_Ent_Oka_Met.tif"))  ## Ecoregion map
pwg.r <- ecos.r; names(pwg.r) <- "PWG"

## LUA codes
lua.r <- rast(file.path(dataDir,'PWG',paste0("LUA_",LANDIS.EXTENT,".tif"))) 

## Riley raster
riley.r <- rast(file.path(dataDir,paste0("TreeMap2016_NoExotics_",LANDIS.EXTENT,"_90m.tif"))); names(riley.r) <- 'tm_id'  ##Riley tree list, already trimmed to study area

### Define Pathway groups: ----
pwgs<-c('Water','Bareground','Grassland','Shrubland','Hardwood','Alpine meadow','Dry mixed conifer','Moist mixed conifer','Cold-moist conifer','Cold-dry conifer')
names(pwgs)<-c(10,11,12,13,14,15,20,30,40,50)


#-----------------------------------------------------------------------------------------------------------------------
### Subsample to grab 100 plots per PWG? ----
if(ext(pwg.r)!=ext(riley.r)) stop('Extent of pwg.r does not match extent of riley.r. This will cause major issues.')
id.to.pwg<-as.data.frame(c(pwg.r, riley.r)) %>%
  filter(!is.na(tm_id)) %>%
  group_by(PWG, tm_id) %>%
  summarise(Count = n()) %>% ungroup() %>%
  arrange(PWG, desc(Count))


## How many plots per PWG?
n=500

## Loop to grab the most abundant n plots:
common.ids.per.pwg<-id.to.pwg %>%
  group_by(PWG) %>% 
  arrange(desc(Count)) %>%
  slice_head(n=n)

### Reduce fia.df by these selected plots: 
fia.df <- fia.df.raw %>%
  filter(CN %in% common.ids.per.pwg$tm_id)  # Rarefied plots

## View result
r<-classify(riley.r,rcl=fia.df[,c('tm_id','PWG')])
r[r>50]<-NA
par(mfrow=c(1,2),oma=rep(0,4),mar=rep(0,4))
plot(r,legend=F)
plot(pwg.r,legend=F)

#-----------------------------------------------------------------------------------------------------------------------
### Prepare FIA data fields: ----
fia.df <- fia.df %>%
  filter(SpecCode%in%species.codes$SpecCode, AG_biomass_gm2 > 0, PWG%in%12:50) %>%  ## Drop problem rows:
  mutate(
    Height.m = HT/3.281,
    PWG = as.factor(PWG),
    Species = factor(SpecCode, levels = species.codes$SpecCode),
    Age = AGE_limit_by_longevity,
    Biomass_gm2 = AG_biomass_gm2,
    GROWTH_HABIT_CD = ifelse(DIA>=5, 'LT', 'SD')
    )

fia.df

### Aggregate all species together (for FC model): ----
## Merge by plot and subplot
# fia.plot.df<-setNames(aggregate(fia.df$Biomass_gm2,by=list(fia.df$tm_id,fia.df$CN,fia.df$SUBP,fia.df$PWG),FUN=sum),c('tm_id','PLT_CN','SUBP','PWG','Biomass.sum.gm2'))
# Merge by plot
fia.plot.df <- fia.df %>%
  group_by(tm_id, CN, PWG) %>%
  summarise(
    Biomass.sum.gm2 = sum(Biomass_gm2),
    Mean.Age = mean(Age),
    Age.90th = quantile(Age, probs=0.9)
    ) %>% ungroup() 


# fia.plot.df<-setNames(aggregate(fia.df$Biomass_gm2,by=list(fia.df$tm_id,fia.df$CN,fia.df$PWG),FUN=sum),c('tm_id','PLT_CN','PWG','Biomass.sum.gm2'))
# fia.plot.df$Mean.Age<-aggregate(fia.df$Age,by=list(fia.df$tm_id,fia.df$CN,fia.df$PWG),FUN=mean)$x
# fia.plot.df$Age.90th<-aggregate(fia.df$Age,by=list(fia.df$tm_id,fia.df$CN,fia.df$PWG),FUN=function(x) return(quantile(x,0.9)))$x

# fia.plot.df$Biomass.sum.gm2<-round(fia.plot.df$Biomass.sum.gm2,-1)
# fia.plot.df$Mean.Age<-round(fia.plot.df$Mean.Age,-1)
# fia.plot.df$Age.90th<-round(fia.plot.df$Age.90th,-1)

#-----------------------------------------------------------------------------------------------------------------------
### Create canopy cover data frame using LANDFIRE data: ----
# ## Load Landfire EVC layer, acquired through Google Earth Engine:
# lf.fc.r<-rast(file.path(Dir,'Data/LANDFIRE_EVC_WenEnt.tif'))
# 
# ## Convert to tree fractional coverage [0-1]:
# unique(values(lf.fc.r)) # Tree cover % are 3 digit numbers from 100 to 109. Cover % are as follows: 101 = 10-20%, 102 = 20-30%, etc.
# lf.fc.r[lf.fc.r<100|lf.fc.r>109]<-NA
# lf.fc.r
# (100:109 - 99.5 ) / 10
# lf.fc.r<- (lf.fc.r - 99.5) / 10 # /10 to convert to [0-1]
# 
# ## Reproject:
# lf.fc.r<-projectrast(lf.fc.r,ecos.r.Wen)
# lf.fc.r[is.na(ecos.r.Wen)]<-NA
# 
# plot(lf.fc.r,col=rev(hcl.colors(10,'Inferno')),xaxt='n',yaxt='n',legend=T,main='LANDFIRE CC') # LANDFIRE CC
# 
# ### Extract LANDFIRE FC values to data frame. Merge with FIA.DF: ---
# riley.r.Wen<-projectrast(riley.r,ecos.r.Wen,method='ngb')
# 
# fc.df<-data.frame('tm_id'=values(riley.r.Wen),'FC'=values(lf.fc.r))
# fc.df<-fc.df[!is.na(fc.df$tm_id)&!is.na(fc.df$FC),]
# 
# ## Calculate mean FC per tm_id?
# fc.df<-setNames(aggregate(fc.df$FC,by=list(fc.df$tm_id),FUN=mean),c('tm_id','FC'))
# 
# ## Merge to plot level age and biomass: 
# fc.df<-merge(fia.plot.df,fc.df,by='tm_id',all_x=T)
# 
# head(fc.df)

### Explore the relationship between canopy cover, age, and biomass: ---
## Okay, how the fuck are we going to estimate cover... correlations are super weak!
#--------------#
# 
# summary(lm(data=fc.df,FC~Mean.Age))
# summary(lm(data=fc.df,FC~Age.90th))
# summary(lm(data=fc.df,FC~Biomass.sum.gm2))
# 
# par(mfrow=c(1,3))
# plot(fc.df$Mean.Age,fc.df$FC)
# plot(fc.df$Age.90th,fc.df$FC)
# mtext('LANDFIRE FC',font=2)
# plot(fc.df$Biomass.sum.gm2,fc.df$FC)
#-----------------------------------------------------------------------------------------------------------------------
### Create canopy cover data frame using FVS-calculated canopy cover based on FIA data (from TreeMap circa 2016): ----
#Riley_plot_list_full$PLT_CN<-Riley_plot_list_full$CN

fc.df <- fia.plot.df %>%
  left_join(Riley_plot_list_full[,c('CN','CANOPYPCT','STANDHT')], by=c('CN')) %>%
  mutate(FC = CANOPYPCT/100)

# fc.df<-merge(fia.plot.df, Riley_plot_list_full[,c('PLT_CN','CANOPYPCT','STANDHT')], by=c('PLT_CN'))
# fc.df$FC<-fc.df$CANOPYPCT/100
head(fc.df)

## Add elevation to the data frame:
# Load DEM
dem.r <- rast(file.path(dataDir,paste0("DEM_90m_",LANDIS.EXTENT,".tif"))); names(dem.r) <- 'elevation'
# Project Riley Raster:
riley.r.AOI <- project(riley.r,ecos.r,method='near')
riley.r.AOI[is.na(ecos.r)]<-NA
# Merge:
dem.df <- c(riley.r.AOI, dem.r) %>% as.data.frame() %>%
  filter(!is.na(tm_id)) %>%
  group_by(tm_id) %>%
  summarise(Elevation = mean(elevation), n.plots = n()) %>%
  unique()

# dem.df<-data.frame('tm_id'=values(riley.r.AOI),'elevation'=round(values(dem.r),-2))
# dem.df<-dem.df[!is.na(dem.df$tm_id),]
# dem.df<-unique(dem.df)  # doesn't this screw up the mean calculation??? 
# t<-setNames(aggregate(dem.df$elevation,by=list(dem.df$tm_id),FUN=mean),c('tm_id','Elevation'))
# t$n.plots<-aggregate(dem.df$elevation,by=list(dem.df$tm_id),FUN=NROW)$x
# t$Elevation<-round(t$Elevation,0)
# dem.df<-merge(dem.df,t,by='tm_id')
# head(dem.df)
# 
# dem.df.unique<-unique(dem.df[,c('tm_id','Elevation')])
fc.df<-fc.df %>%
  mutate(tm_id = as.factor(CN)) %>%
  left_join(dem.df)
  
# merge(fc.df, dem.df.unique, by=c('tm_id'))

#--------------#

summary(lm(data=fc.df,FC~Mean.Age))
summary(lm(data=fc.df,FC~Age.90th))
summary(lm(data=fc.df,FC~Biomass.sum.gm2))

par(mfrow=c(1,3))
plot(fc.df$Mean.Age,fc.df$FC)
plot(fc.df$Age.90th,fc.df$FC)
mtext('FIA FC',font=2)
plot(fc.df$Biomass.sum.gm2,fc.df$FC)

ggplot(data=fc.df,aes(x=Biomass.sum.gm2,y=FC,col=Elevation)) + geom_point() + scale_color_continuous(type='viridis')

plot(fc.df$Elevation,fc.df$FC)

#--------------#

plot(fia.df$Age,fia.df$Height.m)
mtext('FIA Height',font=2)
plot(fia.df$Biomass_gm2,fia.df$Height.m)

#-----------------------------------------------------------------------------------------------------------------------
#### Create glm model with Time 0 Training Data: ----
#   Random effects: Species and PWG with random slope and intercept (0 + x|Species)
#     *Note:  lme4 cheat sheet here: https://stats.stackexchange.com/questions/13166/rs-lmer-cheat-sheet
#             -Intercepts only by random factor: (1 | random.factor)
#             -Slopes only by random factor: (0 + fixed.factor | random.factor)
#             -Intercepts and slopes by random factor: (1 + fixed.factor | random.factor)
#   Fixed effects: Age, Biomass, and LAI

#--------------#
### Fractional Cover model: ----
fc.glm <- loess(data = fc.df, FC ~ Mean.Age + Biomass.sum.gm2,span=0.2) # Use this to see the shape of a LOESS fit. This can help inform model form below.
fc.glm <- lmer(data = fc.df, FC ~ poly(Mean.Age,5) + poly(Biomass.sum.gm2,3) + poly(Elevation,2) + (1 + Biomass.sum.gm2|PWG),  na.action = na.exclude)

# warning('Refitting the FC model without age!!')
# fc.glm <- lmer(data = fc.df, FC ~ Biomass.sum.gm2 + (1 + Biomass.sum.gm2|PWG),  na.action = na.exclude)

fc.glm
summary(fc.glm)

fc.df$cc.pred<-predict(fc.glm,type='response')

# Approximation of an R2: ---
summary(lm(data = fc.df, FC ~ poly(Mean.Age,5) + poly(Biomass.sum.gm2,3) + poly(Elevation,2) + factor(PWG)))
# summary(lm(data = fc.df, FC ~ Biomass.sum.gm2 + factor(PWG))) # Removing age doesn't help much. And age is significant. Keep it in there.

# View model predictions:
par(mfrow=c(1,3),oma=c(0,0,0,0),mar=c(4,4,0,0))
plot(seq(1000,50000,1000),predict(fc.glm,newdata=data.frame('Mean.Age'=100,'Biomass.sum.gm2'=seq(1000,50000,1000),'Elevation'=1000,'Species'='PseuMenz','PWG'=30),type='response'),type='l',ylim=c(0,1),xlab='Biomass',ylab='FC (%)')
points(fc.df[fc.df$Mean.Age>90&fc.df$Mean.Age<110,c('Biomass.sum.gm2','FC')])

par(mar=c(4,2,0,0))
plot(seq(0,200,10),predict(fc.glm,newdata=data.frame('Mean.Age'=seq(0,200,10),'Biomass.sum.gm2'=10000,'Elevation'=1000,'Species'='PseuMenz','PWG'=30),type='response'),type='l',ylim=c(0,1),xlab='Age')
points(fc.df[fc.df$Biomass.sum.gm2>9000&fc.df$Biomass.sum.gm2<11000,c('Mean.Age','FC')])

plot(seq(0,2500,100),predict(fc.glm,newdata=data.frame('Mean.Age'=100,'Biomass.sum.gm2'=10000,'Elevation'=seq(0,2500,100),'Species'='PseuMenz','PWG'=30),type='response'),type='l',ylim=c(0,1),xlab='Age')
points(fc.df[fc.df$Mean.Age>80&fc.df$Mean.Age<120,c('Elevation','FC')])
 
# plot(seq(0,200,10),predict(fc.glm,newdata=data.frame('Mean.Age'=seq(0,200,10),'Biomass.sum.gm2'=seq(0,40000,2000),'Species'='PseuMenz','PWG'=30),type='response'),type='l',ylim=c(0,1),xlab='Years')
# points(fc.df[,c('Mean.Age','FC')])

#--------------#
### Height model: ----
ht.glm <- lmer(data = fia.df, Height.m  ~ log(Age) + log(Biomass_gm2) + (1 + Age|Species) + (1 + Age|PWG), na.action = na.exclude) 

ht.glm
summary(ht.glm)

fia.df$ht.pred<-predict(ht.glm,type='response')


# Approximation of an R2: ---
# Linear form, R2 = 0.74
summary(lm(data = fia.df, Height.m ~ Age + Biomass_gm2 + factor(Species) + factor(PWG)))
# Log form, R2 = 0.81
summary(lm(data = fia.df, Height.m ~ log(Age) + log(Biomass_gm2) + factor(Species) + factor(PWG)))

# View model predictions:
par(mfrow=c(1,3),oma=c(0,0,0,0),mar=c(4,4,0,0))
plot(seq(10,1000,10),predict(ht.glm,newdata=data.frame('Age'=100,'Biomass_gm2'=seq(10,1000,10),'Species'='PseuMenz','PWG'=30),type='response'),type='l',xlab='Biomass',ylab='Height (m)')
points(fia.df[fia.df$Age>90&fia.df$Age<110,c('Biomass_gm2','Height.m')])

par(mar=c(4,2,0,0))
plot(seq(0,200,10),predict(ht.glm,newdata=data.frame('Age'=seq(0,200,10),'Biomass_gm2'=1000,'Species'='PseuMenz','PWG'=30),type='response'),type='l',xlab='Age')
points(fia.df[fia.df$Biomass_gm2>900&fia.df$Biomass_gm2<1100,c('Age','Height.m')])

plot(seq(0,200,10),predict(ht.glm,newdata=data.frame('Age'=seq(0,200,10),'Biomass_gm2'=seq(0,400,20),'Species'='PseuMenz','PWG'=30),type='response'),type='l',xlab='Years')
points(fia.df[,c('Age','Height.m')])

#-----------------------------------------------------------------------------------------------------------------------
### View some points: ----

#-----------------------------------------------------#
### Same thing with trend lines using ggplot:
theme_set(theme_classic()+theme(axis.text=element_text(size=6,color='black'),axis.title=element_text(size=7,color='black'),
                                legend.text=element_text(size=6,color='black'),legend.title=element_text(size=7,color='black'),
                                legend.key.size=unit(0.5,'cm'),axis.ticks = element_line(color='black'),
                                panel.background = element_rect(color='black',fill=NA,size=1)))

## Cover ~ Biomass: ---
ggplot(fc.df,aes(x=Biomass.sum.gm2,y=FC,colour=PWG))+
  ylim(0,1)+xlim(0,60000)+
  geom_point(alpha=0.5)+
  geom_line(col='grey70',aes(y=cc.pred))+
  stat_smooth(method='lm',formula=y~x,color='black',fill='grey80',size=0.5,se=F,linetype='dotdash')+
  # stat_smooth(method='loess',formula=y~x,span=100,color='grey50',se=F,linetype='dotdash')+
  # scale_x_log10()+ # This prevents modeled CC from ever reaching 100%
  facet_wrap(~PWG,nrow=2,labeller=labeller(PWG=function(x) return(pwgs[x])))+
  scale_colour_brewer(palette = "Spectral")
dev.print(tiff,file=file.path(Dir, LANDIS.EXTENT, 'Cover_vs_biomass.tiff'),width=6.5,height=5,res=600,units='in',compression='lzw')

## Cover ~ Age: ---
ggplot(fc.df,aes(x=Mean.Age,y=FC,colour=PWG))+
  ylim(0,1)+xlim(0,200)+
  geom_point(alpha=0.5)+
  geom_line(col='grey70',aes(y=cc.pred))+
  stat_smooth(method='lm',formula=y~x,color='black',fill='grey80',size=0.5,se=F,linetype='dotdash')+
  facet_wrap(~PWG,nrow=2,labeller=labeller(PWG=function(x) return(pwgs[x])))+
  scale_colour_brewer(palette = "Spectral")
dev.print(tiff,file=file.path(Dir, LANDIS.EXTENT, 'Cover_vs_age.tiff'),width=6.5,height=5,res=600,units='in',compression='lzw')

## Polynomial fit:
ggplot(fc.df,aes(x=Biomass.sum.gm2,y=FC,colour=PWG))+ylim(0,1)+
   geom_point(alpha=0.5)+
   stat_smooth(method='glm',formula=y~poly(x,3),method.args = list(family = "binomial"))+
  facet_wrap(~PWG,nrow=2,labeller=labeller(PWG=function(x) return(pwgs[x])))+
  scale_colour_brewer(palette = "Spectral")
#-----------------------------------------------------#

## Height ~ Biomass: ---
ggplot(fia.df,aes(x=Biomass_gm2,y=Height.m,colour=PWG))+ylim(0,60)+
  geom_point(alpha=0.5)+
  geom_line(col='grey70',aes(y=ht.pred))+
  stat_smooth(method='lm',formula=y~log(x),color='black',fill='grey80',size=0.5,se=F,linetype='dotdash')+
  facet_wrap(~PWG,nrow=2,labeller=labeller(PWG=function(x) return(pwgs[x])))+
  scale_colour_brewer(palette = "Spectral")
dev.print(tiff,file=file.path(Dir, LANDIS.EXTENT, 'Height_vs_biomass.tiff'),width=6.5,height=5,res=600,units='in',compression='lzw')

## Height ~ Age: ---
ggplot(fia.df,aes(x=Age,y=Height.m,colour=PWG))+
  ylim(0,60)+xlim(0,200)+
  geom_point(alpha=0.5)+
  geom_line(col='grey70',aes(y=ht.pred))+
  stat_smooth(method='lm',formula=y~log(x),color='black',fill='grey80',size=0.5,se=F,linetype='dotdash')+
  facet_wrap(~PWG,nrow=2,labeller=labeller(PWG=function(x) return(pwgs[x])))+
  scale_colour_brewer(palette = "Spectral")
dev.print(tiff,file=file.path(Dir, LANDIS.EXTENT, 'Height_vs_age.tiff'),width=6.5,height=5,res=600,units='in',compression='lzw')

#-----------------------------------------------------------------------------------------------------------------------
### Export model objects: ----
save(ht.glm,file=file.path(Dir, LANDIS.EXTENT, 'Ht_AgeBiomassSppPWG_glm.Rdata'))
save(fc.glm,file=file.path(Dir, LANDIS.EXTENT, 'FC_AgeBiomassSppPWG_glm.Rdata'))

#-----------------------------------------------------------------------------------------------------------------------
########################################################################################################################
#-----------------------------------------------------------------------------------------------------------------------
### Compare with LANDFIRE EVC layer: ----

## Load ecos.r
#ecos.r <- rast(file.path(dataDir,"PWG/PWG_Wen_Ent.tif"))

#-------------------------------------------#
## Load Landfire EVC layer, acquired through Google Earth Engine:
lf.fc.r<-rast(file.path(Dir,paste0('Data/LANDFIREv2.0_EVC_',LANDIS.EXTENT,'.tif')))

lf.fc.r<-project(lf.fc.r,ecos.r,method='near')
## View overlap:
# plot(lf.fc.r)
# plot(ecos.r,add=T,col='red')

unique(values(lf.fc.r)) # Tree cover % are 3 digit numbers from 100 to 109. Cover % are as follows: 101 = 10-20%, 102 = 20-30%, etc.
lf.fc.r[lf.fc.r<100|lf.fc.r>109]<-NA
lf.fc.r

(100:109 - 99.5 ) / 10

lf.fc.r <- (lf.fc.r - 99.5) / 10

#-------------------------------------------#
## Load LANDIS-II FC layer:

## Year 0 Fractional Cover layer, generated with LANDIS-II:
fc.r<-rast(file.path(Dir,'Data/FracCov_yr-0.tif'))
# Mask out NA values
fc.r[fc.r==0]<-NA
fc.r[ecos.r<13]<-NA

# Lower grass cover % for comparison with landfire. This is the lowest FC bin anyways.
fc.r[fc.r<0.11]<-0.05 
# fc.r[fc.r<0.1]<-NA

lf.fc.r[is.na(fc.r)]<-NA
# lf.fc.r[lf.fc.r<0.1]<-NA

## Function to roundup ages to nearest X: ----
roundTo <- function(x,base=0.1){ 
  base*round(x/base) 
} 

## Round to nearest 0.05 to match with landfire
data.frame('is'=seq(0,1,0.01),'becomes'=roundTo(seq(0,1,0.01)+0.05) - 0.05)

fc.r<-roundTo(fc.r+0.05) - 0.05

#-------------------------------------------#
## Load canopy cover based on Riley Raster (without using LANDIS-II outputs):

riley.r <- rast(file.path(dataDir,"TreeMap2016_NoExotics_WenEntOkaMet_90m.tif"))  ##Riley tree list, already trimmed to study area
riley.r <- project(riley.r,ecos.r,method='near')
riley.r[is.na(ecos.r)]<-NA

riley.df <- read.csv(file.path(bigDataDir,"TreeMap_RDS-2021-0074","TreeMap2016_attribute_table.csv"))

## Generate FIA-based canopy cover raster:
riley.fc.r<-reclassify(riley.r,rcl=data.frame('is'=riley.df$tm_id,'becomes'=riley.df$CANOPYPCT/100))

riley.fc.r<-roundTo(riley.fc.r+0.05) - 0.05

#-------------------------------------------#
## Fill in cover for the low cover areas (with non-zero biomass):
riley.fc.r[is.na(riley.fc.r)&!is.na(fc.r)]<-0.05
lf.fc.r[is.na(lf.fc.r)&!is.na(fc.r)]<-0.05


#-------------------------------------------#
## Summarize frequency:
counts<-data.frame(freq(fc.r,digits=2))
counts$lf<-freq(lf.fc.r,digits=2)[,'count']
counts$riley<-freq(riley.fc.r,digits=2)[,'count']
counts<-counts[!is.na(counts$value),]

#-------------------------------------------#
## View result:
cols<-rev(hcl.colors(10,'viridis'))

par(mfrow=c(2,3),oma=c(0,0,0,0),mar=c(0,0,1,0),cex=1,ps=10,mgp=c(1,0.2,0),tck=-0.02)
plot.new();plot.window(xlim=ext(lf.fc.r)[1:2], ylim=ext(lf.fc.r)[3:4],xaxs="i",yaxs="i",asp=1)
plot(lf.fc.r,col=cols,breaks=seq(0,1,0.1),xaxt='n',yaxt='n',legend=F,add=T);mtext('LANDFIRE FC',font=2,line=-0.1);box('figure') # LANDFIRE CC
plot.new();plot.window(xlim=ext(lf.fc.r)[1:2], ylim=ext(lf.fc.r)[3:4],xaxs="i",yaxs="i",asp=1)
plot(riley.fc.r,col=cols,breaks=seq(0,1,0.1),xaxt='n',yaxt='n',legend=F,add=T);mtext('TreeMap (FVS-based) FC',font=2,line=-0.1);box('figure') # LANDIS-II CC, based on the FIA-based CC model.
plot.new();plot.window(xlim=ext(lf.fc.r)[1:2], ylim=ext(lf.fc.r)[3:4],xaxs="i",yaxs="i",asp=1)
plot(fc.r,col=cols,breaks=seq(0,1,0.1),xaxt='n',yaxt='n',legend=F,add=T);mtext('LANDIS-II (FVS-based) FC',font=2,line=-0.1);box('figure') # LANDIS-II CC, based on the FIA-based CC model.

par(mar=c(2,2,3,0))
barplot(counts$lf,col=cols,yaxt='n',ylab='Frequency',xlab='Fractional cover',main='');box('figure');axis(1,at=seq(0.5,9.5,1)*1.21,labels=counts$value);axis(2,labels=F)
barplot(counts$riley,col=cols,yaxt='n',ylab=NULL,xlab='Fractional cover',main='');box('figure');axis(1,at=seq(0.5,9.5,1)*1.21,labels=counts$value);axis(2,labels=F)
barplot(counts$count,col=cols,yaxt='n',ylab=NULL,xlab='Fractional cover',main='');box('figure');axis(1,at=seq(0.5,9.5,1)*1.21,labels=counts$value);axis(2,labels=F)

dev.print(tiff,file=file.path(Dir,'Output/Height_and_PercentCover/FracCov_LANDFIRE_vs_LANDIS-fvs.tiff'),width=7,height=5,res=600,units='in',compression='lzw')


fc.r[lf.fc.r>=0.75]

#-----------------------------------------------------------------------------------------------------------------------
########################################################################################################################
#####################################################    END    ##################################################   ---- 
stop('\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n
                                                      ####################################################
                                                            =========================================
                                                              -------------------------------------
                                                              
                                                           Code ran successfully! No errors detected.
                                                                  
                                                              -------------------------------------
                                                          *********************************************
                                                     ######################################################\n',call.=F)

#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
########################################################################################################################
########################################################################################################################
########################################################################################################################
########################################################################################################################
#-----------------------------------------------------------------------------------------------------------------------
### Create canopy cover data frame using FIA data (old approach! Leads to strong bias towards high canopy cover.): ----
state='WA'
year=2005

dir<-file.path(bigDataDir,"FIA",paste0(state,'_FIA'))

## Use Subplot Structure Table for total canopy cover:
loadFIAsubplot<-function(states,year){
  df<-c()
  for(state in states){
    dir<-file.path(bigDataDir,"FIA",paste0(state,'_FIA'))
    temp<-read_csv(file.path(dir,paste0(state,"_P2VEG_SUBP_STRUCTURE.csv")), guess_max = 10000,progress=show_progress()) %>%
      filter(INVYR > year) %>%
      filter(LAYER%in%c(1:5)) %>%
      filter(GROWTH_HABIT_CD=='TT') %>%
      dplyr::select(PLT_CN,SUBP,GROWTH_HABIT_CD,LAYER,COVER_PCT)
    temp$STATE<-state
    df<-rbind(df,temp)
  }
  return(df)
}
P2VEG.SUBPLOT.df<-loadFIAsubplot(states=c('WA','ID','OR','MT'),year=2005)
P2VEG.SUBPLOT.df$PLT_CN<-as.character(P2VEG.SUBPLOT.df$PLT_CN)

fia.df$CN.char<-as.character(fia.df$CN)

fia.plot.biomass<-setNames(aggregate(fia.df$Biomass_gm2,by=list(fia.df$CN.char,fia.df$SUBP,fia.df$PWG),FUN=sum),c('PLT_CN','SUBP','PWG','Biomass.sum.gm2'))
fia.plot.biomass$Mean.Age<-aggregate(fia.df$Age,by=list(fia.df$CN.char,fia.df$SUBP,fia.df$PWG),FUN=mean)$x
fia.plot.biomass$Age.90th<-aggregate(fia.df$Age,by=list(fia.df$CN.char,fia.df$SUBP,fia.df$PWG),FUN=function(x) return(quantile(x,0.9)))$x

fia.plot.biomass$Biomass.sum.gm2<-round(fia.plot.biomass$Biomass.sum.gm2,-1)
fia.plot.biomass$Mean.Age<-round(fia.plot.biomass$Mean.Age,-1)
fia.plot.biomass$Age.90th<-round(fia.plot.biomass$Age.90th,-1)

fia.plot.biomass<-fia.plot.biomass[order(fia.plot.biomass$PWG,fia.plot.biomass$PLT_CN,fia.plot.biomass$SUBP),]

cover.df.full<-fia.plot.biomass %>% inner_join(P2VEG.SUBPLOT.df, by = c("PLT_CN","SUBP"))
cover.df.full

### Explore the relationship between canopy cover, age, and biomass: ----
## Okay, how the fuck are we going to estimate cover... correlations are super weak!
#--------------#
# Layer 5
cover.df<-cover.df.full[cover.df.full$LAYER==5,]

summary(lm(data=cover.df,COVER_PCT~Mean.Age))
summary(lm(data=cover.df,COVER_PCT~Age.90th))
summary(lm(data=cover.df,COVER_PCT~Biomass.sum.gm2))

par(mfrow=c(1,3))
plot(cover.df$Mean.Age,cover.df$COVER_PCT)
plot(cover.df$Age.90th,cover.df$COVER_PCT)
mtext('Total cover',font=2)
plot(cover.df$Biomass.sum.gm2,cover.df$COVER_PCT)


#--------------#
# Layer 3
cover.df<-cover.df.full[cover.df.full$LAYER==3,]

summary(lm(data=cover.df,COVER_PCT~Mean.Age))
summary(lm(data=cover.df,COVER_PCT~Age.90th))
summary(lm(data=cover.df,COVER_PCT~Biomass.sum.gm2))

par(mfrow=c(1,3))
plot(cover.df$Mean.Age,cover.df$COVER_PCT)
plot(cover.df$Age.90th,cover.df$COVER_PCT)
mtext('Layer 3 cover',font=2)
plot(cover.df$Biomass.sum.gm2,cover.df$COVER_PCT)

#--------------#
# Layer 4
cover.df<-cover.df.full[cover.df.full$LAYER==4,]

summary(lm(data=cover.df,COVER_PCT~Mean.Age))
summary(lm(data=cover.df,COVER_PCT~Age.90th))
summary(lm(data=cover.df,COVER_PCT~Biomass.sum.gm2))

par(mfrow=c(1,3))
plot(cover.df$Mean.Age,cover.df$COVER_PCT)
plot(cover.df$Age.90th,cover.df$COVER_PCT)
mtext('Layer 4 cover',font=2)
plot(cover.df$Biomass.sum.gm2,cover.df$COVER_PCT)

## Growth Habit:
#   - TT = tally tree species (including seedlings, saplings, and mature trees)
#   - NT = non-tally tree species (rare species for the area, I guess?)
#   - SD = seedlings and saplings
#   - LT = large trees (greater than 5 in dbh)
#   - SH = shrubs
#   - FB = forbs
#   - GR = graminoids

## Layer 1: 0 to 2 ft
## Layer 2: 2 to 6 ft
## Layer 3: 6 to 16 ft
## Layer 4: 16+ ft
## Layer 5: Canopy cover for all layers
### Compare with FIA-derived canopy cover: ----

## Load FIA canopy cover: 
P2VEG.SUBPLOT.df$CN<-P2VEG.SUBPLOT.df$PLT_CN

fia.cc.df<-merge(unique(data.frame('CN'=fia.df.raw$CN,'tm_id'=fia.df.raw$tm_id,'SUBP'=fia.df.raw$SUBP)), 
                 P2VEG.SUBPLOT.df, by = c("CN","SUBP"))

head(fia.cc.df)

temp<-unique(values(riley.r))
temp[!temp%in%fia.cc.df$tm_id]

# OH NO! Some plots, like CN# 40220135010497, have no entry in the P2VEG.SUBPLOT table because we filtered that table 
#       by trees (GROWTH_HABIT_CD == TT), and some plots have no CC measurements for tree species despite having trees in the plot (because FIA data is messy!).
#       It appears this is an issue with MANY plots, biasing the Fractional Cover model towards plots with high tree canopy cover.

## Load riley raster:
riley.r <- rast(file.path(dataDir,"study_area_tl_2014_no_exotics_Wen_Ent_Oka_Met.tif"))  ##Riley tree list, already trimmed to study area

fia.cc.df[fia.cc.df$tm_id==74468,]

## Generate FIA-based canopy cover raster:
cc.r<-list()
for(i in 1:4){ # Subplot loop
  cat('Subplot',i,'...')
  for(j in 1:5){ # Layer loop
    temp <- fia.cc.df[fia.cc.df$SUBP==i&fia.cc.df$LAYER==j,]
    r<-reclassify(riley.r,rcl=data.frame('is'=temp$tm_id,'becomes'=temp$COVER_PCT/100))
    r[r>1]<-NA
    cc.r[[paste('SUBP',i,'LAYER',j,sep='.')]] <- r
  }
}

## Wow... lots of plots missing canopy cover measurements for tree species. These maps are pretty empty...
plot(cc.r[['SUBP.1.LAYER.4']],col=rev(hcl.colors(10,'Inferno')),xaxt='n',yaxt='n',legend=F) # Total cover
plot(cc.r[['SUBP.1.LAYER.5']],col=rev(hcl.colors(10,'Inferno')),xaxt='n',yaxt='n',legend=F) # Canopy cover (16+ ft.)


#-----------------------------------------------------------------------------------------------------------------------
########################################################################################################################
########################################################################################################################
########################################################################################################################
########################################################################################################################