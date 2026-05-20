library(tidyverse)
library(terra)
library(tidyterra)
library(landscapemetrics)
library(hydroMOPSO)

REBURNDir <- file.path("F:", "Tripod_Sim01d1_Thomas", "FireSeverity_Annual")
LANDIS.EXTENT<-'Tripod'
Dir <- file.path('F:/2025_Q4_Scenarios', LANDIS.EXTENT)

bigDataDir<-'F:/LANDIS_Input_Data_Prep/BigData'
dataDir<-'F:/LANDIS_Input_Data_Prep/Data'
modelDir<-'F:/V8_Models'

pwg.r <-  rast(file.path(dataDir, "PWG", paste0("PWG_", LANDIS.EXTENT, ".tif")))

severity.thresholds<-c(5, 41,176,376,2001);names(severity.thresholds)<-c('Unburned','Low','Moderate','High','max') # Miller & Thode 2007
severity.reclass.df<-data.frame('from'=c(-100, severity.thresholds[1:4]),'to'=c(severity.thresholds[1:5]),'becomes'=c(NA,1,2,3,4))
zonemap.df <- data.frame("PWG" = unlist(unique(pwg.r)), "zone" = 1:length(unlist(unique(pwg.r))))  # Needed for stupid freq function implementation

Sys.setenv(TMPDIR = "F:/R_TEMP")
terraOptions(tempdir = "F:/R_TEMP")


### Paramters to be fitted: Suppression at FWI 0-20, Suppression at FWI 20-40, and Suppression at FWI 40+



### Function to read LANDIS output and output a dataframe with fire regime data
read.LANDIS <- function(landisOutputDir){
  scrapplemaps <- dir(file.path(landisOutputDir, "social-climate-fire"))
  
  severity.r <- file.path(landisOutputDir, "social-climate-fire", scrapplemaps[grepl("fire-dnbr", scrapplemaps)]) |> rast() |> flip()
  crs(severity.r) <- crs(pwg.r)
  ext(severity.r) <- ext(pwg.r)
  
  simlength <- nlyr(severity.r)
  
  # evlog.df <- file.path(landisOutputDir, "social-climate-fire", "socialclimatefire-events-log.csv")
  events.r <- file.path(landisOutputDir, "social-climate-fire", scrapplemaps[grepl("event-ID", scrapplemaps)]) |> rast() |> flip() |> subst(0, NA) |>
    focal (w=3, fun = "modal", na.policy = 'only')
  
  crs(events.r) <- crs(pwg.r)
  ext(events.r) <- ext(pwg.r)
  
  #smooth
  severity.r <- ifel(severity.r<=1, NA, severity.r) |>  # have to class 0 and 1 (not burned) as NA so that median window only operates on unburned cells
    focal(w = 3, fun = median, na.rm=T, pad = TRUE, na.policy = 'only') |>
    classify(rcl=severity.reclass.df,include.lowest=T)
  
  
  # events.r <- ifel(!is.na(severity.r) & events.r == 0, NA, 0) |>  # if the smoothe severity got filled in with a value, we use modal to maybe assign those to the closest event id, should prevent blobbing fires beyond their footprint
  #   focal (w=3, fun = "modal", na.policy = 'only')

  ### need 3 dfs: event sizes, patch sizes, pwg area
  evs.df <- freq(events.r) |>
    filter(value != 0) |>
    mutate(EventSize = count*0.81)
  
  ptch.df <- lsm_p_area(severity.r) |>
    rename(Severity = class) |>
    pivot_wider(names_from = "metric", values_from = "value") |>
    mutate(Severity = as.factor(Severity))
  
  sev.pwg.df <- freq(severity.r, zones = pwg.r) |> left_join(zonemap.df) |>
    rename(Severity = value) |>
    mutate(
      Severity = as.factor(Severity),
      Area = count * 0.81,
      Arealog10 = log10(Area)
    )  
  
  fri.low.r <- ifel(severity.r %in% c(1,2), 1, 0) |> mean()
  
  fri.mod.r <-  ifel(severity.r %in% c(3), 1, 0) |> mean()
  
  fri.high.r <-  ifel(severity.r %in% c(4), 1, 0) |> mean()
  
  fri.total.r <- ifel(severity.r %in% c(1, 2, 3, 4), 1, 0) |> mean()
  
  return(
    list(
      "evs.df" = evs.df,
      "ptch.df" = ptch.df,
      "sev.pwg.df" = sev.pwg.df,
      "fri.low.r" = 1/fri.low.r,
      "fri.mod.r" = 1/fri.mod.r,
      "fri.high.r" = 1/fri.high.r,
      "fri.total.r" = 1/fri.total.r
    )
  )
}

testDir <- "F:\\V8_Models\\LANDIS_Sim_Tripod_CalibRun_20260515_1721"

test_landis <- read.LANDIS(testDir)


### Objective function to compare LANDIS output with REBURN data
gof.FUN <- function(){}


### Reburn data for comparison
REBURN_obs <- read.csv()


### Function to run LANDIS with simple parameter set
run.LANDIS <- function(suppression_params){  # input is 3 element vetcor with suppression levels, which ignore ignition type and  
  
  ### Write a new Suppression effort csv using the inputs
  
  ### Run LANDIS
  
  ### Read SCRAPPLE outputs and calculate summaries, then compare to REBURN and calculate objective function
  
  #### Read SCRAPPLE outputs
  read.LANDIS()
  
  #### Objective function: compare FWI at each severity level?
  gof.FUN()
  
}





# model.FUN = "LANDIS"
# model.FUN.args = list(
#   model.drty = ".",
#   param.files = ""  #????
#   exe.fname = "../_NOSUPP_CALIB.bat"
#   stdout = "stdout.txt"
#   stderr = "stderr.txt"
#   out.FUN = "read.LANDIS"
#   out.FUN.args = list(), # ????
#   verbose = TRUE,
#   gof.FUN = gof.FUN,
#   gof.FUN.args = list()
#   obs = REBURN_obs
# )

# out <- hydroMOPSO(
#   fn = "hydromod",
#   model.FUN = 'hydromod',
#   model.FUN.args = model.FUN.args,
#   method = 'spso2011',
#   conrol = list(
#     
#   )
# )

out <- hydromod(
  
)



