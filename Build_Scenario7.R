args <- commandArgs(trailingOnly = TRUE)

# read in the scenario list
scenario_list.df <- read.csv("Scenario_list.csv")
colnames(scenario_list.df)[1] <- "SCNUM"  # stupid but we gotta fix UTF encoding issue
selected_scnum <- args[1]
selected_scenario <- scenario_list.df[scenario_list.df$SCNUM == selected_scnum,]

# write a scenario.txt to model run directory
modeldir <- args[2]
outfile <- file.path(modeldir, "scenario.txt")

cat(paste0("LandisData    \"Scenario\"\n>>  Scenario:", 
           selected_scenario$Scenario, 
           "\n\nDuration     ", selected_scenario$Duration,"\n\nSpecies    ", selected_scenario$Species, 
           "\n\nEcoregions      ../EXTENT/ECOREGIONS_EXTENT.txt\n\nEcoregionsMap   ../EXTENT/ECOREGIONS_EXTENT.tif\n\nCellLength     90 << meters\n\n>>  Succession Extension    Initialization File\n>> ----------------------   -------------------\n"),
    file=outfile)

cat(paste0("\"NECN Succession\"     ", selected_scenario$NECN, "\n\n"), file=outfile, append=T)

if(!is.na(selected_scenario$SCRPPLE)|!is.na(selected_scenario$Harvest)){
    cat(">> Disturbance Extensions   Initialization File\n>> ----------------------   -------------------", file=outfile, append=T)
}

if(!is.na(selected_scenario$MH)){
    cat("\n\"Magic Harvest\"     ", file=outfile, append=T)
    cat(selected_scenario$MH, file=outfile, append=T)
}

if(!is.na(selected_scenario$SCRPPLE)){
    cat("\n\"SCRAPPLE\"     ", file=outfile, append=T)
    cat(selected_scenario$SCRPPLE, file=outfile, append=T)
}

if(!is.na(selected_scenario$Harvest)){
    cat("\n\"Biomass Harvest\"     ", file=outfile, append=T)
    cat(selected_scenario$Harvest, file=outfile, append=T)
}

cat("\n\nDisturbancesRandomOrder  no\n\n", file=outfile, append=T)
cat(">> Other Extensions   Initialization File\n>> ----------------------   -------------------\n\"Output Biomass\"	 	../ext_Output_Biomass10.txt\n\"Output Cohort Statistics\"	../ext_Output_CohortStats10.txt", file=outfile, append=T)

