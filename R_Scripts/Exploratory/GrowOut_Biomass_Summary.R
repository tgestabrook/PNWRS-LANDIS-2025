

ageBiomassStacks <- dir(file.path(landisOutputDir, "ageBiomassOutput"))
allSpp <- str_extract(ageBiomassStacks, "^(.+)-(.+)-yr.tif", group = 1) |> unique() 
treeSpp <- allSpp[!allSpp%in%c("Nfixer_Resprt","NonFxr_Resprt","NonFxr_Seed","Grass_Forb","TotalBiomass")]
treeStacks <- ageBiomassStacks[str_detect(ageBiomassStacks, paste(treeSpp, collapse = "|"))]

mtbs.r <- rast(file.path(
  "F:\\LANDIS_Input_Data_Prep\\BigData\\MTBS_2021_2025_CONUS", c(paste0("mtbs_CONUS_", 2021:2025, ".tif"))
)) |>
  project(pwg.r, method = "near") |>
  crop(pwg.r)

mtbs.r <- ifel(is.na(mtbs.r), 0, mtbs.r)

plot(mtbs.r)
# plot(pwg.r, add = T)

mgmt.r <- rast(file.path(modelDir, LANDIS.EXTENT, paste0("ext_BiomassHarvestMgmt_", LANDIS.EXTENT, ".tif")))
mgt_forest.r <- ifel(mgmt.r <=3, 1, 0) 


mtbs_mgmt.r <- ifel(mgt_forest.r == 1, mtbs.r, 0)
plot(mtbs_mgmt.r)

tree_biomass.r <- rast(file.path(landisOutputDir, "ageBiomassOutput", treeStacks))
tree_biomass_mgmt.r <- ifel(mgt_forest.r == 1, tree_biomass.r, NA)
names(tree_biomass_mgmt.r) <- names(tree_biomass.r)






roads.no.wild.r<-rast(file.path(dataDir,'PWG',paste0("roads_noWild_",LANDIS.EXTENT,"_45m.tif"))) |>
  buffer(183)  # 800 feet yarding distance
roads_mgmt.r <- ifel(mgt_forest.r == 1 & roads.no.wild.r, 1, 0)
mgmt_access.r <- roads_mgmt.r + mgt_forest.r

zones.r <- ifel(mgmt_access.r == 2, mtbs_mgmt.r + 1, (mtbs_mgmt.r * -1) - 1)  # if the area is managed but unreachable by road, invert the severity scale
zones.r <- ifel(mgmt_access.r == 0, NA, zones.r)


# post_fire_biomass.df <- zonal(tree_biomass_mgmt.r, mtbs_mgmt.r, fun = "sum")
post_fire_biomass.df <- data.frame()

for (year in 1:5){
  print(year)
  zone.r <- zones.r[[year]]
  names(zone.r) <- "Severity"
  
  fire_summary <- freq(zone.r) |>
    mutate(Severity = value, area_ha = count * 0.81) |>
    select(Severity, area_ha)
  
  tree_biomass_yr.r <- tree_biomass_mgmt.r |>
    select(contains(paste0("-", year))) |>
    select(contains(c("AbieAmab", "AbieGran", "AbieLasi", "AbieProc", "ChamNoot", "FraxLati", "LariLyal", "LariOcci", "PiceEnge", "PinuAlbi", "PinuCont", "PinuMont", "PinuPond", "PseuMenz", "ThujPlic", "TsugHete", "TsugMert"))) |>
    select(!contains(paste0("-Age", seq(200, 500, 50), "-")))
  
  df <- zonal(tree_biomass_yr.r, zone.r, fun = "sum") |> 
    pivot_longer(ends_with(paste0("-", year)), names_sep = "-", names_to = c("Species", "AgeCohort", "Year"), values_to = "Biomass_gm2") |>
    mutate(In_road_range = ifelse(Severity > 0, T, F)) |>
    mutate(Severity = abs(Severity) - 1) |>  # we don't need the silly negative severity now that we can assign in/out of road range
    mutate(Biomass_MG = Biomass_gm2 * 0.01 * 0.81) |>
    mutate(Pre_commercial = case_when(
      AgeCohort == "age0" ~ T,
      AgeCohort == "age10" ~ T,
      AgeCohort == "age20" ~ T,
      AgeCohort == "age30" ~ T,
      .default =  F
    )) |>
    mutate(
      Merch_biomass = case_when(
        Pre_commercial ~ 0,
        .default = Biomass_MG * 0.55
      )
    ) |>
    mutate(Nonmerch_biomass = Biomass_MG - Merch_biomass) |>
    left_join(fire_summary)
  
  post_fire_biomass.df <- bind_rows(post_fire_biomass.df, df)
}

write_csv(post_fire_biomass.df, file.path(landisOutputDir, "Post-fire-biomass-summary.csv"))

# harvestable_biomass.df <- post_fire_biomass.df |>
#   


post_fire_biomass.df <- read.csv(file.path(landisOutputDir, "Post-fire-biomass-summary.csv")) |>
  mutate(Severity_class = case_when(
    Severity %in% c(0, 5, 6) ~ "Unburned",
    Severity %in% c(1, 2) ~ "Low",
    Severity == 3 ~ "Moderate",
    Severity == 4 ~ "High"
  )) |>
  mutate(Severity_class = factor(Severity_class, levels = c("Unburned", "Low", "Moderate", "High")[4:1])) |>
  mutate(Road = case_when(
    In_road_range ~ "In range of road",
    .default = "Not in range of road"
  )) |>
  pivot_longer(cols = c(Merch_biomass, Nonmerch_biomass), names_to = "Biomass_type", values_to = "Harvested_biomass_MG") |>
  group_by(Year, Severity_class, Biomass_type, Road) |>
  summarise(Harvested_biomass_MG = sum(Harvested_biomass_MG)) |>
  mutate(Biomass_type = Biomass_type |> recode_values(
    "Merch_biomass" ~ "Merchantable biomass",
    "Nonmerch_biomass" ~ "Non-merchantable or chipped biomass"
  )) |> ungroup() |>
  complete(Year, Severity_class, Biomass_type, Road, fill = list(Harvested_biomass_MG = 0))



ggplot(data = post_fire_biomass.df |> filter(Severity_class != "Unburned"), aes(x = Year + 2020, y = Harvested_biomass_MG/1000, fill = Severity_class)) + geom_col() + facet_grid(Biomass_type~Road) + scale_fill_manual(values = c('darkgreen','darkseagreen','goldenrod1','firebrick4')[4:1]) +
  xlab("Year") + ylab("Biomass of harvestable tree cohorts (1000s MG)") + theme_grey()


### Map fire against biomass against road area

mtbs_cats.r <- mtbs_mgmt.r |>
  classify(rcl = data.frame(
    "From" = c(0, 1, 2, 3, 4, 5, 6),
    "To" = c(0, 1, 1, 2, 3, 0, 0) 
  )) |> as.factor()
levels(mtbs_cats.r) <- data.frame(
  id = 0:3,
  Severity = c("Unburned", "Low", "Moderate", "High")
)

plot(mtbs_cats.r[[1]], col =  c('darkgreen','darkseagreen','goldenrod1','firebrick4'), main = paste0(LANDIS.EXTENT, " Burn Severity in Managed Forest 2001"))



r.r <- rast(file.path(dataDir,'PWG',paste0("roads_noWild_",LANDIS.EXTENT,"_45m.tif")))

plot(r.r, legend = F, col = "black", add = T)  

m.r <- ifel(mgt_forest.r == 1 & (pwg.r >= 20), NA, 1)
plot(m.r, col = "grey", legend = F, add = T)




# ggplot() + geom_spatraster(data = as.factor(mtbs_cats.r[[1]])) + scale_fill_manual(values = c('darkgreen','darkseagreen','goldenrod1','firebrick4'))

post_fire_biomass.df <- read.csv(file.path(landisOutputDir, "Post-fire-biomass-summary.csv")) |>
  mutate(Severity_class = case_when(
    Severity %in% c(0, 5, 6) ~ "Unburned",
    Severity %in% c(1, 2) ~ "Low",
    Severity == 3 ~ "Moderate",
    Severity == 4 ~ "High"
  )) |>
  mutate(Severity_class = factor(Severity_class, levels = c("Unburned", "Low", "Moderate", "High")[4:1])) |>
  mutate(Road = case_when(
    In_road_range ~ "In range of road",
    .default = "Not in range of road"
  )) |>
  pivot_longer(cols = c(Merch_biomass, Nonmerch_biomass), names_to = "Biomass_type", values_to = "Harvested_biomass_MG") |>
  group_by(Year, AgeCohort, Severity_class, Biomass_type, Road) |>
  summarise(Harvested_biomass_MG = sum(Harvested_biomass_MG)) |>
  mutate(Biomass_type = Biomass_type |> recode_values(
    "Merch_biomass" ~ "Merchantable biomass",
    "Nonmerch_biomass" ~ "Non-merchantable or chipped biomass"
  )) |> ungroup() |>
  complete(Year, Severity_class, Biomass_type, Road, fill = list(Harvested_biomass_MG = 0)) |>
  mutate(Age = as.numeric(str_extract(AgeCohort, "\\d+")))

ggplot(data = post_fire_biomass.df, aes(x = Age, y = Harvested_biomass_MG/1000, fill = Severity_class)) + geom_col(position = "dodge") + 
  facet_grid(Biomass_type~Road) + scale_fill_manual(values = c('darkgreen','darkseagreen','goldenrod1','firebrick4')[4:1]) +
  xlab("Age cohort") + ylab("Biomass of harvestable tree cohorts (1000s MG)") + theme_grey()





plot(roads.no.wild.r)
plot(m.r, col = "grey", legend = F, add = T)
plot(hillshade.r, col = colorRampPalette(c("white", "black"))(50), alpha = 0.25, add = T, legend = F)







