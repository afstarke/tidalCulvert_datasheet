# Desktop Database Creation
# Join the attributes from the calculated fields that Karen is working on for the portion of the desktop that includes
# the 'PERC' forest, impervious, etc.
# Join in Karen's calculated fields on crossingID

# MARSH_AC == da_saltMarshArea
# BASIN_SQMI == da_WatershedArea_upStream
# IS_PERC_MEAN == da_WatershedLandCover_impervious
# FOR_PERC == da_WatershedLndCover_forested
# WET_PERC == da_WatershedLndCover_wetland
# DEV_PERC == da_WatershedLndCover_developed
# WATER_PERC == new data field that is equal to the % surface water
# MMP_AC_TOTAL == da_MarshMigrPot_acres
# MMP_AC_EU == da_MarshMigrPot_evalUnit

# LIculvertData_location %>% left_join(by = 'crossingID') %>% 
#   select(-ALL BUT THE FIELDS THAT WE'RE REPLACING) %>% 
#   rename(FIELDS FROM BELOW, line 14) %>% then do the pre_fix?

# TODO: ensure that the desktop data that is included in this output is NOT included in some later outputs of the Field assessment data
# which may cause confusion or issues with replicated field names.


# Rename list.
# saltMarshArea = MARSH_AC,
# WatershedArea_upStream = BASIN_SQMI,
# WatershedLandCover_impervious = IS_PERC_MEAN,
# WatershedLndCover_forested = FOR_PERC,
# WatershedLndCover_wetland = WET_PERC,
# WatershedLndCover_developed = DEV_PERC,
# new data field that is equal to the % surface water = WATER_PERC, 
# MarshMigrPot_acres = MMP_AC_TOTAL,
# MarshMigrPot_evalUnit = MMP_AC_EU

# Subset the attributes/columns needed for desktop assessments.
# Retain the attributes that Karen calculated by catchements (from list above); DROP those duplicated but incomplete attributes from workbooks.
desktopData <- LIculvertData_location %>% 
  select(crossingID, Ownership, Road, Rd_Owner, Notes, PtrPriorit, TNCPriorit, MarPriorit, CtchPriori, Name, 
         MarshCompl:Notes, tidyselect::vars_select(names(.), one_of(desktopVars)), BASIN_SQMI:MMP_AC_EU,
         -WatershedArea_upStream, -saltMarshArea, 
         -WatershedLndCover_forested, -WatershedLndCover_impervious,
         -WatershedLndCover_wetland, -WatershedLndCover_developed,
         -MarshMigrPot_acres, -MarshMigrPot_evalUnit) %>% 
  rename(da_saltMarshArea = MARSH_AC,
         da_WatershedArea_upStream = BASIN_SQMI,
         da_WatershedLandCover_imperv = IS_PERC_MEAN,
         da_WatershedLndCover_forest = FOR_PERC,
         da_WatershedLndCover_wetland = WET_PERC,
         da_WatershedLndCover_devlpd = DEV_PERC,
         da_WatershedLndCover_water = WATER_PERC,
         da_MarshMigrPot_acres = MMP_AC,
         da_MarshMigrPot_evalUnit = MMP_AC_EU,
         da_WatershedArea_other = OTHER_PERC)


desktopData %>% st_drop_geometry %>% str()

# Logical test for consolidating multiple Y/N columns to single Y/N/NA
# BUG: st_write drops Bolean columns: https://github.com/r-spatial/sf/issues/592
# FIX: Convert to numeric (T = 1, F = 0, NA = NA), will need to limit options for down stream data entry/editing

desktopData2 <- desktopData %>% 
  mutate(da_crossOutlet_AtlDraining = as.numeric(case_when( # create new variables with case when.
  crossingOutlet_Atlantic_Y == TRUE & crossingOutlet_Atlantic_N == FALSE ~ TRUE,
  crossingOutlet_Atlantic_N == TRUE & crossingOutlet_Atlantic_Y == FALSE ~ FALSE,
  is.na(crossingOutlet_Atlantic_N) & is.na(crossingOutlet_Atlantic_Y) ~ NA,
  crossingOutlet_Atlantic_Y == TRUE & crossingOutlet_Atlantic_N == TRUE ~ NA) # Subtidal outlet
  )) %>% mutate(da_crossOutlet_Subtidal = as.numeric(case_when( # create new variables with case when.
    crossingOutlet_Subtidal_Y == TRUE & crossingOutlet_Subtidal_N == FALSE ~ TRUE,
    crossingOutlet_Subtidal_N == TRUE & crossingOutlet_Subtidal_Y == FALSE ~ FALSE,
    is.na(crossingOutlet_Subtidal_N) & is.na(crossingOutlet_Subtidal_Y) ~ NA,
    crossingOutlet_Subtidal_Y == TRUE & crossingOutlet_Subtidal_N == TRUE ~ NA)
  )) %>% 
  # InundationRisk roadway 
  mutate(da_InundatRisk_roadway = as.numeric(case_when( # create new variables with case when.
    InundationRisk_roadway_Y == TRUE & InundationRisk_roadway_N == FALSE ~ TRUE,
    InundationRisk_roadway_N == TRUE & InundationRisk_roadway_Y == FALSE ~ FALSE,
    is.na(InundationRisk_roadway_N) & is.na(InundationRisk_roadway_Y) ~ NA,
    InundationRisk_roadway_Y == TRUE & InundationRisk_roadway_N == TRUE ~ NA)
  )) %>% 
  # InundationRisk roadway 1%
  mutate(da_InundatRisk_roadway_1 = as.numeric(case_when( # create new variables with case when.
    InundationRisk_roadway_Y_1p == TRUE & InundationRisk_roadway_N_1p == FALSE ~ TRUE,
    InundationRisk_roadway_N_1p == TRUE & InundationRisk_roadway_Y_1p == FALSE ~ FALSE,
    is.na(InundationRisk_roadway_N_1p) & is.na(InundationRisk_roadway_Y_1p) ~ NA,
    InundationRisk_roadway_Y_1p == TRUE & InundationRisk_roadway_N_1p == TRUE ~ NA)
  )) %>% 
  # # InundationRisk development
  # mutate(InundationRisk_development = as.numeric(case_when( # create new variables with case when.
  #   InundationRisk_development_Y == TRUE & InundationRisk_development_N == FALSE ~ TRUE,
  #   InundationRisk_development_N == TRUE & InundationRisk_development_Y == FALSE ~ FALSE,
  #   is.na(InundationRisk_development_N) & is.na(InundationRisk_development_Y) ~ NA,
  #   InundationRisk_development_Y == TRUE & InundationRisk_development_N == TRUE ~ NA)
  # )) %>% 
  # # InundationRisk development 1%
  # mutate(InundationRisk_development_1percent = as.numeric(case_when( # create new variables with case when.
  #   InundationRisk_development_Y_1percent == TRUE & InundationRisk_development_N_1percent == FALSE ~ TRUE,
  #   InundationRisk_development_N_1percent == TRUE & InundationRisk_development_Y_1percent == FALSE ~ FALSE,
  #   is.na(InundationRisk_development_N_1percent) & is.na(InundationRisk_development_Y_1percent) ~ NA,
  #   InundationRisk_development_Y_1percent == TRUE & InundationRisk_development_N_1percent == TRUE ~ NA)
  # )) %>% 
  select(-ends_with("_Y"), -ends_with("_N"), 
         -InundationRisk_roadway_Y_1p, 
         -InundationRisk_roadway_N_1p) %>% 
  rename_at(tidyselect::vars_select(names(.), one_of(desktopVars[c(1:3,5:31)])), function(x) paste0("da_",x)) %>% # prefix 'da_' to desktop assessment variables.
  rename(da_InundRisk_rdwy_Comts = da_InundationRisk_roadway_Comts,
         da_NumbTidalCross_dwnStream = da_NumbTidalCrossings_dwnStream)

desktopData2 %>% st_drop_geometry() %>% writexl::write_xlsx(path = "Z:/desktopData_09Oct2019.xlsx")

desktopData2 %>%
  st_write(dsn = "M:/Projects/LI/Culvert_Assessment/data/Tidal_Desktop_Assessment/TidalCrossings_DesktopData.gpkg",
         layer = "tidalCrossings_DESKTOP", update  = TRUE, delete_dsn=TRUE, layer_options = "OVERWRITE=YES")


desktopData2 %>% 
  st_write(dsn = "M:/Projects/LI/Culvert_Assessment/data/Tidal_Desktop_Assessment/TidalCrossings_DesktopData.geojson")

# From this convert geopkg to gdb in Arcmap and publish to either  NYSPATIAL or ArcOnline (AGOL). 
# TODO: Write new fucntion to pull data from AGOL and integrate to field data.


