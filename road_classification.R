#' Road data processed through below code and using ArcPro to update fields on AGOL 
#' these data are read in with the desktop/location data.
#'

#' Data for finding road functional classification. Errors on reading in using
#' sf, seems to be an issue with a lock file on the gdb layer. Asides, the join
#' feature was monstorous so went into ArcPro on the server and buffered crossing features (500m),
#' extracted road lines, joined road Functional class to crossings (within 20m) and saved as a new 
#' feature. Read in here..
#' 
#' [4/14 2:56 PM] Stephen Lloyd
#' D:\gisdata\Projects\LI\Culvert_Assessment\data\FreshwaterPrioritization\StreetSegment.gdb

#'
#' May 1- dyuplicate records had made their way into the code runs of prioritizations. 
# st_layers(dsn = "M:/Projects/LI/Culvert_Assessment/CulvertPrioritizations/CulvertPrioritizations.gdb")$name
# st_layers(dsn = "../../../Documents/ArcGIS/Projects/Culverts_LongIsland/Culverts_LongIsland.gdb")$name

gis_roadData <- read_sf(dsn = "M:/Projects/LI/Culvert_Assessment/data/Tidal Crossings/Tidal_Crossing_Desktop.gdb", 
                      layer = "tidalCrossings_roadFuncClass")

roadData <- gis_roadData %>% 
  select(crossingID, Functional_Class, FunctionalClass_supervised) %>% 
  left_join(evacRtes %>% select(crossingID, Rd_EvacRte)) %>% st_drop_geometry()

write_rds(x = roadData, path = "data/roadFunctional_class.rds")

# TODO: Update/add domain values from ArcPro - export failed...




evacRte <- read_sf(dsn = "M:/Projects/LI/Culvert_Assessment/data/FreshwaterPrioritization/RoadwayInventorySystem2017Pub.gdb", layer = "Suffolk_EvacRoutes") %>%
  st_transform(crs = 4326)



# 
mapview(roadData) + mapview(LIculvert_GISpts)
# join the evacRte to the tidal points.
tmp <- st_join(x = st_zm(st_transform(LIculvert_GISpts, crs = 26918),drop = T), 
               y = st_zm(evacRte, drop = T), join = st_is_within_distance, dist = 5) %>% 
  mutate(Rd_EvacRte = if_else(is.na(GIS_ID), true = 0, false = 1)) %>% select(crossingID, Rd_EvacRte) %>%
  st_drop_geometry()
tmp2 <- roadData %>% select(crossingID, Functional_Class) %>% st_drop_geometry()

tmp2 %>% 
  left_join(tmp) %>% write_csv("data/roadAppends.csv", na = "")
