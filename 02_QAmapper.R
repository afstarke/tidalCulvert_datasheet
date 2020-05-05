# I'm a map, I'm a map, I'm a map

LIculvertData_location %>% 
  select(crossingID, PtrPriorit, TNCPriorit, MarPriorit) %>% 
  mutate(priority = case_when(
    PtrPriorit == 1 & TNCPriorit == 1 & MarPriorit == 1 ~ "All",
    PtrPriorit == 0 & TNCPriorit == 1 & MarPriorit == 1 ~ "TNC-Marsh",
    PtrPriorit == 0 & TNCPriorit == 0 & MarPriorit == 1 ~ "Marsh only",
    PtrPriorit == 0 & TNCPriorit == 1 & MarPriorit == 0 ~ "TNC only",
    PtrPriorit == 1 & TNCPriorit == 0 & MarPriorit == 0 ~ "Partner only",
    PtrPriorit == 0 & TNCPriorit == 0 & MarPriorit == 0 ~ "NOT priority",
    PtrPriorit == 1 & TNCPriorit == 1 & MarPriorit == 0 ~ "Partner-TNC",
    PtrPriorit == 1 & TNCPriorit == 0 & MarPriorit == 1 ~ "Partner-Marsh",
    TRUE ~ "NOT Priority")) %>% # make NA's not a priority 
  mapview(zcol = "priority", burst = TRUE)


# Freshwater
# multiple surveys

fw_data %>% group_by(CrosCode) %>% tally() %>% filter(n > 1) %>% pull(CrosCode) -> duplicateVisits

fw_data %>% filter(CrosCode %in% duplicateVisits) %>% group_by(CrosCode) %>% 
  arrange(SurveyID) %>% 
  mutate(visitOrder = min_rank(SurveyID)) %>% 
  select(visitOrder, everything()) %>% 
  arrange(CrosCode, visitOrder) %>% writexl::write_xlsx(path = "outputs/allDups.xlsx")

gather(st_drop_geometry(fw_doc_data), -SurveyID, key = "FieldMeasure", value = "k") %>% 
  spread(key = SurveyID, val = k) %>% mutate(diff = .[2] == .[3]) %>% datatable(rownames = F)



fw_data %>% group_by(CrosType, AopNaacc) %>% tally() %>% mapview(zcol= "AopNaacc")
tmap_mode("view")
fw_data %>% group_by(CrosType, AopNaacc) %>% tally() %>% 
    tm_shape() + 
  tm_dots(col = "CrosType", size = 0.1) + 
  tm_basemap(leaflet::providers$CartoDB.PositronNoLabels, group = "CartoDB basemap") + 
  tm_facets(by = "AopNaacc", sync = TRUE, ncol = 2)
  

# # Combinging Fresh and Tidal
# fw_data %>%
#   bind_rows(LIculvertAssessmentData) %>% # Not working- need to explore more.
#   tm_shape() + tm_dots(col = "CrosType", size = 0.1) +
#   tm_basemap(leaflet::providers$CartoDB.PositronNoLabels, group = "CartoDB basemap")
source("code/rmdSetup.R")
source("summarySheetSetup.R")
# Map of intersecting points- reciprocal intersections.
fw_subset <- fw_data[LIculvertAssessmentData, op = st_is_within_distance, dist = 50]
tidal_subset <- LIculvertAssessmentData[fw_data, op = st_is_within_distance, dist = 50]
library(mapedit)

{fw_subset}%>% 
  mapview(col.regions = 'green', layer.name = "FW sites within 50m of Tidal") + 
  {tidal_subset %>% 
      mapview(col.regions = 'blue', layer.name = "Tidal sites within 50m of FW")} -> map
map
fw <- fw_subset %>% select(CrosCode) %>% mutate(protocol = 1) %>% rename(geometry = Shape)

ti <- tidal_subset %>% mutate(CrosCode = as.character(crossingID)) %>% select(CrosCode) %>%  mutate(protocol = 2)

fw_ti <- rbind(ti, fw) 
fw_ti
fw_ti_data <- fw_ti %>% left_join(fw_data %>% st_drop_geometry()) %>% left_join(LIculvertDataStatus %>% mutate(CrosCode = as.character(crossingID)))
fw_ti_data %>% st_write("data/fw_ti.geojson")

#' QA'ed points on Apr 14th with sub-team to choose which protocol each of these
#' duplicated points should fall under. These points were selected by
#' intersecting the points (within 50m of one another) across the 2 data sources
#' and combining them into a single feature class. These points were then
#' reviewed by the team and the preferred protocol for each crossing was
#' choosen. The point associated with that protocol (tidal or fresh) was
#' retained regardless of the data that existed in it's paired point. That
#' paired point was removed from the set to indicate it's exclusion from that
#' protocol. 
#' To fold this (re)classification of the protocol that was selected into our work flow
#' we need to find the crossings that were REMOVED from that dataset and then
#' FILTER THEM OUT of the master datasets. While working through this with the team
#' we encountered a few issues we need to contend with. Some crossings were
#' assessed under the FW protocol but are clearly tidal. Will this discrepency
#' be an issue? We will attempt to migrate the data from the FW to the tidal and
#' see how scoring differs. There are also crossings that were assessed fully
#' under both protocols. These may be useful in scaling the tidal scoring to
#' that of the freshwater scoring. 
#' Hurdles to overcome: - Tidal sites that were assessed as freshwater (is this
#' of concern?) migrate data between the two. - Multiple assessments between and
#' within protocols (Westbrook needs updated data pulled) - Crossings that exist
#' along impounded ponds in areas that would under natural conditions be tidal,
#' but are not. Is the spillway from a fresh impoundment considered a fw
#' crossing or tidal? Should the feasibility of conversion to tidal be
#' considered?
#' 

# read in edited points- 
# 1 = freshwater protocol
# 2 = tidal protocol
# culledPts are the points (protocols really) that we want to retain.
culledPts <- st_read("data/fw_tidal_selected_points.geojson")
fw_ti_data <- st_read("data/fw_ti.geojson")
mapview(culledPts)
fw_keeps <- culledPts %>% filter(protocol == 1) %>% pull(CrosCode) %>% as.character() #points we selected as preferred fw protocl n=13
fw_drops <- fw_ti_data %>% filter(!CrosCode %in% fw_keeps) %>% filter(protocol == 1) # filter out the ones we want from the intersected data to make a list of points to drop
ti_keeps <- culledPts %>% filter(protocol == 2) %>% pull(CrosCode) %>% as.character() # points we selected as preferred tidal protocol n=57
ti_drops <- fw_ti_data %>% filter(!CrosCode %in% ti_keeps) %>% filter(protocol == 2)
mapview(fw_drops, col.regions = "red") +
  mapview(ti_drops, col.regions = "green") +
  mapview(LIculvertData_location, col.regions = "yellow") +
  mapview(fw_data, col.regions = "pink")

fw_data %>% mutate(sumsheet = paste0("D:/culvert_project/html_outputs/", CrosCode, ".html")) %>% mapview()


## Desktop-data from AGOL comparison to catchment data
a <- catchment_data %>% 
  select(Tidal_ID, WET_PERC, MMP_AC_WHOLE, IS_PERC_MEAN, MARSH_AC) %>% rename(crossingID = Tidal_ID) %>% 
  st_drop_geometry()
b <- LIculvertData_location %>% 
  select(crossingID, da_WatershedLandCover_imperv, da_MarshMigrPot_acres, da_saltMarshArea) %>% 
  st_drop_geometry() 

c <- a %>% left_join(b)
