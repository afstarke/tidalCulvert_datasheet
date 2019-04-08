# For trying to prioritize and schedule field visits.

crossingStrategy_24Jan <- LIculvertDataStatus_location %>% 
  select(crossingID, Ownership, MarshCompl, TNCPriorit, MarPriorit, observers) %>% 
  filter(TNCPriorit == 1, is.na(observers)) %>% select(-observers) 

write.xlsx(crossingStrategy_24Jan %>% st_drop_geometry(), file = paste0(tidalCulvert_outputs, "/CulvertData_status_FILTERED.xlsx"), sheetName = "Culvert Status")


mapview(LIculvertDataStatus_location %>% filter(crossingID %in% crossingStrategy_24Jan$crossingID), zcol = 'MarshCompl', layer.name = "Marsh Complexes", cex = "PriorityScore")
        
