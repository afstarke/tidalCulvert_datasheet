# Create a match table for linking photos to points.


matchTable_tidal <- list.files(full.names = T, path = "C:/Users/astarke/Box Sync/Culvert Assessment/Tidal Assessments/Photos", 
           all.files = TRUE, include.dirs = F, recursive = T) %>% 
  as.tibble() %>% 
  mutate(tidalID = sub(x = sub(x = value, pattern = ".*#", replacement = ""), pattern = "/.*", replacement = ""))

# Calculate the number of photos for each crossing ID 
pics <- matchTable_tidal %>% mutate(crossingID = as.numeric(tidalID)) %>% group_by(crossingID) %>% tally(name = "No.pics") %>% drop_na()

matchTable_tidal %>% write_xlsx(path = 'photomatchTable_tidal.xlsx')
# matchTable_tidal %>% datatable()
