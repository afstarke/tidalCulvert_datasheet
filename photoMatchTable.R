# Create a match table for linking photos to points.


matchTable_tidal <- list.files(full.names = T, path = "C:/Users/astarke/Box Sync/Culvert Assessment/Tidal Assessments/Photos", 
           all.files = TRUE, include.dirs = F, recursive = T) %>% 
  as.tibble() %>% 
  mutate(tidalID = sub(x = sub(x = value, pattern = ".*#", replacement = ""), pattern = "/.*", replacement = ""))

matchTable_tidal %>% write_xlsx(path = 'photomatchTable_tidal.xlsx')

