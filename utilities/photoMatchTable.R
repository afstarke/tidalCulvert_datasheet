# Create a match table for linking photos to points.


matchTable_tidal <- tibble(
  paths = list.files(full.names = F, path = here::here("summarySheets/tidal_photos/"), 
                     all.files = TRUE, include.dirs = F, recursive = T),
  ImageName = list.files(full.names = F, path = here::here("summarySheets/tidal_photos/"), 
                         all.files = TRUE, include.dirs = F, recursive = T)) %>% 
  mutate(ID = as.numeric(sub(x = sub(x = paths, pattern = ".*#", replacement = ""), pattern = "/.*", replacement = "")),
         ImageName = sub(x = ImageName, pattern = "[[:alnum:]]/", replacement = ""))

# Calculate the number of photos for each crossing ID 
pics <- matchTable_tidal %>% mutate(crossingID = as.numeric(tidalID)) %>% group_by(crossingID) %>% tally(name = "No.pics") %>% drop_na()

matchTable_tidal %>% write_xlsx(path = here::here("summarySheets",  'photomatchTable_tidal.xlsx'))
# matchTable_tidal %>% datatable()
