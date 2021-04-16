####
# Create a match table for linking photos to points.
# TIDAL CROSSINGS
#' Reads in photos from provided folder, parses out names of crossings from sub-folders
#' and creates a new table with photo path, photo name (typically upstream/downstream etc) 
#' and crossing ID that will be used in any joining operations downstream
#' 

#'
#' The original photo directory used subfolders with the naming scheme of Crossings #xxx which was throwing errors (the # is NG.)
#' The solution is to migrate the full directory into the folder /summarySheets/tidal_photos/ and then replace the # with an _
#' to rename directories use bash terminal -->  for i in *#*; do mv -- "$i" "${i//#/_}"; done


matchTable_tidal <- tibble(
  paths = list.files(full.names = F, path = here::here("summarySheets/tidal_photos/"), 
                     all.files = TRUE, include.dirs = F, recursive = T),
  ImageName = list.files(full.names = F, path = here::here("summarySheets/tidal_photos/"), 
                         all.files = TRUE, include.dirs = F, recursive = T)) %>% 
  dplyr::mutate(ID = as.numeric(sub(x = sub(x = paths, pattern = ".*_", replacement = ""), pattern = "/.*", replacement = "")),
         ImageName = sub(x = ImageName, pattern = ".*/", replacement = "")) # odd bug that popped up. 19Nov2020
  # mutate(ImageName = sub(x = ImageName, pattern = ".*/", replacement = ""))

# Calculate the number of photos for each crossing ID 
pics <- matchTable_tidal %>% mutate(crossingID = as.numeric(ID)) %>% group_by(crossingID) %>% tally(name = "No.pics") %>% drop_na()
# Write out the table generated above.
matchTable_tidal %>% write_xlsx(path = here::here("summarySheets",  'photomatchTable_tidal.xlsx'))
# matchTable_tidal %>% datatable()


## ArcPro friendly 
# 
# matchTable_tidal_pro <- tibble(
#   paths = list.files(full.names = F, path = here::here("summarySheets/tidal_photos/"), 
#                      all.files = TRUE, include.dirs = F, recursive = T),
#   ImageName = list.files(full.names = F, path = here::here("summarySheets/tidal_photos/"), 
#                          all.files = TRUE, include.dirs = F, recursive = T)) %>% 
#   dplyr::mutate(ID = as.numeric(sub(x = sub(x = paths, pattern = ".*_", replacement = ""), pattern = "/.*", replacement = "")),
#                 ImageName = sub(x = ImageName, pattern = ".*/", replacement = ""),
#                 paths = (str_replace(paths, pattern = "*/*", replacement = "\\"))) # odd bug that popped up. 19Nov2020
# # mutate(ImageName = sub(x = ImageName, pattern = ".*/", replacement = ""))
# 
# # Calculate the number of photos for each crossing ID 
# pics <- matchTable_tidal_pro %>% mutate(crossingID = as.numeric(ID)) %>% group_by(crossingID) %>% tally(name = "No.pics") %>% drop_na()
# # Write out the table generated above.
# matchTable_tidal_pro %>% write_xlsx(path = here::here("summarySheets",  'photomatchTable_tidal_pro.xlsx'))
# # matchTable_tidal %>% datatable()

# 
# # Freshwater NAACC photo match table maker-
# 
# matchTable_fresh <- tibble(
#   paths = list.files(full.names = F, path = "D:/culvert_project/html_outputs/photos", 
#                      all.files = TRUE, include.dirs = F, recursive = T, pattern = ".jpg", ignore.case = TRUE)) %>% 
#   mutate(ImageName = sub(x = paths, pattern = ".*/", replacement = ""),
#          ID = (sub(x = sub(x = paths, pattern = ".*#", replacement = ""), pattern = "/.*", replacement = ""))) 
# 
# matchTable_fresh %>% write_csv(path = "D:/culvert_project/html_outputs/photos/matchTable.csv")
