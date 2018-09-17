# Culvert tidy
# Read in tidy style.
# using functional programming

# Read in files from specified folder
# Create tibble with columns of spreadsheet name, size, last modified, and list column with tidyxl 'cells' 
# Then use map and mutate functions to extract out the bits that are needed. 
# Can be done additively and transparently as more variables are desired.



culvert_fetch <- function(filepath){
  cells <- xlsx_cells(filepath) %>% 
  filter(sheet != "Data Sheet - BLANK") %>% 
    select(sheet, address, data_type:character) %>% 
    gather(error:character, key = "dataType", value = "value") %>% 
    filter(!is.na(value)) %>% 
    mutate(same = ifelse(data_type == dataType, 1, 0)) 
  return(cells)
}

culvert_tidy <- function(folder){
  
  culvertFolder <- file.path(folder)
  
  culvertFiles <- list.files(culvertFolder) %>% as.tibble()
  culvertFiles <- culvertFiles %>% rename(filenames = value)
  culvertFiles <- culvertFiles %>% mutate(lastChanges = as.POSIXct(map_dbl(.x = filenames, .f = ~file.info(paste(folder, .x, sep = "/"))$mtime), origin = "1970-01-01"),
                                          filePath = paste(folder, filenames, sep = "/"))
  culvertFiles <- culvertFiles %>% mutate(tidycells = map(.x = filePath, .f = culvert_fetch)) 
  return(culvertFiles)
  
}

test1 <- culvert_tidy("C:/Users/astarke/Desktop/Culvert Excel Sheets")

# Build off of test1
test2 <- test1 %>% mutate(crossingID = map_chr(tidycells, .f = ~culvert_extract(.x, "Data Sheet - SITE", "L7")),
                          observers = map_chr(tidycells, .f = ~culvert_extract(.x, "Data Sheet - SITE", "G9")),
                          # dateAssessed = as.Date(map_dbl(tidycells, .f = ~culvert_extract(.x, "Data Sheet - SITE", "Z9"))),
                          upstreamChannelwidth = map_chr(tidycells, .f = ~culvert_extract(.x, "Data Sheet - SITE", "F243")),
                          dwnstreamChannelwidth = map_chr(tidycells, .f = ~culvert_extract(.x, "Data Sheet - SITE", "K243")),
                          upstreammaxPoolwidth = map_chr(tidycells, .f = ~culvert_extract(.x, "Data Sheet - SITE", "F245")),
                          dwnstreammaxPoolwidth = map_chr(tidycells, .f = ~culvert_extract(.x, "Data Sheet - SITE", "K245")),
                          LiDarHt_CL = map_chr(tidycells, .f = ~culvert_extract(.x, "Data Sheet - SUMMARY", "J54")),
                          streamName = map_chr(tidycells, .f = ~culvert_extract(.x, "Data Sheet - SITE", "G12"))) %>% 
  
  select(-tidycells, -filePath)

