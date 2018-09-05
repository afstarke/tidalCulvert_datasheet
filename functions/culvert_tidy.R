# Culvert tidy
# Read in tidy style.
# using functional programming

# Read in files from specified folder
# Create tibble with columns of spreadsheet name, size, last modified, list column with tidy 'cells' 
# Then use map and mutate functions to extract out the bits that are needed. 
# Can be done additively and transparently as more variables are desired.



culvert_fetch <- function(filepath){
  cells <- xlsx_cells(filepath) %>% 
  filter(is_blank == FALSE, sheet != "Data Sheet - BLANK") 
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

test1 <- culvert_tidy("C:\\Users\\astarke\\Desktop\\Culvert Excel Sheets\\Culvert Excel Sheets")

