

# Helper function to look up values in a cell of interest and map them to a column in a tidy culvert sheet.
# 

culvert_extract <- function(tidycells, sheet, targetCell, plucker){
  
  val <- tidycells %>% 
    filter(sheet == sheet) %>% 
    filter(address == targetCell) %>% 
    pluck(plucker)
  return(val)
  
}
#   

test2 <- test1 %>% mutate(observers = map_chr(.x = tidycells, .f = ~culvert_extract(.x, 'Data Sheet - SITE', "G9", 'character')))
