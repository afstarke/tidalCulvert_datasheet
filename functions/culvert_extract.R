

# Helper function to look up values in a cell of interest and map them to a column in a tidy culvert sheet.
# 
# TODO : Recreate this using the tidied cells column 

# culvert_extract function
# Pulls value from specified sheet and cell to return value
# Returns NA if NULL value returned by query of excel sheet (as happens if cell is blank and thus filtered early in the process)


culvert_extract <- function(tidycells, sheetOI, celladdress){
  
  val <- tidycells %>% 
    filter(sheet == sheetOI) %>% 
    filter(address == celladdress) %>% 
    pull(value)
  
  if(length(val) == 0){
    return(NA)
  } else{
    return(val)
  }
  
  
}
#   

culvert_extract(cells, "Data Sheet - SITE", "Z9") %>% as.numeric() %>% as.Date(origin = "1899-12-30")
culvert_extract(cells, "Data Sheet - SITE", "Z9") %>% as.numeric()
