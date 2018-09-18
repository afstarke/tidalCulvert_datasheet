

# Helper function to look up values in a cell of interest and map them to a column in a tidy culvert sheet.
# 
# TODO : Recreate this using the tidied cells column 

# culvert_extract function
# Pulls value from specified sheet and cell to return value
# Returns NA if NULL value returned by query of excel sheet (as happens if cell is blank and thus filtered early in the process)

# TODO: Fix date bugs in pull from tidyxl cells


culvert_extract <- function(tidycells, sheetOI, celladdress){
  
  val <- tidycells %>% 
    filter(sheet == sheetOI) %>% 
    filter(address == celladdress) %>% 
    pull(value) # TODO: Fix date bugs in pull from tidyxl cells

  if(length(val) == 0){
    return(NA)
  } else{
    return(val)
  }
  
  
}
#   TODO: Move this into documentation and update documentation.

keysheet <- read_excel("spreadsheets/key.xlsx")

decodeSheet <- function(tidyxlcells){
  df <- keysheet %>% 
    rowwise() %>%
    mutate(values = culvert_extract(tidycells = tidyxlcells, sheetOI = .data$Sheet, celladdress = .data$Cell))
  df
  }



decodedSheet2 <- test1 %>% mutate(decoded = map(.x = tidycells, .f = ~decodeSheet(.x)))

decodedSheet2 %>% select(filenames, decoded) %>% unnest() %>% select(filenames, dataName, values) %>% spread(key = dataName, value = values) -> a

