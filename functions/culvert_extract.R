

# Helper function to look up values in a cell of interest and map them to a column in a tidy culvert sheet.
# 


# culvert_extract function
# Pulls value from specified sheet and cell to return value
# Returns NA if NULL value returned by query of excel sheet (as happens if cell is blank and thus filtered early in the process)

# ---- culvert_extract
#' Extract a value of interest by providing a tidyxl output, 
#' excel sheet name and cell address.
#' 
#' @param tidycells column containing tidyxl cells
#' @param sheetOI name of sheet within tidal culvert data sheet
#' @param celladdress cell address as per Excel format (A1)
#' @return cell value at the specified location
#' 
#'

culvert_extract <- function(tidycells, sheetOI, celladdress){
  
  val <- tidycells %>% 
    dplyr::filter(sheet == sheetOI) %>% 
    dplyr::filter(address == celladdress) %>% 
    dplyr::pull(value) 

  if(length(val) == 0){
    return(NA)
  } else{
    return(val)
  }
  
  
}

# ---- decodeSheet
#' Tidy up spread sheet contents using tidyxl for later gathering
#' and summarizing across several files.
#' 
#' @param tidyxlcells column containing tidyxl cells
#' @param key datatable that has key-value data pairs
#' 
#' @return cell value at the specified location
#' 
#'

decodeSheet <- function(tidyxlcells, key){
  df <- key %>% 
    rowwise() %>%
    mutate(values = culvert_extract(tidycells = tidyxlcells, sheetOI = .data$Sheet, celladdress = .data$Cell))
  df
  }
