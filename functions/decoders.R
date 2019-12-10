# Formula Decoding
cells <- LIculvertsAssessments$tidycells[[10]]

# Can use following fucntions to help extract forumals.
get.cell.value <- function(tidycells, sheetOI, cellOfInterest){
  
  val <- tidycells %>% 
    filter(sheet == sheetOI) %>% 
    filter(address == cellOfInterest) %>% 
    pull(formula)
  return(val)
  
}
#   
get.cell.value(LIculvertsAssessments$tidycells[[12]], sheetOI = 'Data Sheet - SITE',  "AA65")


get.cell.formula.text <- function(tidysheet, cellOfInterest){
  val <- tidysheet %>% 
    filter(address == cellOfInterest) %>% 
    select(formula) %>% as.character() #%>% 
   # xlex() 
  return(val)
}
#   
cells <- LIculvertsAssessments$tidycells[[1]]
get.cell.formula.text(cells, "N12")

get.cell.formula.parsed <- function(tidysheet, cellOfInterest){
  val <- tidysheet %>% 
    filter(address == cellOfInterest) %>% 
    select(formula) %>% as.character() %>% 
    xlex()
  return(val)
}
#   
get.cell.formula.parsed(cells, "N12")

# TO DO LIST:
# Tidal range ratio - N14
# Crossing Condition Eval - N12 (summary sheet)

calculationSheet <- cells %>% filter(sheet == "Calculations")
summarySheet <- cells %>% filter(sheet == "Data Sheet - SUMMARY")
siteSheet <- cells %>% filter(sheet == "Data Sheet - SITE")

get.cell.value(calculationSheet, "N14")
get.cell.formula(summarySheet, "N14") %>% as.data.frame()


# Crossing Condition Eval - N12 (summary sheet)
get.cell.value(summarySheet, "N12") # leads to 
  get.cell.value(calculationSheet, "D3") # leads to 
  get.cell.value(siteSheet, "F86") # leads to 
