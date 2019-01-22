
# Documenting the formulas to use in recreation of summary sheet and crossing scoring

cells <- xlsx_cells(LIculvertsAssessments$filePath[12]) %>% 
    filter(sheet != "Data Sheet - BLANK", !is.na(formula))




# Finding constants within formulas:
# directly from : https://nacnudus.github.io/tidyxl/articles/smells.html
tokens <- cells %>%
  filter(!is.na(formula)) %>% # filter out rows that don't contain a formula
  select(sheet, address, row, col, formula) %>%
  mutate(tok = map(formula, xlex))  # using map make a colum containing a dataframe with the information from xlex()

# Count number of cells (not unique) being referenced in a token
reffinder <- function(tokencol){
refNu <- tokencol %>% as.data.frame() %>%
group_by(type) %>% tally() %>%
filter(type == "ref") %>% pluck(2)
return(refNu)
}
reffinder()

# select(-formula)
references <- tokens %>%
  mutate(cellrefs = map_int(.x = tok, .f = reffinder)) %>%  # Create cellrefs to hold the number of cells a formula references.
  arrange(desc(cellrefs))

references %>% head(10)


# Playing around with some complex viz.
library(networkD3)
# 
# references %>% 
#   # unnest() %>% 
#   # filter(address == "N33") %>% 
#   filter(cellrefs > 1) %>% 
#   mutate(sourceCell = paste(sheet, address, sep = "_")) ->a
#   mutate(refCell = map_chr() %>% head()
#   # spread(type, value = token) %>%
#   select(formula, sourceCell, ref, sheet) %>%
#   mutate(destCell = paste(sheet, ref, sep = "_")) %>%
#   select(formula, sourceCell, destCell) %>%
#   simpleNetwork()
# 

