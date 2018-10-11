
# Documenting the formulas to use in recreation of summary sheet and crossing scoring

cells <- xlsx_cells(LIculvertsAssessments$filePath[12]) %>% 
    filter(sheet != "Data Sheet - BLANK", !is.na(formula))




# Finding constants within formulas:
# directly from : https://nacnudus.github.io/tidyxl/articles/smells.html
tokens <- cells %>%
  filter(!is.na(formula)) %>% # filter out rows that don't contain a formula
  select(sheet, address, row, col, formula) %>%
  mutate(tok = map(formula, xlex))  # using map make a colum containing a dataframe with the information from xlex()

reffinder <- function(tokencol){
refNu <- tokencol %>% as.data.frame() %>%
group_by(type) %>% tally() %>%
filter(type == "ref") %>% pluck(2)
return(refNu)
}


# select(-formula)
references <- tokens %>%
  mutate(cellrefs = map_int(.x = tok, .f = reffinder)) %>%  # Create cellrefs to hold the number of cells a formula references.
  arrange(desc(cellrefs))

references %>% head(10)

# xlsx::write.xlsx(characters,
#                  file = "spreadsheets/extractedKey.xlsx",
#                  sheetName = "Likely keys")
# xlsx::write.xlsx(numerics,
#                  file = "spreadsheets/extractedKey.xlsx",
#                  sheetName = "Likely values", append = TRUE)
# references %>% select(-tok) %>% xlsx::write.xlsx(
#                  file = "spreadsheets/extractedKey.xlsx",
#                  sheetName = "formulaKey", append = TRUE)
references %>% ggplot(aes(x = cellrefs)) +
  geom_histogram(binwidth = 2) +
  labs(x = "Number of cells \n referenced in a forumla")


culvert_extract <- function(folder) {
  culvertFiles <- file.path(folder)
  return(culvertFiles)
}


# Playing around with some complex viz.
library(networkD3)

references %>% unnest() %>% filter(address == "N33") %>%
  mutate(sourceCell = paste(sheet, address, sep = "_")) %>%
  spread(type, value = token) %>%
  select(formula, sourceCell, ref, sheet) %>%
  mutate(destCell = paste(sheet, ref, sep = "_")) %>%
  select(formula, sourceCell, destCell) %>%
  simpleNetwork()

