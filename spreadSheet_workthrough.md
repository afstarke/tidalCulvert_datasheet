Tidal Spreadsheet fun
================

read in spreadsheet with tidyxl

``` r
# devtools::install_github("nacnudus/tidyxl")
# devtools::install_github("nacnudus/unpivotr")

library(readr)
library(readxl)
library(tidyxl)
library(unpivotr)
library(tidyverse)
```

``` r
# TODO Start with basic read in of spreadsheet and extracting the proper cells.
sheet <- "spreadsheets/Copy of Culvert37.xlsm" #IDEA perhaps build this as a shiny gadget to allow for pushing onto the web for others?


cells <- xlsx_cells(sheet) %>% 
  filter(is_blank == FALSE, sheet == "Data Sheet - SITE") 
  
cells
```

    ## # A tibble: 352 x 21
    ##    sheet      address   row   col is_blank data_type error logical numeric
    ##    <chr>      <chr>   <int> <int> <lgl>    <chr>     <chr> <lgl>     <dbl>
    ##  1 Data Shee~ A1          1     1 FALSE    character <NA>  NA          NA 
    ##  2 Data Shee~ A5          5     1 FALSE    character <NA>  NA          NA 
    ##  3 Data Shee~ G7          7     7 FALSE    character <NA>  NA          NA 
    ##  4 Data Shee~ L7          7    12 FALSE    numeric   <NA>  NA          37.
    ##  5 Data Shee~ A9          9     1 FALSE    character <NA>  NA          NA 
    ##  6 Data Shee~ G9          9     7 FALSE    character <NA>  NA          NA 
    ##  7 Data Shee~ V9          9    22 FALSE    character <NA>  NA          NA 
    ##  8 Data Shee~ Z9          9    26 FALSE    date      <NA>  NA          NA 
    ##  9 Data Shee~ V10        10    22 FALSE    character <NA>  NA          NA 
    ## 10 Data Shee~ Z10        10    26 FALSE    date      <NA>  NA          NA 
    ## # ... with 342 more rows, and 12 more variables: date <dttm>,
    ## #   character <chr>, character_formatted <list>, formula <chr>,
    ## #   is_array <lgl>, formula_ref <chr>, formula_group <int>, comment <chr>,
    ## #   height <dbl>, width <dbl>, style_format <chr>, local_format_id <int>

There's a few issues (potential) that I've come across.

``` r
character <- cells[cells$data_type == "character", c("sheet", "address", "character")]
xlsx::write.xlsx(character, file = "spreadsheets/extractedKey.xlsx", sheetName = "Likely keys")

# Helper function to look up values in a cell of interest.
get.cell.value <- function(tidysheet, cellOfInterest){
  val <- tidysheet %>% filter(address == cellOfInterest)
  val[character,]
  
}
# 
# cells %>% filter(address == "G7")[]
#   
#   
#   get.cell.value("G7")
#   
```
