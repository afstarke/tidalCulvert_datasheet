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
  filter(is_blank == FALSE, sheet == "Data Sheet - SUMMARY") 
  
cells
```

    ## # A tibble: 581 x 21
    ##    sheet      address   row   col is_blank data_type error logical numeric
    ##    <chr>      <chr>   <int> <int> <lgl>    <chr>     <chr> <lgl>     <dbl>
    ##  1 Data Shee~ A1          1     1 FALSE    character <NA>  NA          NA 
    ##  2 Data Shee~ G4          4     7 FALSE    character <NA>  NA          NA 
    ##  3 Data Shee~ L4          4    12 FALSE    numeric   <NA>  NA          37.
    ##  4 Data Shee~ A5          5     1 FALSE    character <NA>  NA          NA 
    ##  5 Data Shee~ G5          5     7 FALSE    character <NA>  NA          NA 
    ##  6 Data Shee~ V5          5    22 FALSE    character <NA>  NA          NA 
    ##  7 Data Shee~ Z5          5    26 FALSE    date      <NA>  NA          NA 
    ##  8 Data Shee~ V6          6    22 FALSE    character <NA>  NA          NA 
    ##  9 Data Shee~ Z6          6    26 FALSE    date      <NA>  NA          NA 
    ## 10 Data Shee~ A7          7     1 FALSE    character <NA>  NA          NA 
    ## # ... with 571 more rows, and 12 more variables: date <dttm>,
    ## #   character <chr>, character_formatted <list>, formula <chr>,
    ## #   is_array <lgl>, formula_ref <chr>, formula_group <int>, comment <chr>,
    ## #   height <dbl>, width <dbl>, style_format <chr>, local_format_id <int>

There's a few issues (potential) that I've come across.

-   cells are merged which makes it hard to determine which cell actually contains the value of interest.
-   There are values that are selected using a formated control (like a dropdown) which can be very easily altered.
-   accessing these values is best done through the *Data Sheet - SUMMARY* tab.

``` r
characters <- cells[cells$data_type == "character", c("sheet", "address", "character")]
numerics <- cells[cells$data_type == "numeric", c("sheet", "address", "numeric")]
```

``` r
xlsx::write.xlsx(characters, file = "spreadsheets/extractedKey.xlsx", sheetName = "Likely keys") 
xlsx::write.xlsx(numerics, file = "spreadsheets/extractedKey.xlsx", sheetName = "Likely values", append = TRUE)

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
