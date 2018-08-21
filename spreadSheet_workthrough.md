Tidal Spreadsheet fun
================

read in spreadsheet with tidyxl

``` r
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
-   There appear to be cells with just spaces possibly? They are showing up as blank, but NOT empty. Unsure if this is an issue.

``` r
characters <- cells[cells$data_type == "character", c("sheet", "address", "character")]
numerics <- cells[cells$data_type == "numeric", c("sheet", "address", "numeric")]
```

Characters:
===========

Below (and in extractedKey.xlsx) are extracted *character* values with their associated cell address.
**NOTE:** Some of these cells may in fact be data values and not keys.

``` r
characters
```

    ## # A tibble: 321 x 3
    ##    sheet                address character                                 
    ##    <chr>                <chr>   <chr>                                     
    ##  1 Data Sheet - SUMMARY A1      "Tidal Crossing Summary Sheet\r\nNew Hamp~
    ##  2 Data Sheet - SUMMARY G4      Crossing ID:                              
    ##  3 Data Sheet - SUMMARY A5      Observer(s) & Organization:               
    ##  4 Data Sheet - SUMMARY G5      Matthew Grasso, and Matthew Hamilton      
    ##  5 Data Sheet - SUMMARY V5      Date:                                     
    ##  6 Data Sheet - SUMMARY V6      Start Time:                               
    ##  7 Data Sheet - SUMMARY A7      Municipality:                             
    ##  8 Data Sheet - SUMMARY G7      Shelter Island                            
    ##  9 Data Sheet - SUMMARY V7      End Time:                                 
    ## 10 Data Sheet - SUMMARY A8      Stream Name:                              
    ## # ... with 311 more rows

Numerics:
=========

Similar to the characters above, these are assumed to be data values but could in fact be keys.

``` r
numerics
```

    ## # A tibble: 248 x 3
    ##    sheet                address numeric
    ##    <chr>                <chr>     <dbl>
    ##  1 Data Sheet - SUMMARY L4        37.0 
    ##  2 Data Sheet - SUMMARY AA10       2.15
    ##  3 Data Sheet - SUMMARY N12        2.00
    ##  4 Data Sheet - SUMMARY N15        1.00
    ##  5 Data Sheet - SUMMARY P15        1.00
    ##  6 Data Sheet - SUMMARY N21        1.00
    ##  7 Data Sheet - SUMMARY N22        1.00
    ##  8 Data Sheet - SUMMARY N24        4.00
    ##  9 Data Sheet - SUMMARY N26        4.00
    ## 10 Data Sheet - SUMMARY P26        5.00
    ## # ... with 238 more rows

``` r
# Careful here- this could overwrite work if a new file isn't created.
# xlsx::write.xlsx(characters, 
#                  file = "spreadsheets/extractedKey.xlsx", 
#                  sheetName = "Likely keys") 
# xlsx::write.xlsx(numerics, 
#                  file = "spreadsheets/extractedKey.xlsx", 
#                  sheetName = "Likely values", append = TRUE)
```

TO DOs:
=======

-   Using a completed (and backed up) datasheet comb through the *spreadsheets/extractedKey.xlsx* file to match data keys with data values.

-   **VERY IMPORTANT** save this file to new path as the code above ~~can~~ will overwrite this file if run.

``` r
# Helper function to look up values in a cell of interest.
# 
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
