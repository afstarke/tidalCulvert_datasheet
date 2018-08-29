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

<style>
.column-left{
  float: left;
  width: 50%;
  text-align: left;
}
.column-center{
  display: inline-block;
  width: 50%;
  text-align: center;
}
</style>
``` r
# TODO Start with basic read in of spreadsheet and extracting the proper cells.
sheet <- "spreadsheets/Copy of Culvert37.xlsm" 
# Tidy up the sheets. 
# Selecting SUMMARY sheet here.
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

Seems as though there are 2 approaches we can take: \* Extract the RAW values from the **Data Sheet - SITE** sheet and perform the various calculations/lookups using R or other (ArcMap)

### Pros

-   More freedom and flexibility of up and down stream data management/analysis approaches, not limited to embedded excel formulas
-   

### Cons

-   Will require a fair amount of recoding Excel formulas into R code (or other)

There's a few issues (potential) that I've come across.

-   cells are merged which makes it hard to determine which cell actually contains the value of interest.
-   There are values that are selected using a formated control (like a dropdown) which can be very easily altered.
-   accessing these values is best done through the *Data Sheet - SUMMARY* tab.
-   There appear to be cells with just spaces possibly? They are showing up as blank, but NOT empty. Unsure if this is an issue.

Characters (aka Potential Keys):
--------------------------------

Below (and in extractedKey.xlsx) are extracted *character* values with their associated cell address.
**NOTE:** Some of these cells may in fact be data values and not keys.

``` r
characters <- cells[cells$data_type == "character", c("sheet", "address", "character")]

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

Numerics (aka Potential Values):
--------------------------------

Similar to the characters above, these are assumed to be data values but some could in fact be keys.

``` r
numerics <- cells[cells$data_type == "numeric", c("sheet", "address", "numeric")]

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

Functions/Formulas:
-------------------

Where are the formulas? And what do they do?

``` r
formulas <- cells %>% filter(!is.na(formula)) %>% select(sheet, address, formula)

formulas %>% head(5)
```

    ## # A tibble: 5 x 3
    ##   sheet                address formula                
    ##   <chr>                <chr>   <chr>                  
    ## 1 Data Sheet - SUMMARY G4      'Data Sheet - SITE'!G7 
    ## 2 Data Sheet - SUMMARY A5      'Data Sheet - SITE'!A9 
    ## 3 Data Sheet - SUMMARY V5      'Data Sheet - SITE'!V9 
    ## 4 Data Sheet - SUMMARY V6      'Data Sheet - SITE'!V10
    ## 5 Data Sheet - SUMMARY A7      'Data Sheet - SITE'!A11

Note that there are a fair number of formulas that simply reference a single cell from another sheet. An attempt at filtering these out

``` r
#TODO: Confirm we're not filtering anything important out here accidently.
formulas %>% filter(!str_detect(formula, "Data"))
```

    ## # A tibble: 263 x 3
    ##    sheet                address formula                                   
    ##    <chr>                <chr>   <chr>                                     
    ##  1 Data Sheet - SUMMARY N12     IF(AND(Calculations!D3>=3,Calculations!F3~
    ##  2 Data Sheet - SUMMARY N16     IF(Calculations!A2<=1,1,IF(AND(Calculatio~
    ##  3 Data Sheet - SUMMARY P16     IF(Calculations!B2<=1,1,IF(AND(Calculatio~
    ##  4 Data Sheet - SUMMARY N17     AVERAGE(N14,IF(N15>P15,N15,P15),IF(N16>P1~
    ##  5 Data Sheet - SUMMARY N19     N14                                       
    ##  6 Data Sheet - SUMMARY N26     IF(Calculations!A14>6,1,IF(AND(Calculatio~
    ##  7 Data Sheet - SUMMARY P26     IF(Calculations!D14>6,1,IF(AND(Calculatio~
    ##  8 Data Sheet - SUMMARY N27     IF(Calculations!E14>3,1,IF(AND(Calculatio~
    ##  9 Data Sheet - SUMMARY P27     IF(Calculations!F14>3,1,IF(AND(Calculatio~
    ## 10 Data Sheet - SUMMARY N31     IF(N26>P26,IF(AND(N12=1,N26<=2),1,IF(OR(N~
    ## # ... with 253 more rows

Slice into these formulas
-------------------------

If we need to extract some values or explore what a formula is doing we can use xlex by passing the row of interest to the function.

Yikes! On a few of these...

``` r
# Finding constants within formulas:
# directly from : https://nacnudus.github.io/tidyxl/articles/smells.html

tokens <-
  cells %>%
  filter(!is.na(formula)) %>%
  select(row, col, formula) %>%
  mutate(tokens = map(formula, xlex)) %>%
  select(-formula)

constants <- tokens %>% 
  unnest(tokens) %>% 
  filter(type %in% c("error", "bool", "number", "text"))

constants %>%
  count(token, sort = TRUE) %>%
  print(n = Inf)
```

    ## # A tibble: 22 x 2
    ##    token       n
    ##    <chr>   <int>
    ##  1 0          81
    ##  2 3          66
    ##  3 5          56
    ##  4 2          55
    ##  5 1          53
    ##  6 "\"\""     51
    ##  7 4          45
    ##  8 "\"D\""    35
    ##  9 "\"R\""    35
    ## 10 "\"U\""    35
    ## 11 1.5         8
    ## 12 TRUE        6
    ## 13 1.2         4
    ## 14 10          4
    ## 15 12          4
    ## 16 13          4
    ## 17 2.3         4
    ## 18 6           4
    ## 19 0.5         2
    ## 20 0.7         1
    ## 21 0.8         1
    ## 22 0.9         1

TO DOs:
-------

-   Using a completed (and backed up) datasheet comb through the *spreadsheets/extractedKey\_WORKING.xlsx* file to match data keys with data values.

-   **VERY IMPORTANT** work only in the 'extractedKey\_WORKING' file. The other file will/can be overwritten.

-   Once this new master lookup/key is build here's the game plan.
-   Create lookup in R using key-value pairs built from the extractedKey file
-   within this lookup retain the 'expected keys' in a colum to act as an error catcher on data extractions later.
-   Need to extract lookup tables in Lookup tab to form joins to link data value with data description (7 = Riprap under the Wingwall Materials dropdown.)

``` r
# Helper function to look up values in a cell of interest.
# 

get.cell.value <- function(tidysheet, cellOfInterest){
  
  val <- tidysheet %>% 
    filter(address == cellOfInterest) %>% 
    pull(formula)
  return(val)
  
}
#   
get.cell.value(cells, "AA65")
```

    ## [1] "IF(AA111=0,\"\", AA111)"
