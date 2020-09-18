#
#'
#' User Guide:
#' This code chunk helps streamline the process of making summary sheets for the freshwater site (tidal likely to use same design)
#' The premise is as follows:
#' First we generate a list of crossings (by crossing code) that needs summary sheets created. 
#' These crossings will be loaded in the summarySheetSetup.R code. 
source(here::here("summarySheetSetup.R"))
#' Initially this would be all the crossings that we want.
#' Then an HTML output is created for each crossing, and stored in an ouput folder here:
htmlOutputFolder <- "D:/culvert_project/tidalCrossings/html_outputs/"
#' From this folder a list of completed HTML outputs can be generated. Removal of an HTML file from this folder will 
#' trigger a replacement to be made on the next run of this code.
#' 


# Filter out the crossings where we don't want a summary sheet. 
# generating summary sheet for all crossings that have a valid score.
#' Tidal 
#' to start there's a lot of crossing that are missing bits that prohibit a final score from being calculated. Start with those.
tidalsites <- LIculvertPrioritization %>% filter(!is.na(Total_Prioritization)) %>% pull(crossingID)


#' Freshwater
#' fw_data <- read_rds(here::here("data/fwaterPrioritizations_RERUNS.rds"))
#' crossing_list <- fw_data %>% #filter(Total_Benefit != -99) %>%
#'  pull(CrosCode)
# individualReruns <- c("xy4075626072961637", "xy4076273672931253", "xy4083121472708051", "xy4079287373028281", "xy4081809272636601")

# Step #1 -
# Create HTML versions of all the crossing sheets needed.
# Render Rmd to HTML.
makeDocs <- function(crossings) {
  # Pandoc errors occaisonally throwing off loop.
  # Create a log of the pdf files that have been produced and rerun above loop removing the ones already created.
  # Successes are crossings that have been converted to pdf.
  crossing_list <- crossings
   successes <-
    # list.files("../../../Box Sync/LI_Road_Stream_Tidal_Crossing_Project_Resources/Documents/SummarySheets", pattern = ".pdf") %>% str_remove(pattern = ".pdf")
    # list.files("D:/culvert_project/html_outputs", pattern = ".pdf") %>% strstr_remove(pattern = ".pdf")
    list.files("D:/culvert_project/tidalCrossings/html_outputs", pattern = ".html") %>% str_remove(pattern = ".html") # DElete bad copies of the htmls here to reproduce.
    # list.files("D:/culvert_project/pdf_outputs", pattern = ".pdf") %>% str_remove(pattern = ".pdf") # DElete bad copies of the pdfs here to control whats rerun.
  print(paste(
    length(successes),
    "successfully printed, just",
    length(crossing_list) - length(successes),
    "more to go."
  ))
  
  todos <- crossing_list[!crossing_list %in% successes]
  # Uncomment for fresh reruns.
  # purrr::walk(
  #   .x = todos,
  #   .f = ~ rmarkdown::render(
  #     "summarySheets/summarySheet_tidal.Rmd.Rmd",
  #     poster_jacobs(css = "summary.css"),
  #     params = list(crossingCode = .x),
  #     output_file = paste0(.x, ".html"),
  #     output_dir = "D:/culvert_project/html_outputs",
  #     output_options = list(self_contained = FALSE)
  #   )
  # )
  # render tidal sheets.
  purrr::walk(
    .x = todos,
    .f = ~ rmarkdown::render(
      "summarySheets/summarySheet_tidal.Rmd",
      poster_jacobs(css = "tidalsummary.css"),
      params = list(crossingCode = .x),
      output_file = paste0(.x, ".html"),
      output_dir = "D:/culvert_project/tidalCrossings/html_outputs/",
      output_options = list(self_contained = FALSE)
    )
  )
}

# Run makeDocs to render HTMLs to the output folder
makeDocs(tidalsites)




# As of May 2020 automating chrome_print was throwing errors to work around is to open and print to pdf individually.

# Manually printing these things...
# Here's a few helpers-
# Open the files that need to be printed still.
 


# list the HTMLs that are in the working output directory. These are considered done- 
allhtmls <- list.files(path = htmlOutputFolder, # Remove any files from this directory that are UNWANTED.
             pattern = ".html",
             full.names = F) %>% str_remove(".html")
# create a list of successes to pick up in case code breaks. These are pdfs created from the htmls that are listed from above. 
pdfcheck <- function()
{ # returns a pep talk and the list of crossings that are in the pdf output folder.
  pdfs_completed <-
    list.files(
      "M:/Projects/LI/Culvert_Assessment/summarysheets" # Similar to above remove pdf versions that are UNWANTED. i.e. page breaks wrong etc. 
    ) %>%
    str_remove(pattern = ".pdf")
  print(paste0(
    "Hang in there champ, you've got ",
    length(pdfs_completed), " done already"))
    return(pdfs_completed)
  }

pdfcheck()

# filestoconvert <- allhtmls[!allhtmls %in% pdfs_completed]

statuscheck <- function()
{
  pdfs_completed <- pdfcheck()
  allhtmls <- list.files(path = htmlOutputFolder,
                         pattern = ".html",
                         full.names = F) %>% str_remove(".html")
  filestoconvert <- allhtmls[!allhtmls %in% pdfs_completed]
  print(paste0(
    "Hang in there big guy, only ",
    length(filestoconvert), " to go!")
  )}
statuscheck()

# Automating the opening of remaining documents.
get_to_work <- function(){
  allhtmls <- list.files(path = htmlOutputFolder,
                         pattern = ".html",
                         full.names = F) %>% str_remove(".html")
  pdfs_completed <- pdfcheck() # check which pdf's have been created
  # statuscheck()
  filestoconvert <- allhtmls[!allhtmls %in% pdfs_completed] # Create a list of crossings that need to be converted from HTML to PDF
 filestoconvert %>% paste0("file://", htmlOutputFolder, ., ".html") %>% 
    sample(20, replace = F) %>%
   walk(.x = ., ~browseURL(.x)) # Walk through each file and open in browser
  
}


get_to_work()


# PDF file crosswalk table
xwalk <- tibble(paths = list.files("M:/Projects/LI/Culvert_Assessment/summarysheets", pattern = "*.pdf", full.names = T), 
                crosCode = str_remove(paths, pattern = "M:/Projects/LI/Culvert_Assessment/summarysheets/") %>% str_remove(".pdf")) %>% 
  mutate(paths = str_replace(paths, pattern = "M:/", replacement = "D:/gisdata/"))

xwalk %>% write_xlsx(path = "M:/Projects/LI/Culvert_Assessment/summarysheets/crosswalkTable.xlsx")

individualReruns %>% paste0("file://D:/culvert_project/html_outputs/", ., ".html") %>% 
  # sample(20, replace = F) %>%
  walk(.x = ., ~browseURL(.x))


