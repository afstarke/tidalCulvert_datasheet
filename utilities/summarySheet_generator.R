

source(here::here("summarySheetSetup.R"))

# Filter out the crossings where we don't want a summary sheet. 
# TODO: Confirm with team on HOW to do this.
# generating summary sheet for all crossings that have a valid score.
crossing_list <- fw_data %>% filter(Total_Benefit != -99) %>% pull(CrosCode)


# Render Rmd to HTML.
makeDocs <- function() {
  # Pandoc errors occaisonally throwing off loop.
  # Create a log of the pdf files that have been produced and rerun above loop removing the ones already created.
  # Successes are crossings that have been converted to pdf.
  successes <-
    # list.files("../../../Box Sync/LI_Road_Stream_Tidal_Crossing_Project_Resources/Documents/SummarySheets", pattern = ".pdf") %>% str_remove(pattern = ".pdf")
    # list.files("D:/culvert_project/html_outputs", pattern = ".pdf") %>% strstr_remove(pattern = ".pdf")
    # list.files("D:/culvert_project/html_outputs", pattern = ".html") %>% str_remove(pattern = ".html") # DElete bad copies of the htmls here to reproduce.
    list.files("D:/culvert_project/pdf_outputs", pattern = ".pdf") %>% str_remove(pattern = ".pdf") # DElete bad copies of the htmls here to reproduce.
  print(paste(
    length(successes),
    "successfully printed, just",
    length(crossing_list) - length(successes),
    "more to go."
  ))
  
  todos <- crossing_list[!crossing_list %in% successes]
  
  purrr::walk(
    .x = todos,
    .f = ~ rmarkdown::render(
      "summarySheets/summarySheet_fw.Rmd",
      poster_jacobs(css = "summary.css"),
      params = list(crossingCode = .x),
      output_file = paste0(.x, ".html"),
      output_dir = "D:/culvert_project/html_outputs",
      output_options = list(self_contained = FALSE)
    )
  )
}

makeDocs()






# Manually printing these things...
# Here's a few helpers-
# Open the files that need to be printed still.
 


# list the HTMLs that are in the working output directory. These are considered done- 
allhtmls <- list.files(path = "D:/culvert_project/html_outputs/", # Remove any files from this directory that are UNWANTED.
             pattern = ".html",
             full.names = F) %>% str_remove(".html")
# create a list of successes to pick up in case code breaks. These are pdfs created from the htmls that are listed from above. 
pdfcheck <- function()
{ # returns a pep talk and the list of crossings that are in the pdf output folder.
  pdfs_completed <-
    list.files(
      "D:/culvert_project/pdf_outputs/" # Similar to above remove pdf versions that are UNWANTED. i.e. page breaks wrong etc. 
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
  allhtmls <- list.files(path = "D:/culvert_project/html_outputs/",
                         pattern = ".html",
                         full.names = F) %>% str_remove(".html")
  filestoconvert <- allhtmls[!allhtmls %in% pdfs_completed]
  print(paste0(
    "Hang in there big guy, only ",
    length(filestoconvert), " to go!")
  )}
statuscheck()

get_to_work <- function(){
  allhtmls <- list.files(path = "D:/culvert_project/html_outputs/",
                         pattern = ".html",
                         full.names = F) %>% str_remove(".html")
  pdfs_completed <- pdfcheck()
  # statuscheck()
  filestoconvert <- allhtmls[!allhtmls %in% pdfs_completed]
 filestoconvert %>% paste0("file://D:/culvert_project/html_outputs/", ., ".html") %>% 
    # sample(20, replace = F) %>% 
   walk(.x = ., ~browseURL(.x))
  
}


get_to_work()


# PDF file crosswalk table
xwalk <- tibble(paths = list.files("M:/Projects/LI/Culvert_Assessment/summarysheets", pattern = "*.pdf", full.names = T), 
                crosCode = str_remove(paths, pattern = "M:/Projects/LI/Culvert_Assessment/summarysheets/") %>% str_remove(".pdf")) %>% 
  mutate(paths = str_replace(paths, pattern = "M:/", replacement = "D:/gisdata/"))

xwalk %>% write_xlsx(path = "M:/Projects/LI/Culvert_Assessment/summarysheets/crosswalkTable.xlsx")

