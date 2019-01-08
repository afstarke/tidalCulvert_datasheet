# ONE OFF TOOL TO ADD CELLS TO CULVERY WORKBOOKS

library(xlsx)

newCulvertDataFolder <- "../../../Box Sync/Culvert Assessment/Tidal Assessments/TidalAssessmentWorkbooks/"
existingDatafolder <- "../../../Box Sync/Culvert Assessment/Tidal Assessments/Culvert Excel Sheets/"

# Start working with one.
# MUST read in complete file and then save as new file with inserted cells to preserve (fingers crossed) macros that are enabled within.
trialRun <- existingfiles[[1]]


existingfiles <- list.files(existingDatafolder, full.names = TRUE) # list of files of datasheets.

# for each of the file names listed above...

for(wbfile in existingfiles){
  tmp <- loadWorkbook(wbfile)
  cs <- CellStyle(tmp) +
    Font(xl, heightInPoints=10, isBold=TRUE, isItalic=TRUE,
         name="Courier New") +
    Alignment(h="ALIGN_RIGHT", wrapText = FALSE)
  tmpsheet <- getSheets(tmp) # Read sheets of which we're only interested in the SITE sheet (sheet 2)
  tmpcells <- getCells(getRows(tmpsheet[[2]]))
}

xl <- loadWorkbook(trialRun) # feed a file path to load in workbook
# Get style and set it to modify below.
cs <- CellStyle(xl) +
  Font(xl, heightInPoints=10, isBold=TRUE, isItalic=TRUE,
       name="Courier New") +
  Alignment(h="ALIGN_RIGHT", wrapText = FALSE)

## 

sheets <- getSheets(xl) # Read sheets of which we're only interested in the SITE sheet (sheet 2)
 # Get rows from the sheet of interest (sheet 2)
cells <- getCells(getRows(sheets[[2]]))# Get the 
# createRow(sheet = sheets[[2]], rowIndex = 6)

setCellValue(cells[["6.33"]], "Assessment Complete")
setCellStyle(cell = cells[["6.33"]], cellStyle = cs)
setCellValue(cells[["6.34"]], "N")
setCellStyle(cell = cells[["6.34"]], cellStyle = cs)

saveWorkbook(xl, "NewWorkbook.xlsm")
