# Helper functions

# Save a summaryRoutput dataframe to excel for others to review ----

saveSumary <- function(summaryDF){
  xlsx::write.xlsx(x = summaryDF, file = "outputs/summaryRoutput.xlsx", showNA = FALSE)
}
