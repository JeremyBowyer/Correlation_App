observeDownloadComparisonData <- function(input, output, session, vals) {
    output$downloadComparison <- downloadHandler(
    filename = function(){"comparison_data.xlsx"},
    content = function(file) {
      fname <- paste(file,"xlsx",sep=".")
      wb <- loadWorkbook(fname,create = TRUE)
      createSheet(wb,"data")
      writeWorksheet(wb,data = vals$summaryDF, sheet = "data")
      saveWorkbook(wb)
      file.rename(fname,file)
    },
    contentType="application/xlsx" 
  )
}