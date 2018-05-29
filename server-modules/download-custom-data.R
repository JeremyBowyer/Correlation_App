observeDownloadCustomData <- function(input, output, session, vals) {
    output$downloadData <- downloadHandler(
    filename = function(){"custom_data.xlsx"},
    content = function(file) {
      fname <- paste(file,"xlsx",sep=".")
      wb <- loadWorkbook(fname,create = TRUE)
      createSheet(wb,"data")
      writeWorksheet(wb,data = vals$datadf, sheet = "data")
      saveWorkbook(wb)
      file.rename(fname,file)
    },
    contentType="application/xlsx" 
  )
}