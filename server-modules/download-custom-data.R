observeDownloadCustomData <- function(input, output, session, vals) {
    output$downloadData <- downloadHandler(
    filename = function(){"custom_data.csv"},
    content = function(file) {
      write.csv(vals$datadf, file, row.names=FALSE)
      #fname <- paste(file,"xlsx",sep=".")
      #wb <- loadWorkbook(fname,create = TRUE)
      #createSheet(wb,"data")
      #writeWorksheet(wb,data = vals$datadf, sheet = "data")
      #saveWorkbook(wb)
      #file.rename(fname,file)
    },
    contentType="text/csv" 
  )
    
    observeEvent(input$downloadData, {
      
      shinyalert(
          title = "",
          text = "Your data is being prepared for download. It will download when ready. If your dataset is large, it may take several minutes.",
          closeOnEsc = TRUE,
          closeOnClickOutside = TRUE,
          html = FALSE,
          type = "info",
          showConfirmButton = TRUE,
          showCancelButton = FALSE,
          confirmButtonText = "OK",
          confirmButtonCol = "#3E3F3A",
          timer = 0,
          imageUrl = "",
          animation = TRUE
        )
      
    })
    
}