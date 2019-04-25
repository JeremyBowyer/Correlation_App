observeDownloadReport <- function(input, output, session, vals) {
    
    output$downloadReport <- downloadHandler(

    filename = function(){
      xname <- input$xCol
      xname <- gsub("[\\!\\@\\#\\$\\%\\^\\&\\*\\(\\)\\-\\=\\+\\.]", '',xname)
      
      yname <- input$yCol
      yname <- gsub("[\\!\\@\\#\\$\\%\\^\\&\\*\\(\\)\\-\\=\\+\\.]", '',yname)
      paste0(xname, " against ", yname, ".html")
    },
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("documents/report.Rmd", tempReport, overwrite = TRUE)
      
      # Subset DF to only include appropriate columns
      cols <- c(input$xCol, vals$yCol)
      if(input$categoryCol != "") cols <- c(input$categoryCol, cols)
      if(vals$dateCol != "") cols <- c(vals$dateCol, cols)
      df <- vals$metricdivedf[,cols]
      row.names(df) <- 1:nrow(df)
      
      # Set up parameters to pass to Rmd document
      params <- list(metric = input$xCol,
                     df = df,
                     perf = vals$perfdf,
                     dateCol = vals$dateCol,
                     categoryCol = input$categoryCol,
                     dateFormat = vals$dateFormat,
                     yCol = vals$yCol)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport,
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )

}