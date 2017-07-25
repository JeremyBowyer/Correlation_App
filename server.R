
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output, session) {

  output$contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    inFile <- input$csvfile
    
    if (is.null(inFile))
      return(NULL)
    
    datadf = read.csv(inFile$datapath)

    for(col in c("yCol", "dateCol")) {
      updateSelectInput(session, col, choices=names(datadf))
    }
    
    datadf
    
  })
  
  
  observeEvent(input$run, {
    print(input$yCol)
    print(input$dateCol)
  })

})
