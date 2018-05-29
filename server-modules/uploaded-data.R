observeUploadedData <- function(input, output, session, vals) {
   
    observeEvent(input$csvfile, {
        inFile <- input$csvfile

        if (is.null(inFile))
        return(NULL)

        vals$unloadData(session, input, output, vals)

        datadf = read.csv(inFile$datapath)
        vals$datadf <- datadf
        vals$originaldf <- datadf
        vals$refreshInputs(session, input, vals)
    })

}