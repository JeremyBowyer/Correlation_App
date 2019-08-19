observeUploadData <- function(input, output, session, vals) {

  volumes <- c(Home = fs::path_home(),
               "R Installation" = R.home(),
               getVolumes()()
               )
  
  shinyFileChoose(input,
                  'file',
                  roots=c("E:"="E:", 'C:'='C:','Z:'='Z:','D:'='D:'),
                  defaultPath='Dropbox\\Projects\\Catastrophic Loss Research', filetypes=c('', 'csv')
                  )
  shinyFileSave(input,
                "save",
                session = session,
                roots=c("E:"="E:", 'C:'='C:','Z:'='Z:','D:'='D:'),
                defaultPath='E:\\Dropbox\\Projects\\Catastrophic Loss Research'
                )
  
  
  output$name <- renderText({
    vals$name
    
  })
  
  observeEvent(input$file, {
    file = parseFilePaths(roots=c("E:"="E:", "C:"="C:",'Z:'='Z:','D:'='D:'),input$file)
    if(nrow(file) != 0 && (vals$name != file[["name"]]|| vals$dir != file[["datapath"]])) {
      
      vals$unloadData(session=session, input=input, output=output, vals=vals)
      
      vals$name = file[["name"]]
      vals$dir = file[["datapath"]] 
      completename = paste(vals$dir)
      
      datadf = read.csv(completename, check.names=FALSE)
      vals$datadf <- datadf
      vals$originaldf <- datadf
      vals$refreshInputs(session, input, vals)
      
      shinyalert(
        title = "",
        text = paste0(file[["name"]], " successfully uploaded. ", nrow(datadf), " total rows."),
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        html = FALSE,
        type = "success",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#3E3F3A",
        timer = 0,
        imageUrl = "",
        animation = TRUE
        )
      
    } else {
      return(NULL)
    }
     
  }
)}