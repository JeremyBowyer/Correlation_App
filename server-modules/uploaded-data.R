observeUploadedData <- function(input, output, session, vals) {
# 
#     observeEvent(input$button, {
# 
#         inFile = tk_choose.files()
# 
# 
#         if (length(inFile) == 0){
#           return(NULL)
#         }
# 
#         inFile2 = strsplit(inFile[1],"/",fixed=T)
# 
#         direct = inFile2[[1]][1:length(inFile2[[1]])-1]
# 
#         for (strval in direct) {
#           if(vals$direct== '') {
#             vals$direct = strval
#           } else {
#             vals$direct = paste0(vals$direct,"/",strval)
#           }
#         }
# 
#         filename= inFile2[length(inFile2)]
#         if (is.null(inFile) == 1)
#         return(NULL)
# 
#         vals$unloadData(session, input, output, vals)
# 
#         datadf = read.csv(inFile[1], check.names=FALSE)
# 
#         vals$datadf <- datadf
#         vals$originaldf <- datadf
#         vals$refreshInputs(session, input, vals)
#     })
# ( 
# }
  output$filename <- renderText({
    vals$name
    
  })
  observeEvent(input$file, {
    file = parseFilePaths(roots=c("E:"="E:", "C:"="C:",'Z:'='Z:','D:'='D:'),input$file)
    if(NROW(file) & (vals$name != file[["name"]]|| vals$dir != file[["datapath"]])) {
      vals$name = file[["name"]]
      vals$dir = file[["datapath"]] 
      completename = paste(vals$dir)
      
      datadf = read.csv(completename, check.names=FALSE)
      vals$datadf <- datadf
      vals$originaldf <- datadf
      vals$refreshInputs(session, input, vals)
    } else {
      return(NULL)
    }
     
  }
)}