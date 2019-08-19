observeDownloadCustomData <- function(input, output, session, vals) {

#    observeEvent(input$downloadData,{
#      
#      dir = tk_choose.dir(default=vals$direct,caption="select folder")
#      
#      if(is.na(dir)){
#        return(NULL)
#      }
#      
#      shinyalert(
#           title = "",
#           text = "Your data is being prepared for download. It will download when ready. If your dataset is large, it may take several minutes.",
#           closeOnEsc = TRUE,
#           closeOnClickOutside = TRUE,
#           html = FALSE,
#           type = "info",
#           showConfirmButton = TRUE,
#           showCancelButton = FALSE,
#           confirmButtonText = "OK",
#           confirmButtonCol = "#3E3F3A",
#           timer = 0,
#           imageUrl = "",
#           animation = TRUE
#         )
#      
#      filexists = function(count){
#        if(file.exists(paste0(dir,"/",paste("custom_data",count,".csv")))) {
#          filexists(count+1)
#        } else {
#          write.csv(vals$datadf, paste(dir,"/",paste("custom_data",count,".csv")), row.names=FALSE)
#        }
#      }
#      
#      filexists(0)
#   })
# }

  
 observeEvent(input$save, {
   wd = getwd()
   file = parseSavePath(roots=c("E:"="E:", 'C:'='C:','Z:'='Z:','D:'='D:'),input$save)
   print(file)
   if(NROW(file) & (vals$name2 != file[["name"]]|| vals$dir2 != file[["datapath"]])) {
 
  vals$name2 = file[["name"]]
   vals$dir2 = file[["datapath"]] 
   completename = paste(vals$dir2)

     tryCatch({write.csv(vals$datadf,completename,row.names=F)},error=function(e){print(e)})
     


   } else {
  return(NULL)
}
   
   })}