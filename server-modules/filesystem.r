establishconnction<- function(input,output,session,vals){
  
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
}