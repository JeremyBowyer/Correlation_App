
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(XLConnect)
script <- "
var count;
var tableid = 'allCorrelations'

count = $('#' + tableid + ' th').length;

for(i = 0; i < count; i++) {

  colorColumn(tableid, i);

}


function colorColumn(tableid, colindex){

  var columnarray, max, min, n;

  columnarray = [];
  $('#' + tableid + ' tr:not(:first)').each(function(){
  
    columnarray.push(parseInt($(this).find('td').eq(colindex).text()));
  
  })
  
  max = Math.max(...columnarray);
  min = Math.min(...columnarray);
  
  n = max-min;
  
  // Define the min colour, which is white
  xr = 255; // Red value
  xg = 255; // Green value
  xb = 255; // Blue value
  
  // Define the max colour #2ca25f
  yr = 44; // Red value
  yg = 162; // Green value
  yb = 95; // Blue value
  
  $('#' + tableid + ' tr td:nth-child(' + (colindex + 1) + ')').each(function() {

    var val = parseInt($(this).text());    

    console.log(max);
    console.log(min);
    // Catch exceptions outside of range
    if (val > max) {
    var val = max;
    }
    
    else if (val < min) {
    var val = min;
    }

    // Find value's position relative to range
    
    var pos = ((val-min) / (n-1));
    
    // Generate RGB code
    red = parseInt((xr + (( pos * (yr - xr)))).toFixed(0));
    green = parseInt((xg + (( pos * (yg - xg)))).toFixed(0));
    blue = parseInt((xb + (( pos * (yb - xb)))).toFixed(0));
    
    clr = 'rgb('+red+','+green+','+blue+')';
  
    // Apply to cell
    
    $(this).css('background-color', clr);
  
  })
}
"

shinyServer(function(input, output, session) {
  
  output$dataPreview <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    inFile <- input$csvfile
    
    if (is.null(inFile))
      return(NULL)
    
    datadf = read.csv(inFile$datapath)
    
    for(col in c("yCol", "dateCol", "categoryCol")) {
      updateSelectInput(session, col, choices=names(datadf))
    }
    
    datadf
    
  })
  
  observeEvent(input$run, {
      
    output$allCorrelations <- renderTable({
      # Read in user-provided CSV file
      inFile <- input$csvfile
      if (is.null(inFile))
        return(NULL)
      datadf = read.csv(inFile$datapath)
      
      
    })
    
  })
  
  data1 <- mtcars
  
  output$downloadData <- downloadHandler(
    filename = function(){"mtcars.xlsx"},
    content = function(file) {
      fname <- paste(file,"xlsx",sep=".")
      wb <- loadWorkbook(fname,create = TRUE)
      createSheet(wb,"cars")
      writeWorksheet(wb,data = data1,sheet = "cars")
      saveWorkbook(wb)
      file.rename(fname,file)
    },
    contentType="application/xlsx" 
  )
  
  session$onFlushed(function() {
    session$sendCustomMessage(type='jsCode', list(value = script))
  }, FALSE)
  
})
