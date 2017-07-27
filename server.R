
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(XLConnect)
options(stringsAsFactors = FALSE)
script <- "

for(i = 0; i < $('#allCorrelations th').length; i++) {
  colorTable('allCorrelations', i);
}

for(i = 0; i < $('#dateCorrelations th').length; i++) {
  colorTable('dateCorrelations', i);
}


function colorTable(tableid, colindex){

  var columnarray, max, min, n;

  columnarray = [];
  $('#' + tableid + ' tr:not(:first)').each(function(){
    var val = parseFloat($(this).find('td').eq(colindex).text());
    if(val === val) {
      columnarray.push(val);
    }
  })
  
  max = Math.max(...columnarray);
  min = Math.min(...columnarray);
  console.log(columnarray);

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

    var val = parseFloat($(this).text());    

    // Catch exceptions outside of range
    if (val > max) {
    var val = max;
    }
    
    else if (val < min) {
    var val = min;
    }

    // Find value's position relative to range
    
    var pos = ((val-min) / (n));
    
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
    
    # Read in user-provided CSV file
    inFile <- input$csvfile
    if (is.null(inFile))
      return(NULL)
    datadf = read.csv(inFile$datapath)
    
    # Create vector of metric columns
    ignoreCols = c(input$yCol, input$dateCol, input$categoryCol)
    correlCols = names(datadf)[!names(datadf) %in% ignoreCols]
    
    ###################
    # All Data Points #
    ###################
    # Loop through each metric column, run regression, populate summary table
    summaryDF <- data.frame(Metric = character(),
                            Correlation = numeric(),
                            DoF = integer())
    
    for(col in correlCols) {
      form <- as.formula(paste0(input$yCol, " ~ ", col))
      tryCatch({fit <- lm(form, datadf)}, error = function(e) {NULL})
      summaryDF[nrow(summaryDF) + 1, "Metric"] <- col
      summaryDF[nrow(summaryDF), "Correlation"] <- cor(datadf[, col], datadf[, input$yCol], use = "pairwise.complete.obs")
      summaryDF[nrow(summaryDF), "DoF"] <- fit$df
    }
    
    output$allCorrelations <- renderTable({
      summaryDF
    })
    
    ###########
    # By Date #
    ###########
    dateCorrelations <- data.frame(Metric = correlCols,
                                   "Total Years" = integer(length(correlCols)),
                                   "Negative Years" = integer(length(correlCols)),
                                   "Avg Correlation" = numeric(length(correlCols)),
                                   check.names = FALSE)
    # Fill in correlations by date
    for(date in unique(datadf[, input$dateCol])) {
      dateDF <- datadf[datadf[,input$dateCol]==date, ]
      for(col in correlCols) {
        dateCorrelations[dateCorrelations$Metric == col, date] <- cor(dateDF[, col], dateDF[, input$yCol], use = "pairwise.complete.obs")
      }
    }
    
    # Fill in summary stats of date correlations
    for(col in correlCols) {
      metricCorrelations <- as.numeric(dateCorrelations[dateCorrelations$Metric == col, unique(datadf[, input$dateCol])])
      dateCorrelations[dateCorrelations$Metric == col, "Total Years"] <- length(metricCorrelations[!is.na(metricCorrelations)])
      dateCorrelations[dateCorrelations$Metric == col, "Negative Years"] <- length(metricCorrelations[metricCorrelations < 0])
      dateCorrelations[dateCorrelations$Metric == col, "Avg Correlation"] <- mean(metricCorrelations, na.rm = TRUE)
    }
    
    output$dateCorrelations <- renderTable({
      dateCorrelations
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
