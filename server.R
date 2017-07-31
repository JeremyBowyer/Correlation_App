
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(XLConnect)
library(ggvis)
library(dplyr)
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
  
  ###################
  # Reactive Values #
  ###################
  vals <- reactiveValues(filterCount = 0, datadf = data.frame(), originaldf = data.frame())
  
  ###########
  # Methods #
  ###########
  getCols <- reactive({
    df = vals$datadf
    return(names(df))
  })
  
  ###########################
  # Option Panel Conditions #
  ###########################
  output$fileUploaded <- reactive({
    return(nrow(vals$originaldf) > 0)
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  
  output$hierarchicalCheck <- reactive({
    return(input$hierBox)
  })
  outputOptions(output, 'hierarchicalCheck', suspendWhenHidden=FALSE)
  
  ##################
  # Event Handlers #
  ##################
  # File Uploaded
  observeEvent(input$csvfile, {
      inFile <- input$csvfile

      if (is.null(inFile))
        return(NULL)

      datadf = read.csv(inFile$datapath)
      vals$datadf <- datadf
      vals$originaldf <- datadf
  })
  
  # Add filter Button
  observeEvent(input$addFilter, {
    
    if(vals$filterCount == 0){
      insertUI(
        selector="#addFilter",
        where="afterEnd",
        ui = actionLink("filterClear", "Clear All Filters", style="color: #f12828;")
      )
      
      insertUI(
        selector="#addFilter",
        where="afterEnd",
        ui = tags$br(class="filter")
      )
      
      insertUI(
        selector="#tagsDiv",
        where="afterEnd",
        ui = actionButton("applyFilters", "Apply Filters", icon("filter"), style="padding: 5px 10px 5px 10px;")
      )
      
    }
    
    vals$filterCount <- vals$filterCount + 1
    
    insertUI(
      selector="#tagsDiv",
      where="beforeEnd",
      ui = tags$div(selectInput(paste0("filter",vals$filterCount), paste0("Filter ",vals$filterCount), getCols()), class="filter")
    )
    
    insertUI(
      selector="#tagsDiv",
      where="beforeEnd",
      ui = tags$div(numericInput(paste0("filter",vals$filterCount, "Max"), "Max", getCols()), class="filter")
    )
    
    insertUI(
      selector="#tagsDiv",
      where="beforeEnd",
      ui = tags$div(numericInput(paste0("filter",vals$filterCount, "Min"), "Min", getCols()), class="filter")
    )
    
    insertUI(
      selector="#tagsDiv",
      where="beforeEnd",
      tags$div(tags$hr(), class="filter")
    )
    
  })
  
  # Clear Filters Button
  observeEvent(input$filterClear, {
    removeUI(".filter", multiple = TRUE)
    removeUI("#applyFilters")
    removeUI("#filterClear")
    vals$filterCount <- 0
    vals$datadf <- vals$originaldf
  })
  
  # Apply Filters Button
  observeEvent(input$applyFilters, {
    df <- vals$originaldf
    # Subset by filters, if necessary
    if(vals$filterCount > 0) {
      for(filter in 1:vals$filterCount){
        filterCol <- input[[paste0("filter",filter)]]
        filterMax <- input[[paste0("filter",filter, "Max")]]
        filterMin <- input[[paste0("filter",filter, "Min")]]
        
        df <- subset(df, df[,filterCol] <= filterMax & df[,filterCol] >= filterMin)
      }
    }
    
    vals$datadf <- df
  })
  
  # Run Analysis Button
  observeEvent(input$run, {
    
    # Read in user-provided CSV file
    datadf <- vals$datadf
    
    # Y Column
    if(input$hierBox){
      
      form <- as.formula(paste0(input$yCol, " ~ ", input$hierCol))
      fit <- lm(form, datadf)
      intercept <- fit$coefficients[1]
      coefficient <- fit$coefficients[2]
      datadf$residual <- datadf[,input$yCol] - (datadf[, input$hierCol] * coefficient + intercept)
      
      ignoreCols = c(input$hierCol, input$yCol, input$dateCol, input$categoryCol, "residual")
      yColumn <- "residual"
      
    } else {
      ignoreCols = c(input$yCol, input$dateCol, input$categoryCol)
      yColumn <- input$yCol
    }
    
    # Create vector of metric columns
    correlCols = names(datadf)[!names(datadf) %in% ignoreCols]
    
    # Update Metric Dive Dropdown
    updateSelectInput(session, "xCol", choices=correlCols)
    
    ## All Data Points ##
    # Loop through each metric column, run regression, populate summary table
    summaryDF <- data.frame(Metric = character(),
                            Correlation = numeric(),
                            DoF = integer())
    
    for(col in correlCols) {
      form <- as.formula(paste0(yColumn, " ~ ", col))
      tryCatch({fit <- lm(form, datadf)}, error = function(e) {NULL})
      summaryDF[nrow(summaryDF) + 1, "Metric"] <- col
      summaryDF[nrow(summaryDF), "Correlation"] <- cor(datadf[, col], datadf[, yColumn], use = "pairwise.complete.obs")
      summaryDF[nrow(summaryDF), "DoF"] <- fit$df
    }
    
    output$allCorrelations <- renderTable({
      summaryDF
    })
    
    ## By Date ##
    dateCorrelations <- data.frame(Metric = correlCols,
                                   "Total Periods" = integer(length(correlCols)),
                                   "Negative Periods" = integer(length(correlCols)),
                                   "Avg Correlation" = numeric(length(correlCols)),
                                   check.names = FALSE)
    # Fill in correlations by date
    for(date in unique(datadf[, input$dateCol])) {
      dateDF <- datadf[datadf[,input$dateCol]==date, ]
      for(col in correlCols) {
        dateCorrelations[dateCorrelations$Metric == col, date] <- cor(dateDF[, col], dateDF[, yColumn], use = "pairwise.complete.obs")
      }
    }
    
    # Fill in summary stats of date correlations
    for(col in correlCols) {
      metricCorrelations <- as.numeric(dateCorrelations[dateCorrelations$Metric == col, unique(datadf[, input$dateCol])])
      dateCorrelations[dateCorrelations$Metric == col, "Total Periods"] <- length(metricCorrelations[!is.na(metricCorrelations)])
      dateCorrelations[dateCorrelations$Metric == col, "Negative Periods"] <- length(metricCorrelations[metricCorrelations < 0])
      dateCorrelations[dateCorrelations$Metric == col, "Avg Correlation"] <- mean(metricCorrelations, na.rm = TRUE)
    }
    
    output$dateCorrelations <- renderTable({
      dateCorrelations
    })
    
    updateTabsetPanel(session, "mainTabset", selected="correlations")
    
  })
  
  # Upon selection of metric in Metric Dive tab
  observeEvent(input$xCol, {
    if(input$xCol != ""){
      
      vis <- reactive({
        
        df <- vals$datadf
        df <- subset(df, !is.na(df[,input$xCol]) & !is.na(df[,input$yCol]))
        df[, input$categoryCol] <- as.factor(df[, input$categoryCol])
        
        xvar <- prop("x", as.symbol(input$xCol))
        yvar <- prop("y", as.symbol(input$yCol))
        fillvar <- prop("fill", as.symbol(input$categoryCol))
  
        df %>%
          ggvis(x = xvar, y = yvar) %>%
          layer_points(fill = fillvar) %>%
          add_tooltip(function(data){
            paste0(input$categoryCol, ": ", data[[input$categoryCol]], "<br>",
                   input$xCol, ": ", data[[input$xCol]], "<br>",
                   input$yCol, ": ", data[[input$yCol]], "<br>")
          }, "hover") %>%
          layer_model_predictions(model = "lm", se = TRUE) %>%
          hide_legend('fill')
      })
      
      vis %>% bind_shiny("metricPlot")
    }
  })
  
  #######################
  # Data Preview Screen #
  #######################
  output$dataPreview <- renderTable({
    
    for(col in c("hierCol", "yCol", "dateCol", "categoryCol")) {
      updateSelectInput(session, col, choices=getCols(), selected=input[[col]])
    }
    
    return(vals$datadf)
    
  })
  
  #####################################
  # JavaScript Conditional Formatting #
  #####################################
  session$onFlushed(function() {
    session$sendCustomMessage(type='jsCode', list(value = script))
  }, FALSE)
  
})
