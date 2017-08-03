
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(XLConnect)
library(ggvis)
library(dplyr)
options(shiny.deprecation.messages=FALSE)
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
  vals <- reactiveValues(valueFilterCount = 0, percentileFilterCount = 0, dateFilterCount = 0, datadf = data.frame(), originaldf = data.frame())
  
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
  
  # Add filter Buttons
  observeEvent(input$addValueFilter, {
    
    if(vals$valueFilterCount == 0){

      insertUI(
        selector="#valueFilters",
        where="afterBegin",
        ui = h3("Value Filters", class="valueFilter")
      )
      
    }
    
    vals$valueFilterCount <- vals$valueFilterCount + 1
    
    insertUI(
      selector="#valueFilters",
      where="beforeEnd",
      ui = tags$div(selectInput(paste0("valueFilter",vals$valueFilterCount), paste0("Filter ",vals$valueFilterCount), getCols()), class="valueFilter")
    )
    
    insertUI(
      selector="#valueFilters",
      where="beforeEnd",
      ui = tags$div(textInput(paste0("valueFilter",vals$valueFilterCount, "Min"), "Min"), class="valueFilter", style="display:inline-block")
    )
    
    insertUI(
      selector="#valueFilters",
      where="beforeEnd",
      ui = tags$div(textInput(paste0("valueFilter",vals$valueFilterCount, "Max"), "Max"), class="valueFilter", style="display:inline-block")
    )
    
    insertUI(
      selector="#valueFilters",
      where="beforeEnd",
      tags$div(tags$hr(), class="valueFilter")
    )
    
  })
  
  # Percentile Filter
  observeEvent(input$addPercentileFilter, {
    
    if(vals$percentileFilterCount == 0){
      
      insertUI(
        selector="#percentileFilters",
        where="afterBegin",
        ui = h3("Percentile Filters (example: 0.05 for 5th percentile)", class="percentileFilter")
      )
      
    }
    
    vals$percentileFilterCount <- vals$percentileFilterCount + 1
    
    insertUI(
      selector="#percentileFilters",
      where="beforeEnd",
      ui = tags$div(selectInput(paste0("percentileFilter",vals$percentileFilterCount), paste0("Filter ",vals$percentileFilterCount), getCols()), class="percentileFilter")
    )
    
    insertUI(
      selector="#percentileFilters",
      where="beforeEnd",
      ui = tags$div(textInput(paste0("percentileFilter",vals$percentileFilterCount, "Min"), "Min"), class="percentileFilter", style="display:inline-block")
    )
    
    insertUI(
      selector="#percentileFilters",
      where="beforeEnd",
      ui = tags$div(textInput(paste0("percentileFilter",vals$percentileFilterCount, "Max"), "Max"), class="percentileFilter", style="display:inline-block")
    )
    
    insertUI(
      selector="#percentileFilters",
      where="beforeEnd",
      tags$div(tags$hr(), class="percentileFilter")
    )
    
  })
  
  # Date Filter
  observeEvent(input$addDateFilter, {
    
    if(vals$dateFilterCount == 0){
      
      insertUI(
        selector="#dateFilters",
        where="afterBegin",
        ui = tags$br(class = "dateFilter")
      )
      
      insertUI(
        selector="#dateFilters",
        where="afterBegin",
        ui = tags$br(class = "dateFilter")
      )
      
      insertUI(
        selector="#dateFilters",
        where="afterBegin",
        ui =  tags$div(
          h3("Date Filters", class="dateFilter"),
          a("Example Formats", href="http://www.statmethods.net/input/dates.html", target="_blank"),
          class="dateFilter", style="display:inline-block"
        )
      )
    
    }
    
    vals$dateFilterCount <- vals$dateFilterCount + 1
    
    insertUI(
      selector="#dateFilters",
      where="beforeEnd",
      ui = tags$div(selectInput(paste0("dateFilter",vals$dateFilterCount), paste0("Filter ",vals$dateFilterCount), getCols()), class="dateFilter")
    )
    
    insertUI(
      selector="#dateFilters",
      where="beforeEnd",
      ui = tags$div(textInput(paste0("dateFilter",vals$dateFilterCount, "Min"), "Min"), class="dateFilter", style="display:inline-block")
    )
    
    insertUI(
      selector="#dateFilters",
      where="beforeEnd",
      ui = tags$div(textInput(paste0("dateFilter",vals$dateFilterCount, "Max"), "Max"), class="dateFilter", style="display:inline-block")
    )
    
    insertUI(
      selector="#dateFilters",
      where="beforeEnd",
      ui = tags$div(textInput(paste0("dateFilter",vals$dateFilterCount, "Format"), "Format dates are in (see above for example formats).", "%m/%d/%Y"),
        class="dateFilter", style="display:inline-block")
    )
    
    insertUI(
      selector="#dateFilters",
      where="beforeEnd",
      tags$div(tags$hr(), class="dateFilter")
    )
    
  })
  
  # Clear Filters Button
  observeEvent(input$filterClear, {
    removeUI(".valueFilter", multiple = TRUE)
    removeUI(".percentileFilter", multiple = TRUE)
    removeUI(".dateFilter", multiple = TRUE)
    vals$valueFilterCount <- 0
    vals$percentileFilterCount <- 0
    vals$dateFilterCount <- 0
    vals$datadf <- vals$originaldf
  })
  
  # Apply Filters Button
  observeEvent(input$applyFilters, {
    df <- vals$originaldf
    # Value Filters
    if(vals$valueFilterCount > 0) {
      for(filter in 1:vals$valueFilterCount){
        filterCol <- input[[paste0("valueFilter",filter)]]
        filterMin <- input[[paste0("valueFilter",filter, "Min")]]
        filterMax <- input[[paste0("valueFilter",filter, "Max")]]
        
        filterMin <- as.numeric(filterMin)
        filterMax <- as.numeric(filterMax)
        df[, filterCol] <- as.numeric(df[, filterCol])

        if(is.na(filterMax)) filterMax <- Inf
        if(is.na(filterMin)) filterMin <- -Inf
        
        df <- subset(df, (df[,filterCol] <= filterMax & df[,filterCol] >= filterMin) | is.na(df[,filterCol]))
      }
    }
    
    # Percentile Filters
    if(vals$percentileFilterCount > 0) {
      for(filter in 1:vals$percentileFilterCount){
        filterCol <- input[[paste0("percentileFilter",filter)]]
        filterMin <- input[[paste0("percentileFilter",filter, "Min")]]
        filterMax <- input[[paste0("percentileFilter",filter, "Max")]]
        
        df[, filterCol] <- as.numeric(df[, filterCol])
        
        if(filterMax == "") filterMax <- Inf else filterMax <- quantile(df[, filterCol], as.numeric(filterMax), na.rm = TRUE)
        if(filterMin == "") filterMin <- Inf else filterMin <- quantile(df[, filterCol], as.numeric(filterMin), na.rm = TRUE)
        
        df <- subset(df, (df[,filterCol] <= filterMax & df[,filterCol] >= filterMin) | is.na(df[,filterCol]))
      }
    }
    
    # Date Filters
    if(vals$dateFilterCount > 0) {
      for(filter in 1:vals$dateFilterCount){
        filterCol <- input[[paste0("dateFilter",filter)]]
        filterMin <- input[[paste0("dateFilter",filter, "Min")]]
        filterMax <- input[[paste0("dateFilter",filter, "Max")]]
        filterDateFormat <- input[[paste0("dateFilter",filter,"Format")]]
        
        filterMin <- as.Date(filterMin, format= filterDateFormat)
        filterMax <- as.Date(filterMax, format= filterDateFormat)
        
        if(is.na(filterMax)) filterMax <- Inf
        if(is.na(filterMin)) filterMin <- -Inf
        
        df[, filterCol] <- as.Date(as.character(df[, filterCol]), format= filterDateFormat)
        df <- subset(df, (df[,filterCol] <= filterMax & df[,filterCol] >= filterMin) | is.na(df[,filterCol]))
        df[, filterCol] <- as.character(df[, filterCol])
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
      
      ignoreCols = c(input$hierCol, input$yCol, input$dateCol, input$categoryCol, input$ignoreCols, "residual")
      yColumn <- "residual"
      
    } else {
      ignoreCols = c(input$yCol, input$dateCol, input$categoryCol, input$ignoreCols)
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
      rm("fit")
      summaryDF[nrow(summaryDF) + 1, "Metric"] <- col
      tryCatch({
        form <- as.formula(paste0("as.numeric(", yColumn, ") ~ as.numeric(", col,")"))
        fit <- lm(form, datadf)
        summaryDF[nrow(summaryDF), "Correlation"] <- cor(as.numeric(tempDF[, col]), as.numeric(tempDF[, yColumn]), use = "pairwise.complete.obs")
        summaryDF[nrow(summaryDF), "DoF"] <- fit$df
      }, error = function(e) {NULL})
    }
    
    output$allCorrelations <- renderTable({
      summaryDF
    })
    
    ## By Date ##
    if(input$dateCol != "") {
      dateCorrelations <- data.frame(Metric = correlCols,
                                     "Total Periods" = integer(length(correlCols)),
                                     "Negative Periods" = integer(length(correlCols)),
                                     "Avg Correlation" = numeric(length(correlCols)),
                                     check.names = FALSE)
      # Fill in correlations by date
      for(date in unique(datadf[, input$dateCol])) {
        dateDF <- datadf[datadf[,input$dateCol]==date, ]
        for(col in correlCols) {
          dateCorrelations[dateCorrelations$Metric == col, date] <- cor(as.numeric(dateDF[, col]), as.numeric(dateDF[, yColumn]), use = "pairwise.complete.obs")
        }
      }
      
      # Fill in summary stats of date correlations
      for(col in correlCols) {
        metricCorrelations <- as.numeric(dateCorrelations[dateCorrelations$Metric == col, unique(datadf[, input$dateCol])])
        metricCorrelations <- metricCorrelations[!is.na(metricCorrelations)]
        dateCorrelations[dateCorrelations$Metric == col, "Total Periods"] <- length(metricCorrelations)
        dateCorrelations[dateCorrelations$Metric == col, "Negative Periods"] <- length(metricCorrelations[metricCorrelations < 0])
        dateCorrelations[dateCorrelations$Metric == col, "Avg Correlation"] <- mean(metricCorrelations, na.rm = TRUE)
      }
      
      output$dateCorrelations <- renderTable({
        dateCorrelations
      })
    }
    
    updateTabsetPanel(session, "mainTabset", selected="correlations")
    
  })
  
  # Upon selection of metric in Metric Dive tab
  observeEvent({
    c(input$xCol,input$run,input$applyFilters,input$filterClear)
    }, {
    if(input$xCol != ""){

      # Process Data
      df <- vals$datadf
      df <- subset(df, !is.na(df[,input$xCol]) & !is.na(df[,input$yCol]))
      
      if(nrow(df) == 0) {
        showNotification("No Data. Try selecting another column, or removing some filters on the Options page.",
                         type="warning",
                         duration=NULL)
      } else {
      
        # Scatter Plot
        xvar <- prop("x", as.symbol(input$xCol))
        yvar <- prop("y", as.symbol(input$yCol))
  
        if(input$categoryCol == "") {
          fillvar <- prop("fill", as.symbol(input$xCol))
          category <- "Data"
        } else {
          df[, input$categoryCol] <- as.factor(df[, input$categoryCol])
          fillvar <- prop("fill", as.symbol(input$categoryCol))
          category <- input$categoryCol
        }
        
        df %>%
          ggvis(x = xvar, y = yvar) %>%
          set_options(height = 480, width = 800) %>%
          layer_points(fill = fillvar) %>%
          add_tooltip(function(data){
            paste0(category, ": ", data[[category]], "<br>",
                   input$xCol, ": ", data[[input$xCol]], "<br>",
                   input$yCol, ": ", data[[input$yCol]], "<br>")
          }, "hover") %>%
          layer_model_predictions(model = "lm", se = TRUE) %>%
          hide_legend('fill') %>%
          bind_shiny("metricScatter")
        
        # Histogram of X values
        df %>%
          ggvis(xvar) %>%
          set_options(height = 480, width = 800) %>%
          layer_histograms(fill := "#f8f5f0") %>%
          bind_shiny("metricHist")
        
        # QQ Plot
        xyDF <- df[complete.cases(df[, c(input$xCol, input$yCol)]), c(input$xCol, input$yCol)]
        sortedX <- xyDF[order(xyDF[, input$xCol]), input$xCol]
        sortedY <- xyDF[order(xyDF[, input$yCol]), input$yCol]
        sortedDF <- data.frame(x = sortedX, y = sortedY)

        ggvis(sortedDF, ~x, ~y) %>%
          set_options(height = 480, width = 800) %>%
          layer_points() %>%
          bind_shiny("metricQQy")
          
        # ANOVA Output
        output$aovSummary = reactivePrint(function() {
          form <- as.formula(paste0("as.numeric(", input$yCol, ") ~ as.numeric(", input$xCol,")"))
          summary(lm(form, data = tempDF))
        })
        
      }
    }
  })
  
  #######################
  # Data Preview Screen #
  #######################
  output$dataPreview <- renderTable({
    
    for(col in c("hierCol", "yCol", "dateCol", "categoryCol", "ignoreCols")) {
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
