
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(XLConnect)
library(plotly)
library(dplyr)
library(reshape2)
library(quantmod)
options(shiny.deprecation.messages=FALSE)
options(stringsAsFactors = FALSE)


# Defin Functions
source("https://raw.githubusercontent.com/JeremyBowyer/Quintile-Function/master/Quintile_Function.R")
round20 <- function(x) {
  return(sprintf("%.20f", round(as.numeric(x), 20)))
}

script <- "

for(i = 2; i < $('#summaryTable th').length; i++) {
  colorTableByCol('summaryTable', i);
}

for(i = 7; i < $('#dateCorrelations th').length; i++) {
  colorTableByCol('dateCorrelations', i);
}

for(i = 0; i < $('#datePerformance th').length; i++) {
  colorTableByCol('datePerformance', i);
}

function colorTableByCol(tableid, colindex){

  var columnarray, maxval, minval, max, min, n;

  columnarray = [];
  $('#' + tableid + ' tr:not(:first)').each(function(){
    var val = parseFloat($(this).find('td').eq(colindex).text());
    if(val === val) {
      columnarray.push(val);
    }
  })
  
  maxval = Math.max(...columnarray);
  minval = Math.min(...columnarray);

  min = 0;
  if (minval > 0) {
    max = maxval;
  } else if (maxval < 0) {
    max = Math.abs(minval);
  } else {
    max = Math.max(Math.abs(maxval), Math.abs(minval));
  }

  n = max-min;
  
  $('#' + tableid + ' tr td:nth-child(' + (colindex + 1) + ')').each(function() {

    var val = parseFloat($(this).text());    
    var xr, xg, xb, yr, yg, yb;

    // Define the min color, which is white
    xr = 255; // Red value
    xg = 255; // Green value
    xb = 255; // Blue value

    // Define max color, depending on sign of val
    if (val >= 0) {
        
      // Green if val > 0, #2ca25f
      yr = 44; // Red value
      yg = 162; // Green value
      yb = 95; // Blue value

    } else {

      // Red if val < 0, #a12b2b
      yr = 161; // Red value
      yg = 43; // Green value
      yb = 43; // Blue value
    
      val = Math.abs(val);

    }

    // Find value's position relative to range
    var pos = ((val - min) / (n));
    
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
  vals <- reactiveValues(valueFilterCount = 0,
                         percentileFilterCount = 0,
                         dateFilterCount = 0,
                         transformationCount = 0,
                         transformColIndex = NULL,
                         offsetCount = 0,
                         offsetColIndex = NULL,
                         datadf = data.frame(),
                         originaldf = data.frame(),
                         metricdivedf = data.frame(),
                         dateFormat = "%m/%d/%Y")
  
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
  
  output$transformationsCheck <- reactive({
    return(vals$transformationCount > 0)
  })
  outputOptions(output, 'transformationsCheck', suspendWhenHidden=FALSE)  
  
  output$filtersCheck <- reactive({
    return(vals$valueFilterCount > 0 || vals$percentileFilterCount > 0 || vals$dateFilterCount > 0 )
  })
  outputOptions(output, 'filtersCheck', suspendWhenHidden=FALSE)
  
  output$offsetsCheck <- reactive({
    return(vals$offsetCount > 0)
  })
  outputOptions(output, 'offsetsCheck', suspendWhenHidden=FALSE)
  
  output$validX <- reactive({
    if(input$xCol != "" && input$yCol != "") {
      df <- vals$datadf
      df <- subset(df, !is.na(df[,input$xCol]) & !is.na(df[,input$yCol]))
      
      return(sum(!is.na(as.numeric(df[, input$xCol]))) != 0)
    }
  })
  outputOptions(output, 'validX', suspendWhenHidden=FALSE)
  
  output$pagefilter <- reactive({
    return(input$pageFilterCheck)
  })
  outputOptions(output, 'pagefilter', suspendWhenHidden=FALSE)
  
  output$dateColCheck <- reactive({
    return(input$dateCol != "")
  })
  outputOptions(output, 'dateColCheck', suspendWhenHidden=FALSE)
  
  output$catColCheck <- reactive({
    return(input$categoryCol != "")
  })
  outputOptions(output, 'catColCheck', suspendWhenHidden=FALSE)
  
  output$dateColCheck <- reactive({
    return(input$dateCol != "")
  })
  outputOptions(output, 'dateColCheck', suspendWhenHidden=FALSE)
  
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
      ui = tags$div(selectInput(paste0("valueFilter",vals$valueFilterCount), paste0("Filter ",vals$valueFilterCount), getCols()),
                    tags$div(textInput(paste0("valueFilter",vals$valueFilterCount, "Min"), "Min"), style="display:inline-block"),
                    tags$div(textInput(paste0("valueFilter",vals$valueFilterCount, "Max"), "Max"), style="display:inline-block"),
                    tags$div(tags$hr()), class="valueFilter")
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
      ui = tags$div(selectInput(paste0("percentileFilter",vals$percentileFilterCount), paste0("Filter ",vals$percentileFilterCount), getCols()),
                    tags$div(textInput(paste0("percentileFilter",vals$percentileFilterCount, "Min"), "Min"), style="display:inline-block"),
                    tags$div(textInput(paste0("percentileFilter",vals$percentileFilterCount, "Max"), "Max"), style="display:inline-block"),
                    tags$div(tags$hr()), class="percentileFilter")
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
      ui = tags$div(selectInput(paste0("dateFilter",vals$dateFilterCount), paste0("Filter ",vals$dateFilterCount), getCols()),
                    tags$div(textInput(paste0("dateFilter",vals$dateFilterCount, "Min"), "Min"), style="display:inline-block"),
                    tags$div(textInput(paste0("dateFilter",vals$dateFilterCount, "Max"), "Max"), style="display:inline-block"),
                    tags$div(textInput(paste0("dateFilter",vals$dateFilterCount, "Format"), "Format dates are in (see above for example formats).", "%m/%d/%Y"), style="display:inline-block"),
                    tags$div(tags$hr(), class="dateFilter"), class="dateFilter")
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
  
  # Add Transformation Button
  observeEvent(input$addTransformation, {
    
    vals$transformationCount <- vals$transformationCount + 1
    cnt <- vals$transformationCount
    
    insertUI(
      selector="#transformations",
      where="beforeEnd",
      ui = tags$div(radioButtons(paste0("transformType", cnt), "Select transformation", choices=list("Difference" = "diff",
                                                                                                     "Subtract Rolling Median" = "submedian",
                                                                                                     "Subtract Historical Median" = "subhistmedian",
                                                                                                     "% Change" = "perchg",
                                                                                                     "% Change from Median" = "perchgmedian",
                                                                                                     "% Change from Std" = "perchgstd")),
                    textInput(paste0("transformationSuffix", cnt), "Column Suffix Name", value = ""),
                    numericInput(paste0("transformationLag", cnt), "Select Lag", value = 1, min = 1), 
                    selectInput(paste0("transformCols", cnt), "Select columns to transform", choices=getCols(), multiple = TRUE),
                    selectInput(paste0("transformDateCol", cnt), "Select column to transform along (probably a date)", choices=getCols()),
                    selectInput(paste0("transformCategoryCol", cnt), "Select category columns to group by (optional)", choices=getCols(), multiple = TRUE),
                    tags$hr(), class="transformation")
    )
    
  })
  
  # Create Transformations Button
  observeEvent(input$applyTransformations, {
    df <- vals$datadf
    tcolIndex <- vals$transformColIndex
    if(!is.null(tcolIndex)){
      df <- df[, -tcolIndex]
    }
    
    firstCol <- ncol(df) + 1
    
    for (cnt in 1:vals$transformationCount) {
      
      # Grab values from current transformation request
      type <- input[[paste0("transformType", cnt)]]
      cols <- input[[paste0("transformCols", cnt)]]
      dateCol <- input[[paste0("transformDateCol", cnt)]]
      catCols <- input[[paste0("transformCategoryCol", cnt)]]
      lag <- input[[paste0("transformationLag", cnt)]]
      transformSuffix <- gsub(" ", ".", input[[paste0("transformationSuffix", cnt)]])
      
      # Assign transformation function based on type selcted
      switch(type,
             diff={
               transformFunc <- function(x, lag) { return( c(rep(NA, lag), diff(x, lag = lag)) ) }
              },
             submedian={
               transformFunc <- function(x, lag) {
                 n <- as.numeric(x)
                 m <- numeric()
                 
                 for (i in seq_along(n)) {
                   if ((i - lag) > 0) {
                     m[length(m)+1] <- n[i] - median(n[(i - lag):(i - 1)])
                   } else {
                     m[length(m)+1] <- NA
                   }
                 }
                 
                 return( m )
                 
               }
             },
             subhistmedian={
               transformFunc <- function(x, lag) {
                 n <- as.numeric(x)
                 m <- numeric()
                 
                 for (i in seq_along(n)) {
                     m[length(m)+1] <- n[i] - median(n[1:i], na.rm = TRUE)
                 }
                 
                 return( m )
                 
               }
             },
             perchg={
               transformFunc <- function(x, lag) { return( as.numeric(Delt(x, k = lag)) ) }
              },
             perchgmedian={
               transformFunc <- function(x, lag) {
                 
                 d <- c(rep(NA, lag), diff(x, lag = lag))
                 
                 m <- numeric()
                 
                 for (i in seq_along(d)) {
                   m[length(m)+1] <- d[i] / median(x[1:i], na.rm = TRUE)
                 }
                 
                 return( m )
                 
                }
              },
             perchgstd={
               transformFunc <- function(x, lag) {
                 d <- c(rep(NA, lag), diff(x, lag = lag))
                 
                 m <- numeric()
                 
                 for (i in seq_along(d)) {
                   m[length(m)+1] <- d[i] / sd(x[1:i], na.rm = TRUE)
                 }
                 
                 return( m )
                 }
              }
      )

      df[,dateCol] <- as.Date(df[, dateCol], format = vals$dateFormat)
      
      # Run transformation based on whether or not category column was selected.
      if(is.null(catCols)) {
        # No Category columns#
        
        # Order DF by date column
        df <- df[order(df[, dateCol]), ]
        for (col in cols){
          transformName <- paste(col, "_", transformSuffix)
          if(length(names(df)[names(df) == transformName]) > 0) {
            transformName = paste0(transformName, length(names(df)[names(df) == transformName]))
          }
          
          df[, transformName] <- transformFunc(as.numeric(df[, col]), lag)
        }
        
      } else {
        # Category columns #
        
        # Order by category cols then date col.
        # This step is needed to ensure unlisted aggregate data is in proper order
        df <- df[do.call(order, df[c(rev(catCols),dateCol)]), ] #rev() reverses the category column vector, because aggregate() sorts using last in first out
        groupList <- list()
        for (col in catCols){
          groupList[[col]] <- df[, col] 
        }
        
        for (col in cols){
          transformName <- paste0(col, "_", transformSuffix)
          if(length(names(df)[names(df) == transformName]) > 0) {
            transformName = paste0(transformName, length(names(df)[names(df) == transformName]))
          }
          df[, transformName] <- unlist(aggregate(df[,col], by=groupList, function(x) transformFunc(as.numeric(x), lag))[["x"]])
        }
        
        # reorder DF back to user-selected order
        df <- df[do.call(order, df[c(catCols,dateCol)]), ]
        
      }
      
    }
    vals$transformColIndex <- firstCol:ncol(df)
    df[,dateCol] <- as.character(format(df[,dateCol], vals$dateFormat))
    vals$datadf <- df
    
  })
  
  # Clear Transformations Button
  observeEvent(input$transformationsClear, {
    removeUI(".transformation", multiple = TRUE)
    tcolIndex <- vals$transformColIndex
    if(!is.null(tcolIndex)){
      vals$datadf <- vals$datadf[, -tcolIndex]
    }
    vals$transformationCount <- 0
    vals$transformColIndex <- NULL
  })
  
  
  # Add Offset Button
  observeEvent(input$addOffset, {
    
    vals$offsetCount <- vals$offsetCount + 1
    cnt <- vals$offsetCount
    
    insertUI(
      selector="#offsets",
      where="beforeEnd",
      ui = tags$div(radioButtons(paste0("offsetType", cnt), "Select Direction", choices=list("Forward" = "forward",
                                                                                             "Backward" = "backward")),
                    textInput(paste0("offsetSuffix", cnt), "Column Suffix Name", value = ""),
                    numericInput(paste0("offsetLag", cnt), "Select Lag", value = 1, min = 1), 
                    selectInput(paste0("offsetCols", cnt), "Select columns to offset", choices=getCols(), multiple = TRUE),
                    selectInput(paste0("offsetDateCol", cnt), "Select column to offset along (probably a date)", choices=getCols()),
                    selectInput(paste0("offsetCategoryCol", cnt), "Select category columns to group by (optional)", choices=getCols(), multiple = TRUE),
                    tags$hr(), class="offset")
    )
    
  })
  
  # Create Offsets Button
  observeEvent(input$applyOffsets, {
    df <- vals$datadf
    ocolIndex <- vals$offsetColIndex
    if(!is.null(ocolIndex)){
      df <- df[, -ocolIndex]
    }
    
    # variables used in creating offset column index
    firstCol <- ncol(df) + 1
    
    for (cnt in 1:vals$offsetCount) {
      # Grab values from current offset request
      type <- input[[paste0("offsetType", cnt)]]
      cols <- input[[paste0("offsetCols", cnt)]]
      dateCol <- input[[paste0("offsetDateCol", cnt)]]
      catCols <- input[[paste0("offsetCategoryCol", cnt)]]
      lag <- input[[paste0("offsetLag", cnt)]]
      offsetSuffix <- gsub(" ", ".", input[[paste0("offsetSuffix", cnt)]])
      
      # Assign offset function based on type selcted
      switch(type,
             forward={
               offsetFunc <- function(x, lag) { return( c(rep(NA, lag), x[1:(length(x) - lag)]) ) }
             },
             backward={
               offsetFunc <- function(x, lag) { return( c(x[(lag + 1):length(x)], rep(NA, lag)) ) }
             }
      )
      
      df[,dateCol] <- as.Date(df[, dateCol], format = vals$dateFormat)
      
      # Run offset based on whether or not category column was selected.
      if(is.null(catCols)) {
        # No Category columns#
        
        # Order DF by date column
        df <- df[order(df[, dateCol]), ]
        for (col in cols){
          offsetName <- paste0(col, "_", offsetSuffix)
          if(length(names(df)[names(df) == offsetName]) > 0) {
            offsetName = paste0(offsetName, length(names(df)[names(df) == offsetName]))
          }
          df[, paste0(col, offsetName)] <- offsetFunc(df[, col], lag)
        }
        
      } else {
        # Category columns #
        
        # Order by category cols then date col.
        # This step is needed to ensure unlisted aggregate data is in proper order
        df <- df[do.call(order, df[c(rev(catCols),dateCol)]), ] #rev() reverses the category column vector, because aggregate() sorts using last in first out
        groupList <- list()
        for (col in catCols){
          groupList[[col]] <- df[, col] 
        }
        
        for (col in cols){
          offsetName <- paste0(col, "_", offsetSuffix)
          if(length(names(df)[names(df) == offsetName]) > 0) {
            offsetName = paste0(offsetName, length(names(df)[names(df) == offsetName]))
          }
          df[, offsetName] <- unlist(aggregate(df[,col], by=groupList, function(x) offsetFunc(x, lag))[["x"]])
        }
        
        # reorder DF back to user-selected order
        df <- df[do.call(order, df[c(catCols,dateCol)]), ]
        
      }
      
    }
    vals$offsetColIndex <- firstCol:ncol(df)
    df[,dateCol] <- as.character(format(df[,dateCol], vals$dateFormat))
    vals$datadf <- df
    
  })
  
  # Clear Offsets Button
  observeEvent(input$offsetClear, {
    removeUI(".offset", multiple = TRUE)
    ocolIndex <- vals$offsetColIndex
    if(!is.null(ocolIndex)){
      vals$datadf <- vals$datadf[, -ocolIndex]
    }
    vals$offsetCount <- 0
    vals$offsetColIndex <- NULL
  })
  
  
  # Run Analysis Button
  observeEvent(input$run, {
    
    if(input$yCol == "") {
      showNotification("Select a Y column.",
                       type="error",
                       duration=NULL)
      return(NULL)
    }
    
    vals$dateFormat <- input$dateColFormat
    
    # Read in user-provided CSV file
    datadf <- vals$datadf
    
    # Y Column
    if(input$hierBox){
      
      datadf[, hierCol] <- as.numeric(datadf[, hierCol])
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
                            'Turnover (max 0.57)' = character(),
                            Correlation = numeric(),
                            DoF = integer(),
                            check.names = FALSE)
    
    for(col in correlCols) {
      rm("fit")
      summaryDF[nrow(summaryDF) + 1, "Metric"] <- col
      
      tryCatch({
        metricDF <- datadf[order(datadf[,input$dateCol]) ,c(input$dateCol, input$categoryCol, col)]
        wideMetricDF <- dcast(metricDF,as.formula(paste0(input$dateCol," ~ ",input$categoryCol)), value.var = col)[, -1]
        rankDF <- t(apply(wideMetricDF, 1, function(x) rank(x, na.last = "keep") / length(which(!is.na(x))) ))
        diffRankDF <- apply(rankDF, 2, function(x) c(NA, diff(x)))
        stds <- apply(diffRankDF, 1, function(x) sd(x, na.rm = TRUE))
        summaryDF[nrow(summaryDF), "Turnover (max 0.57)"] <- round(mean(stds, na.rm = TRUE), 2)
      }, error = function(e) {NULL})
      
      tryCatch({
        form <- as.formula(paste0("as.numeric(", yColumn, ") ~ as.numeric(", col,")"))
        fit <- lm(form, datadf)
        summaryDF[nrow(summaryDF), "Correlation"] <- cor(as.numeric(datadf[, col]), as.numeric(datadf[, yColumn]), use = "pairwise.complete.obs")
        summaryDF[nrow(summaryDF), "DoF"] <- fit$df
      }, error = function(e) {NULL})
    }
    
    output$summaryTable <- renderTable({
      summaryDF
    })
    
    ## By Date ##
    if(input$dateCol != "") {
      dateCorrelations <- data.frame(Metric = correlCols,
                                     "Total Periods" = integer(length(correlCols)),
                                     "Negative Periods" = integer(length(correlCols)),
                                     "Positive Periods" = integer(length(correlCols)),
                                     "% Negative" = numeric(length(correlCols)),
                                     "% Positive" = numeric(length(correlCols)),
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
        dateCorrelations[dateCorrelations$Metric == col, "Positive Periods"] <- length(metricCorrelations[metricCorrelations > 0])
        dateCorrelations[dateCorrelations$Metric == col, "% Negative"] <- length(metricCorrelations[metricCorrelations < 0]) / length(metricCorrelations)
        dateCorrelations[dateCorrelations$Metric == col, "% Positive"] <- length(metricCorrelations[metricCorrelations > 0]) / length(metricCorrelations)
        dateCorrelations[dateCorrelations$Metric == col, "Avg Correlation"] <- mean(metricCorrelations, na.rm = TRUE)
      }
      
      output$dateCorrelations <- renderTable({
        dateCorrelations
      })
    }
    
    updateTabsetPanel(session, "mainTabset", selected="correlations")
    
  })
  
  output$downloadData <- downloadHandler(
    filename = function(){"custom_data.xlsx"},
    content = function(file) {
      fname <- paste(file,"xlsx",sep=".")
      wb <- loadWorkbook(fname,create = TRUE)
      createSheet(wb,"data")
      writeWorksheet(wb,data = vals$datadf, sheet = "data")
      saveWorkbook(wb)
      file.rename(fname,file)
    },
    contentType="application/xlsx" 
  )
  
  
  # Page Filter
  observeEvent(input$metricDiveFilterDate, {
  
    if(input$pageFilterCheck){
      
      df <- vals$originalmetricdivedf
      
      df <- df[df[,input$dateCol] == input$metricDiveFilterDate, ]
      
      vals$metricdivedf <- df
      
    }
    
  })
  
  observeEvent(input$pageFilterCheck, {
   
    if(!input$pageFilterCheck) {
      vals$metricdivedf <- vals$originalmetricdivedf
    }
     
  })
  
  observeEvent(input$pageBack, {
    
    df <- vals$originalmetricdivedf
    metricDates <- unique(df[order(df[,input$dateCol]), input$dateCol])
    currentdate <- input$metricDiveFilterDate
    currentindex <- which(metricDates %in% currentdate)
    index <- if (currentindex <= 1) currentindex else currentindex - 1
    updateSelectInput(session, 'metricDiveFilterDate', choices=metricDates, selected=metricDates[index])
    
  })
  
  observeEvent(input$pageForward, {
    
    df <- vals$originalmetricdivedf
    metricDates <- unique(df[order(df[,input$dateCol]), input$dateCol])
    currentdate <- input$metricDiveFilterDate
    currentindex <- which(metricDates %in% currentdate)
    index <- if (currentindex >= length(metricDates)) currentindex else currentindex + 1
    updateSelectInput(session, 'metricDiveFilterDate', choices=metricDates, selected=metricDates[index])
    
  })
  
  # Upon selection of metric in Metric Dive tab
  observeEvent({
    c(input$xCol,input$run,input$applyFilters,input$filterClear)
    }, {
    if(input$xCol != ""){
      
      # Process Data
      df <- vals$datadf
      df[,input$xCol] <- as.numeric(df[,input$xCol])
      df[,input$yCol] <- as.numeric(df[,input$yCol])
      df <- subset(df, !is.na(df[,input$xCol]) & !is.na(df[,input$yCol]))
      df <- subset(df, !is.infinite(df[,input$xCol]) & !is.infinite(df[,input$yCol]))
      df <- subset(df, !is.nan(df[,input$xCol]) & !is.nan(df[,input$yCol]))
      
      vals$metricdivedf <- df
      vals$originalmetricdivedf <- df
      
      if(input$dateCol != ""){
        metricDates <- unique(df[order(df[,input$dateCol]), input$dateCol])
        updateSelectInput(session, 'metricDiveFilterDate', choices=metricDates, selected=metricDates[1])
      }
      
      # Check for valid data, otherwise show error notification to user
      if(nrow(df) == 0) {
        # showNotification("No Data. Try selecting another column, or removing some filters on the Options page.",
        #                  type="error",
        #                  duration=NULL)
      } else if(sum(!is.na(as.numeric(df[, input$xCol]))) == 0) {
        removeUI(".metricAlert", multiple = TRUE)
        insertUI(
          selector="#metricAlertDiv",
          where="beforeEnd",
          ui = tags$p("Column has no data after being coerced to numeric.", style="color: red;", class="metricAlert")
        )
        
      } else {
        removeUI(".metricAlert", multiple = TRUE)
        
        # Performance Output
        output$datePerformance = renderTable({
          
          df <- vals$metricdivedf[,c(input$yCol, input$xCol, input$dateCol)]
          df <- df[complete.cases(df), ]
          df[, input$dateCol] <- as.Date(df[, input$dateCol], format = vals$dateFormat)
          
          # Create quintiles by date
          df[,"quints"] <- NA
          
          aggs <- by(df, INDICES = list(df[, input$dateCol]), function(x) {
            tryCatch({
              x[,"quints"] <- quint(as.numeric(x[,input$xCol]))
            }, error = function(e) {
              x[,"quints"] <- rep(NA, nrow(x))
            })
            x
          })

          df <- do.call("rbind", aggs)
          
          # Create performance DF
          allPerformance <- data.frame(Quintile = c("Q1 (Highest)", "Q2", "Q3", "Q4", "Q5 (Lowest)"))
          
          # Populate performance by quintile for All dates
          allPerformance[,"All"] <- aggregate(df[, input$yCol], by = list(df$quints), function(x) mean(x, na.rm = TRUE))["x"]
          
          # Calculate performance by quintile for each date, populate performance df
          if(input$dateCol != "") {
            for(date in unique(as.character(df[, input$dateCol]))) {
              datedf <- df[df[,input$dateCol] == date, ]
              allPerformance[, date] <- NA
              
              tryCatch({
                datedf$quints <- quint(datedf[,input$xCol])
                aggs <- aggregate(datedf[, input$yCol], by = list(datedf$quints), function(x) mean(x, na.rm = TRUE))
                for(row in 1:nrow(aggs)){
                  allPerformance[aggs[row, 1], date] <- aggs[row, "x"]
                }
              }, error = function(e) {
                allPerformance[, date] <- rep(NA, 5)
              })
              
            }
          }
          
          return(allPerformance)
          
        })
        
      }
    }
  })
  
  ##################
  # Reactive plots #
  ##################
  # Metric Scatter
  output$metricScatter <- renderPlotly({
    
    df <- vals$metricdivedf
    if (input$dateCol != ""){
      df$text <- paste0("date: ", df[,input$dateCol])
    } else {
      df$text <- ""
    }
    xform <- as.formula(paste0("~",input$xCol))
    yform <- as.formula(paste0("~",input$yCol))
    
    colorcol <- if(input$categoryCol != "") input$categoryCol else input$xCol
    colorform <- as.formula(paste0("~", colorcol))
    
    form <- as.formula(paste0(input$yCol, "~", input$xCol))
    fit <- lm(form, data = df)
    df %>%
      plot_ly(x = xform, source = "metricScatter") %>%
      add_markers(y = yform, color = colorform, text = ~text)  %>%
      add_lines(x = xform, y = fitted(fit), fill = "red", name = "Regression Line")
    
  })
  
  
  observeEvent({input$includePoints}, {
    
    event.data <- event_data("plotly_selected", source = "metricScatter")
    if(is.null(event.data) == TRUE) return(NULL)
    
    df <- vals$metricdivedf
    
    mod <- 1
    
    if (input$categoryCol != ""){
      df$CatFactor <- as.numeric(as.factor(df[, input$categoryCol]))
      aggs <- by(df, INDICES = list(df[, "CatFactor"]), function(x) {
        
        cat.index <- event.data[event.data$curveNumber == x[1,"CatFactor"] - 1, "pointNumber"] + 1
        x[cat.index*mod, ]
        
      })
      df <- do.call("rbind", aggs)
    } else {
      df <- df[(event.data$pointNumber + 1)*mod, ]
    }
    vals$metricdivedf <- df
    
  })
  
  observeEvent({input$excludePoints}, {
    
    event.data <- event_data("plotly_selected", source = "metricScatter")
    if(is.null(event.data) == TRUE) return(NULL)
    
    df <- vals$metricdivedf
    
    mod <- -1
    
    if (input$categoryCol != ""){
      df$CatFactor <- as.numeric(as.factor(df[, input$categoryCol]))
      aggs <- by(df, INDICES = list(df[, "CatFactor"]), function(x) {
        
        cat.index <- event.data[event.data$curveNumber == x[1,"CatFactor"] - 1, "pointNumber"] + 1
        x[cat.index*mod, ]
        
      })
      df <- do.call("rbind", aggs)
    } else {
      df <- df[(event.data$pointNumber + 1)*mod, ]
    }
    vals$metricdivedf <- df
    
  })
  
  # Metric Histogram
  output$metricHist <- renderPlotly({
    
    df <- vals$metricdivedf
    xform <- as.formula(paste0("~",input$xCol))
    plot_ly(data = df, x = xform, type = "histogram")
    
  })
  
  # Metric QQ Normal Dist
  output$metricQQNorm <- renderPlotly({
    
    df <- vals$metricdivedf
    
    xyDF <- df[complete.cases(df[, c(input$xCol, input$yCol)]), c(input$xCol, input$yCol)]
    sortedX <- xyDF[order(xyDF[, input$xCol]), input$xCol]
    sortedY <- xyDF[order(xyDF[, input$yCol]), input$yCol]
    xNorm <- qnorm(c(1:nrow(xyDF)) / nrow(xyDF), mean(xyDF[,input$xCol], na.rm = TRUE), sd(xyDF[,input$xCol], na.rm = TRUE))
    xNorm <- replace(xNorm, is.infinite(xNorm), NA)
    sortedDF <- data.frame(x = sortedX, y = sortedY, xNorm = xNorm)
  
    plot_ly(data = sortedDF, x = ~x, y = ~xNorm)
    
  })
  
  # Metric QQ Y
  output$metricQQy <- renderPlotly({
    
    df <- vals$metricdivedf
    
    xyDF <- df[complete.cases(df[, c(input$xCol, input$yCol)]), c(input$xCol, input$yCol)]
    sortedX <- xyDF[order(xyDF[, input$xCol]), input$xCol]
    sortedY <- xyDF[order(xyDF[, input$yCol]), input$yCol]
    xNorm <- qnorm(c(1:nrow(xyDF)) / nrow(xyDF), mean(xyDF[,input$xCol], na.rm = TRUE), sd(xyDF[,input$xCol], na.rm = TRUE))
    xNorm <- replace(xNorm, is.infinite(xNorm), NA)
    sortedDF <- data.frame(x = sortedX, y = sortedY, xNorm = xNorm)
    
    plot_ly(data = sortedDF, x = ~x, y = ~y)
    
  })
  
  # Metric Turnover
  output$metricTurnover <- renderPlotly({

    if(input$dateCol != "") {
      df <- vals$originalmetricdivedf
      metricDF <- df[order(df[,input$dateCol]) ,c(input$dateCol, input$categoryCol, input$xCol)]
      wideMetricDF <- dcast(metricDF,as.formula(paste0(input$dateCol," ~ ",input$categoryCol)), value.var = input$xCol)[, -1]
      rankDF <- t(apply(wideMetricDF, 1, function(x) rank(x, na.last = "keep") / length(which(!is.na(x))) ))
      diffRankDF <- apply(rankDF, 2, function(x) c(NA, diff(x)))
      stds <- apply(diffRankDF, 1, function(x) sd(x, na.rm = TRUE))
      dates <- as.Date(unique(metricDF[,"Date"]), format = vals$dateFormat)
      stdDF <- data.frame(date = dates, std = stds)
      stdDF <- stdDF[order(stdDF$date), ]
      plot_ly(data = stdDF, x = ~date, y = ~std, type = 'scatter', mode = 'lines')
    }

  })
  
  # ANOVA
  output$aovSummary = reactivePrint(function() {
    df <- vals$metricdivedf
    form <- as.formula(paste0("as.numeric(", input$yCol, ") ~ as.numeric(", input$xCol,")"))
    summary(lm(form, data = df))
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
