
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

source("global.R", local=TRUE)
source("filters.R", local=TRUE)
source("transformations.R", local=TRUE)
source("offsets.R", local=TRUE)

# Define Functions
source("https://raw.githubusercontent.com/JeremyBowyer/Quintile-Function/master/Quintile_Function.R")

script <- readChar('highlight_cells.js', file.info('highlight_cells.js')$size)

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
  vals$getCols <- function(){
    df = vals$datadf
    return(names(df))
  }
  
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
  
  output$pageFilterCheck <- reactive({
    return(input$pageFilterCheck)
  })
  outputOptions(output, 'pageFilterCheck', suspendWhenHidden=FALSE)
  
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
  
  output$pointFilterCheck <- reactive({
    return(input$pointFilterCheck)
  })
  outputOptions(output,'pointFilterCheck', suspendWhenHidden=FALSE)
  
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
  
  # Update metric dive Y column indicator on Y change
  output$currentY_dive <- renderText({ 
    paste0("Current Y column: ", input$yCol)
  })
  
  
  ##################
  # Filter Section #
  ##################
  # Add filter Buttons
  observeAddFilter(input, output, session, vals)
  # Clear Filters Button
  observeApplyFilters(input, output, session, vals)
  # Apply Filters Button
  observeClearFilters(input, output, session, vals)
  
  ##########################
  # Transformation Section #
  ##########################
  # Add Transformation Button
  observeAddTransformation(input, output, session, vals)
  # Create Transformations Button
  observeCreateTransformations(input, output, session, vals)
  # Clear Transformations Button
  observeClearTransformations(input, output, session, vals)
  
  ##################
  # Offset Section #
  ##################
  # Add Offset Button
  observeAddOffset(input, output, session, vals)
  # Create Offsets Button
  observeApplyOffsets(input, output, session, vals)
  # Clear Offsets Button
  observeClearOffsets(input, output, session, vals)
  
  
  # Run Analysis Button
  observeEvent(input$run, {
    
    if(input$yCol == "") {
      showNotification("Select a Y column.",
                       type="error",
                       duration=NULL)
      return(NULL)
    }
    
    # Store date format
    vals$dateFormat <- input$dateColFormat
    
    # Update Y column indicator
    output$currentY_comparison <- renderText( 
      isolate(paste0("Current Y column: ", input$yCol))
    )
    
    # Read in user-provided CSV file
    datadf <- vals$datadf
    
    # Y Column
    if(input$hierBox){
      
      datadf[, input$hierCol] <- as.numeric(datadf[, input$hierCol])
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
    correlCols = unique(names(datadf)[!names(datadf) %in% ignoreCols])
    
    # Create vector of multi-linear columns
    multiCols = input$multiCols
    
    # Update Metric Dive Dropdown
    updateSelectInput(session, "xCol", choices=correlCols)
    
    ## All Data Points ##
    # Loop through each metric column, run regression, populate summary table
    summaryDF <- data.frame(Metric = character(),
                            'Rank Volatility (max 0.57)' = character(),
                            Correlation = numeric(),
                            DoF = integer(),
                            check.names = FALSE)
    
    for(col in correlCols) {
      if(exists("fit")) { rm("fit") }
      summaryDF[nrow(summaryDF) + 1, "Metric"] <- col
      
      # Rank Volatility
      tryCatch({
        metricDF <- datadf[order(datadf[,input$dateCol]) ,c(input$dateCol, input$categoryCol, col)]
        wideMetricDF <- dcast(metricDF,as.formula(paste0(input$dateCol," ~ ",input$categoryCol)), value.var = col)[, -1]
        rankDF <- t(apply(wideMetricDF, 1, function(x) rank(x, na.last = "keep") / length(which(!is.na(x))) ))
        diffRankDF <- apply(rankDF, 2, function(x) c(NA, diff(x)))
        stds <- apply(diffRankDF, 1, function(x) sd(x, na.rm = TRUE))
        summaryDF[nrow(summaryDF), "Rank Volatility (max 0.57)"] <- round(mean(stds, na.rm = TRUE), 2)
      }, error = function(e) {NULL})
      
      # Correlation and DoF
      tryCatch({
        form <- as.formula(paste0("as.numeric(", yColumn, ") ~ as.numeric(", col,")"))
        fit <- lm(form, datadf)
        summaryDF[nrow(summaryDF), "Correlation"] <- cor(as.numeric(datadf[, col]), as.numeric(datadf[, yColumn]), use = "pairwise.complete.obs")
        summaryDF[nrow(summaryDF), "DoF"] <- fit$df
      }, error = function(e) {NULL})
    }
    
    # Multi-linear
    if(exists("fit")) { rm("fit") }
    summaryDF[nrow(summaryDF) + 1, "Metric"] <- "Multilinear"
    
    formstring <- paste0("as.numeric(", yColumn, ") ~ ")
    for (col in multiCols){
      if(col == multiCols[length(multiCols)]) {
        formstring <- paste0(formstring, " as.numeric(", col, ")")
      } else {
        formstring <- paste0(formstring, " as.numeric(", col, ") +")
      }
    }
    
    tryCatch({
      form <- as.formula(formstring)
      fit <- lm(form, datadf)
      datadf[names(fit$fitted.values), "fitted"] <- fit$fitted.values
      summaryDF[nrow(summaryDF), "Correlation"] <- cor(as.numeric(datadf$fitted), as.numeric(datadf[, yColumn]), use = "pairwise.complete.obs")
      summaryDF[nrow(summaryDF), "DoF"] <- fit$df
    }, error = function(e) {NULL})
    
    output$summaryTable <- renderTable({
      summaryDF
    }, hover = TRUE, bordered = TRUE)
    
    ## By Date ##
    if(input$dateCol != "") {
      dateCorrelations <- data.frame(Metric = c(correlCols, "Multilinear"),
                                     "Total Periods" = integer(length(c(correlCols, "Multilinear"))),
                                     "Negative Periods" = integer(length(c(correlCols, "Multilinear"))),
                                     "Positive Periods" = integer(length(c(correlCols, "Multilinear"))),
                                     "% Negative" = numeric(length(c(correlCols, "Multilinear"))),
                                     "% Positive" = numeric(length(c(correlCols, "Multilinear"))),
                                     "Avg Correlation" = numeric(length(c(correlCols, "Multilinear"))),
                                     check.names = FALSE)
      # Fill in correlations by date
      for(date in unique(datadf[, input$dateCol])) {
        dateDF <- datadf[datadf[,input$dateCol]==date, ]
        
        # single factor regressions
        for(col in correlCols) {
          dateCorrelations[dateCorrelations$Metric == col, date] <- cor(as.numeric(dateDF[, col]), as.numeric(dateDF[, yColumn]), use = "pairwise.complete.obs")
        }
        
        # multi-linear regression
        if(exists("fit")) { rm("fit") }
        
        formstring <- paste0("as.numeric(", yColumn, ") ~ ")
        for (col in multiCols){
          if(col == multiCols[length(multiCols)]) {
            formstring <- paste0(formstring, " as.numeric(", col, ")")
          } else {
            formstring <- paste0(formstring, " as.numeric(", col, ") +")
          }
        }
        
        tryCatch({
          form <- as.formula(formstring)
          fit <- lm(form, dateDF)
          dateDF[names(fit$fitted.values), "fitted"] <- fit$fitted.values
          dateCorrelations[dateCorrelations$Metric == "Multilinear", date] <- cor(as.numeric(dateDF$fitted), as.numeric(dateDF[, yColumn]), use = "pairwise.complete.obs")
        }, error = function(e) {NULL})
        
      }
      
      # Fill in summary stats of date correlations
      for(col in c(correlCols,"Multilinear")) {
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
      }, hover = TRUE, bordered = TRUE)
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
  
  observeEvent(input$pointFilterCheck, {
    
    if(!input$pointFilterCheck) {
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
      add_lines(x = xform, y = fitted(fit), fill = "red", name = "Regression Line") %>%
      layout(dragmode = "lasso")
    
  })
  
  observeEvent({input$clearPoints}, {
    # Process Data
    df <- vals$datadf
    df[,input$xCol] <- as.numeric(df[,input$xCol])
    df[,input$yCol] <- as.numeric(df[,input$yCol])
    df <- subset(df, !is.na(df[,input$xCol]) & !is.na(df[,input$yCol]))
    df <- subset(df, !is.infinite(df[,input$xCol]) & !is.infinite(df[,input$yCol]))
    df <- subset(df, !is.nan(df[,input$xCol]) & !is.nan(df[,input$yCol]))
    
    vals$metricdivedf <- df  
  })
  
  observeEvent({input$keepPoints}, {
    
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
  
  observeEvent({input$removePoints}, {
    
    event.data <- event_data("plotly_selected", source = "metricScatter")
    if(is.null(event.data) == TRUE) return(NULL)
    
    df <- vals$metricdivedf
    
    mod <- -1
    
    if (input$categoryCol != ""){
      df$CatFactor <- as.numeric(as.factor(df[, input$categoryCol]))
      aggs <- by(df, INDICES = list(df[, "CatFactor"]), function(x) {
        
        cat.index <- event.data[event.data$curveNumber == x[1,"CatFactor"] - 1, "pointNumber"] + 1
        if(length(cat.index) > 0){
          x[cat.index*mod, ]
        } else {
          x
        }
        
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
  
  # Metric Rank Volatility
  output$metricRankVolatility <- renderPlotly({
    
    if(input$dateCol != "") {
      df <- vals$originalmetricdivedf
      metricDF <- df[order(df[,input$dateCol]) ,c(input$dateCol, input$categoryCol, input$xCol)]
      wideMetricDF <- dcast(metricDF,as.formula(paste0(input$dateCol," ~ ",input$categoryCol)), value.var = input$xCol)[, -1]
      rankDF <- t(apply(wideMetricDF, 1, function(x) rank(x, na.last = "keep") / length(which(!is.na(x))) ))
      diffRankDF <- apply(rankDF, 2, function(x) c(NA, diff(x)))
      stds <- apply(diffRankDF, 1, function(x) sd(x, na.rm = TRUE))
      dates <- as.Date(unique(metricDF[,"Date"]))
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
    
    for(col in c("hierCol", "yCol", "dateCol", "categoryCol", "ignoreCols", "multiCols")) {
      updateSelectInput(session, col, choices=vals$getCols(), selected=input[[col]])
    }
    
    return(vals$datadf)
    
  }, hover = TRUE, bordered = TRUE)
  
  #####################################
  # JavaScript Conditional Formatting #
  #####################################
  session$onFlushed(function() {
    session$sendCustomMessage(type='jsCode', list(value = script))
  }, FALSE)
  
})