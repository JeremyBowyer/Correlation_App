observeMetricDiveFilters <- function(input, output, session, vals) {
  
  ## Page Filter ##
  observeEvent(input$pageFilterCheck, {
    
    if(input$pageFilterCheck){
      
      vals$dateFilterdf <- data.frame(Date = unique(vals$metricdivedf[, input$dateCol]))
      output$pageFilterTable <- renderDT(vals$dateFilterdf,
                                         options = list(
                                           pageLength = 100,
                                           paging = FALSE,
                                           info = FALSE,
                                           searching = FALSE
                                           ),
                                         rownames = FALSE,
                                         selection = "multiple",
                                         filter = "none",
                                         style = "bootstrap",
                                         autoHideNavigation = TRUE)
     
      vals$metricdivedfPage <- if(input$pointFilterCheck) vals$metricdivedfPoint  else vals$originalmetricdivedf
      
    }
    
    if(!input$pageFilterCheck && !input$pointFilterCheck) {
      vals$metricdivedf <- vals$originalmetricdivedf
      vals$metricdivedfPage <- data.frame()
      return(NULL)
    }
    
    if(!input$pageFilterCheck && input$pointFilterCheck) {
      vals$metricdivedf <- vals$metricdivedfPoint
      vals$metricdivedfPage <- data.frame()
      return(NULL)
    }
    
    
  })
  
  observeEvent(input$pageFilterTable_rows_selected, {

      if(length(input$pageFilterTable_rows_selected) == 0) {
        vals$metricdivedf <- vals$originalmetricdivedf
        return(NULL)
      }
    
      selectedDates <- vals$dateFilterdf[input$pageFilterTable_rows_selected, "Date"]
      
      df <- if(input$pointFilterCheck) vals$metricdivedfPoint else vals$originalmetricdivedf
      df <- df[df[,input$dateCol] %in% selectedDates, ]
      vals$metricdivedf <- df
      vals$metricdivedfPage <- df
  })
  
  ## Point Filter ##
  observeEvent(input$pointFilterCheck, {
    
    vals$metricdivedfPoint <- if(input$pageFilterCheck) vals$metricdivedfPage  else vals$originalmetricdivedf
    
    if(!input$pointFilterCheck && !input$pageFilterCheck) {
      vals$metricdivedf <- vals$originalmetricdivedf
      vals$metricdivedfPoint <- data.frame()
      return(NULL)
    }
    
    if(!input$pointFilterCheck && input$pageFilterCheck) {
      vals$metricdivedf <- vals$metricdivedfPage
      vals$metricdivedfPoint <- data.frame()
      return(NULL)
    }
    
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
    vals$metricdivedfPoint <- df
    
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
    vals$metricdivedfPoint <- df
    
  })
  
}

metricDivePlots <- function(input, output, session, vals) {
  
  ##################
  # Reactive plots #
  ##################
  # Metric Scatter
  output$metricScatter <- renderPlotly({

    if(input$xCol == "" || input$yCol == "" || nrow(vals$metricdivedf) > MAX_ROW_LIMIT)  {
      return(NULL)
    }

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
    p <- df %>%
      plot_ly(x = xform, source = "metricScatter") %>%
      add_markers(y = yform, color = colorform, text = ~text)  %>%
      add_lines(x = xform, y = fitted(fit), fill = "red", name = "Regression Line") %>%
      layout(dragmode = "lasso")
    p
  })
  
  output$staticScatter <- renderPlot({

    if(input$xCol == "" || input$yCol == "" || nrow(vals$metricdivedf) <= MAX_ROW_LIMIT)  {
      return(NULL)
    }

    df <- vals$metricdivedf
    x <- df[,input$xCol]
    y <- df[,input$yCol]

    plot(x, y, xlab=input$xCol, ylab=input$yCol)
    abline(lm(y ~ x))

  })
  
  # Metric Histogram
  output$metricHist <- renderPlotly({
    
    if(input$xCol == "" || input$yCol == "") {
      return(NULL)
    }
    
    df <- vals$metricdivedf
    xform <- as.formula(paste0("~",input$xCol))
    plot_ly(data = df, x = xform, type = "histogram")
    
  })
  
  # Metric Data Points
  # By Date
  output$metricDataPointsDate <- renderPlotly({
    
    if(input$xCol == "" || input$dateCol == "" || input$yCol == "") {
      return(NULL)
    }
    
    metricDF <- vals$datadf
    dataPointsDF <- aggregate(metricDF[, input$xCol], by=list(metricDF[, input$dateCol]), function(x) sum(!is.na(as.numeric(x))))
    names(dataPointsDF) <- c("Date", "DataPoints")
   # dataPointsDF$Date <- as.Date(as.character(dataPointsDF$Date), format = vals$dateFormat)
    dataPointsDF$Date <- parse_date_time(dataPointsDF$Date,order=vals$dateFormat)
    dataPointsDF <- dataPointsDF[order(dataPointsDF$Date), ]
    ##dataPointsDF$Date <- format(dataPointsDF$Date,vals$dateFormat)
    
    plot_ly(data = dataPointsDF, x = ~Date, y = ~DataPoints, type = 'scatter', mode = 'lines')
    
  })
  # By Category
  output$metricDataPointsCategory <- renderPlotly({
    
    if(input$xCol == "" || input$categoryCol == "" || input$yCol == "") {
      return(NULL)
    }
    
    metricDF <- vals$datadf
    dataPointsDF <- aggregate(metricDF[, input$xCol], by=list(metricDF[, input$categoryCol]), function(x) sum(!is.na(as.numeric(x))))
    names(dataPointsDF) <- c("Category", "DataPoints")
    dataPointsDF <- dataPointsDF[order(dataPointsDF$Category), ]
    plot_ly(data = dataPointsDF, x = ~Category, y = ~DataPoints, type = 'bar')
    
  })
  
  # Metric QQ Normal Dist
  output$metricQQNorm <- renderPlotly({
    
    if(input$xCol == "" || input$yCol == "") {
      return(NULL)
    }
    
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
    
    if(input$xCol == "" || input$yCol == "") {
      return(NULL)
    }
    
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
    
    if(input$xCol == "" || input$yCol == "" || input$dateCol == "") {
      return(NULL)
    }
    
    df <- vals$originalmetricdivedf
    metricDF <- df[order(df[,input$dateCol]) ,c(input$dateCol, input$categoryCol, input$xCol)]
    wideMetricDF <- dcast(metricDF,as.formula(paste0(input$dateCol," ~ ",input$categoryCol)), value.var = input$xCol)[, -1]
    rankDF <- t(apply(wideMetricDF, 1, function(x) rank(x, na.last = "keep") / length(which(!is.na(x))) ))
    diffRankDF <- apply(rankDF, 2, function(x) c(NA, diff(x)))
    stds <- apply(diffRankDF, 1, function(x) sd(x, na.rm = TRUE))
    dates <- parse_date_time(unique(as.character(metricDF[,input$dateCol])), order = vals$dateFormat)
    stdDF <- data.frame(date = dates, std = stds)
    stdDF <- stdDF[order(stdDF$date), ]
    plot_ly(data = stdDF, x = ~date, y = ~std, type = 'scatter', mode = 'lines')
    
  })
  
  # ANOVA
  output$aovSummary = reactivePrint(function() {
    
    if(input$xCol == "" || input$yCol == "") {
      return(NULL)
    }
    
    df <- vals$metricdivedf
    form <- as.formula(paste0("as.numeric(", input$yCol, ") ~ as.numeric(", input$xCol,")"))
    summary(lm(form, data = df))
  })
  
}

processMetricDiveDF <- function(input, output, session, vals) {
  
    # Upon selection of metric in Metric Dive tab, or run analysis button
    observeEvent({
      c(input$xCol,input$run)
    }, {
    
      if(input$xCol == "" || input$yCol == ""){
        return(NULL)  
      }
        
      # Process Data
      df <- vals$datadf
      
      df[,input$xCol] <- as.numeric(df[,input$xCol])
      df[,input$yCol] <- as.numeric(df[,input$yCol])
      df <- subset(df, !is.na(df[,input$xCol]) & !is.na(df[,input$yCol]))
      df <- subset(df, !is.infinite(df[,input$xCol]) & !is.infinite(df[,input$yCol]))
      df <- subset(df, !is.nan(df[,input$xCol]) & !is.nan(df[,input$yCol]))
      if(!is.null(input$dateCol) && input$dateCol != "") df <- subset(df, !is.na(df[,input$dateCol]))

      vals$metricdivedf <- df
      vals$originalmetricdivedf <- df
    
    })
  
}

calculateMetricStats <- function(input, output, session, vals) {

    # Update metric dive Y column indicator on Y change
    output$currentY_dive <- renderText({ 
        paste0("Current Y column: ", input$yCol)
    })

  observeEvent({
      c(
      input$xCol,
      input$run,
      input$pageFilterCheck,
      input$pageFilterTable_rows_selected,
      input$pointFilterCheck,
      input$keepPoints,
      input$removePoints
      )
  }, {
    
    if(input$xCol == "" || input$yCol == ""){
      return(NULL)  
    }
    
    df <- vals$metricdivedf
    if(!is.null(input$dateCol) && input$dateCol != "") df <- subset(df, !is.na(df[,input$dateCol]))

    if(nrow(df) == 0) {
      return(NULL)
    } 

    if(sum(!is.na(as.numeric(df[, input$xCol]))) == 0) {
      
      shinyalert(
        title = "",
        text = "Column has no data after being coerced to numeric. Try removing some filters or choosing a different column.",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        html = FALSE,
        type = "error",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#3E3F3A",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
      
      return(NULL)
      
    }
    
    xData <- as.numeric(df[, input$xCol])
    yData <- as.numeric(df[, input$yCol])
      
    summarydf <- data.frame(
      x = c(
        sum(!is.na(xData)),
        sd(xData, na.rm = TRUE),
        mean(xData, na.rm = TRUE),
        median(xData, na.rm = TRUE),
        max(xData, na.rm = TRUE),
        min(xData, na.rm = TRUE)
      ),
      y = c(
        sum(!is.na(yData)),
        sd(yData, na.rm = TRUE),
        mean(yData, na.rm = TRUE),
        median(yData, na.rm = TRUE),
        max(yData, na.rm = TRUE),
        min(yData, na.rm = TRUE)
      ),
      row.names = c("Data Points", "Standard Deviation", "Mean", "Median", "Max","Min")
    )
    
    names(summarydf) <- c(input$xCol,input$yCol)
    
    output$summaryStats <- renderTable(include.rownames = TRUE, {
      return(summarydf)
    })
    
    # Create quintiles by date and more processing
    vals$perfdf <- calculatePerformanceDates(df, input$xCol, input$yCol, input$dateCol, vals$dateFormat)
    
    # Performance Output
    output$datePerformance = renderDT(vals$perfdf,
                                      options = list(
                                           pageLength = 6,
                                           paging = FALSE,
                                           info = FALSE,
                                           searching = FALSE,
                                           ordering = FALSE
                                           ),
                                      rownames = FALSE,
                                      fillContainer = TRUE,
                                      style = "bootstrap",
                                      selection = "none")
    
  })
}