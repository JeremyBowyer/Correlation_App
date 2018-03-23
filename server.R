library(shiny)
library(shinyalert) # https://github.com/daattali/shinyalert
library(plotly)
library(dplyr)
library(reshape2)
library(quantmod)
library(XLConnect)
library(DT)
options(shiny.deprecation.messages=FALSE)
options(stringsAsFactors = FALSE)

source("global.R", local=TRUE)
source("server-modules/filters.R", local=TRUE)
source("server-modules/transformations.R", local=TRUE)
source("server-modules/aggregation.R", local=TRUE)
source("server-modules/performance.R",local=TRUE)
source("server-modules/conditions.R",local=TRUE)

# Define Functions
source("https://raw.githubusercontent.com/JeremyBowyer/Quintile-Function/master/Quintile_Function.R")

script <- readChar('highlight_cells.js', file.info('highlight_cells.js')$size)

shinyServer(function(input, output, session) {
  
  ###################
  # Reactive Values #
  ###################
  vals <- reactiveValues(
    valueFilterCount = 0,
    percentileFilterCount = 0,
    dateFilterCount = 0,
    transformationCount = 0,
    transformColIndex = NULL,
    offsetCount = 0,
    offsetColIndex = NULL,
    datadf = data.frame(),
    originaldf = data.frame(),
    metricdivedf = data.frame(),
    metricdivedfPage = data.frame(),
    metricdivedfPoint = data.frame(),
    dateFilterdf = data.frame(),
    perfdf = data.frame(),
    summarydf = data.frame(),
    yCol = "",
    dateCol = "",
    dateFormat = "%m/%d/%Y",
    IsAggregated = FALSE
    )
  
  ###########
  # Methods #
  ###########
  vals$getCols <- function(){
    df = vals$datadf
    return(names(df))
  }
  
  vals$refreshInputs <- function(session, input, vals) {
    
    cnt <- vals$transformationCount
    transformInputs <- character()
    for(i in 1:cnt) {
      transformInputs <- c(transformInputs,
                           paste0("transformCols", cnt),
                           paste0("transformDateCol", cnt),
                           paste0("transformCategoryCol", cnt),
                           paste0("transformY", cnt)
                           )
    }
    
    for(col in c("hierCol", "yCol", "dateCol", "categoryCol", "ignoreCols", "multiCols", "xCol", "dateAggDateCol", "groupByCols", transformInputs)) {
        updateSelectInput(session, col, choices=vals$getCols(), selected=input[[col]])
    }
  }
  
  vals$unloadData <- function(session, input, output, vals) {
    for(col in c("hierCol", "yCol", "dateCol", "categoryCol", "ignoreCols", "multiCols", "xCol", "dateAggDateCol", "groupByCols")) {
        updateSelectInput(session, col, choices=list(), selected=NULL)
    }
    
    vals$datadf <- NULL
    vals$originaldf <- NULL
  }
  
  vals$validateDates <- function(dateColumn){
    dateCheck <- sum(!is.na(dateColumn)) == 0
    if(dateCheck){
      Sys.sleep(0.5) # To allow previous shinyalert to close
      shinyalert(
        title = "",
        text = "Incorrect date format detected. Check 'Data Preview' tab to confirm your date format.",
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

    }
    return(!dateCheck)
  }
  
  ##################
  # Event Handlers #
  ##################
  # File Uploaded
  observeEvent(input$csvfile, {
    inFile <- input$csvfile
    
    if (is.null(inFile))
      return(NULL)
    
    vals$unloadData(session, input, output, vals)
    
    datadf = read.csv(inFile$datapath)
    vals$datadf <- datadf
    vals$originaldf <- datadf
    vals$refreshInputs(session, input, vals)
  })
  
  # Update metric dive Y column indicator on Y change
  output$currentY_dive <- renderText({ 
    paste0("Current Y column: ", input$yCol)
  })
  
  ###########################
  # Option Panel Conditions #
  ###########################
  loadConditions(input, output, session, vals)
  

  ##################
  # Filter Section #
  ##################
  # Add filter Buttons
  observeAddFilter(input, output, session, vals)
  # Apply Filters Button
  observeApplyFilters(input, output, session, vals)
  
  #############################
  # Aggregate by Date Section #
  #############################
  # Aggregate Data Button
  observeAggregateData(input, output, session, vals)
  
  ############################
  # Clear Filter & Clear Agg #
  ############################
  observeClearFilters(input, output, session, vals)
  observeClearAgg(input, output, session, vals)
  
  ##########################
  # Transformation Section #
  ##########################
  # Add Transformation Button
  observeAddTransformation(input, output, session, vals)
  # Create Transformations Button
  observeCreateTransformations(input, output, session, vals)
  # Clear Transformations Button
  observeClearTransformations(input, output, session, vals)
  
  # Run Analysis Button
  observeEvent(input$run, {
    
    if(!nrow(vals$originaldf) > 0) {
      shinyalert(
        title = "",
        text = "Please upload a valid data set first.",
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
    
    if(input$yCol == "") {
      
      shinyalert(
        title = "",
        text = "Select a Y column",
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
    
    # Store date format
    vals$dateFormat <- input$dateColFormat
    
    # Store current Y column
    vals$yCol <- input$yCol
    
    # Store current date column
    vals$dateCol <- input$dateCol
    
    # Update Y column indicator
    output$currentY_comparison <- renderText( 
      isolate(paste0("Current Y column: ", input$yCol))
    )
    
    # Read in user-provided CSV file
    datadf <- vals$datadf
    
    # Y Column
    ignoreCols = c(input$yCol, input$dateCol, input$categoryCol, input$ignoreCols)
    yColumn <- input$yCol
    
    # Create vector of metric columns
    correlCols = unique(names(datadf)[!names(datadf) %in% ignoreCols])
    
    # Create vector of multi-linear columns
    multiCols = input$multiCols
    
    # Update Metric Dive Dropdown
    updateSelectInput(session, "xCol", choices=correlCols)
    
    # Update report columns dropdown
    updateSelectInput(session, "reportCols", choices=correlCols)
    

    if (!is.null(input$dateCol) && input$dateCol != "") {
      dateFormat <- input$dateColFormat
      
      if(length(grep("%d", dateFormat)) == 0){
        fullDateFormat <- paste0('%d-', dateFormat)
        datadf[,input$dateCol] <- paste0("1-", as.character(datadf[, input$dateCol]))
      } else {
        fullDateFormat <- dateFormat
      }

      datadf[,input$dateCol] <- format(as.Date(as.character(datadf[, input$dateCol]), format = fullDateFormat), dateFormat)
      if(!vals$validateDates(datadf[,input$dateCol])) return(NULL)
      datadf <- datadf[order(datadf[, input$dateCol]), ]
    }

    
    ## All Data Points ##
    # Loop through each metric column, run regression, populate summary table
    summaryDF <- data.frame(Metric = character(),
                            'Rank Volatility (max 0.57)' = character(),
                            Correlation = numeric(),
                            DoF = integer(),
                            check.names = FALSE)
    
    for(col in correlCols) {
      if(exists("fit")) { suppressWarnings(rm("fit")) }
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
    if(exists("fit")) { suppressWarnings(rm("fit")) }
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
    
    output$summaryTable <- renderDT(summaryDF,
                                 options = list(
                                   pageLength = 10
                                 ),
                                 rownames = FALSE,
                                 fillContainer = TRUE,
                                 style = "bootstrap",
                                 selection = "none")
    
    ## By Date ##
    if(input$dateCol != "") {
      dateCorrelations <- data.frame(
        Metric = c(correlCols, "Multilinear"),
        "Total Periods" = integer(length(c(correlCols, "Multilinear"))),
        "Negative Periods" = integer(length(c(correlCols, "Multilinear"))),
        "Positive Periods" = integer(length(c(correlCols, "Multilinear"))),
        "% Negative" = numeric(length(c(correlCols, "Multilinear"))),
        "% Positive" = numeric(length(c(correlCols, "Multilinear"))),
        "Avg Correlation" = numeric(length(c(correlCols, "Multilinear"))),
        check.names = FALSE
        )
      
      # Fill in correlations by date
      for(date in unique(datadf[, input$dateCol])) {
        dateDF <- datadf[datadf[,input$dateCol]==date, ]
        # single factor regressions
        for(col in correlCols) {
          dateCorrelations[dateCorrelations$Metric == col, as.character(date)] <- cor(as.numeric(dateDF[, col]), as.numeric(dateDF[, yColumn]), use = "pairwise.complete.obs")
        }
        
        # multi-linear regression
        if(exists("fit")) { suppressWarnings(rm("fit")) }
        
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
        metricCorrelations <- as.numeric(dateCorrelations[dateCorrelations$Metric == col, as.character(unique(datadf[, input$dateCol]))])
        metricCorrelations <- metricCorrelations[!is.na(metricCorrelations)]
        dateCorrelations[dateCorrelations$Metric == col, "Total Periods"] <- length(metricCorrelations)
        dateCorrelations[dateCorrelations$Metric == col, "Negative Periods"] <- length(metricCorrelations[metricCorrelations < 0])
        dateCorrelations[dateCorrelations$Metric == col, "Positive Periods"] <- length(metricCorrelations[metricCorrelations > 0])
        dateCorrelations[dateCorrelations$Metric == col, "% Negative"] <- length(metricCorrelations[metricCorrelations < 0]) / length(metricCorrelations)
        dateCorrelations[dateCorrelations$Metric == col, "% Positive"] <- length(metricCorrelations[metricCorrelations > 0]) / length(metricCorrelations)
        dateCorrelations[dateCorrelations$Metric == col, "Avg Correlation"] <- mean(metricCorrelations, na.rm = TRUE)
      }
      
      output$dateCorrelations <- renderDT(dateCorrelations,
                                 options = list(
                                   pageLength = 10
                                 ),
                                 rownames = FALSE,
                                 fillContainer = TRUE,
                                 style = "bootstrap",
                                 selection = "none")
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
  
  output$downloadReport <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = function(){
      xname <- input$xCol
      xname <- gsub("[\\!\\@\\#\\$\\%\\^\\&\\*\\(\\)\\-\\=\\+\\.]", '',xname)
      paste0(xname, " report.html")
    },
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("documents/report.Rmd", tempReport, overwrite = TRUE)
      
      # Subset DF to only include appropriate columns
      cols <- c(input$xCol, vals$yCol)
      if(input$categoryCol != "") cols <- c(input$categoryCol, cols)
      if(vals$dateCol != "") cols <- c(vals$dateCol, cols)
      df <- vals$metricdivedf[,cols]
      row.names(df) <- 1:nrow(df)
      
      # Set up parameters to pass to Rmd document
      params <- list(metric = input$xCol,
                     df = df,
                     perf = vals$perfdf,
                     dateCol = vals$dateCol,
                     dateFormat = vals$dateFormat,
                     yCol = vals$yCol)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport,
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  #############
  ## Filters ##
  #############
  
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
  
  # Upon selection of metric in Metric Dive tab
  observeEvent({
    c(input$xCol,input$yCol,input$run,input$applyFilters,input$filterClear)
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
     
      # Check for valid data, otherwise show error notification to user
      if(nrow(df) == 0) {
        
      } else if(sum(!is.na(as.numeric(df[, input$xCol]))) == 0) {
        removeUI(".metricAlert", multiple = TRUE)
        insertUI(
          selector="#metricAlertDiv",
          where="beforeEnd",
          ui = tags$p("Column has no data after being coerced to numeric.", style="color: red;", class="metricAlert")
        )
        
      } else {
                formstring <- paste0("as.numeric(", input$yCol, ") ~ ")
            formstring <- paste0(formstring, " as.numeric(", input$xCol, ")")
        
            form <- as.formula(formstring)
          fit <- lm(form, df)


                
        
        summarydf <- data.frame(x = c(NA,NA,NA,NA,NA), y = c(NA,NA,NA,NA,NA),"ANOVA" =c(NA,NA,NA,NA,NA))
        names(summarydf) <- c(input$xCol,input$yCol)
        row.names(summarydf) <- c("# NAs","# blanks","max","min","cor")
        summarydf["# NAs",input$xCol] <- sum(is.na(df[,input$xCol] ))
        summarydf["# NAs",input$yCol] <- sum(is.na(df[,input$yCol] ))
        summarydf["max",input$xCol] <- max(df[,input$xCol] )
        summarydf["max",input$yCol] <- max(df[,input$yCol] )
        summarydf["min",input$xCol] <- min(df[,input$xCol] )
        summarydf["min",input$yCol] <- min(df[,input$yCol] )
        summarydf["cor","ANOVA"]    <- cor(as.numeric(df[,input$xCol]), as.numeric(df[, input$yCol]), use = "pairwise.complete.obs")
        summarydf["# blanks",input$xCol]      <- sum(df[,input$xCol] == "",na.rm = TRUE)    
          summarydf["# blanks",input$yCol]    <- sum(df[,input$yCol] == "",na.rm = TRUE) 
          print(sum(df[,input$xCol] == ""))
          print(sum(df[,input$yCol] == ""))
          print(sum(is.na(df[,input$xCol] )))
          print(sum(is.na(df[,input$yCol] )))
        removeUI(".metricAlert", multiple = TRUE)

        # Create quintiles by date and more processing
        vals$perfdf <- calculatePerformance(df,input) 
        vals$summarydf <- summarydf
        
        # Performance Output
        output$datePerformance = renderTable({
          return(vals$perfdf)
        })
        output$summaryStats <- renderTable(include.rownames = TRUE,
                                           {
          return( vals$summarydf )
        })
        
      }
    }
  })
  
  ##################
  # Reactive plots #
  ##################
  # Metric Scatter
  output$metricScatter <- renderPlotly({
    
    if(input$xCol == "" || input$yCol == "") {
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
    df %>%
      plot_ly(x = xform, source = "metricScatter") %>%
      add_markers(y = yform, color = colorform, text = ~text)  %>%
      add_lines(x = xform, y = fitted(fit), fill = "red", name = "Regression Line") %>%
      layout(dragmode = "lasso")
    
  })
  
  # Metric Histogram
  output$metricHist <- renderPlotly({
    
    if(input$xCol == "") {
      return(NULL)
    }
    
    df <- vals$metricdivedf
    xform <- as.formula(paste0("~",input$xCol))
    plot_ly(data = df, x = xform, type = "histogram")
    
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
    dates <- as.Date(unique(metricDF[,input$dateCol]), format = vals$dateFormat)
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

  
  
  #######################
  # Data Preview Screen #
  #######################
  output$dataPreview <- renderDT(vals$datadf,
                                 options = list(
                                   pageLength = 10
                                 ),
                                 rownames = FALSE,
                                 fillContainer = TRUE,
                                 style = "bootstrap",
                                 selection = "none")
  
  #####################################
  # JavaScript Conditional Formatting #
  #####################################
  session$onFlushed(function() {
    session$sendCustomMessage(type='jsCode', list(value = script))
  }, FALSE)
  
})