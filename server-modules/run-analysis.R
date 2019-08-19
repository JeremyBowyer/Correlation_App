observeRunAnalysis <- function(input, output, session, vals) {

  observeEvent(input$run, {

    tryCatch({
      #### Validate Data ####
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
      
      #### Grab Inputs ####
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
      ignoreCols <- c(input$yCol, input$dateCol, input$categoryCol, input$ignoreCols)
      yColumn <- input$yCol
      # Date Column
      dateCol <- input$dateCol
      # Create vector of metric columns
      correlCols <- unique(names(datadf)[!names(datadf) %in% ignoreCols])
      # Category column
      categoryCol <- input$categoryCol
      # Create vector of multi-linear columns
      multiCols <- input$multiCols
      # Create vector of correlation matrix columns
      corMatCols <- input$corMatCols
      # Update Metric Dive Dropdown
      updateSelectInput(session, "xCol", choices=correlCols)
      # Update report columns dropdown
      updateSelectInput(session, "reportCols", choices=correlCols)
      
  
      #### Validate Date ####
      # If date format is missing Day component,
      # add one temporarily, then convert to date and validate
      if (!is.null(input$dateCol) && input$dateCol != "") {
        
        dateFormat <- input$dateColFormat
        
        if(length(grep("%d", dateFormat)) == 0){
          fullDateFormat <- paste0('%d-', dateFormat)
          datadf[,input$dateCol] <- paste0("1-", as.character(datadf[, input$dateCol]))
        } else {
          fullDateFormat <- dateFormat
        }
        
        datadf[,input$dateCol] <- as.Date(as.character(datadf[, input$dateCol]), format = fullDateFormat)
        if(!vals$validateDates(datadf[,input$dateCol])) return(NULL)
        datadf <- datadf[order(datadf[, input$dateCol]), ]
        
      }
      
      #### REGRESSIONS - ALL ####
      ## All Data Points ##
      # Loop through each metric column, run regression, populate summary table
      summaryDF <- data.frame(Metric = character(),
                              'Rank Volatility (max 0.57)' = numeric(),
                              Correlation = numeric(),
                              'R-Squared' = numeric(),
                              Slope = numeric(),
                              DoF = integer(),
                              check.names = FALSE,
                              "Performance Differential" = numeric())
      
      # Start Progress Bar
      progress <- shiny::Progress$new()
      progress$set(message = "Running Analysis", value=0)
      on.exit(progress$close(), add=TRUE)
      # Run Regressions
      datadf[[yColumn]] <- as.numeric(datadf[[yColumn]])
      for(col in correlCols) {
        
        datadf[[col]] <- as.numeric(datadf[[col]])
        
        if(exists("fit")) { suppressWarnings(rm("fit")) }
        summaryDF[nrow(summaryDF) + 1, "Metric"] <- col
        
        datadf[!is.finite(datadf[, col]), col] <- NA
        datadf[!is.finite(datadf[, yColumn]), yColumn] <- NA
       
        # Rank Volatility
        tryCatch({
          metricDF <- datadf[order(datadf[,input$dateCol]) ,c(input$dateCol, input$categoryCol, col)]
          wideMetricDF <- dcast(metricDF,as.formula(paste0(input$dateCol," ~ ",categoryCol)), function(x){ mean(x,na.rm=TRUE) },value.var = col)[, -1]
          rankDF <- t(apply(wideMetricDF, 1, function(x) rank(x, na.last = "keep") / length(which(!is.na(x))) ))
          diffRankDF <- apply(rankDF, 2, function(x) c(NA, diff(x)))
          stds <- apply(diffRankDF, 1, function(x) sd(x, na.rm = TRUE))
          summaryDF[nrow(summaryDF), "Rank Volatility (max 0.57)"] <- round(mean(stds, na.rm = TRUE), 10)
        }, error = function(e) {
          return(NULL)
        })

        # Correlation and DoF
        tryCatch({
          form <- as.formula(paste0("`", yColumn, "` ~ `", col,"`"))
          fit <- lm(form, datadf)
          correl <- suppressWarnings(cor(as.numeric(datadf[, col]), as.numeric(datadf[, yColumn]), use = "pairwise.complete.obs"))
          summaryDF[nrow(summaryDF), "Correlation"] <- round(correl, 4)
          summaryDF[nrow(summaryDF), "R-Squared"] <- round(correl*correl, 4)
          summaryDF[nrow(summaryDF), "Slope"] <- round(coef(fit)[2], 4)
          summaryDF[nrow(summaryDF), "DoF"] <- fit$df
        }, error = function(e) {
          summaryDF[nrow(summaryDF), "Correlation"] <- NA
          summaryDF[nrow(summaryDF), "R-Squared"] <- NA
          summaryDF[nrow(summaryDF), "Slope"] <- NA
          summaryDF[nrow(summaryDF), "DoF"] <- NA
        })
        progress$set(value=(match(col, correlCols) / length(correlCols)) * 0.5,detail=paste0("Updating summary table: ", " Column ", match(col, correlCols), " / ", length(correlCols)))
        perfTable <- calculatePerformance(datadf, col, input$yCol, input$dateCol, vals$dateFormat)
        summaryDF[nrow(summaryDF),"Performance Differential"] <- as.numeric(perfTable[nrow(perfTable),"All"])
      }
      
    
      #### REGRESSIONS - MULTI ####
      if(exists("fit")) { suppressWarnings(rm("fit")) }
      summaryDF[nrow(summaryDF) + 1, "Metric"] <- "Multilinear"
      
      formstring <- paste0("as.numeric(`", yColumn, "`) ~ ")
      for (col in multiCols){
        if(col == multiCols[length(multiCols)]) {
          formstring <- paste0(formstring, " as.numeric(`", col, "`)")
        } else {
          formstring <- paste0(formstring, " as.numeric(`", col, "`) +")
        }
      }
      
      
      tryCatch({
        form <- as.formula(formstring)
        fit <- lm(form, datadf)
        datadf[names(fit$fitted.values), "fitted"] <- fit$fitted.values
        correl <- suppressWarnings(cor(as.numeric(datadf$fitted), as.numeric(datadf[, yColumn]), use = "pairwise.complete.obs"))
        summaryDF[nrow(summaryDF), "Correlation"] <- round(correl, 4)
        summaryDF[nrow(summaryDF), "R-Squared"] <- round(correl*correl, 4)
        summaryDF[nrow(summaryDF), "Slope"] <- round(coef(fit)[2], 4)
        summaryDF[nrow(summaryDF), "DoF"] <- fit$df
      }, error = function(e) {
        summaryDF[nrow(summaryDF), "Correlation"] <- NA
        summaryDF[nrow(summaryDF), "R-Squared"] <- NA
        summaryDF[nrow(summaryDF), "Slope"] <- NA
        summaryDF[nrow(summaryDF), "DoF"] <- NA
      })
      vals$summaryDF <- summaryDF
      
      output$summaryTable <- renderDT(vals$summaryDF,
                                      options = list(
                                        pageLength = 10
                                        ),
                                      rownames = FALSE,
                                      fillContainer = TRUE,
                                      style = "bootstrap",
                                      selection = 'none')
      
      
      if (corMatCols != "" && !is.null(corMatCols)) {
        
        corMatData <- datadf[,corMatCols]
        corMatData <- apply(corMatData, 2, as.numeric)
        cor_mat <- cor(corMatData, use="pairwise.complete.obs")
        
        output$corMat <- renderDT(cor_mat,
                                  options = list(
                                    pageLength = 100,
                                    paging = FALSE,
                                    info = FALSE,
                                    searching = FALSE,
                                    scrollY = "600px"),
                                  rownames = TRUE,
                                  extensions = "Scroller",
                                  style = "bootstrap",
                                  selection="none",
                                  autoHideNavigation = TRUE)
      }
      
      
      #### REGRESSIONS - DATE ####
      if(input$dateCol != "") {
        dateCorrelations <- data.frame(
          Metric = c(correlCols, "Multilinear"),
          "Total Periods" = integer(length(c(correlCols, "Multilinear"))),
          "Negative Periods" = integer(length(c(correlCols, "Multilinear"))),
          "Positive Periods" = integer(length(c(correlCols, "Multilinear"))),
          "% Negative" = numeric(length(c(correlCols, "Multilinear"))),
          "% Positive" = numeric(length(c(correlCols, "Multilinear"))),
          "Weighted Avg Correlation" = numeric(length(c(correlCols, "Multilinear"))),
          "Avg Correlation" = numeric(length(c(correlCols, "Multilinear"))),
          check.names = FALSE
          )
        # DF to store data points for all metrics
        dateDataPointsDF = data.frame(Metric = c(correlCols, "Multilinear"))

        # Create DFs to store correlations and datapoints for each metric
        correlDPList = list()
        for (col in correlCols){
          correlDPList[[col]] = data.frame("Correlations" = numeric(), "DataPoints" = numeric(), "CorrelxDP" = numeric())
        }
        
        # Fill in correlations by date
        dates <- unique(datadf[, input$dateCol])
        dates  <- dates[order(dates)]
        for(date in dates) {
        
          dateDF <- datadf[datadf[,input$dateCol]==date, ]
          chardate <- format(as.Date(date,origin="1970/1/1"), format=dateFormat)
          progress$set(value=0.5 + (match(date, dates) / length(dates)) * 0.5,
                       detail=paste0("Updating correlations by date: ", chardate))
          # single factor regressions
          for(col in correlCols) {
            x <- as.numeric(dateDF[, col])
            y <- as.numeric(dateDF[, yColumn])
            
            if(sum(complete.cases(x,y)) < 10){
              dateCorrelations[dateCorrelations$Metric == col, chardate] <- NA
              dateDataPointsDF[dateDataPointsDF$Metric == col, chardate] <- NA
            } else {
              correl = suppressWarnings(cor(x, y, use = "pairwise.complete.obs"))
              dp = sum(complete.cases(x,y))
              
              dateCorrelations[dateCorrelations$Metric == col, chardate] <- correl
              dateDataPointsDF[dateDataPointsDF$Metric == col, chardate] <- dp
              correlDPList[[col]][chardate, "Correlations"] <- correl
              correlDPList[[col]][chardate, "DataPoints"] <- dp
            }

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
            dateCorrelations[dateCorrelations$Metric == "Multilinear", chardate] <- suppressWarnings(cor(as.numeric(dateDF$fitted), as.numeric(dateDF[, yColumn]), use = "pairwise.complete.obs"))
          }, error = function(e) {
            #print(e$message)
          })
          
        }
        output$dateDataPointsDF <- renderDT(dateDataPointsDF,
                                            options = list(
                                              pageLength = 10, 
                                              paging = TRUE
                                              ),
                                            rownames = FALSE,
                                            fillContainer = TRUE,
                                            style = "bootstrap",
                                            selection = "none")

        # Fill in summary stats of date correlations
        for(col in c(correlCols,"Multilinear")) {
          
          tryCatch({
            # Weighted Average Correlation
            colDF <- correlDPList[[col]]
            colDF <- colDF[complete.cases(colDF[, c("Correlations", "DataPoints")]), ]
            colDF[, "CorrelxDP"] <- colDF[, "Correlations"] * colDF[, "DataPoints"]
            wghtdAvg <- sum(colDF[, "CorrelxDP"], na.rm = TRUE) / sum(colDF[, "DataPoints"], na.rm = TRUE)
            
            # Vector of correlations, for summary stats
            chardates <- format(unique(datadf[, input$dateCol]), format=dateFormat)
            metricCorrelations <- as.numeric(dateCorrelations[dateCorrelations$Metric == col, chardates])
            metricCorrelations <- metricCorrelations[!is.na(metricCorrelations)]
            
            # Fill in summary stats
            dateCorrelations[dateCorrelations$Metric == col, "Total Periods"] <- length(metricCorrelations)
            dateCorrelations[dateCorrelations$Metric == col, "Negative Periods"] <- length(metricCorrelations[metricCorrelations < 0])
            dateCorrelations[dateCorrelations$Metric == col, "Positive Periods"] <- length(metricCorrelations[metricCorrelations > 0])
            dateCorrelations[dateCorrelations$Metric == col, "% Negative"] <- length(metricCorrelations[metricCorrelations < 0]) / length(metricCorrelations)
            dateCorrelations[dateCorrelations$Metric == col, "% Positive"] <- length(metricCorrelations[metricCorrelations > 0]) / length(metricCorrelations)
            dateCorrelations[dateCorrelations$Metric == col, "Weighted Avg Correlation"] <- wghtdAvg
            dateCorrelations[dateCorrelations$Metric == col, "Avg Correlation"] <- mean(metricCorrelations, na.rm = TRUE)
          }, error=function(e){
            NULL
          })
          progress$set(value=as.numeric(progress$getValue())+1/(as.numeric(length(correlCols))*2+as.numeric(length(correlCols))*length(unique(datadf[,input$dateCol]))),detail="Filling in summary stats of date correlations")
        }
       
        output$dateCorrelations <- renderDT(dateCorrelations,
                                            options = list(
                                              pageLength = 10, 
                                              paging = TRUE
                                            ),
                                            rownames = FALSE,
                                            fillContainer = TRUE,
                                            style = "bootstrap",
                                            selection = "none")
      }
      
      updateTabsetPanel(session, "mainTabset", selected="correlations")
     },error=function(e) {
       if(!DEBUG_MODE) {
         stop(e)
       }
       shinyerror(e)
       }
     )
  })
}


correlPlots <- function(input, output, session, vals) {
  
  #### Histogram of Correlations ####
  output$correlHist <- renderPlotly({
    df <- vals$summaryDF
    plot_ly(data = df, x = ~Correlation, type = "histogram")
  })
  
  #### Scatter of Correlations ####
  output$correlScatter <- renderPlotly({
    df <- vals$summaryDF
    plot_ly(data = df,
            x = ~DoF,
            y = ~Correlation,
            color = ~Metric,
            text = ~Metric,
            type = 'scatter'
    ) %>% layout(showlegend = FALSE)
  })
  
}