observeRunAnalysis <- function(input, output, session, vals) {

observeEvent(input$run, {
    tryCatch({
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
      ## All Data Points ##
      # Loop through each metric column, run regression, populate summary table
      summaryDF <- data.frame(Metric = character(),
                              'Rank Volatility (max 0.57)' = character(),
                              Correlation = numeric(),
                              DoF = integer(),
                              check.names = FALSE,
                              "Performance Differential" = numeric())
  
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
        }, error = function(e) {
          summaryDF[nrow(summaryDF), "Correlation"] <- NA
          summaryDF[nrow(summaryDF), "DoF"] <- NA
        })
  
        perfdf <-  calculatePerformance(df=datadf,input=input,dateCols=FALSE,col=col,vals=vals) 
        summaryDF[nrow(summaryDF),"Performance Differential"] <- perfdf[6,"All"]
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
      }, error = function(e) {
        summaryDF[nrow(summaryDF), "Correlation"] <- NA
        summaryDF[nrow(summaryDF), "DoF"] <- NA
      })
  
    
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
        dates <- unique(datadf[, input$dateCol])
        dates  <- dates[order(dates)]
        for(date in dates) {
          dateDF <- datadf[datadf[,input$dateCol]==date, ]
          chardate <- format(as.Date(date), format=dateFormat)
          # single factor regressions
          for(col in correlCols) {
            dateCorrelations[dateCorrelations$Metric == col, chardate] <-
              tryCatch({
                cor(as.numeric(dateDF[, col]), as.numeric(dateDF[, yColumn]), use = "pairwise.complete.obs")},
                error=function(e){return(NA)
              })
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
          chardates <- format(unique(datadf[, input$dateCol]), format=dateFormat)
          metricCorrelations <- as.numeric(dateCorrelations[dateCorrelations$Metric == col, chardates])
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
    },error=function(e) {
      if(DEBUG_MODE) {
        stop(e)
      }
      shinyerror(e)
    })
  })
}