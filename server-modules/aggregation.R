aggregateData <- function(input, output, session, vals) {
  
  tryCatch({
    # Grab values from current transformation request
    dateCol <- input$dateAggDateCol
    dateColFormat <- input$dateAggDateColFormat
    catCols <- input$groupByCols
    aggLevel <- input$aggregationLevel
    aggType <- input$aggregationFunc
    
    # Prepare dataframe
    df <- vals$datadf
    df[,dateCol] <- format(as.Date(as.character(df[, dateCol]), format = dateColFormat), aggLevel)
    if(!vals$validateDates(df[,dateCol])) return(NULL)
    df <- df[order(df[, dateCol]), ]
    
    # Choose aggregation function
    switch(aggType,
           sum = {
             aggFunc <- function(x) {
               n <- as.numeric(x)
               n <- n[!is.na(n)]
               if(length(n) == 0) {
                 return(NA)
               } else {
                 return(sum(n))
               }
             }
           },
           average = {
             aggFunc <- function(x) {
               n <- as.numeric(x)
               n <- n[!is.na(n)]
               if(length(n) == 0) {
                 return(NA)
               } else {
                 return(mean(n))
               }
             }
           },
           earliest = {
             aggFunc <- function(x) {
               n <- as.numeric(x)
               n <- n[!is.na(n)]
               if(length(n) == 0) {
                 return(NA)
               } else {
                 return(head(n, 1))
               }
             }
           },
           latest = {
             aggFunc <- function(x) {
               n <- as.numeric(x)
               n <- n[!is.na(n)]
               if(length(n) == 0) {
                 return(NA)
               } else {
                 return(tail(n, 1))
               }
             }
           },
           min = {
             aggFunc <- function(x) {
               n <- as.numeric(x)
               n <- n[!is.na(n)]
               if(length(n) == 0) {
                 return(NA)
               } else {
                 return(min(n))
               }
             }
           },
           max = {
             aggFunc <- function(x) {
               n <- as.numeric(x)
               n <- n[!is.na(n)]
               if(length(n) == 0) {
                 return(NA)
               } else {
                 return(max(n))
               }
             }
           }
    )
    
    # Create formula for aggregate()
    aggCols <- names(df)[!names(df) %in% c(dateCol, catCols)]
    groupFormString <- paste0("cbind(", aggCols[1])
    for(aggCol in aggCols[-1]) {
      groupFormString <- paste0(groupFormString, ", ", aggCol)
    }
    
    groupFormString <- paste0(groupFormString, ") ~ ", dateCol)
    
    for (groupCol in catCols){
      groupFormString <- paste0(groupFormString, " + ", groupCol)
    }
    groupForm <- as.formula(groupFormString)
  
    # Aggregate data
    vals$datadf <- aggregate(formula=groupForm, data=df, FUN=aggFunc, na.action=NULL)
    vals$IsAggregated <- TRUE
    
    Sys.sleep(0.5) # To allow previous shinyalert to close
    
    shinyalert(
          title = "",
          text = "Your data has been aggregated. You can review the results in the 'Data Preview' tab.",
          closeOnEsc = TRUE,
          closeOnClickOutside = TRUE,
          html = FALSE,
          type = "success",
          showConfirmButton = TRUE,
          showCancelButton = FALSE,
          confirmButtonText = "OK",
          confirmButtonCol = "#3E3F3A",
          timer = 0,
          imageUrl = "",
          animation = TRUE
        )
  },error=function(e) {
      if(DEBUG_MODE) {
        stop(e)
      }
      shinyerror(e)
  })  
}


observeAggregateData <- function(input, output, session, vals) {
  
  observeEvent(input$aggregateData, {
    shinyalert(
      title = "WARNING!",
      text = "This process will assume all columns not designated as a Date column or a Category column are numeric and capable of being aggregated. This will result in loss of data if a column is not numeric.",
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = FALSE,
      type = "warning",
      showConfirmButton = TRUE,
      showCancelButton = TRUE,
      confirmButtonText = "OK",
      confirmButtonCol = "#3E3F3A",
      cancelButtonText = "Cancel",
      timer = 0,
      imageUrl = "",
      animation = TRUE,
      callbackR = function(x) { if(x) aggregateData(input, output, session, vals) }
    )
  })
  
}

observeClearAgg <- function(input, output, session, vals) {
  
  # Clear Filters Button
  observeEvent(input$aggClear, {
    
    if(!input$filterClear && !input$aggClear){
      return(NULL)
    }
    
    
    updateSelectInput(session, "dateAggDateCol", choices = vals$getCols(), selected = "")
    updateTextInput(session, "dateAggDateColFormat", value = vals$dateFormat)
    updateSelectInput(session, "groupByCols", choices = vals$getCols(), selected = NULL)
    
    vals$datadf <- vals$originaldf
    vals$IsAggregated <- FALSE
    
    shinyalert(
      title = "",
      text = "Your data aggregation has been removed. Any filters or metric transformations you created will be re-created in the following order: First your data will be filtered, then transformations will be re-created.",
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = FALSE,
      type = "success",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#3E3F3A",
      timer = 0,
      imageUrl = "",
      animation = TRUE
    )
    
    applyFilters(FALSE, TRUE, input, output, session, vals)
    
  })
}  