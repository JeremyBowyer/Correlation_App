observeAddOffset <- function(input, output, session, vals) {
  
  observeEvent(input$addOffset, {
    
    vals$offsetCount <- vals$offsetCount + 1
    cnt <- vals$offsetCount
    
    insertUI(
      selector="#offsets",
      where="afterBegin",
      ui = tags$div(radioButtons(paste0("offsetType", cnt), "Select Direction", choices=list("Forward" = "forward",
                                                                                             "Backward" = "backward")),
                    textInput(paste0("offsetSuffix", cnt), "Column Suffix Name", value = ""),
                    numericInput(paste0("offsetLag", cnt), "Select Lag", value = 1, min = 1), 
                    selectInput(paste0("offsetCols", cnt), "Select columns to offset", choices=vals$getCols(), multiple = TRUE),
                    selectInput(paste0("offsetDateCol", cnt), "Select column to offset along (probably a date)", choices=vals$getCols()),
                    selectInput(paste0("offsetCategoryCol", cnt), "Select category columns to group by (optional)", choices=vals$getCols(), multiple = TRUE),
                    tags$hr(), class="offset")
    )
    
  })

}

observeApplyOffsets <- function(input, output, session, vals) {
  
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
}

observeClearOffsets <- function(input, output, session, vals) {
  observeEvent(input$offsetClear, {
    removeUI(".offset", multiple = TRUE)
    ocolIndex <- vals$offsetColIndex
    if(!is.null(ocolIndex)){
      vals$datadf <- vals$datadf[, -ocolIndex]
    }
    vals$offsetCount <- 0
    vals$offsetColIndex <- NULL
  })
}