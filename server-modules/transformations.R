observeAddTransformation <- function(input, output, session, vals) {
  
  observeEvent(input$addTransformation, {
    
    transformation <- input$transformationselected
    transformationName <- names(transformationList[transformationList==transformation])
    vals$transformationCount <- vals$transformationCount + 1
    cnt <- vals$transformationCount
    
    switch(transformation,
           diff = ,
           rollingsum = ,
           submedian = ,
           perchg = ,
           perchgmedian = ,
           perchgstd =
          {
             insertUI(selector="#transformations",
                      where="afterBegin",
                      ui = tags$div(h3(transformationName),
                                    tags$div(textInput(paste0("transformationType", cnt), label = NULL, value=transformation),style="display:none;"),
                                    textInput(paste0("transformationSuffix", cnt), "Column Suffix Name", value = ""),
                                    numericInput(paste0("transformationLag", cnt), "Select Lag", value = 1, min = 1), 
                                    selectInput(paste0("transformCols", cnt), "Select columns to transform", choices=vals$getCols(), multiple = TRUE),
                                    selectInput(paste0("transformDateCol", cnt), "Select column to transform along (probably a date)", choices=vals$getCols()),
                                    textInput(paste0("transformDateColFormat", cnt), "Format dates are in (check 'Data Preview' tab)", "%m/%d/%Y"),
                                    selectInput(paste0("transformCategoryCol", cnt), "Select category columns to group by (optional)", choices=vals$getCols(), multiple = TRUE),
                                    tags$hr(), class="transformation")
             )
           },
           crossmedian = ,
           zscorecross = { 
             insertUI(selector="#transformations",
                      where="afterBegin",
                      ui = tags$div(h3(transformationName),
                                    tags$div(textInput(paste0("transformationType", cnt), label = NULL, value=transformation),style="display:none;"),
                                    textInput(paste0("transformationSuffix", cnt), "Column Suffix Name", value = ""),
                                    selectInput(paste0("transformCols", cnt), "Select columns to transform", choices=vals$getCols(), multiple = TRUE),
                                    selectInput(paste0("transformCategoryCol", cnt), "Select category columns to group by (optional)", choices=vals$getCols(), multiple = TRUE),
                                    tags$hr(), class="transformation")
             )
           },
           subhistmedian = , 
           zscorelong = {
             insertUI(selector="#transformations",
                      where="afterBegin",
                      ui = tags$div(h3(transformationName),
                                    tags$div(textInput(paste0("transformationType", cnt), label = NULL, value=transformation),style="display:none;"),
                                    textInput(paste0("transformationSuffix", cnt), "Column Suffix Name", value = ""),
                                    selectInput(paste0("transformCols", cnt), "Select columns to transform", choices=vals$getCols(), multiple = TRUE),
                                    selectInput(paste0("transformDateCol", cnt), "Select column to transform along (probably a date)", choices=vals$getCols()),
                                    textInput(paste0("transformDateColFormat", cnt), "Format dates are in (check 'Data Preview' tab)", "%m/%d/%Y"),
                                    selectInput(paste0("transformCategoryCol", cnt), "Select category columns to group by (optional)", choices=vals$getCols(), multiple = TRUE),
                                    tags$hr(), class="transformation")
             )
           },
           binarystring = {
             insertUI(selector="#transformations",
                      where="afterBegin",
                      ui = tags$div(h3(transformationName),
                                    tags$div(textInput(paste0("transformationType", cnt), label = NULL, value=transformation),style="display:none;"),
                                    textInput(paste0("transformationSuffix", cnt), "Column Suffix Name", value = ""),
                                    selectInput(paste0("transformCols", cnt), "Select columns to transform", choices=vals$getCols(), multiple = TRUE),
                                    textInput(paste0("transformBinaryString", cnt), "Value to be flagged as 1", value = ""),
                                    tags$hr(), class="transformation")
             )
           },
           binaryvalue = {
             insertUI(selector="#transformations",
                      where="afterBegin",
                      ui = tags$div(h3(transformationName),
                                    tags$div(textInput(paste0("transformationType", cnt), label = NULL, value=transformation),style="display:none;"),
                                    textInput(paste0("transformationSuffix", cnt), "Column Suffix Name", value = ""),
                                    selectInput(paste0("transformCols", cnt), "Select columns to transform", choices=vals$getCols(), multiple = TRUE),
                                    textInput(paste0("transformBinaryValue", cnt), "Value, data points equal to or above to be flagged as 1", value = ""),
                                    tags$hr(), class="transformation")
             )
           },
          residual = {
            insertUI(selector="#transformations",
                     where="afterBegin",
                     ui = tags$div(h3(transformationName),
                                   tags$div(textInput(paste0("transformationType", cnt), label = NULL, value=transformation),style="display:none;"),
                                   textInput(paste0("transformationSuffix", cnt), "Column Suffix Name", value = ""),
                                   selectInput(paste0("transformCols", cnt), "Select x columns", choices=vals$getCols(), multiple = TRUE),
                                   selectInput(paste0("transformationY", cnt), "Select y column", choices=vals$getCols()),
                                   tags$hr(), class="transformation")
            )
          },
          offsetfwd = ,
          offsetbwd = {
            insertUI(
              selector="#transformations",
              where="afterBegin",
              ui = tags$div(h3(transformationName),
                            tags$div(textInput(paste0("transformationType", cnt), label = NULL, value=transformation),style="display:none;"),
                            textInput(paste0("transformationSuffix", cnt), "Column Suffix Name", value = ""),
                            numericInput(paste0("transformationLag", cnt), "Select Lag", value = 1, min = 1), 
                            selectInput(paste0("transformCols", cnt), "Select columns to offset", choices=vals$getCols(), multiple = TRUE),
                            selectInput(paste0("transformDateCol", cnt), "Select column to offset along (probably a date)", choices=vals$getCols()),
                            textInput(paste0("transformDateColFormat", cnt), "Format dates are in (check 'Data Preview' tab)", "%m/%d/%Y"),
                            selectInput(paste0("transformCategoryCol", cnt), "Select category columns to group by (optional)", choices=vals$getCols(), multiple = TRUE),
                            tags$hr(), class="transformation")
            )
          }
        )
    
  })
  
}

observeCreateTransformations <- function(input, output, session, vals) {
  
  createTransformations <- function(alert, dateformat=FALSE){
    
    if(vals$transformationCount < 1){
      return(NULL)
    }
    
    df <- vals$datadf
    tcolIndex <- vals$transformColIndex
    if(!is.null(tcolIndex)){
      df <- df[, -tcolIndex]
    }
    
    firstCol <- ncol(df) + 1
    
    for (cnt in 1:vals$transformationCount) {
      
      # Grab values from current transformation request
      type <- input[[paste0("transformationType", cnt)]]
      cols <- input[[paste0("transformCols", cnt)]]
      dateCol <- input[[paste0("transformDateCol", cnt)]]
      
      if(dateformat) {
        updateTextInput(session, paste0("transformDateColFormat", cnt), value = input$dateAggDateColFormat)
        dateColFormat <- input$dateAggDateColFormat
      } else {
        dateColFormat <- input[[paste0("transformDateColFormat", cnt)]]
      }
      
      catCols <- input[[paste0("transformCategoryCol", cnt)]]
      lag <- input[[paste0("transformationLag", cnt)]]
      transformSuffix <- gsub(" ", ".", input[[paste0("transformationSuffix", cnt)]])
      transformBinaryString <- input[[paste0("transformBinaryString", cnt)]]
      transformBinaryValue <- input[[paste0("transformBinaryValue", cnt)]]
      regressiony <- input[[paste0("transformationY", cnt)]]

      # Assign transformation function based on type selected
      switch(type,
             residual = {
               transformFunc <- function(x, lag = 0, y) { 
                 x <- as.numeric(x)
                 y <- as.numeric(y)
                 residual <- (y - coef(lm(y ~ x))["x"]*x - coef(lm(y ~ x))["(Intercept)"])
                 return(residual)
                 }
             },
             diff={
               transformFunc <- function(x, lag, y = 0) { return( c(rep(NA, lag), diff(as.numeric(x), lag = lag)) ) }
             },
             rollingsum={   
               transformFunc <- function(x, lag, y = 0) {
                 d <- as.numeric(x)
                 m <- numeric()
                 
                 for (i in seq_along(d)) {
                   
                   if (i <= lag) {
                     m[i] <- NA
                   } else {
                     m[i] <- sum(d[(i - lag):(i)])  
                   }
                   
                 }
                 
                 return( m )
                 }
             },
             submedian={
               transformFunc <- function(x, lag, y = 0) {
                 n <- as.numeric(x)
                 m <- numeric()
                 
                 for (i in seq_along(n)) {
                   if ((i - lag) > 0) {
                     m[length(m)+1] <- n[i] - median(n[(i - lag):(i)], na.rm = TRUE)
                   } else {
                     m[length(m)+1] <- NA
                   }
                 }
                 
                 return( m )
                 }
             },
             subhistmedian={
               transformFunc <- function(x, lag, y = 0) {
                 n <- as.numeric(x)
                 m <- numeric()
                 
                 for (i in seq_along(n)) {
                   if (i == 1){
                     m[length(m)+1] <- NA
                   }
                   else {
                     m[length(m)+1] <- n[i] - median(n[1:i], na.rm = TRUE)
                   }
                   
                 }
                 
                 return( m )
                 }
             },
             crossmedian={
               transformFunc <- function(x, lag, y = 0) {
                 m <- as.numeric(x) - median(as.numeric(x), na.rm = TRUE)
                 return( m )
                 }
             },
             perchg={
               transformFunc <- function(x, lag, y = 0) { return(  as.numeric(Delt(as.numeric(x), k = lag))) }
             },
             perchgmedian={
               transformFunc <- function(x, lag, y = 0) {
                 
                 d <- c(rep(NA, lag), diff(as.numeric(x), lag = lag))
                 
                 m <- numeric()
                 
                 for (i in seq_along(d)) {
                   m[length(m)+1] <- d[i] / median(x[1:i], na.rm = TRUE)
                 }
                 
                 return( m )
                 }
             },
             perchgstd={
               transformFunc <- function(x, lag, y = 0) {
                 d <- c(rep(NA, lag), diff(as.numeric(x), lag = lag))
                 
                 m <- numeric()
                 
                 for (i in seq_along(d)) {
                   m[length(m)+1] <- d[i] / sd(x[1:i], na.rm = TRUE)
                 }
                 
                 return( m )
                 }
             },
             zscorelong={
               transformFunc <- function(x, lag, y = 0) {
                 n <- as.numeric(x)
                 
                 m <- numeric()
                 for (i in seq_along(n)) {
                   m[length(m)+1] <- (n[i] - mean(n[1:i], na.rm = TRUE)) / sd(n[1:i], na.rm = TRUE)
                 }
                 
                 return( m )
                 }
             },
             zscorecross={
               transformFunc <- function(x, lag, y = 0) {
                 n <- as.numeric(x)
                 m <- (n - mean(n, na.rm = TRUE)) / sd(n, na.rm = TRUE)
                 return( m )
                 }
             },
             binarystring={
               transformFunc <- function(x, lag, y = 0) {
                 lowerX <- tolower(as.character(x))
                 m <- rep(0, length(x))
                 m[lowerX == tolower(transformBinaryString)] <- 1
                 return( m )
                 }
             },
             binaryvalue={
               transformFunc <- function(x, lag, y = 0) {
                 n <- as.numeric(x)
                 m <- rep(0, length(x))
                 m[n >= as.numeric(transformBinaryValue)] <- 1
                 return( m )
               }
             },
             offsetfwd={
               transformFunc <- function(x, lag, y = 0) { 
                 return( c(rep(NA, lag), x[1:(length(x) - lag)]) )
                 }
             },
             offsetbwd={
               transformFunc <- function(x, lag, y = 0) {
                 return( c(x[(lag + 1):length(x)], rep(NA, lag)) )
                 }
             }
      )
      
      if(is.null(catCols)) {
        # Run transformation based on whether or not category column was selected.
        # No Category columns#
        
        # Order DF by date column, if present
        if (!is.null(dateCol) && dateCol != "") {
          df[,dateCol] <- format(as.Date(as.character(df[, dateCol]), format = dateColFormat),dateColFormat)
          df <- df[order(df[, dateCol]), ]
        }
        
        for (col in cols){
          transformName <- paste0(col, "_", transformSuffix)
          if(length(names(df)[names(df) == transformName]) > 0) {
            transformName = paste0(transformName, length(names(df)[names(df) == transformName]))
          }
          
          df[, transformName] <- transformFunc(as.numeric(df[,col]), lag)
        }
        
      } else {
        # Category columns #
        # Order by category cols then date col.
        # This step is needed to ensure unlisted aggregate data is in proper order
        # Order DF by date column, if present
        
        if (!is.null(dateCol) && dateCol != "") {
          df[,dateCol] <- format(as.Date(as.character(df[, dateCol]), format = dateColFormat),dateColFormat)
          df <- df[do.call(order, df[c(rev(catCols),dateCol)]), ] #rev() reverses the category column vector, because aggregate() sorts using last in first out
        } else {
          df <- df[do.call(order, df[rev(catCols)]), ] #rev() reverses the category column vector, because aggregate() sorts using last in first out
        }
        
        
        groupList <- list()
        for (col in catCols){
          groupList[[col]] <- df[, col] 
        }
        
        for (col in cols){
          transformName <- paste0(col, "_", transformSuffix)
          if(length(names(df)[names(df) == transformName]) > 0) {
            transformName = paste0(transformName, length(names(df)[names(df) == transformName]))
          }
          df[, transformName] <- unlist(aggregate(df[,col], by=groupList, function(x) transformFunc(as.numeric(x), lag), simplify=FALSE)[["x"]])
        }
        
        # reorder DF back to user-selected order
        if (!is.null(dateCol) && dateCol != "") {
          df <- df[do.call(order, df[c(catCols,dateCol)]), ]
        } else {
          df <- df[do.call(order, df[catCols]), ]
        }
      }
      
    }
    vals$transformColIndex <- firstCol:ncol(df)
    if (!is.null(dateCol) && dateCol != "") {
      df[,dateCol] <- as.character(df[,dateCol])
    }
    vals$datadf <- df
    vals$refreshInputs(session, input, vals)
    
    if(alert) {
      shinyalert(
        title = "",
        text = "New metrics created. You can find them at the end of the table in the 'Data Preview' tab.",
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
    }
  }
  
  # Create Transformations Button
  observeEvent(input$applyTransformations, {
    
    createTransformations(alert=TRUE)
    
  })
  
  # Clear Filters Button
  observeEvent(input$filterClear, {

    createTransformations(alert=FALSE)
    
  })
  
  # Clear Aggregation Button
  observeEvent(input$aggClear, {

    createTransformations(alert=FALSE,dateformat=TRUE)
    
  })
  
}

observeClearTransformations <- function(input, output, session, vals) {
  
  # Clear Transformations Button
  observeEvent(input$transformationsClear, {
    removeUI(".transformation", multiple = TRUE)
    tcolIndex <- vals$transformColIndex
    if(!is.null(tcolIndex)){
      vals$datadf <- vals$datadf[, -tcolIndex]
    }
    vals$transformationCount <- 0
    vals$transformColIndex <- NULL
    vals$refreshInputs(session, input, vals)
  })
}
