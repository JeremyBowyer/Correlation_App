observeAddTransformation <- function(input, output, session, vals) {
  
  observeEvent(input$addTransformation, {
    
    #### Add UI Elements ####
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
           perchgstd = {
             insertUI(
               selector="#transformations",
               where="afterBegin",
               ui = tags$div(
                 h3(transformationName),
                 tags$div(textInput(paste0("transformationType", cnt), label = NULL, value=transformation),style="display:none;"),
                 textInput(paste0("transformationSuffix", cnt), "Column Suffix Name", value = ""),
                 numericInput(paste0("transformationLag", cnt), "Select Lag", value = 1, min = 1), 
                 selectInput(paste0("transformCols", cnt), "Select columns to transform", choices=vals$getCols(), multiple = TRUE),
                 selectInput(paste0("transformDateCol", cnt), "Select column to transform along (probably a date)", choices=vals$getCols()),
                 textInput(paste0("transformDateColFormat", cnt), "Format dates are in (check 'Data Preview' tab)", vals$dateFormat),
                 selectInput(paste0("transformCategoryCol", cnt), "Select category columns to group by (optional)", choices=vals$getCols(), multiple = TRUE),
                 tags$hr(), class="transformation")
               )
           },
           crossmedian = ,
           zscorecross = { 
             insertUI(
               selector="#transformations",
               where="afterBegin",
               ui = tags$div(
                 h3(transformationName),
                 tags$div(textInput(paste0("transformationType", cnt), label = NULL, value=transformation),style="display:none;"),
                 textInput(paste0("transformationSuffix", cnt), "Column Suffix Name", value = ""),
                 selectInput(paste0("transformCols", cnt), "Select columns to transform", choices=vals$getCols(), multiple = TRUE),
                 selectInput(paste0("transformCategoryCol", cnt), "Select category columns (probably date) to group by (optional)", choices=vals$getCols(), multiple = TRUE),
                 tags$hr(), class="transformation")
               )
           },
           subhistmedian = , 
           zscorelong = {
             insertUI(
               selector="#transformations",
               where="afterBegin",
               ui = tags$div(
                 h3(transformationName),
                 tags$div(textInput(paste0("transformationType", cnt), label = NULL, value=transformation),style="display:none;"),
                 textInput(paste0("transformationSuffix", cnt), "Column Suffix Name", value = ""),
                 selectInput(paste0("transformCols", cnt), "Select columns to transform", choices=vals$getCols(), multiple = TRUE),
                 selectInput(paste0("transformDateCol", cnt), "Select column to transform along (probably a date)", choices=vals$getCols()),
                 textInput(paste0("transformDateColFormat", cnt), "Format dates are in (check 'Data Preview' tab)", vals$dateFormat),
                 selectInput(paste0("transformCategoryCol", cnt), "Select category columns to group by (optional)", choices=vals$getCols(), multiple = TRUE),
                 tags$hr(), class="transformation")
               )
           },
           binarystring = {
             insertUI(
               selector="#transformations",
               where="afterBegin",
               ui = tags$div(
                 h3(transformationName),
                 tags$div(textInput(paste0("transformationType", cnt), label = NULL, value=transformation),style="display:none;"),
                 textInput(paste0("transformationSuffix", cnt), "Column Suffix Name", value = ""),
                 selectInput(paste0("transformCols", cnt), "Select columns to transform", choices=vals$getCols(), multiple = TRUE),
                 textInput(paste0("transformString", cnt), "Value to be flagged as 1", value = ""),
                 tags$hr(), class="transformation")
               )
           },
           binaryvalue = {
             insertUI(
               selector="#transformations",
               where="afterBegin",
               ui = tags$div(
                 h3(transformationName),
                 tags$div(textInput(paste0("transformationType", cnt), label = NULL, value=transformation),style="display:none;"),
                 textInput(paste0("transformationSuffix", cnt), "Column Suffix Name", value = ""),
                 selectInput(paste0("transformCols", cnt), "Select columns to transform", choices=vals$getCols(), multiple = TRUE),
                 textInput(paste0("transformBinaryValue", cnt), "Value, data points equal to or above to be flagged as 1", value = ""),
                 tags$hr(), class="transformation")
               )
           },
          binarypercentile = {
            insertUI(
              selector="#transformations",
              where="afterBegin",
              ui = tags$div(
                h3(transformationName),
                tags$div(textInput(paste0("transformationType", cnt), label = NULL, value=transformation),style="display:none;"),
                textInput(paste0("transformationSuffix", cnt), "Column Suffix Name", value = ""),
                selectInput(paste0("transformCols", cnt), NULL, choices=vals$getCols()),
                sliderInput(paste0("transformationSlider", cnt), NULL, min=0, max=1, value=c(0,1), step=0.01),
                tags$div(tags$hr()), class="transformation")
              )
          },
          residual = {
            insertUI(
              selector="#transformations",
              where="afterBegin",
              ui = tags$div(
                h3(transformationName),
                tags$div(textInput(paste0("transformationType", cnt), label = NULL, value=transformation),style="display:none;"),
                textInput(paste0("transformationSuffix", cnt), "Column Suffix Name", value = ""),
                selectInput(paste0("transformCols", cnt), "Select x columns", choices=vals$getCols(), multiple = TRUE),
                selectInput(paste0("transformY", cnt), "Select y column", choices=vals$getCols()),
                tags$hr(), class="transformation")
              )
          },
          offsetfwd = ,
          offsetbwd = {
            insertUI(
              selector="#transformations",
              where="afterBegin",
              ui = tags$div(
                h3(transformationName),
                tags$div(textInput(paste0("transformationType", cnt), label = NULL, value=transformation),style="display:none;"),
                textInput(paste0("transformationSuffix", cnt), "Column Suffix Name", value = ""),
                numericInput(paste0("transformationLag", cnt), "Select Lag", value = 1, min = 1), 
                selectInput(paste0("transformCols", cnt), "Select columns to offset", choices=vals$getCols(), multiple = TRUE),
                selectInput(paste0("transformDateCol", cnt), "Select column to offset along (probably a date)", choices=vals$getCols()),
                textInput(paste0("transformDateColFormat", cnt), "Format dates are in (check 'Data Preview' tab)", vals$dateFormat),
                selectInput(paste0("transformCategoryCol", cnt), "Select category columns to group by (optional)", choices=vals$getCols(), multiple = TRUE),
                tags$hr(), class="transformation")
              )
          },
          ctc = {
            insertUI(
              selector="#transformations",
              where="afterBegin",
              ui = tags$div(
                h3(transformationName),
                tags$div(textInput(paste0("transformationType", cnt), label = NULL, value=transformation),style="display:none;"),
                textInput(paste0("transformationSuffix", cnt), "Resulting column name (required)", value = ""),
                selectInput(paste0("transformCols", cnt), NULL, choices=vals$getCols()),
                selectInput(paste0("transformOperator", cnt), NULL, choices = list("+", "-", "/", "*")),
                selectInput(paste0("transformY", cnt), NULL, choices=vals$getCols()),
                tags$hr(), class="transformation")
              )
          },
          ca = {
            insertUI(
              selector="#transformations",
              where="afterBegin",
              ui = tags$div(
                h3(transformationName),
                tags$div(textInput(paste0("transformationType", cnt), label = NULL, value=transformation),style="display:none;"),
                textInput(paste0("transformationSuffix", cnt), "Resulting column name (required)", value = ""),
                selectInput(paste0("transformCols", cnt), "Column", choices=vals$getCols()),
                selectInput(paste0("transformOperator", cnt), "Operator", choices = list("+", "-", "/", "*")),
                numericInput(paste0("transformBinaryValue", cnt), "Amount", value = 0),
                tags$hr(), class="transformation")
              )
          },
          dateagg = {
            insertUI(
              selector="#transformations",
              where="afterBegin",
              ui = tags$div(
                h3(transformationName),
                tags$div(textInput(paste0("transformationType", cnt), label = NULL, value=transformation),style="display:none;"),
                textInput(paste0("transformationSuffix", cnt), "Resulting column name (required)", value = ""),
                selectInput(paste0("transformCols", cnt), "Select date column to be aggregated", choices=vals$getCols()),
                textInput(paste0("transformDateColFormat", cnt), "Format dates are in (check 'Data Preview' tab)", vals$dateFormat),
                selectInput(paste0("transformAggregationLevel", cnt), "Aggregation Level (your data must be more granular than selected level)", choices = aggregationLevelList),
                tags$hr(), class="transformation")
              )
          },
          bucket = {
            insertUI(
              selector="#transformations",
              where="afterBegin",
              ui = tags$div(
                h3(transformationName),
                tags$div(textInput(paste0("transformationType", cnt), label = NULL, value=transformation),style="display:none;"),
                textInput(paste0("transformationSuffix", cnt), "Resulting column name (required)", value = ""),
                selectInput(paste0("transformBucketCols", cnt), "Columns to bucket", choices=vals$getCols(), multiple=TRUE),
                selectInput(paste0("transformBucketAggregation", cnt), "Bucket Method", choices = bucketFuncList),
                tags$hr(), class="transformation")
              )
          },
          quintile = {
            insertUI(
              selector="#transformations",
              where="afterBegin",
              ui = tags$div(
                h3(transformationName),
                tags$div(textInput(paste0("transformationType", cnt), label = NULL, value=transformation),style="display:none;"),
                textInput(paste0("transformationSuffix", cnt), "Column Suffix Name", value = ""),
                selectInput(paste0("transformCols", cnt), "Columns to transform into quintile", choices=vals$getCols(), multiple=TRUE),
                selectInput(paste0("transformCategoryCol", cnt), "Select category columns to group by (optional)", choices=vals$getCols(), multiple = TRUE),
                tags$hr(), class="transformation")
              )
          },
          
          binarymultiple = {
            insertUI(
              selector="#transformations",
              where="afterBegin",
              ui = tags$div(
                h3(transformationName),
                tags$div(textInput(paste0("transformationType", cnt), label = NULL, value=transformation),style="display:none;"),
                textInput(paste0("transformationSuffix", cnt), "Column Suffix Name (optional)", value = ""),
                selectInput(inputId=paste0("transformColsMult",cnt), "columns", choices=vals$getCols(),multiple = TRUE), 
                checkboxInput(inputId=paste0("transformCheckbox",cnt), "Treat NAs as 0s", value = FALSE),
                tags$hr(), class="transformation")
              )
          }
        )
  }) 
  
}

observeCreateTransformations <- function(input, output, session, vals) {

  createTransformations <- function(alert, dateformat=FALSE){

    # tryCatch({
      
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
        cols <- if(is.null(cols)){""} else{cols}
        dateCol <- input[[paste0("transformDateCol", cnt)]]
        reverse <- input[[paste0("switch", cnt)]]

        # Grab date format from date aggregation, if data is aggregated by date
        if(dateformat) {
          updateTextInput(session, paste0("transformDateColFormat", cnt), value = input$dateAggDateColFormat)
          dateColFormat <- input$dateAggDateColFormat
        } else {
          dateColFormat <- input[[paste0("transformDateColFormat", cnt)]]
        }
        
        # If there is a valid date column, make sure it has a Day
        # component, add one if necessary.
        if (!is.null(dateCol) && dateCol != "") {
          if(length(grep("%d", dateColFormat)) == 0){
            fullDateFormat <- paste0('%d-', dateColFormat)
            df[,dateCol] <- paste0("1-", as.character(df[, dateCol]))
          } else {
            fullDateFormat <- dateColFormat
          }
  
          df[,dateCol] <- as.Date(as.character(df[, dateCol]), format = fullDateFormat)
          if(!vals$validateDates(df[,dateCol])) return(NULL)
          df <- df[order(df[, dateCol]), ]
        }

        # Grab all possible transformation column values
        catCols <- input[[paste0("transformCategoryCol", cnt)]]
        lag <- input[[paste0("transformationLag", cnt)]]
        transformCheckbox <- input[[paste0("transformCheckbox", cnt)]]
        transformSuffix <- gsub(" ", ".", input[[paste0("transformationSuffix", cnt)]])
        transformString <- input[[paste0("transformString", cnt)]]
        transformBinaryValue <- input[[paste0("transformBinaryValue", cnt)]]
        transformSlider <- input[[paste0("transformationSlider", cnt)]]
        transformY <- input[[paste0("transformY", cnt)]]
        transformOp <- input[[paste0("transformOperator", cnt)]]
        transformAggLvl <- input[[paste0("transformAggregationLevel", cnt)]]
        transformBucketCols <- input[[paste0("transformBucketCols", cnt)]]
        transformBucketAggregation <- input[[paste0("transformBucketAggregation", cnt)]]
        transformInputCols <- input[[paste0("transformColsMult", cnt)]]

        if(type %in% c("ctc", "dateagg", "bucket") && transformSuffix == "") {
          shinyalert(
            title = "",
            text = "Please provide a name for the resulting column.",
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
        
        #### Assign Function ####
        # Assign transformation function based on type selected
        switch(type,
               #### Linear Residual ####
               residual = {
                 transformFunc <- function(x, y, ...) { 
                   x <- as.numeric(x)
                   y <- as.numeric(y)
                   residual <- (y - coef(lm(y ~ x))["x"]*x - coef(lm(y ~ x))["(Intercept)"])
                   return(residual)
                   }
               },
               #### Difference ####
               diff={
                 transformFunc <- function(x, lag, ...) { return( c(rep(NA, lag), diff(as.numeric(x), lag = lag)) ) }
               },
               #### Rolling Sum ####
               rollingsum={   
                 transformFunc <- function(x, lag, ...) {
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
               #### Subtract rolling median ####
               submedian={
                 transformFunc <- function(x, lag, ...) {
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
               #### Sub Historical Median ####
               subhistmedian={
                 transformFunc <- function(x, lag, ...) {
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
               #### Sub Cross Sectional Median ####
               crossmedian={
                 transformFunc <- function(x, lag, ...) {
                   m <- as.numeric(x) - median(as.numeric(x), na.rm = TRUE)
                   return( m )
                   }
               },
               #### Percent Chg ####
               perchg={
                 transformFunc <- function(x, lag, ...) {
                   result = as.numeric(Delt(as.numeric(x), k = lag))
                   return( result )
                   }
               },
               #### Percent Chg Median ####
               perchgmedian={
                 transformFunc <- function(x, lag, ...) {
                   
                   d <- c(rep(NA, lag), diff(as.numeric(x), lag = lag))
                   
                   m <- numeric()
                   
                   for (i in seq_along(d)) {
                     m[length(m)+1] <- d[i] / median(x[1:i], na.rm = TRUE)
                   }
                   
                   return( m )
                   }
               },
               #### Percent Chg Std ####
               perchgstd={
                 transformFunc <- function(x, lag, ...) {
                   d <- c(rep(NA, lag), diff(as.numeric(x), lag = lag))
                   
                   m <- numeric()
                   
                   for (i in seq_along(d)) {
                     m[length(m)+1] <- d[i] / sd(x[1:i], na.rm = TRUE)
                   }
                   
                   return( m )
                   }
               },
               #### Z-Score Longitudinal ####
               zscorelong={
                 transformFunc <- function(x, lag, ...) {
                   n <- as.numeric(x)
                   
                   m <- numeric()
                   for (i in seq_along(n)) {
                     dp <- sum(!is.na(n[1:i]))
                     if(dp < 3) {
                       m[length(m)+1] <- NA
                       next
                     }
                     m[length(m)+1] <- (n[i] - mean(n[1:i], na.rm = TRUE)) / sd(n[1:i], na.rm = TRUE)
                   }
                   
                   return( m )
                   }
               },
               #### Z-Score Cross Sectional ####
               zscorecross={
                 transformFunc <- function(x, lag, ...) {
                   n <- as.numeric(x)
                   m <- (n - mean(n, na.rm = TRUE)) / sd(n, na.rm = TRUE)
                   return( m )
                   }
               },
               #### Binary (String) ####
               binarystring={
                 transformFunc <- function(x, transformString, ...) {
                   lowerX <- tolower(as.character(x))
                   m <- rep(0, length(x))
                   m[lowerX == tolower(transformString)] <- 1
                   m[is.na(lowerX)] <- NA
                   return( m )
                   }
               },
               #### Binary (Value) ####
               binaryvalue={
                 transformFunc <- function(x, transformValue, ...) {
                   n <- as.numeric(x)
                   m <- rep(0, length(x))
                   m[n >= as.numeric(transformValue)] <- 1
                   m[is.na(n)] <- NA
                   return( m )
                 }
               },
               #### Binary (Percentile) ####
               binarypercentile={
                 transformFunc <- function(x, transformSlider, ...) {
                   n <- as.numeric(x)
                   d <- rep(NA, length(n))
                   
                   sliderMin <- as.numeric(transformSlider[1])
                   sliderMax <- as.numeric(transformSlider[2])
                   
                   if(is.na(sliderMin)) sliderMin <- 0
                   if(is.na(sliderMax)) sliderMax <- 1
                   
                   sliderMin <- quantile(n, sliderMin, na.rm = TRUE)
                   sliderMax <- quantile(n, sliderMax, na.rm = TRUE)
                   
                   d[n >= sliderMin &  n <= sliderMax] <- 1
                   d[n < sliderMin | n > sliderMax] <- 0
                   return( d )
                 }
               },
               #### Offset Forward ####
               offsetfwd={
                 transformFunc <- function(x, lag, ...) {
                   return( c(rep(NA, lag), x[1:(length(x) - lag)]) )
                   }
               },
               #### Offset Backward ####
               offsetbwd={
                 transformFunc <- function(x, lag, ...) {
                   return( c(x[(lag + 1):length(x)], rep(NA, lag)) )
                   }
               },
               #### Column to Column ####
               ctc={
                 transformFunc <- function(x, y, op, ...) {
                   return(.Primitive(op)(as.numeric(x),as.numeric(y)))
                 }
               },
               #### Column Arithmetic ####
               ca={
                 transformFunc <- function(x, transformValue, op, ...) {
                   return(.Primitive(op)(as.numeric(x),as.numeric(transformValue)))
                 }
               },
               #### Date Aggregation ####
               dateagg={
                 transformFunc <- function(x, aggLvl, dateFormat, ...) {
                   d <- format(as.Date(as.character(x), format = dateFormat),aggLvl)
                   if(!vals$validateDates(d)) return(FALSE)
                   return(d)
                 }
               },
               #### Bucket Columns ####
               bucket={
                 transformFunc <- function(df, transformBucketCols, transformBucketAggregation, ...) {
                   bucket_df <- df[,transformBucketCols]
                   switch(transformBucketAggregation,
                          sum={
                            n <- rowSums(bucket_df, na.rm=TRUE)
                            m <- rowMeans(bucket_df,na.rm=TRUE)
                            n <- replace(n,is.nan(m),NA)
                          },
                          average={
                            n <- rowMeans(bucket_df, na.rm=TRUE)
                          },
                          binary={
                            n <- rowSums(bucket_df, na.rm=TRUE)
                            m <- rowMeans(bucket_df,na.rm=TRUE)
                            n <- replace(n,is.nan(m),NA)
                            n <- replace(n, n<=0, 0)
                            n <- replace(n, n>0, 1)
                            
                          })
                   return(n)
                 }
               },
               #### Quintile ####
               quintile={
                 transformFunc <- function(x,...) {
                   n <- as.numeric(x)
                   q <-quint(n)
                     
                   return(q)
                 }
               },
               #### Binary Multiple ####
               binarymultiple={
                 transformFunc <- function(df, transformInputCols, transformCheckbox, ...) {
                   flagsNeeded <- length(transformInputCols)
                   multDF <- if(flagsNeeded > 1) df[,transformInputCols] else data.frame(onlycol = df[,transformInputCols])

                   for(col in names(multDF)){
                     multDF[,col] <- as.numeric(multDF[,col])
                     if(transformCheckbox) multDF[,col] <- as.numeric(replace(multDF[,col], is.na(multDF[,col]), 0))
                   }

                   sums <- rowSums(multDF)
                   res <- replace(sums, !is.na(sums) & sums < flagsNeeded, 0)
                   res <- replace(res, res > 0, 1)
                   
                   return(res)
                 }
               }
        )
  
        if(is.null(catCols)) {
          # Run transformation based on whether or not category column was selected.
          # No Category columns#
          
          # Order DF by date column, if present
          if (!is.null(dateCol) && dateCol != "") {
            df <- df[order(df[, dateCol]), ]
            df[,dateCol] <- format(df[,dateCol],dateColFormat)
          }
         for (col in cols) {
           transformName <- vals$getTransformName(type, col, transformSuffix, transformInputCols)
           
           if(length(names(df)[names(df) == transformName]) > 0) {
             transformName = paste0(transformName, length(names(df)[names(df) == transformName]))
           }

           xCol <- if(col %in% names(df)) df[,col] else character()
           
           transformRes <- transformFunc(df=df,
                                         x=xCol,
                                         lag=lag,
                                         y=df[,transformY],
                                         op=transformOp,
                                         aggLvl=transformAggLvl,
                                         dateFormat=dateColFormat,
                                         transformCheckbox=transformCheckbox,
                                         transformString=transformString,
                                         transformValue=transformBinaryValue,
                                         transformSlider=transformSlider,
                                         transformBucketCols=transformBucketCols,
                                         transformBucketAggregation=transformBucketAggregation,
                                         transformInputCols=transformInputCols
                                         )
           
           if(length(transformRes) <= 1 && transformRes == FALSE) return(NULL)
           df[, transformName] <- transformRes
         }
        } else {
          # Category columns #
          # Order by category cols then date col.
          # This step is needed to ensure unlisted aggregate data is in proper order
          # Order DF by date column, if present
          if (!is.null(dateCol) && dateCol != "") {
            df <- df[do.call(order, df[c(rev(catCols),dateCol)]), ] #rev() reverses the category column vector, because aggregate() sorts using last in first out
          } else {
            df <- df[do.call(order, df[rev(catCols)]), ] #rev() reverses the category column vector, because aggregate() sorts using last in first out
          }
          
          df[,dateCol] <- format(df[,dateCol],dateColFormat)
          
          groupList <- list()
          for (col in catCols){
            groupList[[col]] <- df[, col] 
          }
          
          for (col in cols){
            transformName <- vals$getTransformName(type, col, transformSuffix, transformInputCols)
            if(length(names(df)[names(df) == transformName]) > 0) {
              transformName = paste0(transformName, length(names(df)[names(df) == transformName]))
            }
            df[, transformName] <- unlist(aggregate(df[,col], by=groupList, function(x) transformFunc(x=as.numeric(x), lag=lag), simplify=FALSE)[["x"]])
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
    # },error=function(e) {
    #   if(DEBUG_MODE) {
    #     stop(e)
    #   }
    #   shinyerror(e)
    # })
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
    inserted <- c()
    vals$inputcount = 0
    shinyalert(
      title = "",
      text = "Your transformations have been removed. Any filters you have applied based on those transformations are still applied.",
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
    
  })
}


