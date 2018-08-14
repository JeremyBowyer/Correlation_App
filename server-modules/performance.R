calculatePerformance <- function(df, xCol, yCol, dateCol, dateFormat, include_df=FALSE) {
  
  #
  # Function to calculate the performance differential of a given column.
  #
  # Returns a dataframe containing the average Y value for a given
  # quintile of X value, and a differential between the top and bottom
  #
  
  # If date column is blank, quintile by entire vector
  # otherwise quintile by date
  if (dateCol == "") {
    
    df <- df[,c(xCol,yCol)]
    df <- df[complete.cases(df), ]
    
    if(nrow(df) == 0) { return(NA) }
    
    df[,"quints"] <- NA
    df$quints <- quint(as.numeric(df[,xCol]))
    
  } else {
    
    df <- df[,c(xCol,yCol,dateCol)]
    df <- df[complete.cases(df), ]
    
    if(nrow(df) == 0) { return(NA) }
    
    df[,"quints"] <- NA
    
    aggs <- by(df, INDICES = df[, dateCol], function(x) {
    
      tryCatch({
        x[,"quints"] <- quint(as.numeric(x[,xCol]))
      }, error = function(e) {
        x[,"quints"] <- rep(NA, nrow(x))
      })
      return(x)
    
    })
    
    df <- do.call("rbind", aggs)
  }
    
  # Calculate Performance, then Performance Differential
  allPerformance <- data.frame(Quintile = c("Q1 (Highest)", "Q2", "Q3", "Q4", "Q5 (Lowest)"), stringsAsFactors=FALSE)
  allPerformance[,"All"] <- NA
  
  quintMeans <- aggregate(df[, yCol], by = list(df$quints), function(x) mean(x, na.rm = TRUE))
  
  for(quint in quintMeans[,"Group.1"]){
    allPerformance[quint,"All"] <- quintMeans[grep(quint,quintMeans[,"Group.1"]),"x"]
  }
  performanceDifferential <- ((allPerformance[1, "All"] * 2 + allPerformance[2, "All"]) / 3) - ((allPerformance[5, "All"] * 2 + allPerformance[4, "All"]) / 3)
  allPerformance[6, "Quintile"] <- "Differential"
  allPerformance[6, "All"] <- performanceDifferential
  
  if(include_df) { return(list(df=df, allPerformance=allPerformance)) }
  
  return(allPerformance)
  
}


calculatePerformanceDates <- function(df, xCol, yCol, dateCol, dateFormat) {
  
  #
  # Function to calculate the performance differential of a given column.
  #
  # Returns a dataframe containing the average Y value for a given
  # quintile of X value, and a differential between the top and bottom.
  #
  # A column for each date is included, in addition to an "All" column
  #
  
  
  # Grab initial performance table with "All" column
  output <- calculatePerformanceDifferential(df, xCol, yCol, dateCol, dateFormat, include_df=TRUE) 
  allPerformance <- output$allPerformance
    
  # This dataframe is already quintiled appropriate
  df <- output$df

  # If there is no date, no need to add columns for each date
  if(dateCol == "") { return(allPerformance) }
  
  
  # Loop through each date and calculate average Y value and differential
  dates <- parse_date_time(as.character(df[, dateCol]), order=dateFormat)
  dateChars <- unique(format(dates[order(dates)], dateFormat))
  
  for(date in dateChars) {
    
    datedf <- df[format(dates, dateFormat) == date, ]
    allPerformance[, as.character(date)] <- NA
    
    tryCatch({
      
      datedf[, "quints"] <- quint(datedf[,xCol])
      aggs <- aggregate(datedf[, yCol], by = list(datedf$quints), function(x) mean(x, na.rm = TRUE))
      performanceDifferential <- ((aggs[1, "x"] * 2 + aggs[2, "x"]) / 3) - ((aggs[5, "x"] * 2 + aggs[4, "x"]) / 3)

      for(row in 1:nrow(aggs)){
        allPerformance[aggs[row, 1], as.character(date)] <- aggs[row, "x"]
      }
      allPerformance[6, as.character(date)] <- performanceDifferential

    }, error = function(e) {
      allPerformance[, as.character(date)] <- rep(NA,nrow(allPerformance))
    })

  }

  return(allPerformance)
  
}