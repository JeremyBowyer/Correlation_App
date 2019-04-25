calculatePerformanceBinary <- function(df, xCol, yCol, dateCol, dateFormat, include_df=FALSE) {
  
  #
  # Function to calculate the performance differential of a given X column,
  # if that column is binary (only has 2 unique values)
  #
  # Returns a dataframe containing the average Y value for the
  # two unique values of X, and the difference between them.
  #
  
  # Subset df by complete cases
  if (dateCol == "") {
    
    df <- df[,c(xCol,yCol)]
    df <- df[complete.cases(df), ]
    
  } else {
    
    df <- df[,c(xCol,yCol,dateCol)]
    df <- df[complete.cases(df), ]
    
  }
  
  if(nrow(df) == 0) { return(data.frame(All=NA)) }
  
  binary_values <- unique(df[,xCol])
  binary_values <- binary_values[order(binary_values)]
  
  if(length(binary_values) != 2) {
    print("Data not binary. Unique values:")
    return(data.frame(All=NA))
  }

  allPerformance <- data.frame(Value = binary_values, stringsAsFactors=FALSE)
  
  allPerformance[,"All"] <- NA
  
  allPerformance[1, "All"] <- mean(as.numeric(df[df[,xCol] == binary_values[1],yCol]), na.rm=TRUE)
  allPerformance[2, "All"] <- mean(as.numeric(df[df[,xCol] == binary_values[2],yCol]), na.rm=TRUE)
  allPerformance[3, ] <- c("Differential", allPerformance[1, "All"] - allPerformance[2, "All"])
  
  if(include_df) { return(list(df=df, allPerformance=allPerformance)) }
  
  return(allPerformance)
  
}


calculatePerformanceDatesBinary <- function(df, xCol, yCol, dateCol, dateFormat) {
  
  #
  # Function to calculate the performance differential of a given X column,
  # if that column is binary (only has 2 unique values)
  #
  # Returns a dataframe containing the average Y value for the
  # two unique values of X, and the difference between them.
  #
  # A column for each date is included, in addition to an "All" column
  #
    
  # Grab initial performance table with "All" column
  allPerformance <- calculatePerformanceBinary(df, xCol, yCol, dateCol, dateFormat) 
  # If there is no date, no need to add columns for each date
  if(dateCol == "") { return(allPerformance) }
  
  # If data isn't binary, return NULL, with explanation
  binary_values <- unique(df[,xCol])
  binary_values <- binary_values[order(binary_values)]
  if(length(binary_values) != 2) {
    print("Data not binary. Unique values:")
    print(binary_values)
    return(data.frame(All=NA))
  }
  
  # Loop through each date and calculate average Y value and differential
  dates <- parse_date_time(as.character(df[, dateCol]), order=dateFormat)
  dateChars <- unique(format(dates[order(dates)], dateFormat))
  
  for(date in dateChars) {
    
    datedf <- df[format(dates, dateFormat) == date, ]
    allPerformance[, as.character(date)] <- NA
    
    allPerformance[1, as.character(date)] <- mean(as.numeric(datedf[datedf[,xCol] == binary_values[1],yCol]), na.rm=TRUE)
    allPerformance[2, as.character(date)] <- mean(as.numeric(datedf[datedf[,xCol] == binary_values[2],yCol]), na.rm=TRUE)
    allPerformance[3, as.character(date)] <- allPerformance[1, as.character(date)] - allPerformance[2, as.character(date)]

  }

  return(allPerformance)
  
}


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
    
    if(nrow(df) == 0) { return(data.frame(All=NA)) }
    
    df[,"quints"] <- NA
    df$quints <- quint(x=as.numeric(df[,xCol]))
    
  } else {
    
    df <- df[,c(xCol,yCol,dateCol)]
    df <- df[complete.cases(df), ]
    
    if(nrow(df) == 0) { return(data.frame(All=NA)) }
    
    df[,"quints"] <- NA
    
    aggs <- by(df, INDICES = df[, dateCol], function(x) {

      tryCatch({
        x[,"quints"] <- quint(as.numeric(x[,xCol]))
      }, error = function(e) {
        print(e)
        x[,"quints"] <- rep(NA, nrow(x))
      })

      return(x)
    
    })
    
    df <- do.call("rbind", aggs)
  }
  
  # Check for binary data
  unique_vals = unique(df[,xCol])
  if(length(unique_vals) == 2) {
    return(calculatePerformanceBinary(df, xCol, yCol, dateCol, dateFormat, include_df=include_df))
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
  output <- calculatePerformance(df, xCol, yCol, dateCol, dateFormat, include_df=TRUE) 
  allPerformance <- output$allPerformance  

  # This dataframe is already quintiled appropriate
  df <- output$df
  # If there is no date, no need to add columns for each date
  if(dateCol == "") { return(allPerformance) }
  
  # Check for binary data
  unique_vals = unique(df[,xCol])
  if(length(unique_vals) == 2) {
    return(calculatePerformanceDatesBinary(df, xCol, yCol, dateCol, dateFormat))
  }
  
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