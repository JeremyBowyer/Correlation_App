DEBUG_MODE = FALSE

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
    IsAggregated = FALSE,
    inputList <- c("hierCol", "yCol", "dateCol", "categoryCol", "ignoreCols", "multiCols", "xCol", "dateAggDateCol", "groupByCols")
    )

    
transformationList = list("Difference" = "diff",
                          "Rolling Sum" = "rollingsum",
                          "Subtract Rolling Median" = "submedian",
                          "Subtract Historical Median" = "subhistmedian",
                          "Subtract Cross Sectional Median" = "crossmedian",
                          "% Change" = "perchg",
                          "% Change from Median" = "perchgmedian",
                          "% Change from Std" = "perchgstd",
                          "Z-Score Cross Sectional" = "zscorecross",
                          "Z-Score Longitudinal" = "zscorelong",
                          "Binary (String)" = "binarystring",
                          "Binary (Value)" = "binaryvalue",
                          "Binary (Percentile)" = "binarypercentile",
                          "Linear Residual" = "residual",
                          "Offset Forward" = "offsetfwd",
                          "Offset Backward" = "offsetbwd",
                          "Column to Column" = "ctc",
                          "Date Aggregation" = "dateagg")

filterList = list("Value Filter" = "valueFilter",
                  "Percentile Filter" = "percentileFilter",
                  "Date Filter" = "dateFilter")

aggregationLevelList = list("Month" = "%Y/%m",
                            "Year" = "%Y")

aggregationFuncList = list("Sum" = "sum",
                           "Average" = "average",
                           "Earliest" = "earliest",
                           "Latest" = "latest",
                           "Lowest Number" = "min",
                           "Highest Number" = "max")

shinyerror <- function(e){
    shinyalert(
        title = "An error occurred",
        text = e$message,
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

tryStack <- function(expr,silent=FALSE) {
  tryenv <- new.env()
  out <- try(withCallingHandlers(expr, error=function(e){
    shinyerror(e)
    stack <- sys.calls()
    stack <- stack[-(2:7)]
    stack <- head(stack, -2)
    stack <- sapply(stack, deparse)
    if(!silent && isTRUE(getOption("show.error.messages"))) {
      assign("stackmsg", value=paste(stack,collapse="\n"), envir=tryenv)
    }
    }), silent=silent)
  if(inherits(out, "try-error")) out[2] <- tryenv$stackmsg
  out
}