options(java.parameters = "-Xmx12048m") 
library(shiny)
library(shinyalert) # https://github.com/daattali/shinyalert
library(shinyWidgets)
library(shinyFiles)
library(plotly)
library(ggplot2)
library(dplyr)
library(reshape2)
library(DT)
library(lubridate)
library(rpart)
library(rpart.plot)
library(magrittr)
library(quantmod)
library(grid)
library(ggforce)
library(Vennerable)
library(diagram)
options(shiny.deprecation.messages=FALSE)
options(stringsAsFactors=FALSE)
options(shiny.maxRequestSize=10000*1024^2) 
options(digits=4)

source("global.R", local=TRUE)
source("server-modules/filters.R", local=TRUE)
source("server-modules/transformations.R", local=TRUE)
source("server-modules/metric-dive.R", local=TRUE)
source("server-modules/download-custom-data.R", local=TRUE)
source("server-modules/download-comparison-data.R", local=TRUE)
source("server-modules/download-report.R", local=TRUE)
source("server-modules/run-analysis.R", local=TRUE)
source("server-modules/upload-data.R", local=TRUE)
source("server-modules/methods.R", local=TRUE)
source("server-modules/aggregation.R", local=TRUE)
source("server-modules/data-preview.R", local=TRUE)
source("server-modules/performance.R",local=TRUE)
source("server-modules/conditions.R",local=TRUE)
source('server-modules/venndiagram.r',local=TRUE)

# Define Functions
source("https://raw.githubusercontent.com/JeremyBowyer/Quintile-Function/master/Quintile_Function.R")

conditionalFormattingScript <- readChar('highlight_cells.js', file.info('highlight_cells.js')$size)

shiny <- shinyServer(function(input, output, session) {
  
  #### Methods ####
  loadMethods(input, output, session, vals)
  
  #### Upload File ####
  observeUploadData(input, output, session, vals)
  
  #### Option Panel Conditions ####
  loadConditions(input, output, session, vals)
  
  #### Filters ####
  observeAddFilter(input, output, session, vals)
  observeApplyFilters(input, output, session, vals)
  
  #### Date Aggregation ####
  observeAggregateData(input, output, session, vals)
  
  #### Clear Filter & Clear Agg ####
  observeClearFilters(input, output, session, vals)
  observeClearAgg(input, output, session, vals)
  
  #### Transformations ####
  observeAddTransformation(input, output, session, vals)
  observeCreateTransformations(input, output, session, vals)
  observeClearTransformations(input, output, session, vals)
  
  #### Download Mechanics ####
  observeDownloadCustomData(input, output, session, vals)
  observeDownloadComparisonData(input, output, session, vals)
  observeDownloadReport(input, output, session, vals)
  
  #### Metric Comparison ####
  observeRunAnalysis(input, output, session, vals)
  correlPlots(input, output, session, vals)
  
  #### Metric Dive ####
  observeMetricDiveFilters(input, output, session, vals)
  metricDivePlots(input, output, session, vals)
  processMetricDiveDF(input, output, session, vals)
  calculateMetricStats(input, output, session, vals)
  
  #### Data Preview Screen ####
  dataPreview(input, output, session, vals)
  
  
  session$onSessionEnded(function() {
      stopApp()
  })
  
  #### JavaScript Conditional Formatting ####
  session$onFlushed(function() {
    session$sendCustomMessage(type='conditionalFormatting', list(value = conditionalFormattingScript))
  }, FALSE)
  
})



