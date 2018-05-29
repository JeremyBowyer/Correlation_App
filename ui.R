library(shiny)
library(shinyalert) # https://github.com/daattali/shinyalert
library(shinythemes)
library(plotly)
library(dplyr)
library(XLConnect)
library(DT)

source("global.R", local=TRUE)
source("ui-modules/ui-options.R", local=TRUE)
source("ui-modules/ui-datapreview.R", local=TRUE)
source("ui-modules/ui-metriccomparison.R", local=TRUE)
source("ui-modules/ui-metricdive.R", local=TRUE)


shinyUI(
  navbarPage(
    title = "analyzeR",
    theme = shinytheme("sandstone"),
    fluid = TRUE,
    id = "mainTabset",
    optionsPage(),
    dataPreviewPage(),
    metricComparisonsPage(),
    metricDivePage()
  )
)
