
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinythemes)
library(XLConnect)
library(ggvis)
library(dplyr)

shinyUI(navbarPage(
  title = "Correlation App",
  theme = shinytheme("sandstone"),
  fluid = TRUE,
  id = "mainTabset",
  tabPanel("Options",
    value = "options",
    sidebarLayout(
      sidebarPanel(
        
        fileInput("csvfile", "Choose CSV File",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
        ),
        conditionalPanel(
          condition = "output.fileUploaded",
          tags$hr(),
          checkboxInput("hierBox", "Hierarchical?", value=FALSE),
          conditionalPanel(
            condition = "output.hierarchicalCheck",
            selectInput("hierCol", "Select First Layer column", choices=list())
          ),
          selectInput("yCol", "Select Y column", choices=list()),
          selectInput("dateCol", "Select Date column (optional)", choices=list()),
          selectInput("categoryCol", "Select Category column (optional)", choices=list()),
          selectInput("ignoreCols", "Select Columns to Ignore (optional)", choices=list(), multiple = TRUE),
          tags$hr(),
          tags$h2("Filters"),
          tags$p("Warning: Filtering by one column will be applied to entire dataset, and will affect all subsequent analysis, including Metric Dive tab."),
          tags$div(actionLink("addValueFilter", "Add Value Filter"), id="valueFilters", style="padding: 0px 5px 0px 5px; background: #e4dfd6; border: 1px solid #b5b3b0; margin: 10px 0 0 0; border-radius: 5px;"),
          tags$div(actionLink("addPercentileFilter", "Add Percentile Filter"), id="percentileFilters", style="padding: 0px 5px 0px 5px; background: #e4dfd6; border: 1px solid #b5b3b0; margin: 10px 0 0 0; border-radius: 5px;"),
          tags$div(actionLink("addDateFilter", "Add Date Filter"), id="dateFilters", style="padding: 0px 5px 0px 5px; background: #e4dfd6; border: 1px solid #b5b3b0; margin: 10px 0 0 0; border-radius: 5px;"),
          actionButton("applyFilters", "Apply Filters", icon("filter"), style="padding: 5px 10px 5px 10px;"),
          tags$br(),
          actionLink("filterClear", "Clear All Filters", style="color: #f12828;"),
          tags$hr(),
          actionButton("run", "Run Analysis", style="color: #fff; background-color: rgb(2, 140, 7); border: solid 1px #005a03;"),
          tags$hr(),
          tags$head(
            tags$script(HTML('Shiny.addCustomMessageHandler("jsCode", function(message) { eval(message.value); });')),
            tags$style(HTML(".shiny-notification {position: fixed; top: 150px; left: 200px;}"))
          )
        )
      ),
      mainPanel(tableOutput("dataPreview"))
    )
  ),
  
  tabPanel("Correlations",
        value="correlations",
        tabsetPanel(
          tabPanel("All", mainPanel(tableOutput("allCorrelations"))),
          tabPanel("By Date", mainPanel(tableOutput("dateCorrelations")))
        )
  ),
  tabPanel("Metric Dive", value="metric", mainPanel(width = 12,
    column(12, align="center",
      selectInput("xCol", "Select Metric", choices=list()),
      h3("Metric Plots"),
      tabsetPanel(
        tabPanel("Scatter", ggvisOutput("metricScatter")),
        tabPanel("Histogram", ggvisOutput("metricHist")),
        tabPanel("QQ - Normal Dist", ggvisOutput("metricQQnorm")),
        tabPanel("QQ - Y", ggvisOutput("metricQQy"))
      ),
      h3('ANOVA Table'),
      verbatimTextOutput('aovSummary')
      )
    )
  )
  
))
