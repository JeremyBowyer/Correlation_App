
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinythemes)
library(XLConnect)
library(plotly)
library(dplyr)

shinyUI(navbarPage(
  title = "Correlation App",
  theme = shinytheme("sandstone"),
  fluid = TRUE,
  id = "mainTabset",
  tabPanel("Instructions",
           value = "instructions",
           h2("Coming Soon.")),
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
          tabsetPanel(
            tabPanel("Column Selection",
              checkboxInput("hierBox", "Hierarchical?", value=FALSE),
              conditionalPanel(
                condition = "output.hierarchicalCheck",
                selectInput("hierCol", "Select First Layer column", choices=list())
              ),
              selectInput("yCol", "Select Y column", choices=list()),
              selectInput("dateCol", "Select Date column (optional)", choices=list()),
              selectInput("categoryCol", "Select Category column (optional)", choices=list()),
              selectInput("ignoreCols", "Select Columns to Ignore (optional)", choices=list(), multiple = TRUE),
              tags$hr()),
            tabPanel("Filters",
                     tags$p("Warning: Filtering by one column will be applied to entire dataset, and will affect all subsequent analysis, including Metric Dive tab."),
                     tags$div(actionLink("addValueFilter", "Add Value Filter"), id="valueFilters", style="padding: 0px 5px 0px 5px; background: #e4dfd6; border: 1px solid #b5b3b0; margin: 10px 0 0 0; border-radius: 5px;"),
                     tags$div(actionLink("addPercentileFilter", "Add Percentile Filter"), id="percentileFilters", style="padding: 0px 5px 0px 5px; background: #e4dfd6; border: 1px solid #b5b3b0; margin: 10px 0 0 0; border-radius: 5px;"),
                     tags$div(actionLink("addDateFilter", "Add Date Filter"), id="dateFilters", style="padding: 0px 5px 0px 5px; background: #e4dfd6; border: 1px solid #b5b3b0; margin: 10px 0 0 0; border-radius: 5px;"),
                     tags$br(),
                     actionButton("applyFilters", "Apply Filters", icon("filter"), style="padding: 5px 10px 5px 10px;"),
                     tags$br(),
                     actionLink("filterClear", "Clear All Filters and Transformations", style="color: #f12828;"),
                     tags$hr()),
            tabPanel("Metric Transformation",
                     tags$div(actionLink("addTransformation", "Add Transformation"), id="transformations", style="padding: 0px 5px 0px 5px; background: #e4dfd6; border: 1px solid #b5b3b0; margin: 10px 0 0 0; border-radius: 5px;"),
                     tags$br(),
                     actionButton("applyTransformations", "Create Transformations", icon("recycle"), style="padding: 5px 10px 5px 10px;"),
                     tags$br(),
                     actionLink("transformationsClear", "Clear All Transformations", style="color: #f12828;"),
                     tags$hr()),
            tabPanel("Offsets",
                     tags$div(actionLink("addOffset", "Add Offset"), id="offsets", style="padding: 0px 5px 0px 5px; background: #e4dfd6; border: 1px solid #b5b3b0; margin: 10px 0 0 0; border-radius: 5px;"),
                     tags$br(),
                     actionButton("applyOffsets", "Create Offsets", icon("arrows-v"), style="padding: 5px 10px 5px 10px;"),
                     tags$br(),
                     actionLink("offsetClear", "Clear All Offsets", style="color: #f12828;"),
                     tags$hr())),
          actionButton("run", "Run Analysis", style="color: #fff; background-color: rgb(2, 140, 7); border: solid 1px #005a03;"),
          downloadButton('downloadData', 'Download Customized Data'),
          
          tags$hr(),
          tags$head(
            tags$script(HTML('Shiny.addCustomMessageHandler("jsCode", function(message) { eval(message.value); });'))
          )
        )
      ),
      mainPanel(tableOutput("dataPreview"))
    )
  ),
  
  tabPanel("Metric Comparison",
        value="correlations",
        tabsetPanel(
          tabPanel("Summary - All Dates", tableOutput("summaryTable")),
          tabPanel("Correlations - By Date", conditionalPanel(
            condition = "output.dateColCheck",
            tableOutput("dateCorrelations")
            ),
            conditionalPanel(
              condition = "!output.dateColCheck",
              h3("Select a date column to see correlations by date.")
            )
          )
        )
  ),
  tabPanel("Metric Dive", value="metric", mainPanel(width = 12,
    column(12, align="center",
      selectInput("xCol", "Select Metric", choices=list()),
      tags$div(id = "metricAlertDiv"),
      conditionalPanel(
        condition = "output.validX",
        tabsetPanel(
          tabPanel("Metric Plots",
            tabsetPanel(
              tabPanel("Scatter", actionButton("includePoints", "Include Selection", value = FALSE),
                       actionButton("excludePoints", "Remove Selection", value = FALSE),
                       plotlyOutput("metricScatter"),
                       verbatimTextOutput("selectedPoints")
              ),
              tabPanel("Histogram", plotlyOutput("metricHist")),
              tabPanel("QQ - Normal Dist", plotlyOutput("metricQQNorm")),
              tabPanel("QQ - Y", plotlyOutput("metricQQy")),
              tabPanel("Turnover", conditionalPanel(
                condition = "output.dateColCheck && output.catColCheck",
                h3("Standard Deviation of Change in Rank Percentile"), plotlyOutput("metricTurnover")
                ),
                conditionalPanel(
                  condition = "!output.dateColCheck || !output.catColCheck",
                  h3("Select a date column and category column to see turnover over time.")
                )
              )
            )),
          tabPanel("Details",
            tabsetPanel(
              tabPanel("ANOVA", verbatimTextOutput('aovSummary')),
              tabPanel("Performance", tableOutput("datePerformance"))
            )
          )
      ),
      conditionalPanel(
        condition = "output.dateColCheck",
        checkboxInput("pageFilterCheck", label = "Date Pages", value = FALSE)
      ),
      conditionalPanel(
        condition = "output.pagefilter",
        div(div(actionButton("pageBack", label="", icon("arrow-left")), style = "display:inline-block"),
            div(selectInput("metricDiveFilterDate", label="", choice = list()), style = "display:inline-block; vertical-align:middle"),
            div(actionButton("pageForward", label="", icon("arrow-right")), style = "display:inline-block"))
      )
      )
    )
  )
  
)))
