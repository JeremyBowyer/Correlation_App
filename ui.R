
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinythemes)
library(plotly)
library(dplyr)
library(XLConnect)

source("global.R", local=TRUE)

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
          tabsetPanel(
            tabPanel("Column Selection",
              checkboxInput("hierBox", "Hierarchical?", value=FALSE),
              conditionalPanel(
                condition = "output.hierarchicalCheck",
                selectInput("hierCol", "Select First Layer column", choices=list())
              ),
              selectInput("yCol", "Select Y column", choices=list()),
              selectInput("dateCol", "Select Date column (optional)", choices=list()),
              conditionalPanel(
                condition = "output.dateColCheck",
                textInput("dateColFormat", "Format dates are in", "%m/%d/%Y")
              ),
              selectInput("categoryCol", "Select Category column (optional)", choices=list()),
              selectInput("ignoreCols", "Select Columns to Ignore (optional)", choices=list(), multiple = TRUE),
              selectInput("multiCols", "Select Columns to be included in the multilinear (optional)", choices=list(), multiple = TRUE),
              tags$hr(),
              actionButton("run", "Run Analysis", style="color: #fff; background-color: rgb(2, 140, 7); border: solid 1px #005a03;"),
              downloadButton('downloadData', 'Download Customized Data')),
            tabPanel("Filters",
                     tags$div(
                       h3("Filters"),
                       tags$p("Warning: Filtering by one column will be applied to entire dataset, and will affect all subsequent analysis, including Metric Dive tab."),
                       conditionalPanel(
                         condition = "output.filtersCheck",
                         actionButton("applyFilters", "Apply Filters", icon("filter"), style="padding: 5px 10px 5px 10px;"),
                         tags$br(),
                         actionLink("filterClear", "Clear All Filters and Transformations", style="color: #f12828;"),
                         tags$hr()
                       ),
                       tags$div(
                         selectInput(
                           "filterSelected",
                           label = NULL,
                           width = "275px",
                           choices = filterList),
                         style= "display:inline-block; vertical-align: top;"
                       ), 
                       tags$div(actionButton("addFilter", "Add Filter", style="padding:4px;font-size: 75%;"),style="display:inline-block; vertical-align: top;"),
                       tags$div(id="filters"),
                       id="filter-container", style="padding: 0px 5px 0px 5px; background: #e4dfd6; border: 1px solid #b5b3b0; margin: 10px 0 0 0; border-radius: 5px;"
                     ),
                     tags$hr()),
            tabPanel("Metric Transformation",
                     tags$div(
                       h3("Metric Transformations"),
                       conditionalPanel(
                         condition = "output.transformationsCheck",
                         actionButton("applyTransformations", "Create Transformations", icon("recycle"), style="padding: 5px 10px 5px 10px;"),
                         tags$br(),
                         actionLink("transformationsClear", "Clear All Transformations", style="color: #f12828;")
                       ),
                       tags$div(
                         selectInput(
                         "transformationselected",
                         label = NULL,
                         width = "275px",
                         choices = transformationList),
                         style= "display:inline-block; vertical-align: top;"
                      ), 
                     tags$div(actionButton("addTransformation", "Add Transformation", style="padding:4px;font-size: 75%;"),style="display:inline-block; vertical-align: top;"),
                     tags$div(id="transformations"),
                     id="transformation-container", style="padding: 0px 5px 0px 5px; background: #e4dfd6; border: 1px solid #b5b3b0; margin: 10px 0 0 0; border-radius: 5px;"),
                     tags$hr()),
            tabPanel("Offsets",
                     tags$div(
                       h3("Offsets"),
                       conditionalPanel(
                         condition = "output.offsetsCheck",
                         actionButton("applyOffsets", "Create Offsets", icon("arrows-v"), style="padding: 5px 10px 5px 10px;"),
                         tags$br(),
                         actionLink("offsetClear", "Clear All Offsets", style="color: #f12828;")
                       ),
                       actionButton("addOffset", "Add Offset", style="padding:4px;font-size: 75%;"),
                       tags$br(),
                       conditionalPanel(
                         condition = "output.offsetsCheck",
                         tags$hr()
                       ),
                       tags$div(id="offsets"),
                       id="offsets-div", style="padding: 0px 5px 0px 5px; background: #e4dfd6; border: 1px solid #b5b3b0; margin: 10px 0 0 0; border-radius: 5px;"),
                     tags$br())
                     ),
          tags$hr(),
          tags$head(
            tags$script(HTML('Shiny.addCustomMessageHandler("jsCode", function(message) { eval(message.value); });')),
            tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
          )
        )
      ),
      mainPanel(h2("Instructions coming soon."))
    )
  ),
  tabPanel("Data Preview",
         value="dataPreview",
         tableOutput("dataPreview")
  ),
  tabPanel("Metric Comparison",
        value="correlations",
        textOutput("currentY_comparison"),
        tags$br(),
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
      textOutput("currentY_dive"),
      tags$br(),
      selectInput("xCol", "Select Metric", choices=list()),
      tags$div(id = "metricAlertDiv"),
      conditionalPanel(
        condition = "output.validX",
        conditionalPanel(
          condition = "output.dateColCheck",
          tags$div(
            tags$div(checkboxInput("pageFilterCheck", label = "Date Pages", value = FALSE), style = "display:inline-block"),
            tags$div(checkboxInput("pointFilterCheck", label = "Filter Using Scatter", value = FALSE), style = "display:inline-block")
          )
        ),
        conditionalPanel(
          condition = "output.pageFilterCheck",
          div(div(actionButton("pageBack", label="", icon("arrow-left")), style = "display:inline-block"),
              div(selectInput("metricDiveFilterDate", label="", choice = list()), style = "display:inline-block; vertical-align:middle"),
              div(actionButton("pageForward", label="", icon("arrow-right")), style = "display:inline-block"))
        ),
        downloadButton('downloadReport', '', class="report_btn"),
        tabsetPanel(
          tabPanel("Metric Plots",
            tabsetPanel(
              tabPanel("Scatter",
                       plotlyOutput("metricScatter"),
                       tags$br(),
                       tags$br(),
                       conditionalPanel(
                         condition = "output.pointFilterCheck",
                         actionButton("keepPoints", "Keep Selection", value = FALSE),
                         actionButton("removePoints", "Remove Selection", value = FALSE)
                       )
              ),
              tabPanel("Histogram", plotlyOutput("metricHist")),
              tabPanel("QQ - Normal Dist", plotlyOutput("metricQQNorm")),
              tabPanel("QQ - Y", plotlyOutput("metricQQy")),
              tabPanel("Rank Volatility", conditionalPanel(
                condition = "output.dateColCheck && output.catColCheck",
                h3("Standard Deviation of Change in Percentile"), plotlyOutput("metricRankVolatility")
                ),
                conditionalPanel(
                  condition = "!output.dateColCheck || !output.catColCheck",
                  h3("Select a date column and category column to see rank volatility over time.")
                )
              )
            )),
          tabPanel("Details",
            tabsetPanel(
              tabPanel("ANOVA", verbatimTextOutput('aovSummary')),
              tabPanel("Performance", tableOutput("datePerformance"))
            )
          )
        )
      )
    )
  ))
))
