
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinythemes)

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
          tags$h3("Filters"),
          actionLink("addFilter", "Add Filter"),
          tags$div(id="tagsDiv"),
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
  tabPanel("Metric Dive", value="metric", mainPanel(
    selectInput("xCol", "Select Metric", choices=list()),
    h3("Metric Plots"),
    tabsetPanel(
      tabPanel("Scatter", ggvisOutput("metricScatter")),
      tabPanel("Histogram", ggvisOutput("metricHist"))
    ),
    h3('ANOVA Table'),
    verbatimTextOutput('aovSummary')
    )
  )
  
))
