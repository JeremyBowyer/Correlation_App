
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
  tabPanel("Options",
    sidebarLayout(
      sidebarPanel(
        
        fileInput("csvfile", "Choose CSV File",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
        ),
        tags$hr(),
        selectInput("yCol", "Select Y column", choices=list()),
        selectInput("dateCol", "Select Date column", choices=list()),
        selectInput("categoryCol", "Select Category column", choices=list()),
        tags$hr(),
        actionButton("run", "Run Analysis"),
        tags$hr(),
        downloadButton('downloadData', 'Download'),
        tags$head(tags$script(HTML('Shiny.addCustomMessageHandler("jsCode", function(message) { eval(message.value); });')))
        
      ),
      mainPanel(tableOutput("dataPreview"))
    )
  ),
  
  tabPanel("Correlations", tabsetPanel(
    tabPanel("All", mainPanel(tableOutput("allCorrelations"))),
    tabPanel("By Date", mainPanel(tableOutput("dateCorrelations")))
    )
  ),
  tabPanel("Metric Dive", mainPanel())
  
))
