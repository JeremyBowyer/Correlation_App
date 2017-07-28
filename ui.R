
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
        conditionalPanel(
          condition = "output.fileUploaded",
          tags$hr(),
          checkboxInput("hierBox", "Hierarchical?", value=FALSE),
          conditionalPanel(
            condition = "output.hierarchicalCheck",
            selectInput("hierCol", "Select First Layer column", choices=list())
          ),
          selectInput("yCol", "Select Y column", choices=list()),
          selectInput("dateCol", "Select Date column", choices=list()),
          selectInput("categoryCol", "Select Category column", choices=list()),
          tags$hr(),
          tags$h3("Filters"),
          actionLink("addFilter", "Add Filter"),
          tags$div(id="tagsDiv"),
          tags$hr(),
          actionButton("run", "Run Analysis", style="color: #fff; background-color: rgb(2, 140, 7); border: solid 1px #005a03;"),
          tags$hr(),
          tags$head(tags$script(HTML('Shiny.addCustomMessageHandler("jsCode", function(message) { eval(message.value); });')))
        )
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
