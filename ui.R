
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
  
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
      tags$hr(),
      actionButton("run", "Run Analysis")
      
    ),
    mainPanel(
      tableOutput("contents")
    )
  )
  
))
