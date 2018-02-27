metricComparisonsPage <- function(){
  
  tabPanel(
    "Metric Comparison",
    value="correlations",
    textOutput("currentY_comparison"),
    tags$br(),
    tabsetPanel(
      tabPanel(
        "Summary - All Dates",
        tableOutput("summaryTable")),
      tabPanel(
        "Correlations - By Date",
        conditionalPanel(
          condition = "output.dateColCheck",
          tableOutput("dateCorrelations")
          ),
        conditionalPanel(
          condition = "!output.dateColCheck",
          h3("Select a date column to see correlations by date.")
          )
        )
      )
    )
  
}