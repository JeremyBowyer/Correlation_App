metricComparisonsPage <- function(){
  
  tabPanel(
    "Metric Comparison",
    value="correlations",
    textOutput("currentY_comparison"),
    tags$br(),
    tabsetPanel(
      tabPanel(
        "Summary - All Dates",
        DTOutput("summaryTable")),
      tabPanel(
        "Correlations - By Date",
        conditionalPanel(
          condition = "output.dateColCheck",
          DTOutput("dateCorrelations")
          ),
        conditionalPanel(
          condition = "!output.dateColCheck",
          h3("Select a date column to see correlations by date.")
          )
        )
      )
    )
  
}