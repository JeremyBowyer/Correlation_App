metricComparisonsPage <- function(){
  
  tabPanel(
    "Metric Comparison",
    value="correlations",
    textOutput("currentY_comparison"),
    tags$br(),tabsetPanel(
      tabPanel(
        "Summary - All Dates",
        tabsetPanel(
          tabPanel(
            "Table",
            DTOutput("summaryTable"),
            downloadButton('downloadComparison', 'Download Comparison Data')),
          tabPanel(
            "Histogram",
            plotlyOutput("correlHist", height="700px")),
          tabPanel(
            "Scatter",
            plotlyOutput("correlScatter", height="700px")
          ),
          tabPanel(
            "Correlation Matrix",
            DTOutput("corMat"))
          
        )
      ),
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
        ),
      tabPanel(
        "Data Points - By Date",
        conditionalPanel(
          condition = "output.dateColCheck",
          DTOutput("dateDataPointsDF")
          ),
        conditionalPanel(
          condition = "!output.dateColCheck",
          h3("Select a date column to see correlations by date.")
          )
        )
      )
    
    )
  
}