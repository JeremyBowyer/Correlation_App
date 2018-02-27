metricDivePage <- function() {
  
  tabPanel("Metric Dive",
           value="metric",
           sidebarLayout(
             sidebarPanel(
               h3(textOutput("currentY_dive")),
               tags$br(),
               tags$br(),
               selectInput("xCol", "Select Metric", choices=list()),
               tags$div(id = "metricAlertDiv"),
               conditionalPanel(
                 condition = "output.validX",
                 conditionalPanel(
                   condition = "output.dateColCheck",
                   checkboxInput("pageFilterCheck", label = "Date Pages", value = FALSE),
                   conditionalPanel(
                     condition = "output.pageFilterCheck",
                     div(
                       DTOutput("pageFilterTable")
                       )
                     ),
                   checkboxInput("pointFilterCheck", label = "Filter Using Scatter", value = FALSE),
                   conditionalPanel(
                     condition = "output.pointFilterCheck",
                     actionButton("keepPoints", "Keep Selection", value = FALSE),
                     actionButton("removePoints", "Remove Selection", value = FALSE)
                     )
                   ),
                 downloadButton('downloadReport', 'Download Report')
                 )
               ),
             mainPanel(
               tabsetPanel(
                 tabPanel(
                   "Metric Plots",
                   tabsetPanel(
                     tabPanel(
                       "Scatter",
                       plotlyOutput("metricScatter"),
                       tags$br(),
                       tags$br()
                       ),
                     tabPanel("Histogram", plotlyOutput("metricHist")),
                     tabPanel("QQ - Normal Dist", plotlyOutput("metricQQNorm")),
                     tabPanel("QQ - Y", plotlyOutput("metricQQy")),
                     tabPanel(
                       "Rank Volatility",
                       conditionalPanel(
                         condition = "output.dateColCheck && output.catColCheck",
                         h3("Standard Deviation of Change in Percentile"),
                         plotlyOutput("metricRankVolatility")
                         ),
                       conditionalPanel(
                         condition = "!output.dateColCheck || !output.catColCheck",
                         h3("Select a date column and category column to see rank volatility over time.")
                         )
                       )
                     )
                   ),
                 tabPanel(
                   "Details",
                   tabsetPanel(
                     tabPanel("ANOVA", verbatimTextOutput('aovSummary')),
                     tabPanel("Performance", tableOutput("datePerformance"))
                     )
                   )
                 )
               )
             )
           )
}