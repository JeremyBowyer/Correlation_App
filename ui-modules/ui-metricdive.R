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
                     condition = "input.pageFilterCheck",
                     DTOutput("pageFilterTable")
                   ),
                   conditionalPanel(
                     condition = "!output.bigDataCheck",
                     checkboxInput("pointFilterCheck", label = "Filter Using Scatter", value = FALSE)
                   ),
                   conditionalPanel(
                     condition = "output.pointFilterCheck",
                     actionButton("keepPoints", "Keep Selection", value = FALSE),
                     actionButton("removePoints", "Remove Selection", value = FALSE),
                     tags$br(),
                     tags$br()
                     ),
                 downloadButton('downloadReport', 'Download Report')
                 )
               )
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel(
                   "Metric Plots",
                   tabsetPanel(
                     tabPanel(
                       "Scatter",
                       conditionalPanel(
                         condition = "!output.bigDataCheck",
                         plotlyOutput("metricScatter", height="800px")
                       ),
                       conditionalPanel(
                         condition = "output.bigDataCheck",
                         plotOutput("staticScatter", height="800px")
                       ),
                       tags$br(),
                       tags$br()
                       ),
                     tabPanel("Histogram", plotlyOutput("metricHist")),
                     tabPanel(
                       "Data Points",
                       tabsetPanel(
                         tabPanel(
                           "By Date",
                           conditionalPanel(
                             condition = "output.dateColCheck",
                             h3("Number of non-na data points by date."),
                             plotlyOutput("metricDataPointsDate")
                             ),
                           conditionalPanel(
                             condition = "!output.dateColCheck",
                             h3("Select a date column to see data points by date.")
                             )
                           ),
                         tabPanel(
                           "By Category",
                           conditionalPanel(
                             condition = "output.catColCheck",
                             h3("Number of non-na data points by category."),
                             plotlyOutput("metricDataPointsCategory")
                             ),
                           conditionalPanel(
                             condition = "!output.catColCheck",
                             h3("Select a category column to see data points by category.")
                             )
                           )
                         )
                       ),
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
                       ),
                    tabPanel(
                       "Classification Tree",
                       conditionalPanel(
                         condition = "output.binaryYCheck",
                         tableOutput("classTreeDF"),
                         plotOutput("classTreePlot")
                         ),
                       conditionalPanel(
                         condition = "!output.binaryYCheck",
                         h3("Y values not binary, or too few datapoints for that X and that Y.")
                         )
                       ),
                    tabPanel(
                      "Area Diagrams",
                      tabsetPanel(
                        tabPanel("Venn Diagram",
                                 fluidRow(column(plotOutput("myImage"),width=5,offset=2)),
                                 fluidRow(column(plotOutput("quadcircles"),width=6),column(imageOutput("VennDiagram2"),width=6))
                                 )
                        )
                      )
                    )
                   ),
                tabPanel(
                   "Details",
                   tabsetPanel(
                     tabPanel("ANOVA", verbatimTextOutput('aovSummary')),
                     tabPanel("Performance", DTOutput("datePerformance")),
                     tabPanel("Summary", tableOutput("summaryStats"))
                     )
                   )
                )
               )
             )
  )
}