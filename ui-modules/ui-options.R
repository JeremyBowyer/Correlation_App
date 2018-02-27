optionsPage <- function() {
  
  tabPanel(
    "Options",
    value = "options",
    sidebarLayout(
      sidebarPanel(
        useShinyalert(),
        fileInput(
          "csvfile",
          "Choose CSV File",
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv"
            )
          ),
        tags$hr(),
        tabsetPanel(
          tabPanel(
            "Column Selection",
            checkboxInput("hierBox", "Hierarchical?", value=FALSE),
            conditionalPanel(
              condition = "output.hierarchicalCheck",
              selectInput("hierCol", "Select First Layer column", choices=list("", "Please upload data first."))
              ),
            selectInput("yCol", "Select Y column", choices=list("", "Please upload data first."), selected=""),
            selectInput("dateCol", "Select Date column (optional)", choices=list("", "Please upload data first.")),
            conditionalPanel(
              condition = "output.dateColCheck",
              textInput("dateColFormat", "Format dates are in", "%m/%d/%Y")
              ),
            selectInput("categoryCol", "Select Category column (optional)", choices=list("", "Please upload data first.")),
            selectInput("ignoreCols", "Select Columns to Ignore (optional)", choices=list("Please upload data first."), multiple = TRUE),
            selectInput("multiCols", "Select Columns to be included in the multilinear (optional)", choices=list("Please upload data first."), multiple = TRUE),
            tags$hr(),
            actionButton("run", "Run Analysis", style="color: #fff; background-color: rgb(2, 140, 7); border: solid 1px #005a03;"),
            downloadButton('downloadData', 'Download Customized Data')),
          tabPanel(
            "Filters",
            tags$div(
              h3("Filters"),
              tags$p("Warning: Filtering by one column will be applied to entire dataset, and will affect all subsequent analysis, including Metric Dive tab."),
              conditionalPanel(
                condition = "output.filtersCheck",
                actionButton("applyFilters", "Apply Filters", icon("filter"), style="padding: 5px 10px 5px 10px;"),
                tags$br(),
                actionLink("filterClear", "Clear All Filters", style="color: #f12828;"),
                tags$hr()
                ),
              tags$div(
                selectInput("filterSelected", label = NULL, width = "275px", choices = filterList),
                style= "display:inline-block; vertical-align: top;"
                ),
              tags$div(actionButton("addFilter", "Add Filter", style="padding:4px;font-size: 75%;"),style="display:inline-block; vertical-align: top;"),
              tags$div(id="filters"),
              id="filter-container", style="padding: 0px 5px 0px 5px; background: #e4dfd6; border: 1px solid #b5b3b0; margin: 10px 0 0 0; border-radius: 5px;"
              ),
            tags$hr()),
          tabPanel(
            "Metric Transformation",
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
              tags$div(
                actionButton(
                  "addTransformation",
                  "Add Transformation",
                  style="padding:4px;font-size: 75%;"
                  ),
                style="display:inline-block; vertical-align: top;"
                ),
              tags$div(id="transformations"),
              id="transformation-container",
              style="padding: 0px 5px 0px 5px; background: #e4dfd6; border: 1px solid #b5b3b0; margin: 10px 0 0 0; border-radius: 5px;"
              ),
            tags$hr()
            ),
            tabPanel(
              "Offsets",
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
                tags$br(),
                conditionalPanel(
                  condition = "output.offsetsCheck",
                  tags$hr()
                  ),
                tags$div(id="offsets"),
                id="offsets-div",
                style="padding: 0px 5px 0px 5px; background: #e4dfd6; border: 1px solid #b5b3b0; margin: 10px 0 0 0; border-radius: 5px;"
                ),
              tags$br()
              )
          ),
        tags$hr(),
        tags$head(
          tags$script(HTML('Shiny.addCustomMessageHandler("jsCode", function(message) { eval(message.value); });')),
          tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
          )
        ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Instructions",
            includeMarkdown("documents/instructions.md")
            ),
          tabPanel(
            "Glossary",
            includeMarkdown("documents/glossary.md")
            )
          )
        )
      )
    )
}