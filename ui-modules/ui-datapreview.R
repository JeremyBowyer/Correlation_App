dataPreviewPage <- function() {
  tabPanel(
    "Data Preview",
    value="dataPreview",
    DTOutput("dataPreview", height = "auto")
    )
}