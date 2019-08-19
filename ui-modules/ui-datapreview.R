dataPreviewPage <- function() {
  tabPanel(
    "Data Preview",
    value="dataPreview",
    DTOutput("dataPreview", height = "auto"),
    conditionalPanel(
                condition = "output.fileUploaded",
                    shinySaveButton("save", "Save file", "Save file as...", filetype = list(text = "csv"))
                )
    )
}