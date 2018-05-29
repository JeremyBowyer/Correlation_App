dataPreviewPage <- function() {
  tabPanel(
    "Data Preview",
    value="dataPreview",
    DTOutput("dataPreview", height = "auto"),
    conditionalPanel(
                condition = "output.fileUploaded",
                    downloadButton('downloadData', 'Download Customized Data')
                )
    )
}