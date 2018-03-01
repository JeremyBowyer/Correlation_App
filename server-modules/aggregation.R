observeAggregateData <- function(input, output, session, vals) {
  
  observeEvent(input$aggregateData, {
    shinyalert(
      title = "WARNING!",
      text = "This process will assume all columns not designated as a Date column or a Category column are numeric and capable of being aggregated. This will result in loss of data if a column is not numeric.",
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = FALSE,
      type = "warning",
      showConfirmButton = TRUE,
      showCancelButton = TRUE,
      confirmButtonText = "OK",
      confirmButtonCol = "#3E3F3A",
      cancelButtonText = "Cancel",
      timer = 0,
      imageUrl = "",
      animation = TRUE
    )
  })
  
}