dataPreview <- function(input, output, session, vals) {
   
    output$dataPreview <- renderDT(vals$datadf,
                                     options = list(
                                       pageLength = 10
                                     ),
                                     rownames = FALSE,
                                     fillContainer = TRUE,
                                     style = "bootstrap",
                                     selection = "none")

}