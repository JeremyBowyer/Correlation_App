loadMethods <- function(input, output, session, vals) {
    
    vals$getCols <- function(){
        df = vals$datadf
        return(names(df))
    }

    vals$refreshInputs <- function(session, input, vals) {

        cnt <- vals$transformationCount
        transformInputs <- character()
        for(i in 1:cnt) {
          transformInputs <- c(transformInputs,
                               paste0("transformCols", cnt),
                               paste0("transformDateCol", cnt),
                               paste0("transformCategoryCol", cnt),
                               paste0("transformY", cnt)
                               )
        }

        for(col in c(vals$inputList, transformInputs)) {
            updateSelectInput(session, col, choices=vals$getCols(), selected=input[[col]])
        }
    }

    vals$unloadData <- function(session, input, output, vals) {
        for(col in vals$inputList) {
            updateSelectInput(session, col, choices=list(), selected=NULL)
        }

        vals$datadf <- NULL
        vals$originaldf <- NULL
    }

    vals$validateDates <- function(dateColumn){
        dateCheck <- sum(!is.na(dateColumn)) == 0
        if(dateCheck){
          Sys.sleep(0.5) # To allow previous shinyalert to close
          shinyalert(
            title = "",
            text = "Incorrect date format detected. Check 'Data Preview' tab to confirm your date format.",
            closeOnEsc = TRUE,
            closeOnClickOutside = TRUE,
            html = FALSE,
            type = "error",
            showConfirmButton = TRUE,
            showCancelButton = FALSE,
            confirmButtonText = "OK",
            confirmButtonCol = "#3E3F3A",
            timer = 0,
            imageUrl = "",
            animation = TRUE
          )

        }
        return(!dateCheck)
    }

}