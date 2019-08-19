observeMetricDiveFilters <- function(input, output, session, vals) {

  #### Date Page Filter ####
  observeEvent(input$pageFilterCheck,{

    if(input$pageFilterCheck){
      
      vals$dateFilterdf <- data.frame(Date = unique(vals$metricdivedf[, input$dateCol]))

      output$pageFilterTable <- renderDT(vals$dateFilterdf,
                                         options = list(
                                           pageLength = 100,
                                           paging = FALSE,
                                           info = FALSE,
                                           searching = FALSE,
                                           scrollY = "300px"
                                           ),
                                         rownames = FALSE,
                                         extensions = "Scroller",
                                         selection = "multiple",
                                         filter = "none",
                                         style = "bootstrap",
                                         autoHideNavigation = TRUE)
     
      vals$metricdivedfPage <- if(input$pointFilterCheck) vals$metricdivedfPoint  else vals$originalmetricdivedf
      
    }
    
    if(!input$pageFilterCheck && !input$pointFilterCheck) {
      vals$metricdivedf <- vals$originalmetricdivedf
      vals$metricdivedfPage <- data.frame()
      return(NULL)
    }
    
    if(!input$pageFilterCheck && input$pointFilterCheck) {
      vals$metricdivedf <- vals$metricdivedfPoint
      vals$metricdivedfPage <- data.frame()
      return(NULL)
    }
  })
  
  # On Date Page Selection
  observeEvent(input$pageFilterTable_rows_selected, {

      if(length(input$pageFilterTable_rows_selected) == 0) {
        vals$metricdivedf <- vals$originalmetricdivedf
        return(NULL)
      }

      selectedDates <- vals$dateFilterdf[input$pageFilterTable_rows_selected, "Date"]

      df <- if(input$pointFilterCheck) vals$metricdivedfPoint else vals$originalmetricdivedf
      df <- df[df[,input$dateCol] %in% selectedDates, ]
      vals$metricdivedf <- df
      vals$metricdivedfPage <- df
  })

  #### Point Filter ####
  # Point Filter Check
  observeEvent(input$pointFilterCheck, {

    vals$metricdivedfPoint <- if(input$pageFilterCheck) vals$metricdivedfPage  else vals$originalmetricdivedf

    if(!input$pointFilterCheck && !input$pageFilterCheck) {
      vals$metricdivedf <- vals$originalmetricdivedf
      vals$metricdivedfPoint <- data.frame()
      return(NULL)
    }

    if(!input$pointFilterCheck && input$pageFilterCheck) {
      vals$metricdivedf <- vals$metricdivedfPage
      vals$metricdivedfPoint <- data.frame()
      return(NULL)
    }

  })
  # Keep Points
  observeEvent({input$keepPoints}, {

    event.data <- event_data("plotly_selected", source = "metricScatter")
    if(is.null(event.data) == TRUE) return(NULL)

    df <- vals$metricdivedf

    mod <- 1

    if (input$categoryCol != ""){
      df$CatFactor <- as.numeric(as.factor(df[, input$categoryCol]))
      aggs <- by(df, INDICES = list(df[, "CatFactor"]), function(x) {

        cat.index <- event.data[event.data$curveNumber == x[1,"CatFactor"] - 1, "pointNumber"] + 1
        x[cat.index*mod, ]

      })
      df <- do.call("rbind", aggs)
    } else {
      df <- df[(event.data$pointNumber + 1)*mod, ]
    }
    vals$metricdivedf <- df
    vals$metricdivedfPoint <- df

  })
  # Remove Points
  observeEvent({input$removePoints}, {

    event.data <- event_data("plotly_selected", source = "metricScatter")
    if(is.null(event.data) == TRUE) return(NULL)

    df <- vals$metricdivedf

    mod <- -1

    if (input$categoryCol != ""){
      df$CatFactor <- as.numeric(as.factor(df[, input$categoryCol]))
      aggs <- by(df, INDICES = list(df[, "CatFactor"]), function(x) {

        cat.index <- event.data[event.data$curveNumber == x[1,"CatFactor"] - 1, "pointNumber"] + 1
        if(length(cat.index) > 0){
          x[cat.index*mod, ]
        } else {
          x
        }

      })
      df <- do.call("rbind", aggs)
    } else {
      df <- df[(event.data$pointNumber + 1)*mod, ]
    }
    vals$metricdivedf <- df
    vals$metricdivedfPoint <- df

  })

}
metricDivePlots <- function(input, output, session, vals) {
 
  #### Metric Scatter ####
  # Dynamic
  output$metricScatter <- renderPlotly({

    if(input$xCol == "" || input$yCol == "" || nrow(vals$metricdivedf) > MAX_ROW_LIMIT)  {
      return(NULL)
    }

    xform <- as.formula(paste0("~`",input$xCol,"`"))
    yform <- as.formula(paste0("~`",input$yCol,"`"))

    colorcol <- if(input$categoryCol != "") input$categoryCol else input$xCol
    colorform <- as.formula(paste0("~`",colorcol,"`"))

    df <- vals$metricdivedf
    if (input$dateCol != ""){
      df$text <- paste0("Date: ", df[,input$dateCol], "</br>Color: ", df[,colorcol])
    } else {
      df$text <- paste0("Color: ", df[,colorcol])
    }
    
    form <- as.formula(paste0("`", input$yCol, "` ~ `", input$xCol,"`"))
    fit <- lm(form, data = df)
    p <- df %>%
      plot_ly(x = xform, source = "metricScatter") %>%
      add_markers(y = yform, color = colorform, text = ~text)  %>%
      add_lines(x = xform, y = fitted(fit), fill = "red", name = "Regression Line") %>%
      layout(dragmode = "lasso")
    p
  })
  # Static
  output$staticScatter <- renderPlot({

    if(input$xCol == "" || input$yCol == "" || nrow(vals$metricdivedf) <= MAX_ROW_LIMIT)  {
      return(NULL)
    }

    df <- vals$metricdivedf
    x <- df[,input$xCol]
    y <- df[,input$yCol]

    plot(x, y, xlab=input$xCol, ylab=input$yCol)
    abline(lm(y ~ x))

  })
  
  #### Quad Circles ####
  output$quadcircles <- renderPlot({
    
    if(input$xCol == "" || input$yCol == "") {
      return(NULL)
    }
    
    df <- vals$metricdivedf
    left1st <- sum(df[,input$yCol] == 1 & df[,input$xCol] == 0)
    left2nd =sum(df[,input$yCol] == 1 & df[,input$xCol] == 1)
    right1st= sum(df[,input$yCol] == 0 & df[,input$xCol] == 1)
    right2nd=sum(df[,input$yCol] == 0 & df[,input$xCol] == 0)
    sum0sand1s = sum(complete.cases(data.frame(x=df[,input$xCol],y=df[,input$yCol])))
    plotdf = data.frame(colours = c("yellow","red","forestgreen","orange1"), x = rep(.5,4), y = rep(.5,4), radii = c(sqrt(left1st/sum0sand1s)/(3.14^2),sqrt(left2nd/sum0sand1s)/(3.14^2),sqrt(right2nd/sum0sand1s)/(3.14^2),sqrt(right1st/sum0sand1s)/(3.14^2)))
    
    plotdf$xpanel = c(0, 1, 0, 1)
    plotdf$ypanel = c(0, 0, 1, 1)
    plotdf$ypanel = factor(plotdf$ypanel, labels = rev(unique(plotdf$ypanel)),  ordered = TRUE)
    maxxy = max(plotdf$radii*2)
    plot <- ggplot() + 
    geom_circle(aes(x0=x, y0=y, r=radii, fill=colours),data = plotdf)+
    coord_fixed()+ 
      facet_grid(ypanel~xpanel, switch="both")+
      scale_colour_identity(aesthetics = 'fill')+
      theme(axis.title.y=  element_text( angle=0, vjust = 0.5,size=20), axis.title.x = element_text(size=20), axis.line=element_line(), panel.spacing.x=unit(1, "lines"), panel.spacing.y=unit(1, "lines"), panel.background = element_rect(colour = "black", size=1, fill=NA),strip.text.y = element_text(angle = -180,size=20), strip.text.x = element_text(size=20), axis.ticks.y = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks.x = element_blank(),strip.background=element_blank()) 

    return(plot)
    })

    output$VennDiagram2 = renderPlot({
      
      if(input$xCol == "" || input$yCol == "") {
        return(NULL)
      }
      
      vendf = vals$metricdivedf[,c(input$yCol,input$xCol)]
      vendf[,input$yCol] = as.character(vendf[,input$yCol])
      vendf[,input$xCol] = as.character(vendf[,input$xCol])
      all = which(complete.cases(vals$metricdivedf[,c(input$yCol,input$xCol)]))
      nx = which(vals$metricdivedf[,c(input$xCol)]  == "1"  )
      ny = which(  vals$metricdivedf[,c(input$yCol)] == "1")
 
      tryCatch({
        plotobj1 = Venn(list(nx=as.character(nx),ny=as.character(ny),all=as.character(all)))
        
        #gp = VennThemes(compute.Venn(plotobj1))
        #gp$Face$`011` = gpar(col = "black", lty = "solid", lwd = 1, fontsize = 16,fill='yellow')
        # gp$Face$`111` = gpar(col = "black", lty = "solid", lwd = 1, fontsize = 16,fill='red')
        # gp$Face$`101` = gpar(col = "black", lty = "solid", lwd = 1, fontsize = 16,fill='orange1')
        # gp$Face$`010` = gpar(col = "black", lty = "solid", lwd = 2, fontsize = 16,fill='yellow')
        # gp$Face$`001` = gpar(col = "black", lty = "solid", lwd = 1, fontsize = 16,fill='forestgreen')
        # gp$Face$`001-1` = gpar(col = "black", lty = "solid", lwd = 1, fontsize = 16,fill='forestgreen')
        # gp$Face$`100` = gpar(col = "black", lty = "solid", lwd = 2, fontsize = 16,fill='orange1')
        # gp$Face$`110` = gpar(col = "black", lty = "solid", lwd = 2, fontsize = 16,fill='red')
        # gp$Set$Set3$col = '#111111' 
        # gp$Set$Set1$col = '#111111'
        # gp$Set$Set2$col = '#111111'
        # gp$Set$Set3$lwd = 1
        # gp$Set$Set1$lwd = 1
        # gp$Set$Set2$lwd = 1
        # gp$SetText$Set1$alpha = 0
        # gp$SetText$Set2$alpha = 0
        # gp$SetText$Set3$alpha = 0
        # gp$FaceText$`011`$alpha = 0
        # gp$FaceText$`111`$alpha = 0
        # gp$FaceText$`101`$alpha = 0
        # gp$FaceText$`001`$alpha = 0
        # gp$FaceText$`001-1`$alpha = 0
        # gp$FaceText$`010`$alpha = 0
        # gp$FaceText$`110`$alpha = 0
        # gp$FaceText$`100`$alpha = 0
        # gp$FaceText$`001-2`$alpha = 0
        #  
      }, error=function(e) {
        print(e)
        left1st <- which(vals$metricdivedf[,input$yCol] == 1 & vals$metricdivedf[,input$xCol] == 0)
        left2nd <- which(vals$metricdivedf[,input$yCol] == 1 & vals$metricdivedf[,input$xCol] == 1)
        right1st <- which(vals$metricdivedf[,input$yCol] == 0 & vals$metricdivedf[,input$xCol] == 1)
        right2nd <- which(vals$metricdivedf[,input$yCol] == 0 & vals$metricdivedf[,input$xCol] == 0)
        plotobj1 <- Venn(list(nx=as.character(nx),ny=as.character(ny),n=as.character(right2nd)))
        
        gp = VennThemes(compute.Venn(plotobj1))
 
        
       
      })
      return(plot(plotobj1,doWeights=TRUE,doEuler =TRUE,gp=gp,type="circles"))
    })
    
    output$myImage <- renderPlot({
      
      if(input$xCol == "" || input$yCol == "") {
        return(NULL)
      }
      
      nxy = sum(as.numeric(vals$metricdivedf[,input$yCol]) == 1 & as.numeric(vals$metricdivedf[,input$xCol]) == 1)
      
      nx = sum(as.numeric(vals$metricdivedf[,input$yCol]) == 0 & as.numeric(vals$metricdivedf[,input$xCol]) == 1)
      ny = sum(as.numeric(vals$metricdivedf[,input$yCol]) == 1 & as.numeric(vals$metricdivedf[,input$xCol]) == 0)
      n = sum(as.numeric(vals$metricdivedf[,input$yCol]) == 0 & as.numeric(vals$metricdivedf[,input$xCol]) == 0)
      
      plot <- ggplot(data=data.frame(color=c("yellow", "red",'orange1',"forestgreen"),xlabel=rep(0.25-0.2*5/7+(0.25-0.2*5/7)/2+(0.25-0.2*5/7)*3+(0.39-0.2*5/7)*.6,4),xdot=rep(0.39-0.1*5/7-.2+.6+(0.25-0.2*5/7)/2+(0.25-0.2*5/7)*1.2+(0.39-0.2*5/7)*.6,4),y=c(3.5,2.5,1.5,0.5),label=paste0(c('False Negative ',"True Positive ","False Positive ",'True Negative '),c("(X=0,Y=1)","(X=1,Y=1)","(X=1,Y=0)","(X=0,Y=0)")))) +geom_point(aes(x=xdot,y=y),color=c("yellow", "red",'orange1',"forestgreen"),size=6)+xlim(-.5,0.4+.4+.4+.2)+ylim(.25,3.75)+theme(legend.title = element_blank(),axis.ticks.y = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "none") +geom_text(aes(x=rep((0.25-0.2*5/7)/6+(0.25-0.2*5/7+(0.25-0.2*5/7)/2+(0.25-0.2*5/7)*3+(0.39-0.2*5/7)*.6)/2-(0.25-0.2*5/7)*.6,4),y=y,label=label),size=10,family='raleway')+geom_text(aes(x=rep(0.39-0.2*5/7+.9+(0.39-0.2*5/7)*.6,4),y=y,label=c(paste0("n=",ny),paste0("n=",nxy),paste0("n=",nx),paste0("n=",n))),size=10,family='raleway')+theme(legend.title = element_blank(),axis.ticks.y = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(), panel.background = element_blank(),axis.ticks.x=element_blank())+labs(x=NULL,y=NULL)
      
      return(plot)
    })
    
  #### Metric Histogram ####
  output$metricHist <- renderPlotly({
    if(input$xCol == "" || input$yCol == "") {
      return(NULL)
    }
    
    df <- vals$metricdivedf
    xform <- as.formula(paste0("~`",input$xCol,"`"))
    plot_ly(data = df, x = xform, type = "histogram")
  })
  
  #### Metric Data Points ####
  # By Date
  output$metricDataPointsDate <- renderPlotly({
    if(input$xCol == "" || input$dateCol == "" || input$yCol == "") {
      return(NULL)
    }
  
    metricDF <- vals$metricdivedf
    dataPointsDF <- aggregate(metricDF[, input$xCol], by=list(metricDF[, input$dateCol]), function(x) sum(!is.na(as.numeric(x))))
    names(dataPointsDF) <- c("Date", "DataPoints")
    dataPointsDF$Date <- parse_date_time(dataPointsDF$Date,order=vals$dateFormat)
    dataPointsDF <- dataPointsDF[order(dataPointsDF$Date), ]
    
    plot_ly(data = dataPointsDF, x = ~Date, y = ~DataPoints, type = 'scatter', mode = 'lines')
    
  })
  # By Category
  output$metricDataPointsCategory <- renderPlotly({
    
    if(input$xCol == "" || input$categoryCol == "" || input$yCol == "") {
      return(NULL)
    }
    
    metricDF <- vals$metricdivedf
    dataPointsDF <- aggregate(metricDF[, input$xCol], by=list(metricDF[, input$categoryCol]), function(x) sum(!is.na(as.numeric(x))))
    names(dataPointsDF) <- c("Category", "DataPoints")
    dataPointsDF <- dataPointsDF[order(dataPointsDF$Category), ]
    plot_ly(data = dataPointsDF, x = ~Category, y = ~DataPoints, type = 'bar')
    
  })
  
  #### Metric QQ Normal Dist ####
  output$metricQQNorm <- renderPlotly({
    
    if(input$xCol == "" || input$yCol == "") {
      return(NULL)
    }
    
    df <- vals$metricdivedf
    
    xyDF <- df[complete.cases(df[, c(input$xCol, input$yCol)]), c(input$xCol, input$yCol)]
    sortedX <- xyDF[order(xyDF[, input$xCol]), input$xCol]
    sortedY <- xyDF[order(xyDF[, input$yCol]), input$yCol]
    xNorm <- qnorm(c(1:nrow(xyDF)) / nrow(xyDF), mean(xyDF[,input$xCol], na.rm = TRUE), sd(xyDF[,input$xCol], na.rm = TRUE))
    xNorm <- replace(xNorm, is.infinite(xNorm), NA)
    sortedDF <- data.frame(x = sortedX, y = sortedY, xNorm = xNorm)
    
    plot_ly(data = sortedDF, x = ~x, y = ~xNorm)
    
  })
  
  #### Metric QQ Y ####
  output$metricQQy <- renderPlotly({
    
    if(input$xCol == "" || input$yCol == "") {
      return(NULL)
    }
    
    df <- vals$metricdivedf
    
    xyDF <- df[complete.cases(df[, c(input$xCol, input$yCol)]), c(input$xCol, input$yCol)]
    sortedX <- xyDF[order(xyDF[, input$xCol]), input$xCol]
    sortedY <- xyDF[order(xyDF[, input$yCol]), input$yCol]
    xNorm <- qnorm(c(1:nrow(xyDF)) / nrow(xyDF), mean(xyDF[,input$xCol], na.rm = TRUE), sd(xyDF[,input$xCol], na.rm = TRUE))
    xNorm <- replace(xNorm, is.infinite(xNorm), NA)
    sortedDF <- data.frame(x = sortedX, y = sortedY, xNorm = xNorm)
    
    plot_ly(data = sortedDF, x = ~x, y = ~y)
    
  })
  
  #### Metric Rank Volatility ####
  output$metricRankVolatility <- renderPlotly({
    
    if(input$xCol == "" || input$yCol == "" || input$dateCol == "") {
      return(NULL)
    }
    
    df <- vals$originalmetricdivedf
    metricDF <- df[order(df[,input$dateCol]) ,c(input$dateCol, input$categoryCol, input$xCol)]
    wideMetricDF <- dcast(metricDF,as.formula(paste0("`",input$dateCol,"` ~ `",input$categoryCol,"`")), value.var = input$xCol)[, -1]
    rankDF <- t(apply(wideMetricDF, 1, function(x) rank(x, na.last = "keep") / length(which(!is.na(x))) ))
    diffRankDF <- apply(rankDF, 2, function(x) c(NA, diff(x)))
    stds <- apply(diffRankDF, 1, function(x) sd(x, na.rm = TRUE))
    dates <- parse_date_time(unique(as.character(metricDF[,input$dateCol])), order = vals$dateFormat)
    stdDF <- data.frame(date = dates, std = stds)
    stdDF <- stdDF[order(stdDF$date), ]
    plot_ly(data = stdDF, x = ~date, y = ~std, type = 'scatter', mode = 'lines')
    
  })
  
  #### ANOVA ####
  output$aovSummary = reactivePrint(function() {
    
    if(input$xCol == "" || input$yCol == "") {
      return(NULL)
    }
    
    df <- vals$metricdivedf
    form <- as.formula(paste0("as.numeric(`", input$yCol, "`) ~ as.numeric(`", input$xCol,"`)"))
    summary(lm(form, data = df))
  })
  
  #### Classification Tree ####
  output$classTreePlot = renderPlot({
    
    if(input$xCol == "" || input$yCol == "") {
      return(NULL)
    }
    df <- vals$metricdivedf[, c(input$xCol, input$yCol)]
    form <- as.formula(paste0("`", input$yCol, "` ~ `", input$xCol, "`"))
    control <- rpart.control(minsplit=1, minbucket=1, cp=0, maxdepth=1)
    mod <- rpart(form, df, control = control, method = "class")
    rpart.plot(mod)
  })


}

processMetricDiveDF <- function(input, output, session, vals) {
  
    # Upon selection of metric in Metric Dive tab, or run analysis button
    observeEvent({
      c(input$xCol,input$run)
    }, {
    
      if(input$xCol == "" || input$yCol == ""){
        return(NULL)  
      }

      # Process Data
      df <- vals$datadf

      df[,input$xCol] <- as.numeric(df[,input$xCol])
      df[,input$yCol] <- as.numeric(df[,input$yCol])
      df <- subset(df, !is.na(df[,input$xCol]) & !is.na(df[,input$yCol]))
      df <- subset(df, !is.infinite(df[,input$xCol]) & !is.infinite(df[,input$yCol]))
      df <- subset(df, !is.nan(df[,input$xCol]) & !is.nan(df[,input$yCol]))
      if(!is.null(input$dateCol) && input$dateCol != "") df <- subset(df, !is.na(df[,input$dateCol]))

      vals$metricdivedf <- df
      vals$originalmetricdivedf <- df

    })
  
}

calculateMetricStats <- function(input, output, session, vals) {

  # Update metric dive Y column indicator on Y change
  output$currentY_dive <- renderText({ 
      paste0("Current Y column: ", input$yCol)
  })

  observeEvent({
      c(
      input$xCol,
      input$run,
      input$pageFilterCheck,
      input$pageFilterTable_rows_selected,
      input$pointFilterCheck,
      input$keepPoints,
      input$removePoints
      )
   }, {

    if(input$xCol == "" || input$yCol == ""){
      return(NULL)
    }

   
    df <- vals$metricdivedf
    if(!is.null(input$dateCol) && input$dateCol != "") df <- subset(df, !is.na(df[,input$dateCol]))

    if(is.null(df)) return(NULL)
    if(nrow(df) == 0) return(NULL)

    if(sum(!is.na(as.numeric(df[, input$xCol]))) == 0) {

      shinyalert(
        title = "",
        text = "Column has no data after being coerced to numeric. Try removing some filters or choosing a different column.",
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

      return(NULL)

    }

    xData <- as.numeric(df[, input$xCol])
    yData <- as.numeric(df[, input$yCol])

    summarydf <- data.frame(
      x = c(
        sum(!is.na(xData)),
        sd(xData, na.rm = TRUE),
        mean(xData, na.rm = TRUE),
        median(xData, na.rm = TRUE),
        max(xData, na.rm = TRUE),
        min(xData, na.rm = TRUE)
      ),
      y = c(
        sum(!is.na(yData)),
        sd(yData, na.rm = TRUE),
        mean(yData, na.rm = TRUE),
        median(yData, na.rm = TRUE),
        max(yData, na.rm = TRUE),
        min(yData, na.rm = TRUE)
      ),
      row.names = c("Data Points", "Standard Deviation", "Mean", "Median", "Max","Min")
    )

    names(summarydf) <- c(input$xCol,input$yCol)

    output$summaryStats <- renderTable(include.rownames = TRUE, {
      return(summarydf)
    })

    # Create quintiles by date and more processing
    vals$perfdf <- calculatePerformanceDates(df, input$xCol, input$yCol, input$dateCol, vals$dateFormat)

    # Performance Output
    output$datePerformance = renderDT(vals$perfdf,
                                      options = list(
                                           pageLength = 6,
                                           paging = FALSE,
                                           info = FALSE,
                                           searching = FALSE,
                                           ordering = FALSE
                                           ),
                                      rownames = FALSE,
                                      fillContainer = TRUE,
                                      style = "bootstrap",
                                      selection = "none")

    # Classification Tree DF
    # Create Model
    df <- vals$metricdivedf[, c(input$xCol, input$yCol)]
    form <- as.formula(paste0("`", input$yCol, "` ~ `", input$xCol, "`"))
    control <- rpart.control(minsplit=1, minbucket=1, cp=0, maxdepth=1)
    tryCatch({
      mod <- rpart(form, df, control = control, method = "class")
      modYVals <- unique(mod$y)
      yVals = unique(df[!is.na(df[,input$yCol]), input$yCol])
      yValDict <- list()
      yValDict[as.character(yVals[1])] <- modYVals[1]
      yValDict[as.character(yVals[2])] <- modYVals[2]
  
      # Create empty output dataframe
      outputDF <- data.frame("1" = c("Root","Left", "Right"),
                             "2" = numeric(3),
                             "3" = numeric(3),
                             "4" = numeric(3),
                             "5" = numeric(3),
                             "6" = numeric(3),
                             "7" = numeric(3),
                             "8" = numeric(3),
                             "9" = numeric(3),
                             "10" = numeric(3))
  
      dfnames <- c("Node",
                   "Predicted Value",
                   "N",
                   "N % of Dataset",
                   paste0(names(yValDict)[1]," -- N"),
                   paste0(names(yValDict)[1]," -- N % of Dataset"),
                   paste0(names(yValDict)[1]," -- N % of Node Dataset"),
                   paste0(names(yValDict)[2]," -- N"),
                   paste0(names(yValDict)[2]," -- N % of Dataset"),
                   paste0(names(yValDict)[2]," -- N % of Node Dataset"))
      names(outputDF) <- dfnames
  
  
      # Extract Stats
      nObs <- length(mod$y)
      nVal1 <- length(mod$y[mod$y == yValDict[1]])
      nVal2 <- length(mod$y[mod$y == yValDict[2]])
      
      for(i in 1:dim(mod$frame$yval2)[1]){
        nodeDetails = mod$frame$yval2[i,]
        outputDF[i, "Predicted Value"] <- as.numeric(names(yValDict)[nodeDetails[1]])
        outputDF[i, "N"] <- nodeDetails[2] + nodeDetails[3]
        outputDF[i, "N % of Dataset"] <- nodeDetails[6]
        outputDF[i, paste0(names(yValDict)[1]," -- N")] <- nodeDetails[2]
        outputDF[i, paste0(names(yValDict)[1]," -- N % of Dataset")] <- nodeDetails[2] / nObs
        outputDF[i, paste0(names(yValDict)[1]," -- N % of Node Dataset")] <- nodeDetails[2] / (nodeDetails[2] + nodeDetails[3])
        outputDF[i, paste0(names(yValDict)[2]," -- N")] <- nodeDetails[3]
        outputDF[i, paste0(names(yValDict)[2]," -- N % of Dataset")] <- nodeDetails[3] / nObs
        outputDF[i, paste0(names(yValDict)[2]," -- N % of Node Dataset")] <- nodeDetails[3] / (nodeDetails[2] + nodeDetails[3])
      }
      output$classTreeDF <- renderTable(include.rownames=FALSE, {
        return(outputDF)
      })
    }, error = function(e) {
        NULL
      })
  })
}