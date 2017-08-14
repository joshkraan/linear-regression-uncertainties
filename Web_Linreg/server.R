library(shiny)
library(tidyverse)
library(stringr)
library(magrittr)
library(ggthemes)
library(latex2exp)
library(shinyjs)
library(DT)


#TODO: Take maximum of found uncertainty vs uncertainty in fit of original data

shinyServer(function(input, output, session) {
  
  # Disable the download button until the file is uploaded.
  
  shinyjs::disable("downloadPlot")
  observe({
    if(!is.null(input$csvFile)) {
      shinyjs::enable("downloadPlot")
    }
  })
  
  # Read the uploaded data file and produce a visual data table.
  
  output$dataTable = DT::renderDataTable(options = list(columnDefs = list(list(className = 'dt-left', targets = "_all"))), {
    
    if (is.null(input$csvFile)) {
      return(data.frame(X = numeric(0), Y = numeric(0)))
    }
    
    #TODO: Use readr
    data = read.csv(input$csvFile$datapath, header = input$header)
    
    names(data) = c('XValue', 'XUncertainty', 'YValue', 'YUncertainty')
    
    data %<>% unite(X, XValue, XUncertainty, sep = "\u00B1", remove = TRUE)
    data %<>% unite(Y, YValue, YUncertainty, sep = "\u00B1", remove = TRUE)
    
    data
  })
  
  # Switch to graph tab once button in data tab is pressed.
  
  observeEvent(input$graphData, {
    if(is.null(input$csvFile)){
      #TODO: Dismiss this warning after some time.
      createAlert(session, "alert", title = "Error", content = "Please input data before proceeding.", append = FALSE, style = "danger")
      return(NULL)
    }
    updateTabItems(session, "menu", "graph")
  })
  
  # Calculate the regression once clicked and store the calculated values in a reactiveValues() so they can be accessed by other functions
      
  regressionValues = reactiveValues()
  
  observeEvent(input$calculateFit, {
    
    #TODO
    validate(
      need(input$setNumber >= 100 && input$setNumber <= 100000, "Please select a number from 100 to 100,000")
    )
    
    samples = input$setNumber
    
    if (is.null(input$csvFile))
      return(NULL)
    
    #Read uploaded data file. Uses header if option is selected in UI.
    data = read.csv(input$csvFile$datapath, header = input$header)
    
    normaldistribution = function(n, mean, uncertainty) {
      #Standard Deviation can be approximated to range/4 with 95% accuracy, range = 2*uncertainty, so sd equals approximately unc/2
      # Two or 3 standard deviations?
      # TODO
      sd = uncertainty/2
      #rnorm() can take vectors (term for lists), will cycle through them
      result = rnorm(n*length(mean), mean = mean, sd = sd)
      #Create matrix of results. Each row is a point, and each column is a generated set of points.
      resultmatrix = matrix(data = result, ncol = n, byrow = FALSE)
      #Reorganize the result
      gatheredData = gather(as_data_frame(resultmatrix))
      return(gatheredData)
    }
    
    regressionValues$xdata = normaldistribution(samples, data[,1], data[,2])
    regressionValues$ydata = normaldistribution(samples, data[,3], data[,4])
    
    # yerrors = aes(ymax = data[,3] + data[,4], ymin = data[,3] - data[,4])
    # xerrors = aes(xmax = data[,1] + data[,2], xmin = data[,1] - data[,2])
    
    mergeddata = bind_cols(regressionValues$xdata, regressionValues$ydata)
    head(mergeddata)
    mergeddata[,3] = NULL
    names(mergeddata) = c("Index", "xValue", "yValue")
    
    withProgress(message = "Computing Regressions...", {
      regressionfunction = function(x){
        incProgress(1/samples)
        result = lsfit(matrix(x$xValue), x$yValue)
        return(data.frame(as.list(coef(result))))
      }
      
      regressions = mergeddata %>% group_by(Index) %>% do(regressionfunction(.))
    })
    
    #TODO: Check if it would be faster to not constantly reference the reactive values
    
    regressionValues$slopevalues = as.numeric(unlist(regressions[,3]))
    regressionValues$interceptvalues = as.numeric(unlist(regressions[,2]))
    
    regressionValues$bestlineslope = mean(regressionValues$slopevalues)
    regressionValues$bestlineintercept = mean(regressionValues$interceptvalues)
    
    #TODO: Figure out number of standard deviations.
    regressionValues$slopeUncertainty = 2*sd(regressionValues$slopevalues)
    regressionValues$interceptUncertainty = 2*sd(regressionValues$interceptvalues)
    
    regressionValues$highslope = regressionValues$bestlineslope + regressionValues$slopeUncertainty
    regressionValues$highintercept = regressionValues$bestlineintercept + regressionValues$interceptUncertainty
    
    regressionValues$lowslope = regressionValues$bestlineslope - regressionValues$slopeUncertainty
    regressionValues$lowintercept = regressionValues$bestlineintercept - regressionValues$interceptUncertainty
    
    roundedSlopeError = signif(regressionValues$slopeUncertainty, 1)
    slopeExp = floor(log10(roundedSlopeError))
    
    if(slopeExp > 0) {
      roundedSlope = round(regressionValues$bestlineslope, -slopeExp)
    } else {
      roundedSlope = format(round(regressionValues$bestlineslope, -slopeExp), nsmall = -slopeExp)
    }
    
    roundedInterceptError = signif(regressionValues$interceptUncertainty, 1)
    interceptExp = floor(log10(roundedInterceptError))
    
    if(interceptExp > 0) {
      roundedIntercept = round(regressionValues$bestlineintercept, -interceptExp)
    } else {
      roundedIntercept = format(round(regressionValues$bestlineintercept, -interceptExp), nsmall = -interceptExp)
    }
    
    # regressionValues$equationLabel = TeX(paste0("Slope: (", roundedSlope, " $\\pm$ ", roundedSlopeError, ")", input$yUnits, "/", input$xUnits,
    #                                            "Intercept: (", roundedIntercept, " $\\pm$ ", roundedInterceptError, ")", input$yUnits), output = "character")
    
    # test12 = expression(alpha %+-% 5)
    # 
    # regressionValues$equationLabel = bquote(atop("Slope: " ( .(roundedSlope) %+-% .(roundedSlopeError) ) ~ .(test12), 
    #                                              "Intercept: " ( .(roundedIntercept) %+-% .(roundedInterceptError) ) ))
    
    # test243 = TeX(input$yUnits)
    # regressionValues$equationLabel = bquote("test" ~ .(test243[[1]]))
    
    # regressionValues$equationLabel = as.character(bquote(expression("hope this works" ~ .(test243[[1]]))))[[2]]
    
    regressionValues$slopeLabel = paste0("Slope: (", roundedSlope, "\u00B1", roundedSlopeError, ")")
    regressionValues$interceptLabel = paste0("Intercept: (", roundedIntercept, "\u00B1", roundedInterceptError, ")")
  })
  
  
  #Reactive values for positioning the equation label on plot click
  clickValues = reactiveValues(click = NULL)
  
  observeEvent(input$plot_click, {
    clickValues$click = input$plot_click
    # print("1. ~~~~~~~~~~~~~~~~~~~~")
    # print(TeX(input$yUnits, output = "expression"))
    # # print(TeX(input$yUnits, output = "ast"))
    # print("2. ~~~~~~~~~~~~~~~~~~~~~~~")
    # print(TeX(input$yUnits, output = "character"))
    # print(toString(TeX(input$yUnits)))
    # print(toString(TeX(input$yUnits, output = "character")))
    # print("label:")
    # print(TeX(input$yUnits, output = "text"))
    # print(1)
    # print(parse(text = deparse(TeX(input$yUnits, output = "character"))))
    # print(2)
    # print(TeX(input$yUnits, output = "character"))
    # print(3)
    # print(parse(text = TeX(input$yUnits, output = "character")))
    # print(4)
    #print(eval(TeX(input$yUnits)))
    # print(TeX(input$xUnits, output = "character"))
    # print(parse(text = TeX(input$xUnits, output = "character")))
    # print(eval(parse(text = TeX(input$xUnits, output = "character"))))
  })
  
  #Ugly hack to make sure PPI stays the same across screen sizes
  #1218 is standard width on 1920 x 1080 screen
  ppi = function() {
    (session$clientData$output_scatterPlot_width / 1218) * (150 * (input$setPPI / 100))
  }
  
  plot1 = reactive({
    
    if (is.null(input$csvFile)){
      return(NULL)
    }
    
    data = read.csv(input$csvFile$datapath, header = input$header)
    
    yerrors = aes(ymax = data[,3] + data[,4], ymin = data[,3] - data[,4])
    xerrors = aes(xmax = data[,1] + data[,2], xmin = data[,1] - data[,2])
    
    #TODO: Fix this hack
    #A plot used to calculte limits that actually isn't shown
    #The error bars have 0 width/height because they won't be seen but need to be used to calculate limits
    plotframe =
      qplot(data[,1], data[,3]) +
      geom_errorbar(yerrors, width = 0)  +
      geom_errorbarh(xerrors, height = 0)
    
    maxydata = layer_scales(plotframe)$y$range$range[2]
    maxxdata = layer_scales(plotframe)$x$range$range[2]
    minydata = layer_scales(plotframe)$y$range$range[1]
    minxdata = layer_scales(plotframe)$x$range$range[1]
    
    xrange = maxxdata - minxdata
    yrange = maxydata - minydata
    
    #TODO change constant depending upon aspect ratio, do this for error bars too
    ybuffer = 0.05*yrange
    xbuffer = 0.05*xrange
    
    if(is.na(input$xMin)|is.na(input$yMin)|is.na(input$xMax)|is.na(input$yMax)) {
      updateTextInput(session, "xMin", value = minxdata - xbuffer)
      updateTextInput(session, "xMax", value = maxxdata + xbuffer)
      updateTextInput(session, "yMin", value = minydata - ybuffer)
      updateTextInput(session, "yMax", value = maxydata + ybuffer)
    }
    
    observeEvent(input$setAxisToZero, {
      updateTextInput(session, "xMin", value = "0")
      updateTextInput(session, "yMin", value = "0")
    })
    
    #TODO: Figure out issues with selecting two different CSV in a row
    
    if(!is.na(input$xMin) & !is.na(input$yMin) & !is.na(input$xMax) & !is.na(input$yMax)) {
      regressionPlot =
        qplot(data[,1], data[,3]) +
        geom_errorbar(yerrors, width = 0.02*(xrange))  +
        geom_errorbarh(xerrors, height = 0.02*(yrange)*(1/as.numeric(input$aspectRatio))) +
        eval(parse(text = input$selectTheme)) +
        theme(aspect.ratio = input$aspectRatio, plot.background=element_blank()) +
        ggtitle(TeX(input$graphTitle)) +
        xlab(if(input$xUnits != "") {
          TeX(paste0(input$xLabel, " (", input$xUnits, ")"))
        } else {
          TeX(input$xLabel)
        }) +
        ylab(if(input$yUnits != "") {
          TeX(paste0(input$yLabel, " (", input$yUnits, ")"))
        } else {
          TeX(input$yLabel)
        }) +
        xlim(input$xMin, input$xMax) +
        ylim(input$yMin, input$yMax) +
        scale_size_identity()
    } else {
      return(NULL)
    }
    #TODO: Figure out why there is an else clause above.
    
    if(input$xMin < 0) {
      regressionPlot$layers = c(geom_vline(xintercept = 0, alpha = 5/10), regressionPlot$layers)
    }
    
    if(input$yMin < 0) {
      regressionPlot$layers = c(geom_hline(yintercept = 0, alpha = 5/10), regressionPlot$layers)
    }
    
    if(input$showGenerated == TRUE & !is.null(regressionValues$xdata)){
      regressionPlot$layers = c(geom_point(aes(regressionValues$xdata[,2], regressionValues$ydata[,2]), 
                                           color = input$dataColor, alpha = 1/20), regressionPlot$layers)
    }
    
    if(input$showSpread == TRUE & !is.null(regressionValues$interceptvalues)){
      regressionPlot$layers = c(geom_abline(intercept = regressionValues$interceptvalues, slope = regressionValues$slopevalues, 
                                            alpha = 1/10, color = input$spreadColor), regressionPlot$layers)
    }
    
    if(!is.null(regressionValues$bestlineslope)){
      regressionPlot$layers = c(regressionPlot$layers, geom_abline(intercept = regressionValues$bestlineintercept, 
                                                                   slope = regressionValues$bestlineslope))
    }
    
    if(input$showMaxMin == TRUE & !is.null(regressionValues$highintercept)){
      regressionPlot$layers = c(regressionPlot$layers, geom_abline(intercept = regressionValues$highintercept, slope = regressionValues$lowslope, 
                                                                   linetype = 3), geom_abline(intercept = regressionValues$lowintercept, slope = regressionValues$highslope, linetype = 3))
    }
    
    # TODO: Fix label size
    if(input$showEquationFloat == TRUE & !is.null(regressionValues$slopeLabel)){
      if(input$yUnits != "" || input$xUnits != "") {
        equationLabel = as.character(bquote(expression(atop(.(regressionValues$slopeLabel) ~ .(TeX(input$yUnits)[[1]]) * "/" * .(TeX(input$xUnits)[[1]]), 
                                                            .(regressionValues$interceptLabel) ~ .(TeX(input$yUnits)[[1]])))))[[2]]
        parseMath = TRUE
      } else {
        equationLabel = paste0(regressionValues$slopeLabel, "\n", regressionValues$interceptLabel)
        parseMath = FALSE
      }
     
      # regressionPlot$layers = c(regressionPlot$layers, annotate("label", x = clickValues$click$x, y = clickValues$click$y,  
      #                                                           label.r = unit(0, "lines"), label = equationLabel, parse = parseMath, hjust = "left", size = 5 * (input$setPPI / 100)))
      
      regressionPlot$layers = c(regressionPlot$layers, geom_label(aes(x = clickValues$click$x, y = clickValues$click$y), 
                                                                label.r = unit(0, "lines"), label.padding = unit(0.30, "lines"), label = equationLabel, parse = parseMath, hjust = "left", 
                                                                size = (input$setLabelScale / 100) * 5 * (1/(sqrt(ppi() / 100)))))
    }
    
    regressionPlot
    
  })
  
  
  
  observeEvent({
    input$setPPI
    session$clientData$output_scatterPlot_width}, {
    output$scatterPlot = renderPlot(height = 600, res = ppi(), {
      
      plot1()
      
    })
  })
  
  output$downloadPlot = downloadHandler(
    filename = function() {paste0(str_replace(input$csvFile, ".csv", ""), '.', input$fileFormat)},
    content = function(file) {
      width = 1218 / input$setPPI
      switch(input$fileFormat, 
             "pdf" = {
               pdf(file, width = width, height = width * as.numeric(input$aspectRatio))
             },
             "png" = {
               png(file, width = as.numeric(input$downloadResolution), height = as.numeric(input$downloadResolution) * as.numeric(input$aspectRatio), 
                   res = (as.numeric(input$downloadResolution) / 1218) * as.numeric(input$setPPI))
             }, 
             "svg" = {
               svg(file, width = width, height = width * as.numeric(input$aspectRatio))
             })
      print(plot1())
      dev.off()
    }
  )
  
})
  
