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
  
  #Disable the download button until sufficient options are set.
  shinyjs::disable("downloadPlot")
  
  #Read the uploaded data file and produce a visual data table.
  output$dataTable = DT::renderDataTable(options = list(columnDefs = list(list(className = 'dt-left', targets = "_all"))), {
    inputFile = input$csvFile
    
    if (is.null(inputFile)) {
      return(data.frame(X = numeric(0), Y = numeric(0)))
    }
    
    
    #TODO: Use readr
    data = read.csv(inputFile$datapath, header = input$header)
    
    names(data) = c('XValue', 'XUncertainty', 'YValue', 'YUncertainty')
    
    #TODO: clean up the following
    
    data %<>% unite(X, XValue, XUncertainty, sep = "\u00B1", remove = TRUE)
    data %<>% unite(Y, YValue, YUncertainty, sep = "\u00B1", remove = TRUE)
    
    data
  })
  
  #Switch to graph tab once button in data tab is pressed.
  observeEvent(input$graphData, {
    if(is.null(input$csvFile)){
      #TODO: Dismiss this warning after some time.
      createAlert(session, "alert", title = "Error", content = "Please input data before proceeding.", append = FALSE, style = "danger")
      return(NULL)
    }
    updateTabItems(session, "menu", "graph")
  })
      
  #regressionValues = reactiveValues(xdata, ydata, slopevalues, interceptvalues, bestlineslope, bestlineintercept, slopeUncertainty, interceptUncertainty, highslope, highintercept, lowslope, lowintercept)
  regressionValues = reactiveValues()
  
  observeEvent(input$calculateFit, {
    
    validate(
      need(input$setNumber >= 100 && input$setNumber <= 100000, "Please select a number from 100 to 100,000")
    )
    
    samples = input$setNumber
    
    inputFile = input$csvFile
    
    if (is.null(inputFile))
      return(NULL)
    
    #Read uploaded data file. Uses header if option is selected in UI.
    data = read.csv(inputFile$datapath, header = input$header)
    
    normaldistribution = function(n, mean, uncertainty) {
      #Standard Deviation can be approximated to range/4 with 95% accuracy, range = 2*uncertainty, so sd equals approximately unc/2
      # Two or 3 standard deviations?
      sd = uncertainty/3
      #rnorm() can take vectors (term for lists), will cycle through them
      result = rnorm(n*length(mean), mean = mean, sd = sd)
      #Create matrix of results. Each row is a point, and each column is a generated set of points.
      resultmatrix = matrix(data = result, ncol = n, byrow = FALSE)
      #Reorganize the result
      gatheredData = gather(as_data_frame(resultmatrix))
      #melteddata[,1] = NULL
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
    
    regressionValues$slopevalues = as.numeric(unlist(regressions[,3]))
    regressionValues$interceptvalues = as.numeric(unlist(regressions[,2]))
    
    regressionValues$bestlineslope = mean(regressionValues$slopevalues)
    regressionValues$bestlineintercept = mean(regressionValues$interceptvalues)
    
    regressionValues$slopeUncertainty = 3*sd(regressionValues$slopevalues)
    regressionValues$interceptUncertainty = 3*sd(regressionValues$interceptvalues)
    
    regressionValues$highslope = regressionValues$bestlineslope + regressionValues$slopeUncertainty
    regressionValues$highintercept = regressionValues$bestlineintercept + regressionValues$interceptUncertainty
    
    regressionValues$lowslope = regressionValues$bestlineslope - regressionValues$slopeUncertainty
    regressionValues$lowintercept = regressionValues$bestlineintercept - regressionValues$interceptUncertainty
    
    #plotclick = reactive(input$plot_click)
    
  })
  
  output$scatterPlot = renderPlot(height = 600, res = input$setPPI, {
    
    inputFile = input$csvFile
    
    if (is.null(inputFile)){
      return(NULL)
    }
    
    data = read.csv(inputFile$datapath, header = input$header)
    
    yerrors = aes(ymax = data[,3] + data[,4], ymin = data[,3] - data[,4])
    xerrors = aes(xmax = data[,1] + data[,2], xmin = data[,1] - data[,2])
    
    #TODO: Fix slight error in graph limits
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
    
    #TODO possibly change constant depending upon aspect ratio so one isn't larger, do this for error bars too
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
      plot1 = 
        qplot(data[,1], data[,3]) + 
        geom_errorbar(yerrors, width = 0.02*(xrange))  + 
        geom_errorbarh(xerrors, height = 0.02*(yrange)) + 
        eval(parse(text = input$selectTheme)) +
        theme(aspect.ratio = input$aspectRatio, plot.background=element_blank()) +
        ggtitle(TeX(input$graphTitle)) +
        xlab(TeX(input$xLabel)) +
        ylab(TeX(input$yLabel)) +
        xlim(input$xMin, input$xMax) +
        ylim(input$yMin, input$yMax)
      
      #TODO figure out the x and y axis lines
      if((input$xMax - input$xMin) > input$xMax) {
        plot1 =
          plot1 +
          geom_vline(0)
      }
    } else {
      return(NULL)
    }
    
    equationLabel = paste("Slope:\n", regressionValues$bestlineslope, "\u00B1", regressionValues$slopeUncertainty, "\nIntercept:\n", regressionValues$bestlineintercept,
                          "\u00B1", regressionValues$interceptUncertainty)
    
    if(!is.null(regressionValues$bestlineslope)){
      plot1 = 
        plot1 +
        geom_abline(intercept = regressionValues$bestlineintercept, slope = regressionValues$bestlineslope)
    }
    
    if(input$showSpread == TRUE & !is.null(regressionValues$interceptvalues)){
      plot1 = 
        plot1 + 
        geom_abline(intercept = regressionValues$interceptvalues, slope = regressionValues$slopevalues, alpha = 1/10, color = input$spreadColor)
    }
    
    if(input$showGenerated == TRUE & !is.null(regressionValues$xdata)){
      plot1 = 
        plot1 +
        geom_point(aes(regressionValues$xdata[,2], regressionValues$ydata[,2]), color = input$dataColor, alpha = 1/20)
    }
    
    if(input$showMaxMin == TRUE & !is.null(regressionValues$highintercept)){
      plot1 = 
        plot1 +
        geom_abline(intercept = regressionValues$highintercept, slope = regressionValues$lowslope, linetype = 3) + 
        geom_abline(intercept = regressionValues$lowintercept, slope = regressionValues$highslope, linetype = 3)
    }
    
    #TODO fix equation
    if(input$showEquationFloat == TRUE){
      
      plot1 =
        plot1 +
        annotate("text", x = input$plot_click$x, y = input$plot_click$y, label = equationLabel)
      
    }
    
    observe({
      if(!is.null(inputFile)) {
        shinyjs::enable("downloadPlot")
      }
    })
    
    output$downloadPlot = downloadHandler(
      filename = function() {paste0(str_replace(input$csvFile, ".csv", ""), '.', input$fileFormat)},
      content = function(file) {
        ggsave(file, plot = plot1, device = input$fileFormat)
      }
    )
    
    return(plot1)
    
  })
  
  
})
  
