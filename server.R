library(shiny)
library(tidyverse)
library(stringr)
library(magrittr)
library(ggthemes)
library(latex2exp)
library(shinyjs)
library(DT)

shinyServer(function(input, output, session) {
  
  dataQuality = reactiveValues()
  
  # Remove the graph tab when a new csv file is uploaded
  shinyjs::hide(selector = "a[data-value='graph']")
  observeEvent(input$csvFile, {
    shinyjs::hide(selector = "a[data-value='graph']")
    dataQuality$good = "Yes"
  })

  # Check if data file is uploaded for Data Table help text.
  
  output$dataFile = reactive({
    return(!is.null(input$csvFile))
  })
  outputOptions(output, "dataFile", suspendWhenHidden = FALSE)
  
  # Disable the download button until the file is uploaded.
  
  observe({
    if(!is.null(input$csvFile)) {
      shinyjs::enable("downloadPlot")
    } else {
      shinyjs::disable("downloadPlot")
    }
  })
  
  # Read the uploaded data file and produce a visual data table.
  
  columnNames = reactiveValues()
  
  output$dataTable = DT::renderDataTable(options = list(columnDefs = list(list(className = 'dt-left', targets = "_all"))), {
    
    if (is.null(input$csvFile)) {
      return(data.frame(X = numeric(0), Y = numeric(0)))
    }

    data = read.csv(input$csvFile$datapath, header = input$header, check.names = FALSE)
    
    # Validate that the CSV file is properly formatted.
    if (ncol(data) != 4) {
      shinyjs::alert("The file uploaded was not properly formatted, please see the tutorial for help.")
      shinyjs::reset("csvFile")
      #TODO: Fix issues with this working only once.
      shinyjs::runjs("Shiny.onInputChange('csvFile', null)")
      return(data.frame(X = numeric(0), Y = numeric(0)))
    }
    
    if(input$header == TRUE) {
      columnNames$x = colnames(data)[1]
      columnNames$y = colnames(data)[3]
    }
    
    names(data) = c('XValue', 'XUncertainty', 'YValue', 'YUncertainty')
    
    data %<>% tidyr::unite(X, XValue, XUncertainty, sep = "\u00B1", remove = TRUE)
    data %<>% tidyr::unite(Y, YValue, YUncertainty, sep = "\u00B1", remove = TRUE)
    
    if(input$header == TRUE) {
      names(data) = c(columnNames$x, columnNames$y)
    }
    
    data
  })
  
  # Switch to graph tab once button in data tab is pressed.
  
  observeEvent(input$graphData, {
    if(is.null(input$csvFile)){
      shinyjs::alert("Please input data before proceeding.")
      return(NULL)
    } else {
      shinyjs::show(selector = "a[data-value='graph']")
      updateTabItems(session, "menu", "graph")
      if(input$header == TRUE) {
        updateTextInput(session, "xLabel", value = columnNames$x)
        updateTextInput(session, "yLabel", value = columnNames$y)
      } else {
        updateTextInput(session, "xLabel", value = "X")
        updateTextInput(session, "yLabel", value = "Y")
      }
    }
  })
  
  # Calculate the regression once clicked and store the calculated values in a reactiveValues() so they can be accessed by other functions
      
  regressionValues = reactiveValues()
  
  observeEvent(input$calculateFit, {
    
    validate(
      need(input$setNumber >= 100 && input$setNumber <= 1000, "Please select a number from 100 to 1,000")
    )
    
    samples = input$setNumber
    
    if(is.null(input$csvFile)) {
      return(NULL)
    }
        
    #Read uploaded data file. Uses header if option is selected in UI.
    data = read.csv(input$csvFile$datapath, header = input$header)
    
    normaldistribution = function(n, mean, uncertainty) {
      #Standard Deviation can be approximated to range/4 with 95% accuracy, range = 2*uncertainty, so sd equals approximately unc/2
      sd = uncertainty/2
      #rnorm() can take vectors, will cycle through them
      result = rnorm(n*length(mean), mean = mean, sd = sd)
      #Create matrix of results. Each row is a point, and each column is a generated set of points.
      resultmatrix = matrix(data = result, ncol = n, byrow = FALSE)
      #Reorganize the result
      gatheredData = gather(as_data_frame(resultmatrix))
      return(gatheredData)
    }
    
    xdata = normaldistribution(samples, data[,1], data[,2])
    ydata = normaldistribution(samples, data[,3], data[,4])
    
    mergeddata = bind_cols(xdata, ydata)
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
    
    # Here the calculated value is compared with that of a standard linear regression and the max is used
    
    linearFit = lm(data[,3] ~ data[,1])
    
    if(summary(linearFit)$coefficients[2,2] > 2*sd(as.numeric(unlist(regressions[,3])))) {
      showNotification("Uncertainty in normal linear regression larger than calculated uncertainty.
                       This may mean that the data is non-linear or uncertainties were underestimated.
                       As generated results aren't used they have been removed from the appearance panel.", 
                       duration = 20, type = "warning")
      
      updateCheckboxInput(session, "showSpread", value = FALSE)
      updateCheckboxInput(session, "showGenerated", value = FALSE)
      
      shinyjs::hide("showSpread")
      shinyjs::hide("showGenerated")
      
      regressionValues$bestlineslope = summary(linearFit)$coefficients[2,1]
      regressionValues$bestlineintercept = summary(linearFit)$coefficients[1,1]
      
      slopeUncertainty = summary(linearFit)$coefficients[2,2]
      interceptUncertainty =  summary(linearFit)$coefficients[1,2]
    } else {
      shinyjs::show("showSpread")
      shinyjs::show("showGenerated")
      
      regressionValues$xdata = xdata
      regressionValues$ydata = ydata
      
      regressionValues$slopevalues = as.numeric(unlist(regressions[,3]))
      regressionValues$interceptvalues = as.numeric(unlist(regressions[,2]))
      
      regressionValues$bestlineslope = mean(regressionValues$slopevalues)
      regressionValues$bestlineintercept = mean(regressionValues$interceptvalues)
      
      slopeUncertainty = 2*sd(regressionValues$slopevalues)
      interceptUncertainty = 2*sd(regressionValues$interceptvalues)
    }
    
    regressionValues$highslope = regressionValues$bestlineslope + slopeUncertainty
    regressionValues$highintercept = regressionValues$bestlineintercept + interceptUncertainty
    
    regressionValues$lowslope = regressionValues$bestlineslope - slopeUncertainty
    regressionValues$lowintercept = regressionValues$bestlineintercept - interceptUncertainty
    
    roundedSlopeError = signif(slopeUncertainty, 1)
    slopeExp = floor(log10(roundedSlopeError))
    
    if(slopeExp > 0) {
      roundedSlope = round(regressionValues$bestlineslope, -slopeExp)
    } else {
      roundedSlope = format(round(regressionValues$bestlineslope, -slopeExp), nsmall = -slopeExp)
    }
    
    roundedInterceptError = signif(interceptUncertainty, 1)
    interceptExp = floor(log10(roundedInterceptError))
    
    if(interceptExp > 0) {
      roundedIntercept = round(regressionValues$bestlineintercept, -interceptExp)
    } else {
      roundedIntercept = format(round(regressionValues$bestlineintercept, -interceptExp), nsmall = -interceptExp)
    }
    
    regressionValues$slopeLabel = paste0("Slope: (", roundedSlope, "\u00B1", roundedSlopeError, ")")
    regressionValues$interceptLabel = paste0("Intercept: (", roundedIntercept, "\u00B1", roundedInterceptError, ")")
    
    regressionValues$verbatimLabel = renderText(paste0(regressionValues$slopeLabel, "\n", regressionValues$interceptLabel))
  })
  
  observe({
    if(!is.null(regressionValues$verbatimLabel)) {
      output$fitResult = regressionValues$verbatimLabel
    } else {
      output$fitResult = renderText(paste0("Slope:", "\n", "Intercept:"))
    }
  })
  
  #Reactive values for positioning the equation label on plot click
  clickValues = reactiveValues(click = NULL)
  
  observeEvent(input$plot_click, {
    clickValues$click = input$plot_click
  })
  
  #Ugly hack to make sure PPI stays the same across screen sizes
  #1218 is standard width on 1920 x 1080 screen
  ppi = function() {
    (session$clientData$output_scatterPlot_width / 1218) * (150 * (input$setPPI / 100))
  }
  
  plotValues = reactiveValues()
  
  observeEvent(input$graphData, {
    if(!is.null(input$csvFile)) {
      
      # Set all regressionValues() to NULL so old calculations are gone
      # TODO: Find a more elegant way to clear regressionValues() in the future
      
      regressionValues$xdata = NULL
      regressionValues$ydata = NULL
      regressionValues$slopevalues = NULL
      regressionValues$interceptvalues = NULL
      regressionValues$bestlineslope = NULL
      regressionValues$bestlineintercept = NULL
      regressionValues$highslope = NULL
      regressionValues$highintercept = NULL
      regressionValues$lowslope = NULL
      regressionValues$lowintercept = NULL
      regressionValues$slopeLabel = NULL
      regressionValues$interceptLabel = NULL
      
      
      # Calculations for the plot limits
      
      data = read.csv(input$csvFile$datapath, header = input$header)
      
      yerrors = aes(ymax = data[,3] + data[,4], ymin = data[,3] - data[,4])
      xerrors = aes(xmax = data[,1] + data[,2], xmin = data[,1] - data[,2])
      
      #TODO: Possibly fix this hack in the future
      #A plot used to calculte limits that actually isn't shown
      #The error bars have 0 width/height because they won't be seen but need to be used to calculate limits
      plotframe =
        qplot(data[,1], data[,3]) +
        geom_errorbar(yerrors, width = 0)  +
        geom_errorbarh(xerrors, height = 0)
      
      maxYdata = layer_scales(plotframe)$y$range$range[2]
      maxXdata = layer_scales(plotframe)$x$range$range[2]
      minYdata = layer_scales(plotframe)$y$range$range[1]
      minXdata = layer_scales(plotframe)$x$range$range[1]
      
      plotValues$xrange = maxXdata - minXdata
      plotValues$yrange = maxYdata - minYdata

      yBuffer = 0.05*plotValues$yrange*(1/as.numeric(input$aspectRatio))
      xBuffer = 0.05*plotValues$xrange
      
      updateTextInput(session, "xMin", value = minXdata - xBuffer)
      updateTextInput(session, "xMax", value = maxXdata + xBuffer)
      updateTextInput(session, "yMin", value = minYdata - yBuffer)
      updateTextInput(session, "yMax", value = maxYdata + yBuffer)
    }
  })
  
  observeEvent(input$setAxisToZero, {
    updateTextInput(session, "xMin", value = "0")
    updateTextInput(session, "yMin", value = "0")
  })
  
  plot1 = reactive({
    
    if (is.null(input$csvFile)){
      return(NULL)
    }
    
    data = read.csv(input$csvFile$datapath, header = input$header)
    
    yerrors = aes(ymax = data[,3] + data[,4], ymin = data[,3] - data[,4])
    xerrors = aes(xmax = data[,1] + data[,2], xmin = data[,1] - data[,2])
    
    if(!is.na(input$xMin) & !is.na(input$yMin) & !is.na(input$xMax) & !is.na(input$yMax)) {
      regressionPlot =
        qplot(data[,1], data[,3]) +
        geom_errorbar(yerrors, width = 0.02*(plotValues$xrange))  +
        geom_errorbarh(xerrors, height = 0.02*(plotValues$yrange)*(1/as.numeric(input$aspectRatio))) +
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
        scale_x_continuous(limits = c(input$xMin, input$xMax), expand = c(0,0)) +
        scale_y_continuous(limits = c(input$yMin, input$yMax), expand = c(0,0))
        # xlim(input$xMin, input$xMax) +
        # ylim(input$yMin, input$yMax)
    } else {
      return(NULL)
    }
    
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
                                                                   linetype = "longdash", size = 0.3), geom_abline(intercept = regressionValues$lowintercept, slope = regressionValues$highslope, linetype = "longdash", size = 0.3))
    }
    
    if(input$showEquationFloat == TRUE & !is.null(regressionValues$slopeLabel)){
      if(input$yUnits != "" || input$xUnits != "") {
        equationLabel = as.character(bquote(expression(atop(.(regressionValues$slopeLabel) ~ .(TeX(input$yUnits)[[1]]) * "/" * .(TeX(input$xUnits)[[1]]), 
                                                            .(regressionValues$interceptLabel) ~ .(TeX(input$yUnits)[[1]])))))[[2]]
        parseMath = TRUE
      } else {
        equationLabel = paste0(regressionValues$slopeLabel, "\n", regressionValues$interceptLabel)
        parseMath = FALSE
      }
      
      if(input$advancedSettings %% 2 == 1){
        xPosition = clickValues$click$x
        yPosition = clickValues$click$y
      } else {
        xRange = input$xMax - input$xMin
        xPosition = input$xMin + 0.1*xRange*(9/16)
        yRange = input$yMax - input$yMin
        if(regressionValues$bestlineslope > 0){
          yPosition = input$yMin + 0.9*yRange
        } else {
          yPosition = input$yMin + 0.1*yRange
        }
      }
      
      regressionPlot$layers = c(regressionPlot$layers, geom_label(aes(x = xPosition, y = yPosition), 
                                                                label.r = unit(0, "lines"), label.padding = unit(0.30, "lines"), label = equationLabel, parse = parseMath, hjust = "left", 
                                                                size = (input$setLabelScale / 100) * 50 * (1/(sqrt(150 * (input$setPPI / 100))))))
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
      width = 1218 / (150 * (input$setPPI/100))
      switch(input$fileFormat, 
             "pdf" = {
               pdf(file, width = width, height = width * as.numeric(input$aspectRatio))
             },
             "png" = {
               png(file, width = as.numeric(input$downloadResolution), height = as.numeric(input$downloadResolution) * as.numeric(input$aspectRatio), 
                   res = (as.numeric(input$downloadResolution) / 1218) * (150 * (as.numeric(input$setPPI)/100)))
             }, 
             "svg" = {
               svg(file, width = width, height = width * as.numeric(input$aspectRatio))
             })
      print(plot1())
      dev.off()
    }
  )
  
})
  
