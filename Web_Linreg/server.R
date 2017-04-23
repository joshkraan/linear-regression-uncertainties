library(shiny)
library(ggplot2)
library(ggthemes)
library(scales)
library(reshape2)
library(nlme)
library(dplyr)
library(Cairo)
library(latex2exp)
options(shiny.usecairo=T)

shinyServer(function(input, output) {
  
  output$dataTable = renderTable({
    inputFile = input$csvFile
    
    if (is.null(inputFile))
      return(NULL)
    
    #Read uploaded data file. Uses header if option is selected in UI.
    data = read.csv(inputFile$datapath, header = input$header)
    
    names(data) = c('X Value', 'X Uncertainty', 'Y Value', 'Y Uncertainty')
    
    data
  })
      
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
      resultmatrix = matrix(result, , ncol = n, byrow = FALSE)
      #Reorganize the result
      melteddata = melt(resultmatrix)
      melteddata[,1] = NULL
      return(melteddata)
    }
    
    xdata = normaldistribution(samples, data[,1], data[,2])
    ydata = normaldistribution(samples, data[,3], data[,4])
    
    yerrors = aes(ymax = data[,3] + data[,4], ymin = data[,3] - data[,4])
    xerrors = aes(xmax = data[,1] + data[,2], xmin = data[,1] - data[,2])
    
    mergeddata = cbind(xdata, ydata)
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
  
    #regressions = coef(lmList(yValue ~ xValue | Index, data = mergeddata, pool = FALSE))
    
    slopevalues = as.numeric(unlist(regressions[,3]))
    interceptvalues = as.numeric(unlist(regressions[,2]))
    
    bestlineslope = mean(slopevalues)
    bestlineintercept = mean(interceptvalues)
    
    slopeUncertainty = 3*sd(slopevalues)
    interceptUncertainty = 3*sd(interceptvalues)
    
    highslope = bestlineslope + slopeUncertainty
    highintercept = bestlineintercept + interceptUncertainty
    
    lowslope = bestlineslope - slopeUncertainty
    lowintercept = bestlineintercept - interceptUncertainty
    
    output$scatterPlot = renderPlot(height = 600, res = input$setPPI, {
      
      inputFile = input$csvFile
      
      if (is.null(inputFile)){
        return(NULL)
      }
      
      data = read.csv(inputFile$datapath, header = input$header)
      
      plot1 = 
        qplot(data[,1], data[,3]) + 
        geom_errorbar(yerrors, width = 0.2)  + 
        geom_errorbarh(xerrors, height = 0.3) + 
        geom_abline(intercept = bestlineintercept, slope = bestlineslope) +
        theme_bw() +
        theme(aspect.ratio = input$aspectRatio) +
        xlab(TeX(input$xLabel)) +
        ylab(TeX(input$yLabel))
      
      xrange = layer_scales(plot1)$x$range$range[2] - layer_scales(plot1)$x$range$range[1]
      yrange = layer_scales(plot1)$y$range$range[2] - layer_scales(plot1)$y$range$range[1]
      
      equationLabel = paste("Slope:\n", bestlineslope, "\u00B1", slopeUncertainty, "\nIntercept:\n", bestlineintercept,
                            "\u00B1", interceptUncertainty)
      
      if(input$showSpread == TRUE){
        plot1 = 
          plot1 + 
          geom_abline(intercept = interceptvalues, slope = slopevalues, alpha = 1/10, color = input$spreadColor)
      }
      
      if(input$showGenerated == TRUE){
        plot1 = 
          plot1 +
          geom_point(aes(xdata[,2], ydata[,2]), color = input$dataColor, alpha = 1/20)
      }
      
      if(input$showMaxMin == TRUE){
        plot1 = 
          plot1 +
          geom_abline(intercept = highintercept, slope = lowslope) + 
          geom_abline(intercept = lowintercept, slope = highslope)
      }
      
      if(input$showEquationFloat == TRUE){
        xoffset = (input$equationHorizontal/100)*xrange
        yoffset = (input$equationVertical/100)*yrange
        
        plot1 = 
          plot1 +
          annotate("text", xoffset, yoffset, label = equationLabel)
      }
      
      plot1
      
    })

  })
  
})
  
