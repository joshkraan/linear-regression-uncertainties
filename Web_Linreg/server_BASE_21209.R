library(shiny)
library(ggplot2)
library(ggthemes)
library(scales)
library(reshape2)
library(nlme)

#TODO: Seperate graph and regression calculations
#TODO: Use ggsave()
#TODO: Add density plots
#TODO: Add option to disable/enable floating label
#Position label within graph bounds
#TODO: Add dropdown with selectable theme.

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
    
    withProgress(message = "Calculating Fit", value = 1, {
      
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
      
      xdata = normaldistribution(input$setNumber, data[,1], data[,2])
      ydata = normaldistribution(input$setNumber, data[,3], data[,4]) # How many of these for good accuracy?
      
      yerrors = aes(ymax = data[,3] + data[,4], ymin = data[,3] - data[,4])
      xerrors = aes(xmax = data[,1] + data[,2], xmin = data[,1] - data[,2])
      
      mergeddata = cbind(xdata, ydata)
      mergeddata[,3] = NULL
      names(mergeddata) = c("Index", "xValue", "yValue")
      
      #This takes the most time.
      regressions = coef(lmList(yValue ~ xValue | Index, data = mergeddata, pool = FALSE))
      #See https://stat.ethz.ch/R-manual/R-devel/library/nlme/html/intervals.lmList.html for info on confidence intervals in Lmlist
      #Used to get confidence interval for each individual line fit 
      #Possibly weight by uncertainty?
      #See: https://stackoverflow.com/questions/14636052/using-smooth-in-ggplot2-to-fit-a-linear-model-using-the-errors-given-in-the-data
      
      
      bestlineslope = mean(regressions[,2])
      bestlineintercept = mean(regressions[,1])
      
      slopeUncertainty = 3*sd(regressions[,2])
      interceptUncertainty = 3*sd(regressions[,1])
      
      highslope = mean(regressions[,2]) + slopeUncertainty
      highintercept = mean(regressions[,1]) + interceptUncertainty
      
      lowslope = mean(regressions[,2]) - slopeUncertainty
      lowintercept = mean(regressions[,1]) - interceptUncertainty
      
      output$scatterPlot = renderPlot({
        
        inputFile = input$csvFile
        
        if (is.null(inputFile)){
          return(NULL)
        }
        
        data = read.csv(inputFile$datapath, header = input$header)
        
        plot1 = 
          qplot(data[,1], data[,3]) + 
          geom_errorbar(yerrors, width = 0.2)  + 
          geom_errorbarh(xerrors, height = 0.3) + 
          geom_abline(intercept = bestlineintercept, slope = bestlineslope)
        
        xrange = layer_scales(plot1)$x$range$range[2] - layer_scales(plot1)$x$range$range[1]
        yrange = layer_scales(plot1)$y$range$range[2] - layer_scales(plot1)$y$range$range[1]
        
        equationLabel = paste("Slope:\n", bestlineslope, "\u00B1", slopeUncertainty, "\nIntercept:\n", bestlineintercept,
                              "\u00B1", interceptUncertainty)
        
        if(input$showSpread == TRUE){
          plot1 = 
            plot1 + 
            geom_abline(intercept = regressions[,1], slope = regressions[,2], alpha = 1/10, color = input$spreadColor)
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
            geom_label(aes(xoffset, yoffset, label = equationLabel, hjust = "left"), label.r = unit(0, "lines"))
        }
        
        plot1 = 
          plot1 + 
          theme_bw()
        
        print(plot1)
        
      })
      
    })

  })
  
})
  
