library("ggplot2")
library("ggthemes")
library("scales")
library("reshape2")
library(tidyverse)

#TODO: Take max of found uncertainty vs uncertainty in fit of original data

# Give data as csv file with columns: x, xuncertainty, y, yuncertainty
# If you have a header on your file set header = TRUE below.

data = read.csv("testvalues4.csv", header = FALSE)

#TODO Fix this to use tidyverse conventions
normaldistribution = function(n, mean, uncertainty) {
  #Assuming range contains 95% of data, sd = range/4
  #Range = 2*uncertainty
  sd = uncertainty/2
  #rnorm() can take vectors (term for lists), will cycle through them
  result = rnorm(n*length(mean), mean = mean, sd = sd)
  #Create matrix of results. Each row is a point, and each column is a generated set of points.
  resultmatrix = matrix(data = result, ncol = n, byrow = FALSE)
  #Reorganize the result
  gatheredData = gather(as_data_frame(resultmatrix))
  return(gatheredData)
}

#Change the n value for these for more accuracy. Values of 10000 or higher seem to work well, but take some time.
numberofpoints = 1000
xdata = normaldistribution(numberofpoints, data[,1], data[,2])
ydata = normaldistribution(numberofpoints, data[,3], data[,4])

yerrors = aes(ymax = data[,3] + data[,4], ymin = data[,3] - data[,4])
xerrors = aes(xmax = data[,1] + data[,2], xmin = data[,1] - data[,2])

mergeddata = cbind(xdata, ydata)
mergeddata[,3] = NULL
names(mergeddata) = c("Index", "xValue", "yValue")

regressions = mergeddata %>% group_by(Index) %>% do(data.frame(as.list(coef(lsfit(matrix(.$xValue), .$yValue)))))

slopevalues = as.numeric(unlist(regressions[,3]))
interceptvalues = as.numeric(unlist(regressions[,2]))

bestlineslope = mean(slopevalues)
bestlineintercept = mean(interceptvalues)

slopeUncertainty = 2*sd(slopevalues)
interceptUncertainty = 2*sd(interceptvalues)

highslope = bestlineslope + slopeUncertainty
highintercept = bestlineintercept + interceptUncertainty

lowslope = bestlineslope - slopeUncertainty
lowintercept = bestlineintercept - interceptUncertainty

plot1 = 
  qplot(data[,1], data[,3]) + 
  theme_bw() +
  #Change the below errorbar width and heights to match the graph.
  geom_errorbar(yerrors, width = 0.2)  + 
  geom_errorbarh(xerrors, height = 0.3) + 
  geom_abline(intercept = bestlineintercept, slope = bestlineslope) +
  geom_abline(intercept = highintercept, slope = lowslope, linetype = 3) + 
  geom_abline(intercept = lowintercept, slope = highslope, linetype = 3) #+ 
  #Below shows the generated data in red.
  #geom_point(aes(xdata[,2], ydata[,2]), color = "red", alpha = 1/150) +
  #Below shows the generated regressions lines in grey.
  #geom_abline(intercept = interceptvalues, slope = slopevalues, alpha = 1/50, color = "grey")

print(plot1)

cat("\nSlope: ", bestlineslope, "Slope Uncertainty: ", slopeUncertainty, 
    "\nIntercept: ", bestlineintercept, "Intercept Uncertainty: ", interceptUncertainty)
