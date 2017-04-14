library("ggplot2")
library("ggthemes")
library("scales")
library("reshape2")
library("nlme")

#args = commandArgs(trailingOnly = TRUE)

# Give data as csv file with columns x, xuncertainty, y, yuncertainty (no labels)

data = read.csv("testvalues.csv", header = FALSE)

normaldistribution = function(n, mean, uncertainty) {
  #Standard Deviation can be approximated to range/4 with 95% accuracy, range = 2*uncertainty, so sd equals approximately unc/2
  sd = uncertainty/2
  #rnorm() can take vectors (term for lists), will cycle through them
  result = rnorm(n*length(mean), mean = mean, sd = sd)
  #Create matrix of results. Each row is a point, and each column is a generated set of points.
  resultmatrix = matrix(result, , ncol = n, byrow = FALSE)
  #Reorganize the result
  melteddata = melt(resultmatrix)
  melteddata[,1] = NULL
  return(melteddata)
}

xdata = normaldistribution(100, data[,1], data[,2])
ydata = normaldistribution(100, data[,3], data[,4]) # How many of these for good accuracy?

yerrors = aes(ymax = data[,3] + data[,4], ymin = data[,3] - data[,4])
xerrors = aes(xmax = data[,1] + data[,2], xmin = data[,1] - data[,2])

mergeddata = cbind(xdata, ydata)
mergeddata[,3] = NULL
names(mergeddata) = c("Index", "xValue", "yValue")

regressions = coef(lmList(yValue ~ xValue | Index, data = mergeddata, pool = FALSE))
#See https://stat.ethz.ch/R-manual/R-devel/library/nlme/html/intervals.lmList.html for info on confidence intervals in Lmlist
#Used to get confidence interval for each individual line fit 
#Possibly weight by uncertainty?
#See: https://stackoverflow.com/questions/14636052/using-smooth-in-ggplot2-to-fit-a-linear-model-using-the-errors-given-in-the-data


bestlineslope = mean(regressions[,2])
bestlineintercept = mean(regressions[,1])

slopeUncertainty = 2*sd(regressions[,2])
interceptUncertainty = 2*sd(regressions[,1])

highslope = mean(regressions[,2]) + slopeUncertainty
highintercept = mean(regressions[,1]) + interceptUncertainty

lowslope = mean(regressions[,2]) - slopeUncertainty
lowintercept = mean(regressions[,1]) - interceptUncertainty


#Plot original data and error bars, generated data, and best fit line with uncertainty
plot1 = 
  qplot(data[,1], data[,3]) + 
  geom_errorbar(yerrors, width = 0.2)  + 
  geom_errorbarh(xerrors, height = 0.3) + 
  geom_abline(intercept = bestlineintercept, slope = bestlineslope) +
  geom_abline(intercept = highintercept, slope = lowslope) + 
  geom_abline(intercept = lowintercept, slope = highslope) + 
  geom_point(aes(xdata[,2], ydata[,2]), color = "red", alpha = 1/20) +
  geom_abline(intercept = regressions[,1], slope = regressions[,2], alpha = 1/10, color = "grey")

#Theming of the plot
plot1 = 
  plot1 + 
  theme_tufte() + 
  geom_rangeframe() + 
  scale_x_continuous(breaks = extended_range_breaks()(data[,1])) + 
  scale_y_continuous(breaks = extended_range_breaks()(data[,3])) 


# TODO: Fix the graph labeling and make it look better, look into ggtheme on github
# TODO: Fix errorbar height and width so it changes depending on graph scale

cat("Slope: ", bestlineslope, "Slope Uncertainty: ", slopeUncertainty, 
    "\nIntercept: ", bestlineintercept, "Intercept Uncertainty: ", interceptUncertainty)

#TODO: Fix errors with inconsistency in produced values.

print(plot1)
