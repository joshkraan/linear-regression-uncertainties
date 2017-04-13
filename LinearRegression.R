library("ggplot2")
library("ggthemes")
library("scales")
library("reshape2")
library("nlme")

#args = commandArgs(trailingOnly = TRUE)

# Give data as csv file with columns x, xuncertainty, y, yuncertainty (no labels)



data = read.csv("testvalues.csv", header = FALSE)

normaldistribution = function(n, mean, uncertainty) {
  sd = uncertainty/2 # sd approximately equals range/4 with 95% accuracy, range = 2*uncertainty, so sd approximately is unc/2
  result = rnorm(n*length(mean), mean = mean, sd = sd) # rnorm() can take vectors (term for lists), will cycle through them
  resultmatrix = matrix(result, , ncol = n, byrow = FALSE)
  melteddata = melt(resultmatrix)
  melteddata[,1] = NULL
  return(melteddata)
}

xdata = normaldistribution(100, data[,1], data[,2])
ydata = normaldistribution(100, data[,3], data[,4])

yerrors = aes(ymax = data[,3] + data[,4], ymin = data[,3] - data[,4])
xerrors = aes(xmax = data[,1] + data[,2], xmin = data[,1] - data[,2])

mergeddata = cbind(xdata, ydata)
mergeddata[,3] = NULL
names(mergeddata) = c("Index", "xValue", "yValue")

regressions = coef(lmList(yValue ~ xValue | Index, data = mergeddata, pool = FALSE))

bestlineslope = mean(regressions[,2])
bestlineintercept = mean(regressions[,1])

highslope = mean(regressions[,2]) + 2*sd(regressions[,2])
highintercept = mean(regressions[,1]) + 2*sd(regressions[,1])

lowslope = mean(regressions[,2]) - 2*sd(regressions[,2])
lowintercept = mean(regressions[,1]) - 2*sd(regressions[,1])

plot1 = qplot(data[,1], data[,3])
plot1 = plot1 + theme_tufte() + geom_rangeframe() + 
  scale_x_continuous(breaks = extended_range_breaks()(data[,1])) + scale_y_continuous(breaks = extended_range_breaks()(data[,3])) + 
  geom_errorbar(yerrors, width = 0.2) + geom_point(aes(xdata[,2], ydata[,2], col = "gray", alpha = 0.01)) +
  geom_errorbarh(xerrors, height = 0.3) + geom_abline(intercept = bestlineintercept, slope = bestlineslope) +
  geom_abline(intercept = highintercept, slope = lowslope) + geom_abline(intercept = lowintercept, slope = highslope)
# TODO: Fix the graph labeling and make it look better, look into ggtheme on github
# TODO: Fix errorbar height and width so it changes depending on graph scale



print(plot1)
