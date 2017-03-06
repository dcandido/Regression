# Load the data.
sulfateData <- read.table(url("http://www.statsci.org/data/general/brunhild.txt"), header = TRUE)

# Take the log of every time and every concentration
logSulfateData <- log(sulfateData)

# Linear regress the logs
logSulfateLinearModel <− lm(Sulfate ~ Hours, data = logSulfateData)

# Plot the regression line on log coordinates
plot(logSulfateData)
title(main = "Sulfate Concentration over Time, log-log")
abline(logSulfateLinearModel)

# Plot the regression curve on original coordinates
plot(sulfateData)
title(main = "Sulfate Concentration over Time\nLog-log Regression in Original Coordinates")
lines(exp(predict(logSulfateLinearModel))~sulfateData$Hours)

# Plot residuals against fitted values in log coordinates
plot(logSulfateLinearModel$fitted.values, logSulfateLinearModel$residuals, xlab = "Fitted Value", ylab = "Residual")
title(main = "Sulfate Fitted Values and Residuals\nLog Sulfate")
abline(h = 0) # The zero residual line

# Plot residuals against fitted values in original coordinates
origCoordResiduals <- array(dim = length(logSulfateLinearModel$residuals))
for (i in 1:length(logSulfateLinearModel$residuals)) {
	origCoordResiduals[i] <- sulfateData$Sulfate[i] - exp(logSulfateLinearModel$fitted.values[i])
}
plot(exp(logSulfateLinearModel$fitted.values), origCoordResiduals, xlab = "Fitted Value", ylab = "Residual")
title(main = "Sulfate Fitted Values and Residuals\nLog Sulfate Regression in Original Coordinates")
abline(h = 0) # The zero residual line