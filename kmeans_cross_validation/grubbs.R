# Author: dxjester 
# Comments: Using crime data from the file uscrime.txt (http://www.statsci.org/data/general/uscrime.txt, description at http://www.statsci.org/data/general/uscrime.html), test to see whether there are any outliers in the last column (number of crimes per 100,000 people). Use the grubbs.test function in the outliers package in R.
# install.packages("outliers")
install.packages("DAAG")
# -------------------------------------------------------------- # 
# --------------------- Environment Setup ---------------------- # 
# -------------------------------------------------------------- # 

library(ggplot2) 
library(outliers) 

rm(list = ls())

# Phase 1: Read in table, manually inspect for any outliers by predictor variables
crimeData <- read.table("uscrime.txt", stringsAsFactors = FALSE, header = TRUE)
head(crimeData)

boxplot(crimeData$Crime)
boxplot(crimeData[,-c(12, 16)])

timePlot <- ggplot(crimeData, aes(x = Time, y = Crime, color=Crime)) + geom_point() + geom_line()
plot(timePlot)

wealthPlot<- ggplot(crimeData, aes(x = Wealth, y = Crime, color=Crime)) + geom_point()
plot(wealthPlot)

popPlot <- ggplot(crimeData, aes(x = Pop, y = Crime, color=Crime)) + geom_point()
plot(popPlot)

ineqPlot <- ggplot(crimeData, aes(x = Ineq, y = Crime, color=Crime)) + geom_point()
plot(ineqPlot)

# Phase 2: Normalize the data set, execute grubbs function
shapiro.test(crimeData$Crime)
qqnorm(crimeData$Crime)

# Test for one side outlier
gTest1 <- grubbs.test(crimeData$Crime, type = 10)
gTest1$p.value
gTest1$alternative
gTest1$p.value

# Test for two sideded outlier
gTest2 <- grubbs.test(crimeData$Crime, type = 11)
gTest2$p.value
gTest2$alternative
gTest2$p.value

# Exception error whe type = 20
gTest3 <- grubbs.test(crimeData$Crime, type = 20)
gTest3$p.value