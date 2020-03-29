# Filename: HW# 4 - Benitez P. 
# Question: 7.2
# Author: Patrick C. Benitez 
# Date created: 5-FEB-19
# Date updated: 5-FEB-19
# install.packages("xlsx")
# install.packages("rJava")
# install.packages("xlsxjars")

# -------------------------------------------------------------- # 
# --------------------- BEGIN 7.2 PROBLEM ---------------------- # 
# -------------------------------------------------------------- # 

library(ggplot2) 
library(outliers) 

rm(list = ls())

# Phase 1: Read in table, manually inspect for any outliers by predictor variables
tempsData <- read.table("temps.txt", stringsAsFactors = FALSE, header = TRUE)
head(tempsData)

# Phase 2: Organize data into vector, transition into a time series format
tempsVector <- as.vector(unlist(tempsData[,2:21]))
tempsVector

tempsTimeSeries <- ts(tempsVector, start=1996,frequency = 123)
tempsTimeSeries
plot(tempsTimeSeries)

# Phase 3: Utilize HoltWinters function & plot
tempsHW <- HoltWinters(tempsTimeSeries, alpha = NULL, beta = NULL, gamma = NULL, seasonal = "multiplicative")
head(tempsHW)

plot(tempsHW)

plot(tempsHW$fitted)

plot(decompose(tempsTimeSeries, type="multiplicative"))

# Phase 4: Apply Shapiro test, normalize
shapiro.test(tempsTimeSeries)
qqnorm(tempsTimeSeries)

# Phase 5: Export data to Excel
write.csv(fitted(tempsHW), file = "tempsExport.csv")