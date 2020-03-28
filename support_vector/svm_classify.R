# Author: dxjester
# Comments:  R script to calculate classification line for credit card data # with various kernels

set.seed(654)

# Load library
library(kernlab)

# read in credit card data from working directory
ccData <- read.table("credit_card_data.txt", stringsAsFactors = FALSE, header = FALSE)

# Start trial number: 1
# Kernel: laplacedot
# C Value: 100
# Accuracy: 100%
# Comments: Initial (proposed) C value provided in the homework assignment

ksFunction <- function(x){
  #values <- vector(mode="numeric", length="654") # create a vector to fill on assigned values for each data point
  
  loopKS <- ksvm(V11~., data = ccData, type="C-svc", kernel="laplacedot", C=10^x,scaled=TRUE)
  values <- predict(loopKS,ccData[,1:10])
  returnValue = sum(values == ccData[,11]) / nrow(ccData)
  return(returnValue)
}

loopKStest <-vector(mode="numeric", length = "10")
for(x in 1:10){
  loopKStest[x] = ksFunction(x)
}

print(loopKStest)
plot(loopKStest) # plot values based on 'k' index
title("Polydot Accuracy by K Values")
# End lpacedot trial 

# Start trial number: 2
# Kernel: Polydot
# C Value: multiple, dynamic
# Accuracy: various
# Comments: 

ksFunction <- function(x){
  #values <- vector(mode="numeric", length="654") # create a vector to fill on assigned values for each data point
  
  loopKS <- ksvm(V11~., data = ccData, type="C-svc", kernel="polydot", C=10^x,scaled=TRUE)
  values <- predict(loopKS,ccData[,1:10])
  returnValue = sum(values == ccData[,11]) / nrow(ccData)
  return(returnValue)
}

loopKStest <-vector(mode="numeric", length = "10")
for(x in 1:10){
  loopKStest[x] = ksFunction(x)
}

print(loopKStest)
plot(loopKStest) # plot values based on 'k' index
title("Laplacedot Accuracy by K Values")
# End polydot trial 2

# Start trial number: 3
# Kernel: rbfdot
# C Value: multiple, dynamic
# Accuracy: various
# Comments: 

ksFunction <- function(x){
  #values <- vector(mode="numeric", length="654") # create a vector to fill on assigned values for each data point
  
  loopKS <- ksvm(V11~., data = ccData, type="C-svc", kernel="rbfdot", C=10^x,scaled=TRUE)
  values <- predict(loopKS,ccData[,1:10])
  returnValue = sum(values == ccData[,11]) / nrow(ccData)
  return(returnValue)
}

loopKStest <-vector(mode="numeric", length = "10")
for(x in 1:10){
  loopKStest[x] = ksFunction(x)
}

print(loopKStest)
plot(loopKStest) # plot values based on 'k' index
title("rbfdot Accuracy by K Values")
# End rbfdot trial 3

