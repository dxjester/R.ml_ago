
# Author: dxjester
# Comments:  Using the support vector machine function ksvm contained in the R package kernlab, find a good classifier for this data. Show the equation of your classifier, and how well it classifies the data points in the full data set. (Don’t worry about test/validation data yet; we’ll cover that topic soon.)


# Load library
library(kernlab)

# read in text file from working directory
ccData <- read.table("credit_card_data.txt", stringsAsFactors = FALSE, header = FALSE)

# Start trial number: 1
# C value: 100
# Accuracy: 86.39%
# Comments: Initial (proposed) C value provided in the homework assignment, single execution, manual
van1 <- ksvm(V11~., data = ccData, type="C-svc", kernel="vanilladot", C=100,scaled=TRUE)

a <- colSums(van1@xmatrix[[1]] * van1@coef[[1]])
a
a0 <- -van1@b
a0

pred <- predict(van1,ccData[,1:10])
pred

sum(pred == ccData[,11]) / nrow(ccData)
# End ksvm trial 1


# Start trial number: 2
# C value: 100000
# Accuracy: 86.39%
# Comments: Single execution, manual
van2 <- ksvm(V11~., data = ccData, type="C-svc", kernel="vanilladot", C=100000,scaled=TRUE)

a <- colSums(van2@xmatrix[[1]] * van2@coef[[1]])
a
a0 <- -van2@b
a0

pred <- predict(van2,ccData[,1:10])
pred

sum(pred == ccData[,11]) / nrow(ccData)

van2
# attributes(van1)
# End ksvm trial 2


# Start trial number: 3
# C value: multiple, dynamic
# Accuracy: various
# Comments: Utilize for loop and function call to store accuracy values for follow on plot
ksFunction <- function(x){
    values <- vector(mode="numeric", length="654") # create a vector to fill on assigned values for each data point

    loopKS <- ksvm(V11~., data = ccData, type="C-svc", kernel="vanilladot", C=100^x,scaled=TRUE)
    values <- predict(loopKS,ccData[,1:10])
    returnValue = sum(values == ccData[,11]) / nrow(ccData)
    return(returnValue)
}

loopKStest <-vector(mode="numeric", length = "10")
for(x in 1:10){
  loopKStest[x] = ksFunction(x)
}

print(loopKStest) # print accuracy values
plot(loopKStest) # plot values based on 'k' index
title("KSVM Accuracy by K Values")

# End 
