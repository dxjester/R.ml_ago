
# Author: dxjester
# Comments:  Using the same data set (credit_card_data.txt or credit_card_data-headers.txt) as in Question 2.2, use the ksvm or kknn function to find a good classifier:

# -------------------------------------------------------------- #
# ---------------------  BEGIN PART A -------------------------- # 
# -------------------------------------------------------------- #


library(kknn)
set.seed(654)


ccData <- read.table("credit_card_data.txt", stringsAsFactors = FALSE, header = FALSE) # read in data set from working directory

i <- 30 # set vector and k max values

trainModel <- train.kknn(V11~.,ccData,kmax=i,scale=TRUE) # call train.kknn function to run the test

train.values <- vector(mode="numeric", length=i) # create a vector with length 30 to fill on assigned values for each data point

# determine calculate prediction qualities, aggregate findings via the 'for loop'
for (i in 1:i) {
  trainPredict <- as.integer(fitted(trainModel)[[i]][1:nrow(ccData)] + 0.5) 
  train.values[i] <- sum(trainPredict == ccData$V11)
}

max(train.values) #identify the max value in the vector

trainValues <- which(train.values == max(train.values)) # identify 'k' value which stores the 'max(values)' from the previous statement

trainValues # display 'k' values assessed for high accuracy - [12, 15, 16, 17]

train.values # display administrative information from models in values

# utilize the cv.kknn function, print out values
cv.values <- vector(mode="numeric", length=i) # create a vector to fill on assigned values for each data point

for (i in 1:i) {
  cvModel <- cv.kknn(V11~.,ccData,
                   kcv=10, # 10-fold cross-validation
                   k=i, # number of neighbors
                   scale=TRUE) # scale data
  cvPredict <- as.integer(cvModel[[1]][,2] + 0.5) 
  cv.values[i] <- sum(cvPredict == ccData$V11)
}

max(cv.values) #identify the max value in the vector

cvValues <- which(cv.values == max(cv.values)) # identify 'k' value which stores the 'max(values)' from the previous statement 560

cvValues # display 'k' values assessed for high accuracy - [9, 19]

cv.values # display administrative information from models in values
  
# -------------------------------------------------------------- #
# ---------------------  BEGIN PART B -------------------------- # 
# -------------------------------------------------------------- #
  
totalRow = nrow(ccData) #Number of rows in cc data

bValues <- vector(mode="numeric", length=20) 

trainingSize = sample(totalRow, size = floor(totalRow* 0.7))
trainArray = ccData[trainingSize,] # training data set

residual = ccData[-trainingSize, ]  # all rows except training

residualSize = sample(nrow(residual), size = floor(nrow(residual)/2)) # split the remaining 30 rows to sets of 15 for validation and test

validateArray = residual[residualSize,]  # validation data set
testArray = residual[-residualSize, ] # test data set

for (i in 1:20) {
  knTest <- kknn(V11~.,trainArray,validateArray,k=i,scale=TRUE)
  predictor <- as.integer(fitted(knTest)+0.5) # round off to 0 or 1
  bValues[i] = sum(predictor == validateArray$V11) / nrow(validateArray)
}

bValues[1:20] #display aggregated values based on 'k' iterator
max(bValues) # display highest value for k

bFinal <- which(bValues == max(bValues)) # load vector locations where max(bValues) resides
bFinal # print k value, k=5
