# Author: dxjester
# Comments:  R script which utilizes the kknn function to calculate classification line for credit card data #

# Load library
library(kknn)

ccData <- read.table("credit_card_data.txt", stringsAsFactors = FALSE, header = FALSE)
set.seed(654)

# Trial number: 1
# Kernel: kknn
# K value: 10, static
# Accuracy: Fitted value of 77.75%, verified at C[236] of '1'
# Comments: Manual test trial to determine code executability

i<-123 # set data point 
manualK1 = kknn(V11~., ccData[-i,], ccData[i,], k=10, distance=2, kernel = "optimal", scale = TRUE)
manualK1
fitted.values(manualK1)
ccData[i,11]

predicted <- as.integer(fitted(manualK1)+ 0.5)
predicted
# End trial 1

# Start trial number: 2
# Kernel: kknn
# K value: 5, static
# Test parameters: for loop, with static 'k' variable of '5'
# Comments: Conduct automated for loop trial analysis w/ K value of '5'
# Accuracy: 85.17%

# begin initiailization of model through for loop

values <- vector(mode="numeric", length="654")

for (i in 1:654) {
  # call 'kknn' function and temporarily store value into loopK2 variable
  loopK1 = kknn(V11~.,ccData[-i,],ccData[i,],k=5,distance=2,kernel = "optimal",scale = TRUE)
  
  # call fitted function and round value
  values[i]<-as.integer(fitted(loopK1) + 0.5)
}

sum(values == ccData[,11]) / nrow(ccData)
# End trial 2

# Start trial number: 3
# Kernel: kknn
# K value: x, dynamic
# Test parameters: for loop, with static 'k' variable of '10'
# Comments: Conduct automated for loop trial analysis w/ built in function to pass parameters
# Accuracy: 85.01%

kkFunction <- function(x){
  values <- vector(mode="numeric", length="654") # create a vector to fill on assigned values for each data point

  for (i in 1:654) {
    # call 'kknn' function and temporarily store value into loopK2 variable
    loopK2 = kknn(V11~.,ccData[-i,],ccData[i,],k=x,distance=2,kernel = "optimal",scale = TRUE) 
    
    # call fitted function and round value
    values[i]<-as.integer(fitted(loopK2) + 0.5)
  } # end for loop
  
  returnValue = sum(values == ccData[,11]) / nrow(ccData)
  return(returnValue)
}

loopK2test <-vector(mode="numeric", length = "20")
for(x in 1:20){
  loopK2test[x] = kkFunction(x)
}

print(loopK2test)
plot(loopK2test) # plot values based on 'k' index
title("KKNN Accuracy by K Values")
# End trial 3

summary <- c(0.8149847, 0.8149847, 0.8149847, 0.8149847, 0.8516820, 0.8455657, 0.8470948, 0.8486239, 0.8470948, 0.8501529, 0.8516820, 0.8532110,
            0.8516820, 0.8516820, 0.8532110, 0.8516820, 0.8516820, 0.8516820, 0.8501529, 0.8501529)
fivenum(summary)
sd(summary)
var(summary)

# Start trial number: 4
# Kernel: kknn
# K value: 12, static
# Test parameters: for loop, with static 'k' variable of '12'
# Comments: Conduct automated for loop trial analysis w/ K value of '12'
# Accuracy: 85.32%

# begin initiailization of model through for loop
i<-123 # set data point 
values <- vector(mode="numeric", length="654")
rawValues <- vector(mode="numeric", length="654")

for (i in 1:654) {
  # call 'kknn' function and temporarily store value into loopK2 variable
  loopK1 = kknn(V11~.,ccData[-i,],ccData[i,],k=12,distance=2,kernel = "optimal",scale = TRUE)
  
  # call fitted function and round value
  rawValues[i] <- fitted.values(loopK1)
  values[i]<-as.integer(fitted(loopK1) + 0.5)
}

sum(values == ccData[,11]) / nrow(ccData)
plot(rawValues)
# End trial 4

