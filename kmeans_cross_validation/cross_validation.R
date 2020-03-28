
# Author: dxjester
# Comments:  Using the same data set (credit_card_data.txt or credit_card_data-headers.txt) as in Question 2.2, use the ksvm or kknn function to find a good classifier:
# (a) using cross-validation (do this for the k-nearest-neighbors model; SVM is optional); and
# (b) splitting the data into training, validation, and test data sets (pick either KNN or SVM; the other is optional).

# Load library
library(kknn)

ccData <- read.table("credit_card_data.txt", stringsAsFactors = FALSE, header = FALSE)

set.seed(654)

#Number of rows in cc data
d.rows = nrow(ccData) 

#Randomly selecting (1/3)rd indexes among 654 indexes
d.sample = sample(1:d.rows, size = round(d.rows/3), replace = FALSE)
d.sample
#Training data selected by excluding the 1/3rd sample
d.train = ccData[-d.sample,]
#Test data seleted by including the 2/3rd sample
d.test = ccData[d.sample,]  

#Training of kknn method via leave-one-out (train.kknn) crossvalidation, we want to find the optimal value of 'k'
xval=train.kknn(V11 ~ ., data = d.train, kmax = 100, kernel = c("optimal","rectangular", "inv", "gaussian", "triangular"), scale = TRUE) 
xval