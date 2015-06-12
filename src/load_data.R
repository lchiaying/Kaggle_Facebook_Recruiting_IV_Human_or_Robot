library(caret) # createDataPartition, trainControl, train
library(e1071)  #svm
library(data.table)  # fread, data.table
require(bit64)  # use integer64 representation for time encoding

# Training set
train_bidders <- fread('data//raw//train.csv')
#train_bidders[,c("payment_account", 'address')] <- NULL
setkey(train_bidders, bidder_id)

# Bid information
bids <- fread('data//raw//bids.csv')

bids$merchandise <- as.factor(bids$merchandise)
bids$country <- as.factor(bids$country)
bids$device <- as.factor(bids$device)

setkey(bids, bidder_id)

# Test set
test_bidders <- fread('data//raw//test.csv')
setkey(test_bidders, bidder_id)
