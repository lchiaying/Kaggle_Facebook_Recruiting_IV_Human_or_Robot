#####
# Cross-validation and model selection using caret package
#####
library(caret)

#####
# Extract training bidders (29 bidders in original data are missing their bids data)
#####

train <- as.data.frame(merge(train_bidders, stats, by='bidder_id'))
test <- as.data.frame(stats[!(stats$bidder_id %in% train$bidder_id), ])

train$outcome <- paste0('x', as.character(train$outcome))  # level names are converted to column names when predicting with type='prob', so they need to be admissible R column names
train$outcome <- as.factor(train$outcome)


#####
# Specify the cross-validation procedure and grid of parameters to try
#####
fit_control <- trainControl(method='repeatedcv', number=10, # 10-fold cross validation
                            repeats=5, # repeated 5 times
                            classProbs = T,  # estimate class probabilities
                            summaryFunction = twoClassSummary)  # twoClassSummary computes ROC, sens, spec

param_grid <- expand.grid(mtry=3*(1:3)) #training method 'rf' (Random Forest)


#####
## Fit
#####
fit_model <- train(outcome ~ . , data = train[,-(1:3)], # model and data
                   method = "rf", # training method 
                   trControl = fit_control, # cross-validation procedure
                   tuneGrid = param_grid, # parameters of training method
                   metric = 'ROC') # evaluation metric (ROC is computed by the summary function twoClassSummary)

print(fit_model)

trellis.par.set(caretTheme())
plot(fit_model)


######
## Predict
#####
pred <- data.frame(bidder_id = as.character(test$bidder_id),
                   prediction = predict(fit_model, newdata=test, type='prob')$x1 )

