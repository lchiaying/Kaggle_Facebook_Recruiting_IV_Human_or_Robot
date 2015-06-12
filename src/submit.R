######
#Save model and write out data

fname <- 'rf_9'


## Write out prediction to a submittable csv file
writeToCSV <- function(pred, file, predColName = 'prediction') {
  submit <- read.csv('data/submit//sampleSubmission.csv')
  names(submit)[2] <- 'asdf'
  submit$bidder_id <- as.character(submit$bidder_id)
  
  submit <- merge(submit, pred, by='bidder_id', all.x=T, sort=F)
  submit <- submit[,c(which(names(submit) == 'bidder_id'), which(names(submit) == predColName))]
  names(submit) <- c('bidder_id', 'prediction')
  
  submit$prediction[is.na(submit$prediction)] <- 0
  
  write.csv(submit, file=file, row.names=F)
}


# Write to file
writeToCSV(pred, file=paste('data//submit//', fname,'.csv', sep=''))
save(fit_model, file=paste('data//submit//', fname, '.model.Rdata', sep=''))
