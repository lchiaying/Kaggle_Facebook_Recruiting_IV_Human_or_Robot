

#####
# Preamble: Renormalize time by offset from first bid
#####
#Get first and last bid times and lifespan for each bidder
stats1 <- bids[, .(firstBidTime=min(time), lastBidTime=max(time)) , by=bidder_id]
stats1$bidLifespan <- stats1$lastBidTime - stats1$firstBidTime

#Get offset from bidder's first bid
bid_timeOffset <- merge(bids[,.(bid_id, bidder_id, time)], stats1[,.(bidder_id, firstBidTime)], 
                        by='bidder_id')
bid_timeOffset$timeFromFirstBid <- bid_timeOffset$time - bid_timeOffset$firstBidTime

setkey(bids, bid_id)
bids <- merge(bids, bid_timeOffset[, .(bid_id, timeFromFirstBid)], by='bid_id')

rm(stats1)

# saved as a column in bid_outcome.Rdata
bids <- merge(bids, bid_outcome[, .(bid_id, outcome, timeFromFirstBid)], by='bid_id')


#####
# Time series analysis
#####

## Obtain similarity of countries (trained on human bidders)
country_sim <- getSimilarityFromBids(bids[bids$outcome == 0, .(bidder_id, country)])

## Bag of tokens uses behaviour exhibited by robots
# Use 4 consequentive bids to form triplet of dissimilarities between steps
len_snippet <- 3
# Select bidders who have bid 4 or more times
slct <- stats$bidder_id[ (stats$n_bids > len_snippet) ] # &  (!is.na(stats$outcome)) ]
bids_slct <- bids[ (bids$bidder_id %in% slct), .(bidder_id, country, timeFromFirstBid)]
setkey(bids_slct, bidder_id)
# Convenient conversion to integer for indexing
bids_slct$country <- as.integer(bids_slct$country)
# Start with time series of from which to extract tokens
token_ts <- data.table( bidder_id = bids_slct$bidder_id[2:dim(bids_slct)[1]],
                        interbidTime = diff(bids_slct$timeFromFirstBid),
                        jumpDist = 1-country_sim[toMtxIdx(bids_slct$country, size=dim(country_sim)[1])]  )
# Discretize values of jumpDist and index it with token_key
token_key <- c(0, .05+(4:9)/10, 1)  # Manual selection of binning and token keys
token_ts$jumpDistIdx <- floor(token_ts$jumpDist*10)-3  # bin in 0, .4, .5, ... , .9, 1
token_ts$jumpDistIdx[token_ts$jumpDistIdx<0] <- 0

# Create bag of tokens
# manually set up auxillary data matrix with len_snippet columns
tkn <- cbind(token_ts$jumpDistIdx[1:(dim(token_ts)[1]-2)], token_ts$jumpDistIdx[2:(dim(token_ts)[1]-1)], token_ts$jumpDistIdx[3:dim(token_ts)[1]])
# Encode in base (length(token_key))
tkn <- tkn %*% length(token_key)^(c(0:(len_snippet-1)))
# Augment with bidder_id
token_ls <- data.table(bidder_id = token_ts$bidder_id[len_snippet:dim(token_ts)[1]],
                       jumpDistIdx = as.factor(tkn) )
setnames(token_ls, 'jumpDistIdx.V1', 'jumpDistIdx')  # data.table gives this column a weird name!
# Get indices that correspond to border b/w two bidders, not actual data
toRemove <- which(token_ts$interbidTime < 0)
blacklist <- c()
for (i in 0:(len_snippet-1)) {blacklist <- c(blacklist, toRemove-i)}
blacklist <- sort(blacklist)
# Remove non-data
token_ls <- token_ls[ !( 1:length(tkn) %in% blacklist), ]
# Clean up
rm(i, tkn, toRemove, blacklist); gc()

# One hot encoding
country_tokens <- sparseOneHotEncoding(token_ls$jumpDistIdx, id=token_ls$bidder_id)

# Dimension reduction
cntry <- dim_red(country_tokens$OH_Encoding)
cntry$bidder_id <- country_tokens$id
stats <- merge(stats, cntry, by='bidder_id')

#####
# CLEAN UP
#####
rm(country_sim,country_tokens, len_snippet, token_key,token_ls, token_ts)
gc()

