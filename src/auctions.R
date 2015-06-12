#####
# (1) Derived auction features
#       time_from_last_auction_bid
# (2) Derived auction statistics
#       n_bids
#       n_bidders
#       bidder_last
#       mean_interbid_time
#####

# auction statistics
setkey(bids, auction)

auction_stats <- bids[, .(n_bids = length(bidder_id),
                          n_bidders = length(unique(bidder_id)),
                          bidder_last = getLastBidder(bidder_id, time),
                          bidder_most = names(which.max(table(bidder_id))),
                          mean_interbid_time = mean((getInterbidTime(time))[[1]], na.rm=T)
                          ),
                      by = auction]
auction_stats$mostActiveBidderWins <- auction_stats$bidder_last == auction_stats$bidder_most

save(auction_stats, file='data/auction_stats.Rdata')


# Mean bid wait time from last bidder within the same auction
ibt <- bids[, getInterbidTime(time, bid_id), by=auction]
setnames(ibt, c('bid_time', 'id'), c('time_from_last_auction_bid', 'bid_id'))
setkey(ibt, bid_id)
ibt[, auction := NULL]
bids <- merge(bids, ibt, by = 'bid_id')

setkey(bids, bidder_id)
stats1 <- bids[, .(mean_timeFromLastAuctionBid = mean(time_from_last_auction_bid, na.rm=T)),
               by = bidder_id]
stats <- merge(stats, stats1, by='bidder_id', all.x=T)
idx <- is.na(stats$mean_timeFromLastAuctionBid)
stats$mean_timeFromLastAuctionBid[idx] <- mean(stats$mean_timeFromLastAuctionBid, na.rm=T)

rm(stats1, idx, ibt)


## Number of auctions won by each bidder
setkey(auction_stats, bidder_last)
won <- auction_stats[, .(n_auctionsWon = length(n_bids)), by=bidder_last]
setnames(won, 'bidder_last', 'bidder_id')
stats <- merge(stats, won, by='bidder_id', all.x=T)
stats$n_auctionsWon[is.na(stats$n_auctionsWon)] <- 0
rm(won)


## Number of auctions where bidder is most active, and most active but didn't win
setkey(auction_stats, bidder_most)
most <- auction_stats[, .(n_mostActive = .N, 
                          n_mostActiveNoWin = sum(!mostActiveBidderWins)), 
                      by=bidder_most]
setnames(most, 'bidder_most', 'bidder_id')
stats <- merge(stats, most, by='bidder_id', all.x=T)
stats$n_mostActive[is.na(stats$n_mostActive)] <- 0
stats$n_mostActiveNoWin[is.na(stats$n_mostActiveNoWin)] <- 0
rm(most)



## For each auction, compute proportion of bids by each bidder. 
# Then for each bidder, compute average proportion of auction bids
setkey(bids, auction, bidder_id)
nbids <- bids[, .N, by = .(auction, bidder_id)]
nbids <- merge(nbids, nbids[, sum(N), by = auction], by = 'auction', all.x=T)
nbids$N <- nbids$N / nbids$V1

