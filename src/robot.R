#####
#####
setwd("D:/Kaggle/fb_RobotBidding/Kaggle_Facebook_Recruiting_IV_Human_or_Robot/")

source('src/load_data.R')
source('src/util.R')


#####
# Obtain derived statistics for each bidder:
#   n_bids -- total number of bids placed
#   n_auctions -- number of auctions participated
#   mean/min/median/sd_interbid_time -- statistics of time between bids
#   mean_time_from_last_auction_bid -- mean time that bidder ups a previous bid within an auction
#   n_device, n_country, n_url, n_ip -- number of distinct categories bidded within these features
#   auction/country/.../url_jumps -- number of times the category of the feature changed between successive bids
#####
source('src/bid_stats.R')


#####
## Auction dependent statistics
#   mean_timeFromLastAuctionBid - Mean bid wait time from last bidder within the same auction
#   n_auctionsWon, n_mostActive, n_mostActiveNoWin
#####
source('src/auction.R')

#####
## Time series analysis of country from where a bidder bid
#  Extract snippets of 4 consecutive bids, quantify the extent of unusual jumping between 
#  countries using Jaccard similarity
#####
source('src/bidder_ts.R')


#####
## Cross-validate
#####
source('src/cross_validate.R')


#####
## Output to file
#####
source('src/submit.R')

