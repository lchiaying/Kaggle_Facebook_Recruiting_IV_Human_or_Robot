#####
# Obtain derived statistics for each bidder:
#   n_bids -- total number of bids placed
#   n_auctions -- number of auctions participated
#   mean/min/median/sd_interbid_time -- statistics of time between bids
#   mean_time_from_last_auction_bid -- mean time that bidder ups a previous bid within an auction
#   n_device, n_country, n_url, n_ip -- number of distinct categories bidded within these features
#   auction/country/.../url_jumps -- number of times the category of the feature changed between successive bids
#####
stats <- bids[, .(auction_jumps = jumpScore(time, auction),
                  country_jumps = jumpScore(time, country),
                  device_jumps = jumpScore(time, device),
                  ip_jumps = jumpScore(time, ip),
                  url_jumps = jumpScore(time, url),
                  n_bids = length(auction), 
                  n_auctions = length(unique(auction)),
                  mean_interbid_time = mean(getInterbidTime(time)[[1]], na.rm=T),
                  median_interbid_time = median(getInterbidTime(time)[[1]], na.rm=T),
                  min_interbid_time = min(getInterbidTime(time)[[1]], na.rm=T),
                  sd_interbid_time =  sd(getInterbidTime(time)[[1]], na.rm=T),
                  n_device = length(unique(device)),
                  n_country = length(unique(country)),
                  n_url = length(unique(url)),
                  n_ip = length(unique(ip))
),
by=bidder_id]

# Replace the NA values with imputed means
idx <- is.na(stats[,median_interbid_time])  
stats[idx, 4] <- mean(stats$mean_interbid_time[!idx])
stats[idx, 5] <- mean(stats$median_interbid_time[!idx])
stats[idx, 6] <- mean(stats$min_interbid_time[!idx])
rm(idx)

# Add feature n_bidsPerAuction
stats$n_bidsPerAuction <- stats$n_bids/stats$n_auctions


stats <- as.data.frame(merge(stats, onehot_merch, by = 'bidder_id'))
idx <- which(names(stats) == names(onehot_merch)[2:dim(onehot_merch)[2]])
stats[, idx] <- stats[, idx] / stats$n_bids
rm(idx)

stats <- as.data.table(stats)


#####
# Include distribution of bids in the different categories under merchanise, country, devices
# - First, one-hot encoding of the feature, then an SVD to reduce the dimension
#####

## Onehot encoding on merchandise
onehot_merch <- OneHotEncoding(bids$merchandise, colnamehead='merch', bidder_id = bids$bidder_id)


## Onehot encoding on country; variation for memory constraints (number of levels too large)
lvls <- levels(bids$country)
onehot_country <- bids[, OneHotEncoding(as.integer(country), lvls=lvls, colnamehead='country'), by=bidder_id]
rm(lvls);gc()


## Onehot encoding on popular devices
# Choose the most popular devices, lump other devices together
tbl <- table(bids$device, bids$outcome)
for (i in 1:2) {tbl[,i]<-tbl[,i]/colSums[i]}
popular_devices <- row.names(tbl)[which(tbl[,1]>.005 | tbl[,2]>.005)]
# Only popular devices, the rest lumped under 'phoneOther', recast into factor
bids$device_popular <- as.character(bids$device)
bids$device_popular[!(bids$device_popular %in% popular_devices)] <- 'phoneOther'
bids$device_popular <- as.factor(bids$device_popular)
# One-hot encoding
onehot_device <- bids[, OneHotEncoding(as.integer(device_popular), 
                                       lvls=levels(bids$device_popular), colnamehead=''), 
                      by=bidder_id]
rm(tbl,popular_devices);gc()

## Dimension reduction via PCA
merch_pca <- dim_red(as.matrix(data.frame(onehot_merch)[,-1]))
setnames(merch_pca, names(merch_pca), paste0('merch', 1:dim(merch_pca)[2]))
merch_pca$bidder_id <- stats$bidder_id
stats <- merge(stats, merch_pca, by='bidder_id')

country_pca <- dim_red(onehot_country, rnk=5)
stats <- merge(stats, country_pca, by='bidder_id')

device_pca <- dim_red(as.matrix(as.data.frame(onehot_device)[,-1]))
setnames(device_pca, names(device_pca), paste0('device', 1:dim(device_pca)[2]))
device_pca$bidder_id <- onehot_device$bidder_id
stats <- merge(stats, device_pca, by='bidder_id')

rm(merch_pca, country_pca, device_pca)


## Cluster bidding time period into era
era <- kmeans(as.numeric(bids$time), centers = 3) 
onehot_era <- OneHotEncoding(as.factor(era$cluster), colnamehead='era', bidder_id = bids$bidder_id)
stats <- merge(stats, onehot_era, by='bidder_id')
stats[, c('era_1', 'era_2', 'era_3') := list(era_1/n_bids, era_2/n_bids, era_3/n_bids)]
