
library(Matrix)

#####
# Construct similarity matrix
#####
getSimilarityFromBids <- function(bids, type='jaccard') {
  # Input: bids -- two columns bidder_id and the feature against which to compute similarity
  #                 the feature column must be a factor, and in applications ideally comprising ALL desired levels, not just those appearing in bids
  #                 the bidder_id will be converted to factor (locally)
  
  setnames(bids, names(bids)[which(names(bids)!='bidder_id')], 'feature')
  bids$bidder_id <- as.factor(bids$bidder_id)
  # Form matrix of bidders vs countries, humans only
  mtx <- sparseMatrix(i = as.integer(bids$bidder_id), j = as.integer(bids$feature), 
                      x = rep(1, dim(bids)[1]),
                      dims = c(length(unique(bids$bidder_id)), length(levels(bids$feature))) )
  # Similarity matrix
  return( similarity(mtx = mtx>0, type = type) )
}


similarity <- function(mtx, type='jaccard') {
  # mtx -- binary matrix (either T/F or 0/1)
  if (type == 'jaccard') {
    sim <- crossprod(mtx) / (dim(mtx)[1] - crossprod(1-mtx))
    sim[is.na(sim)] <- 1
    return(sim)
  }
}


#####
# Jumps between country, auction, etc
#####
jumpScore <- function(times, category, sim_mtx=NULL) {
  if (length(times)==1) {return(0)}
  # category -- must be (coercible to) a factor corresponding to the indexing of sim_mtx
  category <- as.integer(category[order(times)])
  if (is.null(sim_mtx)) {
    return( sum(diff(as.integer(category))!=0) / length(unique(category)) )   
  } else {
    idx <- toMtxIdx(category, size=dim(sim_mtx)[1])
    return( sum(1-sim_mtx[idx]) / length(unique(category)) )
  }
}


toMtxIdx <- function(idx1, idx2=NULL, size) {
  # Inputs: idx1, idx2 -- [i,j]; if is.null(idx2) then assume it's offset of idx1
  #         size -- nrow in target matrix
  idx1 <- idx1 - 1
  if (is.null(idx2)) {idx2 <- idx1[2:length(idx1)]; idx1 <- idx1[1:(length(idx1)-1)]}
  return(idx2*size + idx1 + 1)
}



#####
# Feature extraction/formation/reduction
#####
OneHotEncoding <- function(factors, lvls = NULL, colnamehead, bidder_id = NULL) {
  # Returns a matrix with number of columns equal to number of levels of factors, counting
  # the number of occurrances of each level for each bidder_id
  # Input:  factors -- a list of class factor
  #         lvls -- levels of factors (impt if factors is a subset of a larger collection of levels)
  #         colnamehead -- header for feature naming
  #         bidder_id -- provide this if factors correspond to different bidders
  if (is.null(lvls)) { lvls <- levels(factors) }
  contr <- as.data.table(matrix( contr.treatment(1+length(lvls))[1+as.integer(factors),] , nrow=length(factors) ))
  setnames(contr, names(contr),
           paste0(colnamehead, '_', sapply(lvls, function(x) strsplit(x, split=' ')[[1]][1])))
  if (is.null(bidder_id)) {
    return( lapply(contr, sum) )
  } else {
    contr$bidder_id <- bidder_id
    setkey(contr, bidder_id)
    return( contr[, lapply(.SD, sum) , by=bidder_id] )
  }
}

sparseOneHotEncoding <- function(factors, id) {
  # One hot encoding (OHE) utilizing sparse matrix representations
  # Inputs: factors -- list of class factor
  #         id -- identifer corresponding to each value in factors
  # Output: OH_Encoding -- counts of occurances of each factor level for each unique entry in id
  #         id -- id labels for the rows of OH_Encoding
  # Can be fed into dimesion reduction function before converting to full matrix
  
  contr <- Matrix(contr.treatment( 1 + length(levels(factors)) ))[ 1 + as.integer(factors), ]
  id <- as.factor(id)
  mask <- sparseMatrix(i = as.integer(id), j = 1:length(id), x = rep(1,length(id)))  # A mask matrix to sum rows of contr  
  return(list(id = levels(id), OH_Encoding =  mask %*% contr))
}


dim_red <- function(X, rnk=NULL) {
  # Dimension reduction using SVD. The rank, rnk, can be specified initially or prompted for
  X <- Matrix(X)
  mu <- colMeans(X)
  XtX <- crossprod(X)
  Xsvd <- svd(XtX)
  if (is.null(rnk)) {
    plot(1:length(Xsvd$d), Xsvd$d)
    rnk <- readline(prompt="Choose the rank: ")
    rnk <- as.integer(rnk)
  }
  return( data.table(as.matrix(X %*% Xsvd$v[, 1:rnk])) )
}


#####
# Statistics
#####
getInterbidTime <- function(bid_time, id = NULL) {
  ord <- order(bid_time)
  out <- c(NA, as.numeric(diff(bid_time[ord])))
  return(list( bid_time = out, id = id))
}


getLastBidder <- function(bidders, bid_times) {
  bidders <- bidders[order(bid_times)]
  return(bidders[length(bidders)])
}

