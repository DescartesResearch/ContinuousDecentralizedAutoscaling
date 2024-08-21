reduce_dist <- function(dist, accuracy = 1) {
  
  #indy <- which(cumsum(dist$prob) >= accuracy)[1]
  #indy <- cumsum(dist$prob) %>% detect_index(function(x) x >= accuracy)
  t <- cumsum(dist$prob)
  #indy <- length(t)+1 - length(t[t >= accuracy])
  
  indy <- min(which(t >= accuracy), length(t))
  
  if (length(indy) > 0) {
    ret <- dist$prob[1:indy]
  } else {
    ret <- dist$prob
  }
  
  ret <- list(prob = ret / sum(ret), x = (0:(indy-1)) + min(dist$x))
  
  return(ret)
  
}

reduce_dist_numerical <- function(dist, accuracy = 1) {
  dist$prob[dist$prob < 1e-10] <- 0

  dist <- remove_leading_zeros(dist)
  
  return(dist)
}

remove_leading_zeros <- function(dist) {
  nElements <- length(dist$x)
  
  which_result <- which(dist$prob > 0 & dist$x < 0)
  
  if (length(which_result) > 0) {
    min_indy <- min(min(which_result), which(dist$x == 0))
  } else {
    min_indy <- which(dist$x == 0)
  }
  
  ret <- list(x = dist$x[(min_indy):nElements], prob = dist$prob[(min_indy):nElements])
  
  return(ret)
}
