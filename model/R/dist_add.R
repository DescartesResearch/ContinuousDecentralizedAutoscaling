dist_add <- function(dist1, dist2, weight = 1) {
  
  dist2$prob <- dist2$prob * weight
  
  if (length(dist1$x) <= length(dist2$x)) {
    ret <- dist2
    ret$prob[1:length(dist1$prob)] <- ret$prob[1:length(dist1$prob)] + dist1$prob
  } else {
    ret <- dist_add(dist2, dist1)
  }
  return(ret)
}
