compute_minimum <- function(dist1, dist2) {
  
  dists <- same_dimension(dist1, dist2)
  
  pdf1 <- dists$dist1
  pdf2 <- dists$dist2
  cdf1 <- compute_cdf(pdf1)
  cdf2 <- compute_cdf(pdf2)
  
  #minDist <- list(prob = (pdf1$prob * (1-cdf2$prob)) + (pdf2$prob * (1-cdf1$prob)), x = pdf1$x)
  prob <- cdf1$prob + cdf2$prob - cdf1$prob * cdf2$prob
  prob <- c(prob[1], diff(prob))
  minDist <- list(x = pdf1$x, prob = prob)
  
  return(minDist)
}
