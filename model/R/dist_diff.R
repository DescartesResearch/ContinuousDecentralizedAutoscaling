dist_diff <- function(dist1, dist2) {

  dists <- same_dimension(dist1, dist2)

  dist1 <- dists$dist1
  dist2 <- dists$dist2

  ret <- sum(abs(dist1$prob - dist2$prob))

  return(ret)
}
