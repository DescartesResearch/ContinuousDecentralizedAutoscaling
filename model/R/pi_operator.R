pi_operator <- function(dist1, m = 0, side = "left") {
  if (side == "left") {
    temp <- sum(dist1$prob[dist1$x <= m])
    dist1$x <- m:max(dist1$x)
    dist1$prob <- dist1$prob[(length(dist1$prob)-length(dist1$x)+1):length(dist1$prob)]
  } else if (side == "right") {
    temp <- sum(dist1$prob[dist1$x >= m])
    dist1$x <- min(dist1$x):m
    dist1$prob <- dist1$prob[1:length(dist1$x)]
  }
  dist1$prob[m+1] <- temp
  return(dist1)
}
