E <- function(dist) {
  ea <- 0
  for (i in 1:length(dist$x)) {
    ea <- ea + dist$x[i] * dist$prob[i]
  }
  return(ea)
}

SD <- function(dist) {
  dist_squared <- dist
  dist_squared$x <- dist_squared$x^2

  sdt <- sqrt(E(dist_squared) - E(dist)^2)
  return(sdt)
}
