recurrence_time <- function(dist) {
  res <- list(x = dist$x, prob = 1 - cumsum(dist$prob))
  res$prob <- res$prob / sum(res$prob)
  return(res)
}