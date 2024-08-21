normalize <- function(dist) {
  dist$prob <- dist$prob / sum(dist$prob)
  return(dist)
}