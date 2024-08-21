mirror_distribution <- function(dist) {
  dist$x <- -rev(dist$x)
  dist$prob <- rev(dist$prob)
  
  return(dist)
}