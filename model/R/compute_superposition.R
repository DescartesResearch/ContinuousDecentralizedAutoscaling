compute_superposition <- function(dist1.pdf, dist2.pdf) {
  
  ea <- 1/(1/E(dist1.pdf) + 1/E(dist2.pdf))
  
  r.pdf <- normalize(compute_minimum(recurrence_time(dist1.pdf), recurrence_time(dist2.pdf)))
  #sup.cdf <- list(prob = 1 - r.pdf$prob * ea, x = r.pdf$x)
  sup.cdf <- list(prob = 1 - r.pdf$prob * ea, x = r.pdf$x)
  sup.pdf <- list(prob = c(sup.cdf$prob[1], diff(sup.cdf$prob)), x = sup.cdf$x)
  
  return(sup.pdf)
}