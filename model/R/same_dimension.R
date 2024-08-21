same_dimension <- function(a, b) {
  # Left side
  if (min(a$x) < min(b$x)) {
    b$prob <- c(rep(0, min(b$x) - min(a$x)), b$prob)
    b$x <- min(a$x):max(b$x)
  } else {
    a$prob <- c(rep(0, min(a$x) - min(b$x)), a$prob)
    a$x <- min(b$x):max(a$x)
  }
  
  # Right side
  if (max(a$x) > max(b$x)) {
    b$prob <- c(b$prob, rep(0, max(a$x) - max(b$x)))
    b$x <- min(b$x):max(a$x)
  } else {
    a$prob <- c(a$prob, rep(0, max(b$x) - max(a$x)))
    a$x <- min(a$x):max(b$x)
  }
  
  return(list(dist1 = a, dist2 = b))
}