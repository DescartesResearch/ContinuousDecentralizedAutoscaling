u1 <- function(current_value) {
  threshold <- 0.9
  if (current_value < threshold) {
    return(0)
  } else {
    return(1)
  }
}
d1 <- function(current_value) {
  threshold <- 0.3
  if (current_value < threshold) {
    return(1)
  } else {
    return(0)
  }
}

u2 <- function(current_value) {
  lower_threshold <- 0.8
  cap <- 1
  upper_threshold <- 0.95
  k <- 1 / (lower_threshold - upper_threshold) * log(1.0 / (1 + cap))
  A <- (1 + cap) * exp(-1 * k * upper_threshold)
  if (current_value < lower_threshold) {
    return(0)
  } else {
    return(min(c(cap, A * exp(k * current_value) - 1)))
  }
}
d2 <- function(current_value) {
  lower_threshold <- 0
  upper_threshold <- 0.6
  cap <- 1
  k <- 1 / (lower_threshold - upper_threshold) * log(1 + cap)
  A <- exp(-1 * k * upper_threshold)
  if (current_value > upper_threshold) {
    return(0)
  } else {
    return(min(c(cap, A * exp(k * current_value) - 1)))
  }
}

scaling_function1 <- \(rho) {
  p_up <- u1(rho)
  p_down <- d1(rho)
  p_hold <- 1 - p_up - p_down
  return(list(x = c(-1, 0, 1), prob = c(p_down, p_hold, p_up)))
}

scaling_function2 <- \(rho) {
  p_up <- u2(rho)
  p_down <- d2(rho)
  p_hold <- 1 - p_up - p_down
  return(list(x = c(-1, 0, 1), prob = c(p_down, p_hold, p_up)))
}
