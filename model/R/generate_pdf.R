
generate_pdf <- function(params, xmax = 1e6, accuracy = 1) {

  require(extraDistr)

  if (params$dist == "binom") {
    n <- params$n
    mean <- params$mean
    p <- mean / n
    vec <- dbinom(0:xmax, n, p)
  }

  if (params$dist == "nbin") {
    cx <- params$c
    mean <- params$mean
    if (mean * cx * cx > 1) {
      r <- mean / ( mean*(cx^2) - 1)
      p <- 1 / (mean * (cx^2))
      vec <- dnbinom(0:xmax, r, p)
    } else {
      stop("Invalid combination of cx and mean.")
    }
  }

  if (params$dist == "det") {
    d = params$d
    vec <- c(rep(0, d), 1)
  }

  if (params$dist == "geom0") {
    p <- 1 / (params$mean+1)
    vec <- dgeom(0:xmax, p)
  }

  if (params$dist == "geom1") {
    p <- 1 / (params$mean)
    vec <- c(0, dgeom(0:xmax, p))
  }

  if (params$dist == "pois") {
    vec <- dpois(0:xmax, params$lambda)
  }

  if (params$dist == "unif") {
    vec <- dunif(0:params$b, params$a, params$b)
  }

  if (params$dist == "dnorm") {
    vec <- ddnorm(0:xmax, mean = params$mean, sd = params$sd)
  }

  # flog.info("pmass: %.2f", sum(vec))

  if (sum(vec) >= accuracy) {
    ret <- list(prob = vec / sum(vec), x = 0:(length(vec)-1))
    #indy <- which.max(cumsum(ret$prob) < 1)
    #ret$x <- ret$x[1:indy]
    #ret$prob <- ret$prob[1:indy]
    #ret <- reduce_dist(ret, accuracy)
    ret$params <- params
    return(ret)
  } else {
    return(generate_pdf(params, xmax*1.1, accuracy))
  }
}
