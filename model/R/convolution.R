convolve2 <- function(x, y, conj=FALSE, type=c("circular", "open", "filter")) {
  type <- match.arg(type)
  nx <- length(x)
  ny <- length(y)
  if (type == "circular") {
    fft_length <- max(nx, ny)
  } else {
    if (conj) {
      y <- rev(y)
      if (is.complex(y))
        y <- Conj(y)
      conj <- FALSE
    }
    nz <- nx + ny - 1
    fft_length <- 2^ceiling(log2(nz))
  }
  if (fft_length > nx)
    x[(nx+1):fft_length] <- as.integer(0)
  if (fft_length > ny)
    y[(ny+1):fft_length] <- as.integer(0)
  fy <- fft(y)
  if (conj)
    fy <- Conj(fy)
  z <- fft(fft(x) * fy, inverse=TRUE) / length(x)
  if (type == "open") {
    z <- z[1:nz]
  } else {
    if (type == "filter")
      z <- z[1:nx]
  }
  if (is.numeric(x) && is.numeric(y))
    z <- Re(z)
  if (is.integer(x) && is.integer(y))
    z <- as.integer(round(z))
  z
}

convolution <- function(a, b) {
  convolution2(a, b)
}

convolution2 <- function(a, b) {
  dists <- same_dimension(a, b)
  prob <- convolve2(dists$dist1$prob, dists$dist2$prob, type = "open")
  prob[prob < 1e-10] <- 0
  x <- (min(dists$dist1$x)+min(dists$dist2$x)):(max(dists$dist1$x)+max(dists$dist2$x))
  return(list(x = x, prob = prob))
}

"%c%" <- convolution

convolutionN <- function(dist, n, reduce = F, accuracy = 1) {
  if (n == 0) {
    return(list(x = 0, prob = 1))
  }
  ret <- dist
  nVec <- rep(1, n-1)
  for (i in nVec) {
    if (reduce) {
      ret <- reduce_dist(convolution2(ret, dist), accuracy)
    } else {
      ret <- convolution2(ret, dist)
    }
  }
  return(ret)
}

deconvolution <- function(a, b) {
  require(pracma)
  #dists <- same_dimension(reduce_dist_numerical(a), reduce_dist_numerical(b))
  prob <- deconv(a$prob, b$prob)$q
  prob[prob < 1e-10] <- 0

  max_a <- max(a$x)
  max_b <- max(b$x)
  x <- (min(a$x, b$x)):(max(max_a-max_b, max_b-max_a))
  return(list(x = x, prob = prob))
}
