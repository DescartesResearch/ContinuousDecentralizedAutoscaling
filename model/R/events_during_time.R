events_during_time <- function(tau, a, accuracy, start_with_ra = T) {
  
  # ##EVENTSDURINGTIME Calculate the number of events that occur during intervals that are distributed according to tau.
  # # tau:                  Distribution of the interval length.
  # # a:                    Distribution of interarrival times.
  # # accuracy:             Break condition.
  
  # Recurrence time of a.
  reca <- recurrence_time(a)
  
  # Special case of zero events.
  r <- list(x = 0, prob = 0)
  
  if (start_with_ra) {
    convN <- reca
    
    # Zero events occur if the recurrence time (i.e., the time until the first event) is at least as long as the observation interval (or longer).
    tauShort <- convolution2(tau, mirror_distribution(reca))
    r$prob[1] <- sum(tauShort$prob[tauShort$x <= 0])
    
  } else {
    convN <- generate_pdf(list(dist = "det", d = 0)) # This may be wrong!
    
    # Zero events occur if the interarrival time (i.e., the time until the first event) is at least as long as the observation interval (or longer).
    tauShort <- convolution2(tau, mirror_distribution(a))
    r$prob[1] <- 0
    #r$prob[1] <- sum(tauShort$prob[tauShort$x <= 0])
  }
  
  j <- 1
  
  oldProb <- 2
  
  while (sum(r$prob) < accuracy) {
    
    #flog.info(paste0(j, ". iteration: ", sum(r$prob)))
    
    r$x[j + 1] <- j
    r$prob[j + 1] <- 0
    
    # Cf. formula from technical report.
    f_j <- convN #convolution2(reca, convolutionN(a, j - 1))
    convN <- reduce_dist(convolution2(convN, a), accuracy)
    f_j1 <- convN #convolution2(reca, convolutionN(a, j))
    
    dists <- same_dimension(f_j, f_j1)
    f_j <- dists$dist1
    f_j1 <- dists$dist2
    
    dists <- same_dimension(f_j, tau)
    f_j <- dists$dist1
    tau <- dists$dist2
    
    dists <- same_dimension(f_j1, tau);
    f_j1 <- dists$dist1
    tau <- dists$dist2
    
    term2 <- 0
    the_sum <- 0
    for (m in 2:length(tau$x)) {
      # This if statement significantly speeds up the process (up to a factor of ~50 depending on parameter combination)
      if (m == 2) {
        the_sum <- sum(f_j$prob[1:(m-1)] - f_j1$prob[1:(m-1)]) 
      } else {
        the_sum <- the_sum + f_j$prob[m-1] - f_j1$prob[m-1]
      }
      term2 <- term2 + tau$prob[m] * the_sum
    }
    r$prob[j + 1] <- r$prob[j + 1] + term2
    j <- j + 1
    
    # if (abs(oldProb-sum(r$prob)) < (1-accuracy)) {
    #   warning("events_during_time: Not enough probability mass accumulated. Stopping. Be careful!")
    #   break
    # }
    
    oldProb <- sum(r$prob)
  }
  
  #r$prob[r$prob < 0] <- 0
  r <- normalize(r)
  
  if (start_with_ra == F) {
    r$prob <- c(r$prob[2:length(r$prob)], 0)
  }
  
  return(r)
  
}
