library(data.table)
library(tidyverse)
library(futile.logger)
library(plotly)

rm(list = ls())

files.sources = list.files("./R", full.names = T)
lapply(files.sources, source, echo = F)

source("./autoscaling_model/jump_up/scaling_function.R")
scaling_function <- scaling_function1 # config 1
#scaling_function <- scaling_function2 # config 2

# Initial system state
#### HERE WE USE THE K FROM THE HOLD MODEL RUN
K <- readRDS("./autoscaling_model/K_hold_config1.Rds")
#K <- generate_pdf(params = list(dist = "det", d = 8)) # 8 instances at start
U <- generate_pdf(params = list(dist = "det", d = 100)) # arbitrary system state at start
W <- generate_pdf(params = list(dist = "unif", a = 30, b = 90), xmax = 100) # config 1
#W <- generate_pdf(params = list(dist = "unif", a = 30, b = 60), xmax = 100) # config 2
k_max <- 500

lambda <- 80
mu <- 6.25

difference <- Inf
accuracy <- 0.999999

# Rho history
rho_history <-  data.frame(it = numeric(), k = numeric(), rho = numeric(), kprob = numeric())

# W_Cache[[ii]] = dist of ii superimposed Ws

W_Cache <- vector(mode = "list", length = k_max)
W_Cache[[1]] <- W

tmpu_cache <- vector(mode = "list", length = k_max)
tmpu_cache[[1]] <- (events_during_time(W_Cache[[1]], reduce_dist(generate_pdf(params = list(dist = "geom0", mean = 1/lambda))), accuracy) %c%
                      ((events_during_time(W_Cache[[1]], reduce_dist(generate_pdf(params = list(dist = "geom0", mean = 1/mu))), accuracy)) %>%
                         mirror_distribution())) %>% reduce_dist(accuracy = accuracy)

for (ii in 2:k_max) {
  flog.info(paste0("Caching iteration: ", ii))
  W_Cache[[ii]] <- compute_superposition(W_Cache[[ii - 1]], W)
  tmpu_cache[[ii]] <- (events_during_time(W_Cache[[ii]], reduce_dist(generate_pdf(params = list(dist = "geom0", mean = 1/lambda))), accuracy) %c%
                         ((events_during_time(W_Cache[[ii]], reduce_dist(generate_pdf(params = list(dist = "geom0", mean = 1/mu))), accuracy) %>% convolutionN(ii, reduce = T, accuracy = accuracy)) %>%
                            mirror_distribution())) %>% reduce_dist(accuracy = accuracy)
}


df_ek <- data.frame(it = numeric(), ek = numeric(), stdk = numeric())
df_eu <- data.frame(it = numeric(), eu = numeric(), stdu = numeric())
df_ew <- data.frame(it = numeric(), ew = numeric(), stdw = numeric(), ewt = numeric(), stdwt = numeric())


i <- 0
diff_counter <- 0
Wn_total <- list(x = 0, prob = 1)

while(diff_counter <= 100) {

  flog.info(paste0("Iteration: ", i))
  flog.info(paste0("E(K): ", E(K)))

  i <- i+1

  # if (i > 100) {
  #   break
  # }

  K_new <- list(x = 0, prob = 0)
  U_new <- list(x = 0, prob = 0)
  Wn_local <- list(x = 0, prob = 0)

  for (k in K$x) {

    kprob <- K$prob[K$x == k]

    if (kprob == 0 || k == 0) {
      next
    }

    Wn <- W_Cache[[k]]
    Wn_local <- Wn_local %>% dist_add(Wn, weight = kprob)

    tmpu <- tmpu_cache[[k]]

    # flog.info("k = %d, E(Wn) = %.2f, E(Wn x lambda) = %.2f, E(Wn x mu x k) = %.2f, E(tmpu) = %.2f, kprob = %.2f",
    #           k,
    #           E(Wn),
    #           generate_pdf(list(dist = "pois", lambda = E(Wn) * lambda), xmax = 100, accuracy = .99999) %>% E,
    #           generate_pdf(list(dist = "pois", lambda = E(Wn) * mu * k), xmax = 100, accuracy = .99999) %>% mirror_distribution() %>% E,
    #           E(tmpu),
    #           kprob)

    S <- list(x = 0, prob = 0)

    interval_length <- E(Wn)

    U_reduced <- reduce_dist(U, accuracy)
    work_length <- U_reduced$x / (mu * k)
    # rho vector right now
    rho_vec_present <- pmin((work_length / interval_length) + (lambda / (k * mu)), 1)

    # dynamic history length (60 seconds)
    histlen <- min(i-1, df_ew %>% mutate(csew = cumsum(ew)) %>% filter(csew <= 60) %>% count %>% pull(n))

    # Lower end of the range of iteration numbers to consider.
    miniter <- (i - histlen)

    # exponential decay (approx. to linux load computation)
    decay_vector <- exp(-1:-(histlen))

    rho_historical <-
      rho_history %>%
      filter(it >= miniter, it < i) %>% # Stefan: We need to exclude the current iteration, as it only contains _some_ instances of K at this point, which artifically reduces rho
      group_by(it) %>%
      summarise(meanrho = sum(rho * kprob)) %>% # Stefan: I think we make an error here if we don't exclude the current iteration
      arrange(-it) %>%
      mutate(exponential_meanrho = meanrho * decay_vector) %>%
      summarize(meanrho_final = sum(exponential_meanrho) / sum(decay_vector)) %>%
      pull(meanrho_final)

    # Weight of present rho values.
    w_present <- 1-sum(decay_vector)
    if (histlen == 0) {
      rho_vec <- rho_vec_present
    } else {
      rho_vec <- w_present * rho_vec_present + (1 - w_present) * rho_historical
    }

    mean_rho <- sum(rho_vec[U_reduced$prob > 0] * U_reduced$prob[U_reduced$prob > 0]) / sum(U_reduced$prob[U_reduced$prob > 0])
    rho_history <- rho_history %>% add_row(it = i, k = k, rho = mean_rho, kprob = kprob)

    S_list <- lapply(rho_vec, scaling_function)

    for (ii in 1:length(S_list)) {
      S <- S %>% dist_add(S_list[[ii]], weight = U$prob[ii])
    }

    U_new <- U_new %>% dist_add(tmpu, weight = kprob)

    tmp <- pi_operator(generate_pdf(params = list(dist = "det", d = k)) %c% S)

    K_new <- dist_add(K_new,tmp, weight = kprob)

  }


  Wn_total <- reduce_dist(Wn_total, accuracy) %c% reduce_dist(Wn_local, accuracy)

  flog.info("E(Wn_total) = %.2f,",
            Wn_total %>% E)

  difference <- dist_diff(K, K_new)

  if (difference <= (1-0.999)) {
    diff_counter <- diff_counter + 1
  }

  df_ek <- df_ek %>% add_row(it = i, ek = E(K), stdk = SD(K))

  K <- reduce_dist(K_new, accuracy)

  flog.info("E(U) = %.2f, E(U_new) = %.2f, E(U conv U_new) = %.2f",
            U %>% E,
            U_new %>% E,
            (U %c% U_new) %>% E)

  flog.info("mean rho: %.4f",
            rho_history %>% filter(it == i) %>% group_by(it) %>% summarize(meanrho = sum(rho * kprob)) %>% pull(meanrho))

  # Until here, U_new was just the term for the difference in additionally arrived and processed work.
  # Now, we add it to the unfinished work at the previous embedding time.
  U_new <- (reduce_dist(U, accuracy) %c% reduce_dist(U_new, accuracy)) %>% pi_operator()

  #difference <- dist_diff(U, U_new)

  U <- U_new

  # flog.info("E[U_new] = %.3f", U_new$x %*% U_new$prob)
  df_eu <- df_eu %>% add_row(it = i, eu = E(U_new), stdu = SD(U_new))
  df_ew <- df_ew %>% add_row(it = i, ew = E(Wn_local), stdw = SD(Wn_local), ewt = E(Wn_total), stdwt = SD(Wn_total))
}

flog.info(paste0("Final E(K): ", E(K)))

df_data <- df_ek %>%
  left_join(df_ew, by = "it") %>%
  left_join(df_eu, by = "it") %>%
  mutate(mean_time = cumsum(ew))

ggplot(df_data) +
  geom_point(aes(x = mean_time, y = ek, ymin = ek - stdk, ymax = ek + stdk), color = "red") +
  geom_line(aes(x = mean_time, y = ek, ymin = ek - stdk, ymax = ek + stdk), color = "red") +
  #geom_ribbon(aes(x = mean_time, y = ek, ymin = ek - stdk, ymax = ek + stdk), fill = "red", alpha = 0.3) +
  geom_point(aes(x = mean_time, y = ew, ymin = ew - stdw, ymax = ew + stdw), color = "blue") +
  geom_line(aes(x = mean_time, y = ew, ymin = ew - stdw, ymax = ew + stdw), color = "blue") +
  #geom_ribbon(aes(x = mean_time, y = ew, ymin = ew - stdw, ymax = ew + stdw), fill = "blue", alpha = 0.3) +
  geom_point(aes(x = mean_time, y = eu, ymin = eu - stdu, ymax = eu + stdu), color = "orange") +
  geom_line(aes(x = mean_time, y = eu, ymin = eu - stdu, ymax = eu + stdu), color = "orange")

saveRDS(K, "./autoscaling_model/K_jump_up_config1.Rds")
fwrite(df_data, file = "jump_up_config1.csv")
