library(ggplot2)
library(reshape)

print_number_of_scaling_decisions <- function(mo_attributes) {
  temp1 <- mo_attributes
  data <- temp1[temp1$key == "num_instances",]
  rownames(data) <- NULL
  up <- 0
  hold <- 0
  down <- 0
  old_val <- data[1, 4]
  for (i in 2:nrow(data)) {
    if (data[i, 4] > old_val) {
      up <- up + 1
    } else if (data[i, 4] < old_val) {
      down <- down + 1
    } else {
      hold <- hold + 1
    }
    old_val <- data[i, 4]
  }
  sprintf("Upscalings: %d\nHolds: %d\nDownscalings: %d\n", up, hold, down)
}

get_instances_over_time <- function(mo_attributes) {
  temp1 <- mo_attributes
  data <- temp1[temp1$key == "num_instances",]
  rownames(data) <- NULL
  return(data[, c(1, 4)])
}

get_inter_decision_times <- function(mo_attributes) {
  temp1 <- mo_attributes
  data <- temp1[temp1$key == "num_instances",]
  rownames(data) <- NULL
  result <- data.frame(time = numeric(),
                       value = numeric())
  old_val <- data[1, 1]
  for (i in 2:nrow(data)) {
    result[nrow(result) + 1,] = c(data[i, 1], data[i, 1] - old_val)
    old_val <- data[i, 1]
  }
  return(result)
}

get_response_times_over_time <- function(mo_arrivals) {
  temp1 <- mo_arrivals
  data <- temp1[grepl("request.*", temp1[, 1]),]
  data <- data[order(data$start_time),]
  rownames(data) <- NULL
  data$response_time <- data$end_time - data$start_time

  result <- subset(data, select = c(start_time, response_time))
  names(result)[1] <- "time"
  names(result)[2] <- "value"

  # for (i in 1:nrow(data)) {
  # result[i, ] = c(data[i, 2], data[i, 3] - data[i, 2])
  # }
  return(result)
}

get_waiting_times_over_time <- function(mo_arrivals) {
  temp1 <- mo_arrivals
  data <- temp1[grepl("request.*", temp1[, 1]),]
  data <- data[order(data$start_time),]
  rownames(data) <- NULL
  data$waiting_time <- data$end_time -
    data$start_time -
    data$activity_time

  result <- subset(data, select = c(start_time, waiting_time))
  names(result)[1] <- "time"
  names(result)[2] <- "value"
  return(result)
}

get_utilization_over_time <- function(mo_resources) {
  data <- mo_resources
  result <- data.frame(time = seq(from = 5000, to = now(env), by = 5000))
  current_time <- 5000
  all_services <- unique(data$resource)
  for (current_instance in all_services) {
    result_for_instance <- NULL
    while (current_time <= now(env)) {
      # Own implementation with CPU throttling support
      temp1 <- data[data$resource == current_instance,] # Get all data from this instance
      # Calculate starting condition
      data_before <- temp1[temp1$time < current_time - 60000,]
      # No entry means resource was not in use
      if (nrow(data_before) == 0) {
        in_use <- FALSE
      } else {
        # Resource was and is in use
        if (data_before[nrow(data_before), 3] == 1) {
          in_use <- TRUE
          # Set beginning to start
          in_use_since <- current_time - 60000
        } else {
          in_use <- FALSE
        }
      }
      temptemp <- temp1[temp1$time >= current_time - 60000,]
      temp2 <- temptemp[temptemp$time < current_time,] # Get the last minute
      if (nrow(temp2) > 0) {
        rownames(temp2) <- NULL # Reset row names / indices
        time_used <- 0
        for (z in 1:nrow(temp2)) {
          if (temp2[z, 3] == 0 & in_use == TRUE) { # If the instance is now free, but was in use before
            time_used <- time_used + temp2[z, 2] - in_use_since
            in_use <- FALSE
          } else if (temp2[z, 3] == 1 & in_use == FALSE) { # If the instance is now occupied, but was free before
            in_use <- TRUE
            in_use_since <- temp2[z, 2]
          }
        }
        if (temp2[nrow(temp2), 3] == 1) { # If last entry indicates instance in use, then it is still in use
          time_used <- time_used + current_time - in_use_since
        }
        monitored_value <- time_used / min(c(current_time, 60000)) # Utilization = time_in_use / 60 seconds
      } else {
        if (in_use == TRUE) {
          monitored_value <- 1
        } else {
          monitored_value <- 0
        }
      }
      result_for_instance <- append(result_for_instance, monitored_value)
      current_time <- current_time + 5000
    }
    result[current_instance] <- result_for_instance
    current_time <- 5000
  }
  return(result)
}

get_slo_violations_over_time <- function(slo_target, mo_arrivals) {
  temp1 <- mo_arrivals
  data <- temp1[grepl("request.*", temp1[, 1]),]
  data <- data[order(data$start_time),]
  rownames(data) <- NULL
  data$response_time <- data$end_time - data$start_time

  result <- subset(data, select = c(start_time, response_time))
  result$response_time[result$response_time <= slo_target] <- 0
  result$response_time[result$response_time > slo_target] <- 1
  names(result)[1] <- "time"
  names(result)[2] <- "value"
  return(result)
}

print_p_metric <- function(slo_target, mo_arrivals, mo_attributes, no_env) {
  slo_violations <- get_slo_violations_over_time(slo_target, mo_arrivals)
  V <- sum(slo_violations$value) / nrow(slo_violations)
  # sprintf("V: %f\n", V)
  instances <- get_instances_over_time(mo_attributes)
  total_costs <- 0
  old_time <- instances[1, 1]
  old_val <- instances[1, 2]
  for (i in 2:nrow(instances)) {
    total_costs <- total_costs + (instances[i, 1] - old_time) * old_val
    old_time <- instances[i, 1]
    old_val <- instances[i, 2]
  }
  total_costs <- total_costs + (no_env - old_time) * old_val
  # sprintf("Ctotal: %f\n", total_costs)
  max_costs <- no_env * max(instances$value)
  # sprintf("Cmax: %f\n", max_costs)
  norm_costs <- total_costs / max_costs
  # sprintf("C: %f\n", norm_costs)
  p1_2 <- 0.5 * norm_costs + 0.5 * V
  # sprintf("P0.5: %f\n", p1_2)
  p2_3 <- norm_costs / 3 + 2 * V / 3
  sprintf("V: %f\nCtotal: %f\nCmax: %f\nC: %f\nP0.5: %f\nP0.66: %f\n", V, total_costs, max_costs, norm_costs, p1_2, p2_3)
}

plot_instances_over_time <- function(file_name = "") {
  data <- get_instances_over_time()
  # pdf(file=file_name, width = 11, height = 6)
  ggplot() +
    geom_step(data = data, mapping = aes(x = time, y = value), color = "black") +
    # xlim(0,20)
    xlab("Time") +
    theme_bw() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      text = element_text(size = 30),
      axis.title.y = element_text(color = "black")
    ) +
    scale_y_continuous( # limits = c(0,15),
      name = "Instances"
    )
  if (file_name != "") {
    ggsave(file_name, limitsize = FALSE)
  }
}

plot_inter_decision_times <- function(file_name = "") {
  data <- get_inter_decision_times()
  # if (file_name != "") {
  # pdf(file=file_name, width = 11, height = 6)
  # }
  ggplot() +
    geom_step(data = data, mapping = aes(x = time, y = value), color = "black") +
    # xlim(0,20)
    xlab("Time") +
    theme_bw() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      text = element_text(size = 30),
      axis.title.y = element_text(color = "black")
    ) +
    scale_y_continuous( # limits = c(0,15),
      name = "Inter-Decision Time"
    )
  if (file_name != "") {
    ggsave(file_name, limitsize = FALSE)
  }
}

plot_response_times <- function(file_name = "") {
  temp1 <- get_response_times_over_time()
  # get one value per second (average)
  time_bound <- 60000
  data <- data.frame(time = numeric(),
                     value = numeric())
  data[1,] <- c(0, 0)
  start_index <- 1
  end_index <- 1
  while (end_index < nrow(temp1)) {
    if (temp1[end_index, 1] <= time_bound) {
      end_index <- end_index + 1
      next
    }
    temp2 <- temp1[start_index:end_index,]
    average_value <- mean(temp2$value)
    data[nrow(data) + 1,] <- c(time_bound, average_value)
    end_index <- end_index + 1
    start_index <- end_index
    time_bound <- time_bound + 60000
  }
  # if (file_name != "") {
  # pdf(file=file_name, width = 11, height = 6)
  # }
  ggplot() +
    geom_step(data = data, mapping = aes(x = time, y = value), color = "black") +
    # xlim(0,20)
    xlab("Time") +
    theme_bw() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      text = element_text(size = 30),
      axis.title.y = element_text(color = "black")
    ) +
    scale_y_continuous( # limits = c(0,15),
      name = "Response Time"
    )
  if (file_name != "") {
    ggsave(file_name, limitsize = FALSE)
  }
}

plot_waiting_times <- function(file_name = "") {
  temp1 <- get_waiting_times_over_time()
  # get one value per second (average)
  time_bound <- 60000
  data <- data.frame(time = numeric(),
                     value = numeric())
  data[1,] <- c(0, 0)
  start_index <- 1
  end_index <- 1
  while (end_index < nrow(temp1)) {
    if (temp1[end_index, 1] <= time_bound) {
      end_index <- end_index + 1
      next
    }
    temp2 <- temp1[start_index:end_index,]
    average_value <- mean(temp2$value)
    data[nrow(data) + 1,] <- c(time_bound, average_value)
    end_index <- end_index + 1
    start_index <- end_index
    time_bound <- time_bound + 60000
  }
  # if (file_name != "") {
  # pdf(file=file_name, width = 11, height = 6)
  # }
  ggplot() +
    geom_step(data = data, mapping = aes(x = time, y = value), color = "black") +
    # xlim(0,20)
    xlab("Time") +
    theme_bw() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      text = element_text(size = 30),
      axis.title.y = element_text(color = "black")
    ) +
    scale_y_continuous( # limits = c(0,15),
      name = "Waiting Time"
    )
  if (file_name != "") {
    ggsave(file_name, limitsize = FALSE)
  }
}

plot_utilization <- function(file_name = "") {
  temp1 <- get_utilization_over_time()
  data <- melt(temp1, id.vars = "time")
  # if (file_name != "") {
  # pdf(file=file_name, width = 11, height = 6)
  # }
  ggplot(data, aes(x = time, y = value, colour = variable)) +
    geom_line() +
    # xlim(0,20)
    xlab("Time") +
    theme_bw() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      text = element_text(size = 30),
      axis.title.y = element_text(color = "black")
    ) +
    scale_y_continuous(
      limits = c(0,1),
      name = "CPU Utilization"
    )
  if (file_name != "") {
    ggsave(file_name, limitsize = FALSE)
  }
}

plot_slo_violations <- function(slo_target, file_name = "") {
  temp1 <- get_slo_violations_over_time(slo_target)
  # get one value per second (average)
  current_time <- now_env
  time_bound <- 60000
  data <- data.frame(time = numeric(),
                     value = numeric())
  data[1,] <- c(0, 0)
  while (time_bound < current_time + 60000) {
    temp2 <- temp1[temp1$time > time_bound - 60000 & temp1$time <= time_bound,]
    average_value <- mean(temp2$value)
    data[nrow(data) + 1,] <- c(time_bound, average_value)
    time_bound <- time_bound + 60000
  }
  # if (file_name != "") {
  # pdf(file=file_name, width = 11, height = 6)
  # }
  ggplot() +
    geom_step(data = data, mapping = aes(x = time, y = value), color = "black") +
    # xlim(0,20)
    xlab("Time") +
    theme_bw() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      text = element_text(size = 30),
      axis.title.y = element_text(color = "black")
    ) +
    scale_y_continuous( # limits = c(0,15),
      name = "SLO Violated"
    )
  if (file_name != "") {
    ggsave(file_name, limitsize = FALSE)
  }
}

plot_instances_and_slo_violations <- function(slo_target, file_name = "") {
  instance_data <- get_instances_over_time()
  # for (i in 1:12) {
  # instance_data[nrow(instance_data) + 1, ] <- c(4260000 + i * 60000, 1)
  # }
  # -----------
  temp1 <- get_slo_violations_over_time(slo_target)
  # get one value per second (average)
  current_time <- now_env
  time_bound <- 60000
  data <- data.frame(time = numeric(),
                     value = numeric())
  data[1,] <- c(0, 0)
  while (time_bound < current_time + 60000) {
    temp2 <- temp1[temp1$time > time_bound - 60000 & temp1$time <= time_bound,]
    average_value <- mean(temp2$value)
    data[nrow(data) + 1,] <- c(time_bound, average_value)
    time_bound <- time_bound + 60000
  }
  start_time <- 0
  end_time <- 0
  in_area <- FALSE
  list_of_areas <- data.frame(start = numeric(),
                              end = numeric())
  data[is.nan(data$value), 2] <- 0
  for (i in 1:nrow(data)) {
    if (in_area == FALSE & data[i, 2] >= 0.01) {
      in_area <- TRUE
      start_time <- data[i, 1]
    } else if (in_area == TRUE & data[i, 2] < 0.01) {
      end_time <- data[i, 1]
      list_of_areas[nrow(list_of_areas) + 1,] = c(start_time, end_time)
      in_area <- FALSE
    }
  }
  # final_plot <- ggplot()
  # for (i in 1:nrow(list_of_areas)) {
  # final_plot <- final_plot + geom_rect(aes(xmin = list_of_areas[i, 1], xmax = list_of_areas[i, 2], ymin = -Inf, ymax = Inf, fill = "red"), alpha = .2)
  # }
  # if (file_name != "") {
  # pdf(file=file_name, width = 11, height = 6)
  # }
  ggplot() +
    geom_rect(aes(xmin = list_of_areas$start / 1000, xmax = list_of_areas$end / 1000, ymin = -Inf, ymax = Inf, fill = "red"), alpha = .2, inherit.aes = FALSE) +
    geom_step(data = instance_data, mapping = aes(x = time / 1000, y = value), color = "black") +
    # xlim(c(0,4980)) +
    xlab("Time [s]") +
    # ggtitle("Larger Tolerance Zone") +
    theme_bw() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      plot.title = element_text(hjust = 0.5),
      text = element_text(size = 30),
      axis.title.y = element_text(color = "black")
    ) +
    scale_y_continuous( # limits = c(0,15),
      name = "Instances"
    ) +
    theme(legend.position = "none")
  if (file_name != "") {
    ggsave(file_name, limitsize = FALSE)
  }
}