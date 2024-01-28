# ------------------------
# CLEAR ENVIRONMENT
rm(list = ls())

# ------------------------
# DEPENDENCIES
library(extraDistr)
library(simmer)
library(stringr)
library(parallel)
library(data.table)
source("post_mortem_evaluation.R")

# ------------------------
# INPUTS
number_of_instances_at_start <- 8
loadbalancer_policy <- "random"
metric <- "utilization"
downscaling_policy <- "newest-first"
readiness_time_dist <- function() 2000
workload_trace <- read.csv('ExampleWorkload.txt', header = FALSE)
workload_trace <- workload_trace[['V2']]
get_service_time_from_dist <- function() rexp(1, 0.00625)
cpu_share <- 1
use_cpu_throttling <- FALSE
waiting_time_dist <- function() runif(1, min = 30000, max = 90000)
upscaling_function <- function(current_value) {
  threshold <- 0.9
  if (current_value < threshold) {
     return(0)
  } else {
     return(1)
  }
}
downscaling_function <- function(current_value) {
  threshold <- 0.3
  if (current_value < threshold) {
     return(1)
  } else {
     return(0)
  }
}

# ------------------------
# VARIABLES (ALL VALUES TO TEST)
seeds <- c(1, 6, 26, 42, 8808, 11, 16, 126, 142, 18808, 31, 36, 326, 342, 38808, 41, 46, 426, 442, 48808, 51, 56, 526, 542, 58808, 61, 66, 626, 642, 68808)

for (slot_start in c(1, 16)) {
    print(Sys.time())
    print(sprintf("slot_start = %.0f", slot_start))
    slot_end <- slot_start + 14
    results_for_all_seeds <-
      mclapply(seeds[slot_start:slot_end], function(current_seed) {
        load_iteration <- 0
        position_in_trace <- 1
        current_load_rate <- workload_trace[position_in_trace]

        calculate_interarrival_times <- function() {
          load_iteration <<- load_iteration + 1
          if (load_iteration > workload_trace[position_in_trace]) {
            load_iteration <<- 1
            position_in_trace <<- position_in_trace + 1
            if (position_in_trace > length(workload_trace)) {
              return(-1)
            }
            current_load_rate <<- workload_trace[position_in_trace]
          }
          return(1000.0 / current_load_rate)
        }

        set.seed(current_seed)

        print(sprintf("Run with seed %f", current_seed))

        # THE SIMULATION LOGIC

        env <- simmer("DASSimulation")

        # ------------------------
        # HELPER FUNCTIONS

        get_active_instances <-
          function() {
            # Function that returns list of instances with queue_size bigger than 0
            instances_list <-
              get_resources(env)[grepl("testservice.*", get_resources(env))]
            active_instances <- character()
            if (is.null(instances_list)) {
              return(active_instances)
            }
            for (i in 1:length(instances_list)) {
              if (get_queue_size(env, instances_list[i]) != 0) {
                active_instances <- append(active_instances, instances_list[i])
              }
            }
            return(active_instances)
          }

        downscaling_instance <-
          NULL # Name of generator that should be turned off next

        downscaling_possible <-
          function() {
            # Function that determines whether there is an instance that has a queue_size bigger than 0
            instances_list <-
              get_resources(env)[grepl("testservice.*", get_resources(env))]
            active_instances <- 0
            for (i in 1:length(instances_list)) {
              if (get_queue_size(env, instances_list[i]) != 0) {
                active_instances <- active_instances + 1
                if (active_instances > 1) {
                  return(1)
                }
              }
            }
            return(2)
          }

        set_downscaling_instance <-
          function() {
            # Function that returns name of next instance to downscale
            instances_list <-
              get_resources(env)[grepl("testservice.*", get_resources(env))]
            if (downscaling_policy == "random") {
              ordered_list <- sample(instances_list)
            } else if (downscaling_policy == "newest-first") {
              ordered_list <- rev(instances_list)
            } else {
              # oldest-first
              ordered_list <- instances_list
            }
            for (i in 1:length(ordered_list)) {
              if (get_queue_size(env, ordered_list[i]) == 0) {
                next
              }
              downscaling_instance <<-
                paste0("generator-scaler-", ordered_list[i], "-")
              return(ordered_list[i])
            }
            # Should not happen
            return(NULL)
          }

        perform_scale_up <- function() {
          offset <-
            get_resources(env)[grepl("testservice.*", get_resources(env))] %>%
              length() # count instances
          name_of_new_instance <-
            paste0("testservice", offset + 1) # assign new instance name
          add_resource(env, name_of_new_instance, 1) # add simmer resource
          # Add budget count variable for this resoource
          # start value = min(max_computing_time , time_till_end_of_slot)
          if (use_cpu_throttling == TRUE) {
            add_global(env,
                       paste0("budget-", name_of_new_instance),
                       min(c(
                         cpu_share * 100, round(now(env) + 50, digits = -2) - now(env)
                       )))
          }
          scaling_function_of_new_instance <-
            generate_scaling_function(name_of_new_instance)
          scaling_evaluation_trajectory_new <-
            trajectory(paste0("scaler-", name_of_new_instance)) %>%
              branch(
                function()
                  scaling_function_of_new_instance(),
                continue = TRUE,
                # Case 1: Scale up
                trajectory() %>%
                  # log_("Scale up") %>%
                  timeout(function()
                            readiness_time_dist()) %>%
                  timeout(function()
                            perform_scale_up()) %>%
                  set_global("num_instances", function()
                    get_global(env, "num_instances") + 1),
                # Case 2: Scale down
                trajectory() %>%
                  branch(
                    function()
                      downscaling_possible(),
                    continue = TRUE,
                    # Case 2.1: Downscaling is possible
                    trajectory() %>%
                      # log_("Scale down") %>%
                      select(function()
                               set_downscaling_instance()) %>%
                      set_queue_size_selected(0) %>%
                      deactivate(function()
                                   return(downscaling_instance)) %>%
                      set_global("num_instances", function()
                        get_global(env, "num_instances") - 1),
                    # Case 2.2: Downscaling not possible
                    trajectory() %>%
                      # log_("Wanted downscaling, but not possible") %>%
                      set_global("num_instances", function()
                        get_global(env, "num_instances"))
                  ),
                # Case 3: No scaling
                trajectory() %>%
                  # log_("No Scaling") %>%
                  set_global("num_instances", function()
                    get_global(env, "num_instances")),
                # Case 4: No monitoring values
                trajectory()
              ) # %>%
          # log_("No monitoring values available"))
          #skip_first_w <- waiting_time_dist()
          add_generator(
            env,
            paste0("generator-scaler-", name_of_new_instance, "-"),
            scaling_evaluation_trajectory_new,
            waiting_time_dist
          ) # create scaling generator
          return(0)
        }

        generate_scaling_function <- function(instance_name) {
          return(function() {
            data <- get_mon_resources(env) # Get monitoring data
            if (nrow(data) == 0) {
              # If there are none available
              monitored_value <- NaN
            }
            if (metric == "utilization") {
              # Function from simmer.optim uses deprecated dependencies: monitored_value <- msr_resource_utilization(env, instance_name)
              # Own implementation
              current_time <- now(env)
              temp1 <-
                data[data$resource == instance_name,] # Get all data from this instance
              if (nrow(temp1) > 0) {
                temp2 <-
                  temp1[temp1$time >= current_time - 60000,] # Get the last minute
                if (nrow(temp2) > 0) {
                  rownames(temp2) <- NULL # Reset row names / indices
                  time_used <- 0 # Sum of time, instance is occupied
                  if (temp2[1, 3] == 0) {
                    # If the first entry indicates 0 requests at server, there was a request in processing before
                    time_used <-
                      time_used + temp2[1, 2] - (current_time - 60000)
                    in_use <-
                      FALSE # Indicates whether instance is in use at the moment
                  } else {
                    in_use_since <- temp2[1, 2] # Time since the instance is occupied
                    in_use <- TRUE
                  }
                  if (nrow(temp2) > 1) {
                    for (z in 2:nrow(temp2)) {
                      if (temp2[z, 3] == 0 &
                        in_use == TRUE) {
                        # If the instance is now free, but was in use before
                        time_used <-
                          time_used + temp2[z, 2] - in_use_since
                        in_use <- FALSE
                      } else if (temp2[z, 3] == 1 &
                        in_use == FALSE) {
                        # If the instance is now occupied, but was free before
                        in_use <- TRUE
                        in_use_since <- temp2[z, 2]
                      }
                    }
                  }
                  if (temp2[nrow(temp2), 3] == 1) {
                    # If last entry indicates instance in use, then it is still in use
                    time_used <-
                      time_used + current_time - in_use_since
                  }
                  monitored_value <-
                    time_used / min(c(current_time, 60000)) # Utilization = time_in_use / 60 seconds
                } else {
                  monitored_value <-
                    0 # there is data from this instance but no action in the last minute, so utilization seems to be zero
                }
              } else {
                monitored_value <- NaN
              }
            }
            else if (metric == "received-requests") {
              current_time <- now(env)
              temp1 <-
                data[data$resource == instance_name,] # Get all data from this instance
              if (nrow(temp1) > 0) {
                temp2 <-
                  temp1[temp1$time >= current_time - 60000,] # Get the last minute
                if (nrow(temp2) > 0) {
                  rownames(temp2) <- NULL # Reset row names / indices
                  received <- 0 # Sum of received requests
                  temp3 <-
                    temp1[temp1$time < current_time - 60000,] # Get also values older than 1 minute
                  if (nrow(temp3) > 0) {
                    # If old values exist
                    system_state_before <-
                      temp3[nrow(temp3), 7] # Take the latest value older than 1 minute as start value
                  } else {
                    system_state_before <- 0 # Else start with 0
                  }
                  for (z in 1:nrow(temp2)) {
                    # For all entries
                    if (temp2[z, 7] > system_state_before) {
                      # If requests in instance system (processing + in queue) is increasing
                      received <-
                        received + temp2[z, 7] - system_state_before # Add the difference to the request counter
                    }
                    system_state_before <-
                      temp2[z, 7] # Update state
                  }
                  monitored_value <-
                    received # Sum of received requests
                } else {
                  monitored_value <- NaN
                }
              } else {
                monitored_value <- NaN
              }
            }
            else if (metric == "queue-size") {
              temp1 <-
                data[data$resource == instance_name,] # Get all data from this instance
              if (nrow(temp1) > 0) {
                # If instance has logs
                monitored_value <-
                  temp1[nrow(temp1), 4] # take latest log and return elements in queue
              } else {
                monitored_value <-
                  0 # if instance has no logs, queue size should be zero
              }
            }
            else {
              print("Metric not known, returing NaN")
              momitored_value <- NaN
            }
            # print(monitored_value)
            if (is.nan(monitored_value)) {
              return(4)
            }
            # From here, it is the actual scaling algorithm
            random_variable <-
              runif(1, min = 0, max = 1) # Draw from uniform distribution
            p_up <-
              upscaling_function(monitored_value) # Get upscaling probability
            p_down <-
              downscaling_function(monitored_value) # Get downscaling probability
            if (p_up > 0 & random_variable < p_up) {
              # If upscaling
              return(1)
            } else if (p_down > 0 &
              random_variable < p_down) {
              # If downscaling
              return(2) # Downscaling will be handled in branch trajectory
            } else {
              return(3) # No scaling
            }
          })
        }

        calculate_cpu_time <- function(instance_name, cpu_share) {
          # Return value: list of (remaining budget, total timeout)
          service_time <- get_service_time_from_dist()
          current_time <- now(env)
          remaining_budget <-
            get_global(env, paste0("budget-", instance_name))
          # Due to requests reneging after 10 seconds, the ahead of time computation of the remaining budget fails sometimes
          # If at this stage, the remaining budget is negative we need to recover the correct value
          if (remaining_budget < 0) {
            # the remaining budget is always the time passed from the beginning of the slot
            time_since_slot_start <-
              current_time - round(current_time - 50, digits = -2)
            if (time_since_slot_start <= cpu_share * 100) {
              remaining_budget <- cpu_share * 100 - time_since_slot_start
            } else {
              remaining_budget <- 0
            }
          }
          # If the sum of service_time fits in the budget just compute the request
          if (remaining_budget - service_time >= 0) {
            return(c(remaining_budget - service_time, service_time))
          } else {
            # We compute the total request time with cpu throttling
            total_time <- 0
            # Consum budget from current 100ms slot
            total_time <- total_time + remaining_budget
            service_time_left <- service_time - remaining_budget
            # We need to add the remaining time of this 100ms slot when we are throttled
            # = end of 100ms slot - current_time - time_we_computed
            total_time <-
              total_time + round(current_time + 50, digits = -2) -
                current_time -
                remaining_budget
            # Evaluate future slots
            while (service_time_left > 0) {
              # We dont need full slot
              if (service_time_left < cpu_share * 100) {
                total_time <- total_time + service_time_left
                service_time_left <- 0
              } else {
                # We need 100ms full slot
                total_time <- total_time + 100
                # From this slot we just computed cpu_share
                service_time_left <-
                  service_time_left - cpu_share * 100
              }
            }
            return(c(remaining_budget - service_time, total_time))
          }
        }

        # ------------------------
        # TRAJECTORIES
        # trajectory of a request
        if (use_cpu_throttling == TRUE) {
          request_trajectory <- trajectory("request path") %>%
            # log_("Request generated") %>%
            select(function()
                     get_active_instances(),
                   loadbalancer_policy) %>%
            renege_in(10000) %>%
            seize_selected(1) %>%
            set_attribute(c("remaining-budget", "total-time"), function()
              calculate_cpu_time(get_selected(env), cpu_share)) %>%
            set_global(function()
                         paste0("budget-", get_selected(env)), function()
                         return(get_attribute(env, "remaining-budget"))) %>%
            timeout(function()
                      return(get_attribute(env, "total-time"))) %>%
            release_selected(1)
        } else {
          request_trajectory <- trajectory("request path") %>%
            select(function()
                     get_active_instances(),
                   loadbalancer_policy) %>%
            renege_in(10000) %>%
            seize_selected(1) %>%
            timeout(function()
                      get_service_time_from_dist()) %>%
            release_selected(1)
        }

        # show time trajectory
        show_time_trajectory <- trajectory("show time") %>%
          log_(function()
                 toString(now(env) / 1000))

        # cpu throttling and budget renewal
        if (use_cpu_throttling == TRUE) {

          increase_all_budgets <- function(.trj) {
            active_instances <- get_active_instances()
            for (instance in active_instances) {
              .trj <- set_global(.trj, paste0("budget-", instance), function() min(c(cpu_share * 100, get_global(env, paste0("budget-", instance)) + cpu_share * 100)))
            }
            return(.trj)
          }

          increase_budget_trajectory <- trajectory("increase-budget-trajectory") %>%
            increase_all_budgets()
          add_generator(env, "increase-budget-generator", increase_budget_trajectory, from(100, function() 100))
        }

        # ------------------------
        # START STATE

        create_resource <- function(instance_name) {
          add_resource(env, instance_name, 1) # Add simmer resource
          if (use_cpu_throttling == TRUE) {
            add_global(env,
                       paste0("budget-", instance_name),
                       cpu_share * 100)
          }
          evaluate_for_scaling <-
            generate_scaling_function(instance_name)
          # The trajectory with a log function wrapping the scaling function
          scaling_evaluation_trajectory <-
            trajectory(paste0("scaler-", instance_name)) %>%
              branch(
                function()
                  evaluate_for_scaling(),
                continue = TRUE,
                # Case 1: Scale up
                trajectory() %>%
                  # log_("Scale up") %>%
                  timeout(function()
                            readiness_time_dist()) %>%
                  timeout(function()
                            perform_scale_up()) %>%
                  set_global("num_instances", function()
                    get_global(env, "num_instances") + 1),
                # Case 2: Scale down
                trajectory() %>%
                  branch(
                    function()
                      downscaling_possible(),
                    continue = TRUE,
                    # Case 2.1: Downscaling is possible
                    trajectory() %>%
                      # log_("Scale down") %>%
                      select(function()
                               set_downscaling_instance()) %>%
                      set_queue_size_selected(0) %>%
                      deactivate(function()
                                   return(downscaling_instance)) %>%
                      set_global("num_instances", function()
                        get_global(env, "num_instances") - 1),
                    # Case 2.2: Downscaling not possible
                    trajectory() %>%
                      # log_("Wanted downscaling, but not possible") %>%
                      set_global("num_instances", function()
                        get_global(env, "num_instances"))
                  ),
                # Case 3: No scaling
                trajectory() %>%
                  # log_("No Scaling") %>%
                  set_global("num_instances", function()
                    get_global(env, "num_instances")),
                # Case 4: No monitoring values available
                trajectory() # %>%
                # log_("No monitoring values available")
              )

          #skip_first_w <- waiting_time_dist()
          # A generator creating the trajectory dependent on waiting_time_dist
          add_generator(
            env,
            paste0("generator-scaler-", instance_name, "-"),
            scaling_evaluation_trajectory,
            waiting_time_dist
          )
        }


        for (ii in 1:number_of_instances_at_start) {
          name <- paste0("testservice", ii) # Name of the service instance
          create_resource(name)
        }

        # The generator for the requests
        add_generator(env, "requestgenerator1", request_trajectory, function() calculate_interarrival_times())

        # The generator for the show time trajectory
        add_generator(env, "showtimegenerator", show_time_trajectory, function() 60000)

        # Global instance count
        add_global(env, "num_instances", number_of_instances_at_start)

        # ------------------------
        # SIMULATION RUN
        env %>%
          run(1000 * length(workload_trace)) %>%
          now()

        # ------------------------
        # SAVE RESULTS
        mon_arrivals <- get_mon_arrivals(env)
        mon_attributes <- get_mon_attributes(env)
        mon_resources <- get_mon_resources(env)
        write.csv(mon_arrivals, sprintf("seed%.0f_arrivals.csv", current_seed))
        write.csv(mon_resources, sprintf("seed%.0f_resources.csv", current_seed))
        now_env <- now(env)
        instances_over_time <- get_instances_over_time(mon_attributes)
        write.csv(instances_over_time, sprintf("seed%.0f_instances.csv", current_seed))
      }, mc.cores = 15)
}
