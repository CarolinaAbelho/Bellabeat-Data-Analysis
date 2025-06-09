library(dplyr)
library(lubridate) 

# List of data sets
datasets_to_check <- c("hourlyCalories", "hourlySteps", "hourlyIntensities",
                       "hourlyMETs", "hourlySleep", "hourlyHeartRate")

# For each data set, check:                                             

# TEMPORAL CONSISTENCY:
# date range: 12/03/2016 to 12/05/2016 (expected 62 tracked days)
# hour range: 0 to 23

for (dataset_name in datasets_to_check) {
  ds <- get(dataset_name)
  
  cat("\n--- Dataset:", dataset_name, "---\n")

  # Find the minimum and maximum dates
  min_date <- min(ds$Date, na.rm = FALSE)
  max_date <- max(ds$Date, na.rm = FALSE)
  
  # Find the minimum and maximum hours
  min_hour <- min(ds$Hour, na.rm = FALSE)
  max_hour <- max(ds$Hour, na.rm = FALSE)
  
  # Print the results  and number of tracked days
  cat(" Hours Range: From", format(min_hour), "to", format(max_hour), "\n")
  cat("  Date Range: From", format(min_date), "to", format(max_date), "\n")
  cat("Tracked days:", as.integer(max_date - min_date) + 1, "days\n")
}

# UNIQUE USER IDENTIFICATION

for (dataset_name in datasets_to_check) {
  ds <- get(dataset_name)
  
  cat("\n--- Dataset:", dataset_name, "---\n")
  
  # Get the number of IDs
  distinct_ids <- n_distinct(ds$Id)
  
  # Get the list of unique IDs
  list_distinct_ids <- unique(ds$Id)

  cat("Number of distinct IDs:", distinct_ids, "\n")
  cat("List of distinct IDs:", paste(sort(list_distinct_ids), collapse = ", "), "\n")
}

# RANGE CHECK FOR EACH METRIC

for (dataset_name in datasets_to_check) {
  ds <- get(dataset_name)
  
  cat("\n--- Dataset:", dataset_name, "---\n")
  
  # Define the metric columns for the current data set
  metric_cols <- switch(dataset_name,
                        "hourlyCalories" = "Calories",
                        "hourlySteps" = "StepTotal",
                        "hourlyIntensities" = c("SedentaryMinutes", "FairlyActiveMinutes",
                                                "LightlyActiveMinutes", "VeryActiveMinutes"),
                        "hourlyMETs" = c("LowMETs", "LightMETs", "ModerateMETs", "HighMETs"),
                        "hourlySleep" = c("minutes_SleepLevel1", "minutes_SleepLevel2", "minutes_SleepLevel3"),
                        "hourlyHeartRate" = "avgValue"
  )
  
 # Iterate through each specified metric column for the current data set
  for (col_name in metric_cols) {
    
    # Get the values for the current metric column
    metric_values <- ds[[col_name]] #
    
    # Find min and max
    min_metric <- min(metric_values, na.rm = TRUE)
    max_metric <- max(metric_values, na.rm = TRUE)
    na_count <- sum(is.na(metric_values))
    
    # Print results
    cat("  - Metric Column: '", col_name, "'\n", sep="")
    cat("    Min Value:", min_metric, "\n")
    cat("    Max Value:", max_metric, "\n")
    cat("    NA Values:", na_count, "\n")
  }
}

summary(hourlyCalories$Calories) 
summary(hourlySteps$StepTotal)

summary(hourlyIntensities$SedentaryMinutes)
summary(hourlyIntensities$LightlyActiveMinutes)
summary(hourlyIntensities$FairlyActiveMinutes)
summary(hourlyIntensities$VeryActiveMinutes)

summary(hourlyMETs$LowMETs)
summary(hourlyMETs$LightMETs)
summary(hourlyMETs$ModerateMETs)
summary(hourlyMETs$HighMETs)

summary(hourlySleep$minutes_SleepLevel1)
summary(hourlySleep$minutes_SleepLevel2)
summary(hourlySleep$minutes_SleepLevel3)

summary(hourlyHeartRate$avgValue)

#LOGICAL INTEGRITY CHECKS:

#INTENSITIES
# Calculate the sum of all intensity minutes for each hour
hourlyIntensities_check <- hourlyIntensities %>%
  mutate(
    TotalMinutes = SedentaryMinutes + FairlyActiveMinutes +
      LightlyActiveMinutes + VeryActiveMinutes
  ) %>%
# Check if all TotalMinutes are greater than 60 and print result
  filter(TotalMinutes > 60)
  print(hourlyIntensities_check$TotalMinutes)

#METs
# Calculate the sum of minutes in all METs categories for each hour
hourlyMETs_check <- hourlyMETs %>%
  mutate(
    TotalMinutes = LowMETs + LightMETs + ModerateMETs + HighMETs
  ) %>%
  # Check if all TotalMinutes are greater than 60 and print result
  filter(TotalMinutes > 60)
  print(hourlyMETs_check$TotalMinutes)

#SLEEP
# Calculate the sum of minutes in sleep levels for each hour
hourlySleep_check <- hourlySleep %>%
  mutate(
    TotalMinutes = minutes_SleepLevel1 + minutes_SleepLevel2 + minutes_SleepLevel3
  ) %>%
  # Check if all TotalMinutes are greater than 60 and print result
  filter(TotalMinutes > 60)
  print(hourlySleep_check$TotalMinutes)

#CHECK FOR DATA COMPLETENESS FOR EACH USER

for (df_name in datasets_to_check) {
  
  df <- get(df_name)
 
  cat("\n--- Dataset:", df_name, "---\n")
  
  # Summarize records per user
  completeness_summary <- df %>%
    group_by(Id) %>%
    summarize(
      Tracked_Days = n_distinct(Date, na.rm = TRUE), # Number of unique days
      Tracked_Hours = n() # Number of hourly records
    )  %>%
  # Order from largest Tracked_Days to smallest 
  arrange(desc(Tracked_Days))
  
  # Print the summary for the current data set
  print(completeness_summary, n=35)
}

