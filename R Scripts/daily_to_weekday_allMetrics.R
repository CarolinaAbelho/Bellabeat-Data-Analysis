# Load the necessary libraries
library(lubridate)
library(dplyr) 
library(tidyr)

# Calculate the average daily calorie expenditure per weekday
weekdaysCalories <- dailyActivity %>%
  group_by(Weekday) %>%
  summarize(average_calories = round(mean(Calories, na.rm = TRUE))) %>%
  arrange(factor(Weekday, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))) # Ensure weekdays are in order

# Calculate the total steps per weekday
weekdaySteps <- dailyActivity %>%
  group_by(Weekday) %>%
  summarize(total_steps = round(mean(StepTotal, na.rm = TRUE))) %>%
  arrange(factor(Weekday, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))) # Ensure weekdays are in order

# Calculate total minutes for each intensity level per weekday
weekdaysIntensities <- dailyActivity %>%
  group_by(Weekday) %>%
  summarize(
    VeryActiveMinutes = mean(MinutesVeryActive, na.rm = TRUE),
    FairlyActiveMinutes = mean(MinutesFairlyActive, na.rm = TRUE),
    LightlyActiveMinutes = mean(MinutesLightlyActive, na.rm = TRUE),
    SedentaryMinutes = mean(MinutesSedentary, na.rm = TRUE),
  ) %>%
  arrange(factor(Weekday, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")))

# Calculate the total minutes for each METs intensity level per weekday
weekdaysMETs <- dailyActivity %>%
  group_by(Weekday) %>%
  summarize(
    MinutesLowMETs = mean(MinutesLowMETs, na.rm = TRUE),
    MinutesLightMETs = mean(MinutesLightMETs, na.rm = TRUE),
    MinutesModerateMETs = mean(MinutesModerateMETs, na.rm = TRUE),
    MinutesVigorousMETS = mean(MinutesVigorousMETS, na.rm = TRUE)
  ) %>%
  arrange(factor(Weekday, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")))

# Calculate the total minutes for each sleep level per weekday
weekdaySleep <- dailyActivity %>%
  group_by(Weekday) %>%
  summarize(
    Level1Minutes = mean(MinutesLevel1, na.rm = TRUE),
    Level2Minutes = mean(MinutesLevel2, na.rm = TRUE),
    Level3Minutes = mean(MinutesLevel3, na.rm = TRUE),
  ) %>%
  arrange(factor(Weekday, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")))

# 1. Join calories and steps
averaged_metrics_weekday <- weekdaysCalories %>%
  full_join(weekdaySteps, by = "Weekday")

# 2. Join intensities
averaged_metrics_weekday <- averaged_metrics_weekday %>%
  full_join(weekdaysIntensities, by = "Weekday")

# 3. Join METs
averaged_metrics_weekday <- averaged_metrics_weekday %>%
  full_join(weekdaysMETs, by = "Weekday")

# 3. Join SleepLevels
averaged_metrics_weekday <- averaged_metrics_weekday %>%
  full_join(weekdaySleep, by = "Weekday")

# Export the data frame to a CSV file
write.csv(averaged_metrics_weekday, file = "average_metrics_by_weekday.csv", row.names = FALSE)

