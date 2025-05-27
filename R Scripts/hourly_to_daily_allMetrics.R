# Load the necessary libraries
library(lubridate)
library(dplyr) 
library(tidyr)

# R Environment should be set with the foloowing datasets: hourlyCalories,
# hourlyHeartRate, hourlyIntensities, hourlyMETs, hourlySleep and hourlySteps

# Daily Aggregation for hourlyCalories
dailyCalories <- hourlyCalories %>%
  group_by(Id, ActivityDate) %>%
  summarise(
    Calories = sum(Calories, na.rm = TRUE),
    .groups = "drop"
  )

# Daily Aggregation for hourlySteps
dailySteps <- hourlySteps %>%
  group_by(Id, ActivityDate) %>%
  summarise(
    StepTotal = sum(StepTotal, na.rm = TRUE),
    .groups = "drop"
  )

# Daily Aggregation for hourlyIntensities
dailyIntensities <- hourlyIntensities %>%
  group_by(Id, ActivityDate) %>%
  summarise(
    MinutesVeryActive = sum(VeryActiveMinutes, na.rm = TRUE),
    MinutesFairlyActive = sum(FairlyActiveMinutes, na.rm = TRUE),
    MinutesLightlyActive = sum(LightlyActiveMinutes, na.rm = TRUE),
    MinutesSedentary = sum(SedentaryMinutes, na.rm = TRUE),
    .groups = "drop"
  )

# Daily Aggregation for hourlyMETs
dailyMETs <- hourlyMETs %>%
  group_by(Id, ActivityDate) %>%
  summarise(
    MinutesLowMETs = sum(LowMETs, na.rm = TRUE),
    MinutesLightMETs = sum(LightMETs, na.rm = TRUE),
    MinutesModerateMETs = sum(ModerateMETs, na.rm = TRUE),
    MinutesVigorousMETS = sum(HighMETs, na.rm = TRUE),
    .groups = "drop"
  )

# Daily Aggregation for hourlySleep
dailySleep <- hourlySleep %>%
  group_by(Id, Date) %>%
  summarise(
    MinutesLevel1 = sum(minutes_SleepLevel1, na.rm = TRUE),
    MinutesLevel2 = sum(minutes_SleepLevel2, na.rm = TRUE),
    MinutesLevel3 = sum(minutes_SleepLevel3, na.rm = TRUE),
    .groups = "drop"
  )

# 6. Daily Aggregation for hourlyHeartRate
dailyHeartRate <- hourlyHeartRate %>%
  group_by(Id, Date) %>%
  summarize(
    avgHeartRate = round(mean(avgValue, na.rm = TRUE),1),
    .groups = "drop"
  )

# Perform a full join to keep all rows from both dailySteps and dailyCalories
dailyActivity <- dailySteps %>%
  full_join(dailyCalories, by = c("Id", "ActivityDate"))

# Perform a full join to combine dailyActivity and dailyIntensities
dailyActivity <- dailyActivity %>%
  full_join(dailyIntensities, by = c("Id", "ActivityDate"))

# Perform a full join to combine dailyActivity and dailyMETs
dailyActivity <- dailyActivity %>%
  full_join(dailyMETs, by = c("Id", "ActivityDate"))

# Rename the ActivityDate column in dailyActivity to match hourlyHeartRate
dailyActivity <- dailyActivity %>%
  rename(Date = ActivityDate)

# Perform a full join to combine dailyActivity and dailyHeartRate
dailyActivity <- dailyActivity %>%
  full_join(dailyHeartRate, by = c("Id", "Date" ))

# Perform a full join to combine dailyActivity and dailySleep
dailyActivity <- dailyActivity %>%
  full_join(dailySleep, by = c("Id", "Date"))

# Convert ActivityDate to Date data type if it's not already
dailyActivity <- dailyActivity %>%
  mutate(Date_as_date = dmy(Date))

# Add a new column 'Weekday'
dailyActivity <- dailyActivity %>%
  mutate(Weekday = wday(Date_as_date, label = TRUE, week_start = 1))