library(dplyr)
library(tidyr)
library(lubridate)

#Import data set into R environment from available csv files on unzipped folders from each time period:
  # 1. 12/03/2016 - 11/04/2016  
  # 2. 12/04/2016 - 12/05/2016

#Steps
hourlySteps1 <- read.csv("C:/Users/carol/Desktop/FitBit Fitness Tracker Data/Fitabase Data 3.12.16-4.11.16/hourlySteps_merged.csv")
hourlySteps2  <- read.csv("C:/Users/carol/Desktop/FitBit Fitness Tracker Data/Fitabase Data 4.12.16-5.12.16/hourlySteps_merged.csv")

#Calories
hourlyCal1 <- read.csv("C:/Users/carol/Desktop/FitBit Fitness Tracker Data/Fitabase Data 3.12.16-4.11.16/hourlyCalories_merged.csv")
hourlyCal2 <- read.csv("C:/Users/carol/Desktop/FitBit Fitness Tracker Data/Fitabase Data 4.12.16-5.12.16/hourlyCalories_merged.csv")

#Intensities
minIntensities1 <- read.csv("C:/Users/carol/Desktop/FitBit Fitness Tracker Data/Fitabase Data 3.12.16-4.11.16/minuteIntensitiesNarrow_merged.csv")
minIntensities2 <- read.csv("C:/Users/carol/Desktop/FitBit Fitness Tracker Data/Fitabase Data 4.12.16-5.12.16/minuteIntensitiesNarrow_merged.csv")

#Intensities
METs1 <- read.csv("C:/Users/carol/Desktop/FitBit Fitness Tracker Data/Fitabase Data 3.12.16-4.11.16/minuteMETsNarrow_merged.csv")
METs2 <- read.csv("C:/Users/carol/Desktop/FitBit Fitness Tracker Data/Fitabase Data 4.12.16-5.12.16/minuteMETsNarrow_merged.csv")

#Sleep
minuteSleep1 <- read.csv("C:/Users/carol/Desktop/FitBit Fitness Tracker Data/Fitabase Data 3.12.16-4.11.16/minuteSleep_merged.csv")
minuteSleep2 <- read.csv("C:/Users/carol/Desktop/FitBit Fitness Tracker Data/Fitabase Data 4.12.16-5.12.16/minuteSleep_merged.csv")

#Heart Rate
secondHeartRate1 <- read.csv("C:/Users/carol/Desktop/FitBit Fitness Tracker Data/Fitabase Data 3.12.16-4.11.16/heartrate_seconds_merged.csv")
secondHeartRate2 <- read.csv("C:/Users/carol/Desktop/FitBit Fitness Tracker Data/Fitabase Data 4.12.16-5.12.16/heartrate_seconds_merged.csv")

# Append files from both time periods
hourlyCalories <- rbind(hourlyCal2, hourlyCal1)
hourlySteps <- rbind(hourlySteps2, hourlySteps1)
minuteIntensities <- rbind(minIntensities2, minIntensities1)
minuteMETs <- rbind(METs1, METs2)
minuteSleep <- rbind(minuteSleep2, minuteSleep1)
secondsHeartRate <- rbind(secondHeartRate2, secondHeartRate1)

#Remove previous, unnecessary for analysis, datasets
rm(hourlyCal1, hourlyCal2, hourlySteps1, hourlySteps2, minIntensities1, minIntensities2, METs1, METs2, minuteSleep1, minuteSleep2, secondHeartRate1, secondHeartRate2)

# ___ STANDARDIZATION OF TEMPORAL DATA ___
# Convert the combined 'DateTime' column into a proper datetime object
# Split column regarding Date/Time in Date and Time

#CALORIES
hourlyCalories <- hourlyCalories %>%
  mutate(
    DateTime = mdy_hms(ActivityHour),
    Date = as.Date(DateTime),
    Time = hms::as_hms(DateTime)
  ) %>%
  select(-ActivityHour, -DateTime) # Remove the original ActivityHour and the intermediate DateTime

#Check for errors
na_count_date <- sum(is.na(hourlyCalories$Date))
na_count_time <- sum(is.na(hourlyCalories$Time))

#STEPS
hourlySteps <- hourlySteps %>%
  mutate(
    DateTime = mdy_hms(ActivityHour),
    Date = as.Date(DateTime),
    Time = hms::as_hms(DateTime)
  ) %>%
  select(-ActivityHour, -DateTime) # Remove the original ActivityHour and the intermediate DateTime

#Check for errors
na_count_date <- sum(is.na(hourlySteps$Date))
na_count_time <- sum(is.na(hourlySteps$Time))

#INTENSITIES
minuteIntensities <- minuteIntensities %>%
  mutate(
      DateTime = mdy_hms(ActivityMinute),
      Date = as.Date(DateTime),
      Time = hms::as_hms(DateTime)
    ) %>%
      select(-ActivityMinute, -DateTime) # Remove the original ActivityMinute and the intermediate DateTime
    
#Check for errors
na_count_date <- sum(is.na(minuteIntensities$Date))
na_count_time <- sum(is.na(minuteIntensities$Time))

#METS
minuteMETs <- minuteMETs %>%
  mutate(
    DateTime = mdy_hms(ActivityMinute),
    Date = as.Date(DateTime),
    Time = hms::as_hms(DateTime)
  ) %>%
  select(-ActivityMinute, -DateTime) # Remove the original ActivityMinute and the intermediate DateTime

#Check for errors
na_count_date <- sum(is.na(minuteMETs$Date))
na_count_time <- sum(is.na(minuteMETs$Time))

# SLEEP
minuteSleep <- minuteSleep %>%
  mutate(
    DateTime = mdy_hms(date),
    Date = as.Date(DateTime),
    Time = hms::as_hms(DateTime)
  ) %>%
  select( -date, -DateTime) # Remove the original date and the intermediate DateTime

#Check for errors
na_count_date <- sum(is.na(minuteSleep$Date))
na_count_time <- sum(is.na(minuteSleep$Time))

# HEART RATE
secondsHeartRate <- secondsHeartRate %>%
  mutate(
    DateTime = mdy_hms(Time),
    Date = as.Date(DateTime),
    Time = hms::as_hms(DateTime)
  ) %>%
  select( -DateTime) # Remove the intermediate DateTime

#Check for errors
na_count_date <- sum(is.na(secondsHeartRate$Date))
na_count_time <- sum(is.na(secondsHeartRate$Time))

# ___ REMOVAL OF DUPLICATE RECORDS BASED ON ID AND TIMESTAMP COMBINATIONS ___
hourlyCalories <- hourlyCalories %>%
  distinct(Id, Date, Time, .keep_all = TRUE)

hourlySteps <- hourlySteps %>%
  distinct(Id, Date, Time, .keep_all = TRUE)

minuteIntensities <- minuteIntensities %>%
  distinct(Id, Date, Time, .keep_all = TRUE)

minuteMETs <- minuteMETs %>%
  distinct(Id, Date, Time, .keep_all = TRUE)

minuteSleep <- minuteSleep %>%
  distinct(Id, Date, Time, .keep_all = TRUE)

secondsHeartRate <- secondsHeartRate %>%
  distinct(Id, Date, Time, .keep_all = TRUE)

# ___ STANDARDIZATION OF TIME REPRESENTATION FOR AGGREGATION OF INTENSITIES, METS, SLEEP AND HEART RATE ___
# Transform Time (HH:MM:SS) to start of hour (HH:00:00)
hourlyCalories <- hourlyCalories %>%
  mutate(Hour = hour(Time))

hourlySteps <- hourlySteps %>%
  mutate(Hour = hour(Time))

minuteIntensities <- minuteIntensities %>%
  mutate(Hour = hour(Time))

minuteMETs <- minuteMETs %>%
  mutate(Hour = hour(Time))

minuteSleep <- minuteSleep %>%
  mutate(Hour = hour(Time))

secondsHeartRate <- secondsHeartRate %>%
  mutate(Hour = hour(Time))

# ___ AGGREGATION OF DATA FOR CONSITENCY ___
# ___           INTENSITIES              ___
# Count occurrences of each intensity level (0, 1, 2, 3) within each hour to determine the total minutes spent at each level.
hourlyIntensities <- minuteIntensities %>%
  group_by(Id, Date, Hour, Intensity) %>%
  summarize(TotalMinutes = n(), .groups = "drop") %>%
  
  pivot_wider(                   # Pivoted into separate columns for each intensity
    id_cols = c(Id, Date, Hour),
    names_from = Intensity,
    values_from = TotalMinutes,
    names_prefix = "Intensity_",
    values_fill = 0) %>%
  
  rename(                        # Rename columns
    SedentaryMinutes = Intensity_0,
    LightlyActiveMinutes = Intensity_1,
    FairlyActiveMinutes = Intensity_2,
    VeryActiveMinutes = Intensity_3
  )

#  ___              METS                 ___
# Correct METs Value
minuteMETs <- minuteMETs %>%     
  mutate(METs = METs / 10)
# Classify METs values into categories
minuteMETs <- minuteMETs %>% 
  mutate(                        
    METs_category = case_when(
      METs <= 1.5 ~ "LowMETs",
      METs >= 1.6 & METs <= 2.9 ~ "LightMETs",
      METs >= 3.0 & METs <= 5.9 ~ "ModerateMETs",
      METs >= 6.0 ~ "HighMETs",
      TRUE ~ NA_character_ # Handle any other cases (if any)  
          )
    )

# Group data by hour
hourlyMETs <- minuteMETs %>%   
  group_by(Id, Date, Hour, METs_category) %>%  
  summarize(minutes = n(), .groups = "drop") %>%
  
  pivot_wider(                  # Pivot METs_category and count minutes
    id_cols = c(Id, Date, Hour),
    names_from = METs_category,
    values_from = minutes,
    values_fill = 0
  )

#SLEEP
# Aggregate the data 
hourlySleep <- minuteSleep %>%
  group_by(Id, Date, Hour, value) %>%  # Group by Id, Date, Hour, and value
  summarize(
    minutes = n(),                    # Count the number of minutes for each group
    sleepRecords = n_distinct(logId), # Count the distinct logId values
    .groups = "drop"
  )

# Pivot the data 
hourlySleep <- hourlySleep %>%
  pivot_wider(
    id_cols = c(Id, Date, Hour),
    names_from = value,       # Use 'value' to create new columns
    values_from = c(minutes, sleepRecords), # Get minutes and distinct logId counts.
    names_prefix = "SleepLevel",     # Prefix for the new columns
    values_fill = 0          # Fill missing combinations with 0
  )

# Remove the row with Date == "2016-03-11" 
hourlySleep <- hourlySleep %>%
  filter(Date != "2016-03-11")

#HEART RATE
# Calculate average heart rate per user, date, and hour
hourlyHeartRate <- secondsHeartRate %>%
  group_by(Id, Date, Hour) %>%
  summarize(avgValue = round(mean(Value, na.rm = TRUE),0), .groups = "drop")




# ___ DAILY AGGREGATION OF DATA FOR ANALYSIS ___
# Calories
dailyCalories <- hourlyCalories %>%
  group_by(Id, Date) %>%
  summarise(
    Calories = sum(Calories, na.rm = TRUE),
    .groups = "drop"
  )

#Steps
dailySteps <- hourlySteps %>%
  group_by(Id, Date) %>%
  summarise(
    StepTotal = sum(StepTotal, na.rm = TRUE),
    .groups = "drop"
  )

#Intensities
dailyIntensities <- hourlyIntensities %>%
  group_by(Id, Date) %>%
  summarise(
    MinutesVeryActive = sum(VeryActiveMinutes, na.rm = TRUE),
    MinutesFairlyActive = sum(FairlyActiveMinutes, na.rm = TRUE),
    MinutesLightlyActive = sum(LightlyActiveMinutes, na.rm = TRUE),
    MinutesSedentary = sum(SedentaryMinutes, na.rm = TRUE),
    .groups = "drop"
  )

#METs
dailyMETs <- hourlyMETs %>%
  group_by(Id, Date) %>%
  summarise(
    MinutesLowMETs = sum(LowMETs, na.rm = TRUE),
    MinutesLightMETs = sum(LightMETs, na.rm = TRUE),
    MinutesModerateMETs = sum(ModerateMETs, na.rm = TRUE),
    MinutesVigorousMETS = sum(HighMETs, na.rm = TRUE),
    .groups = "drop"
  )

#Sleep
dailySleep <- hourlySleep %>%
  group_by(Id, Date) %>%
  summarise(
    MinutesLevel1 = sum(minutes_SleepLevel1, na.rm = TRUE),
    MinutesLevel2 = sum(minutes_SleepLevel2, na.rm = TRUE),
    MinutesLevel3 = sum(minutes_SleepLevel3, na.rm = TRUE),
    .groups = "drop"
  )

#HeartRate
dailyHeartRate <- hourlyHeartRate %>%
  group_by(Id, Date) %>%
  summarize(
    avgHeartRate = round(mean(avgValue, na.rm = TRUE),1),
    .groups = "drop"
  )

# CREATE A DAILY DATASET 'dailyActivity' joining daily aggregated metrics

# join records from Steps and Calories
dailyActivity <- dailySteps %>%
  full_join(dailyCalories, by = c("Id", "Date"))

# join records to combine dailyActivity and Intensities
dailyActivity <- dailyActivity %>%
  full_join(dailyIntensities, by = c("Id", "Date"))

# join records to combine dailyActivity and METs
dailyActivity <- dailyActivity %>%
  full_join(dailyMETs, by = c("Id", "Date"))

# join records to combine dailyActivity and Sleep
dailyActivity <- dailyActivity %>%
  full_join(dailySleep, by = c("Id", "Date"))

# join records to combine dailyActivity and Heart rate
dailyActivity <- dailyActivity %>%
  full_join(dailyHeartRate, by = c("Id", "Date"))