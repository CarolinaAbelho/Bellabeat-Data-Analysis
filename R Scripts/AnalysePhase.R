# Add a new column 'WEEKDAY' to dailyActivity
dailyActivity <- dailyActivity %>%
  mutate(Weekday = wday(Date, label = TRUE, week_start = 1))

# Calculate ActiveMinutes and add as a new column to dailyActivity ---
dailyActivity <- dailyActivity %>%
  mutate(
    ActiveMinutes = MinutesVeryActive + MinutesFairlyActive + MinutesLightlyActive
  )

#DAILY TOTAL for steps, calories, and minutes in each intensity7MET level from hourly data.
#_______________________________________________________________________________________________________________
#___CALORIES
# Calculate average Calories per hour
calories_expenditure_by_hour <- hourlyCalories %>%
  group_by(Hour) %>%
  summarize(AverageCalories = round(mean(Calories, na.rm = TRUE),0),
            .groups = 'drop') %>%
  arrange(Hour) # Ensure hours are in order

#___STEPS
# Calculate average Steps per hour
steps_per_hour <- hourlySteps %>%
  group_by(Hour) %>%
  summarize(AverageSteps = round(mean(StepTotal, na.rm = TRUE),0),
            .groups = 'drop') %>%
  arrange(Hour) # Ensure hours are in order

#___INTENSITIES
# Calculate total minutes for each intensity level per hour
minutesIntensities_by_hour <- hourlyIntensities %>%
  group_by(Hour) %>%
  summarize(
    VeryActiveMinutes = round(mean(VeryActiveMinutes, na.rm = TRUE),2),
    FairlyActiveMinutes = round(mean(FairlyActiveMinutes, na.rm = TRUE),2),
    LightlyActiveMinutes = round(mean(LightlyActiveMinutes, na.rm = TRUE),2),
    SedentaryMinutes = round(mean(SedentaryMinutes, na.rm = TRUE),2),
  ) %>%
  arrange(Hour)

# --- Reshape data to long format ---
minutes_intensities_long <- minutesIntensities_by_hour %>%
  pivot_longer(
    cols = c(VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes, SedentaryMinutes),
    names_to = "IntensityLevel",
    values_to = "AverageMinutes"
  ) %>%
  # Order Intensity Levels for consistent facet order and fill colors
  mutate(IntensityLevel = factor(IntensityLevel, levels = c("SedentaryMinutes", "LightlyActiveMinutes",
                                                            "FairlyActiveMinutes", "VeryActiveMinutes"),
                                 labels = c("Sedentary", "Lightly Active", "Fairly Active", "Very Active")))


#___METS
# Calculate total minutes for each MET level per hour
minutesMETs_by_hour <- hourlyMETs %>%
  group_by(Hour) %>%
  summarize(
    LowMETs = round(mean(LowMETs, na.rm = TRUE),2),
    LightMETs = round(mean(LightMETs, na.rm = TRUE),2),
    ModerateMETs = round(mean(ModerateMETs, na.rm = TRUE),2),
    HighMETs = round(mean(HighMETs, na.rm = TRUE),2),
  ) %>%
  arrange(Hour)

# --- Reshape data to long format ---
minutes_mets_long <- minutesMETs_by_hour %>%
  pivot_longer(
    cols = c(LowMETs, LightMETs, ModerateMETs, HighMETs),
    names_to = "METLevel",
    values_to = "AverageMinutes"
  ) %>%
  # Order MET Levels for consistent facet order and fill colors
  mutate(METLevel = factor(METLevel, levels = c("LowMETs", "LightMETs", "ModerateMETs", "HighMETs"),
                                 labels = c("Low", "Light", "Moderate", "High")))

#___HEART RATE
# Calculate average HR per hour
heart_rate_by_hour <- hourlyHeartRate %>%
  group_by(Hour) %>%
  summarize(AverageHR = round(mean(avgValue, na.rm = TRUE),0),
            .groups = 'drop') %>%
  arrange(Hour) # Ensure hours are in order

#___HEART RATE
# Calculate total minutes for each Sleep level per hour
minutesSleep_by_hour <- hourlySleep %>%
  group_by(Hour) %>%
  summarize(
    minutes_SleepLevel1 = round(mean(minutes_SleepLevel1, na.rm = TRUE),2),
    minutes_SleepLevel2 = round(mean(minutes_SleepLevel2, na.rm = TRUE),2),
    minutes_SleepLevel3 = round(mean(minutes_SleepLevel3, na.rm = TRUE),2),
  ) %>%
  arrange(Hour)

# --- Reshape data to long format ---
minutes_sleep_long <- minutesSleep_by_hour %>%
  pivot_longer(
    cols = c(minutes_SleepLevel1, minutes_SleepLevel2, minutes_SleepLevel3),
    names_to = "SleepLevel",
    values_to = "AverageMinutes"
  ) %>%
  # Order MET Levels for consistent facet order and fill colors
  mutate(SleepLevel = factor(SleepLevel, levels = c("minutes_SleepLevel3", "minutes_SleepLevel2", "minutes_SleepLevel1"),
                           labels = c("Level3", "Level2", "Level1")))


# CORRELATION: correlation between different activity metrics and calorie expenditure.
#_______________________________________________________________________________________________________________

# Calories burned vs steps
cat("\n Correlation Calories burned vs steps:", cor(dailyActivity$Calories,dailyActivity$StepTotal), "---\n")

# Calories burned vs minutes in intensity "Very Active"
cat("\n Correlation Calories burned vs minutes in intensity 'Very Active':", cor(dailyActivity$Calories,dailyActivity$MinutesVeryActive), "---\n")

# Calories burned vs minutes in MET level 'High'
cat("\n Correlation Calories burned vs minutes in MET level 'High':", cor(dailyActivity$Calories,dailyActivity$MinutesVigorousMETS), "---\n")

# Steps vs minutes in MET Level 'High'
cat("\n Correlation Steps vs minutes in MET Level 'High':", cor(dailyActivity$StepTotal,dailyActivity$MinutesVigorousMETS), "---\n")

# Steps vs minutes in intensity 'Very Active'
cat("\n Correlation Steps vs minutes in intensity 'Very Active':", cor(dailyActivity$StepTotal,dailyActivity$MinutesVeryActive), "---\n")

# minutes in intensity 'Very Active' vs minutes in MET Level 'High'
cat("\n Correlation minutes in intensity 'Very Active' vs minutes in MET 'High':", cor(dailyActivity$MinutesVigorousMETS,dailyActivity$MinutesVeryActive), "---\n")


# ___________________WEEKDAY TOTALS_______________________________________________
# CALORIES
weekdayCalories <- dailyActivity %>%
  group_by(Weekday) %>% 
  summarize(AverageCalories = round(mean(Calories, na.rm = TRUE), 0), .groups = 'drop') %>%
  arrange(factor(Weekday, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))) # Order weekdays

# STEPS
weekdaySteps <- dailyActivity %>%
  group_by(Weekday) %>% 
  summarize(AverageSteps = round(mean(StepTotal, na.rm = TRUE), 0), .groups = 'drop') %>%
  arrange(factor(Weekday, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))) # Order weekdays

# INTENSITIES
weekdayIntensities <- dailyActivity %>%
group_by(Weekday) %>% # Then group by Weekday for averages
  summarize(
    AvgMinVeryActive = round(mean(MinutesVeryActive, na.rm = TRUE), 0),
    AvgMinFairlyActive = round(mean(MinutesFairlyActive, na.rm = TRUE), 0),
    AvgMinLightlyActive = round(mean(MinutesLightlyActive, na.rm = TRUE), 0),
    AvgMinSedentary = round(mean(MinutesSedentary, na.rm = TRUE), 0),
    AvgMinActive = round(mean(ActiveMinutes, na.rm = TRUE), 0),
    .groups = 'drop'
  ) %>%
  arrange(factor(Weekday, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")))

# SLEEP
weekdaySleep <- dailyActivity %>%
  group_by(Weekday) %>% # Then group by Weekday for averages
  summarize(
    AvgMinSleepL1 = round(mean(MinutesLevel1, na.rm = TRUE), 0),
    AvgMinSleepL2 = round(mean(MinutesLevel2, na.rm = TRUE), 0),
    AvgMinSleepL3 = round(mean(MinutesLevel3, na.rm = TRUE), 0),
    .groups = 'drop'
  ) %>%
  arrange(factor(Weekday, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")))


#_______________________________________________________________________________________________________________
# HEART RATE ANALYSIS

# Calculate average Calories per hour
average_hr_by_hour <- hourlyHeartRate %>%
  group_by(Hour) %>%
  summarize(AverageHR = mean(avgValue, na.rm = TRUE))

# Join 'hourlyIntensities' with 'hourlyHeartRate' as the left table
hourly_HR_INT <- hourlyHeartRate %>%
  left_join(hourlyIntensities, by = c("Id", "Hour", "Date"))

#Categorize Intensities 
hourly_HR_INT <- hourly_HR_INT %>%
  mutate(ActivityLevel = case_when(
    VeryActiveMinutes > 0 ~ "Very Active",
    FairlyActiveMinutes > 0 ~ "Fairly Active",
    LightlyActiveMinutes > 0 ~ "Lightly Active",
    SedentaryMinutes > 0 ~ "Sedentary",
    TRUE ~ NA_character_
  ))

#Calculate avg HR in each Intensity level
average_hr_by_activity_level <- hourly_HR_INT %>%
  group_by(ActivityLevel) %>%
  summarize(AverageHR = round(mean(avgValue, na.rm = TRUE),0),
            n_observations = n()) %>%
  filter(!is.na(ActivityLevel))%>%
  # Order Intensity Levels
  mutate(ActivityLevel = factor(ActivityLevel, 
                                levels = c("Sedentary", "Lightly Active","Fairly Active", "Very Active")))
  

#_______________________________________________________________________________________________________________
# SLEEP ANALYSIS

# Calculate total minutes for each intensity level per hour
minutesSleep_by_hour <- hourlySleep %>%
  group_by(Hour) %>%
  summarize(
    minutes_SleepL1 = round(mean(minutes_SleepLevel1, na.rm = TRUE),0),
    minutes_SleepL2 = round(mean(minutes_SleepLevel2, na.rm = TRUE),0),
    minutes_SleepL3 = round(mean(minutes_SleepLevel3, na.rm = TRUE),0),
  ) %>%
  arrange(Hour)

# --- Reshape data to long format for stacking ---
minutes_sleep_long <- minutesSleep_by_hour %>%
  pivot_longer(
    cols = c(minutes_SleepL1, minutes_SleepL2, minutes_SleepL3),
    names_to = "SleepLevel",
    values_to = "AverageMinutes"
  ) %>%
  # Order Sleep Levels for consistent stacking
  mutate(SleepLevel = factor(SleepLevel, levels = c("minutes_SleepL3", "minutes_SleepL2", "minutes_SleepL1"),
                             labels = c("Level 3", "Level 2", "Level 1")))
