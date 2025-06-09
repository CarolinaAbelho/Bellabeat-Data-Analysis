library(ggplot2)

#DAILY TOTALS CHARTS

# Bar chart Average Calories throughout the day
ggplot(data = calories_expenditure_by_hour, 
              aes(x = factor(Hour), y = AverageCalories, fill = AverageCalories)) +
         geom_bar(stat = "identity")+ #use y-value directly
         geom_text(aes(label = round(AverageCalories, 0)),
                   vjust = -1, # Position labels above the bars
                   color = "black", 
                   size = 3) + 
         scale_fill_gradient(low = "#d1e9fa", high = "#1068a4") +
         scale_y_continuous(limits = c(0, 130)) + # Start at 0, expand top slightly
         labs(title = "Average Calories Burned throughout a day",
              subtitle = "Across all users and days",
              x = "Hour of the day",
              y = "Average calories burned") +
         theme_minimal()+
         theme(
           plot.title = element_text(size = 13,  hjust = 0.5),
           plot.subtitle = element_text(size = 9, hjust = 0.5, color = "gray30"),
           axis.title = element_text(size = 10, face = "bold"),
           axis.text.x = element_text(size = 9, angle = 0, vjust = 5), 
           axis.text.y = element_text(size = 10),
           legend.position = "none",  #Remove legend if fill is just for gradient
           panel.grid.major.x = element_blank(), # Remove vertical grid lines for bars
           panel.grid.minor.x = element_blank(),
           panel.grid.major.y = element_line(color = "gray", linetype = "dotted"),
           panel.grid.minor.y = element_blank()
         )
 
 # Bar chart Average Steps throughout the day
ggplot(data = steps_per_hour, 
              aes(x = factor(Hour), y = AverageSteps, fill = AverageSteps)) +
         geom_bar(stat = "identity") + #use y-value directly
         geom_text(aes(label = round(AverageSteps, 0)),
                   vjust = -1, # Position labels above the bars
                   color = "black", 
                   size = 3) + 
         scale_fill_gradient(low = "#fdebd0", high = "#f39c12") +
         scale_y_continuous(limits = c(0, 600)) + # Start at 0, expand top slightly
         labs(title = "Average Steps throughout a day",
              subtitle = "Across all users and days",
              x = "Hour of the day",
              y = "Average Steps") +
         theme_minimal()+
         theme(
           plot.title = element_text(size = 13,  hjust = 0.5),
           plot.subtitle = element_text(size = 9, hjust = 0.5, color = "gray30"),
           axis.title = element_text(size = 10, face = "bold"),
           axis.text.x = element_text(size = 9, angle = 0, vjust = 5), 
           axis.text.y = element_text(size = 9),
           legend.position = "none",  #Remove legend if fill is just for gradient
           panel.grid.major.x = element_blank(), # Remove vertical grid lines for bars
           panel.grid.minor.x = element_blank(),
           panel.grid.major.y = element_line(color = "gray", linetype = "dotted"),
           panel.grid.minor.y = element_blank()
         )
 
# stacked Bar chart Average Minutes in Intensity Level throughout the day 
ggplot(data = minutes_intensities_long, 
       aes(x = factor(Hour), y = AverageMinutes, fill = IntensityLevel)) +
   geom_bar(stat = "identity", position = "stack") +
   scale_fill_manual(
    values = c("Very Active" = "#e74c3c",    
               "Fairly Active" = "#f4d03f", 
               "Lightly Active" = "#2ecc71",
               "Sedentary" = "#2e86c1")) +
   labs(title = "Average Minutes in each Intensity level throughout a day",
       subtitle = "Across all users and days",
       x = "Hour of the day",
       y = "Average Minutes") + 
  theme_minimal() +
   theme(
    plot.title = element_text(size = 13,  hjust = 0.5),
    plot.subtitle = element_text(size = 9, hjust = 0.5, color = "gray30"),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(size = 9, angle = 0, vjust = 5), 
    axis.text.y = element_text(size = 9),
    legend.position = "bottom",  #Remove legend if fill is just for gradient
    panel.grid.major.x = element_blank(), # Remove vertical grid lines for bars
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray", linetype = "dotted"),
    panel.grid.minor.y = element_blank()
  )

# 100% stacked Bar chart Average Minutes in Intensity Level throughout the day 
ggplot(
  data = minutes_mets_long,
  aes(x = factor(Hour), y = AverageMinutes, fill = METLevel)) +
  geom_bar(stat = "identity", position = "fill") + # position = "fill" is key
  scale_y_continuous(labels = scales::percent) + # Y-axis as percentage
  scale_fill_manual(
    values = c("High" = "#e74c3c",    
               "Moderate" = "#f4d03f", 
               "Light" = "#2ecc71",
               "Low" = "#2e86c1")) +
  labs(
    title = "Proportion of time in different MET Levels throughout a day",
    subtitle = "Across all users and days",
    x = "Hour of the day",
    y = "Proportion of time",
    fill = "MET Level"
  ) +
  theme_minimal() + 
  theme(
    plot.title = element_text(size = 13,  hjust = 0.5),
    plot.subtitle = element_text(size = 9, hjust = 0.5, color = "gray30"),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(size = 9, angle = 0, vjust = 5), 
    axis.text.y = element_text(size = 9),
    legend.position = "bottom",
    panel.grid.major.x = element_blank(), # Remove vertical grid lines for bars
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray", linetype = "dotted"),
    panel.grid.minor.y = element_blank()
  ) 

#Line chart for HR throughout the day
ggplot(data = heart_rate_by_hour, aes(x = factor(Hour), y = AverageHR, group = 1)) +
  geom_line(color = "#e74c3c", size = 1.2) +
  geom_point(color = "#e74c3c", size = 3, shape = 21, fill = "#e74c3c") +
  geom_text(aes(label = AverageHR),
            vjust = -1, # Position labels above the bars
            color = "#e74c3c", 
            size = 3) + 
  scale_y_continuous(limits = c(60, 90)) + # Start at 0, expand top slightly
  labs(title = "Average Heart Rate throughout a day",
       subtitle = "Across all users and days",
       x = "Hour of the Day",
       y = "Average Heart Rate (bpm)") +
  theme_minimal() + 
  theme(
    plot.title = element_text(size = 13,  hjust = 0.5),
    plot.subtitle = element_text(size = 9, hjust = 0.5, color = "gray30"),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(size = 9, angle = 0, vjust = 0), 
    axis.text.y = element_text(size = 9),
    legend.position = "bottom",
    panel.grid.major.x = element_line(color = "gray", linetype = "dotted"), # Remove vertical grid lines for bars
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray", linetype = "dotted"),
    panel.grid.minor.y = element_blank()
  ) 


# stacked Bar chart Average Minutes in each Sleep Level throughout the day 
ggplot(data = minutes_sleep_long, 
       aes(x = factor(Hour), y = AverageMinutes, fill = SleepLevel)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(
    values = c("Level 3" = "#8e44ad",    
               "Level 2" = "#bb8fce", 
               "Level 1" = "#d2b4de")) +
  labs(title = "Average Minutes in each Sleep level throughout a 24h cycle",
       subtitle = "Across all users and days",
       x = "Hour of the day",
       y = "Average Minutes") + 
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13,  hjust = 0.5),
    plot.subtitle = element_text(size = 9, hjust = 0.5, color = "gray30"),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(size = 9, angle = 0, vjust = 5), 
    axis.text.y = element_text(size = 9),
    legend.position = "bottom",  #Remove legend if fill is just for gradient
    panel.grid.major.x = element_blank(), # Remove vertical grid lines for bars
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray", linetype = "dotted"),
    panel.grid.minor.y = element_blank()
  )


#HEATMAP ACTIVITY INTENSITY ACROSS THE WEEK
weekday_hourly_active_minutes <- hourlyIntensities %>%
  mutate(
    HourlyActiveMinutes = VeryActiveMinutes + FairlyActiveMinutes + LightlyActiveMinutes,
    Weekday = wday(Date, label = TRUE, abbr = FALSE), # Add Weekday
    # Ensure Weekday is a factor with desired order
    Weekday = factor(Weekday, levels = c("Sunday", "Saturday", "Friday", "Thursday", "Wednesday", "Tuesday", "Monday"))
  ) %>%
  group_by(Hour, Weekday) %>% # Group by both Hour and Weekday
  summarize(
    AverageHourlyActiveMinutes = round(mean(HourlyActiveMinutes, na.rm = TRUE), 0),
    .groups = 'drop'
  ) %>%
  arrange(Hour, Weekday) # Ensure data is ordered for consistency

#heatmap
ggplot(data = weekday_hourly_active_minutes, aes(x = factor(Hour), y = Weekday, fill = AverageHourlyActiveMinutes)) +
  geom_tile() + # This creates the tiles
  geom_text(aes(label = AverageHourlyActiveMinutes), # Position labels above the bars
            color = "white", 
            size = 3) +
  scale_fill_gradient(low = "#f9e79f", high = "#cb4335") + # Assigns colors based on Value
  labs(title = "Average active minutes per hour across each day of the week",
       subtitle = "Across all users and days",
       x = "Hour of the day",
       y = "Weekday") + 
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13,  hjust = 0.5),
    plot.subtitle = element_text(size = 9, hjust = 0.5, color = "gray30"),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(size = 9, angle = 0, vjust = 5), 
    axis.text.y = element_text(size = 9),
    legend.position = "none",  
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )

# bar Chart for WEEKDAY analysis
#STEPS
ggplot(data = weekdaySteps, 
       aes(x = Weekday, y = AverageSteps, fill = AverageSteps)) +
  geom_bar(stat = "identity") + #use y-value directly
  geom_text(aes(label = AverageSteps),
            vjust = 2, # Position labels 
            color = "black", 
            size = 3) + 
  scale_fill_gradient(low = "#fdebd0", high = "#f39c12") +
  labs(title = "Average daily step by weekday",
       x = "Weekday",
       y = "Average Daily Steps") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 13,  hjust = 0.5),
    plot.subtitle = element_text(size = 9, hjust = 0.5, color = "gray30"),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(size = 9, angle = 0, vjust = 5), 
    axis.text.y = element_text(size = 9),
    legend.position = "none",  #Remove legend if fill is just for gradient
    panel.grid.major.x = element_blank(), # Remove vertical grid lines for bars
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray", linetype = "dotted"),
    panel.grid.minor.y = element_blank()
  )


#INTENSITY
# --- Reshape data to long format for Dodging ---
intensities_long_weekday <- weekdayIntensities %>%
  pivot_longer(
    cols = starts_with("Avg"),
    names_to = "IntensityLevel",
    values_to = "AverageMinutes"
  ) %>%
  # Improve Intensity Level names
  mutate(IntensityLevel = case_when(
    IntensityLevel == "AvgMinVeryActive" ~ "Very Active",
    IntensityLevel == "AvgMinFairlyActive" ~ "Fairly Active",
    IntensityLevel == "AvgMinLightlyActive" ~ "Lightly Active",
    IntensityLevel == "AvgMinSedentary" ~ "Sedentary",
   # TRUE ~ IntensityLevel # Keep original if no match
  )) %>%
  # Order Intensity Levels
  mutate(IntensityLevel = factor(IntensityLevel, levels = c("Sedentary", "Lightly Active","Fairly Active", "Very Active"))) %>%
  filter(!is.na(IntensityLevel))

#CHART
ggplot(intensities_long_weekday, aes(x = Weekday, y = AverageMinutes, fill = IntensityLevel)) +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_text(aes(label = AverageMinutes), # Add data labels
            position = position_dodge(width = 0.9), # Align labels with dodged bars
            vjust = -0.5, # Position labels slightly above the bars
            color = "black",
            size = 2.5) + # Adjusted size for readability
  scale_fill_manual(
    values = c("Very Active" = "#d62728",    # Red
               "Fairly Active" = "#ff7f0e",  # Orange
               "Lightly Active" = "#2ca02c", # Green
               "Sedentary" = "#1f77b4")     # Blue
  ) +
  scale_y_continuous(limits = c(0, 1300)) + # Start at 0, expand top slightly
  labs(
    title = "Average Daily Minutes per Intensity Level by Weekday",
    subtitle = "Aggregated across all users",
    x = "Day of the Week",
    y = "Average Daily Minutes",
    fill = "Intensity Level"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13,  hjust = 0.5),
    plot.subtitle = element_text(size = 9, hjust = 0.5, color = "gray30"),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(size = 9, angle = 0, vjust = 5), 
    axis.text.y = element_text(size = 9),
    legend.position = "bottom",  #Remove legend if fill is just for gradient
    panel.grid.major.x = element_blank(), # Remove vertical grid lines for bars
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray", linetype = "dotted"),
    panel.grid.minor.y = element_blank()
  )

#SLEEP
# --- Reshape data to long format for Dodging ---
sleep_long_weekday <- weekdaySleep %>%
  pivot_longer(
    cols = starts_with("Avg"),
    names_to = "SleepLevel",
    values_to = "AverageMinutes"
  ) %>%
  # Improve Intensity Level names
  mutate(SleepLevel = case_when(
    SleepLevel == "AvgMinSleepL1" ~ "Level 1",
    SleepLevel == "AvgMinSleepL2" ~ "Level 2",
    SleepLevel == "AvgMinSleepL3" ~ "Level 3"
  )) %>%
  # Order Intensity Levels
  mutate(SleepLevel = factor(SleepLevel, levels = c("Level 1", "Level 2","Level 3"))) %>%
  filter(!is.na(SleepLevel))


#PLOT
ggplot(data = sleep_long_weekday, 
       aes(x = Weekday, y = AverageMinutes, fill = SleepLevel)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = AverageMinutes), # Add data labels
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            color = "black",
            size = 2.5) +
  scale_fill_manual(
    values = c("Level 1" = "#d2b4de",    
               "Level 2" = "#bb8fce", 
               "Level 3" = "#8e44ad")) +
  labs(title = "Average Minutes in each Sleep level by weekday",
       subtitle = "Across all users",
       x = "Weekday",
       y = "Average Minutes") + 
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13,  hjust = 0.5),
    plot.subtitle = element_text(size = 9, hjust = 0.5, color = "gray30"),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(size = 9, angle = 0, vjust = 5), 
    axis.text.y = element_text(size = 9),
    legend.position = "bottom",  #Remove legend if fill is just for gradient
    panel.grid.major.x = element_blank(), # Remove vertical grid lines for bars
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray", linetype = "dotted"),
    panel.grid.minor.y = element_blank()
  )



#CORRELATIONS
#Correlation minutes in intensity 'Very Active' vs minutes in MET 'High'
ggplot(data= dailyActivity, aes(x= MinutesVigorousMETS, y=MinutesVeryActive)) + 
  geom_point(color="purple") + 
  geom_smooth(color = "darkblue")+
  labs(title="Minutes in High METs vs VeryActive Minutes", x="High METs (Minutes)", y="Very Active Intensity (minutes)")+
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13,  hjust = 0.5),
    plot.subtitle = element_text(size = 9, hjust = 0.5, color = "gray30"),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(size = 9, angle = 0, vjust = 5), 
    axis.text.y = element_text(size = 9),
    legend.position = "bottom",  #Remove legend if fill is just for gradient
    panel.grid.major.x = element_blank(), # Remove vertical grid lines for bars
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray", linetype = "dotted"),
    panel.grid.minor.y = element_blank()
  )


# Calorie Expenditure vs VeryActive Minutes
ggplot(data= dailyActivity, aes(x= Calories, y=MinutesVeryActive)) + 
  geom_point(color="Orange") + 
  geom_smooth(color = "darkred")+
  labs(title="Calorie Expenditure vs VeryActive Minutes", x="Calories burned (kcal)", y="Very Active Intensity (minutes)")+
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13,  hjust = 0.5),
    plot.subtitle = element_text(size = 9, hjust = 0.5, color = "gray30"),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(size = 9, angle = 0, vjust = 5), 
    axis.text.y = element_text(size = 9),
    legend.position = "bottom",  #Remove legend if fill is just for gradient
    panel.grid.major.x = element_blank(), # Remove vertical grid lines for bars
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray", linetype = "dotted"),
    panel.grid.minor.y = element_blank()
  )


#HEART RATE VARIABILITY IN RELATION TO ACTIVITY LEVELS
# BAR CHART for AVG HR by ACTIVITY LEVEL
ggplot(average_hr_by_activity_level, aes(x = ActivityLevel, y = AverageHR, fill = ActivityLevel)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = AverageHR), # Add data labels
            vjust = -0.5, # Position labels slightly above the bars
            color = "black",
            size = 2.5) + # Adjusted size for readability
  scale_fill_manual(
    values = c("Very Active" = "#d62728",    # Red
               "Fairly Active" = "#ff7f0e",  # Orange
               "Lightly Active" = "#2ca02c", # Green
               "Sedentary" = "#1f77b4")     # Blue
  ) +
  scale_y_continuous(limits = c(0, 100)) + # Start at 0, expand top slightly
  labs(title = "Average Heart Rate by Activity Level",
       x = "Activity Level",
       y = "Average Heart Rate (bpm)",
       fill = "Activity Level") +
  theme_minimal() + 
  theme(
   plot.title = element_text(size = 13,  hjust = 0.5),
    plot.subtitle = element_text(size = 9, hjust = 0.5, color = "gray30"),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(size = 9, angle = 0, vjust = 5), 
    axis.text.y = element_text(size = 9),
    legend.position = "bottom",  #Remove legend if fill is just for gradient
    panel.grid.major.x = element_blank(), # Remove vertical grid lines for bars
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray", linetype = "dotted"),
    panel.grid.minor.y = element_blank()
)