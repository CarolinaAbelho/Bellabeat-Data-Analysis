library(dplyr)
library(tidyr)
library(lubridate)

#Import data set into R environment from available csv files on unzipped folders from each time period:
# 12/03/2016 - 11/04/2016: minIntensities1
# 12/04/2016 - 12/05/2016: minIntensities2

# Append minIntensities1 and minIntensities2
minuteIntensities <- rbind(minIntensities2, minIntensities1)

# Use parse_date_time to handle multiple possible formats
# Split column ActivityMinute in ActivityDate and ActivityTime
minuteIntensities <- minuteIntensities %>%
  mutate(
    ActivityDateTime = parse_date_time(ActivityMinute, orders = c("m/d/Y I:M:S p", "d/m/Y I:M:S p", "mdyIMSp", "dmyIMSp"), quiet = FALSE),
    ActivityDate = format(ActivityDateTime, "%d/%m/%Y"),
    ActivityTime = format(ActivityDateTime, "%H:%M:%S")
  ) %>%
  select(-ActivityMinute, -ActivityDateTime) # Remove the original ActivityMinute and the intermediate ActivityDateTime

# Remove duplicates
minuteIntensities <- minuteIntensities %>%
  distinct(Id, ActivityDate, ActivityTime, .keep_all = TRUE)

# Extract the hour from Time
minuteIntensities <- minuteIntensities %>%
  mutate(ActivityHour = format(as.POSIXct(floor_date(as.POSIXct(ActivityTime, format = "%H:%M:%S"), "hour")), "%H:%M:%S"))

# Aggregate the data
hourlyIntensities <- minuteIntensities %>%
  group_by(Id, ActivityDate, ActivityHour, Intensity) %>%
  summarize(TotalMinutes = n(), .groups = "drop")

# Pivot the data
hourlyIntensities <- hourlyIntensities %>%
  pivot_wider(
    id_cols = c(Id, ActivityDate, ActivityHour),
    names_from = Intensity,
    values_from = TotalMinutes,
    names_prefix = "Intensity_",
    values_fill = 0
  )

# Rename columns
hourlyIntensities <- hourlyIntensities %>%
  rename(
    SedentaryMinutes = Intensity_0,
    LightlyActiveMinutes = Intensity_1,
    FairlyActiveMinutes = Intensity_2,
    VeryActiveMinutes = Intensity_3
  )

# Check for NA values in the final data (and print a helpful message)
na_count_final <- sum(is.na(hourlyIntensities))
if (na_count_final > 0) {
  cat("WARNING: There are", na_count_final, "NA values in the final data frame!\n")
  cat("Please examine the data and the conversion process.\n")
} else {
  cat("No NA values found in the final data frame.\n")
}

# Export the data
write.csv(hourlyIntensities, file = "hourlyIntensities.csv", row.names = FALSE)