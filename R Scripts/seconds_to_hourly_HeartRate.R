# Load the necessary libraries
library(lubridate)
library(dplyr) 
library(tidyr)

#Import data set into R environment from available csv files on unzipped folders from each time period:
# 12/03/2016 - 11/04/2016: secondHeartRate1
# 12/04/2016 - 12/05/2016: secondHeartRate2

# Append secondHeartRate1 and secondHeartRate2
secondsHeartRate <- rbind(secondHeartRate2, secondHeartRate1)

# Use parse_date_time to handle multiple possible formats
# Split column Time in Date and Time
secondsHeartRate <- secondsHeartRate %>%
  mutate(
    DateTime = parse_date_time(Time, orders = c("m/d/Y I:M:S p", "d/m/Y I:M:S p", "mdyIMSp", "dmyIMSp"), quiet = FALSE),
    Date = format(DateTime, "%d/%m/%Y"),
    Time = format(DateTime, "%H:%M:%S")
  ) %>%
  select( -DateTime) # Remove the intermediate DateTime

# Remove duplicates based on Id, Date, and Time
secondsHeartRate <- secondsHeartRate %>%
  distinct(Id, Date, Time, .keep_all = TRUE)

# Extract the hour from Time
secondsHeartRate <- secondsHeartRate %>%
  mutate(Hour = format(as.POSIXct(floor_date(as.POSIXct(Time, format = "%H:%M:%S"), "hour")), "%H:%M:%S"))

# Calculate average heart rate per user, date, and hour
hourlyHeartRate <- secondsHeartRate %>%
  group_by(Id, Date, Hour) %>%
  summarize(avgValue = round(mean(Value, na.rm = TRUE),0), .groups = "drop")

# Export the data to a CSV file
write.csv(hourlyHeartRate, file = "hourlyHeartRate.csv", row.names = FALSE)