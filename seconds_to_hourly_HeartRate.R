# Load the necessary library
library(lubridate)
library(dplyr) # Load dplyr to use the pipe operator %>%
library(tidyr)

# Step 1: Append minuteSleep1 and minuteSleep2
secondsHeartRate <- rbind(secondHeartRate2, secondHeartRate1)

# Step 2:Use parse_date_time to handle multiple possible formats
secondsHeartRate <- secondsHeartRate %>%
  mutate(
    DateTime = parse_date_time(Time, orders = c("m/d/Y I:M:S p", "d/m/Y I:M:S p", "mdyIMSp", "dmyIMSp"), quiet = FALSE),
    Date = format(DateTime, "%d/%m/%Y"),
    Time = format(DateTime, "%H:%M:%S")
  ) %>%
  select( -DateTime) # Remove the intermediate DateTime

# Step 3: Remove duplicates based on Id, Date, and Time
secondsHeartRate <- secondsHeartRate %>%
  distinct(Id, Date, Time, .keep_all = TRUE)

# Step 4: Extract the hour from Time
secondsHeartRate <- secondsHeartRate %>%
  mutate(Hour = format(as.POSIXct(floor_date(as.POSIXct(Time, format = "%H:%M:%S"), "hour")), "%H:%M:%S"))

# Step 6: Calculate average bpm per user, date, and hour
hourlyHeartRate <- secondsHeartRate %>%
  group_by(Id, Date, Hour) %>%
  summarize(avgValue = round(mean(Value, na.rm = TRUE),0), .groups = "drop")