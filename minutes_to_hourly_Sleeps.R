# Load the necessary library
library(lubridate)
library(dplyr) # Load dplyr to use the pipe operator %>%
library(tidyr)

# Step 1: Append minuteSleep1 and minuteSleep2
minuteSleep <- rbind(minuteSleep2, minuteSleep1)

# Step 2:Use parse_date_time to handle multiple possible formats
minuteSleep <- minuteSleep %>%
  mutate(
    DateTime = parse_date_time(date, orders = c("m/d/Y I:M:S p", "d/m/Y I:M:S p", "mdyIMSp", "dmyIMSp"), quiet = FALSE),
    Date = format(DateTime, "%d/%m/%Y"),
    Time = format(DateTime, "%H:%M:%S")
  ) %>%
  select(-date, -DateTime) # Remove the original date and the intermediate DateTime

# Step 3: Remove duplicates based on Id, Date, and Time
minuteSleep <- minuteSleep %>%
  distinct(Id, Date, Time, .keep_all = TRUE)

# Step 4: Extract the hour from Time
minuteSleep <- minuteSleep %>%
  mutate(Hour = format(as.POSIXct(floor_date(as.POSIXct(Time, format = "%H:%M:%S"), "hour")), "%H:%M:%S"))

# 8. Aggregate the data
# Step 5: Aggregate the data as you need
counted_minSleep <- minuteSleep %>%
  group_by(Id, Date, Hour, value) %>%  # Group by Id, Date, Hour, and value
  summarize(
    minutes = n(),                    # Count the number of minutes for each group
    sleepRecords = n_distinct(logId), # Count the distinct logId values
    .groups = "drop"
  )

# Step 6: Pivot the data 
pivoted_sleep <- counted_minSleep %>%
  pivot_wider(
    id_cols = c(Id, Date, Hour),
    names_from = value,       # Use 'value' to create new columns
    values_from = c(minutes, sleepRecords), # Get minutes and distinct logId counts.
    names_prefix = "SleepLevel_",     # Prefix for the new columns
    values_fill = 0          # Fill missing combinations with 0
  )

# Step 8:  Remove the row with Date == "11/03/2016" 
pivoted_sleep <- pivoted_sleep %>%
  filter(Date != "11/03/2016")

# Step 9: Export the data to a CSV file
write.csv(pivoted_sleep, file = "hourlySleep.csv", row.names = FALSE)

