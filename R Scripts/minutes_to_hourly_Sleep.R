# Load the necessary library
library(lubridate)
library(dplyr) 
library(tidyr)

#Import data set into R environment from available csv files on unzipped folders from each time period:
# 12/03/2016 - 11/04/2016: minuteSleep1
# 12/04/2016 - 12/05/2016: minuteSleep2

# Append minuteSleep1 and minuteSleep2
minuteSleep <- rbind(minuteSleep2, minuteSleep1)

# Use parse_date_time to handle multiple possible formats
# Split column date in Date and Time
minuteSleep <- minuteSleep %>%
  mutate(
    DateTime = parse_date_time(date, orders = c("m/d/Y I:M:S p", "d/m/Y I:M:S p", "mdyIMSp", "dmyIMSp"), quiet = FALSE),
    Date = format(DateTime, "%d/%m/%Y"),
    Time = format(DateTime, "%H:%M:%S")
  ) %>%
  select( -date, -DateTime) # Remove the original date and the intermediate DateTime

# Remove duplicates based on Id, Date, and Time
minuteSleep <- minuteSleep %>%
  distinct(Id, Date, Time, .keep_all = TRUE)

# Extract the hour from Time
minuteSleep <- minuteSleep %>%
  mutate(Hour = format(as.POSIXct(floor_date(as.POSIXct(Time, format = "%H:%M:%S"), "hour")), "%H:%M:%S"))

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

# Remove the row with Date == "11/03/2016" 
hourlySleep <- hourlySleep %>%
  filter(Date != "11/03/2016")

# Export the data to a CSV file
write.csv(hourlySleep, file = "hourlySleep.csv", row.names = FALSE)
