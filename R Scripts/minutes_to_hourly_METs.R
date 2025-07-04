# Load the necessary library
library(lubridate)
library(dplyr) 
library(tidyr)

#Import data set into R environment from available csv files on unzipped folders from each time period:
# 12/03/2016 - 11/04/2016: METs1
# 12/04/2016 - 12/05/2016: METs2

# Append METS1 and METS2
minuteMETs <- rbind(METs1, METs2)

# Use parse_date_time to handle multiple possible formats
# Split column ActivityMinute in ActivityDate and ActivityTime
minuteMETs <- minuteMETs %>%
  mutate(
    ActivityDateTime = parse_date_time(ActivityMinute, orders = c("m/d/Y I:M:S p", "d/m/Y I:M:S p", "mdyIMSp", "dmyIMSp"), quiet = FALSE),
    ActivityDate = format(ActivityDateTime, "%d/%m/%Y"),
    ActivityTime = format(ActivityDateTime, "%H:%M:%S")
  ) %>%
  select(-ActivityMinute, -ActivityDateTime) # Remove the original ActivityMinute and the intermediate ActivityDateTime

# Remove duplicates
minuteMETs <- minuteMETs %>%
  distinct(Id, ActivityDate, ActivityTime, .keep_all = TRUE)

# Extract the hour from Time
minuteMETs <- minuteMETs %>%
  mutate(ActivityHour = format(as.POSIXct(floor_date(as.POSIXct(ActivityTime, format = "%H:%M:%S"), "hour")), "%H:%M:%S"))

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
      TRUE ~ NA_character_  # Handle any other cases (if any)
    )
  )

# Group data by hour
# Pivot METs_category and count minutes
hourlyMETs <- minuteMETs %>%
  group_by(Id, ActivityDate, ActivityHour, METs_category) %>%
  summarize(minutes = n(), .groups = "drop") %>%
  pivot_wider(
    id_cols = c(Id, ActivityDate, ActivityHour),
    names_from = METs_category,
    values_from = minutes,
    values_fill = 0
  )

# Order the columns
col_order <- c("Id", "ActivityDate", "ActivityHour", "LowMETs", "LightMETs", "ModerateMETs", "HighMETs")
hourlyMETs <- hourlyMETs %>%
  select(all_of(col_order))

# Check for NA values in the final data (and print a helpful message)
na_count_final <- sum(is.na(hourlyMETs))
if (na_count_final > 0) {
  cat("WARNING: There are", na_count_final, "NA values in the final data frame!\n")
  cat("Please examine the data and the conversion process.\n")
} else {
  cat("No NA values found in the final data frame.\n")
}

# Export the data to a CSV file
write.csv(hourlyMETs, file = "hourlyMETs.csv", row.names = FALSE)