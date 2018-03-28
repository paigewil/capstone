library(ggplot2)
library(dplyr)

# Read in file
df_vis <- read.csv("E:/Learning/Springboard Intro to Data Science/capstone/CT_cleaned_edit.csv")

ggplot(df_vis, aes(stop_outcome, fill = is_arrested)) +
  geom_bar()
# stop_outcome has "arrest" so only "arrest" has is_arrested = TRUE

ggplot(df_vis %>% filter(violation_count != 1), aes(violation_count, fill = is_arrested)) +
  geom_bar()

ggplot(df_vis, aes(violation_count, fill = is_arrested)) +
  geom_bar()

ggplot(df_vis, aes(driver_race_raw, fill = is_arrested)) +
  geom_bar(position = "fill")
#hispanics, proportionally, have the most arrests

ggplot(df_vis %>% filter(is.na(stop_outcome) == FALSE), aes(driver_race_raw, fill = stop_outcome)) +
  geom_bar(position = "fill")

ggplot(df_vis, aes(stop_outcome, fill = driver_race_raw)) +
  geom_bar(position = "fill")

ggplot(df_vis, aes(driver_race_raw)) +
  geom_bar()

ggplot(df_vis, aes(driver_age, fill = is_arrested)) +
  geom_histogram(binwidth = 5, position = "fill")

ggplot(df_vis, aes(driver_age, fill = stop_outcome)) +
  geom_bar(position = "fill")

ggplot(df_vis, aes(driver_age, fill = stop_outcome)) +
  geom_histogram(binwidth = 5, position = "fill")
# As you get older, less tickets, more verbal warnings

ggplot(df_vis, aes(driver_gender, fill = is_arrested)) +
  geom_bar(position = "fill")

ggplot(df_vis, aes(driver_gender, fill = stop_outcome)) +
  geom_bar(position = "fill")

ggplot(df_vis, aes(driver_race_raw, fill = stop_outcome, color = driver_gender)) +
  geom_bar(position = "fill", size = 1)
# within each race, men seem to get proportionally more tickets


ggplot(df_vis, aes(county_name, fill = stop_outcome)) +
  geom_bar(position = "fill")
#fairfield has a high number of arrests
ggplot(df_vis, aes(county_name, fill = driver_race_raw)) +
  geom_bar(position = "fill")

ggplot(df_vis, aes(county_name, fill = is_arrested)) +
  geom_bar(position = "fill")

df_vis$viol_greater_1 <- ifelse(df_vis$violation_count > 1, TRUE, FALSE)

ggplot(df_vis, aes(county_name, fill = viol_greater_1)) +
  geom_bar(position = "fill")

ggplot(df_vis, aes(viol_greater_1, fill = is_arrested)) +
  geom_bar(position = "fill")
# no indication that number of violations leads to arrest

ggplot(df_vis, aes(is_arrested, viol_greater_1)) +
  geom_jitter()
