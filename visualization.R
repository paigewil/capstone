library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(plyr)
library(ggpubr)
library(gridExtra)

# Read in file
df_clean <- read.csv("E:/Learning/Springboard Intro to Data Science/capstone/CT_cleaned_edit.csv")
df_split <- read.csv("E:/Learning/Springboard Intro to Data Science/capstone/CT_cleaned_split.csv")

#ADDING EXTERNAL DATA SOURCES:
census <- read.csv("E:/Learning/Springboard Intro to Data Science/capstone/cc-est2016-alldata-09.csv")
census$COUNTY <- as.integer(census$COUNTY + 9000)
names(census)[3] <- "county_fips"

# Only want numbers from 2014, since only full year in our dataset and can't get
# partial year information for 2013 & 2015 to average everything out.
census <- census %>% filter(YEAR == 7)
census_county <- census %>% filter(AGEGRP == 0)
census_agegrp <- census %>% group_by(AGEGRP) %>% dplyr::summarise_at(vars(TOT_POP:HNAC_FEMALE), sum)
census_whole_state <- census %>% filter(AGEGRP == 0) %>% group_by(STATE, STNAME) %>% dplyr::summarise_at(vars(TOT_POP:HNAC_FEMALE), sum)

df_county <- left_join(df_clean, census_county, by = "county_fips")


theme1 <- theme(axis.text.x=element_text(angle=90, hjust=1))

#ONE-DIMENSIONAL DISTRIBUTIONS:

#stop_date:
#df_clean$stop_date <- as.POSIXct(df_clean$stop_date, "%m/%d/%Y", tz = "America/New_York")
df_clean$stop_date <- as.POSIXct(df_clean$stop_date, "%Y-%m-%d", tz = "America/New_York")
ggplot(df_clean, aes(stop_date)) + geom_bar(fill = "#1E90FF")  +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%m/%Y") +
  theme1 + 
  labs(title = "Police stops per day (10/1/2013 - 03/31/2015)", x = "Stop Date")
ggsave("./EDA_images/stop_date_bar.png")
ggplot(df_clean, aes(stop_date, ..prop.., group = 1)) + 
  geom_bar(fill = "#1E90FF")  +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%m/%Y") +
  theme1 + 
  labs(title = "Proportion of police stops per day (10/1/2013 - 03/31/2015)", x = "Stop Date", y = "Proportion")
ggsave("./EDA_images/stop_date_bar_prop.png")
df_date <- setNames(data.frame(table(df_clean$stop_date)),c("StopDate","Count"))
df_date$StopDate <- as.POSIXct(df_date$StopDate, "%Y-%m-%d", tz = "America/New_York")
ggplot(df_date, aes(x = StopDate, y = Count)) + 
  geom_bar(stat = "identity", fill = "#1E90FF")  +
  geom_smooth(se = FALSE, color = "red") + 
  scale_x_datetime(date_breaks = "1 month", date_labels = "%m/%Y") +
  theme1 + 
  labs(title = "Count of police stops per day (10/1/2013 - 03/31/2015)", x = "Stop Date")
ggsave("./EDA_images/stop_date_bar_prop_w_loess.png")
# line plot
# find number of dates so one bin per date
no_dates <- as.numeric(range(df_clean$stop_date)[2] - range(df_clean$stop_date)[1])
ggplot(df_clean, aes(stop_date)) + 
  geom_line(stat = "bin", bins = no_dates, color = "#1E90FF") +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%m/%Y") +
  theme1 + 
  labs(title = "Police stops per day (10/1/2013 - 03/31/2015)", x = "Stop Date")
ggsave("./EDA_images/stop_date_line1.png")
#OR
df_stop_date <- setNames(data.frame(table(df_clean$stop_date)),c("Stop.Date","Count"))
df_stop_date$Stop.Date <- as.POSIXct(df_stop_date$Stop.Date, "%Y-%m-%d", tz = "America/New_York")
ggplot(df_stop_date, aes(x = Stop.Date, y = Count, group = 1)) + geom_line(color = "#1E90FF") +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%m/%Y") +
  theme1 +
  labs(title = "Police stops per day (10/1/2013 - 03/31/2015)", x = "Stop Date")
ggsave("./EDA_images/stop_date_line2.png")
ggplot(df_stop_date, aes(x = Stop.Date, y = Count, group = 1)) + 
  geom_line(color = "#1E90FF") +
  geom_smooth(se = FALSE, color = "red") +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%m/%Y") +
  theme1 +
  labs(title = "Police stops per day (10/1/2013 - 03/31/2015)", x = "Stop Date")
ggsave("./EDA_images/stop_date_line2_w_loess.png")
df_stop_date <- df_stop_date %>% mutate(stop_date_prop = Count/sum(Count))
ggplot(df_stop_date, aes(x = Stop.Date, y = stop_date_prop)) + 
  geom_line(color = "#1E90FF") +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%m/%Y") +
  theme1 +
  labs(title = "Proportion of police stops per day (10/1/2013 - 03/31/2015)", x = "Stop Date", y = "Proportion")
ggsave("./EDA_images/stop_date_line2_prop.png")
# Notes: There are a low number of stops around 02/2014 and 02/2015.
#        There are a higher number of stops around 05/2014 and 07/2014
#        Similar (yet smaller) peaks in December followed by a decrease and then increase in March.
#        In general, it seems like there are less stops around the end of the year/
#        beginning of the new year, and more stops over the summer.
# Specific dates of highs and lows:
head(df_stop_date %>% arrange(desc(Count)),10)
#   Stop.Date Count
# 1  2014-07-03  1532
# 2  2014-05-23  1490
# 3  2014-08-29  1380
# 4  2014-07-05  1328
# 5  2014-08-30  1295
# 6  2014-09-23  1271
# 7  2014-05-24  1179
# 8  2014-05-25  1166
# 9  2014-09-12  1107
# 10 2014-04-25  1083
head(df_stop_date %>% arrange(Count),10)
# Stop.Date Count
# 1  2015-02-02    47
# 2  2015-02-09    71
# 3  2015-02-15    82
# 4  2014-12-25    89
# 5  2014-02-13    96
# 6  2014-02-05   106
# 7  2015-01-27   115
# 8  2013-12-25   121
# 9  2014-12-09   132
# 10 2014-01-03   136
#Notes: Two of the lowest stop dates are Christmas and the day after New Year's,
#       which might correlate to a low number of people being out on the roads.
#       No other dates seem to fall on/around a holiday.
#       The day before and after the 4th of July have high stop counts.
#       Otherwise, holidays don't seem to be a huge indicator of stop likelihood.

# month
df_clean$stop_month <- month(df_clean$stop_date)
df_clean$stop_month <- factor(df_clean$stop_month, 
                              ordered = TRUE,
                              levels = as.character(1:12))
ggplot(df_clean, aes(stop_month)) + 
  geom_bar(fill = "#1E90FF") +
  labs(title = "Stops per month (total count)", x= "Stop Month")
ggsave("./EDA_images/stop_month_total.png")

# month: proportion
months <- c(2,2,2,1,1,1,1,1,1,2,2,2)
# -> occurence of each month in dataset
months_df <- setNames(data.frame(factor(as.character(1:12), levels = as.character(1:12)), months), c("month", "occurrence"))
table_months <- setNames(data.frame(table(df_clean$stop_month)),c("month", "count"))
table_months$count <- as.numeric(table_months$count)
months_df <- left_join(table_months, months_df, by = "month")
months_df <- months_df %>% mutate(month_prop = count/occurrence)
ggplot(months_df, aes(month, month_prop)) + 
  geom_bar(stat = "identity", fill = "#1E90FF") +
  labs(title = "Stops per month (by proportion of months in dataset)", x = "Stop Month", y = "# of stops/month")
ggsave("./EDA_images/stop_month_prop.png")
# Notes: The data set goes from 10/2013 - 03/2015 so the raw counts might be skewed
#        heavily towards months 10, 11, 12, 1, 2, 3 which appear across multiple
#        years.
#        Dividing the counts by the number of years a month is present in the data set
#        gives a more proportioned perspective. With this, we can see that May, August, 
#        and September have the highest stops while February has the least.
#        This aligns with what we saw in stop_date plots.

# month and year
#   To help solve the month only problem.
mo_yr_label <- seq(as.Date("2013/10/1"), as.Date("2015/3/31"), by = "month")
mo_yr_label <- format(mo_yr_label, "%m-%Y")
df_clean$month_year <- format(df_clean$stop_date, "%m-%Y")
df_clean$month_year <- factor(df_clean$month_year,
                              ordered = TRUE,
                              levels = mo_yr_label)
ggplot(df_clean, aes(month_year)) + 
  geom_bar(fill = "#1E90FF") + 
  theme1 + 
  labs(title = "Stops per month/year", x = "Stop Month/Year")
ggsave("./EDA_images/stop_month_year.png")
ggplot(df_clean, aes(month_year, ..prop.., group = 1)) + 
  geom_bar(fill = "#1E90FF") + 
  theme1 + 
  labs(title = "Stop proportions per month/year", x = "Stop Month/Year", y = "Proportion")
ggsave("./EDA_images/stop_month_year_prop.png")
# Notes: The lowest months are 02/2014 and 02/2015, while the highest months are
#        05/2014 and 08/2014, which is consitent with the full date analysis.

# year
df_clean$stop_year <- year(df_clean$stop_date)
df_clean$stop_year <- factor(df_clean$stop_year, 
                              ordered = TRUE,
                              levels = c("2013", "2014", "2015"))
ggplot(df_clean, aes(stop_year)) + 
  geom_bar(fill = "#1E90FF") +
  labs(title = "Stops per year (data from 10/2013 - 3/2015)", x = "Year")
ggsave("./EDA_images/stop_year.png")
# Notes: More stops in 2014 because the whole years of 2013 and 2015 aren't recorded.

# day
a <- c("01", "02", "03", "04", "05", "06", "07", "08", "09")
b <- as.character(10:31)
days_label <- c(a, b)
df_clean$day_of_month <- format(df_clean$stop_date, "%d")
df_clean$day_of_month <- factor(df_clean$day_of_month,
                              ordered = TRUE,
                              levels = days_label)
ggplot(df_clean, aes(day_of_month)) + 
  geom_bar(fill = "#1E90FF") + 
  theme1 +
  labs(title = "Stops by day of month (total count)", x = "Day of Month")
ggsave("./EDA_images/stop_day_total.png")

# day: proportion
days_prop <- c(rep(c(18,16), times = c(28, 2)), 11)
days_df <- setNames(data.frame(factor(days_label, levels = days_label), days_prop), c("day", "occurrence"))
table_days <- setNames(data.frame(table(df_clean$day_of_month)),c("day", "count"))
table_days$count <- as.numeric(table_days$count)
days_df <- left_join(table_days, days_df, by = "day")
days_df <- days_df %>% mutate(day_prop = count/occurrence)
ggplot(days_df, aes(day, day_prop)) + 
  geom_bar(stat = "identity", fill = "#1E90FF") + 
  theme1 +
  labs(title = "Stops by day of month: \nRatio of count to occurence of days in year", x = "Day of Month", y = "Ratio Count")
ggsave("./EDA_images/stop_day_prop.png")
ggplot(days_df, aes(as.numeric(day), day_prop)) + 
  geom_bar(stat = "identity", fill = "#1E90FF") + 
  geom_smooth(method = "lm", se = FALSE, color = "red") + 
  theme1 +
  labs(title = "Stops by day of month: \nRatio of count to occurence of days in year w best fit line", x = "Day of Month", y = "Ratio Count")
ggsave("./EDA_images/stop_day_prop_w_lm.png")
# loess model shows the same trend
ggplot(days_df, aes(as.numeric(day), day_prop)) + 
  geom_bar(stat = "identity", fill = "#1E90FF") + 
  geom_smooth(se = FALSE, color = "red") + 
  theme1 +
  labs(title = "Stops by day of month: \nRatio of count to occurence of days in year w LOESS", x = "Day of Month", y = "Ratio Count")
ggsave("./EDA_images/stop_day_prop_w_loess.png")
# Notes: Looking at the proportion, the end of the month seems to have more stops
#        than the beginning.
#        Adding the linear model line confirms that the number of stops trends
#        towards the end of the year.

#day of the week
df_clean$day_of_week <- weekdays(df_clean$stop_date)
df_clean$day_of_week <- factor(df_clean$day_of_week, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
ggplot(df_clean, aes(day_of_week)) + 
  geom_bar(fill = "#1E90FF") +
  theme1 +
  labs(title = "Stops by day of week", x = "Day of Week")
ggsave("./EDA_images/stop_day_of_the_week.png")
ggplot(df_clean, aes(day_of_week, ..prop.., group = 1)) + 
  geom_bar(fill = "#1E90FF") +
  theme1 +
  labs(title = "Stop proportions by day of week", x = "Day of Week", y = "Proportion")
ggsave("./EDA_images/stop_day_of_the_week_prop.png")
#Notes: More stops happen towards the end of the week (Friday and Saturday) with
#       the least happening on Sunday.

#stop_time:
ggplot(df_clean %>% filter(stop_time != "0:00"), aes(stop_time)) + 
  geom_bar(fill = "#1E90FF") +
  labs(title = "Stop times by count", x = "Stop Time")
ggsave("./EDA_images/stop_time.png")

# hour only; remove 0:00 since is a NA-filled value
df_clean$stop_time_hour <- df_clean$stop_time
df_clean$stop_time_hour[df_clean$stop_time_hour == "0:00"] <- NA
df_clean$stop_time_hour <- sub(":.*", "", df_clean$stop_time_hour)
df_clean$stop_time_hour <- factor(df_clean$stop_time_hour, 
                                  ordered = TRUE,
                                  levels = as.character(0:23))
df_clean$stop_time_hour_numeric <- as.numeric(df_clean$stop_time_hour) - 1
# http://www.learnersdictionary.com/qa/parts-of-the-day-early-morning-late-morning-etc
df_clean$stop_hour_part_of_day <- vector(mode = "character", length = nrow(df_clean))
df_clean$stop_hour_part_of_day[df_clean$stop_time_hour_numeric >= 5 & df_clean$stop_time_hour_numeric < 12] <- "Morning"
df_clean$stop_hour_part_of_day[df_clean$stop_time_hour_numeric >= 12 & df_clean$stop_time_hour_numeric < 17] <- "Afternoon"
df_clean$stop_hour_part_of_day[df_clean$stop_time_hour_numeric >= 17 & df_clean$stop_time_hour_numeric < 21] <- "Evening"
df_clean$stop_hour_part_of_day[(df_clean$stop_time_hour_numeric >= 21 & df_clean$stop_time_hour_numeric <= 23) | (df_clean$stop_time_hour_numeric >= 0 & df_clean$stop_time_hour_numeric < 5)] <- "Night"
df_clean$stop_hour_part_of_day <- factor(df_clean$stop_hour_part_of_day,
                                         ordered = TRUE,
                                         levels = c("Morning", "Afternoon", "Evening", "Night"))
ggplot(df_clean %>% filter(is.na(stop_time_hour) != TRUE), aes(stop_time_hour)) + 
  geom_bar(fill = "#1E90FF") +
  labs(title = "Stop times (hour) by count", x = "Stop Time (Hour)")
ggsave("./EDA_images/stop_hour.png")
ggplot(df_clean %>% filter(is.na(stop_time_hour) != TRUE), aes(stop_time_hour, ..prop.., group = 1)) + 
  geom_bar(fill = "#1E90FF") +
  labs(title = "Stop times (hour) by proportion", x = "Stop Time (Hour)", y = "Proportion")
ggsave("./EDA_images/stop_hour_prop.png")
ggplot(df_clean %>% filter(is.na(stop_time_hour) != TRUE), aes(stop_hour_part_of_day, ..prop.., group = 1)) + 
  geom_bar(fill = "#1E90FF") +
  labs(title = "Stop proportion by parts of the day", x = "Part of Day", y = "Proportion") +
  scale_x_discrete(labels = c("Morning\n(0500 - 1159)", "Afternoon\n(1200 - 1659)", "Evening\n(1700 - 2059)", "Night\n(2100 - 0459)"))
ggsave("./EDA_images/stop_hour_prop_part_of_day.png")
ggplot(df_clean %>% filter(is.na(stop_time_hour) != TRUE), aes(day_of_week, ..count../sum(..count..))) + 
  geom_bar(fill = "#1E90FF") +
  facet_grid(stop_hour_part_of_day ~ .) +
  labs(title = "Stop proportion by parts of the day and days of the week", x = "Day of the Week", y = "Proportion")
ggsave("./EDA_images/stop_hour_part_of_day_day_of_week_prop.png")
df_clean$stop_hour_part_of_day2 <- vector(mode = "character", length = nrow(df_clean))
df_clean$stop_hour_part_of_day2[df_clean$stop_time_hour_numeric >= 0 & df_clean$stop_time_hour_numeric < 5] <- "[0000, 0500)"
df_clean$stop_hour_part_of_day2[df_clean$stop_time_hour_numeric >= 5 & df_clean$stop_time_hour_numeric < 10] <- "[0500, 1000)"
df_clean$stop_hour_part_of_day2[df_clean$stop_time_hour_numeric >= 10 & df_clean$stop_time_hour_numeric < 15] <- "[1000, 1500)"
df_clean$stop_hour_part_of_day2[df_clean$stop_time_hour_numeric >= 15 & df_clean$stop_time_hour_numeric < 20] <- "[1500, 2000)"
df_clean$stop_hour_part_of_day2[df_clean$stop_time_hour_numeric >= 20 & df_clean$stop_time_hour_numeric <= 23] <- "[2000, 2400)"
df_clean$stop_hour_part_of_day2 <- factor(df_clean$stop_hour_part_of_day2,
                                         ordered = TRUE,
                                         levels = c("[0000, 0500)", "[0500, 1000)", "[1000, 1500)", "[1500, 2000)", "[2000, 2400)"))
ggplot(df_clean %>% filter(is.na(stop_time_hour) != TRUE), aes(stop_hour_part_of_day2, ..prop.., group = 1)) + 
  geom_bar(fill = "#1E90FF") +
  labs(title = "Stop proportion by parts of the day", x = "Part of Day", y = "Proportion")
ggsave("./EDA_images/stop_hour_prop_part_of_day2.png")
ggplot(df_clean %>% filter(is.na(stop_time_hour) != TRUE), aes(day_of_week, ..count../sum(..count..))) + 
  geom_bar(fill = "#1E90FF") +
  facet_grid(stop_hour_part_of_day2 ~ .) +
  theme(strip.text.y = element_text(angle = 45)) +
  labs(title = "Stop proportion by parts of the day and day of the week", x = "Day of Week", y = "Proportion")
ggsave("./EDA_images/stop_hour_part_of_day_day_of_week_prop2.png")
ggplot(df_clean %>% filter(is.na(stop_time_hour) != TRUE), aes(day_of_week, ..count../sum(..count..), group = 1)) + 
  geom_line(stat = "count", color = "red", size = 1) +
  facet_grid(stop_hour_part_of_day2 ~ .) +
  theme(strip.text.y = element_text(angle = 45)) +
  labs(title = "Stop proportion by parts of the day and day of the week", x = "Day of Week", y = "Proportion")
ggsave("./EDA_images/stop_hour_part_of_day_day_of_week_prop2_line2.png")
# Notes: Low number of stops from 0300 - 0500 while the highest number of stops occur
#        between 0900 and 1000 possibly coinciding with the high volume of people 
#        driving to work?
#        Other smaller peaks around 1400 and 2100.
#        When comparing the hours (not as a rearranged, ordered factor) graph to 
#        the time (hours and minutes) graph, there are not really any differences in patterns, 
#        so using just the hours graph for investigation is okay, and also easier.
#        Every day has the same number of hours regardless of year or month or day
#        of month, so don't need to do proportions to normalize.
#        Breaking up by time of day/ day of week indicates that the early hours of the 
#        morning/evening hours have more stops on the weekend, and during the day, 
#        more stops happen during the work week. 
#        From looking at part of day buckets:
#        [1500, 2400) doesn’t fluctuate much, however, it does increase on
#        Thursday, Friday, and Saturday.
#        The two highest peaks from [0500, 1500) occur on Tuesday and Friday, with
#        Saturday and Sunday at the lowest, while the two highest peaks between 
#        [0000, 0500) occur on Sunday and Saturday, with days in between scooping down.
#       	This indicates that the early hours of the morning/evenings have
#        more stops on the weekend, and during the day, more stops happen during the 
#        work week.

#county_name:
ggplot(df_clean %>%  filter(county_name != ""), aes(county_name)) + 
  geom_bar(fill = "#1E90FF") +
  theme1 +
  labs(title = "Stop count by county", x = "County")
ggsave("./EDA_images/county_name.png")

# county name: proportion to population
df_county_small <- df_county[, c(which(colnames(df_county) == "county_name"), 
                                 which(colnames(df_county) == "county_fips"), 
                                 which(colnames(df_county) == "TOT_POP"):which(colnames(df_county) == "HNAC_FEMALE"))]
df_county_small <- df_county_small %>% group_by(county_fips) %>% dplyr::mutate(count = n(), count_prop = count / TOT_POP) %>% distinct(county_fips, .keep_all = TRUE)
ggplot(df_county_small %>% filter(is.na(county_fips) != TRUE), aes(county_name, count_prop)) + 
  geom_bar(stat = "identity", fill = "#1E90FF") +
  theme1 +
  labs(title = "Proportion of stops to county population", x = "County", y = "Proportion to Population")
ggsave("./EDA_images/county_name_prop_to_pop.png")
# Notes: The county with the most stops is New Haven County and the one with the
#        one with the least is Litchfield County
#        While New Haven had the largest number of stops, proportionally, it has one
#        of the lowest # of stops/ county population ratios.
#        Tolland County by far, has the highest ratio of stops.

#driver_gender:
ggplot(df_clean, aes(driver_gender)) + 
  geom_bar(fill = "#1E90FF") +
  labs(title = "Stop count by gender", x = "Gender") +
  scale_x_discrete(labels = c("Female", "Male"))
ggsave("./EDA_images/driver_gender.png")
ggplot(df_clean, aes(factor(1), ..count.., fill = driver_gender)) +  
  geom_bar(position = "fill") +
  labs(title = "Stop proportion by gender", x = "Gender") +
  #scale_x_discrete(labels = "") + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
ggsave("./EDA_images/driver_gender_prop.png")

# Driver gender: proportion to population:
gender_pop <- gather(census_whole_state[,4:5], Gender, pop)
gender_pop$Gender[gender_pop$Gender == "TOT_MALE"] <- "M"
gender_pop$Gender[gender_pop$Gender == "TOT_FEMALE"] <- "F"
gender_pop$Gender <- as.factor(gender_pop$Gender)
gender_prop <- setNames(data.frame(table(df_clean$driver_gender, exclude = NULL)), c("Gender", "count")) 
gender_prop <- left_join(gender_prop, gender_pop, by = "Gender") %>%  mutate(gender_prop = count/pop)
ggplot(gender_prop, aes(Gender, gender_prop)) +
  geom_bar(stat = "identity", fill = "#1E90FF") +
  labs(title = "Proportion of stop count to population total", y = "Proportion to Population") +
  scale_x_discrete(labels = c("Female", "Male"))
ggsave("./EDA_images/gender_prop_to_pop.png")
# Notes: Men make up the most stops at over 60%
#        The ratio of men stopped per the population of men in Connecticut is higher
#        that the female ratio, indicating men are more likely to be stopped. 

#driver_age:
x_breaks <- seq(min(df_clean$driver_age[is.na(df_clean$driver_age) != TRUE]), max(df_clean$driver_age[is.na(df_clean$driver_age) != TRUE]) + 5, 5)
ggplot(df_clean %>%  filter(is.na(driver_age) != TRUE), aes(driver_age)) + 
  geom_bar(fill = "#56B4E9") + 
  scale_x_continuous(breaks = as.integer(x_breaks), labels = as.character(x_breaks)) +
  labs(title = "Stop count by driver age", x = "Age") +
  annotate("text", x = 65, y = 11500, label = "* ages < 15 and >= 80 have been made NA.", size = 3, color = "#696969")
ggsave("./EDA_images/driver_age_bar.png")
#Notes: The ages with the most stops are from 20-25 with the number of stops decreasing
#       as age increases, save for another small peak at age 40, 45, and 50.
#       Positively skewed age distribution

# driver_age: proportion to population:
# Connection between age group and age range:
# 4 = Age 15 to 19 years
# 5 = Age 20 to 24 years
# 6 = Age 25 to 29 years
# 7 = Age 30 to 34 years
# 8 = Age 35 to 39 years
# 9 = Age 40 to 44 years
# 10 = Age 45 to 49 years
# 11 = Age 50 to 54 years
# 12 = Age 55 to 59 years
# 13 = Age 60 to 64 years
# 14 = Age 65 to 69 years
# 15 = Age 70 to 74 years
# 16 = Age 75 to 79 years

age_df <- df_clean %>% select(driver_age) %>% group_by(driver_age) %>% dplyr::mutate(count = n()) %>% distinct(driver_age, .keep_all = TRUE)
age_df <- age_df %>% arrange(driver_age)
age_df$AGEGRP <- vector(mode="character", length = nrow(age_df))
age_df$AGEGRP[age_df$driver_age >= 15 & age_df$driver_age <= 19] <- 4
age_df$AGEGRP[age_df$driver_age >= 20 & age_df$driver_age <= 24] <- 5
age_df$AGEGRP[age_df$driver_age >= 25 & age_df$driver_age <= 29] <- 6
age_df$AGEGRP[age_df$driver_age >= 30 & age_df$driver_age <= 34] <- 7
age_df$AGEGRP[age_df$driver_age >= 35 & age_df$driver_age <= 39] <- 8
age_df$AGEGRP[age_df$driver_age >= 40 & age_df$driver_age <= 44] <- 9
age_df$AGEGRP[age_df$driver_age >= 45 & age_df$driver_age <= 49] <- 10
age_df$AGEGRP[age_df$driver_age >= 50 & age_df$driver_age <= 54] <- 11
age_df$AGEGRP[age_df$driver_age >= 55 & age_df$driver_age <= 59] <- 12
age_df$AGEGRP[age_df$driver_age >= 60 & age_df$driver_age <= 64] <- 13
age_df$AGEGRP[age_df$driver_age >= 65 & age_df$driver_age <= 69] <- 14
age_df$AGEGRP[age_df$driver_age >= 70 & age_df$driver_age <= 74] <- 15
age_df$AGEGRP[age_df$driver_age >= 75 & age_df$driver_age <= 79] <- 16
age_grp_df <- age_df %>% group_by(AGEGRP) %>% dplyr::mutate(count2 = sum(count)) %>% distinct(AGEGRP, .keep_all = TRUE)
age_grp_df$AGEGRP <- as.integer(age_grp_df$AGEGRP)
census_agegrp_small <- census_agegrp %>% select(1:2)
age_grp_df <- left_join(age_grp_df, census_agegrp_small, by = "AGEGRP")
age_grp_df <- age_grp_df %>% mutate(prop_to_pop = count2/TOT_POP)
ggplot(age_grp_df %>% filter(AGEGRP != ""), aes(AGEGRP, prop_to_pop)) +
  geom_bar(stat = "identity", fill = "#56B4E9") +
  labs(title = "Proportion of stops by age group to age group population", x = "Age Group", y = "Proportion of population") +
  #scale_x_continuous(breaks = 4:16, labels = as.character(4:16))
  scale_x_continuous(breaks = 4:16, labels = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79"))
ggsave("./EDA_images/agegroup_prop_to_pop.png")
# Notes: Proportion to the age populations of Connecticut, ages 20-29 have the most 
#        stops for its population. In general, the age proportion distribution 
#        follows a positive skew bell curve.  

#driver_race_raw:
ggplot(df_clean, aes(driver_race_raw)) + 
  geom_bar(fill = "#56B4E9") +
  labs(title = "Stop count by driver race", x = "Race")
ggsave("./EDA_images/driver_race_bar.png")
ggplot(df_clean, aes(driver_race_raw, ..prop.., group = 1)) + 
  geom_bar(fill = "#56B4E9") +
  labs(title = "Stop count proportion by driver race", x = "Race")
ggsave("./EDA_images/driver_race_bar_prop.png")
#Notes: Most of the stop population is white, taking up about 75% of the total

# driver_race_raw: proportion to population:
# races in dataset and correlated census columns:
# driver_race_raw
# 1           Black -> BAC_MALE + BAC_FEMALE 
# 2           White -> WAC_MALE + WAC_FEMALE 
# 3        Hispanic -> H_MALE + H_FEMALE
# 4 Native American -> IAC_MALE + IAC_FEMALE
# 5           Asian -> AAC_MALE + AAC_FEMALE 
# since the dataset doesn't have combination races, I will use the "alone or in 
# combination" (AC columns) columns from census because that doesn't leave anyone 
# (mixed race individuals) out. 
# The census also doesn't consider Hispanic a race, instead an ethnicity, so there
# is no AC column to use. This also means the population counts aren't mutually 
# exclusive like the dataset's race column is.
census_race <- census_whole_state %>% select(1:5, 
                                             BAC_MALE, BAC_FEMALE, 
                                             WAC_MALE, WAC_FEMALE, 
                                             H_MALE, H_FEMALE, 
                                             IAC_MALE,IAC_FEMALE, 
                                             AAC_MALE, AAC_FEMALE) %>% 
                                      dplyr::mutate(Black = BAC_MALE + BAC_FEMALE, 
                                                    White = WAC_MALE + WAC_FEMALE,
                                                    Hispanic = H_MALE + H_FEMALE,
                                                    NativeAmerican = IAC_MALE + IAC_FEMALE,
                                                    Asian = AAC_MALE + AAC_FEMALE)
census_race <- census_race %>% select(-c(6:15))
race_cols <- colnames(census_race)[6:10]
census_race <- data.frame(t(census_race[,-(1:5)])) # removing non-race columns
census_race$race <- race_cols
census_race <- arrange(census_race, race)
table_race <- setNames(data.frame(table(df_clean$driver_race_raw)),c("race", "count"))
table_race <- bind_cols(table_race, census_race)
temp_name <- colnames(table_race)
temp_name[3] <- "total_pop" #rename weird column
colnames(table_race) <- temp_name
table_race <- table_race %>% mutate(pop_prop = count/total_pop)
ggplot(table_race, aes(race, pop_prop)) + 
  geom_bar(stat = "identity", fill = "#56B4E9") +
  theme1 +
  labs(title = "Proportion of stops by race to race population", x = "Race", y = "Proportion of population")
ggsave("./EDA_images/race_prop_to_pop.png")
# Note: In the Census data, Hispanic is not considered a race, it's considered an 
#       ethnicity, so the race populations are not necessarily mutually exclusive.
#       Based on the proportion to the population, Blacks and whites have the highest
#       proportion of stops. Asian has the lowest. 
#       For whites, this isn’t too surprising because they make up 75% of the stops. 
#       Blacks only make up less than 15% of the stops. However, over 8% of the 
#       black population is stopped.
#       At 8.155939%, they actually have the highest percent to population stop ratio
#       with whites following closely behind with 8.110469%. 

#search_conducted:
ggplot(df_clean, aes(search_conducted)) + 
  geom_bar(fill = "#56B4E9") +
  labs(title = "Stop counts by searches conducted", x = "Search Conducted") +
  annotate("text", x = 2.3, y = 310000, label = "*Vehicle searches", size = 3, color = "#696969")
ggsave("./EDA_images/search_conducted_bar.png")
ggplot(df_clean, aes(search_conducted, ..prop.., group = 1)) + 
  geom_bar(fill = "#56B4E9") +
  labs(title = "Stop count proportion by searches conducted", x = "Search Conducted") +
  annotate("text", x = 2.3, y = .97, label = "*Vehicle searches", size = 3, color = "#696969")
ggsave("./EDA_images/search_conducted_bar_prop.png")
sc <- ggplot(df_clean, aes(search_conducted, ..prop.., group = 1)) + geom_bar()
plt_sc <- ggplot_build(sc)
plt_sc$data[[1]]
# y  count      prop x group PANEL ymin      ymax xmin xmax colour   fill size
# 1 0.9832679 313337 0.9832679 1     1     1    0 0.9832679 0.55 1.45     NA grey35  0.5
# 2 0.0167321   5332 0.0167321 2     1     1    0 0.0167321 1.55 2.45     NA grey35  0.5
# linetype alpha
# 1        1    NA
# 2        1    NA
#Notes: Most of the stops did not result in a search (~1.7%)

#search_type_raw:
ggplot(df_clean %>% filter(df_clean$search_type_raw != ""), aes(search_type_raw)) + 
  geom_bar(fill = "#56B4E9") +
  annotate("text", x = 2, y = -100, label = "*An inventory search is a warrantless search of a lawfully \nimpounded vehicle conducted by police.", size = 2) +
  labs(title = "Stop count by search type", x = "Search Type")
ggsave("./EDA_images/search_type_bar.png")
ggplot(df_clean %>% filter(df_clean$search_type_raw != ""), aes(search_type_raw, ..prop.., group = 1)) + 
  geom_bar(fill = "#56B4E9") +
  annotate("text", x = 2, y = -.025, label = "*An inventory search is a warrantless search of a lawfully \nimpounded vehicle conducted by police.", size = 2) +
  labs(title = "Stop count proportion by search type", x = "Search Type")
ggsave("./EDA_images/search_type_bar_prop.png")
#Notes: Consent and Other are the two top search types, with inventory coming in
#       a very far third

#contraband_found:
ggplot(df_clean, aes(contraband_found)) + 
  geom_bar(fill = "#56B4E9") +
  labs(title = "Search count by contraband found status", x = "Contraband Found")
ggsave("./EDA_images/contraband_found_bar.png")
ggplot(df_clean, aes(contraband_found, ..prop.., group = 1)) + 
  geom_bar(fill = "#56B4E9") +
  labs(title = "Search count proportion by contraband found status", x = "Contraband Found")
ggsave("./EDA_images/contraband_found_bar_prop.png")
c <- ggplot(df_clean, aes(contraband_found, ..prop.., group = 1)) + geom_bar()
plt_c <- ggplot_build(c)
plt_c$data[[1]]
#Notes: A very small proportion of contraband was found (0.57%)

#stop_outcome:
ggplot(df_clean %>% filter(is.na(stop_outcome) != TRUE), aes(stop_outcome)) + 
  geom_bar(fill = "#56B4E9") +
  labs(title = "Stop count by stop outcome", x = "Stop Outcome")
ggsave("./EDA_images/stop_outcome_bar.png")
ggplot(df_clean %>% filter(is.na(stop_outcome) != TRUE), aes(stop_outcome, ..prop.., group = 1)) + 
  geom_bar(fill = "#56B4E9") +
  labs(title = "Stop count proportion by stop outcome", x = "Stop Outcome", y = "Stop Proportion")
ggsave("./EDA_images/stop_outcome_bar_prop.png")
#Notes: Most stops end in a ticket (70%), with verbal warning coming in a far second.

#is_arrested:
ggplot(df_clean %>% filter(is_arrested != ""), aes(is_arrested)) + 
  geom_bar(fill = "#56B4E9") +
  labs(title = "Stop count by arrested status", x = "Is Arrested")
ggsave("./EDA_images/is_arrested_bar.png")
ggplot(df_clean %>% filter(is_arrested != ""), aes(is_arrested, ..prop.., group = 1)) + 
  geom_bar(fill = "#56B4E9") +
  labs(title = "Stop count proportion by arrested status", x = "Is Arrested", y = "Count Proportion")
ggsave("./EDA_images/is_arrested_bar_prop.png")
a <- ggplot(df_clean, aes(is_arrested, ..prop.., group = 1)) + geom_bar()
plt_a <- ggplot_build(a)
plt_a$data[[1]]
# y  count       prop x group PANEL ymin       ymax xmin xmax colour   fill
# 1 0.96024715 306001 0.96024715 1     1     1    0 0.96024715 0.55 1.45     NA grey35
# 2 0.02294544   7312 0.02294544 2     1     1    0 0.02294544 1.55 2.45     NA grey35
# 3 0.01680741   5356 0.01680741 3     1     1    0 0.01680741 2.55 3.45     NA grey35
# size linetype alpha
# 1  0.5        1    NA
# 2  0.5        1    NA
# 3  0.5        1    NA
#Notes: Most people are not arrested at 2.3% of stops ending in arrests.

#officer_id:
ggplot(df_clean, aes(officer_id)) + 
  geom_bar(fill = "#56B4E9") +
  labs(title = "Stop count by officer", x = "Officer ID")
ggsave("./EDA_images/officer_id_bar.png")
#There are some heavy stoppers so let's narrow the window to investigate the top 50
officer_stops <- setNames(data.frame(table(df_clean$officer_id)), c("officer", "count"))
officer_stops <- officer_stops %>% arrange(desc(count))
officer_stops <- officer_stops %>% mutate(stop_prop = count/ sum(count))
top_50 <- head(officer_stops, 50)
ggplot(top_50, aes(officer, count)) + 
  geom_bar(stat = "identity", fill = "#56B4E9") +
  theme1 +
  labs(title = "Stop counts of top 50 officers", x = "Officer ID")
ggsave("./EDA_images/officer_id_top_50.png")
ggplot(top_50, aes(reorder(officer, -stop_prop), stop_prop)) + 
  geom_col(fill = "#56B4E9") + 
  theme1 +
  labs(title = "Stop proportion per officer", x = "Officer ID", y = "Proportion")
ggsave("./EDA_images/officer_id_top_50_prop.png")
officer_plot2 <- ggplot(officer_stops, aes(count)) + geom_histogram(binwidth = 250)
plt_officer_plot2 <- ggplot_build(officer_plot2)
officer_plot_x_labs2 <- plt_officer_plot2$data[[1]]$x
ggplot(officer_stops, aes(count, ..density..)) + 
  geom_histogram(binwidth = 250, fill = "#191970") +
  scale_x_continuous(breaks = officer_plot_x_labs2, labels = as.character(officer_plot_x_labs2)) + 
  theme1 +
  labs(title = "Proportion of total officers per stop count", x = "# of stops", y = "% of officers")
ggsave("./EDA_images/stop_count_by_officers.png")
df_officer <- setNames(data.frame(table(df_clean$officer_id, df_clean$stop_date)), c("OfficerID", "StopDate", "count"))
officer_tot_stops <- df_officer %>% group_by(OfficerID) %>% dplyr::summarise(sum_stops  = sum(count))
df_officer <- left_join(df_officer, officer_tot_stops, by = "OfficerID")
df_officer <- df_officer %>% arrange(desc(sum_stops), OfficerID)
top_10 <- head(df_officer, 10*nrow(df_officer %>% select(StopDate) %>% distinct()))
top_10$StopDate <- as.POSIXct(top_10$StopDate, "%Y-%m-%d", tz = "America/New_York")
# Top stops officers on one plot:
A <- ggplot(top_10 %>% filter(OfficerID == "706750556"), aes(StopDate, count)) +
  geom_line(stat = "identity") +
  scale_y_continuous(limits = c(0,60)) +
  scale_x_datetime(date_breaks = "2 month", date_labels = "%m/%Y") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
B <- ggplot(top_10 %>% filter(OfficerID == "1000003154"), aes(StopDate, count)) +
  geom_line(stat = "identity") +
  scale_y_continuous(limits = c(0,60)) +
  scale_x_datetime(date_breaks = "2 month", date_labels = "%m/%Y") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
C <- ggplot(top_10 %>% filter(OfficerID == "790642042"), aes(StopDate, count)) +
  geom_line(stat = "identity") +
  scale_y_continuous(limits = c(0,60)) +
  scale_x_datetime(date_breaks = "2 month", date_labels = "%m/%Y") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
D <- ggplot(top_10 %>% filter(OfficerID == "614680420"), aes(StopDate, count)) +
  geom_line(stat = "identity") +
  scale_y_continuous(limits = c(0,60)) +
  scale_x_datetime(date_breaks = "2 month", date_labels = "%m/%Y") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
E <- ggplot(top_10 %>% filter(OfficerID == "1000002362"), aes(StopDate, count)) +
  geom_line(stat = "identity") +
  scale_y_continuous(limits = c(0,60)) +
  scale_x_datetime(date_breaks = "2 month", date_labels = "%m/%Y") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
G <- ggplot(top_10 %>% filter(OfficerID == "1000002598"), aes(StopDate, count)) +
  geom_line(stat = "identity") +
  scale_y_continuous(limits = c(0,60)) +
  scale_x_datetime(date_breaks = "2 month", date_labels = "%m/%Y") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
#grid.arrange(A, B, C, D, E, G, ncol = 1, top = "Stops per day for top officers", bottom = "Stop Date", left = "Count")
officer_g <- arrangeGrob(A, B, C, D, E, G, ncol = 1, top = "Stops per day for top officers", bottom = "Stop Date", left = "Count")
ggsave(file = "./EDA_images/stop_count_by_top6_officers.png", officer_g)
#Notes: There are some officers who have a significantly higher count of stops than
#       the rest. The top 3 each hold more than .75% of the stops, when most hold 
#       less than .375%
# Here are the top 3:
# officer count
# 1   706750556  2822
# 2  1000003154  2746
# 3   790642042  2348
#       Looking at the proportion of total officers per stop count, there isn't a
#       normal curve, instead, most officers have < 200 stops from 10/2013 - 3/2015.
#       The more stops, the less and less officers there are. There is an outlier
#       of around 2750 stops with a few officers, as other plots showed. 
#       Looking at the top officers stops per day, it doesn’t seem to be one outlier 
#       spike in stops, but rather a regular occurrence. The top two offenders did 
#       not even have stops for the whole time, indicating they didn’t work during 
#       the whole time tracked in the dataset.

# Take average of stops a day except for days where # stops is 0, because assume 
# either not a police officer then or not working that day.
df_officer <- df_officer %>% group_by(OfficerID) %>% dplyr::mutate(avg_stops_per_day = mean(count[count >0]))
avg_stops_officer <- df_officer %>% group_by(OfficerID) %>% select(OfficerID, sum_stops, avg_stops_per_day) %>% distinct(OfficerID, .keep_all = TRUE)
ggplot(avg_stops_officer, aes(OfficerID, avg_stops_per_day)) +
  geom_point(stat = "identity", color = "#56B4E9") +
  theme1 +
  labs(title = "Average Stops/Day for each officer", x = "Officer ID", y = "Average stops/day")
#bucket by avg stops
ggplot(avg_stops_officer, aes(avg_stops_per_day)) +
  geom_histogram(binwidth = 1, fill = "#56B4E9") +
  scale_x_continuous(breaks = 1:14, labels = as.character(1:14)) +
  labs(title = "Average Stops/Day", x = "Average stops per day", y = "# of officers") +
  annotate("text", x = 10, y = 390, label = "*Days with 0 stops not included in calculations.", size = 3, color = "#696969")
ggsave("./EDA_images/avg_stops_per_day_bar.png")
# Notes: Most police officers have 2 stops per day on average, while some have
#        as high as 10. 

#stop_duration:
ggplot(df_clean, aes(stop_duration)) + 
  geom_bar(fill = "#56B4E9") +
  labs(title = "Stop count by stop duration", x = "Stop Duration")
ggsave("./EDA_images/stop_duration_bar.png")
ggplot(df_clean, aes(stop_duration, ..prop.., group = 1)) + 
  geom_bar(fill = "#56B4E9") +
  labs(title = "Stop count proportion by stop duration", x = "Stop Duration", y = "Count proportion")
ggsave("./EDA_images/stop_duration_bar_prop.png")
sd <- ggplot(df_clean, aes(stop_duration, ..prop.., group = 1)) + geom_bar()
plt_sd <- ggplot_build(sd)
plt_sd$data[[1]]
# y  count       prop x group PANEL ymin       ymax xmin xmax colour   fill
# 1 0.91130923 290406 0.91130923 1     1     1    0 0.91130923 0.55 1.45     NA grey35
# 2 0.07031120  22406 0.07031120 2     1     1    0 0.07031120 1.55 2.45     NA grey35
# 3 0.01837957   5857 0.01837957 3     1     1    0 0.01837957 2.55 3.45     NA grey35
# size linetype alpha
# 1  0.5        1    NA
# 2  0.5        1    NA
# 3  0.5        1    NA
#Notes: Most stops (91.1%) had a duration of 1-15 min.

#violation_count:
ggplot(df_clean, aes(violation_count)) + 
  geom_bar(fill = "#56B4E9") +
  labs(title = "Stop count by violation count", x = "Violation Count")
ggsave("./EDA_images/violation_count_bar.png")
ggplot(df_clean, aes(violation_count, ..prop.., group = 1)) + 
  geom_bar(fill = "#56B4E9") +
  labs(title = "Stop count proportion by violation count", x = "Violation Count", y = "Count Proportion")
ggsave("./EDA_images/violation_count_bar_prop.png")
vc <- ggplot(df_clean, aes(violation_count, ..prop.., group = 1)) + geom_bar()
plt_vc <- ggplot_build(vc)
plt_vc$data[[1]]
# y  count         prop x group PANEL ymin         ymax xmin xmax colour
# 1 9.929896e-01 316435 9.929896e-01 1     1     1    0 9.929896e-01 0.55 1.45     NA
# 2 6.244724e-03   1990 6.244724e-03 2     1     1    0 6.244724e-03 1.55 2.45     NA
# 3 6.840954e-04    218 6.840954e-04 3     1     1    0 6.840954e-04 2.55 3.45     NA
# 4 7.217520e-05     23 7.217520e-05 4     1     1    0 7.217520e-05 3.55 4.45     NA
# 5 9.414157e-06      3 9.414157e-06 5     1     1    0 9.414157e-06 4.55 5.45     NA
# fill size linetype alpha
# 1 grey35  0.5        1    NA
# 2 grey35  0.5        1    NA
# 3 grey35  0.5        1    NA
# 4 grey35  0.5        1    NA
# 5 grey35  0.5        1    NA
#Notes: Most of the stops (99.3%) only had 1 violation.

#violations:
# Edit df_split for visualization
df_split2 <- df_split %>% gather("violation_new", "n", 'violation_raw_Cell.Phone':'violation_raw_Window.Tint')
ggplot(df_split2, aes(reorder(violation_new, -n), n)) + 
  geom_col(fill = "#56B4E9") + 
  theme1 +
  labs(title = "Stop counts per violation", x = "Violation", y = "Count")
ggsave("./EDA_images/violations_bar.png")
#Notes: The top 5 violations are Speed.Related, Other, Registration, Moving.Violation,
#       and Cell.Phone.
df_split2 %>% group_by(violation_new) %>% summarise(count = sum(n)) %>% arrange(desc(count))
# # A tibble: 14 x 2
# violation_new                        count
# <chr>                                <int>
# 1 violation_raw_Speed.Related          89764
# 2 violation_raw_Other                  61567
# 3 violation_raw_Registration           27762
# 4 violation_raw_Moving.Violation       24513
# 5 violation_raw_Cell.Phone             17778
# 6 violation_raw_Other.Error            14999
# 7 violation_raw_Seatbelt               11473
# 8 violation_raw_Defective.Lights       10305
# 9 violation_raw_Stop.Sign               6199
# 10 violation_raw_Display.of.Plates       5160
# 11 violation_raw_Traffic.Control.Signal  4314
# 12 violation_raw_Suspended.License       2523
# 13 violation_raw_Window.Tint             1996
# 14 violation_raw_Equipment.Violation      615

ggplot(df_split2, aes(reorder(violation_new, -n), n/sum(n))) + 
  geom_col(fill = "#56B4E9") + 
  theme1 +
  labs(title = "Stop count per violation proportion", x = "Violation", y = "Proportion")
ggsave("./EDA_images/violations_bar_prop.png")
df_split2 %>% group_by(violation_new) %>% summarise(count = sum(n)) %>% mutate(viol_freq = count/sum(count)) %>% arrange(desc(count))
# # A tibble: 14 x 3
# violation_new                        count viol_freq
# <chr>                                <int>     <dbl>
# 1 violation_raw_Speed.Related          89764   0.322  
# 2 violation_raw_Other                  61567   0.221  
# 3 violation_raw_Registration           27762   0.0995 
# 4 violation_raw_Moving.Violation       24513   0.0879 
# 5 violation_raw_Cell.Phone             17778   0.0637 
# 6 violation_raw_Other.Error            14999   0.0538 
# 7 violation_raw_Seatbelt               11473   0.0411 
# 8 violation_raw_Defective.Lights       10305   0.0369 
# 9 violation_raw_Stop.Sign               6199   0.0222 
# 10 violation_raw_Display.of.Plates       5160   0.0185 
# 11 violation_raw_Traffic.Control.Signal  4314   0.0155 
# 12 violation_raw_Suspended.License       2523   0.00904
# 13 violation_raw_Window.Tint             1996   0.00715
# 14 violation_raw_Equipment.Violation      615   0.00220



# MORE THAN ONE VARIABLE:

# ggplot(df_split2, aes(reorder(violation_new, -n), n, fill = driver_race_raw)) + geom_col() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
# ggplot(df_split2, aes(violation_new, n, fill = driver_race_raw)) + geom_col()
# ggplot(df_split2, aes(reorder(violation_new, -n), n, fill = driver_race_raw)) + geom_col(position = "fill") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
# ggplot(df_split2, aes(reorder(violation_new, -n), n, fill = driver_race_raw)) + geom_col(position = "fill") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Proportions- gender by arrests
g <- ggplot(df_clean, aes(is_arrested, fill = driver_gender)) + geom_bar(aes(y = ..prop.., group = driver_gender))
plt_g <- ggplot_build(g)
plt_g$data[[1]]
# fill          y  count       prop x group PANEL       ymin       ymax xmin xmax
# 1 #00BFC4 0.95527291 202408 0.95527291 1     2     1 0.00000000 0.95527291 0.55 1.45
# 2 #F8766D 1.92539015 103593 0.97011725 1     1     1 0.95527291 1.92539015 0.55 1.45
# 3 #00BFC4 0.02650022   5615 0.02650022 2     2     1 0.00000000 0.02650022 1.55 2.45
# 4 #F8766D 0.04239212   1697 0.01589189 2     1     1 0.02650022 0.04239212 1.55 2.45
# 5 #00BFC4 0.01822687   3862 0.01822687 3     2     1 0.00000000 0.01822687 2.55 3.45
# 6 #F8766D 0.03221773   1494 0.01399086 3     1     1 0.01822687 0.03221773 2.55 3.45
# colour size linetype alpha
# 1     NA  0.5        1    NA
# 2     NA  0.5        1    NA
# 3     NA  0.5        1    NA
# 4     NA  0.5        1    NA
# 5     NA  0.5        1    NA
# 6     NA  0.5        1    NA
# <=>
table(df_clean$driver_gender, df_clean$is_arrested, exclude = NULL)
arrest_by_gender <- setNames(data.frame(table(df_clean$driver_gender, df_clean$is_arrested, exclude = NULL)), c("Gender", "Arrested", "count")) 
gender_totals <- data.frame(arrest_by_gender %>% group_by(Gender) %>% dplyr::summarise(sum_gender = sum(count)))
arrest_by_gender <- left_join(arrest_by_gender, gender_totals, by = "Gender")
arrest_by_gender <- arrest_by_gender %>% mutate(percent_gender  = count/sum_gender)
# Proportions and counts within each gender not of gender within each arrest status:
ggplot(arrest_by_gender, aes(Arrested, count, fill = Gender)) + 
  geom_bar(stat = "identity") +
  labs(title = "Arrest status counts for each gender", x = "Arrest Status", y = "Count") +
  scale_fill_discrete(labels=c("Female","Male"))
ggsave("./EDA_images/gender_by_arrests_count.png")
ggplot(arrest_by_gender, aes(Arrested, percent_gender, fill = Gender)) + 
  geom_bar(stat = "identity") +
  labs(title = "Arrest status proportion for each gender", x = "Arrest Status", y = "Proportion") +
  scale_fill_discrete(labels=c("Female","Male"))
ggsave("./EDA_images/gender_by_arrests_prop.png")
ggplot(df_clean, aes(is_arrested, fill = driver_gender)) + 
  geom_bar(aes(y = ..prop.., group = driver_gender), position = "dodge") +
  labs(title = "Arrest status proportion for each gender", x = "Arrest Status", y = "Proportion") +
  scale_fill_discrete(labels=c("Female","Male"), name = "Gender")
ggsave("./EDA_images/gender_by_arrests_dodge.png")
# Notes: The dodge image more clearly shows that males who are stopped are more 
#        likely to be arrested than females who are stopped.
#        In fact, stopped men are arrested 1.5 times more than stopped women:
#        0.02650022 / 0.01589189 = 1.667531

# #getting a better picture of is_arrested == TRUE
# ggplot(df_clean %>% filter(is_arrested == TRUE | is.na(is_arrested) == TRUE), aes(is_arrested, fill = driver_gender)) + 
#   geom_bar(aes(y = ..prop.., group = driver_gender), position = "dodge")
# -> trying to show percent arrest of gender as a percent of the total gender in the dataset
