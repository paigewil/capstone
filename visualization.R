# This file is a code dump for all my univariate and bivariate plotting testing

library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(plyr)
library(ggpubr)
library(gridExtra)
library(maps)
library(choroplethr)
library(choroplethrMaps)
library(RColorBrewer)

# Read in file
df_clean <- read.csv("E:/Learning/Springboard Intro to Data Science/capstone/CT_cleaned_edit.csv")
df_split <- read.csv("E:/Learning/Springboard Intro to Data Science/capstone/CT_cleaned_split.csv")

#ADDING EXTERNAL DATA SOURCES:
census <- read.csv("E:/Learning/Springboard Intro to Data Science/capstone/cc-est2016-alldata-09.csv")
census$COUNTY <- as.integer(census$COUNTY + 9000)
names(census)[3] <- "county_fips"

# Only want numbers from 2014, since it's the only full year in our dataset and can't get
# partial year information for 2013 & 2015 to average everything out.
census <- census %>% filter(YEAR == 7)
#AGERP == 0 contains information on all ages
census_county <- census %>% filter(AGEGRP == 0)
df_county <- left_join(df_clean, census_county, by = "county_fips")
census_agegrp <- census %>% group_by(AGEGRP) %>% dplyr::summarise_at(vars(TOT_POP:HNAC_FEMALE), sum)
census_whole_state <- census %>% filter(AGEGRP == 0) %>% group_by(STATE, STNAME) %>% dplyr::summarise_at(vars(TOT_POP:HNAC_FEMALE), sum)

theme1 <- theme(axis.text.x=element_text(angle=90, hjust=1))

poverty <- read.csv("E:/Learning/Springboard Intro to Data Science/capstone/SAIPESNC_01JUN18_17_18_03_66.csv", stringsAsFactors=F)

uninsured <- read.csv("E:/Learning/Springboard Intro to Data Science/capstone/SAHIE_01JUN18_19_07_35_90.csv", stringsAsFactors=F)

miles <- read.csv("E:/Learning/Springboard Intro to Data Science/capstone/2014_Daily_Vehicle_Miles_Travelled_By_Town_And_Roadway_Classification.csv", stringsAsFactors=F)

weather <- read.csv("E:/Learning/Springboard Intro to Data Science/capstone/1361834.csv", stringsAsFactors=F)

bridges <- read.csv("E:/Learning/Springboard Intro to Data Science/capstone/2014NBI.csv", stringsAsFactors=F)

commutes <- read.csv("E:/Learning/Springboard Intro to Data Science/capstone/county_work_residencecsv.csv", stringsAsFactors=F)





#ONE-DIMENSIONAL DISTRIBUTIONS:

#stop_date:
#df_clean$stop_date <- as.POSIXct(df_clean$stop_date, "%m/%d/%Y", tz = "America/New_York")
df_clean$stop_date <- as.POSIXct(df_clean$stop_date, "%Y-%m-%d", tz = "America/New_York")
ggplot(df_clean, aes(stop_date)) + geom_bar(fill = "#1E90FF")  +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%m/%Y") +
  theme1 + 
  labs(title = "Police stops per day (10/1/2013 - 03/31/2015)", x = "Stop Date")
ggsave("./EDA_images/0_stop_date_bar.png")
ggplot(df_clean, aes(stop_date, ..prop.., group = 1)) + 
  geom_bar(fill = "#1E90FF")  +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%m/%Y") +
  theme1 + 
  labs(title = "Proportion of police stops per day (10/1/2013 - 03/31/2015)", x = "Stop Date", y = "Proportion")
ggsave("./EDA_images/1_stop_date_bar_prop.png")
df_date <- setNames(data.frame(table(df_clean$stop_date)),c("StopDate","Count"))
df_date$StopDate <- as.POSIXct(df_date$StopDate, "%Y-%m-%d", tz = "America/New_York")
ggplot(df_date, aes(x = StopDate, y = Count)) + 
  geom_bar(stat = "identity", fill = "#1E90FF")  +
  geom_smooth(se = FALSE, color = "red") + 
  scale_x_datetime(date_breaks = "1 month", date_labels = "%m/%Y") +
  theme1 + 
  labs(title = "Count of police stops per day (10/1/2013 - 03/31/2015)", x = "Stop Date")
ggsave("./EDA_images/2_stop_date_bar_prop_w_loess.png")
# line plot
# find number of dates so one bin per date
no_dates <- as.numeric(range(df_clean$stop_date)[2] - range(df_clean$stop_date)[1])
ggplot(df_clean, aes(stop_date)) + 
  geom_line(stat = "bin", bins = no_dates, color = "#1E90FF") +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%m/%Y") +
  theme1 + 
  labs(title = "Police stops per day (10/1/2013 - 03/31/2015)", x = "Stop Date")
ggsave("./EDA_images/3_stop_date_line1.png")
#OR
df_stop_date <- setNames(data.frame(table(df_clean$stop_date)),c("Stop.Date","Count"))
df_stop_date$Stop.Date <- as.POSIXct(df_stop_date$Stop.Date, "%Y-%m-%d", tz = "America/New_York")
ggplot(df_stop_date, aes(x = Stop.Date, y = Count, group = 1)) + geom_line(color = "#1E90FF") +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%m/%Y") +
  theme1 +
  labs(title = "Police stops per day (10/1/2013 - 03/31/2015)", x = "Stop Date")
ggsave("./EDA_images/4_stop_date_line2.png")
ggplot(df_stop_date, aes(x = Stop.Date, y = Count, group = 1)) + 
  geom_line(color = "#1E90FF") +
  geom_smooth(se = FALSE, color = "red") +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%m/%Y") +
  theme1 +
  labs(title = "Police stops per day (10/1/2013 - 03/31/2015)", x = "Stop Date")
ggsave("./EDA_images/5_stop_date_line2_w_loess.png")
df_stop_date <- df_stop_date %>% mutate(stop_date_prop = Count/sum(Count))
ggplot(df_stop_date, aes(x = Stop.Date, y = stop_date_prop)) + 
  geom_line(color = "#1E90FF") +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%m/%Y") +
  theme1 +
  labs(title = "Proportion of police stops per day (10/1/2013 - 03/31/2015)", x = "Stop Date", y = "Proportion")
ggsave("./EDA_images/6_stop_date_line2_prop.png")
# Notes: There are a low number of stops around 02/2014 and 02/2015.
#        There are a higher number of stops around 05/2014 and 07/2014
#        Similar (yet smaller) peaks in December followed by a decrease and then increase in March.
#        In general, it seems like there are less stops around the end of the year/
#        beginning of the new year (winter), and more stops over the summer.
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
ggsave("./EDA_images/7_stop_month_total.png")

# month: weighted
months <- c(2,2,2,1,1,1,1,1,1,2,2,2)
# -> occurence of each month in dataset
months_df <- setNames(data.frame(factor(as.character(1:12), levels = as.character(1:12)), months), c("month", "occurrence"))
table_months <- setNames(data.frame(table(df_clean$stop_month)),c("month", "count"))
table_months$count <- as.numeric(table_months$count)
months_df <- left_join(table_months, months_df, by = "month")
months_df <- months_df %>% mutate(month_prop = count/occurrence)
ggplot(months_df, aes(month, month_prop)) + 
  geom_bar(stat = "identity", fill = "#1E90FF") +
  labs(title = "Stops per month (weighted)", x = "Stop Month", y = "# of stops/month \n(Weighted by month's count in dataset)")
ggsave("./EDA_images/8_stop_month_weighted.png")
# Notes: The data set goes from 10/2013 - 03/2015 so the raw counts might be skewed
#        heavily towards months 10, 11, 12, 1, 2, 3 which appear across multiple
#        years.
#        Dividing the counts by the number of years in which a month is present in the data set
#        gives a more proportioned/weighted perspective. With this, we can see that May, August, 
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
ggsave("./EDA_images/9_stop_month_year.png")
ggplot(df_clean, aes(month_year, ..prop.., group = 1)) + 
  geom_bar(fill = "#1E90FF") + 
  theme1 + 
  labs(title = "Stop proportions per month/year", x = "Stop Month/Year", y = "Proportion")
ggsave("./EDA_images/10_stop_month_year_prop.png")
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
ggsave("./EDA_images/11_stop_year.png")
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
ggsave("./EDA_images/12_stop_day_total.png")

# day: weighted
days_prop <- c(rep(c(18,16), times = c(28, 2)), 11)
days_df <- setNames(data.frame(factor(days_label, levels = days_label), days_prop), c("day", "occurrence"))
table_days <- setNames(data.frame(table(df_clean$day_of_month)),c("day", "count"))
table_days$count <- as.numeric(table_days$count)
days_df <- left_join(table_days, days_df, by = "day")
days_df <- days_df %>% mutate(day_prop = count/occurrence)
ggplot(days_df, aes(day, day_prop)) + 
  geom_bar(stat = "identity", fill = "#1E90FF") + 
  theme1 +
  labs(title = "Stops by day of month: \nRatio of stop count weighted by occurence of days in year", x = "Day of Month", y = "Ratio Count (weighted)")
ggsave("./EDA_images/13_stop_day_weighted.png")
ggplot(days_df, aes(as.numeric(day), day_prop)) + 
  geom_bar(stat = "identity", fill = "#1E90FF") + 
  geom_smooth(method = "lm", se = FALSE, color = "red") + 
  theme1 +
  labs(title = "Stops by day of month: \nRatio of stop count weighted by occurence of days in year \nw best fit line", x = "Day of Month", y = "Ratio Count")
ggsave("./EDA_images/14_stop_day_weighted_w_lm.png")
# loess model shows the same trend
ggplot(days_df, aes(as.numeric(day), day_prop)) + 
  geom_bar(stat = "identity", fill = "#1E90FF") + 
  geom_smooth(se = FALSE, color = "red") + 
  theme1 +
  labs(title = "Stops by day of month: \nRatio of stop count weighted by occurence of days in year \nw LOESS", x = "Day of Month", y = "Ratio Count")
ggsave("./EDA_images/15_stop_day_weighted_w_loess.png")
# Notes: Looking at the weighted counts, the end of the month seems to have more stops
#        than the beginning.
#        Adding the linear model & LOESS line confirms that the number of stops increase
#        towards the end of the month.

#day of the week
df_clean$day_of_week <- weekdays(df_clean$stop_date)
df_clean$day_of_week <- factor(df_clean$day_of_week, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
ggplot(df_clean, aes(day_of_week)) + 
  geom_bar(fill = "#1E90FF") +
  theme1 +
  labs(title = "Stops by day of week", x = "Day of Week")
ggsave("./EDA_images/16_stop_day_of_the_week.png")
ggplot(df_clean, aes(day_of_week, ..prop.., group = 1)) + 
  geom_bar(fill = "#1E90FF") +
  theme1 +
  labs(title = "Stop proportions by day of week", x = "Day of Week", y = "Proportion")
ggsave("./EDA_images/17_stop_day_of_the_week_prop.png")
#Notes: More stops happen towards the end of the week (Friday and Saturday) with
#       the least happening on Sunday.

#stop_time:
ggplot(df_clean %>% filter(stop_time != "0:00"), aes(stop_time)) + 
  geom_bar(fill = "#1E90FF") +
  labs(title = "Stop times by count", x = "Stop Time")
ggsave("./EDA_images/18_stop_time.png")

#to see that NAs are dispersed so it's okay to ignore:
ggplot(df_clean %>% filter(is.na(stop_time_hour) == TRUE), aes(stop_date)) + 
  geom_bar(fill = "#1E90FF") +
  labs(title = "NAs by stop date", x = "Stop Date")
ggsave("./EDA_images/18.2_stop_timeNAs.png")

# hour only; remove 0:00 since is a NA-filled value
df_clean$stop_time_hour <- df_clean$stop_time
df_clean$stop_time_hour[df_clean$stop_time_hour == "0:00"] <- NA
df_clean$stop_time_hour <- sub(":.*", "", df_clean$stop_time_hour)
df_clean$stop_time_hour <- factor(df_clean$stop_time_hour, 
                                  ordered = TRUE,
                                  levels = as.character(0:23))
df_clean$stop_time_hour_numeric <- as.numeric(df_clean$stop_time_hour) - 1
# http://www.learnersdictionary.com/qa/parts-of-the-day-early-morning-late-morning-etc
#initialize with blank vector
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
ggsave("./EDA_images/19_stop_hour.png")
ggplot(df_clean %>% filter(is.na(stop_time_hour) != TRUE), aes(stop_time_hour, ..prop.., group = 1)) + 
  geom_bar(fill = "#1E90FF") +
  labs(title = "Stop times (hour) by proportion", x = "Stop Time (Hour)", y = "Proportion")
ggsave("./EDA_images/20_stop_hour_prop.png")
ggplot(df_clean %>% filter(is.na(stop_time_hour) != TRUE), aes(stop_hour_part_of_day, ..prop.., group = 1)) + 
  geom_bar(fill = "#1E90FF") +
  labs(title = "Stop proportion by parts of the day", x = "Part of Day", y = "Proportion") +
  scale_x_discrete(labels = c("Morning\n(0500 - 1159)", "Afternoon\n(1200 - 1659)", "Evening\n(1700 - 2059)", "Night\n(2100 - 0459)"))
ggsave("./EDA_images/21_stop_hour_prop_part_of_day.png")
ggplot(df_clean %>% filter(is.na(stop_time_hour) != TRUE), aes(day_of_week, ..count../sum(..count..))) + 
  geom_bar(fill = "#1E90FF") +
  facet_grid(stop_hour_part_of_day ~ .) +
  labs(title = "Stop proportion by parts of the day and days of the week", x = "Day of the Week", y = "Proportion")
ggsave("./EDA_images/22_stop_hour_part_of_day_day_of_week_prop.png")
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
ggsave("./EDA_images/23_stop_hour_prop_part_of_day2.png")
ggplot(df_clean %>% filter(is.na(stop_time_hour) != TRUE), aes(day_of_week, ..count../sum(..count..))) + 
  geom_bar(fill = "#1E90FF") +
  facet_grid(stop_hour_part_of_day2 ~ .) +
  theme(strip.text.y = element_text(angle = 45)) +
  labs(title = "Stop proportion by parts of the day and day of the week", x = "Day of Week", y = "Proportion")
ggsave("./EDA_images/24_stop_hour_part_of_day_day_of_week_prop2.png")
ggplot(df_clean %>% filter(is.na(stop_time_hour) != TRUE), aes(day_of_week, ..count../sum(..count..), group = 1)) + 
  geom_line(stat = "count", color = "red", size = 1) +
  facet_grid(stop_hour_part_of_day2 ~ .) +
  theme(strip.text.y = element_text(angle = 45)) +
  labs(title = "Stop proportion by parts of the day and day of the week", x = "Day of Week", y = "Proportion")
ggsave("./EDA_images/25_stop_hour_part_of_day_day_of_week_prop2_line2.png")
# ggplot(df_clean %>% filter(is.na(stop_time_hour) != TRUE), aes(stop_time_hour, ..count../sum(..count..))) + 
#   geom_bar(fill = "#1E90FF") +
#   facet_grid(driver_gender ~ .) +
#   labs(title = "Stop times (hour) by proportion", x = "Stop Time (Hour)", y = "Proportion")
# ggplot(df_clean %>% filter(is.na(stop_time_hour) != TRUE), aes(stop_time_hour, ..count../sum(..count..), group = driver_gender, fill = driver_gender)) + 
#   geom_bar() +
#   labs(title = "Stop times (hour) by proportion", x = "Stop Time (Hour)", y = "Proportion")
ggplot(df_clean %>% filter(is.na(stop_time_hour) != TRUE), aes(stop_time_hour, ..count../sum(..count..), group = driver_gender, fill = driver_gender)) + 
  geom_bar(position = "dodge") +
  labs(title = "Stop times (hour) by proportion", x = "Stop Time (Hour)", y = "Proportion") +
  scale_y_continuous(labels = scales::percent)
ggsave("./EDA_images/62_stop_hour_prop_by_gender.png")
# Notes: Low number of stops from 0300 - 0500 while the highest number of stops occur
#        between 0900 and 1000 possibly coinciding with the high volume of people 
#        driving to work?
#        Other smaller peaks around 1700 and 0100.
#        When comparing the hours (not as a rearranged, ordered factor) graph to 
#        the time (hours and minutes) graph, there are not really any differences in patterns, 
#        so using just the hours graph for investigation is okay, and also easier.
#        Every day has the same number of hours regardless of year or month or day
#        of month, so don't need to weight to normalize.
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
#        At no time of day, do women have more stops than men.

#county_name:
ggplot(df_clean %>%  filter(county_name != ""), aes(county_name)) + 
  geom_bar(fill = "#1E90FF") +
  theme1 +
  labs(title = "Stop count by county", x = "County")
ggsave("./EDA_images/26_county_name.png")

# county name: weighted by population
df_county_small <- df_county[, c(which(colnames(df_county) == "county_name"), 
                                 which(colnames(df_county) == "county_fips"), 
                                 which(colnames(df_county) == "TOT_POP"):which(colnames(df_county) == "HNAC_FEMALE"))]
df_county_small <- df_county_small %>% group_by(county_fips) %>% dplyr::mutate(count = n(), count_prop = count / TOT_POP) %>% distinct(county_fips, .keep_all = TRUE)
ggplot(df_county_small %>% filter(is.na(county_fips) != TRUE), aes(county_name, count_prop)) + 
  geom_bar(stat = "identity", fill = "#1E90FF") +
  theme1 +
  labs(title = "Stops per county, weighted by county population", x = "County", y = "Counts weighted by Population")
ggsave("./EDA_images/27_county_name_weighted.png")
# Notes: The county with the most stops is New Haven County and the one with the
#        one with the least is Litchfield County
#        While New Haven had the largest number of stops, when weighted, it has one
#        of the lowest # of stops/ county population ratios.
#        Tolland County by far, has the highest ratio of stops.

#driver_gender:
ggplot(df_clean, aes(driver_gender)) + 
  geom_bar(fill = "#1E90FF") +
  labs(title = "Stop count by gender", x = "Gender") +
  scale_x_discrete(labels = c("Female", "Male"))
ggsave("./EDA_images/28_driver_gender.png")
ggplot(df_clean, aes(factor(1), ..count.., fill = driver_gender)) +  
  geom_bar(position = "fill") +
  labs(title = "Stop proportion by gender", x = "Gender") +
  #scale_x_discrete(labels = "") + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
ggsave("./EDA_images/29_driver_gender_prop.png")
ggplot(df_clean, aes(driver_gender, ..prop.., group = 1)) +  
  geom_bar(fill = "#1E90FF") +
  labs(title = "Stop proportion by gender", x = "Gender", y = "Proportion of Stop Count") +
  scale_x_discrete(labels = c("Female", "Male"))
ggsave("./EDA_images/29.2_driver_gender_prop.png")

# Driver gender: weighted by population:
gender_pop <- gather(census_whole_state[,4:5], Gender, pop)
gender_pop$Gender[gender_pop$Gender == "TOT_MALE"] <- "M"
gender_pop$Gender[gender_pop$Gender == "TOT_FEMALE"] <- "F"
gender_pop$Gender <- as.factor(gender_pop$Gender)
gender_prop <- setNames(data.frame(table(df_clean$driver_gender, exclude = NULL)), c("Gender", "count")) 
gender_prop <- left_join(gender_prop, gender_pop, by = "Gender") %>%  mutate(gender_prop = count/pop)
ggplot(gender_prop, aes(Gender, gender_prop)) +
  geom_bar(stat = "identity", fill = "#1E90FF") +
  labs(title = "Weighted stop count by population total", y = "Stop Count/ Gender Population") +
  scale_x_discrete(labels = c("Female", "Male"))
ggsave("./EDA_images/30_gender_weighted.png")
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
ggsave("./EDA_images/31_driver_age_bar.png")
table_age <- setNames(data.frame(table(df_clean$driver_age, exclude = NULL)),c("age", "count"))
table_age <- table_age %>% mutate(count_prop = count/ sum(count))
ggplot(table_age %>%  filter(is.na(age) != TRUE), aes(age, count_prop)) + 
  geom_bar(stat = "identity", fill = "#56B4E9") + 
  theme1 + 
  labs(title = "Stop count proportion by driver age", x = "Age", y = "Proportion") +
  annotate("text", x = 50, y = .035, label = "* ages < 15 and >= 80 have been made NA.", size = 3, color = "#696969")
ggsave("./EDA_images/31.2_driver_age_bar_prop.png")
#Notes: The ages with the most stops are from 20-25 with the number of stops decreasing
#       as age increases, save for another small peak at age 40, 45, and 50.
#       Positively skewed age distribution

# driver_age: weighted by population:
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
  labs(title = "Stop counts by age group weighted by \ntotal age group population", x = "Age Group", y = "Age group stop count/ age group population") +
  scale_x_continuous(breaks = 4:16, labels = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79"))
ggsave("./EDA_images/32_agegroup_weighted.png")
# Notes: Weighted by the age populations of Connecticut, ages 20-29 have the most 
#        stops for its population. In general, the age proportion distribution 
#        follows a positive skew bell curve.  

#driver_race_raw:
ggplot(df_clean, aes(driver_race_raw)) + 
  geom_bar(fill = "#56B4E9") +
  labs(title = "Stop count by driver race", x = "Race")
ggsave("./EDA_images/33_driver_race_bar.png")
ggplot(df_clean, aes(driver_race_raw, ..prop.., group = 1)) + 
  geom_bar(fill = "#56B4E9") +
  labs(title = "Stop count proportion by driver race", x = "Race")
ggsave("./EDA_images/34_driver_race_bar_prop.png")
#Notes: Most of the stop population is white, taking up about 75% of the total

# driver_race_raw: weighted by population:
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
  labs(title = "Stop counts by race weighted by race population", x = "Race", y = "Weighted stop counts by race")
ggsave("./EDA_images/35_race_weighted.png")
rtest <- ggplot(table_race, aes(race, pop_prop)) + geom_bar(stat = "identity")
plt_rtest <- ggplot_build(rtest)
plt_rtest$data[[1]]
# Note: In the Census data, Hispanic is not considered a race, it's considered an 
#       ethnicity, so the race populations are not necessarily mutually exclusive.
#       Based on the weights by population, Blacks and whites have the highest
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
ggsave("./EDA_images/36_search_conducted_bar.png")
ggplot(df_clean, aes(search_conducted, ..prop.., group = 1)) + 
  geom_bar(fill = "#56B4E9") +
  labs(title = "Stop count proportion by searches conducted", x = "Search Conducted") +
  annotate("text", x = 2.3, y = .97, label = "*Vehicle searches", size = 3, color = "#696969")
ggsave("./EDA_images/37_search_conducted_bar_prop.png")
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
ggsave("./EDA_images/38_search_type_bar.png")
ggplot(df_clean %>% filter(df_clean$search_type_raw != ""), aes(search_type_raw, ..prop.., group = 1)) + 
  geom_bar(fill = "#56B4E9") +
  annotate("text", x = 2, y = -.025, label = "*An inventory search is a warrantless search of a lawfully \nimpounded vehicle conducted by police.", size = 2) +
  labs(title = "Stop count proportion by search type", x = "Search Type")
ggsave("./EDA_images/39_search_type_bar_prop.png")
#Of those stops where a search was conducted and we have information on the search type that was conducted...
ggplot(df_clean %>% filter(df_clean$search_conducted == TRUE & df_clean$search_type_raw != ""), aes(search_type_raw, ..prop.., group = 1)) + 
  geom_bar(fill = "#56B4E9") +
  annotate("text", x = 2, y = -.025, label = "*An inventory search is a warrantless search of a lawfully \nimpounded vehicle conducted by police.", size = 2) +
  labs(title = "Stop count proportion by search type", x = "Search Type")
ggsave("./EDA_images/39.2_search_type_bar_prop.png")
searchtype <- df_clean %>% filter(search_conducted == TRUE) %>% group_by(search_type_raw) %>% dplyr::summarise(n = n()) %>% mutate(st_prop = n/sum(n))
ggplot(searchtype, aes(search_type_raw, st_prop)) + 
  geom_bar(stat = "identity", fill = "#56B4E9") +
  annotate("text", x = 3, y = -.025, label = "*An inventory search is a warrantless search of a lawfully \nimpounded vehicle conducted by police.", size = 2) +
  labs(title = "Stop count proportion by search type", x = "Search Type", y = "Proportion") +
  scale_x_discrete(labels = c("NA", "Consent", "Inventory", "Other"))
ggsave("./EDA_images/39.3_search_type_bar_prop.png")



#Notes: Consent and Other are the two top search types, with inventory coming in
#       a very far third

#contraband_found:
ggplot(df_clean, aes(contraband_found)) + 
  geom_bar(fill = "#56B4E9") +
  labs(title = "Search count by contraband found status", x = "Contraband Found")
ggsave("./EDA_images/40_contraband_found_bar.png")
ggplot(df_clean, aes(contraband_found, ..prop.., group = 1)) + 
  geom_bar(fill = "#56B4E9") +
  labs(title = "Search count proportion by contraband found status", x = "Contraband Found")
ggsave("./EDA_images/41_contraband_found_bar_prop.png")
#Of those stops where a search was conducted...
ggplot(df_clean %>% filter(search_conducted == TRUE), aes(contraband_found, ..prop.., group = 1)) + 
  geom_bar(fill = "#56B4E9") +
  labs(title = "Search count proportion by contraband found status", x = "Contraband Found")
ggsave("./EDA_images/41.2_contraband_found_bar_prop.png")
c <- ggplot(df_clean, aes(contraband_found, ..prop.., group = 1)) + geom_bar()
plt_c <- ggplot_build(c)
plt_c$data[[1]]
c2 <- ggplot(df_clean %>% filter(search_conducted == TRUE), aes(contraband_found, ..prop.., group = 1)) + geom_bar()
plt_c2 <- ggplot_build(c2)
plt_c2$data[[1]]
#Notes: A very small proportion of contraband was found (0.57%)
#       Of those stops where a search was conducted, ~34.1% of the stops resulted
#       in contraband being found.

#stop_outcome:
ggplot(df_clean %>% filter(is.na(stop_outcome) != TRUE), aes(stop_outcome)) + 
  geom_bar(fill = "#56B4E9") +
  labs(title = "Stop count by stop outcome", x = "Stop Outcome")
ggsave("./EDA_images/42_stop_outcome_bar.png")
ggplot(df_clean %>% filter(is.na(stop_outcome) != TRUE), aes(stop_outcome, ..prop.., group = 1)) + 
  geom_bar(fill = "#56B4E9") +
  labs(title = "Stop count proportion by stop outcome", x = "Stop Outcome", y = "Stop Proportion")
ggsave("./EDA_images/43_stop_outcome_bar_prop.png")
stopoutcome <- df_clean %>% group_by(stop_outcome) %>% dplyr::summarise(n = n()) %>% mutate(so_prop = n/sum(n))
ggplot(stopoutcome, aes(stop_outcome, so_prop)) + 
  geom_bar(stat = "identity", fill = "#56B4E9") +
  labs(title = "Stop count proportion by stop outcome", x = "Stop Outcome", y = "Stop Proportion")
ggsave("./EDA_images/43.2_stop_outcome_bar_prop.png")
ggplot(df_clean, aes(stop_outcome, ..prop.., group = 1)) + 
  geom_bar(fill = "#56B4E9") +
  labs(title = "Stop count proportion by stop outcome", x = "Stop Outcome", y = "Stop Proportion")
ggsave("./EDA_images/43.3_stop_outcome_bar_prop.png")
#Notes: Most stops end in a ticket (70%), with verbal warning coming in a far second.

#is_arrested:
ggplot(df_clean %>% filter(is_arrested != ""), aes(is_arrested)) + 
  geom_bar(fill = "#56B4E9") +
  labs(title = "Stop count by arrested status", x = "Is Arrested")
ggsave("./EDA_images/44_is_arrested_bar.png")
ggplot(df_clean %>% filter(is_arrested != ""), aes(is_arrested, ..prop.., group = 1)) + 
  geom_bar(fill = "#56B4E9") +
  labs(title = "Stop count proportion by arrested status", x = "Is Arrested", y = "Count Proportion")
ggsave("./EDA_images/45_is_arrested_bar_prop.png")
ggplot(df_clean, aes(is_arrested, ..prop.., group = 1)) + 
  geom_bar(fill = "#56B4E9") +
  labs(title = "Stop count proportion by arrested status", x = "Is Arrested", y = "Count Proportion")
ggsave("./EDA_images/45.2_is_arrested_bar_prop.png")
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
ggsave("./EDA_images/46_officer_id_bar.png")
#There are some heavy stoppers so let's narrow the window to investigate the top 50
officer_stops <- setNames(data.frame(table(df_clean$officer_id)), c("officer", "count"))
officer_stops <- officer_stops %>% arrange(desc(count))
officer_stops <- officer_stops %>% mutate(stop_prop = count/ sum(count))
top_50 <- head(officer_stops, 50)
ggplot(top_50, aes(officer, count)) + 
  geom_bar(stat = "identity", fill = "#56B4E9") +
  theme1 +
  labs(title = "Stop counts of top 50 officers", x = "Officer ID")
ggsave("./EDA_images/47_officer_id_top_50.png")
ggplot(top_50, aes(reorder(officer, -stop_prop), stop_prop)) + 
  geom_col(fill = "#56B4E9") + 
  theme1 +
  labs(title = "Stop proportion per officer (top 50)", x = "Officer ID", y = "Proportion")
ggsave("./EDA_images/48_officer_id_top_50_prop.png")
officer_plot2 <- ggplot(officer_stops, aes(count)) + geom_histogram(binwidth = 250)
plt_officer_plot2 <- ggplot_build(officer_plot2)
officer_plot_x_labs2 <- plt_officer_plot2$data[[1]]$x
ggplot(officer_stops, aes(count, ..count../sum(..count..))) + 
  geom_histogram(binwidth = 250, fill = "#191970") +
  scale_x_continuous(breaks = officer_plot_x_labs2, labels = as.character(officer_plot_x_labs2)) + 
  theme1 +
  labs(title = "Proportion of total officers per stop count", x = "# of stops", y = "% of officers") +
  scale_y_continuous(labels = scales::percent)
ggsave("./EDA_images/49_stop_count_by_officers.png")
officer_plot3 <- ggplot(officer_stops, aes(count)) + geom_histogram(binwidth = 50)
plt_officer_plot3 <- ggplot_build(officer_plot3)
officer_plot_x_labs3 <- plt_officer_plot3$data[[1]]$x
ggplot(officer_stops, aes(count, ..count../sum(..count..))) + 
  geom_histogram(binwidth = 50, fill = "#191970") +
  scale_x_continuous(breaks = officer_plot_x_labs3, labels = as.character(officer_plot_x_labs3)) + 
  theme1 +
  labs(title = "Proportion of total officers per stop count", x = "# of stops", y = "% of officers") +
  scale_y_continuous(labels = scales::percent)
ggsave("./EDA_images/49.2_stop_count_by_officers.png")
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
ggsave(file = "./EDA_images/50_stop_count_by_top6_officers.png", officer_g)
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
  annotate("text", x = 10, y = 390, label = "*Days with 0 stops not included in an officer's \naverage calculation.", size = 3, color = "#696969")
ggsave("./EDA_images/51_avg_stops_per_day_bar.png")
df_officer_month <- df_officer
df_officer_month$StopDate <- as.POSIXct(df_officer_month$StopDate, "%Y-%m-%d", tz = "America/New_York")
df_officer_month$month_year <- format(df_officer_month$StopDate, "%m-%Y")
df_officer_month <- df_officer_month %>% group_by(OfficerID, sum_stops, month_year) %>% dplyr::summarise(total_month = sum(count))
df_officer_month_avg <- df_officer_month %>% group_by(OfficerID) %>% dplyr::summarise(avg_per_month = mean(total_month))
ggplot(df_officer_month_avg, aes(avg_per_month)) +
  geom_histogram(binwidth = 10, fill = "#56B4E9") +
  scale_x_continuous(breaks = seq(0,150, by = 10), labels = as.character(seq(0,150, by = 10))) +
  labs(title = "Average Stops/Month by officers", x = "Average stops per month", y = "# of officers")
ggsave("./EDA_images/59_avg_stops_per_month_bar.png")
ggplot(df_officer_month_avg, aes(avg_per_month, ..count../sum(..count..))) +
  geom_histogram(binwidth = 10, fill = "#56B4E9") +
  scale_x_continuous(breaks = seq(0,150, by = 10), labels = as.character(seq(0,150, by = 10))) +
  labs(title = "Average Stops/Month by officers", x = "Average stops per month", y = "# of officers") +
  scale_y_continuous(labels = scales::percent)
ggsave("./EDA_images/60_avg_stops_per_month_bar_prop.png")
top6_officer_per_month <- head(arrange(df_officer_month, desc(sum_stops)), 6*18)
top6_officer_per_month$month_year <- factor(top6_officer_per_month$month_year,
                                            ordered = TRUE,
                                            levels = mo_yr_label)
top6_officer_per_month$OfficerID <- factor(top6_officer_per_month$OfficerID,
                                           ordered = TRUE,
                                           levels = c("706750556", "1000003154", "790642042", "614680420", "1000002362", "1000002598"))
ggplot(top6_officer_per_month, aes(month_year, total_month, group = 1)) +
  geom_line(stat = "identity", color = "black") +
  facet_grid(OfficerID ~ .) +
  theme1 +
  theme(strip.text.y = element_text(angle = 0)) +
  labs(title = "Stops/Month per Officer", x = "Stops per month", y = "# of officers")
ggsave("./EDA_images/58_stops_per_month_line.png")
df_officer2 <- df_officer %>% filter(count > 0) %>% group_by(OfficerID, sum_stops) %>% dplyr::summarise(active_days = n())
ggplot(df_officer2, aes(active_days, sum_stops)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(title = "Total stops by # of active days per officer", x = "# of active days", y = "Total stops")
ggsave("./EDA_images/61_total_stops_per_active_days.png")
# Notes: Most police officers have 2 stops per day on average, while some have
#        as high as 13. 
# > max(avg_stops_officer$avg_stops_per_day)
# [1] 13.31132

#stop_duration:
ggplot(df_clean, aes(stop_duration)) + 
  geom_bar(fill = "#56B4E9") +
  labs(title = "Stop count by stop duration", x = "Stop Duration")
ggsave("./EDA_images/52_stop_duration_bar.png")
ggplot(df_clean, aes(stop_duration, ..prop.., group = 1)) + 
  geom_bar(fill = "#56B4E9") +
  labs(title = "Stop count proportion by stop duration", x = "Stop Duration", y = "Count proportion")
ggsave("./EDA_images/53_stop_duration_bar_prop.png")
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
ggsave("./EDA_images/54_violation_count_bar.png")
ggplot(df_clean, aes(violation_count, ..prop.., group = 1)) + 
  geom_bar(fill = "#56B4E9") +
  labs(title = "Stop count proportion by violation count", x = "Violation Count", y = "Count Proportion")
ggsave("./EDA_images/55_violation_count_bar_prop.png")
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
log_viol_count <- plt_vc$data[[1]]
ggplot(log_viol_count, aes(x, log(prop))) + geom_bar(stat = "identity")
#Notes: Most of the stops (99.3%) only had 1 violation.

#violations:
# Edit df_split for visualization
df_split2 <- df_split %>% gather("violation_new", "n", 'violation_raw_Cell.Phone':'violation_raw_Window.Tint')
ggplot(df_split2, aes(reorder(violation_new, -n), n)) + 
  geom_col(fill = "#56B4E9") + 
  theme1 +
  labs(title = "Stop counts per violation", x = "Violation", y = "Count")
ggsave("./EDA_images/56_violations_bar.png")
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
ggsave("./EDA_images/57_violations_bar_prop.png")
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



# BIVARIATE:

#violations:
# ggplot(df_split2, aes(reorder(violation_new, -n), n, fill = driver_race_raw)) + geom_col() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
# ggplot(df_split2, aes(violation_new, n, fill = driver_race_raw)) + geom_col()
# ggplot(df_split2, aes(reorder(violation_new, -n), n, fill = driver_race_raw)) + geom_col(position = "fill") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
# ggplot(df_split2, aes(reorder(violation_new, -n), n, fill = driver_race_raw)) + geom_col(position = "fill") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#gender by arrest status
# gender arrest status/total stops per gender
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
# Proportions within each gender not of gender within each arrest status:
ggplot(arrest_by_gender, aes(Arrested, percent_gender, fill = Gender)) + 
  geom_bar(stat = "identity") +
  labs(title = "Arrest status proportion for each gender", x = "Arrest Status", y = "Proportion") +
  scale_fill_discrete(labels=c("Female","Male"))
#ggsave("./EDA_images/59_gender_by_arrests_prop.png")
#not really that good of a viz so don't keep; not as easy to read as dodge
ggplot(df_clean, aes(is_arrested, fill = driver_gender)) + 
  geom_bar(aes(y = ..prop.., group = driver_gender), position = "dodge") +
  labs(title = "Arrest status proportion for each gender", x = "Arrest Status", y = "Proportion") +
  scale_fill_discrete(labels=c("Female","Male"), name = "Gender")
ggsave("./EDA_images/60_gender_by_arrests_dodge.png")
ggplot(arrest_by_gender, aes(Gender, percent_gender, fill = Arrested)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Arrest status proportion for each gender", x = "Arrest Status", y = "Proportion")
# another way to visualize
# Using facets isn't as clear as dodge:
ggplot(df_clean, aes(is_arrested, fill = driver_gender)) + 
  geom_bar(aes(y = ..prop.., group = driver_gender), position = "dodge") +
  facet_grid(driver_gender ~ .) +
  labs(title = "Arrest status proportion for each gender", x = "Arrest Status", y = "Proportion") +
  scale_fill_discrete(labels=c("Female","Male"), name = "Gender")
ggplot(arrest_by_gender, aes(Arrested, percent_gender, fill = Gender)) + 
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(Gender ~ .) +
  labs(title = "Arrest status proportion for each gender", x = "Arrest Status", y = "Proportion of gender") +
  scale_fill_discrete(labels=c("Female","Male"))
# getting a better picture of other arrest statuses than FALSE:
ggplot(arrest_by_gender, aes(Arrested, log(percent_gender), fill = Gender)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Arrest status proportion for each gender", x = "Arrest Status", y = "Proportion of gender") +
  scale_fill_discrete(labels=c("Female","Male"))
# -> trying to use log... unsuccessfully
ggplot(arrest_by_gender %>% filter(Arrested == TRUE |is.na(Arrested) == TRUE), aes(Arrested, percent_gender, fill = Gender)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Arrest status proportion for each gender", x = "Arrest Status", y = "Proportion of gender") +
  scale_fill_discrete(labels=c("Female","Male")) +
  annotate("text", x = 2.05, y = .026, label = "*Excludes Arrest Status = FALSE", size = 3, color = "#696969")
ggsave("./EDA_images/63_gender_by_arrests_dodge_no_FALSE.png")
ggplot(arrest_by_gender %>% filter(Arrested == TRUE), aes(Arrested, percent_gender, fill = Gender)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Arrest status proportion for each gender", x = "Arrest Status", y = "Proportion of gender") +
  scale_fill_discrete(labels=c("Female","Male"))
ggsave("./EDA_images/63_redo_gender_by_arrests_dodge_no_FALSE.png")
# Notes: The dodge image more clearly shows that males who are stopped are more 
#        likely to be arrested than females who are stopped.
#        In fact, stopped men are arrested 1.5 times more than stopped women:
#        0.02650022 / 0.01589189 = 1.667531

#arrests by gender
ggplot(arrest_by_gender, aes(Arrested, count, fill = Gender)) + 
  geom_bar(stat = "identity") +
  labs(title = "Arrest status counts for each gender", x = "Arrest Status", y = "Count") +
  scale_fill_discrete(labels=c("Female","Male"))
ggsave("./EDA_images/58_arrest_status_by_gender_count.png")
ggplot(df_clean, aes(is_arrested, ..count../sum(..count..), fill = driver_gender)) + 
  geom_bar(position = "dodge") +
  labs(title = "Arrest status by gender", x = "Arrest Status", y = "Proportion") +
  scale_fill_discrete(labels=c("Female","Male"), name = "Gender") +
  scale_y_continuous(labels = scales::percent)
ggsave("./EDA_images/64_arrest_status_by_gender.png")
ga <- ggplot(df_clean, aes(is_arrested, ..count../sum(..count..), fill = driver_gender)) + geom_bar(position = "dodge")
plt_ga <- ggplot_build(ga)
plt_ga$data[[1]]
# fill           y  count prop     x PANEL group ymin        ymax xmin xmax colour
# 1 #00BFC4 0.635166897 202408    1 1.225     1     2    0 0.635166897 1.00 1.45     NA
# 2 #F8766D 0.325080256 103593    1 0.775     1     1    0 0.325080256 0.55 1.00     NA
# 3 #00BFC4 0.017620164   5615    1 2.225     1     4    0 0.017620164 2.00 2.45     NA
# 4 #F8766D 0.005325275   1697    1 1.775     1     3    0 0.005325275 1.55 2.00     NA
# 5 #00BFC4 0.012119158   3862    1 3.225     1     6    0 0.012119158 3.00 3.45     NA
# 6 #F8766D 0.004688250   1494    1 2.775     1     5    0 0.004688250 2.55 3.00     NA
# size linetype alpha
# 1  0.5        1    NA
# 2  0.5        1    NA
# 3  0.5        1    NA
# 4  0.5        1    NA
# 5  0.5        1    NA
# 6  0.5        1    NA
# Notes: More men are arrested: 5615 (1.76%) compared to 1697 (.53%) women. However, 
#        there are a lot more men stopped in the first place. In fact, men have a 
#        higher percentage/proportion for every status, confirming that they are more
#        populous in the dataset.

#arrests by race:
ggplot(df_clean, aes(is_arrested, ..count../sum(..count..), fill = driver_race_raw)) + 
  geom_bar(position = "dodge") +
  labs(title = "Arrest status distribution by race", x = "Arrest Status", y = "Proportion") +
  scale_fill_discrete(name = "Race") +
  scale_y_continuous(labels = scales::percent)
ggsave("./EDA_images/65_arrest_status_by_race.png")
ggplot(df_clean, aes(is_arrested, ..count../sum(..count..), fill = driver_race_raw)) + 
  geom_bar(position = "fill") +
  labs(title = "Arrest status distribution by race", x = "Arrest Status", y = "Proportion") +
  scale_fill_discrete(name = "Race") +
  scale_y_continuous(labels = scales::percent)
ggsave("./EDA_images/65.2_arrest_status_by_race_fill.png")

#race by arrest status:
ggplot(df_clean, aes(is_arrested, ..prop.., fill = driver_race_raw, group = driver_race_raw)) + 
  geom_bar(position = "dodge") +
  labs(title = "Race by arrest status", x = "Arrest Status", y = "Proportion of race") +
  scale_fill_discrete(name = "Race") +
  scale_y_continuous(labels = scales::percent)
ggsave("./EDA_images/66_race_by_arrest_status.png")
#create table to get closer view of TRUE status
# ra <- ggplot(df_clean, aes(is_arrested, ..prop.., fill = driver_race_raw, group = driver_race_raw)) + 
#       geom_bar(position = "dodge")
# plt_ra <- ggplot_build(ra)
# plt_ra$data[[1]]
race_by_arrest <- setNames(data.frame(table(df_clean$driver_race_raw, df_clean$is_arrested, exclude = NULL)), c("Race", "Arrested", "count")) 
race_totals <- data.frame(race_by_arrest %>% group_by(Race) %>% dplyr::summarise(sum_race = sum(count)))
race_by_arrest <- left_join(race_by_arrest, race_totals, by = "Race")
race_by_arrest <- race_by_arrest %>% mutate(percent_of_race  = count/sum_race)
ggplot(race_by_arrest %>% filter(Arrested == TRUE | is.na(Arrested) == TRUE), aes(Arrested, percent_of_race, fill = Race)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Race by arrest status", x = "Arrest Status", y = "Percent of race") +
  scale_fill_discrete(name = "Race") +
  scale_y_continuous(labels = scales::percent) +
  annotate("text", x = 2, y = .038, label = "*Excludes Arrest Status = FALSE", size = 3, color = "#696969")
ggsave("./EDA_images/67_race_by_arrest_status_no_FALSE.png")
ggplot(race_by_arrest %>% filter(Arrested == TRUE), aes(Arrested, percent_of_race, fill = Race)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Arrests/stops by race", x = "Arrest Status", y = "Percent of race") +
  scale_fill_discrete(name = "Race") +
  scale_y_continuous(labels = scales::percent)
  # + annotate("text", x = 2, y = .038, label = "*Excludes Arrest Status = FALSE", size = 3, color = "#696969")
ggsave("./EDA_images/67_redo_race_by_arrest_status_no_FALSE.png")
race_test <- ggplot(race_by_arrest %>% filter(Arrested == TRUE), aes(Arrested, percent_of_race, fill = Race)) + geom_bar(stat = "identity", position = "dodge")
plt_race_test <- ggplot_build(race_test)
plt_race_test$data[[1]]
ggplot(race_by_arrest, aes(Race, percent_of_race, fill = Arrested)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Arrest proportion for each race", y = "Arrests/stops") +
  scale_y_continuous(labels = scales::percent)
ggsave("./EDA_images/67.2_race_by_arrest_prop.png")
# Notes: When looking at arrest status broken up by race,
#        it isn't a surprise that most arrests (and non-arrests) are White people
#        because they make up a vast majority of the stops.
#        Looking at the percent of arrests per each race, Hispanics followed by Blacks
#        have the highest percentage of arrests for their race. Asians have
#        the lowest. 
#        For all races, most stops don’t end in arrest.

# arrests by gender and race:
race_label <- c(
                  "Asian" = "Asian",
                  "Black" = "Black",
                  "Hispanic" = "Hispanic",
                  "Native American" = "Native\nAmerican",
                  "White" = "White"
)
ggplot(df_clean, aes(is_arrested, ..count../sum(..count..), fill = driver_gender)) + 
  geom_bar() +
  facet_grid(. ~ driver_race_raw, labeller = as_labeller(race_label)) + 
  theme1
  # labs(title = "Arrest status by race", x = "Arrest Status", y = "Proportion") +
  # scale_fill_discrete(name = "Race") +
  # scale_y_continuous(labels = scales::percent)
#ggsave("./EDA_images/65_arrest_status_by_race.png")
#denominator here is entire dataset (318669)... not what want because White has the
#highest proportion because largest amount of stops in dataset

# gender/race by arrest status:
# https://stackoverflow.com/questions/13942844/ggplot-group-by-one-categorical-variable-and-color-by-a-second-one?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa
# https://stackoverflow.com/questions/41688141/grouped-bar-plot-in-ggplot-with-y-values-based-on-combination-of-2-categorical-v?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa 
ggplot(df_clean, aes(is_arrested, ..prop.., group = paste(driver_race_raw, driver_gender), fill = driver_gender)) + 
  geom_bar(position = "dodge") +
  facet_grid(. ~ driver_race_raw, labeller = as_labeller(race_label)) + 
  theme1
#shows gender's proportion of arrest status for each race, essentially arrest status
#   by gender faceted by race.
#   Another way of thinking about it, is it is the arrest status proportion for
#   each gender/race combination.
#another way to visualize-
ggplot(df_clean, aes(is_arrested, ..prop.., group = paste(driver_race_raw, driver_gender))) + 
  geom_bar(position = "dodge") +
  facet_grid(driver_gender ~ driver_race_raw) + 
  theme1
# data
# gra <- ggplot(df_clean, aes(is_arrested, ..prop.., group = paste(driver_race_raw, driver_gender))) + 
#   geom_bar(position = "dodge") +
#   facet_grid(driver_gender ~ driver_race_raw) + 
#   theme1
# plt_gra <- ggplot_build(gra)
# plt_gra$data[[1]]
# <==>
#table(df_clean$driver_gender, df_clean$is_arrested, exclude = NULL)
gender_race_by_arrest <- setNames(data.frame(table(df_clean$driver_gender, df_clean$driver_race_raw, df_clean$is_arrested, exclude = NULL)), c("Gender", "Race", "Arrested", "count")) 
race_totals3 <- data.frame(gender_race_by_arrest %>% group_by(Race, Gender) %>% dplyr::summarise(sum_race_gender = sum(count)))
gender_race_by_arrest2 <- left_join(gender_race_by_arrest, race_totals3, by = c("Race", "Gender"))
gender_race_by_arrest2 <- gender_race_by_arrest2 %>% mutate(percent_of_race_gender  = count/sum_race_gender)
gender_race_by_arrest2 <- gender_race_by_arrest2 %>% mutate(race_gender = paste(Race, Gender))
ggplot(gender_race_by_arrest2, aes(Arrested, percent_of_race_gender, group = interaction(Race, Gender), fill = Gender)) + 
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(. ~ Race, labeller = as_labeller(race_label)) + 
  theme1 +
  labs(title = "Proportion of Arrest Status for each Race/Gender pairing", x = "Arrest Status", y = "Proportion of Race/Gender") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete(labels=c("Female","Male"))
ggsave("./EDA_images/68_race_gender_by_arrest_status.png")
ggplot(gender_race_by_arrest2 %>% filter(Arrested == TRUE), aes(race_gender, percent_of_race_gender)) +
  geom_bar(stat = "identity")
#arrested == TRUE; zooming in
ggplot(gender_race_by_arrest2 %>% filter(Arrested == TRUE), aes(Arrested, percent_of_race_gender, group = interaction(Race, Gender), fill = Gender)) + 
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(. ~ Race, labeller = as_labeller(race_label)) + 
  theme1 +
  labs(title = "Arrest Proportions for each Race/Gender pairing", 
       #subtitle = "Arrest == TRUE",
       x = "Race/Gender", 
       y = "Arrests/Stops") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete(labels=c("Female","Male")) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())
ggsave("./EDA_images/66_race_gender_by_arrest_status_TRUE.png")
ggplot(gender_race_by_arrest2 %>% filter(Arrested == TRUE), aes(Gender, percent_of_race_gender, fill = Race)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Arrest Proportions for each Race/Gender pairing", 
       subtitle = "Arrest == TRUE",
       x = "Race/Gender", 
       y = "Arrests/Stops") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = c("Female", "Male")) +
  theme(axis.ticks.x=element_blank())
ggsave("./EDA_images/66.2_race_gender_by_arrest_status_TRUE.png")
#failed attempt-
race_totals2 <- data.frame(gender_race_by_arrest %>% group_by(Race) %>% dplyr::summarise(sum_race = sum(count)))
gender_race_by_arrest <- left_join(gender_race_by_arrest, race_totals2, by = "Race")
gender_race_by_arrest <- gender_race_by_arrest %>% mutate(percent_of_race  = count/sum_race)
ggplot(gender_race_by_arrest, aes(Arrested, percent_of_race, group = interaction(Race, Gender), fill = Gender)) + 
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(. ~ Race, labeller = as_labeller(race_label)) + 
  theme1
# -> has problem that males are going to take up the majority in all categories because there are significantly more men in database
# Notes: For all races except Native American, Females are more likely to not be
#        arrested.
#        Looking at just the arrests, Hispanic Males are more than 2 times as likely
#        to be arrested when stopped than females who are stopped. 
#        Black Males who are stopped are almost twice as likely to be arrested.
#        By contrast, Native American females are more likely to be arrested when 
#        stopped than their male counterparts. This is the only race where that 
#        is the case. 

#arrests by age:
ggplot(df_clean, aes(driver_age, ..count../sum(..count..))) + 
  geom_bar(position = "dodge") +
  facet_grid(is_arrested ~ .)
age_by_arrest <- setNames(data.frame(table(df_clean$driver_age, df_clean$is_arrested, exclude = NULL)), c("Age", "Arrested", "count")) 
age_by_arrest$Age <- as.integer(as.character(age_by_arrest$Age))
age_counts <- df_clean %>% group_by(driver_age) %>% dplyr::summarise(age_count = n())
age_by_arrest <- left_join(age_by_arrest, age_counts, by = c("Age" = "driver_age"))
age_by_arrest <- age_by_arrest %>% mutate(arrest_to_stops = count/age_count)
ggplot(age_by_arrest, aes(Age, arrest_to_stops, fill = Arrested)) +
  geom_bar(stat = "identity", position = "Dodge")

ggplot(age_by_arrest %>% filter(Arrested == TRUE & is.na(Age) != TRUE), aes(Age, count)) +
  geom_bar(stat = "identity", position = "Dodge", fill = "#1E90FF") + 
  scale_x_continuous(breaks = as.integer(x_breaks), labels = as.character(x_breaks)) +
  labs(title = "Count of arrests by age")
ggsave("./EDA_images/70.4_arrests_by_age.png")

  
#arrests/stop by age:
#since a lot of groups for age, focusing on arrest = TRUE
ggplot(age_by_arrest %>% filter(Arrested == TRUE & is.na(Age) != TRUE), aes(Age, arrest_to_stops)) +
  geom_bar(stat = "identity", position = "Dodge", fill = "#1E90FF") +
  scale_x_continuous(breaks = as.integer(x_breaks), labels = as.character(x_breaks)) +
  annotate("text", x = 65, y = .0425, label = "* ages < 15 and >= 80 have been made NA.", size = 3, color = "#696969") +
  labs(title = "Proportion of arrests to stops by age", y = "Arrests to stops") +
  scale_y_continuous(labels = scales::percent)
ggsave("./EDA_images/70_arrests_to_stop_prop_age.png")

age_gender_arrest <- setNames(data.frame(table(df_clean$driver_age, df_clean$driver_gender, df_clean$is_arrested, exclude = NULL)), c("Age", "Gender", "Arrested", "count")) 
age_gender_arrest$Age <- as.integer(as.character(age_gender_arrest$Age))
age_gender_arrest_count <- age_gender_arrest %>% group_by(Age, Gender) %>% dplyr::summarise(n = sum(count))
age_gender_arrest <- left_join(age_gender_arrest, age_gender_arrest_count, by = c("Age", "Gender"))
age_gender_arrest <- age_gender_arrest %>% mutate(arrest_to_stops = count/n)

ggplot(age_gender_arrest %>% filter(Arrested == TRUE), aes(Age, arrest_to_stops, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge")
ggsave("./EDA_images/70.2_arrests_to_stop_prop_age_gender.png")

df_clean$AGEGRP <- vector(mode="character", length = nrow(df_clean))
df_clean$AGEGRP[df_clean$driver_age >= 15 & df_clean$driver_age <= 19] <- 4
df_clean$AGEGRP[df_clean$driver_age >= 20 & df_clean$driver_age <= 24] <- 5
df_clean$AGEGRP[df_clean$driver_age >= 25 & df_clean$driver_age <= 29] <- 6
df_clean$AGEGRP[df_clean$driver_age >= 30 & df_clean$driver_age <= 34] <- 7
df_clean$AGEGRP[df_clean$driver_age >= 35 & df_clean$driver_age <= 39] <- 8
df_clean$AGEGRP[df_clean$driver_age >= 40 & df_clean$driver_age <= 44] <- 9
df_clean$AGEGRP[df_clean$driver_age >= 45 & df_clean$driver_age <= 49] <- 10
df_clean$AGEGRP[df_clean$driver_age >= 50 & df_clean$driver_age <= 54] <- 11
df_clean$AGEGRP[df_clean$driver_age >= 55 & df_clean$driver_age <= 59] <- 12
df_clean$AGEGRP[df_clean$driver_age >= 60 & df_clean$driver_age <= 64] <- 13
df_clean$AGEGRP[df_clean$driver_age >= 65 & df_clean$driver_age <= 69] <- 14
df_clean$AGEGRP[df_clean$driver_age >= 70 & df_clean$driver_age <= 74] <- 15
df_clean$AGEGRP[df_clean$driver_age >= 75 & df_clean$driver_age <= 79] <- 16
age_by_arrest2 <- setNames(data.frame(table(df_clean$AGEGRP, df_clean$is_arrested, exclude = NULL)), c("AgeGroup", "Arrested", "count")) 
age_by_arrest2$AgeGroup <- as.integer(as.character(age_by_arrest2$AgeGroup))
#age_counts2 <- age_by_arrest2%>% group_by(AgeGroup) %>% dplyr::summarise(age_count = n())
age_counts2 <- data.frame(age_by_arrest2 %>% group_by(AgeGroup) %>% dplyr::summarise(age_count = sum(count)))
age_by_arrest2 <- left_join(age_by_arrest2, age_counts2, by = "AgeGroup")
age_by_arrest2 <- age_by_arrest2 %>% mutate(arrest_to_stops = count/age_count)
ggplot(age_by_arrest2 %>% filter(Arrested == TRUE & is.na(AgeGroup) != TRUE), aes(AgeGroup, arrest_to_stops)) +
  geom_bar(stat = "identity", fill = "#1E90FF") +
  scale_x_continuous(breaks = 4:16, labels = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79")) +
  labs(title = "Arrests/Stops by age group" , x = "Age Group", y = "Arrests/Stops")
ggsave("./EDA_images/70.3_arrests_to_stop_prop_agegrp.png")


# Notes: The proportion of arrests to stops for each age doesn’t as closely follow 
#        the positive skew of stops by age as I would expect. However, the 
#        proportion of arrests to stops does decrease as age increases.
#        There are high ratios of arrests for ages 15, 69, 73, and 78.

#arrests by county:
ggplot(df_clean, aes(is_arrested, ..count../sum(..count..), fill = county_name)) + 
  geom_bar(position = "dodge")
# at brief inspection, the arrest proportions seem to match the stop proportions

#county by arrest status:
# need to create table to remove NA
county_by_arrest <- setNames(data.frame(table(df_clean$county_name, df_clean$is_arrested, exclude = NULL)), c("County", "Arrested", "count")) 
county_totals <- data.frame(county_by_arrest %>% group_by(County) %>% dplyr::summarise(sum_county = sum(count)))
county_by_arrest <- left_join(county_by_arrest, county_totals, by = "County")
county_by_arrest <- county_by_arrest %>% mutate(percent_of_county  = count/sum_county)
ggplot(county_by_arrest %>% filter(County != ""), aes(Arrested, percent_of_county, fill = County)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Arrest status by county", x = "Arrest Status", y = "Proportion") +
  scale_y_continuous(labels = scales::percent)
# only arrested == TRUE:
ggplot(county_by_arrest %>% filter(County != "" & Arrested == TRUE), aes(Arrested, percent_of_county, fill = County)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Ratio of arrests to stops by county", x = "", y = "Proportion") +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
ggsave("./EDA_images/72_proportion_of_stops_that_end_in_arrest_county.png")

county_by_arrest2 <- setNames(data.frame(table(df_clean$county_name, df_clean$driver_race_raw, df_clean$is_arrested, exclude = NULL)), c("County", "Race", "Arrested", "count")) 
county_totals2 <- data.frame(county_by_arrest2 %>% group_by(County, Race) %>% dplyr::summarise(sum_county = sum(count)))
county_by_arrest2 <- left_join(county_by_arrest2, county_totals2, by = c("County", "Race"))
county_by_arrest2 <- county_by_arrest2 %>% mutate(percent_of_county  = count/sum_county)
ggplot(county_by_arrest2 %>% filter(Arrested == TRUE & County != ""), aes(County, percent_of_county, fill = Race)) +
  geom_bar(stat = "identity", position = "dodge")
ggsave("./EDA_images/72.4_proportion_of_stops_that_end_in_arrest_county_race.png")


census_county_gender <- census_county %>% gather("Gender", "n", 9:10) %>% select(CTYNAME, TOT_POP, Gender, n)
census_county_gender <- census_county_gender %>% mutate(prop = n/TOT_POP)
ggplot(census_county_gender, aes(CTYNAME, prop, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme1
ggsave("./EDA_images/72.2_proportion_gender_in_counties.png")
#male/female spread of the counties aren't noticeable

census_county_race <- census_county %>% dplyr::mutate(Black = BAC_MALE + BAC_FEMALE, 
                                                      White = WAC_MALE + WAC_FEMALE,
                                                      Hispanic = H_MALE + H_FEMALE,
                                                      NativeAmerican = IAC_MALE + IAC_FEMALE,
                                                      Asian = AAC_MALE + AAC_FEMALE)
census_county_race <- census_county_race %>% select(CTYNAME, TOT_POP, Black, White, Hispanic, NativeAmerican, Asian)
census_county_race <- census_county_race %>% gather(Race, n, 3:7) 
census_county_race <- census_county_race %>% mutate(prop = n/TOT_POP)
ggplot(census_county_race, aes(CTYNAME, prop, fill = Race)) +
  geom_bar(stat = "identity", position = "dodge")
ggplot(census_county_race, aes(Race, prop, fill = CTYNAME)) +
  geom_bar(stat = "identity", position = "dodge")
ggsave("./EDA_images/72.3_proportion_race_in_counties.png")
# Notes: Those who are stopped in New London have a higher chance of being arrested,
#        followed by Windham and Hartford.
#        Stops in Middlesex have the smallest proportion that end in arrest.

#arrests by officer:
ggplot(df_clean, aes(is_arrested, ..count../sum(..count..), group = officer_id)) + 
  geom_bar(position = "dodge")
# since a lot to parse through, just focus on arrests = TRUE
arrests_by_officer <- setNames(data.frame(table(df_clean$officer_id, df_clean$is_arrested, exclude = NULL)), c("Officer", "Arrested", "count")) 
officer_totals <- data.frame(arrests_by_officer %>% group_by(Officer) %>% dplyr::summarise(sum_officer = sum(count)))
arrests_by_officer <- left_join(arrests_by_officer, officer_totals, by = "Officer")
arrests_by_officer <- arrests_by_officer %>% mutate(percent_of_stops  = count/sum_officer)
ggplot(arrests_by_officer %>% filter(Arrested == TRUE), aes(reorder(Officer, -percent_of_stops), percent_of_stops, group = Officer)) +
  geom_bar(stat = "identity", position = "dodge", fill = "#1E90FF") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "% of stops that end in arrest per officer", x = "Officer", y = "% of arrests/stops") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
ggsave("./EDA_images/73_arrests_by_officer.png")  
#There are some officers who arrest 100% of the time they stop-- let's investigate
top_arresting_officers <- arrests_by_officer %>% filter(Arrested == TRUE) %>% arrange(desc(percent_of_stops))
top_arresting_officers <- head(top_arresting_officers, 50)
ggplot(top_arresting_officers, aes(reorder(Officer, -percent_of_stops), percent_of_stops, group = Officer)) +
  geom_bar(stat = "identity", position = "dodge", fill = "#1E90FF") +
  theme1 +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Arrests by officer (top 50)", x = "Officer", y = "% of arrests/stops") 
ggsave("./EDA_images/74_arrests_by_officer_top_50.png") 
#investigate those with 100% arrests
arrests_only_officers <- arrests_by_officer %>% filter(Arrested == TRUE & percent_of_stops == as.numeric(1))
ggplot(arrests_only_officers, aes(Officer, sum_officer, group = Officer)) +
  geom_bar(stat = "identity", position = "dodge", fill = "#1E90FF") +
  theme1 +
  labs(title = "Count of stops for officers with 100% arrests", y = "Stop/arrest count")
ggsave("./EDA_images/75_arrests_by_officer_100percent.png") 
# -> officers with 100% arrests have either 1 or 3 stops/arrests total so not concerning
#investigating those with over 50% arrest rate
arrests_over50_officers <- arrests_by_officer %>% filter(Arrested == TRUE & percent_of_stops > as.numeric(.5))
ggplot(arrests_over50_officers, aes(Officer, sum_officer, group = Officer)) +
  geom_bar(stat = "identity", position = "dodge", fill = "#1E90FF") +
  theme1 +
  labs(title = "Count of stops for officers with > 50% arrests", y = "Stop/arrest count")
ggsave("./EDA_images/76_arrests_by_officer_50percent.png") 
#investigate the officer with over 15 stops:
inv_off <- arrests_by_officer %>% filter(Officer == "857851420")
# has a 68.75% arrest rate which might be concerning
#Notes: There are some officers with a high proportion of arrests to stops, but the 
#       ones with 100% proportion had very few stops/arrests so it’s not concerning.
#       Looking at the ones with >50% arrest rate follows a similar trend of a low 
#       number of dataset lifetime stops. There is one outlier, officer 857851420 
#       who has a 68.75% arrest rate. 
#       In general, most officers have an arrest rate of 25% or lower.

#arrests per month/year:
ggplot(df_clean %>% filter(is_arrested == TRUE), aes(month_year)) +
  geom_bar(fill = "#1E90FF") +
  #geom_smooth(se = FALSE, color = "red") + 
  theme1 +
  labs(title = "Arrests over time (month/year)", x = "Month/Year", y = "Count")
ggsave("./EDA_images/77.2_arrests_over_time_mo_yr.png")
my <- df_clean %>% filter(is_arrested == TRUE)
month_yr_arrests <- setNames(data.frame(table(my$month_year)),c("month_year", "count"))
ggplot(month_yr_arrests, aes(as.numeric(month_year), count)) +
  geom_bar(stat = "identity", fill = "#1E90FF") +
  geom_smooth(se = FALSE, color = "red") + 
  theme1 +
  labs(title = "Arrests over time (month/year)", x = "Month/Year", y = "Count") +
  scale_x_continuous(breaks = as.numeric(month_yr_arrests$month_year), labels = mo_yr_label)
ggsave("./EDA_images/77.3_arrests_over_time_mo_yr.png")

ggplot(df_clean %>% filter(is_arrested == TRUE), aes(month_year, ..prop.., group = 1)) +
  geom_bar(fill = "#1E90FF") +
  theme1 +
  labs(title = "Arrests over time (month/year)", x = "Month/Year", y = "Proportion") +
  scale_y_continuous(labels = scales::percent)
ggsave("./EDA_images/77_arrests_over_time_mo_yr_prop.png")
# Notes: Roughly, the arrests over time (month/year) follow the stops over time
#        trends with low arrests in the winter months and more during the warmer, 
#        summer months. 

#arrests/stops by month/year:
arrests_mo_yr <- setNames(data.frame(table(df_clean$month_year, df_clean$is_arrested, exclude = NULL)), c("moyr", "Arrested", "count")) 
mo_yr_totals <- data.frame(arrests_mo_yr %>% group_by(moyr) %>% dplyr::summarise(sum_stops = sum(count)))
arrests_mo_yr <- left_join(arrests_mo_yr, mo_yr_totals, by = "moyr")
arrests_mo_yr <- arrests_mo_yr %>% mutate(arrests_per_stops  = count/sum_stops)
ggplot(arrests_mo_yr %>% filter(Arrested == TRUE), aes(moyr, arrests_per_stops)) +
  geom_bar(stat = "identity", fill = "#1E90FF") +
  theme1 +
  labs(title = "% of stops that end in arrest over time (month/year)", x = "Month/Year", y = "Arrests/stops") +
  scale_y_continuous(labels = scales::percent)
ggsave("./EDA_images/78_arrests_per_stops_over_time_mo_yr_prop.png")
#Notes: There is a higher ratio of arrests to stops in 11/2014 with almost 4%, 
#       which the next highest, 02/2014 is at 3%
#       For most months, the arrest to stops ratio is around 2%

#arrests per day:
arrests_per_day <- data.frame(df_clean %>% group_by(day_of_month, is_arrested) %>% dplyr::summarise(sum_arrests = n()))
days_df$day <- factor(days_df$day,
                      ordered = TRUE,
                      levels = days_label)
arrests_per_day <- left_join(arrests_per_day, days_df, by = c("day_of_month" = "day"))
arrests_per_day <- arrests_per_day %>% mutate(arrests_per_stop  = sum_arrests/count, arrests_weighted = sum_arrests/occurrence)  
ggplot(arrests_per_day %>% filter(is_arrested == TRUE), aes(day_of_month, arrests_per_stop)) +
  geom_bar(stat = "identity", fill = "#1E90FF") + 
  labs(title = "% of stops that end in arrest by day of month", x = "Day of Month", y = "Arrests/stops") +
  scale_y_continuous(labels = scales::percent)
ggsave("./EDA_images/79_arrests_per_stops_day_of_month_prop.png")
ggplot(arrests_per_day %>% filter(is_arrested == TRUE), aes(day_of_month, arrests_weighted)) +
  geom_bar(stat = "identity")

ggplot(arrests_per_day %>% filter(is_arrested == TRUE), aes(as.numeric(day_of_month), arrests_per_stop)) +
  geom_bar(stat = "identity", fill = "#1E90FF") + 
  geom_smooth(se = FALSE, color = "red") + 
  labs(title = "% of stops that end in arrest by day of month", x = "Day of Month", y = "Arrests/stops") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = as.numeric(arrests_per_day$day_of_month), labels = as.character(arrests_per_day$day_of_month))
ggsave("./EDA_images/79.2_arrests_per_stops_day_of_month_prop_w_LOESS.png")

#my <- df_clean %>% filter(is_arrested == TRUE)
day_arrests <- setNames(data.frame(table(my$day_of_month)),c("day_of_month", "count"))
ggplot(day_arrests, aes(as.numeric(day_of_month), count)) +
  geom_bar(stat = "identity", fill = "#1E90FF") +
  geom_smooth(se = FALSE, color = "red") + 
  theme1 +
  labs(title = "Arrests over day of month", x = "Day of Month", y = "Count") +
  scale_x_continuous(breaks = as.numeric(day_arrests$day_of_month), labels = as.character(day_arrests$day_of_month))
ggsave("./EDA_images/79.3_arrests_over_time_day.png")

# -> similar trend/shape to arrests_per_stop
#Notes: The beginning and middle of the month have a high ratio of stops that end 
#       in arrests (days 1 and 14).
#       Other high arrests/stops ratio days are 22 and 28.

#arrests per day of week:
arrests_per_day_of_week <- data.frame(df_clean %>% group_by(day_of_week, is_arrested) %>% dplyr::summarise(sum_arrests = n()))
arrests_dow_total <- arrests_per_day_of_week %>% group_by(day_of_week) %>% dplyr::summarise(sum_count = sum(sum_arrests))
arrests_per_day_of_week  <- left_join(arrests_per_day_of_week , arrests_dow_total, by = "day_of_week")
arrests_per_day_of_week <- arrests_per_day_of_week %>% mutate(arrests_per_stop  = sum_arrests/sum_count)  
ggplot(arrests_per_day_of_week %>% filter(is_arrested == TRUE), aes(day_of_week, arrests_per_stop)) +
  geom_bar(stat = "identity", fill = "#1E90FF") +
  labs(title = "% of stops that end in arrest by day of week", x = "Day of Week", y = "Arrests/stops") +
  scale_y_continuous(labels = scales::percent)
ggsave("./EDA_images/80_arrests_per_stops_day_of_week_prop.png")
# Notes: Saturdays and Sundays have the highest ratio of arrests to stops with 
#        Friday in third, indicating that stops over the weekend have a higher 
#        likelihood of ending in arrests.

#Weighted:
df_clean_arrests <- df_clean %>% filter(is_arrested == TRUE)


#time:
arrest_time <- setNames(data.frame(table(df_clean$stop_time_hour, df_clean$is_arrested, exclude = NULL)), c("Hour", "Arrested", "count")) 
arrest_time_totals <- data.frame(arrest_time %>% group_by(Hour) %>% dplyr::summarise(sum_stops = sum(count)))
arrest_time <- left_join(arrest_time, arrest_time_totals, by = "Hour")
arrest_time <- arrest_time %>% mutate(arrests_per_stops  = count/sum_stops)
ggplot(arrest_time %>% filter(Arrested == TRUE & is.na(Hour) != TRUE), aes(Hour, arrests_per_stops)) +
  geom_bar(stat = "identity", fill = "#1E90FF") +
  labs(title = "Arrests/stop by hour")
ggsave("./EDA_images/79.4_arrests_by_hour.png")


#gender:
gender_arrests <- df_clean_arrests %>% group_by(driver_gender) %>% dplyr::summarise(gender_arrested_count = n())
gender_arrests <- left_join(gender_arrests, gender_prop, by = c("driver_gender" = "Gender"))
gender_arrests <- gender_arrests %>% mutate(arrests_to_pop = gender_arrested_count/pop)
ggplot(gender_arrests, aes(driver_gender, arrests_to_pop)) +
  geom_bar(stat = "identity", fill = "#1E90FF") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Arrests by gender weighted by gender population", x = "Gender", y = "Arrests weighted by population") +
  scale_x_discrete(labels = c("Female", "Male"))
ggsave("./EDA_images/69_gender_arrests_pop_weighted.png")
#Notes: Similar to stops in general, males have a much 
#       higher percentage of their population arrested than females. 

#race:
race_arrests <- df_clean_arrests %>% group_by(driver_race_raw) %>% dplyr::summarise(race_arrested_count = n())
race_arrests <- left_join(race_arrests, table_race, by = c("driver_race_raw" = "race"))
race_arrests <- race_arrests %>% mutate(arrests_to_pop = race_arrested_count/total_pop)
ggplot(race_arrests, aes(driver_race_raw, arrests_to_pop)) +
  geom_bar(stat = "identity", fill = "#1E90FF") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Arrests by race weighted by race population", x = "Race", y = "Arrests weighted by population")
ggsave("./EDA_images/71_race_arrests_pop_weighted.png")
#Notes: Hispanics are the third highest in terms of stops when weighted by race 
#       population. However, they have the highest proportion of arrests to their 
#       population. With Blacks coming in second and Whites coming in third.
#       This coincides with the race by arrest status analysis that found Hispanics 
#       have the highest proportion of arrests per stops.

#arrests by search conducted:
searches_conducted <- setNames(data.frame(table(df_clean$search_conducted, df_clean$is_arrested, exclude = NULL)), c("Search_Conducted", "Arrested", "count")) 
searches_total <- searches_conducted %>% group_by(Search_Conducted) %>% dplyr::summarise(total_count = sum(count))
searches_conducted <- left_join(searches_conducted, searches_total, by = "Search_Conducted")
searches_conducted<- searches_conducted %>% mutate(arrests_to_pop = count/total_count)
ggplot(searches_conducted, aes(Search_Conducted, arrests_to_pop, fill = Arrested)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Ratio of arrests/stops by search conducted", x = "Search Conducted", y = "Arrests/Stops") +
  scale_y_continuous(labels = scales::percent)
ggsave("./EDA_images/81_arrests_to_stops_search_conducted.png")

sc_nums <- ggplot(searches_conducted, aes(Search_Conducted, arrests_to_pop, fill = Arrested)) + geom_bar(stat = "identity", position = "dodge")
plt_sc_nums <- ggplot_build(sc_nums)
plt_sc_nums$data[[1]]
# 0.274193548/0.018669994 => 14.68632

ggplot(searches_conducted %>% filter(Arrested == TRUE), aes(Search_Conducted, arrests_to_pop)) +
  geom_bar(stat = "identity", fill = "#1E90FF") +
  labs(title = "Ratio of stops that end in arrest by search conducted", x = "Search Conducted", y = "Arrests/Stops") +
  scale_y_continuous(labels = scales::percent)
ggsave("./EDA_images/82_arrests_to_stops_search_conducted_TRUE.png")

searches_conducted2 <- setNames(data.frame(table(df_clean$search_conducted, df_clean$driver_gender, df_clean$is_arrested, exclude = NULL)), c("Search_Conducted", "Gender", "Arrested", "count")) 
searches_total2 <- searches_conducted2 %>% group_by(Search_Conducted, Gender) %>% dplyr::summarise(total_count = sum(count))
searches_conducted2 <- left_join(searches_conducted2, searches_total2, by = c("Search_Conducted", "Gender"))
searches_conducted2 <- searches_conducted2 %>% mutate(arrests_to_pop = count/total_count)
ggplot(searches_conducted2, aes(Gender, arrests_to_pop, fill = Arrested)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(. ~ Search_Conducted)

searches_conducted3 <- setNames(data.frame(table(df_clean$search_conducted, df_clean$driver_race_raw, df_clean$is_arrested, exclude = NULL)), c("Search_Conducted", "Race", "Arrested", "count")) 
searches_total3 <- searches_conducted3 %>% group_by(Search_Conducted, Race) %>% dplyr::summarise(total_count = sum(count))
searches_conducted3 <- left_join(searches_conducted3, searches_total3, by = c("Search_Conducted", "Race"))
searches_conducted3 <- searches_conducted3 %>% mutate(arrests_to_pop = count/total_count)
ggplot(searches_conducted3, aes(Race, arrests_to_pop, fill = Arrested)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(. ~ Search_Conducted) +
  labs(title = "Arrests/Stops for Searches Conducted by Race", x = "Search Conducted by Race", y = "Arrests/Stops") +
  theme1
ggsave("./EDA_images/82.2_arrests_to_stops_search_conducted_byrace.png")
ggplot(searches_conducted3 %>% filter(Arrested == TRUE & Search_Conducted == FALSE), aes(Race, arrests_to_pop, fill = Arrested)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(. ~ Search_Conducted) +
  labs(title = "Arrests/Stops for Searches Conducted by Race", x = "Search Conducted by Race", y = "Arrests/Stops") +
  theme1
ggplot(searches_conducted3 %>% filter(Arrested == TRUE), aes(Race, arrests_to_pop, fill = Arrested)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(. ~ Search_Conducted) +
  labs(title = "Arrests/Stops for Searches Conducted by Race", x = "Search Conducted by Race", y = "Arrests/Stops") +
  theme1
ggsave("./EDA_images/82.3_arrests_to_stops_search_conducted_byrace.png")


sc_race <- setNames(data.frame(table(df_clean$search_conducted, df_clean$driver_race_raw, exclude = NULL)), c("Search_Conducted", "Race", "count")) 
sc_race_total <- sc_race %>% group_by(Search_Conducted) %>% dplyr::summarise(total_count = sum(count))
sc_race <- left_join(sc_race, sc_race_total, by = c("Search_Conducted"))
sc_race <- sc_race %>% mutate(prop = count/total_count)
ggplot(sc_race, aes(Search_Conducted, prop, fill = Race)) +
  geom_bar(stat = "identity", position = "dodge")

sc_race2 <- setNames(data.frame(table(df_clean$search_conducted, df_clean$driver_race_raw, exclude = NULL)), c("Search_Conducted", "Race", "count")) 
sc_race_total2 <- sc_race2 %>% group_by(Race) %>% dplyr::summarise(total_count = sum(count))
sc_race2 <- left_join(sc_race2, sc_race_total2, by = "Race")
sc_race2 <- sc_race2 %>% mutate(prop = count/total_count)
ggplot(sc_race2, aes(Race, prop, fill = Search_Conducted)) +
  geom_bar(stat = "identity", position = "dodge")



search_conducted2 <- setNames(data.frame(table(df_clean$search_conducted, df_clean$contraband_found, df_clean$is_arrested, exclude = NULL)), c("Search_Conducted", "Contraband_Found", "Arrested", "count")) 
search_conducted2_total <- search_conducted2 %>% group_by(Search_Conducted) %>% dplyr::summarise(total_count = sum(count))
search_conducted2 <- left_join(search_conducted2, search_conducted2_total, by = "Search_Conducted")
search_conducted2 <- search_conducted2 %>% mutate(arrests_to_pop = count/total_count)
ggplot(search_conducted2 %>% filter(Search_Conducted == TRUE), aes(Contraband_Found, arrests_to_pop, fill = Arrested)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Ratio of arrests/stops by Contraband Found status \nfor Search Conducted = TRUE", x = "Contraband Found", y = "Arrests/Stops") +
  scale_y_continuous(labels = scales::percent)
ggsave("./EDA_images/81_additional_arrests_to_stops_search_conducted.png")



# Notes: Most stops don’t end in arrest, regardless of whether a search was 
#        conducted. However, if a search was conducted, there is 14.69 times more of 
#        a chance the stop will end in an arrest than if there wasn’t a search 
#        conducted

#search type by arrests:
#only looking for stops where search_conducted == TRUE
st_df <- df_clean %>% filter(search_conducted == TRUE)
search_types <- setNames(data.frame(table(st_df$search_type, st_df$is_arrested, exclude = NULL)), c("Search_Type", "Arrested", "count")) 
search_types_total <- search_types %>% group_by(Search_Type) %>% dplyr::summarise(total_count = sum(count))
search_types <- left_join(search_types, search_types_total, by = "Search_Type")
search_types <- search_types %>% mutate(arrests_to_pop = count/total_count)
ggplot(search_types, aes(Search_Type, arrests_to_pop, fill = Arrested)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Ratio of arrests/stops by search type", x = "Search Type", y = "Arrests/Stops") +
  scale_y_continuous(labels = scales::percent)
ggsave("./EDA_images/83_arrests_to_stops_search_type.png")

search_types_raw <- setNames(data.frame(table(st_df$search_type_raw, st_df$is_arrested, exclude = NULL)), c("Search_Type", "Arrested", "count")) 
search_types_raw_total <- search_types_raw %>% group_by(Search_Type) %>% dplyr::summarise(total_count = sum(count))
search_types_raw <- left_join(search_types_raw, search_types_raw_total, by = "Search_Type")
search_types_raw <- search_types_raw %>% mutate(arrests_to_pop = count/total_count)
ggplot(search_types_raw, aes(Search_Type, arrests_to_pop, fill = Arrested)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Ratio of arrests/stops by search type", x = "Search Type", y = "Arrests/Stops") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = c("NA", "Consent", "Inventory", "Other"))
ggsave("./EDA_images/83_redo_arrests_to_stops_search_type.png")

ggplot(search_types %>% filter(Arrested == TRUE), aes(Search_Type, arrests_to_pop)) +
  geom_bar(stat = "identity", fill = "#1E90FF") +
  labs(title = "Ratio of arrests/stops by search type", x = "Search Type", y = "Arrests/Stops") +
  scale_y_continuous(labels = scales::percent)
ggsave("./EDA_images/84_arrests_to_stops_search_type_TRUE.png")

search_types2 <- setNames(data.frame(table(st_df$search_type, st_df$contraband_found, st_df$is_arrested, exclude = NULL)), c("Search_Type", "Contraband_Found", "Arrested", "count")) 
search_types_total2 <- search_types2 %>% group_by(Search_Type, Contraband_Found) %>% dplyr::summarise(total_count = sum(count))
search_types2 <- left_join(search_types2, search_types_total2, by = c("Search_Type", "Contraband_Found"))
search_types2 <- search_types2 %>% mutate(arrests_to_pop = count/total_count)
ggplot(search_types2, aes(Contraband_Found, arrests_to_pop, fill = Arrested)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(. ~ Search_Type) +
  labs(title = "Ratio of arrests/stops by Contraband Found and Search Type", x = "Contraband Found", y = "Arrests/Stops") +
  scale_y_continuous(labels = scales::percent)
ggsave("./EDA_images/84.2_arrests_to_stops_search_type_contraband.png")

search_types3 <- setNames(data.frame(table(st_df$search_type, st_df$contraband_found, st_df$driver_race_raw,st_df$is_arrested, exclude = NULL)), c("Search_Type", "Contraband_Found", "Race","Arrested", "count")) 
search_types_total3 <- search_types3 %>% group_by(Search_Type, Contraband_Found,Race) %>% dplyr::summarise(total_count = sum(count))
search_types3 <- left_join(search_types3, search_types_total3, by = c("Search_Type", "Contraband_Found", "Race"))
search_types3 <- search_types3 %>% mutate(arrests_to_pop = count/total_count)
ggplot(search_types3, aes(Contraband_Found, arrests_to_pop, fill = Arrested)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(Race ~ Search_Type)
ggsave("./EDA_images/84.3_arrests_to_stops_search_type_contraband_race.png")


#Notes: Most of the inventory searches ended in arrest while all other search types 
#       mostly did not end in arrest
#       Additionally, inventory search types have the highest arrests/stops ratios

#contraband found arrest status distribution:
contraband <- setNames(data.frame(table(st_df$contraband_found, st_df$is_arrested, exclude = NULL)), c("Contraband", "Arrested", "count")) 
contraband_total <- contraband %>% group_by(Contraband) %>% dplyr::summarise(total_count = sum(count))
contraband <- left_join(contraband, contraband_total, by = "Contraband")
contraband <- contraband %>% mutate(arrests_to_pop = count/total_count)
ggplot(contraband, aes(Contraband, arrests_to_pop, fill = Arrested)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Arrest Status distribution per contraband found status", y = "Arrests/stops") +
  scale_y_continuous(labels = scales::percent)
ggsave("./EDA_images/85_arrests_to_stops_contraband.png")

contraband2 <- setNames(data.frame(table(st_df$contraband_found, st_df$driver_race_raw, st_df$is_arrested, exclude = NULL)), c("Contraband", "Race","Arrested", "count")) 
contraband_total2 <- contraband2 %>% group_by(Contraband, Race) %>% dplyr::summarise(total_count = sum(count))
contraband2 <- left_join(contraband2, contraband_total2, by = c("Contraband","Race"))
contraband2 <- contraband2 %>% mutate(arrests_to_pop = count/total_count)
ggplot(contraband2, aes(Race, arrests_to_pop, fill = Arrested)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(. ~ Contraband)
ggsave("./EDA_images/85.2_arrests_to_stops_contraband_race.png")

contraband3 <- setNames(data.frame(table(st_df$contraband_found, st_df$driver_gender, st_df$is_arrested, exclude = NULL)), c("Contraband", "Gender","Arrested", "count")) 
contraband_total3 <- contraband3 %>% group_by(Contraband, Gender) %>% dplyr::summarise(total_count = sum(count))
contraband3 <- left_join(contraband3, contraband_total3, by = c("Contraband","Gender"))
contraband3 <- contraband3 %>% mutate(arrests_to_pop = count/total_count)
ggplot(contraband3, aes(Gender, arrests_to_pop, fill = Arrested)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(. ~ Contraband)
ggsave("./EDA_images/85.3_arrests_to_stops_contraband_gender.png")


ggplot(contraband %>% filter(Arrested == TRUE), aes(Contraband, arrests_to_pop)) +
  geom_bar(stat = "identity", fill = "#1E90FF") +
  labs(title = "Arrests per contraband found status", y = "Arrests/stops") +
  scale_y_continuous(labels = scales::percent)
ggsave("./EDA_images/86_arrests_to_stops_contraband_TRUE.png")
# Notes: Most stops don’t end in arrest, regardless of contraband found status
#        A top that has contraband found is about 30% more likely to end in arrest

# Stop duration by arrests:
stop_durations <- setNames(data.frame(table(df_clean$stop_duration, df_clean$is_arrested, exclude = NULL)), c("Stop_Duration", "Arrested", "count")) 
stop_durations_total <- stop_durations%>% group_by(Stop_Duration) %>% dplyr::summarise(total_count = sum(count))
stop_durations <- left_join(stop_durations, stop_durations_total, by = "Stop_Duration")
stop_durations <- stop_durations %>% mutate(arrests_to_pop = count/total_count)
ggplot(stop_durations, aes(Stop_Duration, arrests_to_pop, fill = Arrested)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Arrest status distribution per stop duration", x = "Stop Duration", y = "Arrests/stops") +
  scale_y_continuous(labels = scales::percent)
ggsave("./EDA_images/87_arrests_to_stops_stop_duration.png")
ggplot(stop_durations %>% filter(Arrested == TRUE), aes(Stop_Duration, arrests_to_pop)) +
  geom_bar(stat = "identity", fill = "#1E90FF") +
  labs(title = "Arrests per stop duration", y = "Arrests/stops") +
  scale_y_continuous(labels = scales::percent)
ggsave("./EDA_images/88_arrests_to_stops_stop_duration_TRUE.png")
# Investigating stop duration spread of stops and arrests- 
ggplot(df_clean %>% filter(is_arrested == TRUE), aes(stop_duration, ..prop.., group = 1)) +
  geom_bar(fill = "#1E90FF") +
  labs(title = "Proportion of stop duration for arrests")
ggsave("./EDA_images/88.2_arrests_stop_duration.png")

stop_durations2 <- setNames(data.frame(table(df_clean$stop_duration, df_clean$search_conducted, df_clean$is_arrested, exclude = NULL)), c("Stop_Duration", "Search_Conducted", "Arrested", "count")) 
stop_durations_total2 <- stop_durations2 %>% group_by(Stop_Duration, Search_Conducted) %>% dplyr::summarise(total_count = sum(count))
stop_durations2 <- left_join(stop_durations2, stop_durations_total2, by = c("Stop_Duration", "Search_Conducted"))
stop_durations2 <- stop_durations2 %>% mutate(arrests_to_pop = count/total_count)
ggplot(stop_durations2, aes(Search_Conducted, arrests_to_pop, fill = Arrested)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(. ~ Stop_Duration)
ggsave("./EDA_images/88.3_arrests_stop_duration_search_conducted.png")

ggplot(stop_durations2, aes(Stop_Duration, arrests_to_pop, fill = Arrested)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(. ~ Search_Conducted)

sd_df <- df_clean %>% filter(search_conducted == TRUE)
stop_durations3 <- setNames(data.frame(table(sd_df$stop_duration, sd_df$contraband_found, sd_df$is_arrested, exclude = NULL)), c("Stop_Duration", "Contraband_Found", "Arrested", "count")) 
stop_durations_total3 <- stop_durations3 %>% group_by(Stop_Duration, Contraband_Found) %>% dplyr::summarise(total_count = sum(count))
stop_durations3 <- left_join(stop_durations3, stop_durations_total3, by = c("Stop_Duration", "Contraband_Found"))
stop_durations3 <- stop_durations3 %>% mutate(arrests_to_pop = count/total_count)
ggplot(stop_durations3, aes(Contraband_Found, arrests_to_pop, fill = Arrested)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(. ~ Stop_Duration)
ggsave("./EDA_images/88.4_arrests_stop_duration_search_conductedTRUE.png")

ggplot(df_clean, aes(stop_duration, ..prop.., group = 1)) +
  geom_bar()
#Surprisingly, most stops and arrests take from 1-15 min.

# Notes: Most stops don’t end in arrest, regardless of stop duration
#        Stops that take longer than 30 minutes are more likely to have ended in an 
#        arrest, which makes sense because intuitively, the arresting process will 
#        take longer.


#violation count:
violation_counts <- setNames(data.frame(table(df_clean$violation_count, df_clean$is_arrested, exclude = NULL)), c("Violation_Count", "Arrested", "count")) 
violation_counts_total <- violation_counts%>% group_by(Violation_Count) %>% dplyr::summarise(total_count = sum(count))
violation_counts <- left_join(violation_counts, violation_counts_total, by = "Violation_Count")
violation_counts <- violation_counts %>% mutate(arrests_to_pop = count/total_count)
ggplot(violation_counts %>% filter(Arrested == TRUE), aes(Violation_Count, arrests_to_pop)) +
  geom_bar(stat = "identity", fill = "#1E90FF") +
  labs(title = "Arrest proportion by violation count", y = "Arrests/stops") +
  scale_y_continuous(labels = scales::percent)
ggsave("./EDA_images/89_arrests_to_stops_violation_count_TRUE.png")

ggplot(violation_counts, aes(Violation_Count, arrests_to_pop, fill = Arrested)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Arrest proportion by violation count", y = "Arrests/stops") +
  scale_y_continuous(labels = scales::percent)
ggsave("./EDA_images/89.2_arrests_to_stops_violation_count.png")

df_split3 <- df_split %>% gather("violation_new", "n", 'violation_raw_Cell.Phone':'violation_raw_Window.Tint')
df_split3 <- df_split3 %>% filter(n != 0)
violations <- setNames(data.frame(table(df_split3$violation_new, df_split3$is_arrested, exclude = NULL)), c("Violation", "Arrested", "count")) 
violations_total <- violations%>% group_by(Violation) %>% dplyr::summarise(total_count = sum(count))
violations <- left_join(violations, violations_total, by = "Violation")
violations <- violations %>% mutate(arrests_to_pop = count/total_count)
ggplot(violations, aes(Violation, arrests_to_pop, fill = Arrested)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme1
ggsave("./EDA_images/89.3_arrests_to_stops_violation.png")


st_split <- df_split3 %>% filter(search_type_raw == "Inventory")
violations_st <- setNames(data.frame(table(st_split$violation_new, st_split$is_arrested, exclude = NULL)), c("Violation", "Arrested", "count")) 
violations_st_total <- violations_st%>% group_by(Violation) %>% dplyr::summarise(total_count = sum(count))
violations_st <- left_join(violations_st, violations_st_total, by = "Violation")
violations_st <- violations_st %>% mutate(arrests_to_pop = count/total_count)
ggplot(violations_st, aes(Violation, arrests_to_pop, fill = Arrested)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme1 +
  labs(title = "Arrest Proportion for Inventory Violations", y = "Arrest/Stop") +
  scale_y_continuous(labels = scales::percent)
ggsave("./EDA_images/89.33_arrests_to_stops_inv_violation.png")

ggplot(violations %>% filter(Arrested == TRUE), aes(Violation, arrests_to_pop)) +
  geom_bar(stat = "identity", position = "dodge", fill = "#56B4E9") +
  theme1
ggsave("./EDA_images/89.4_arrests_to_stops_violation_arrestTRUE.png")

df_split3$stop_date <- as.POSIXct(df_split3$stop_date, "%Y-%m-%d", tz = "America/New_York")
df_split3$day_of_week <- weekdays(df_split3$stop_date)
df_split3$day_of_week <- factor(df_split3$day_of_week, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
ggplot(df_split3, aes(violation_new, ..count.., fill = day_of_week)) +
  geom_bar(position = "dodge") +
  theme1

ggplot(df_split3, aes(day_of_week, ..count.., fill = violation_new)) +
  geom_bar(position = "dodge") +
  theme1

ggplot(df_split3 %>% filter(violation_new %in% c("violation_raw_Suspended.License", "violation_raw_Moving.Violation", "violation_raw_Other", "violation_raw_Other.Error", "violation_raw_Traffic.Control.Signal")), aes(day_of_week, ..count.., fill = violation_new)) +
  geom_bar() +
  theme1

ggplot(df_split3 %>% filter(violation_new %in% c("violation_raw_Suspended.License", "violation_raw_Moving.Violation", "violation_raw_Other", "violation_raw_Other.Error", "violation_raw_Traffic.Control.Signal")), aes(violation_new, ..count.., fill = day_of_week)) +
  geom_bar(position = "dodge") +
  theme1
ggsave("./EDA_images/89.5_violations_over_time.png")

df_split3$month_year <- format(df_split3$stop_date, "%m-%Y")
df_split3$month_year <- factor(df_split3$month_year,
                              ordered = TRUE,
                              levels = mo_yr_label)
ggplot(df_split3, aes(violation_new, ..count.., fill = month_year)) +
  geom_bar(position = "dodge") +
  theme1

ggplot(df_split3 %>% filter(violation_new %in% c("violation_raw_Suspended.License", "violation_raw_Moving.Violation", "violation_raw_Other", "violation_raw_Other.Error", "violation_raw_Traffic.Control.Signal")), aes(violation_new, ..count.., fill = month_year)) +
  geom_bar(position = "dodge") +
  theme1

ggplot(df_split3 %>% filter(violation_new %in% c("violation_raw_Suspended.License", "violation_raw_Moving.Violation", "violation_raw_Other", "violation_raw_Other.Error", "violation_raw_Traffic.Control.Signal")), aes(month_year, ..count..)) +
  geom_bar(fill = "#1E90FF") +
  facet_grid(violation_new ~ .) +
  theme1
ggsave("./EDA_images/89.6_violations_over_time.png")









#stop outcome
#day of week:
so_per_day_of_week2 <- data.frame(df_clean %>% group_by(day_of_week, stop_outcome) %>% dplyr::summarise(sum_so = n()))
so_dow_total2 <- so_per_day_of_week2 %>% group_by(day_of_week) %>% dplyr::summarise(sum_count = sum(sum_so))
so_per_day_of_week2  <- left_join(so_per_day_of_week2 , so_dow_total2, by = "day_of_week")
so_per_day_of_week2 <- so_per_day_of_week2 %>% mutate(so_by_DOW  = sum_so/sum_count)
ggplot(so_per_day_of_week2, aes(stop_outcome, so_by_DOW, fill = day_of_week)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Stop Outcomes proportions for each Day of the Week", x = "Stop Outcome", y = "Stop Outcome Proportion") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete(name = "Day of Week")
ggsave("./EDA_images/92_arrests_per_stops_day_of_week_prop.png")
so_per_day_of_week <- data.frame(df_clean %>% group_by(day_of_week, stop_outcome) %>% dplyr::summarise(sum_so = n()))
so_dow_total <- so_per_day_of_week %>% group_by(stop_outcome) %>% dplyr::summarise(sum_count = sum(sum_so))
so_per_day_of_week  <- left_join(so_per_day_of_week , so_dow_total, by = "stop_outcome")
so_per_day_of_week <- so_per_day_of_week %>% mutate(so_by_dow  = sum_so/sum_count)  
ggplot(so_per_day_of_week %>% filter(is.na(stop_outcome) != TRUE), aes(day_of_week, so_by_dow)) +
  geom_bar(stat = "identity", fill = "#1E90FF") +
  facet_grid(stop_outcome ~ .) +
  labs(title = "Proportion of Stop outcomes by Day of Week", x = "Day of Week", y = "Proportion of Stop Outcome") +
  scale_y_continuous(labels = scales::percent)
ggsave("./EDA_images/90_stop_outcome_by_DOW.png")

# ggplot(so_per_day_of_week %>% filter(is.na(stop_outcome) != TRUE), aes(day_of_week, so_by_dow, fill = stop_outcome)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   #facet_grid(stop_outcome ~ .) +
#   labs(title = "Proportion of Stop outcomes by Day of Week", x = "Day of Week", y = "Proportion of Stop Outcome") +
#   scale_y_continuous(labels = scales::percent)


ggplot(so_per_day_of_week %>% filter(is.na(stop_outcome) != TRUE), aes(day_of_week, so_by_dow)) +
  geom_bar(stat = "identity", fill = "#1E90FF") +
  facet_grid(. ~ stop_outcome) +
  labs(title = "Proportion of Stop outcomes by Day of Week", x = "Day of Week", y = "Proportion of Stop Outcome") +
  scale_y_continuous(labels = scales::percent) +
  theme1
ggsave("./EDA_images/90.2_stop_outcome_by_DOW.png")

so_dow <- data.frame(df_clean %>% group_by(day_of_week, stop_outcome) %>% dplyr::summarise(sum_so = n()))
so_dow_total2 <- so_dow %>% group_by(day_of_week) %>% dplyr::summarise(sum_count = sum(sum_so))
so_dow  <- left_join(so_dow , so_dow_total2, by = "day_of_week")
so_dow <- so_dow %>% mutate(so_by_dow  = sum_so/sum_count)  
ggplot(so_dow %>% filter(is.na(stop_outcome) != TRUE), aes(stop_outcome, so_by_dow)) +
  geom_bar(stat = "identity", fill = "#1E90FF") +
  facet_grid(. ~ day_of_week) +
  labs(title = "Proportion of Stop outcomes by Day of Week", x = "Day of Week", y = "Proportion of Stop Outcome") +
  scale_y_continuous(labels = scales::percent) +
  theme1

ggplot(so_dow %>% filter(is.na(stop_outcome) != TRUE), aes(stop_outcome, so_by_dow, fill = day_of_week)) +
  geom_bar(stat = "identity", position = "dodge") +
  #facet_grid(. ~ day_of_week) +
  labs(title = "Proportion of Stop outcomes by Day of Week", x = "Day of Week", y = "Proportion of Stop Outcome") +
  scale_y_continuous(labels = scales::percent) +
  theme1
ggsave("./EDA_images/90.3_stop_outcome_by_DOW.png")


so_dow2 <- data.frame(df_clean %>% group_by(day_of_week, stop_outcome) %>% dplyr::summarise(sum_so = n()))
so_dow_total3 <- so_dow2 %>% group_by(day_of_week, stop_outcome) %>% dplyr::summarise(sum_count = sum(sum_so))
so_dow2  <- left_join(so_dow2 , so_dow_total3, by = c("day_of_week", "stop_outcome"))
tot <- sum(so_dow2$sum_so)
so_dow2 <- so_dow2 %>% mutate(so_by_dow  = sum_so/tot)  
ggplot(so_dow2 %>% filter(is.na(stop_outcome) != TRUE & stop_outcome != "Ticket"), aes(day_of_week, so_by_dow, fill = stop_outcome)) +
  geom_bar(stat = "identity", position = "dodge") +
  #facet_grid(stop_outcome ~ .) +
  labs(title = "Proportion of Stop outcomes by Day of Week", x = "Day of Week", y = "Proportion of Stop Outcome") +
  scale_y_continuous(labels = scales::percent)


#month
so_mo <- data.frame(df_clean %>% group_by(stop_month, stop_outcome) %>% dplyr::summarise(sum_so = n()))
so_mo_total <- so_mo %>% group_by(stop_month) %>% dplyr::summarise(sum_count = sum(sum_so))
so_mo  <- left_join(so_mo, so_mo_total, by = "stop_month")
#tot <- sum(so_dow2$sum_so)
so_mo <- so_mo %>% mutate(so_by_mo  = sum_so/sum_count)  
ggplot(so_mo %>% filter(is.na(stop_outcome) != TRUE), aes(stop_month, so_by_mo)) +
  geom_bar(stat= "identity", fill = "#1E90FF") +
  facet_grid(. ~ stop_outcome) +
  labs(title = "Stop Outcome by month")
ggsave("./EDA_images/90.5_so_by_month.png")



#time (hour)
so_time <- setNames(data.frame(table(df_clean$stop_time_hour, df_clean$stop_outcome, exclude = NULL)), c("Hour", "StopOutcome", "count")) 
so_time_totals <- data.frame(so_time %>% group_by(Hour) %>% dplyr::summarise(sum_stops = sum(count)))
so_time <- left_join(so_time, so_time_totals, by = "Hour")
so_time <- so_time %>% mutate(so_per_stops  = count/sum_stops)
ggplot(so_time %>% filter(is.na(Hour) != TRUE & is.na(StopOutcome) != TRUE), aes(Hour, so_per_stops)) +
  geom_bar(stat = "identity", fill = "#1E90FF") +
  facet_grid(. ~ StopOutcome) +
  labs(title = "Stop Outcome by hour")
ggsave("./EDA_images/90.1_so_by_hour.png")


#day of month
so_dom <- setNames(data.frame(table(df_clean$day_of_month, df_clean$stop_outcome, exclude = NULL)), c("dayofmonth", "StopOutcome", "count")) 
so_dom_totals <- data.frame(so_dom %>% group_by(dayofmonth) %>% dplyr::summarise(sum_stops = sum(count)))
so_dom <- left_join(so_dom, so_dom_totals, by = "dayofmonth")
so_dom <- so_dom %>% mutate(so_per_stops  = count/sum_stops)
ggplot(so_dom %>% filter(is.na(StopOutcome) != TRUE), aes(dayofmonth, so_per_stops)) +
  geom_bar(stat = "identity", fill = "#1E90FF") +
  facet_grid(. ~ StopOutcome) +
  labs(title = "Stop Outcome by day of month")
ggsave("./EDA_images/90.4_so_by_dom.png")

#county_name
county_by_so <- setNames(data.frame(table(df_clean$county_name, df_clean$stop_outcome, exclude = NULL)), c("County", "Stop_Outcome", "count")) 
county_totals_so <- data.frame(county_by_so %>% group_by(County) %>% dplyr::summarise(sum_county = sum(count)))
county_by_so <- left_join(county_by_so, county_totals_so, by = "County")
county_by_so <- county_by_so %>% mutate(percent_of_county  = count/sum_county)
ggplot(county_by_so %>% filter(County != "" & is.na(Stop_Outcome) != TRUE), aes(Stop_Outcome, percent_of_county, fill = County)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Proportion of Stop outcomes by County", x = "Stop Outcome", y = "Percent of stops") +
  scale_y_continuous(labels = scales::percent)
ggsave("./EDA_images/91_stop_outcome_by_county.png")

#driver_gender
gender_by_so <- setNames(data.frame(table(df_clean$driver_gender, df_clean$stop_outcome, exclude = NULL)), c("Gender", "Stop_Outcome", "count")) 
gender_totals_so <- data.frame(gender_by_so %>% group_by(Gender) %>% dplyr::summarise(sum_gender = sum(count)))
gender_by_so <- left_join(gender_by_so, gender_totals_so, by = "Gender")
gender_by_so <- gender_by_so %>% mutate(percent_of_gender  = count/sum_gender)
ggplot(gender_by_so %>% filter(is.na(Stop_Outcome) != TRUE), aes(Stop_Outcome, percent_of_gender, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Proportion of Stop outcomes by Gender", x = "Stop Outcome", y = "Percent of stops") +
  scale_y_continuous(labels = scales::percent)
ggsave("./EDA_images/93_stop_outcome_by_gender.png")

#driver_race
race_by_so <- setNames(data.frame(table(df_clean$driver_race_raw, df_clean$stop_outcome, exclude = NULL)), c("Race", "Stop_Outcome", "count")) 
race_totals_so <- data.frame(race_by_so %>% group_by(Race) %>% dplyr::summarise(sum_race = sum(count)))
race_by_so <- left_join(race_by_so, race_totals_so, by = "Race")
race_by_so <- race_by_so %>% mutate(percent_of_race  = count/sum_race)
ggplot(race_by_so %>% filter(is.na(Stop_Outcome) != TRUE), aes(Stop_Outcome, percent_of_race, fill = Race)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Proportion of Stop outcomes by Race", x = "Stop Outcome", y = "Percent of stops") +
  scale_y_continuous(labels = scales::percent)
ggsave("./EDA_images/94_stop_outcome_by_race.png")

#search_conducted
sc_by_so <- setNames(data.frame(table(df_clean$search_conducted, df_clean$stop_outcome, exclude = NULL)), c("Search_Conducted", "Stop_Outcome", "count")) 
sc_totals_so <- data.frame(sc_by_so %>% group_by(Search_Conducted) %>% dplyr::summarise(sum_sc = sum(count)))
sc_by_so <- left_join(sc_by_so, sc_totals_so, by = "Search_Conducted")
sc_by_so <- sc_by_so %>% mutate(percent_of_sc  = count/sum_sc)
ggplot(sc_by_so %>% filter(is.na(Stop_Outcome) != TRUE), aes(Stop_Outcome, percent_of_sc, fill = Search_Conducted)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Proportion of Stop outcomes by Search Conducted Status", x = "Stop Outcome", y = "Percent of stops") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete(name = "Search Conducted")
ggsave("./EDA_images/95_stop_outcome_by_sc.png")

#contraband_found
st_df
cf_by_so <- setNames(data.frame(table(df_clean$contraband_found, df_clean$stop_outcome, exclude = NULL)), c("Contraband_Found", "Stop_Outcome", "count")) 
cf_totals_so <- data.frame(cf_by_so %>% group_by(Contraband_Found) %>% dplyr::summarise(sum_cf = sum(count)))
cf_by_so <- left_join(cf_by_so, cf_totals_so, by = "Contraband_Found")
cf_by_so <- cf_by_so %>% mutate(percent_of_cf  = count/sum_cf)

cf_by_so2 <- setNames(data.frame(table(st_df$contraband_found, st_df$stop_outcome, exclude = NULL)), c("Contraband_Found", "Stop_Outcome", "count")) 
cf_totals_so2 <- data.frame(cf_by_so2 %>% group_by(Contraband_Found) %>% dplyr::summarise(sum_cf = sum(count)))
cf_by_so2 <- left_join(cf_by_so2, cf_totals_so2, by = "Contraband_Found")
cf_by_so2 <- cf_by_so2 %>% mutate(percent_of_cf  = count/sum_cf)
ggplot(cf_by_so2 %>% filter(is.na(Stop_Outcome) != TRUE), aes(Stop_Outcome, percent_of_cf, fill = Contraband_Found)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Proportion of Stop outcomes by Contraband Found status", x = "Stop Outcome", y = "Percent of stops") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete(name = "Contraband Found")
ggsave("./EDA_images/96.2_stop_outcome_by_cf.png")

# 
# cf_by_so3 <- setNames(data.frame(table(st_df$contraband_found, st_df$stop_outcome, exclude = NULL)), c("Contraband_Found", "Stop_Outcome", "count")) 
# cf_by_so3$Stop_Outcome2 <- as.character(cf_by_so3$Stop_Outcome)
# cf_by_so3$Stop_Outcome2[cf_by_so3$Stop_Outcome %in% c("Arrest", "Summons")] <- "Worse"
# cf_by_so3$Stop_Outcome2[cf_by_so3$Stop_Outcome %in% c("Verbal Warning", "Written Warning")] <- "Best"
df_clean$Stop_Outcome2 <- as.character(df_clean$stop_outcome)
df_clean$Stop_Outcome2[df_clean$Stop_Outcome %in% c("Arrest", "Summons")] <- "Worse"
df_clean$Stop_Outcome2[df_clean$Stop_Outcome %in% c("Verbal Warning", "Written Warning")] <- "Best"
st_df2 <- df_clean %>% filter(search_conducted == TRUE)
cf_by_so3 <- setNames(data.frame(table(st_df2$contraband_found, st_df2$Stop_Outcome2, exclude = NULL)), c("Contraband_Found", "Stop_Outcome", "count")) 
cf_totals_so3 <- data.frame(cf_by_so3 %>% group_by(Contraband_Found) %>% dplyr::summarise(sum_cf = sum(count)))
cf_by_so3 <- left_join(cf_by_so3, cf_totals_so3, by = "Contraband_Found")
cf_by_so3 <- cf_by_so3 %>% mutate(percent_of_cf  = count/sum_cf)
ggplot(cf_by_so3 %>% filter(is.na(Stop_Outcome) != TRUE), aes(Stop_Outcome, percent_of_cf, fill = Contraband_Found)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Proportion of Stop outcomes by Contraband Found status", x = "Stop Outcome", y = "Percent of stops") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete(name = "Contraband Found")
ggsave("./EDA_images/96.3_stop_outcome_by_cf.png")

#search_type
st_by_so <- setNames(data.frame(table(df_clean$search_type_raw, df_clean$stop_outcome, exclude = NULL)), c("Search_Type", "Stop_Outcome", "count")) 
st_totals_so <- data.frame(st_by_so %>% group_by(Search_Type) %>% dplyr::summarise(sum_st = sum(count)))
st_by_so <- left_join(st_by_so, st_totals_so, by = "Search_Type")
st_by_so <- st_by_so %>% mutate(percent_of_st  = count/sum_st)

st_by_so2 <- setNames(data.frame(table(st_df$search_type_raw, st_df$stop_outcome, exclude = NULL)), c("Search_Type", "Stop_Outcome", "count")) 
st_totals_so2 <- data.frame(st_by_so2 %>% group_by(Search_Type) %>% dplyr::summarise(sum_st = sum(count)))
st_by_so2 <- left_join(st_by_so2, st_totals_so2, by = "Search_Type")
st_by_so2 <- st_by_so2 %>% mutate(percent_of_st  = count/sum_st)
ggplot(st_by_so %>% filter(is.na(Stop_Outcome) != TRUE & Search_Type != ""), aes(Stop_Outcome, percent_of_st, fill = Search_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Proportion of Stop outcomes by Search Type", x = "Stop Outcome", y = "Percent of stops") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete(name = "Search Type")
ggsave("./EDA_images/97_stop_outcome_by_st.png")

ggplot(st_by_so2 %>% filter(is.na(Stop_Outcome) != TRUE & Search_Type != ""), aes(Stop_Outcome, percent_of_st, fill = Search_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Proportion of Stop outcomes by Search Type", x = "Stop Outcome", y = "Percent of stops") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete(name = "Search Type")
ggsave("./EDA_images/97.2_stop_outcome_by_st.png")

#stop_duration
sd_by_so <- setNames(data.frame(table(df_clean$stop_duration, df_clean$stop_outcome, exclude = NULL)), c("Stop_Duration", "Stop_Outcome", "count")) 
sd_totals_so <- data.frame(sd_by_so %>% group_by(Stop_Duration) %>% dplyr::summarise(sum_sd = sum(count)))
sd_by_so <- left_join(sd_by_so, sd_totals_so, by = "Stop_Duration")
sd_by_so <- sd_by_so %>% mutate(percent_of_sd  = count/sum_sd)
ggplot(sd_by_so %>% filter(is.na(Stop_Outcome) != TRUE), aes(Stop_Outcome, percent_of_sd, fill = Stop_Duration)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Proportion of Stop outcomes by Stop Duration", x = "Stop Outcome", y = "Percent of stops") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete(name = "Stop duration")
ggsave("./EDA_images/98_stop_outcome_by_sd.png")

#violation_count
vc_by_so <- setNames(data.frame(table(df_clean$violation_count, df_clean$stop_outcome, exclude = NULL)), c("Violation_Count", "Stop_Outcome", "count")) 
vc_totals_so <- data.frame(vc_by_so %>% group_by(Violation_Count) %>% dplyr::summarise(sum_vc = sum(count)))
vc_by_so <- left_join(vc_by_so, vc_totals_so, by = "Violation_Count")
vc_by_so <- vc_by_so %>% mutate(percent_of_vc  = count/sum_vc)
ggplot(vc_by_so %>% filter(is.na(Stop_Outcome) != TRUE), aes(Stop_Outcome, percent_of_vc, fill = Violation_Count)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Proportion of Stop outcomes by Violation Count", x = "Stop Outcome", y = "Percent of stops") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete(name = "Violation Count")
ggsave("./EDA_images/99_stop_outcome_by_vc.png")


df_clean$violation_count2 <- as.character(df_clean$violation_count)
df_clean$violation_count2[df_clean$violation_count > 1] <- "> 1"
vc_by_so2 <- setNames(data.frame(table(df_clean$violation_count2, df_clean$stop_outcome, exclude = NULL)), c("Violation_Count", "Stop_Outcome", "count")) 
vc_totals_so2 <- data.frame(vc_by_so2 %>% group_by(Violation_Count) %>% dplyr::summarise(sum_vc = sum(count)))
vc_by_so2 <- left_join(vc_by_so2, vc_totals_so2, by = "Violation_Count")
vc_by_so2 <- vc_by_so2 %>% mutate(percent_of_vc  = count/sum_vc)
ggplot(vc_by_so2 %>% filter(is.na(Stop_Outcome) != TRUE), aes(Stop_Outcome, percent_of_vc, fill = Violation_Count)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Proportion of Stop outcomes by Violation Count", x = "Stop Outcome", y = "Percent of stops") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete(name = "Violation Count")
ggsave("./EDA_images/99.1_stop_outcome_by_vc.png")

vc_by_so3 <- setNames(data.frame(table(df_clean$violation_count2, df_clean$driver_race_raw, df_clean$stop_outcome, exclude = NULL)), c("Violation_Count", "Race","Stop_Outcome", "count")) 
vc_totals_so3 <- data.frame(vc_by_so3 %>% group_by(Violation_Count, Race) %>% dplyr::summarise(sum_vc = sum(count)))
vc_by_so3 <- left_join(vc_by_so3, vc_totals_so3, by = c("Violation_Count", "Race"))
vc_by_so3 <- vc_by_so3 %>% mutate(percent_of_vc  = count/sum_vc)
ggplot(vc_by_so3 %>% filter(is.na(Stop_Outcome) != TRUE), aes(Stop_Outcome, percent_of_vc, fill = Violation_Count)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(Race ~ .) +
  labs(title = "Proportion of Stop outcomes by Violation Count", x = "Stop Outcome", y = "Percent of stops") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete(name = "Violation Count")


#violation:
violations2 <- setNames(data.frame(table(df_split3$violation_new, df_split3$stop_outcome, exclude = NULL)), c("Violation", "StopOutcome", "count")) 
violations_total2 <- violations2 %>% group_by(Violation) %>% dplyr::summarise(total_count = sum(count))
violations2 <- left_join(violations2, violations_total2, by = "Violation")
violations2 <- violations2 %>% mutate(so_to_pop = count/total_count)
ggplot(violations2 %>% filter(is.na(StopOutcome) != TRUE), aes(Violation, so_to_pop, fill = StopOutcome)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme1
ggsave("./EDA_images/102_stop_outcome_by_stops_violation.png")

ggplot(violations2 %>% filter(is.na(StopOutcome) != TRUE), aes(StopOutcome, so_to_pop, fill = Violation)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme1
ggsave("./EDA_images/102.1_stop_outcome_by_stops_violation.png")

ggplot(violations2 %>% filter(is.na(StopOutcome) != TRUE & StopOutcome == "Summons"), aes(Violation, so_to_pop)) +
  geom_bar(stat = "identity", position = "dodge", fill = "#1E90FF") +
  theme1
ggsave("./EDA_images/103_stop_outcome_by_stops_violation.png")

ggplot(violations2 %>% filter(is.na(StopOutcome) != TRUE & StopOutcome == "Ticket"), aes(Violation, so_to_pop)) +
  geom_bar(stat = "identity", position = "dodge", fill = "#1E90FF") +
  theme1
ggsave("./EDA_images/104_stop_outcome_by_stops_violation.png")

ggplot(violations2 %>% filter(is.na(StopOutcome) != TRUE & StopOutcome == "Verbal Warning"), aes(Violation, so_to_pop)) +
  geom_bar(stat = "identity", position = "dodge", fill = "#1E90FF") +
  theme1
ggsave("./EDA_images/105_stop_outcome_by_stops_violation.png")

ggplot(violations2 %>% filter(is.na(StopOutcome) != TRUE & StopOutcome == "Written Warning"), aes(Violation, so_to_pop)) +
  geom_bar(stat = "identity", position = "dodge", fill = "#1E90FF") +
  theme1
ggsave("./EDA_images/106_stop_outcome_by_stops_violation.png")

ggplot(violations2 %>% filter(Violation == "violation_raw_Defective.Lights"), aes(StopOutcome, so_to_pop, fill = StopOutcome)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme1

race_by_violation <- setNames(data.frame(table(df_split3$violation_new, df_split3$driver_race_raw, exclude = NULL)), c("Violation", "Race", "count")) 
race_by_viol_tot <- race_by_violation %>% group_by(Race) %>% dplyr::summarise(total_count = sum(count))
race_by_violation <- left_join(race_by_violation, race_by_viol_tot, by = "Race")
race_by_violation <- race_by_violation %>% mutate(so_to_pop = count/total_count)

ggplot(race_by_violation %>% filter(Violation == "violation_raw_Suspended.License"), aes(Race, so_to_pop)) + 
  geom_bar(stat = "identity", position = "dodge", fill = "#1E90FF") +
  theme1 +
  labs(title = "Suspended License")
ggsave("./EDA_images/107_race_violation.png")

ggplot(race_by_violation %>% filter(Violation == "violation_raw_Seatbelt"), aes(Race, so_to_pop)) + 
  geom_bar(stat = "identity", position = "dodge", fill = "#1E90FF") +
  theme1 +
  labs(title = "Seatbelt")
ggsave("./EDA_images/108_race_violation.png")

ggplot(race_by_violation %>% filter(Violation == "violation_raw_Defective.Lights"), aes(Race, so_to_pop)) + 
  geom_bar(stat = "identity", position = "dodge", fill = "#1E90FF") +
  theme1 +
  labs(title = "Defective Lights")
ggsave("./EDA_images/109_race_violation.png")

ggplot(race_by_violation %>% filter(Violation == "violation_raw_Display.of.Plates"), aes(Race, so_to_pop)) + 
  geom_bar(stat = "identity", position = "dodge", fill = "#1E90FF") +
  theme1 +
  labs(title = "Display of Plates")
ggsave("./EDA_images/110_race_violation.png")

ggplot(race_by_violation %>% filter(Violation == "violation_raw_Equipment.Violation"), aes(Race, so_to_pop)) + 
  geom_bar(stat = "identity", position = "dodge", fill = "#1E90FF") +
  theme1 +
  labs(title = "Equipment Violation")
ggsave("./EDA_images/111_race_violation.png")

race_viol_so <- setNames(data.frame(table(df_split3$violation_new, df_split3$driver_race_raw, df_split3$stop_outcome, exclude = NULL)), c("Violation", "Race", "StopOutcome", "count")) 
race_viol_so_tot <- race_viol_so %>% group_by(Race, Violation) %>% dplyr::summarise(total_count = sum(count))
race_viol_so <- left_join(race_viol_so, race_viol_so_tot, by = c("Race", "Violation"))
race_viol_so <- race_viol_so %>% mutate(so_to_pop = count/total_count)

ggplot(race_viol_so %>% filter(Race == "Black"), aes(StopOutcome, so_to_pop, fill = Violation)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme1

ggplot(race_viol_so %>% filter(Race == "White"), aes(StopOutcome, so_to_pop, fill = Violation)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme1

ggplot(race_viol_so %>% filter(Race == "Asian"), aes(StopOutcome, so_to_pop, fill = Violation)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme1

ggplot(race_viol_so %>% filter(Violation == "violation_raw_Display.of.Plates"), aes(StopOutcome, so_to_pop, fill = Race)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme1
ggsave("./EDA_images/123_display_of_plates-so_race.png")

ggplot(race_viol_so %>% filter(Violation == "violation_raw_Defective.Lights"), aes(StopOutcome, so_to_pop, fill = Race)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme1
ggsave("./EDA_images/124_defective_lights_so_race.png")

ggplot(race_viol_so %>% filter(Violation == "violation_raw_Suspended.License"), aes(StopOutcome, so_to_pop, fill = Race)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme1
ggsave("./EDA_images/125_suspended_license_so_race.png")


gen_by_violation <- setNames(data.frame(table(df_split3$violation_new, df_split3$driver_gender, exclude = NULL)), c("Violation", "Gender", "count")) 
gen_by_viol_tot <- gen_by_violation %>% group_by(Gender) %>% dplyr::summarise(total_count = sum(count))
gen_by_violation <- left_join(gen_by_violation, gen_by_viol_tot, by = "Gender")
gen_by_violation <- gen_by_violation %>% mutate(so_to_pop = count/total_count)

ggplot(gen_by_violation %>% filter(Violation == "violation_raw_Suspended.License"), aes(Gender, so_to_pop)) + 
  geom_bar(stat = "identity", position = "dodge", fill = "#1E90FF") +
  theme1 +
  labs(title = "Suspended License")
ggsave("./EDA_images/107.1_gender_violation.png")

ggplot(gen_by_violation %>% filter(Violation == "violation_raw_Seatbelt"), aes(Gender, so_to_pop)) + 
  geom_bar(stat = "identity", position = "dodge", fill = "#1E90FF") +
  theme1 +
  labs(title = "Seatbelt")
ggsave("./EDA_images/108.1_gender_violation.png")

ggplot(gen_by_violation %>% filter(Violation == "violation_raw_Defective.Lights"), aes(Gender, so_to_pop)) + 
  geom_bar(stat = "identity", position = "dodge", fill = "#1E90FF") +
  theme1 +
  labs(title = "Defective Lights")
ggsave("./EDA_images/109.1_gender_violation.png")

county_by_violation <- setNames(data.frame(table(df_split3$violation_new, df_split3$county_name, exclude = NULL)), c("Violation", "County", "count")) 
county_by_viol_tot <- county_by_violation %>% group_by(County) %>% dplyr::summarise(total_count = sum(count))
county_by_violation <- left_join(county_by_violation, county_by_viol_tot, by = "County")
county_by_violation <- county_by_violation %>% mutate(so_to_pop = count/total_count)

ggplot(county_by_violation, aes(County, so_to_pop, fill = Violation)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme1

ggplot(county_by_violation %>%  filter(County != ""), aes(Violation, so_to_pop, fill = County)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme1


#driver_age
# age_by_so <- setNames(data.frame(table(df_clean$driver_age, df_clean$stop_outcome, exclude = NULL)), c("Age", "Stop_Outcome", "count")) 
# age_totals_so <- data.frame(age_by_so %>% group_by(Stop_Outcome) %>% dplyr::summarise(sum_so = sum(count)))
# age_by_so <- left_join(age_by_so, age_totals_so, by = "Stop_Outcome")
# age_by_so <- age_by_so %>% mutate(percent_of_so  = count/sum_so)
age_by_so <- setNames(data.frame(table(df_clean$driver_age, df_clean$stop_outcome, exclude = NULL)), c("Age", "Stop_Outcome", "count")) 
age_totals_so <- data.frame(age_by_so %>% group_by(Age) %>% dplyr::summarise(sum_so = sum(count)))
age_by_so <- left_join(age_by_so, age_totals_so, by = "Age")
age_by_so <- age_by_so %>% mutate(percent_of_so  = count/sum_so)
ggplot(age_by_so %>% filter(is.na(Stop_Outcome) != TRUE & is.na(Age) != TRUE), aes(Age, percent_of_so)) +
  geom_bar(stat = "identity", fill = "#1E90FF") +
  facet_grid(Stop_Outcome ~ .) +
  labs(title = "Stop Outcome by Age", y = "Proportion") +
  scale_y_continuous(labels = scales::percent)
ggsave("./EDA_images/100.2_stop_outcome_by_age.png")

ggplot(age_by_so %>% filter(is.na(Stop_Outcome) != TRUE & is.na(Age) != TRUE & Stop_Outcome == "Verbal Warning"), aes(Age, percent_of_so)) +
  geom_bar(stat = "identity", fill = "#1E90FF") +
  facet_grid(Stop_Outcome ~ .) +
  labs(title = "Stop Outcome by Age", y = "Proportion") +
  scale_y_continuous(labels = scales::percent)
ggsave("./EDA_images/100.1_stop_outcome_by_age.png")

ggplot(age_by_so %>% filter(is.na(Stop_Outcome) != TRUE & is.na(Age) != TRUE & Stop_Outcome == "Written Warning"), aes(Age, percent_of_so)) +
  geom_bar(stat = "identity", fill = "#1E90FF") +
  facet_grid(Stop_Outcome ~ .) +
  labs(title = "Stop Outcome by Age", y = "Proportion") +
  scale_y_continuous(labels = scales::percent)
ggsave("./EDA_images/100.3_stop_outcome_by_age.png")

ggplot(age_by_so %>% filter(is.na(Stop_Outcome) != TRUE & is.na(Age) != TRUE & Stop_Outcome == "Ticket"), aes(Age, percent_of_so)) +
  geom_bar(stat = "identity", fill = "#1E90FF") +
  facet_grid(Stop_Outcome ~ .) +
  labs(title = "Stop Outcome by Age", y = "Proportion") +
  scale_y_continuous(labels = scales::percent)
ggsave("./EDA_images/100.4_stop_outcome_by_age.png")

ggplot(age_by_so %>% filter(is.na(Stop_Outcome) != TRUE & is.na(Age) != TRUE & Stop_Outcome == "Summons"), aes(Age, percent_of_so)) +
  geom_bar(stat = "identity", fill = "#1E90FF") +
  facet_grid(Stop_Outcome ~ .) +
  labs(title = "Stop Outcome by Age", y = "Proportion") +
  scale_y_continuous(labels = scales::percent)
ggsave("./EDA_images/100.5_stop_outcome_by_age.png")


ggplot(age_by_so %>% filter(is.na(Stop_Outcome) != TRUE & is.na(Age) != TRUE & Stop_Outcome == "Written Warning"), aes(Age, count)) +
  geom_bar(stat = "identity", fill = "#1E90FF") +
  facet_grid(Stop_Outcome ~ .) +
  labs(title = "Stop Outcome by Age", y = "Count")
ggsave("./EDA_images/100.6_stop_outcome_by_age.png")

ggplot(age_by_so %>% filter(is.na(Stop_Outcome) != TRUE & is.na(Age) != TRUE & Stop_Outcome == "Arrest"), aes(Age, percent_of_so)) +
  geom_bar(stat = "identity", fill = "#1E90FF") +
  facet_grid(Stop_Outcome ~ .) +
  labs(title = "Stop Outcome by Age", y = "Proportion") +
  scale_y_continuous(labels = scales::percent)
ggsave("./EDA_images/100.7_stop_outcome_by_age.png")


# stop outcome by stop duration
df_so_sd <- setNames(data.frame(table(df_clean$stop_outcome, df_clean$stop_duration, exclude = NULL)), c("Stop_Outcome", "Stop_Duration","count")) 
so_sd_tot <- data.frame(df_so_sd %>% group_by(Stop_Outcome) %>% dplyr::summarise(sum_so = sum(count)))
df_so_sd <- left_join(df_so_sd, so_sd_tot, by = "Stop_Outcome")
df_so_sd<- df_so_sd %>% mutate(percent_of_so  = count/sum_so)
ggplot(df_so_sd %>% filter(is.na(Stop_Outcome) != TRUE), aes(Stop_Duration, percent_of_so, fill = Stop_Outcome)) +
  geom_bar(stat = "identity", position = "dodge")
ggsave("./EDA_images/101_stop_outcome_by_stop duration.png")



# ggplot(search_types %>% filter(Arrested == TRUE), aes(Search_Type, count)) +
#   geom_bar(stat = "identity", fill = "#1E90FF") 
# 
# 
# ggplot(df_clean %>% filter(is.na(search_type) != TRUE), aes(search_type)) +
#   geom_bar(position = "dodge") 
# ggplot(df_clean, aes(search_type)) +
#   geom_bar(position = "dodge") 
# ggplot(df_clean %>% filter(is_arrested == TRUE), aes(search_type)) +
#   geom_bar(position = "dodge") 

#violation by race
viol_race <- setNames(data.frame(table(df_split3$violation_new, df_split3$driver_race_raw, exclude = NULL)), c("Violation", "Race", "count")) 
viol_race_tot <- viol_race %>% group_by(Race) %>% dplyr::summarise(total_count = sum(count))
viol_race <- left_join(viol_race, viol_race_tot, by = "Race")
viol_race <- viol_race%>% mutate(so_to_pop = count/total_count)
ggplot(viol_race, aes(Violation, so_to_pop, fill = Race)) +
  geom_bar(stat = "identity", position = "dodge") + 
  theme1

viol_race2 <- setNames(data.frame(table(df_split3$violation_new, df_split3$driver_race_raw, df_split3$stop_outcome,exclude = NULL)), c("Violation", "Race", "StopOutcome","count")) 
viol_race_tot2 <- viol_race2 %>% group_by(Race, Violation) %>% dplyr::summarise(total_count = sum(count))
viol_race2 <- left_join(viol_race2, viol_race_tot2, by = c("Race", "Violation"))
viol_race2 <- viol_race2 %>% mutate(so_to_pop = count/total_count)

ggplot(viol_race2 %>% filter(Violation == "violation_raw_Suspended.License"), aes(StopOutcome, so_to_pop, fill = Race)) +
  geom_bar(stat = "identity", position = "dodge") + 
  theme1

ggplot(viol_race2 %>% filter(Violation == "violation_raw_Moving.Violation"), aes(StopOutcome, so_to_pop, fill = Race)) +
  geom_bar(stat = "identity", position = "dodge") + 
  theme1

ggplot(viol_race2 %>% filter(Violation == "violation_raw_Defective.Lights"), aes(StopOutcome, so_to_pop, fill = Race)) +
  geom_bar(stat = "identity", position = "dodge") + 
  theme1

ggplot(viol_race2 %>% filter(Violation == "violation_raw_Display.of.Plates"), aes(StopOutcome, so_to_pop, fill = Race)) +
  geom_bar(stat = "identity", position = "dodge") + 
  theme1


viol_race3 <- setNames(data.frame(table(df_split3$violation_new, df_split3$driver_race_raw, df_split3$stop_outcome,exclude = NULL)), c("Violation", "Race", "StopOutcome","count")) 
viol_race_tot3 <- viol_race3 %>% group_by(Race, StopOutcome) %>% dplyr::summarise(total_count = sum(count))
viol_race3 <- left_join(viol_race3, viol_race_tot3, by = c("Race", "StopOutcome"))
viol_race3 <- viol_race3 %>% mutate(so_to_pop = count/total_count)

ggplot(viol_race3 %>% filter(StopOutcome == "Ticket"), aes(Violation, so_to_pop, fill = Race)) +
  geom_bar(stat = "identity", position = "dodge") + 
  theme1

ggplot(viol_race3 %>% filter(StopOutcome == "Arrest"), aes(Violation, so_to_pop, fill = Race)) +
  geom_bar(stat = "identity", position = "dodge") + 
  theme1


#maps

#county_by_arrest_TRUE <- county_by_arrest %>% filter(Arrested == TRUE & County != "")
#map(database = "df_clean", fill = TRUE, col = df_clean)
county_by_arrest_TRUE <- setNames(data.frame(table(df_clean$county_fips, df_clean$is_arrested, exclude = NULL)), c("County", "Arrested", "count")) 
county_totals_TRUE <- data.frame(county_by_arrest_TRUE %>% group_by(County) %>% dplyr::summarise(sum_county = sum(count)))
county_by_arrest_TRUE <- left_join(county_by_arrest_TRUE, county_totals_TRUE, by = "County")
county_by_arrest_TRUE <- county_by_arrest_TRUE %>% mutate(percent_of_county  = count/sum_county)
county_by_arrest_TRUE <- county_by_arrest_TRUE %>% filter(Arrested == TRUE, is.na(County) != TRUE)
county_map_df <- data.frame(region = as.integer(as.character(county_by_arrest_TRUE$County)),
                            value = county_by_arrest_TRUE$count)
#map(database = "county_map_df", fill = TRUE)
county_choropleth(county_map_df, legend = "# of Arrests", state_zoom = "connecticut", num_colors = 1)
#county_choropleth(county_map_df, legend = "# of Arrests", state_zoom = "connecticut", num_colors = 1, reference_map = TRUE)

gcounty <- map_data("county")
gcounty <- mutate(gcounty, polyname = paste(region, subregion, sep = ","))
gcounty <- left_join(gcounty, county.fips, "polyname")
gcounty_arrests <- left_join(gcounty, county_map_df, by = c("fips" = "region"))
ggplot(gcounty_arrests %>% filter(is.na(value) == FALSE)) +
  geom_polygon(aes(long, lat, group = group, fill = value)) +
  geom_text(data = county_label, aes(x = avg_long, y = avg_lat, label = c_name)) +
  scale_fill_gradientn(colours = brewer.pal(n=9, name = "YlGnBu"), name = "Total Arrests")
  #scale_fill_gradient(low = "#56B1F7", high = "#132B43")
ggsave("./EDA_images/112_arrests_per_county.png")


county_map_df2 <- data.frame(region = as.integer(as.character(county_by_arrest_TRUE$County)),
                            value = county_by_arrest_TRUE$percent_of_county)
gcounty_arrests_per <- left_join(gcounty, county_map_df2, by = c("fips" = "region"))
ggplot(gcounty_arrests_per %>% filter(is.na(value) == FALSE)) +
  geom_polygon(aes(long, lat, group = group, fill = value)) +
  geom_text(data = county_label, aes(x = avg_long, y = avg_lat, label = c_name)) +
  scale_fill_gradientn(colours = brewer.pal(n=9, name = "YlGnBu"))
  #scale_fill_gradient(low = "#56B1F7", high = "#132B43")
ggsave("./EDA_images/112.2_arrests_per_county_percent.png")


county_by_arrest2 <- setNames(data.frame(table(df_clean$county_fips, df_clean$is_arrested, exclude = NULL)), c("County", "Arrested", "count")) 
county_by_arrest2$County <- as.integer(as.character(county_by_arrest2$County))
county_arrests <- left_join(county_by_arrest2, county_pop, by = c("County" = "county_fips"))
county_arrests <- county_arrests %>% mutate(percent = count/TOT_POP)
county_arrests <- county_arrests %>% filter(is.na(County) != TRUE & Arrested == TRUE)
gcounty_arrests_per2 <- left_join(gcounty, county_arrests, by = c("fips" = "County"))
ggplot(gcounty_arrests_per2 %>% filter(is.na(percent) == FALSE)) +
  geom_polygon(aes(long, lat, group = group, fill = percent)) +
  geom_text(data = county_label, aes(x = avg_long, y = avg_lat, label = c_name)) +
  scale_fill_gradientn(colours = brewer.pal(n=9, name = "YlGnBu"))
  #scale_fill_gradient(low = "#56B1F7", high = "#132B43")
ggsave("./EDA_images/112.3_arrests_per_county_percent_of_pop.png")


#tst <- gcounty_arrests %>% filter(is.na(value) == FALSE) %>% group_by(fips) %>% dplyr::mutate(avg_lat = mean(gcounty_arrests$lat), avg_lon = mean(gcounty_arrests$long))
#tst <- gcounty_arrests %>% filter(is.na(value) == FALSE) %>% group_by(fips) %>% dplyr::summarise(avg_lat = mean(gcounty_arrests$lat), avg_lon = mean(gcounty_arrests$long))
#tst <- gcounty_arrests %>% filter(is.na(value) == FALSE) %>% group_by(fips) %>% dplyr::summarise(n = n(), avg = mean(gcounty_arrests$lat), tot_lat = sum(lat), tot_long = sum(gcounty_arrests$long)) %>% dplyr::summarise(avg_lat = tot_lat/n, avg_lon = tot_long/n)
county_label <- gcounty_arrests %>% filter(is.na(value) == FALSE) %>% group_by(fips) %>% dplyr::summarise(avg_lat = mean(lat), avg_long = mean(long))
#county_label <- gcounty_arrests %>% filter(is.na(value) == FALSE) %>% group_by(fips) %>% dplyr::summarise(avg_lat = median(lat), avg_long = median(long))
county_name <- df_clean %>% dplyr::select(county_fips, county_name) %>% distinct()
county_label <- left_join(county_label, county_name, by = c("fips" = "county_fips"))
county_label <- county_label %>% mutate(c_name = gsub(" County", "", county_name))


county_pop <- census_county %>% select(county_fips, TOT_POP, CTYNAME) %>% mutate(county_name = gsub(" County", "", CTYNAME))
gcounty_pop <- left_join(gcounty, county_pop, by = c("fips" = "county_fips"))
ggplot(gcounty_pop %>% filter(is.na(TOT_POP) == FALSE)) +
  geom_polygon(aes(long, lat, group = group, fill = TOT_POP)) +
  geom_text(data = county_label, aes(x = avg_long, y = avg_lat, label = c_name)) +
  scale_fill_gradientn(colours = brewer.pal(n=9, name = "YlGnBu"))
  #scale_fill_gradient(low = "#56B1F7", high = "#132B43")
ggsave("./EDA_images/113_pop_per_county.png")


county_pov <- data.frame(fips= as.integer(as.character(poverty$County.ID)), poverty = as.character(poverty$All.Ages.in.Poverty.Count))
county_pov$poverty <- as.numeric(gsub(",", "", county_pov$poverty))
gcounty_pov <- left_join(gcounty, county_pov, by = "fips")
ggplot(gcounty_pov %>% filter(is.na(poverty) == FALSE)) +
  geom_polygon(aes(long, lat, group = group, fill = poverty)) +
  geom_text(data = county_label, aes(x = avg_long, y = avg_lat, label = c_name)) +
  scale_fill_gradientn(colours = brewer.pal(n=9, name = "YlGnBu"))
  #scale_fill_gradient(low = "#56B1F7", high = "#132B43")
ggsave("./EDA_images/114_pov_per_county.png")


county_pov_percent <- data.frame(fips= as.integer(as.character(poverty$County.ID)), poverty_per = as.character(poverty$All.Ages.in.Poverty.Percent))
county_pov_percent$poverty_per <- as.numeric(gsub(",", "", county_pov_percent$poverty_per))
gcounty_pov_percent <- left_join(gcounty, county_pov_percent, by = "fips")
ggplot(gcounty_pov_percent %>% filter(is.na(poverty_per) == FALSE)) +
  geom_polygon(aes(long, lat, group = group, fill = poverty_per)) +
  geom_text(data = county_label, aes(x = avg_long, y = avg_lat, label = c_name)) +
  scale_fill_gradientn(colours = brewer.pal(n=9, name = "YlGnBu"))
  #scale_fill_gradient(low = "#56B1F7", high = "#132B43")
ggsave("./EDA_images/114.2_pov_percent_per_county.png")


county_income <- data.frame(fips= as.integer(as.character(poverty$County.ID)), income = as.character(poverty$Median.Household.Income.in.Dollars))
county_income$income <- gsub(",", "", county_income$income)
county_income$income <- as.numeric(gsub("\\$", "", county_income$income))
gcounty_income <- left_join(gcounty, county_income, by = "fips")
ggplot(gcounty_income %>% filter(is.na(income) == FALSE)) +
  geom_polygon(aes(long, lat, group = group, fill = income)) +
  geom_text(data = county_label, aes(x = avg_long, y = avg_lat, label = c_name)) +
  scale_fill_gradientn(colours = brewer.pal(n=9, name = "YlGnBu"))
  #scale_fill_gradient(low = "#56B1F7", high = "#132B43")
ggsave("./EDA_images/115_income_per_county.png")


#uninsured
county_uninsured <- data.frame(fips= as.integer(as.character(uninsured$ID)), uninsured = uninsured$Uninsured...)
gcounty_uninsured <- left_join(gcounty, county_uninsured, by = "fips")
ggplot(gcounty_uninsured %>% filter(is.na(uninsured) == FALSE)) +
  geom_polygon(aes(long, lat, group = group, fill = uninsured)) +
  geom_text(data = county_label, aes(x = avg_long, y = avg_lat, label = c_name)) +
  scale_fill_gradientn(colours = brewer.pal(n=9, name = "YlGnBu"))
  #scale_fill_gradient(low = "#56B1F7", high = "#132B43")
ggsave("./EDA_images/116_uninsured_per_county.png")


#miles travelled
miles_df <- data.frame(county = miles$County, interstate = miles$Interstate.DVMT, freeway = miles$Freeway.DVMT)
miles_df$county <- as.character(miles_df$county)
miles_df2 <- miles_df %>% group_by(county) %>% dplyr::summarise(sum_interstate = sum(interstate), sum_freeway = sum(freeway))
miles_df2 <- left_join(miles_df2, county_label, by = c("county" = "c_name"))
gcounty_miles <- left_join(gcounty, miles_df2, by = "fips")
ggplot(gcounty_miles %>% filter(is.na(sum_interstate) == FALSE)) +
  geom_polygon(aes(long, lat, group = group, fill = sum_interstate)) +
  geom_text(data = county_label, aes(x = avg_long, y = avg_lat, label = c_name)) +
  scale_fill_gradientn(colours = brewer.pal(n=9, name = "YlGnBu"), name = "Total Miles")
  #scale_fill_gradient(low = "#56B1F7", high = "#132B43")
ggsave("./EDA_images/117_interstate_miles_per_county.png")

ggplot(gcounty_miles %>% filter(is.na(sum_freeway) == FALSE)) +
  geom_polygon(aes(long, lat, group = group, fill = sum_freeway)) +
  geom_text(data = county_label, aes(x = avg_long, y = avg_lat, label = c_name)) +
  scale_fill_gradientn(colours = brewer.pal(n=9, name = "YlGnBu"), name = "Total Miles")
  #scale_fill_gradient(low = "#56B1F7", high = "#132B43")
ggsave("./EDA_images/118_freeway_miles_per_county.png")

#weather
weather_df <- weather %>% group_by(DATE) %>% dplyr::summarise(tot_precip = sum(PRCP, na.rm = TRUE))
weather_df$DATE <- as.POSIXct(weather_df$DATE, "%m/%d/%Y", tz = "America/New_York")

ggplot(weather_df, aes(x = DATE, y = tot_precip)) + 
  geom_line(stat = "identity", fill = "#1E90FF")  +
  geom_smooth(se = FALSE, color = "red") + 
  scale_x_datetime(date_breaks = "1 month", date_labels = "%m/%Y") +
  theme1 + 
  labs(title = "Total precipitation per day")
ggsave("./EDA_images/119_precipitation.png")


#bridges
bridges$County.Code <- bridges$County.Code + 9000
bridge_count <- bridges %>% group_by(County.Code) %>% dplyr::summarise(tot = n())
gcounty_briges <- left_join(gcounty, bridge_count, by = c("fips" = "County.Code"))
gcounty_bridges <- gcounty_briges %>% filter(is.na(tot) == FALSE & is.na(fips) == FALSE)
ggplot(gcounty_bridges) +
  geom_polygon(aes(long, lat, group = group, fill = tot)) +
  geom_text(data = county_label, aes(x = avg_long, y = avg_lat, label = c_name)) +
  scale_fill_gradientn(colours = brewer.pal(n=9, name = "YlGnBu"))
  #scale_fill_gradient(low = brewer.pal(n=9, name = "YlGnBu")[1], high = brewer.pal(n=9, name = "YlGnBu")[9])
  #scale_fill_distiller(palette ="YlGnBu", direction = -1)
  #scale_fill_brewer(palette = "BrBG", direction = -1)
  #scale_fill_gradientn(colours = heat.colors(10), direction = -1)
  #scale_fill_gradient(low = "#56B1F7", high = "#132B43")
ggsave("./EDA_images/120_bridges_per_county.png")


#commutes
commutes_ct <- commutes %>% filter(FIPS.State.Code.of.Residence == 9)
commutes_ct$samecounty <- 1
commutes_ct$samecounty[commutes_ct$FIPS.County.Code.of.Residence != commutes_ct$FIPS.County.Code.of.Work] <- 0

commute_df <- setNames(data.frame(table(commutes_ct$FIPS.County.Code.of.Residence, commutes_ct$FIPS.County.Code.of.Work, exclude = NULL)), c("residence", "work", "count")) 
commute_df$samecounty <- 1
commute_df$samecounty[commute_df$residence != commute_df$work] <- 0

commute_df2 <- setNames(data.frame(table(commutes_ct$FIPS.County.Code.of.Residence, commutes_ct$samecounty, exclude = NULL)), c("residence", "samecounty", "count")) 
commute_tot <- commute_df2 %>% group_by(residence) %>% dplyr::summarise(tot = sum(count))
commute_df2 <- left_join(commute_df2, commute_tot, by = "residence")
commute_df2 <- commute_df2 %>% mutate(prop = count/tot)
commute_df2 <- commute_df2 %>% mutate(fips = as.integer(as.character(commute_df2$residence)) + 9000)


gcounty_commute <- left_join(gcounty, commute_df2, by = "fips")
gcounty_commute$samecounty <- as.integer(as.character(gcounty_commute$samecounty))
ggplot(gcounty_commute %>% filter(is.na(prop) == FALSE & samecounty == 0)) +
  geom_polygon(aes(long, lat, group = group, fill = prop)) +
  geom_text(data = county_label, aes(x = avg_long, y = avg_lat, label = c_name)) +
  scale_fill_gradientn(colours = brewer.pal(n=9, name = "YlGnBu"))
ggsave("./EDA_images/121_commute_out_per_county.png")

ggplot(gcounty_commute %>% filter(is.na(prop) == FALSE & samecounty == 1)) +
  geom_polygon(aes(long, lat, group = group, fill = prop)) +
  geom_text(data = county_label, aes(x = avg_long, y = avg_lat, label = c_name)) +
  scale_fill_gradientn(colours = brewer.pal(n=9, name = "YlGnBu"))
ggsave("./EDA_images/122_commute_win_per_county.png")


#------------------------------------------------------------------------
#Exploring demographic information against the high performing variables
#from the ML section

# stop duration and race by arrest
sd_race <- setNames(data.frame(table(df_clean$stop_duration, df_clean$driver_race_raw, df_clean$is_arrested, exclude = NULL)), c("Stop_Duration", "Race", "Arrested", "count")) 
sd_race_total <- sd_race %>% group_by(Stop_Duration, Race) %>% dplyr::summarise(total_count = sum(count))
sd_race <- left_join(sd_race, sd_race_total, by = c("Stop_Duration", "Race"))
sd_race <- sd_race %>% mutate(arrests_to_pop = count/total_count)

ggplot(sd_race %>% filter(Arrested == TRUE), aes(Race, arrests_to_pop, fill = Race)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(.~ Stop_Duration) +
  labs(title = "Arrests/stops by race and stop duration", x = "Race/Stop Duration", y = "Arrests/stops") +
  scale_y_continuous(labels = scales::percent) +
  theme1
ggsave("./EDA_images/126_arrest_rate_stop_duration_race.png")

# Not this way because can't get arrest/stop proportion
# df_clean_arrested <- df_clean %>% filter(is_arrested == TRUE)
# sd_race2 <- setNames(data.frame(table(df_clean_arrested$stop_duration, df_clean_arrested$driver_race_raw, exclude = NULL)), c("Stop_Duration", "Race", "count")) 
# sd_race_total2 <- sd_race2 %>% group_by(Race) %>% dplyr::summarise(total_count = sum(count))
# sd_race2 <- left_join(sd_race2, sd_race_total2, by = "Race")
# sd_race2 <- sd_race2 %>% mutate(arrests_to_pop = count/total_count)
# 
# ggplot(sd_race2, aes(Race, arrests_to_pop, fill = Race)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   facet_grid(.~ Stop_Duration)


# stop duration and gender by arrest
sd_gender <- setNames(data.frame(table(df_clean$stop_duration, df_clean$driver_gender, df_clean$is_arrested, exclude = NULL)), c("Stop_Duration", "Gender", "Arrested", "count")) 
sd_gender_total <- sd_gender %>% group_by(Stop_Duration, Gender) %>% dplyr::summarise(total_count = sum(count))
sd_gender <- left_join(sd_gender, sd_gender_total, by = c("Stop_Duration", "Gender"))
sd_gender <- sd_gender %>% mutate(arrests_to_pop = count/total_count)

ggplot(sd_gender %>% filter(Arrested == TRUE), aes(Gender, arrests_to_pop, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(.~ Stop_Duration)
ggsave("./EDA_images/127_arrest_rate_stop_duration_gender.png")


# search conducted and race by arrest
sc_race <- setNames(data.frame(table(df_clean$search_conducted, df_clean$driver_race_raw, df_clean$is_arrested, exclude = NULL)), c("Search_Conducted", "Race", "Arrested", "count")) 
sc_race_total <- sc_race %>% group_by(Search_Conducted, Race) %>% dplyr::summarise(total_count = sum(count))
sc_race <- left_join(sc_race, sc_race_total, by = c("Search_Conducted", "Race"))
sc_race <- sc_race %>% mutate(arrests_to_pop = count/total_count)

ggplot(sc_race %>% filter(Arrested == TRUE), aes(Race, arrests_to_pop, fill = Race)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(.~ Search_Conducted)
ggsave("./EDA_images/128_arrest_rate_search_conducted_race.png")


sc_race2 <- setNames(data.frame(table(df_clean_arrested$search_conducted, df_clean_arrested$driver_race_raw, exclude = NULL)), c("Search_Conducted", "Race", "count")) 
sc_race_total2 <- sc_race2 %>% group_by(Race) %>% dplyr::summarise(total_count = sum(count))
sc_race2 <- left_join(sc_race2, sc_race_total2, by = "Race")
sc_race2 <- sc_race2 %>% mutate(arrests_to_pop = count/total_count)

ggplot(sc_race2, aes(Race, arrests_to_pop, fill = Race)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(.~ Search_Conducted)


# search type raw and race by arrest
str_race <- setNames(data.frame(table(df_clean$search_type_raw, df_clean$driver_race_raw, df_clean$is_arrested, exclude = NULL)), c("Search_Type", "Race", "Arrested", "count")) 
str_race_total <- str_race %>% group_by(Search_Type, Race) %>% dplyr::summarise(total_count = sum(count))
str_race <- left_join(str_race, str_race_total, by = c("Search_Type", "Race"))
str_race <- str_race %>% mutate(arrests_to_pop = count/total_count)

ggplot(str_race %>% filter(Arrested == TRUE), aes(Race, arrests_to_pop, fill = Race)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(.~ Search_Type)
ggsave("./EDA_images/129_arrest_rate_search_type_race.png")


# contraband found and race by arrest
cf_race <- setNames(data.frame(table(df_clean$contraband_found, df_clean$driver_race_raw, df_clean$is_arrested, exclude = NULL)), c("Contraband_Found", "Race", "Arrested", "count")) 
cf_race_total <- cf_race %>% group_by(Contraband_Found, Race) %>% dplyr::summarise(total_count = sum(count))
cf_race <- left_join(cf_race, cf_race_total, by = c("Contraband_Found", "Race"))
cf_race <- cf_race %>% mutate(arrests_to_pop = count/total_count)

ggplot(cf_race %>% filter(Arrested == TRUE), aes(Race, arrests_to_pop, fill = Race)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(.~ Contraband_Found)
ggsave("./EDA_images/130_arrest_rate_contraband_found_race.png")


#stop_hour_part_of_day and race by arrest
df_clean_hr <- df_clean
df_clean_hr$stop_time_hour <- df_clean_hr$stop_time
df_clean_hr$stop_time_hour[df_clean_hr$stop_time_hour == "0:00"] <- NA
df_clean_hr$stop_time_hour <- sub(":.*", "", df_clean_hr$stop_time_hour)

# grouping hour into times of day since 24 levels might be too granular
df_clean_hr$stop_time_hour_numeric <- as.numeric(df_clean_hr$stop_time_hour)
df_clean_hr$stop_hour_part_of_day <- vector(mode = "character", length = nrow(df_clean_hr))
df_clean_hr$stop_hour_part_of_day[df_clean_hr$stop_time_hour_numeric >= 0 & df_clean_hr$stop_time_hour_numeric < 6] <- "time_block1"
df_clean_hr$stop_hour_part_of_day[df_clean_hr$stop_time_hour_numeric >= 6 & df_clean_hr$stop_time_hour_numeric < 12] <- "time_block2"
df_clean_hr$stop_hour_part_of_day[df_clean_hr$stop_time_hour_numeric >= 12 & df_clean_hr$stop_time_hour_numeric < 18] <- "time_block3"
df_clean_hr$stop_hour_part_of_day[df_clean_hr$stop_time_hour_numeric >= 18 & df_clean_hr$stop_time_hour_numeric <= 23] <- "time_block4"
df_clean_hr$stop_hour_part_of_day[df_clean_hr$stop_hour_part_of_day == ""] <- NA
df_clean_hr$stop_hour_part_of_day <- factor(df_clean_hr$stop_hour_part_of_day)


pod_race <- setNames(data.frame(table(df_clean_hr$stop_hour_part_of_day, df_clean_hr$driver_race_raw, df_clean_hr$is_arrested, exclude = NULL)), c("time_block", "Race", "Arrested", "count")) 
pod_race_total <- pod_race %>% group_by(time_block, Race) %>% dplyr::summarise(total_count = sum(count))
pod_race <- left_join(pod_race, pod_race_total, by = c("time_block", "Race"))
pod_race <- pod_race %>% mutate(arrests_to_pop = count/total_count)

ggplot(pod_race %>% filter(Arrested == TRUE), aes(Race, arrests_to_pop, fill = Race)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(.~ time_block)
ggsave("./EDA_images/131_arrest_rate_hr_pod_race.png")


#violation_raw_Registration
#df_split_vr <- df_split %>% filter(violation_raw_Registration == 1)
#df_split_arrest <- df_split %>% filter(is_arrested == TRUE)

vr_race <- setNames(data.frame(table(df_split$violation_raw_Registration, df_split$driver_race_raw, df_split$is_arrested, exclude = NULL)), c("Registration_Violation", "Race", "Arrested", "count")) 
vr_race_total <- vr_race %>% group_by(Registration_Violation, Race) %>% dplyr::summarise(total_count = sum(count))
vr_race <- left_join(vr_race, vr_race_total, by = c("Registration_Violation", "Race"))
vr_race <- vr_race %>% mutate(arrests_to_pop = count/total_count)

ggplot(vr_race %>% filter(Arrested == TRUE), aes(Race, arrests_to_pop, fill = Race)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(.~ Registration_Violation)
ggsave("./EDA_images/132_arrest_rate_viol_registration_race.png")


# Similar/same option
df_split_vr <- df_split %>% filter(violation_raw_Registration == 1)
vr_race2 <- setNames(data.frame(table(df_split_vr$driver_race_raw, df_split_vr$is_arrested, exclude = NULL)), c("Race", "Arrested", "count")) 
vr_race_total2 <- vr_race2 %>% group_by(Race) %>% dplyr::summarise(total_count = sum(count))
vr_race2 <- left_join(vr_race2, vr_race_total2, by = "Race")
vr_race2 <- vr_race2 %>% mutate(arrests_to_pop = count/total_count)

ggplot(vr_race2 %>% filter(Arrested == TRUE), aes(Race, arrests_to_pop, fill = Race)) +
  geom_bar(stat = "identity", position = "dodge")


#violation_raw_Other
vo_race <- setNames(data.frame(table(df_split$violation_raw_Other, df_split$driver_race_raw, df_split$is_arrested, exclude = NULL)), c("Other_Violation", "Race", "Arrested", "count")) 
vo_race_total <- vo_race %>% group_by(Other_Violation, Race) %>% dplyr::summarise(total_count = sum(count))
vo_race <- left_join(vo_race, vo_race_total, by = c("Other_Violation", "Race"))
vo_race <- vo_race %>% mutate(arrests_to_pop = count/total_count)

ggplot(vo_race %>% filter(Arrested == TRUE), aes(Race, arrests_to_pop, fill = Race)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(.~ Other_Violation)
ggsave("./EDA_images/133_arrest_rate_viol_other_race.png")


#violation_raw_Moving.Violation
vmv_race <- setNames(data.frame(table(df_split$violation_raw_Moving.Violation, df_split$driver_race_raw, df_split$is_arrested, exclude = NULL)), c("Moving_Violation", "Race", "Arrested", "count")) 
vmv_race_total <- vmv_race %>% group_by(Moving_Violation, Race) %>% dplyr::summarise(total_count = sum(count))
vmv_race <- left_join(vmv_race, vmv_race_total, by = c("Moving_Violation", "Race"))
vmv_race <- vmv_race %>% mutate(arrests_to_pop = count/total_count)

ggplot(vmv_race %>% filter(Arrested == TRUE), aes(Race, arrests_to_pop, fill = Race)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(.~ Moving_Violation)
ggsave("./EDA_images/134_arrest_rate_viol_moving_race.png")


# Compare to a non-important violation
#violation_raw_Window.Tint
vwt_race <- setNames(data.frame(table(df_split$violation_raw_Window.Tint, df_split$driver_race_raw, df_split$is_arrested, exclude = NULL)), c("WindowTint_Violation", "Race", "Arrested", "count")) 
vwt_race_total <- vwt_race %>% group_by(WindowTint_Violation, Race) %>% dplyr::summarise(total_count = sum(count))
vwt_race <- left_join(vwt_race, vwt_race_total, by = c("WindowTint_Violation", "Race"))
vwt_race <- vwt_race %>% mutate(arrests_to_pop = count/total_count)

ggplot(vwt_race %>% filter(Arrested == TRUE), aes(Race, arrests_to_pop, fill = Race)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(.~ WindowTint_Violation)
ggsave("./EDA_images/135_arrest_rate_viol_window_tint_race.png")


#violation_raw_Traffic.Control.Signal
vtcs_race <- setNames(data.frame(table(df_split$violation_raw_Traffic.Control.Signal, df_split$driver_race_raw, df_split$is_arrested, exclude = NULL)), c("TCS_Violation", "Race", "Arrested", "count")) 
vtcs_race_total <- vtcs_race %>% group_by(TCS_Violation, Race) %>% dplyr::summarise(total_count = sum(count))
vtcs_race <- left_join(vtcs_race, vtcs_race_total, by = c("TCS_Violation", "Race"))
vtcs_race <- vtcs_race %>% mutate(arrests_to_pop = count/total_count)

ggplot(vtcs_race %>% filter(Arrested == TRUE), aes(Race, arrests_to_pop, fill = Race)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(.~ TCS_Violation)
ggsave("./EDA_images/136_arrest_rate_viol_tcs_race.png")


#violation_raw_Cell.Phone
vcp_race <- setNames(data.frame(table(df_split$violation_raw_Cell.Phone, df_split$driver_race_raw, df_split$is_arrested, exclude = NULL)), c("Cell_Phone", "Race", "Arrested", "count")) 
vcp_race_total <- vcp_race %>% group_by(Cell_Phone, Race) %>% dplyr::summarise(total_count = sum(count))
vcp_race <- left_join(vcp_race, vcp_race_total, by = c("Cell_Phone", "Race"))
vcp_race <- vcp_race %>% mutate(arrests_to_pop = count/total_count)

ggplot(vcp_race %>% filter(Arrested == TRUE), aes(Race, arrests_to_pop, fill = Race)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(.~ Cell_Phone)
ggsave("./EDA_images/137_arrest_rate_viol_cp_race.png")


#violation_raw_Defective.Lights
vdl_race <- setNames(data.frame(table(df_split$violation_raw_Defective.Lights, df_split$driver_race_raw, df_split$is_arrested, exclude = NULL)), c("Defective_Lights", "Race", "Arrested", "count")) 
vdl_race_total <- vdl_race %>% group_by(Defective_Lights, Race) %>% dplyr::summarise(total_count = sum(count))
vdl_race <- left_join(vdl_race, vdl_race_total, by = c("Defective_Lights", "Race"))
vdl_race <- vdl_race %>% mutate(arrests_to_pop = count/total_count)

ggplot(vdl_race %>% filter(Arrested == TRUE), aes(Race, arrests_to_pop, fill = Race)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(.~ Defective_Lights)
ggsave("./EDA_images/138_arrest_rate_viol_dl_race.png")


#violation_raw_Display.Of.Plates
vdop_race <- setNames(data.frame(table(df_split$violation_raw_Display.of.Plates, df_split$driver_race_raw, df_split$is_arrested, exclude = NULL)), c("Display.Of.Plates", "Race", "Arrested", "count")) 
vdop_race_total <- vdop_race %>% group_by(Display.Of.Plates, Race) %>% dplyr::summarise(total_count = sum(count))
vdop_race <- left_join(vdop_race, vdop_race_total, by = c("Display.Of.Plates", "Race"))
vdop_race <- vdop_race %>% mutate(arrests_to_pop = count/total_count)

ggplot(vdop_race %>% filter(Arrested == TRUE), aes(Race, arrests_to_pop, fill = Race)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(.~ Display.Of.Plates)
ggsave("./EDA_images/139_arrest_rate_viol_dop_race.png")


#violation_raw_Equipment.Violation
vev_race <- setNames(data.frame(table(df_split$violation_raw_Equipment.Violation, df_split$driver_race_raw, df_split$is_arrested, exclude = NULL)), c("Equipment.Violation", "Race", "Arrested", "count")) 
vev_race_total <- vev_race %>% group_by(Equipment.Violation, Race) %>% dplyr::summarise(total_count = sum(count))
vev_race <- left_join(vev_race, vev_race_total, by = c("Equipment.Violation", "Race"))
vev_race <- vev_race %>% mutate(arrests_to_pop = count/total_count)

ggplot(vev_race %>% filter(Arrested == TRUE), aes(Race, arrests_to_pop, fill = Race)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(.~ Equipment.Violation)
ggsave("./EDA_images/140_arrest_rate_viol_ev_race.png")


#violation_raw_Seatbelt
vsb_race <- setNames(data.frame(table(df_split$violation_raw_Seatbelt, df_split$driver_race_raw, df_split$is_arrested, exclude = NULL)), c("Seatbelt", "Race", "Arrested", "count")) 
vsb_race_total <- vsb_race %>% group_by(Seatbelt, Race) %>% dplyr::summarise(total_count = sum(count))
vsb_race <- left_join(vsb_race, vsb_race_total, by = c("Seatbelt", "Race"))
vsb_race <- vsb_race %>% mutate(arrests_to_pop = count/total_count)

ggplot(vsb_race %>% filter(Arrested == TRUE), aes(Race, arrests_to_pop, fill = Race)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(.~ Seatbelt)
ggsave("./EDA_images/141_arrest_rate_viol_sb_race.png")


#violation_raw_Speed.Related
vsr_race <- setNames(data.frame(table(df_split$violation_raw_Speed.Related, df_split$driver_race_raw, df_split$is_arrested, exclude = NULL)), c("Speed.Related", "Race", "Arrested", "count")) 
vsr_race_total <- vsr_race %>% group_by(Speed.Related, Race) %>% dplyr::summarise(total_count = sum(count))
vsr_race <- left_join(vsr_race, vsr_race_total, by = c("Speed.Related", "Race"))
vsr_race <- vsr_race %>% mutate(arrests_to_pop = count/total_count)

ggplot(vsr_race %>% filter(Arrested == TRUE), aes(Race, arrests_to_pop, fill = Race)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(.~ Speed.Related)
ggsave("./EDA_images/142_arrest_rate_viol_sr_race.png")


#violation_raw_Stop.Sign
vss_race <- setNames(data.frame(table(df_split$violation_raw_Stop.Sign, df_split$driver_race_raw, df_split$is_arrested, exclude = NULL)), c("Stop.Sign", "Race", "Arrested", "count")) 
vss_race_total <- vss_race %>% group_by(Stop.Sign, Race) %>% dplyr::summarise(total_count = sum(count))
vss_race <- left_join(vss_race, vss_race_total, by = c("Stop.Sign", "Race"))
vss_race <- vss_race %>% mutate(arrests_to_pop = count/total_count)

ggplot(vss_race %>% filter(Arrested == TRUE), aes(Race, arrests_to_pop, fill = Race)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(.~ Stop.Sign)
ggsave("./EDA_images/143_arrest_rate_viol_ss_race.png")


#violation_raw_Suscpended.License
vsl_race <- setNames(data.frame(table(df_split$violation_raw_Suspended.License, df_split$driver_race_raw, df_split$is_arrested, exclude = NULL)), c("Suscpended.License", "Race", "Arrested", "count")) 
vsl_race_total <- vsl_race %>% group_by(Suscpended.License, Race) %>% dplyr::summarise(total_count = sum(count))
vsl_race <- left_join(vsl_race, vsl_race_total, by = c("Suscpended.License", "Race"))
vsl_race <- vsl_race %>% mutate(arrests_to_pop = count/total_count)

ggplot(vsl_race %>% filter(Arrested == TRUE), aes(Race, arrests_to_pop, fill = Race)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(.~ Suscpended.License)
ggsave("./EDA_images/144_arrest_rate_viol_sl_race.png")


# => Regardless of the "importance" of the violation in terms of the ML algorithms,
#    Hispanics always seem to ahve a higher arrest/stop proportion