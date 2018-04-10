library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)

# Read in file
df_clean <- read.csv("E:/Learning/Springboard Intro to Data Science/capstone/CT_cleaned_edit.csv")
df_split <- read.csv("E:/Learning/Springboard Intro to Data Science/capstone/CT_cleaned_split.csv")

#ONE-DIMENSIONAL DISTRIBUTIONS:

#stop_date:
df_clean$stop_date <- as.POSIXct(df_clean$stop_date, "%m/%d/%Y", tz = "America/New_York")
ggplot(df_clean, aes(stop_date)) + geom_bar()  +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%m/%Y") +
  theme(axis.text.x=element_text(angle=90, hjust=1))
# line plot
# find number of dates so one bin per date
no_days <- as.numeric(range(df_clean$stop_date)[2] - range(df_clean$stop_date)[1])
ggplot(df_clean, aes(stop_date)) + 
  geom_line(stat = "bin", bins = no_days) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%m/%Y") +
  theme(axis.text.x=element_text(angle=90, hjust=1))
#OR
df_stop_date2 <- setNames(data.frame(table(df_clean$stop_date)),c("Stop.Date","Count"))
df_stop_date2$Stop.Date <- as.POSIXct(df_stop_date2$Stop.Date, "%Y-%m-%d", tz = "America/New_York")
ggplot(df_stop_date2, aes(x = Stop.Date, y = Count, group = 1)) + geom_line() +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%m/%Y") +
  theme(axis.text.x=element_text(angle=90, hjust=1))
# Notes: There are a low number of stops around 02/2014 and 02/2015.
#        There are a higher number of stops around 05/2014 and 07/2014
#        Similar (yet smaller) peaks in December followed by a decrease and then increase in March.
#        In general, it seems like there are less stops around the end of the year/
#        beginning of the new year, and more stops over the summer.
# Specific dates of highs and lows:
head(df_stop_date2 %>% arrange(desc(Count)),10)
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
head(df_stop_date2 %>% arrange(Count),10)
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
#       No other dates seem to fall on a holiday.
#       The day before and after the 4th of July have high stop counts.
#       Otherwise, holidays don't seem to be a huge indicator of stop likelihood.

# month
df_clean$stop_month <- month(df_clean$stop_date)
df_clean$stop_month <- factor(df_clean$stop_month, 
                              ordered = TRUE,
                              levels = as.character(1:12))
ggplot(df_clean, aes(stop_month)) + geom_bar()
#proportion
months <- c(2,2,2,1,1,1,1,1,1,2,2,2)
# -> occurence of each month in dataset
months_df <- setNames(data.frame(factor(as.character(1:12), levels = as.character(1:12)), months), c("month", "occurrence"))
table_months <- setNames(data.frame(table(df_clean$stop_month)),c("month", "count"))
table_months$count <- as.numeric(table_months$count)
months_df <- left_join(table_months, months_df, by = "month")
months_df <- months_df %>% mutate(month_prop = count/occurrence)
ggplot(months_df, aes(month, month_prop)) + geom_bar(stat = "identity")
# Notes: The data set goes from 10/2013 - 03/2015 so these numbers might be skewed
#        heavily towards months 10, 11, 12, 1, 2, 3 which appear across multiple
#        years.
#        Dividing the counts by the number of years a month is present in the data set
#        gives a more proportion perspective. With this, we can see that May, August, 
#        and September have the highest stops while February has the least.

# month and year
#   To help solve the month only problem.
mo_yr_label <- seq(as.Date("2013/10/1"), as.Date("2015/3/31"), by = "month")
mo_yr_label <- format(mo_yr_label, "%m-%Y")
df_clean$month_year <- format(df_clean$stop_date, "%m-%Y")
df_clean$month_year <- factor(df_clean$month_year,
                              ordered = TRUE,
                              levels = mo_yr_label)
ggplot(df_clean, aes(month_year)) + geom_bar() + theme(axis.text.x=element_text(angle=90, hjust=1))
# Notes: The lowest months are 02/2014 and 02/2015, while the highest months are
#        05/2014 and 08/2014, which is consitent with the full date analysis.

# year
df_clean$stop_year <- year(df_clean$stop_date)
df_clean$stop_year <- factor(df_clean$stop_year, 
                              ordered = TRUE,
                              levels = c("2013", "2014", "2015"))
ggplot(df_clean, aes(stop_year)) + geom_bar()
# Notes: More stops in 2014 because the whole years of 2013 and 2015 aren't recorded.

# day
a <- c("01", "02", "03", "04", "05", "06", "07", "08", "09")
b <- as.character(10:31)
days_label <- c(a, b)
df_clean$day_of_month <- format(df_clean$stop_date, "%d")
df_clean$day_of_month <- factor(df_clean$day_of_month,
                              ordered = TRUE,
                              levels = days_label)
ggplot(df_clean, aes(day_of_month)) + geom_bar() + theme(axis.text.x=element_text(angle=90, hjust=1))
#proportion
days_prop <- c(rep(c(18,16), times = c(28, 2)), 11)
days_df <- setNames(data.frame(factor(days_label, levels = days_label), days_prop), c("day", "occurrence"))
table_days <- setNames(data.frame(table(df_clean$day_of_month)),c("day", "count"))
table_days$count <- as.numeric(table_days$count)
days_df <- left_join(table_days, days_df, by = "day")
days_df <- days_df %>% mutate(day_prop = count/occurrence)
ggplot(days_df, aes(day, day_prop)) + geom_bar(stat = "identity") + theme(axis.text.x=element_text(angle=90, hjust=1))
# Notes: Nothing sticks out here and given that there are not the same number of 
#        days per month and not the count of each month in the dataset, this isn't
#        the best graph to analyze.
#        Looking at the proportion, the end of the month seems to have more stops
#        than the beginning.

#day of the week
df_clean$day_of_week <- weekdays(as.Date(df_clean$stop_date))
df_clean$day_of_week <- factor(df_clean$day_of_week, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
ggplot(df_clean, aes(day_of_week)) + geom_bar()
#Notes: More attests happen towards the end of the week (Friday and Saturday) with
#       the least happening on Sunday.

#stop_time:
ggplot(df_clean %>% filter(stop_time != ""), aes(stop_time)) + 
  geom_bar()

# hour only; remove 0:00 since is a NA-filled value
df_clean$stop_time_hour <- sub(":.*", "", df_clean$stop_time)
df_clean$stop_time_hour <- factor(df_clean$stop_time_hour, 
                                  ordered = TRUE,
                                  levels = as.character(0:23))
ggplot(df_clean %>% filter(stop_time_hour != ""), aes(stop_time_hour)) + geom_bar()
# Notes: Low number of stops from 0300 - 0500 while the highest number of stops occur
#        between 0900 and 1000 possibly coinciding with the high volume of people 
#        driving to work?
#        Other smaller peaks around 1400 and 2100.
#        When comparing the hours (not as a rearranged, ordered factor) graph to 
#        the time (hours and minutes) graph, there are not really any differences in patterns, 
#        so using just the hours graph for investigation is okay, and also easier.
#        Every day has the same number of hours regardless of year or month or day
#        of month, so don't need to do proportions to normalize.

#county_name:
ggplot(df_clean %>%  filter(county_name != ""), aes(county_name)) + 
  geom_bar() +
  theme(axis.text.x=element_text(angle=90, hjust=1))
# Notes: The county with the most stops is New Haven County and the one with the
#        one with the least is Litchfield County

#driver_gender:
ggplot(df_clean, aes(driver_gender)) + 
  geom_bar()
ggplot(df_clean, aes(factor(1), ..count.., fill = driver_gender)) +  geom_bar(position = "fill")
#Notes: Men make up the most stops at over 60%
#Proportions
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
gender_totals <- data.frame(arrest_by_gender %>% group_by(Gender) %>% summarise(sum_gender = sum(count)))
arrest_by_gender <- left_join(arrest_by_gender, gender_totals, by = "Gender")
arrest_by_gender <- arrest_by_gender %>% mutate(percent_gender  = count/sum_gender)
ggplot(arrest_by_gender, aes(Arrested, count, fill = Gender)) + geom_bar(stat = "identity")
ggplot(arrest_by_gender, aes(Arrested, percent_gender, fill = Gender)) + geom_bar(stat = "identity")

x_labs <- plt_g$data[[1]][,c("prop", "x", "group")]
x_labs <- x_labs %>% arrange(x_labs, group)
ggplot(df_clean, aes(is_arrested, fill = driver_gender)) + 
  geom_bar(aes(y = ..prop.., group = driver_gender), position = "dodge")
# #getting a better picture of is_arrested == TRUE
# ggplot(df_clean %>% filter(is_arrested == TRUE | is.na(is_arrested) == TRUE), aes(is_arrested, fill = driver_gender)) + 
#   geom_bar(aes(y = ..prop.., group = driver_gender), position = "dodge")
# -> trying to show percent arrest of gender as a percent of the total gender in the dataset

#driver_age:
x_breaks <- seq(min(df_clean$driver_age[is.na(df_clean$driver_age) != TRUE]), max(df_clean$driver_age[is.na(df_clean$driver_age) != TRUE]) + 5, 5)
ggplot(df_clean %>%  filter(is.na(driver_age) != TRUE), aes(driver_age)) + 
  geom_bar() + 
  scale_x_continuous(breaks = as.integer(x_breaks), labels = as.character(x_breaks))
#Notes: The ages with the most stops are from 20-25 with the number of stops decreasing
#       as age increased, save for another small peak at age 45.

#driver_race_raw:
ggplot(df_clean, aes(driver_race_raw)) + geom_bar()
ggplot(df_clean, aes(driver_race_raw, ..prop.., group = 1)) + geom_bar()
#Notes: Most of the population is white, taking up about 75% of the population

#search_conducted:
ggplot(df_clean, aes(search_conducted)) + geom_bar()
ggplot(df_clean, aes(search_conducted, ..prop.., group = 1)) + geom_bar()
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
ggplot(df_clean %>% filter(df_clean$search_type_raw != ""), aes(search_type_raw)) + geom_bar()
#Notes: Consent and Other are the two top search types, with inventory coming in
#       a very far third

#contraband_found:
ggplot(df_clean, aes(contraband_found)) + geom_bar()
c <- ggplot(df_clean, aes(contraband_found, ..prop.., group = 1)) + geom_bar()
plt_c <- ggplot_build(c)
plt_c$data[[1]]
#Notes: A very small proportion of contraband was count (0.57%)

#stop_outcome:
ggplot(df_clean %>% filter(is.na(stop_outcome) != TRUE), aes(stop_outcome)) + geom_bar()
#Notes: Most stops end in a ticket, with verbal warning coming in a far second.

#is_arrested:
ggplot(df_clean %>% filter(is_arrested != ""), aes(is_arrested)) + geom_bar()
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
ggplot(df_clean, aes(officer_id)) + geom_bar()
#There are some heavy stoppers so let's narrow the window to investigate the top 50
officer_stops <- setNames(data.frame(table(df_clean$officer_id)), c("officer", "count"))
officer_stops <- officer_stops %>% arrange(desc(count))
top_50 <- head(officer_stops, 50)
ggplot(top_50, aes(officer, count)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=90, hjust=1))
#Notes: There are some officers who have a significantly higher count of stops than
#       the rest. Here are the top 3:
# officer count
# 1   706750556  2822
# 2  1000003154  2746
# 3   790642042  2348

#stop_duration:
ggplot(df_clean, aes(stop_duration)) + geom_bar()
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
ggplot(df_clean, aes(violation_count)) + geom_bar()
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
  geom_col() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
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
  geom_col() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
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


# ggplot(df_split2, aes(reorder(violation_new, -n), n, fill = driver_race_raw)) + geom_col() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
# ggplot(df_split2, aes(violation_new, n, fill = driver_race_raw)) + geom_col()
# ggplot(df_split2, aes(reorder(violation_new, -n), n, fill = driver_race_raw)) + geom_col(position = "fill") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
# ggplot(df_split2, aes(reorder(violation_new, -n), n, fill = driver_race_raw)) + geom_col(position = "fill") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
