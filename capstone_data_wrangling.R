library(dplyr)
library(tidyr)
library(stringr)
library(splitstackshape)
library(ggplot2)

# Read in file
df <- read.csv("E:/Learning/Springboard Intro to Data Science/capstone/CT_cleaned.csv", stringsAsFactors = FALSE)

df_clean <- df

#id
# Any duplicate ids?
df_clean[duplicated(df_clean$id) == TRUE]
# -> no
# Any blanks? NAs?
df_clean %>% filter(id == "" | is.na(id) == TRUE)
# -> no

#stop_time
class(df_clean$stop_time)
# -> character
# can't change type to time
# set time for unknown times to 0:00 to make datetime field later
df_clean$stop_time[df_clean$stop_time == ""] <- "0:00"

#combine stop_date and stop_time and make into new column
df_clean$stop_date_time <- paste(df_clean$stop_date, df_clean$stop_time, sep =" ")
# change to datetime
df_clean$stop_date_time2 <- as.POSIXct(df_clean$stop_date_time, format = "%m/%d/%Y %H:%M")
# How many NAs are there? 
# -> The date/times that are NA are 3/9/2014 and 3/8/2015 in the 0200 hour time frame, which is
#    Daylight Savings Time. Since 0200 - 0259 doesn't exist on those days and is technically
#    0300- 0359, I will need to add an hour to those times. 
# -> CT observes DST so I will update these times one hour (0200 -> 0300)
# df_clean %>% filter(stop_time == "") %>% select(stop_date_time)
x <- df_clean %>% filter(is.na(stop_date_time2) == TRUE) %>% select(stop_date, stop_time, stop_date_time, stop_date_time2)
x_dst <- x$stop_date_time
x_dst <- gsub(pattern = "2:", replacement = "3:", x = x_dst)
df_clean[is.na(df_clean$stop_date_time2) == TRUE,]$stop_date_time <- x_dst
# re-run stop_date_time2 calculations
df_clean$stop_date_time2 <- as.POSIXct(df_clean$stop_date_time, format = "%m/%d/%Y %H:%M")


#stop_date
class(df_clean$stop_date)
# -> character
# change type to date
df_clean$stop_date <- as.POSIXct(df_clean$stop_date, format = "%m/%d/%Y")
# Any NAs?
df_clean %>% filter(is.na(stop_date) == TRUE)
# -> no


#location_raw & fine_grained_location
# Not going to clean because county_name & county_fips track location more cleanly

#county_name
df_clean %>% arrange(county_name) %>% select(county_name) %>% distinct()
# -> some blanks worth investigating/cleaning
cn_blanks <- df_clean %>% filter(county_name == "") %>%  select(location_raw, county_fips, fine_grained_location)
# main st:
df_clean %>% filter(grepl(pattern = "*main st*", x = tolower(fine_grained_location))) %>% select(fine_grained_location, county_name, location_raw) %>% distinct(county_name)
# -> no distinct county for "main st"
# rocky neck:
df_clean %>% filter(grepl(pattern = "*rocky neck*", x = tolower(fine_grained_location))) %>% select(fine_grained_location, county_name) %>% distinct(county_name)
# -> distinct county is New London County
# -> Google search indicates it's a state park in New London
# figure out the corresponding FIP
df_clean %>% filter(county_name == "New London County") %>% select(county_fips) %>% distinct()
# -> 9011
# update county_name and county_fips for "rocky neck" matches
df_clean <- df_clean %>% mutate(county_name = replace(county_name, county_name == "" & grepl(pattern = "*rocky neck*", x = tolower(fine_grained_location)), "New London County"),
                          county_fips = replace(county_fips, county_name == "New London County" & is.na(county_fips), 9011))
# update matches for "east lyme"
df_clean <- df_clean %>% mutate(county_name = replace(county_name, county_name == "" & grepl(pattern = "*east lyme*", x = tolower(fine_grained_location)), "New London County"),
                                county_fips = replace(county_fips, county_name == "New London County" & is.na(county_fips), 9011))
# update matches for "37 hov" with FIP 9003 and county name Hartford County
df_clean <- df_clean %>% mutate(county_name = replace(county_name, county_name == "" & grepl(pattern = "*37 hov*", x = tolower(fine_grained_location)), "Hartford County"),
                                county_fips = replace(county_fips, county_name == "Hartford County" & is.na(county_fips), 9003))
# update matches for "niantic"
df_clean <- df_clean %>% mutate(county_name = replace(county_name, county_name == "" & grepl(pattern = "*niantic*", x = tolower(fine_grained_location)), "New London County"),
                                county_fips = replace(county_fips, county_name == "New London County" & is.na(county_fips), 9011))
# update matches for "terminal"
df_clean <- df_clean %>% mutate(county_name = replace(county_name, county_name == "" & grepl(pattern = "*terminal*", x = tolower(fine_grained_location)), "Hartford County"),
                                county_fips = replace(county_fips, county_name == "Hartford County" & is.na(county_fips), 9003))
# update matches for "I-95 sb 43"
df_clean <- df_clean %>% mutate(county_name = replace(county_name, county_name == "" & grepl(pattern = "*i-95 sb 43*", x = tolower(fine_grained_location)), "New Haven County"),
                                county_fips = replace(county_fips, county_name == "New Haven County" & is.na(county_fips), 9009))
#update matches for "rt 6/195"
df_clean <- df_clean %>% mutate(county_name = replace(county_name, county_name == "" & grepl(pattern = "*rt 6/195*", x = tolower(fine_grained_location)), "Tolland County"),
                                county_fips = replace(county_fips, county_name == "Tolland County" & is.na(county_fips), 9013))
# update matches for "schoeph"
df_clean <- df_clean %>% mutate(county_name = replace(county_name, county_name == "" & grepl(pattern = "*schoeph*", x = tolower(fine_grained_location)), "Hartford County"),
                                county_fips = replace(county_fips, county_name == "Hartford County" & is.na(county_fips), 9003))
# update matches for "e litchfield rd"
df_clean <- df_clean %>% mutate(county_name = replace(county_name, county_name == "" & grepl(pattern = "*e litchfield rd*", x = tolower(fine_grained_location)), "Litchfield County"),
                                county_fips = replace(county_fips, county_name == "Litchfield County" & is.na(county_fips), 9005))
# update matches for "T130"
df_clean <- df_clean %>% mutate(county_name = replace(county_name, county_name == "" & grepl(pattern = "*t130*", x = tolower(fine_grained_location)), "New Haven County"),
                                county_fips = replace(county_fips, county_name == "New Haven County" & is.na(county_fips), 9009))
# update matches for "RT 229 @ RT 72"
df_clean <- df_clean %>% mutate(county_name = replace(county_name, county_name == "" & grepl(pattern = "*rt 229 @ rt 72*", x = tolower(fine_grained_location)), "Hartford County"),
                                county_fips = replace(county_fips, county_name == "Hartford County" & is.na(county_fips), 9003))
#update matches for "i84 exit 9"
df_clean <- df_clean %>% mutate(county_name = replace(county_name, county_name == "" & grepl(pattern = "*i84 exit 9*", x = tolower(fine_grained_location)), "Fairfield County"),
                                county_fips = replace(county_fips, county_name == "Fairfield County" & is.na(county_fips), 9001))
# update matches for "liberty way"
df_clean <- df_clean %>% mutate(county_name = replace(county_name, county_name == "" & grepl(pattern = "*liberty way*", x = tolower(fine_grained_location)), "New London County"),
                                county_fips = replace(county_fips, county_name == "New London County" & is.na(county_fips), 9011))
# update matches for "rt 8 n ext 23"
df_clean <- df_clean %>% mutate(county_name = replace(county_name, county_name == "" & grepl(pattern = "*rt 8 n ext 23*", x = tolower(fine_grained_location)), "New Haven County"),
                                county_fips = replace(county_fips, county_name == "New Haven County" & is.na(county_fips), 9009))
# update matches for "i-91 south exit 4"
df_clean <- df_clean %>% mutate(county_name = replace(county_name, county_name == "" & grepl(pattern = "*i-91 south exit 4*", x = tolower(fine_grained_location)), "New Haven County"),
                                county_fips = replace(county_fips, county_name == "New Haven County" & is.na(county_fips), 9009))
# update matches for "rt 172"
df_clean <- df_clean %>% mutate(county_name = replace(county_name, county_name == "" & grepl(pattern = "*rt 172*", x = tolower(fine_grained_location)), "New Haven County"),
                                county_fips = replace(county_fips, county_name == "New Haven County" & is.na(county_fips), 9009))
# update matches for "91n x 34"
df_clean <- df_clean %>% mutate(county_name = replace(county_name, county_name == "" & grepl(pattern = "*91n x 34*", x = tolower(fine_grained_location)), "Hartford County"),
                                county_fips = replace(county_fips, county_name == "Hartford County" & is.na(county_fips), 9003))
# update matches for "main t045"
df_clean <- df_clean %>% mutate(county_name = replace(county_name, county_name == "" & grepl(pattern = "*main t045*", x = tolower(fine_grained_location)), "New London County"),
                                county_fips = replace(county_fips, county_name == "New London County" & is.na(county_fips), 9011))
# update matches for "91 n x 36"
df_clean <- df_clean %>% mutate(county_name = replace(county_name, county_name == "" & grepl(pattern = "*91 n x 36*", x = tolower(fine_grained_location)), "Hartford County"),
                                county_fips = replace(county_fips, county_name == "Hartford County" & is.na(county_fips), 9003))
# update matches for "fern"
df_clean <- df_clean %>% mutate(county_name = replace(county_name, county_name == "" & grepl(pattern = "*fern*", x = tolower(fine_grained_location)), "Litchfield County"),
                                county_fips = replace(county_fips, county_name == "Litchfield County" & is.na(county_fips), 9005))
# update matches for "nb south of exit 15" 
df_clean <- df_clean %>% mutate(county_name = replace(county_name, county_name == "" & grepl(pattern = "*nb south of exit 15*", x = tolower(fine_grained_location)), "New Haven County"),
                                county_fips = replace(county_fips, county_name == "New Haven County" & is.na(county_fips), 9009))
#update matches for "x 64"
df_clean <- df_clean %>% mutate(county_name = replace(county_name, county_name == "" & grepl(pattern = "*x 64*", x = tolower(fine_grained_location)), "Middlesex County"),
                                county_fips = replace(county_fips, county_name == "Middlesex County" & is.na(county_fips), 9007))
#update matches for "post road fairfield"
df_clean <- df_clean %>% mutate(county_name = replace(county_name, county_name == "" & grepl(pattern = "*post road fairfield*", x = tolower(fine_grained_location)), "Fairfield County"),
                                county_fips = replace(county_fips, county_name == "Fairfield County" & is.na(county_fips), 9001))
# -> no other groups (patterns that appear more than once) to investigate-- 16 blank county_names left

#county_fips
df_clean %>% arrange(county_fips) %>% select(county_fips) %>% distinct()
# -> some NAs
df_clean %>% filter(is.na(county_fips)) %>%  select(location_raw, county_fips, fine_grained_location, county_name)
# -> no useful information to fill in the NAs

#police_department
df_clean %>% arrange(police_department) %>% select(police_department) %>% distinct()
# -> one value = State Police
# Any blanks? NAs?
df_clean %>% filter(police_department == "" | is.na(police_department) == TRUE)
# -> no

#driver_gender
df_clean %>% select(driver_gender) %>% distinct()
# -> all either M or F
# Any blanks? NAs?
df_clean %>% filter(driver_gender == "" | is.na(driver_gender) == TRUE)
# -> no

#driver_age_raw
df_clean %>% arrange(driver_age_raw) %>% select(driver_age_raw) %>% distinct()

#driver_age
df_clean %>% arrange(driver_age) %>% select(driver_age) %>% distinct()
# -> made < 15 blanks -- leave as is
# Use this column over raw because < 15 entries have been cleared
# make blanks NAs for easier use with is.na()
df_clean <- df_clean %>% mutate(driver_age = replace(driver_age, driver_age == "", NA))
df_clean %>% filter(is.na(driver_age) == TRUE) %>% count()
# -> 274 NAs
# investigate spread of ages
mean(df_clean$driver_age, na.rm = TRUE)
# -> 38.11949
median(df_clean$driver_age, na.rm = TRUE)
# -> 35
# -> Not much difference between median and mean
class(df_clean$driver_age)
# -> integer
ggplot(df_clean, aes(driver_age)) + geom_bar()
#    There seems to be a small bump towards age 100, indicating maybe some inaccuracy
#    in capturing elderly individual's age. To investigate, count ages:
age_table <- df_clean %>% arrange(driver_age) %>% group_by(driver_age) %>% count()
tail(age_table, 25)
# driver_age     n
# <int> <int>
#   1         76   426
#   2         77   306
#   3         78   356
#   4         79   261
#   5         80   295
#   6         81   171
#   7         82   166
#   8         83   144
#   9         84   135
#   10         85   115
# # ... with 15 more rows
tail(age_table, 15)
# driver_age     n
# <int> <int>
#   1         86    70
#   2         87    69
#   3         88    48
#   4         89    30
#   5         90    36
#   6         91    22
#   7         92    14
#   8         93     6
#   9         94     8
#   10         95     7
#   11         96     2
#   12         97     3
#   13         98     1
#   14         99    84
#   15         NA   274
#    -> Bump seems to be coming from age 99. However, the 80s also seem to have a 
#       high volume of people, so will make >= 80 NA.
df_clean <- df_clean %>% mutate(driver_age = replace(driver_age, driver_age >= 80, NA))
# Recheck mean and median age:
mean(df_clean$driver_age, na.rm = TRUE)
# -> 37.91098
median(df_clean$driver_age, na.rm = TRUE)
# -> 35
# By gender
df_clean %>% group_by(driver_gender) %>% summarise(mean(driver_age, na.rm = TRUE))
# driver_gender `mean(driver_age, na.rm = TRUE)`
# <chr>                                    <dbl>
#   1 F                                         37.4
#   2 M                                         38.2
df_clean %>% group_by(driver_gender) %>% summarise(median(driver_age, na.rm = TRUE))
# driver_gender `median(driver_age, na.rm = TRUE)`
# <chr>                                      <dbl>
#   1 F                                            35.
#   2 M                                            36.
# -> Since the ages are integers and the median is an integer & the mean and median are similar, I'll populate NAs with the median where appropriate.
#    Additionally, the ages are positively skewed, so median is a better measure to use that is not as affected by the skew.
#    To make the medians used even more accurate, I'll break up assignment based on gender, using 35 for Females nad 36 for Males.


#driver_race_raw
df_clean %>% arrange(driver_race_raw) %>% select(driver_race_raw) %>% distinct()
# Any blanks? NAs?
df_clean %>% filter(driver_race_raw == "" | is.na(driver_race_raw) == TRUE)
# -> no

#driver_race
df_clean %>% arrange(driver_race) %>% select(driver_race) %>% distinct()
# -> Use driver_race_raw since driver_race buckets into "other" which removes details

#violation 
# Any blanks? NAs?
df_clean %>% filter(violation_raw == "" | is.na(violation_raw) == TRUE)
# -> no
# Which violation column should I use? Is one cleaner than the other?
df_clean %>% arrange(violation) %>% select(violation) %>% distinct() %>% count()
# -> 125
df_clean %>% arrange(violation_raw) %>% select(violation_raw) %>% distinct() %>% count()
# -> 202
# -> Both have close to the same number of distinct entries, indicating one isn't really better/cleaner than the other
# saving the distinct entries of the columns as vectors to compare:
dist_viol <- df_clean %>% arrange(violation) %>% select(violation) %>% distinct()
dist_viol_raw <- df_clean %>% arrange(violation_raw) %>% select(violation_raw) %>% distinct()
dist_viol <- as.vector(dist_viol[[1]])
dist_viol_raw <- as.vector(dist_viol_raw[[1]])
# How many elements in dist_viol_raw match dist_viol?
subset(dist_viol_raw, !(tolower(dist_viol_raw) %in% tolower(dist_viol)))
# -> 195 elements returned, i.e. not many similar violations
# -> use raw since has more groupings, so more detail & the non-raw doesn't show much of an advantage so use original/source
# separate distinct violations into separate columns
# first: determine max violations for a stop
no_viols <- str_count(dist_viol_raw, ",")
max(no_viols) + 1
# 5 -> = number of new columns to create
# second: create # of violations column & copy of violation_raw column to use in separate()
df_clean <- df_clean %>% mutate(violation_raw2 = violation_raw,
                                violation_count = str_count(violation_raw, ",") + 1)
df_clean <- separate(df_clean, violation_raw2, c("violation_raw1", "violation_raw2", "violation_raw3", "violation_raw4", "violation_raw5"), sep = ",")

#search_conducted
df_clean %>% select(search_conducted) %>% distinct()
df_clean %>% filter(search_conducted == "" | is.na(search_conducted) == TRUE)
# -> TRUE or FALSE, no missing values

#search_type & search_type_raw
df_clean %>% arrange(search_type_raw) %>% select(search_type_raw) %>% distinct()
df_clean %>% arrange(search_type) %>% select(search_type) %>% distinct()
# -> no difference between search_type and search_type_raw
# change blanks to NAs
df_clean <- df_clean %>% mutate(search_type = replace(search_type, search_type == "", NA))
df_clean %>% count(search_type)
# -> 313823 NAs-- aligns closely with search_conducted == TRUE

#contraband_found
df_clean %>% select(contraband_found) %>% distinct()
df_clean %>% filter(contraband_found == "" | is.na(contraband_found) == TRUE)
# -> TRUE or FALSE, no missing values

#stop_outcome
df_clean %>% arrange(stop_outcome) %>% select(stop_outcome) %>% distinct()
# make blanks NA
df_clean <- df_clean %>% mutate(stop_outcome = replace(stop_outcome, stop_outcome == "", NA))
df_clean %>% count(stop_outcome)
# -> 5356 NAs

#is_arrested
df_clean %>% select(is_arrested) %>% distinct()
# -> already has NAs
df_clean %>% count(is_arrested)
# -> 5356 NAs

#officer_id
df_clean %>% arrange(officer_id) %>% select(officer_id) %>% distinct()
class(df_clean$officer_id)
# -> character
is.numeric(df_clean$officer_id)
# -> FALSE
df[is.na(df_clean$officer_id) ==  TRUE, ]
# -> 0 rows returned, no NAs
df[df_clean$officer_id ==  "", ]
# -> no blanks
df_clean %>% arrange(officer_id) %>% select(officer_id) %>% distinct() %>% count()
# -> 1225 unique officers

#stop_duration
df_clean %>% arrange(stop_duration) %>% select(stop_duration) %>% distinct()
# -> no NAs or  blanks
#make ordered factor
df_clean$stop_duration_fact <- factor(df_clean$stop_duration, order = TRUE)


#write df_clean to file
currentDate <- Sys.Date()
#df_clean_filename <- paste("E:/Learning/Springboard Intro to Data Science/capstone/CT_cleaned_edit", currentDate, ".csv", sep = "")
df_clean_filename <- "E:/Learning/Springboard Intro to Data Science/capstone/CT_cleaned_edit.csv"
write.csv(file = df_clean_filename, x=df_clean, row.names = FALSE)
#write.csv(file = "E:/Learning/Springboard Intro to Data Science/capstone/CT_cleaned_edit4_10_2018.csv", x=df_clean, row.names = FALSE)

#Make second data frame with different layout; splitting violation_raw:
# solution found here: https://stackoverflow.com/questions/42387859/dummify-character-column-and-find-unique-values
library(splitstackshape)

#remove other violation columns to make datset smaller
df_split <- df_clean %>% select(-(violation_raw1:violation_raw5))
#split
df_split <- cSplit_e(df_split, "violation_raw", ",", mode = "binary", type = "character", fill = 0)

#write df_split to file
#df_split_filename <- paste("E:/Learning/Springboard Intro to Data Science/capstone/CT_cleaned_split", currentDate, ".csv", sep = "")
df_split_filename <- "E:/Learning/Springboard Intro to Data Science/capstone/CT_cleaned_split.csv"
write.csv(file = df_split_filename, x=df_split, row.names = FALSE)
#write.csv(file = "E:/Learning/Springboard Intro to Data Science/capstone/CT_cleaned_split.csv", x=df_split, row.names = FALSE)


#INVESTIGATION OF NAS
#Columns (of interest) with NAs/missing values:
# -> stop_time: 222
# -> county_name: 38
# -> driver_age: 274
# -> search_type: 313823
# -> stop_outcome: 5356
# -> is_arrested: 5356

#How many total rows are affected:
df_clean %>% filter(stop_time == "0:00") %>% select(stop_time, county_name, driver_age, search_type, stop_outcome, is_arrested) #%>% count()
df_clean %>% filter(stop_time == "0:00" & is.na(search_type) == TRUE) %>% select(stop_time, county_name, driver_age, search_type, stop_outcome, is_arrested) %>% count()
# -> 215, stop_time missing values also largely have search_type missing values, nothing else really
df_clean %>% filter(county_name == "") %>% select(stop_time, county_name, driver_age, search_type, stop_outcome, is_arrested) #%>% count()
# -> county_name missing values also largely have search_type missing values, no other columns really
df_clean %>% filter(is.na(driver_age) == TRUE) %>% select(stop_time, county_name, driver_age, search_type, stop_outcome, is_arrested) #%>% count()
# -> county_name missing values also largely have search_type missing values, no other columns really
df_clean %>% filter(is.na(search_type) == TRUE) %>% select(stop_time, county_name, driver_age, search_type, stop_outcome, is_arrested) #%>% count()
# Does search_type = NA coincide with search_conducted = FALSE?
df_clean %>% filter(is.na(search_type) == TRUE & search_conducted == TRUE) %>% select(search_type, search_conducted) %>%  count()
# -> 486 searches conducted with no search type
df_clean %>% filter(is.na(search_type) == TRUE & search_conducted == FALSE) %>% select(search_type, search_conducted) %>%  count()
# -> the rest of the blank value search_types (313337) coincide with search_conducted == FALSE, as expected
# Any searches not conducted but there's a search type?
df_clean %>% filter(is.na(search_type) != TRUE & search_conducted == FALSE) %>% select(search_type, search_conducted) %>%  count()
# -> 0 as expected, no
df_clean %>% filter(is.na(stop_outcome) == TRUE) %>% select(stop_time, county_name, driver_age, search_type, stop_outcome, is_arrested)
df_clean %>% filter(is.na(stop_outcome) == TRUE & is.na(is_arrested) == TRUE) %>% select(stop_outcome, is_arrested) %>%  count()
df_clean %>% filter(is.na(stop_outcome) == TRUE & is.na(is_arrested) != TRUE) %>% select(stop_outcome, is_arrested) %>%  count()
# -> is_arrested and stop_outcome have same NAs
df_clean %>% filter(is.na(is_arrested) == TRUE) %>% select(stop_time, county_name, driver_age, search_type, stop_outcome, is_arrested) #%>% count()
df_clean %>% filter(is.na(is_arrested) == TRUE | stop_time == "0:00" | county_name == "" | is.na(driver_age) == TRUE| is.na(search_type) == TRUE | is.na(stop_outcome) == TRUE) %>% count()
# -> 313879
#finding rows where search_type != NA but other columns are
df_clean %>% filter((is.na(is_arrested) == TRUE | stop_time == "0:00" | county_name == "" | is.na(driver_age) == TRUE | is.na(stop_outcome) == TRUE) & is.na(search_type) == FALSE) %>% select(stop_time, county_name, driver_age, search_type, stop_outcome, is_arrested)
# -> largely due to stop_outcome and is_arrested NAs
# OVERALL: Most of the NA columns don't impact the NA status of other variables, save for search_conducted obviously influencing search_type.

#Thoughts on how to handle NAs:
# -> stop_time: When analyzing just time (not datetime), remove NAs from analysis. 
#               For time series analysis (with datetime), to use other times, set to 0:00 since no other non-NA time is 0:00, so will be distinct and is the standard default time for a day with no time. 
# -> county_name: Remove NAs from analysis since such small number and no logical way to interpolate a fill value (like a mean value).
# -> driver_age: If just analyzing age, remove NAs.
#                If using in combination with other columns that are populated in those rows, use median (see above for explanation).
# -> search_type: There are 486 searches conducted with no search_type. Since a fill value can't logically be inferred, I'll ignore those NAs from analysis of search_type and search_conducted.
# -> stop_outcome: Since this is one of the variables I'd like to predict, it makes no sense to populate missing values.
#                  Similarly, there is no logical way to interpolate an "average" value to populate.                 
# -> is_arrested: Since this is one of the variables I'd like to predict, it makes no sense to populate missing values.

# Will be using:
#   - search_type over search_type_raw
#   - driver_race_raw over driver_race
#   - driver_age over driver_age_raw
