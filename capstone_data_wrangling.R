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
# set time for unknown times to 0:00
df_clean$stop_time[df_clean$stop_time == ""] <- "0:00"

#combine stop_date and stop_time and make into new column
df_clean$stop_date_time <- paste(df_clean$stop_date, df_clean$stop_time, sep =" ")
# change to datetime
df_clean$stop_date_time2 <- as.POSIXct(df_clean$stop_date_time, format = "%m/%d/%Y %H:%M")
# How many NAs are there?
# df_clean %>% filter(stop_time == "") %>% select(stop_date_time)
x <- df_clean %>% filter(is.na(stop_date_time2) == TRUE) %>% select(stop_date, stop_time, stop_date_time)

#stop_date
class(df_clean$stop_date)
# -> character
# change type to date
df_clean$stop_date <- as.Date(df_clean$stop_date, "%m/%d/%Y")
# Any NAs?
df_clean %>% filter(is.na(stop_date) == TRUE)
# -> no

#location_raw
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
# -> no other groups (patterns that appear more than once) to investigate-- 38 blank county_names left

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
# make blanks NAs for easier use with is.na()
df_clean <- df_clean %>% mutate(driver_age = replace(driver_age, driver_age == "", NA))
df_clean %>% filter(is.na(driver_age) == TRUE) %>% count()
# -> 274 NAs

#driver_race_raw
df_clean %>% arrange(driver_race_raw) %>% select(driver_race_raw) %>% distinct()
# Any blanks? NAs?
df_clean %>% filter(driver_race_raw == "" | is.na(driver_race_raw) == TRUE)
# -> no

#driver_race
df_clean %>% arrange(driver_race) %>% select(driver_race) %>% distinct()
# -> use driver_race_raw since driver_race buckets into "other" which removes details

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
# separate violations into separate columns
# first: determine max violations for a stop
no_viols <- str_count(dist_viol_raw, ",")
max(no_viols) + 1
# 5 -> number of new columns to create
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
# -> 313823 NAs found!!

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
write.csv(file = "E:/Learning/Springboard Intro to Data Science/capstone/CT_cleaned_edit3.csv", x=df_clean, row.names = FALSE)

#Make second data frame with different layout; splitting violation_raw:
# solution found here: https://stackoverflow.com/questions/42387859/dummify-character-column-and-find-unique-values
library(splitstackshape)

df_split <- df_clean %>% select(-(violation_raw1:violation_raw5))
df_split <- cSplit_e(df_split, "violation_raw", ",", mode = "binary", type = "character", fill = 0)

#write df_split to file
write.csv(file = "E:/Learning/Springboard Intro to Data Science/capstone/CT_cleaned_split.csv", x=df_split, row.names = FALSE)


#Summary
#Columns (of interest) with NAs/missing values:
# -> stop_time
# -> county_name
# -> driver_age
# -> search_type
# -> stop_outcome
# -> is_arrested