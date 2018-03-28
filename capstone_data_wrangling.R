library(dplyr)
library(tidyr)
library(stringr)

# Read in file
df <- read.csv("E:/Learning/Springboard Intro to Data Science/capstone/CT_cleaned.csv")

df_clean <- df
# CHECK FOR MISSING VALUES IN COLUMNS
# CLEAN UP

#to investigate:
# stop_date
# stop_time- right format?


#location_raw
df %>% arrange(location_raw) %>% select(location_raw) %>% distinct()
# -> e granby and east granby

#country_name
df %>% arrange(county_name) %>% select(county_name) %>% distinct()
# -> some blanks worth investigating/cleaning
cn_blanks <- df %>% filter(county_name == "") %>%  select(location_raw, county_fips, fine_grained_location)
# main st:
head(df %>% filter(grepl(pattern = "*main st*", x = tolower(fine_grained_location))) %>% select(fine_grained_location, county_name))
# -> no distinct county for "main st"
# rocky neck:
rocky_neck <- df %>% filter(grepl(pattern = "*rocky neck*", x = tolower(fine_grained_location))) %>% select(fine_grained_location, county_name)
rocky_neck %>% select(county_name) %>% distinct()
# -> distinct county is New London County
# -> Google search indicates it's a state park in New London
df %>% filter(county_name == "New London County") %>% select(county_fips) %>% distinct()
# -> 9011
# update county_name and county_fips for "rocky neck" matches
df_clean <- df %>% mutate(county_name = replace(county_name, county_name == "" & grepl(pattern = "*rocky neck*", x = tolower(fine_grained_location)), "New London County"),
                          county_fips = replace(county_fips, county_name == "New London County" & is.na(county_fips), 9011))
# verify worked
rocky_neck2 <- df_clean %>% filter(grepl(pattern = "*rocky neck*", x = tolower(fine_grained_location))) %>% select(fine_grained_location, county_name, county_fips)
rocky_neck2 %>% select(county_name, county_fips) %>% distinct()
# -> no other groups (patterns that appear more than once) to investigate

#county_fips
df_clean %>% arrange(county_fips) %>% select(county_fips) %>% distinct()
df_clean %>% filter(is.na(county_fips)) %>%  select(location_raw, county_fips, fine_grained_location, county_name)
# -> no useful information to fill in the blanks

#police_department
df_clean %>% arrange(police_department) %>% select(police_department) %>% distinct()
# -> one value = State Police

#driver_gender
df_clean %>% select(driver_gender) %>% distinct()
# -> all either M or F

#driver_age_raw
df_clean %>% arrange(driver_age_raw) %>% select(driver_age_raw) %>% distinct()

#driver_age
df_clean %>% arrange(driver_age) %>% select(driver_age) %>% distinct()
# -> made < 15 blanks -- leave as is
# make blanks NAs for easier use with is.na()
df_clean <- df_clean %>% mutate(driver_age = replace(driver_age, driver_age == "", NA))

#driver_race_raw
df_clean %>% arrange(driver_race_raw) %>% select(driver_race_raw) %>% distinct()

#driver_race
df_clean %>% arrange(driver_race) %>% select(driver_race) %>% distinct()
# -> use driver_race_raw since driver_race buckets into "other" which removes details

#violation 
df_clean %>% arrange(violation) %>% select(violation) %>% distinct() %>% count()
# -> 125
df_clean %>% arrange(violation_raw) %>% select(violation_raw) %>% distinct() %>% count()
# -> 202
dist_viol <- df_clean %>% arrange(violation) %>% select(violation) %>% distinct()
dist_viol_raw <- df_clean %>% arrange(violation_raw) %>% select(violation_raw) %>% distinct()
dist_viol <- as.vector(dist_viol[[1]])
dist_viol_raw <- as.vector(dist_viol_raw[[1]])
subset(dist_viol_raw, !(tolower(dist_viol_raw) %in% tolower(dist_viol)))
# -> 195 elements returned, i.e. not many similar violation namings
# -> use raw since has more groupings, so more detail
# separate violations into separate columns
# first: determine max violations for a stop
no_viols <- str_count(dist_viol_raw, ",")
max(no_viols) + 1
# 5 -> number of new columns to create
# second: create # of violations column & copy fo violation_raw column to use in separate()
df_clean <- df_clean %>% mutate(violation_raw2 = violation_raw,
                                violation_count = str_count(violation_raw, ",") + 1)
df_clean <- separate(df_clean, violation_raw2, c("violation_raw1", "violation_raw2", "violation_raw3", "violation_raw4", "violation_raw5"), sep = ",")

#search_conducted
df_clean %>% select(search_conducted) %>% distinct()
# -> TRUE or FALSE, no missing values

#search_type & search_type_raw
df_clean %>% arrange(search_type_raw) %>% select(search_type_raw) %>% distinct()
df_clean %>% arrange(search_type) %>% select(search_type) %>% distinct()
# -> no difference between search_type and search_type_raw
# change blanks to NAs
df_clean <- df_clean %>% mutate(search_type = replace(search_type, search_type == "", NA))

#contraband_found
df_clean %>% select(contraband_found) %>% distinct()
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

#officer_id
df_clean %>% arrange(officer_id) %>% select(officer_id) %>% distinct()
class(df_clean$officer_id)
# -> Factor
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

#write df_clean to file
write.csv(file = "E:/Learning/Springboard Intro to Data Science/capstone/CT_cleaned_edit.csv", x=df_clean, row.names = FALSE)

