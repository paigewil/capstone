# This file contains all of my statistical analysis tests

library(ggplot2)
library(dplyr)
library(tidyr)
library(plyr)
library(corrplot)
library(gplots)
library(ggpubr)

# Read in file
df_clean <- read.csv("E:/Learning/Springboard Intro to Data Science/capstone/CT_cleaned_edit.csv")
df_split <- read.csv("E:/Learning/Springboard Intro to Data Science/capstone/CT_cleaned_split.csv")


#### is_arrested

#gender:
#-> chi-square test for independence
gender_arrest_chi <- table(df_clean$driver_gender, df_clean$is_arrested, useNA = "ifany")
chisq1 <- chisq.test(gender_arrest_chi)
chisq1

#-> 2-sample test to compare proportions
addmargins(table(df_clean$driver_gender, df_clean$is_arrested, useNA = "ifany"))
# FALSE   TRUE   <NA>    Sum
# F   103593   1697   1494 106784
# M   202408   5615   3862 211885
# Sum 306001   7312   5356 318669

#male arrests/stops = 5615/211885 = 0.02650022
#female arrests/stops = 1697/106784 = 0.01589189
# -> Is the observed proportion of arrests in males more than the observed proportion
# of arrests in females?
res4 <- prop.test(x = c(5615, 1697), n = c(211885, 106784), alternative = "greater")
res4

# -> plots
# png("./stats_images/4_gender_residuals.png")
# corrplot(chisq1$residuals, is.corr = FALSE)
# dev.off()
# 
# png("./stats_images/3_gender_contribution.png")
# corrplot(100*chisq1$residuals^2/chisq1$statistic, is.corr = FALSE)
# dev.off()



# race:
# -> chi-square test for independence
race_arrest_chi <- table(df_clean$driver_race_raw, df_clean$is_arrested, useNA = "ifany")
chisq2 <- chisq.test(race_arrest_chi)
chisq2

# -> 2-sample test to compare proportions
# -> Is the observed proportion of arrests in Hispanics more than the observed proportion
# of arrests in the other races?
race_table <- table(df_clean$driver_race_raw, df_clean$is_arrested, useNA = "ifany")
non_hispanic <- colSums(race_table[c(1,2,4,5),])
race_table2 <- rbind(race_table, non_hispanic)
race_table2 <- race_table2[c(3,6),]
race_table2 <- addmargins(race_table2)
race_table2 <- race_table2[c(1,2),]
res5 <- prop.test(x = as.vector(race_table2[,2]), n = as.vector(race_table2[,4]), alternative = "greater")
res5

# -> plots
png("./stats_images/1_race_residuals.png")
corrplot(chisq2$residuals, is.corr = FALSE)
dev.off()

png("./stats_images/2_race_contribution.png")
corrplot(100*chisq2$residuals^2/chisq2$statistic, is.corr = FALSE)
dev.off()



#race and gender:
# -> 2-sample test to compare proportions
# -> Do Native American women have a statistically significant larger proportion
# of arrests compared to the other female groups?
df_clean_female <- df_clean %>% filter(driver_gender == "F")
race_gender_arrest_p <- addmargins(table(df_clean_female$driver_race_raw, df_clean_female$is_arrested, useNA = "ifany"))
race_gender_table <- head(race_gender_arrest_p, -1)
non_na <- colSums(race_gender_table[c(1,2,3,5),])
race_gender_table2 <- rbind(race_gender_table, non_na)
race_gender_table2 <- race_gender_table2[c(4,6),]
res7 <- prop.test(x = as.vector(race_gender_table2[,2]), n = as.vector(race_gender_table2[,4]), alternative = "greater")
res7

# -> Do Native American women have a statistically significant larger proportion
# of arrests compared to their male counterparts?
df_clean_na <- df_clean %>% filter(driver_race_raw == "Native American")
na_arrests <- addmargins(table(df_clean_na$driver_gender, df_clean_na$is_arrested, useNA = "ifany"))
na_arrests <- head(na_arrests, -1)
res9 <- prop.test(x = as.vector(na_arrests[,2]), n = as.vector(na_arrests[,4]), alternative = "greater")
res9



#age:
age_arrest_chi <- table(df_clean$driver_age, df_clean$is_arrested, useNA = "ifany")
chisq3 <- chisq.test(age_arrest_chi)
chisq3
#warning message because expected values might be below threshold(i.e. <5)
age_table <- addmargins(table(df_clean$driver_age, df_clean$is_arrested, useNA = "ifany"))
res4 <- prop.test(x = as.vector(age_table[,2]), n = as.vector(age_table[,4]))
res4
#same issue as the chi-square test

# -> mean comparisons:
# -> mean age by gender
df_age <- df_clean %>% group_by(driver_gender) %>% dplyr::summarise(mean_age = mean(driver_age, na.rm = TRUE))
df_age
t.test(driver_age ~ driver_gender, data = df_clean, alternative = "less")

ggboxplot(df_clean, x = "driver_gender", y = "driver_age", color = "driver_gender",
          xlab = "Driver Gender", ylab = "Driver Age")

# -> mean age by race
df_age2 <- df_clean %>% group_by(driver_race_raw) %>% dplyr::summarise(mean_age = mean(driver_age, na.rm = TRUE))
df_age2
# -> Hispanic seems to have a low mean age.
df_clean$race <- vector(mode="character", length = nrow(df_clean))
df_clean$race[df_clean$driver_race_raw != "Hispanic"] <- "NonHispanic"
df_clean$race[df_clean$driver_race_raw == "Hispanic"] <- "Hispanic"
t.test(driver_age ~ race, data = df_clean, alternative = "less")
# -> Hispanic and Black
df_clean$race2 <- vector(mode="character", length = nrow(df_clean))
df_clean$race2[df_clean$driver_race_raw != "Hispanic" & df_clean$driver_race_raw != "Black"] <- "NonHB"
df_clean$race2[df_clean$driver_race_raw == "Hispanic" | df_clean$driver_race_raw == "Black" ] <- "HB"

#-> Of arrests only 
# -> mean age by gender
df_age <- df_clean %>% filter(is_arrested == TRUE) %>% group_by(driver_gender) %>% dplyr::summarise(mean_age = mean(driver_age, na.rm = TRUE))
df_age
t.test(driver_age ~ driver_gender, data = df_clean %>% filter(is_arrested == TRUE))

ggboxplot(df_clean %>% filter(is_arrested == TRUE), x = "driver_gender", y = "driver_age", color = "driver_gender",
          xlab = "Driver Gender", ylab = "Driver Age")
ggsave("./stats_images/7_gender_age_boxplot.png")


# -> mean age by race
df_age2 <- df_clean %>% filter(is_arrested == TRUE) %>% group_by(driver_race_raw) %>% dplyr::summarise(mean_age = mean(driver_age, na.rm = TRUE))
df_age2
# -> Hispanic seems to have a low mean age.
t.test(driver_age ~ race, data = df_clean %>% filter(is_arrested == TRUE), alternative = "less")

# -> Hispanic and Black seem to have low mean ages.
t.test(driver_age ~ race2, data = df_clean %>% filter(is_arrested == TRUE), alternative = "less")


ggboxplot(df_clean %>% filter(is_arrested == TRUE), x = "driver_race_raw", y = "driver_age", color = "driver_race_raw",
          xlab = "Driver Race", ylab = "Driver Age")
ggsave("./stats_images/6_race_age_boxplot.png")


#-> age group
#adding in age group column
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

# -> chi-square test for independence
agegrp_arrest_chi <- table(df_clean$AGEGRP, df_clean$is_arrested, useNA = "ifany")
chisq3 <- chisq.test(agegrp_arrest_chi)
chisq3

# -> 2-sample test to compare proportions
# -> Is the proportion of 25-29 significantly different than the other ages?
agegrp_table <- addmargins(table(df_clean$AGEGRP, df_clean$is_arrested, useNA = "ifany"))
agegrp1 <- agegrp_table[11,]
agegrp2 <- colSums(agegrp_table[c(1:10,12:14),])
agegrp_table2 <- rbind(agegrp1, agegrp2)
res3 <- prop.test(x = as.vector(agegrp_table2[,2]), n = as.vector(agegrp_table2[,4]), alternative = "greater")
res3

# -> Is the proportion of 75-79 significantly different than the other ages?
agegrp3 <- agegrp_table[8,]
agegrp4 <- colSums(agegrp_table[c(1:7,9:14),])
agegrp_table3 <- rbind(agegrp3, agegrp4)
res3_1 <- prop.test(x = as.vector(agegrp_table3[,2]), n = as.vector(agegrp_table3[,4]), alternative = "less")
res3_1



#county:
# -> chi-square test for independence
county_arrest_chi <- table(df_clean$county_name, df_clean$is_arrested, useNA = "ifany")
chisq6 <- chisq.test(county_arrest_chi[-1,])
# the blank county names have such a low number, we are ignoring them
chisq6

# -> 2-sample test to compare proportions
county_arrest_ft <- addmargins(table(df_clean$county_name, df_clean$is_arrested, useNA = "ifany"))
county_arrest_ft2 <- tail(head(county_arrest_ft, -1),-1)
# -> Is the arrest/stop proportion of New London significantly greater than
# the proportions of the other counties?
non_newlondon <- colSums(county_arrest_ft2[c(1,2,3,4,5,7,8),])
county_table <- rbind(county_arrest_ft2, non_newlondon)
county_table <- county_table[c(6,9),]
res11 <- prop.test(x = as.vector(county_table[,2]), n = as.vector(county_table[,4]), alternative = "greater")
res11

# -> Is the proportion of Asians arrests in Litchfield statistically significant?
df_litchfield <- df_clean %>% filter(county_name == "Litchfield County")
litchfield_race <- addmargins(table(df_litchfield$driver_race_raw, df_litchfield$is_arrested, useNA = "ifany"))
litchfield_race <- head(litchfield_race, -1)
non_asian2 <- colSums(litchfield_race[c(2,3,4,5),])
litchfield_race2 <- rbind(litchfield_race, non_asian2)
litchfield_race2 <- litchfield_race2[c(1,6),]
res13 <- prop.test(x = as.vector(litchfield_race2[,2]), n = as.vector(litchfield_race2[,4]), alternative = "greater")
res13
#warning



# month/year:
df_clean$stop_date <- as.POSIXct(df_clean$stop_date, "%Y-%m-%d", tz = "America/New_York")
mo_yr_label <- seq(as.Date("2013/10/1"), as.Date("2015/3/31"), by = "month")
mo_yr_label <- format(mo_yr_label, "%m-%Y")
df_clean$month_year <- format(df_clean$stop_date, "%m-%Y")
df_clean$month_year <- factor(df_clean$month_year,
                              ordered = TRUE,
                              levels = mo_yr_label)
# -> chi-square test for independence
mtyr_arrest_chi <- table(df_clean$month_year, df_clean$is_arrested, useNA = "ifany")
chisq7 <- chisq.test(mtyr_arrest_chi)
chisq7

# -> 2-sample test to compare proportions
# Is the proportion of arrests/stops in Winter statistically significant from
# the proportion in non-winter months?
winter_months <- addmargins(table(df_clean$month_year, df_clean$is_arrested, useNA = "ifany"))
winter_months <- head(winter_months, -1)
non_winter <- colSums(winter_months[c(1,2,6:14, 18),])
winter <- colSums(winter_months[c(3:5, 15:17),])
winter_months2 <- rbind(winter, non_winter)
res14 <- prop.test(x = as.vector(winter_months2[,2]), n = as.vector(winter_months2[,4]), alternative = "greater")
res14



#day of month:
a <- c("01", "02", "03", "04", "05", "06", "07", "08", "09")
b <- as.character(10:31)
days_label <- c(a, b)
df_clean$day_of_month <- format(df_clean$stop_date, "%d")
df_clean$day_of_month <- factor(df_clean$day_of_month,
                                ordered = TRUE,
                                levels = days_label)
# -> chi-square test for independence
dom_arrest_chi <- table(df_clean$day_of_month, df_clean$is_arrested, useNA = "ifany")
chisq8 <- chisq.test(dom_arrest_chi)
chisq8

# -> 2-sample test to compare proportions
# Is the proportion of arrests/stops from days 21-31 lower than the other days
# of the month (roughly the last 3rd)?
end_of_month <- addmargins(table(df_clean$day_of_month, df_clean$is_arrested, useNA = "ifany"))
end_of_month <- head(end_of_month, -1)
end <- colSums(end_of_month[c(21:31),])
beginning <- colSums(end_of_month[c(1:20),])
end_of_month2 <- rbind(end, beginning)
res15 <- prop.test(x = as.vector(end_of_month2[,2]), n = as.vector(end_of_month2[,4]), alternative = "less")
res15



#day of week:
df_clean$day_of_week <- weekdays(df_clean$stop_date)
df_clean$day_of_week <- factor(df_clean$day_of_week, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
# -> chi-square test for independence
dow_arrest_chi <- table(df_clean$day_of_week, df_clean$is_arrested, useNA = "ifany")
chisq9 <- chisq.test(dow_arrest_chi)
chisq9

# -> 2-sample test to compare proportions
# Is the greater proportion of arrests/stops on Saturday and Sunday statistically
# significant?
weekend <- addmargins(table(df_clean$day_of_week, df_clean$is_arrested, useNA = "ifany"))
weekend <- head(weekend, -1)
satsun <- colSums(weekend[c(1,7),])
weekday <- colSums(weekend[c(2:6),])
weekend2 <- rbind(satsun, weekday)
res17 <- prop.test(x = as.vector(weekend2[,2]), n = as.vector(weekend2[,4]), alternative = "greater")
res17



# hour:
# -> chi-square test for independence
hour_arrest_chi <- table(df_clean$stop_time_hour_numeric, df_clean$is_arrested, useNA = "ifany")
chisq10 <- chisq.test(hour_arrest_chi)
chisq10
# warning

# -> Is the greater proportion of arrests/stops between 0000- 0300 statistically
# significant?
times <- addmargins(table(df_clean$stop_time_hour_numeric, df_clean$is_arrested, useNA = "ifany"))
times <- head(times, -1)
early <- colSums(times[c(1:4),])
other <- colSums(times[c(5:25),])
times2 <- rbind(early, other)
res19 <- prop.test(x = as.vector(times2[,2]), n = as.vector(times2[,4]), alternative = "greater")
res19



# search conducted:
# -> chi-square test for independence
sc_arrest_chi <- table(df_clean$search_conducted, df_clean$is_arrested, useNA = "ifany")
chisq11 <- chisq.test(sc_arrest_chi)
chisq11

# -> 2-sample test to compare proportions
# Is search conducted == TRUE > search conducted == FALSE?
sc_arrest_ft <- addmargins(table(df_clean$search_conducted, df_clean$is_arrested, useNA = "ifany"))
sc_arrest_ft <- sc_arrest_ft[c(2,1),]
res19 <- prop.test(x = as.vector(sc_arrest_ft[,2]), n = as.vector(sc_arrest_ft[,4]), alternative = "greater")
res19



#search type:
df_search_conducted <- df_clean %>% filter(search_conducted == TRUE)
# -> chi-square test for independence
st_arrest_chi <- table(df_search_conducted$search_type_raw, df_search_conducted$is_arrested, useNA = "ifany")
chisq12 <- chisq.test(st_arrest_chi)
chisq12
#warning

# -> 2-sample test to compare proportions
st_arrest_ft <- addmargins(table(df_search_conducted$search_type_raw, df_search_conducted$is_arrested, useNA = "ifany"))
st_arrest_ft <- head(st_arrest_ft,-1)
# -> Is is statistically significant that inventory's arrest/stop proportion
# is greater than the other props?
inv <- st_arrest_ft[3,]
non_inv <- colSums(st_arrest_ft[c(1:2,4),])
inv_non_inv <- rbind(inv, non_inv)
res25 <- prop.test(x = as.vector(inv_non_inv[,2]), n = as.vector(inv_non_inv[,4]), alternative = "greater")
res25

# -> is it likely that if you have an inventory search that you will end with 
# an arrest?-- what is the confidence interval of values?
inv_true <- inv[[2]]
inv_other <- sum(inv[c(1,3)])
inv_n <- inv[[4]]
prop.test(inv_true, inv_n)



# contraband found:
# -> chi-square test for independence
cf_arrest_chi <- table(df_search_conducted$contraband_found, df_search_conducted$is_arrested, useNA = "ifany")
chisq14 <- chisq.test(cf_arrest_chi)
chisq14

# -> 2-sample test to compare proportions
# -> Is TRUE greater?
cf_arrest_ft <- addmargins(table(df_search_conducted$contraband_found, df_search_conducted$is_arrested, useNA = "ifany"))
cf_arrest_ft <- cf_arrest_ft[c(2,1),]
res27 <- prop.test(x = as.vector(cf_arrest_ft[,2]), n = as.vector(cf_arrest_ft[,4]), alternative = "greater")
res27



# stop duration:
# -> chi-square test for independence
sd_arrest_chi <- table(df_clean$stop_duration, df_clean$is_arrested, useNA = "ifany")
chisq16 <- chisq.test(sd_arrest_chi)
chisq16

# -> Is the difference between the arrest/stop proportion of 30+ stop durations
# and the other durations statistically significant?
sd_arrest_chi <- addmargins(table(df_clean$stop_duration, df_clean$is_arrested, useNA = "ifany"))
sd_30 <- sd_arrest_chi[3,]
sd_not_30 <- colSums(sd_arrest_chi[c(1:2),])
sd_30_not_30 <- rbind(sd_30, sd_not_30)
res30 <- prop.test(x = as.vector(sd_30_not_30[,2]), n = as.vector(sd_30_not_30[,4]), alternative = "greater")
res30



# violation count:
# -> chi-square test for independence
vc_arrest_chi <- table(df_clean$violation_count, df_clean$is_arrested, useNA = "ifany")
chisq17 <- chisq.test(vc_arrest_chi)
chisq17
# Warning message:
#   In chisq.test(vc_arrest_chi) : Chi-squared approximation may be incorrect



# violation
df_split3 <- df_split %>% gather("violation_new", "n", 'violation_raw_Cell.Phone':'violation_raw_Window.Tint')
df_split3 <- df_split3 %>% filter(n != 0)
# -> chi-square test for independence
v_arrest_chi <- table(df_split3$violation_new, df_split3$is_arrested, useNA = "ifany")
chisq18 <- chisq.test(vc_arrest_chi)
chisq18
# Warning message:
#   In chisq.test(vc_arrest_chi) : Chi-squared approximation may be incorrect

# -> Is the difference in arrest/stop proportion for suspended licenses signficantly
# different?
v_arrest_chi <- addmargins(table(df_split3$violation_new, df_split3$is_arrested, useNA = "ifany"))
susp <- v_arrest_chi[12,]
not_susp <- colSums(v_arrest_chi[c(1:11, 13:14),])
susp_not_susp <- rbind(susp, not_susp)
res31 <- prop.test(x = as.vector(susp_not_susp[,2]), n = as.vector(susp_not_susp[,4]), alternative = "greater")
res31




####stop_outcome

#day of week:
so_dow_chi <- addmargins(table(df_clean$day_of_week, df_clean$stop_outcome,  useNA = "ifany"))
other_ticket <- colSums(so_dow_chi[c(1:6),])
s_ticket <- so_dow_chi[7,]
ticket_dow_combined <- rbind(s_ticket, other_ticket)
prop.test(x = as.vector(ticket_dow_combined [,3]), n = as.vector(ticket_dow_combined[,7]), alternative = "less")

other_written <- colSums(so_dow_chi[c(2:7),])
s_written <- so_dow_chi[1,]
written_dow_combined <- rbind(s_written, other_written)
prop.test(x = as.vector(written_dow_combined [,5]), n = as.vector(written_dow_combined[,7]), alternative = "less")

# # -> chi-square test for independence
# so_dow <- table(df_clean$day_of_week, df_clean$stop_outcome,  useNA = "ifany")
# chisq.test(so_dow)

# # -> heatmap plot
# heatmaptab <- setNames(data.frame(table(df_clean$day_of_week, df_clean$stop_outcome, exclude = NULL)), c("day_of_week", "stop_outcome", "count"))
# tot2 <- sum(heatmaptab$count)
# heatmaptab<- heatmaptab %>% mutate(prop = count/tot2)
# ggplot(heatmaptab, aes(x=day_of_week, y = stop_outcome)) +
#   geom_tile(aes(fill = prop)) +
#   scale_fill_gradient(low = "#56B1F7", high = "#132B43")
# #ggsave("./stats_images/5_race_gender_arrest_prop.png")



#month:
df_clean$stop_month <- month(df_clean$stop_date)
df_clean$stop_month <- factor(df_clean$stop_month, 
                              ordered = TRUE,
                              levels = as.character(1:12))
so_mo_p <- addmargins(table(df_clean$stop_month, df_clean$stop_outcome,  useNA = "ifany"))
winter_mo <- colSums(so_mo_p[c(1:2, 12),])
non_winter_mo <- colSums(so_mo_p[c(3:11),])
mo_combined <- rbind(winter_mo, non_winter_mo)
prop.test(x = as.vector(mo_combined[,3]), n = as.vector(mo_combined[,7]), alternative = "less")
prop.test(x = as.vector(mo_combined[,4]), n = as.vector(mo_combined[,7]), alternative = "greater")
prop.test(x = as.vector(mo_combined[,5]), n = as.vector(mo_combined[,7]), alternative = "greater")
prop.test(x = as.vector(mo_combined[,1]), n = as.vector(mo_combined[,7]), alternative = "greater")
prop.test(x = as.vector(mo_combined[,2]), n = as.vector(mo_combined[,7]), alternative = "greater")



#time:
ticket_hr_chi <- addmargins(table(df_clean$stop_time_hour_numeric, df_clean$stop_outcome,  useNA = "ifany"))
more_tickets <- colSums(ticket_hr_chi[c(7:20),])
less_tickets <- colSums(ticket_hr_chi[c(1:6, 21:25),])
ticket_hr_combined <- rbind(more_tickets, less_tickets)
prop.test(x = as.vector(ticket_hr_combined [,3]), n = as.vector(ticket_hr_combined [,7]), alternative = "greater")

less_verbal <- colSums(ticket_hr_chi[c(7:20, 25),])
more_verbal <- colSums(ticket_hr_chi[c(1:6, 21:24),])
verbal_hr_combined <- rbind(more_verbal, less_verbal)
prop.test(x = as.vector(verbal_hr_combined [,4]), n = as.vector(verbal_hr_combined [,7]), alternative = "greater")



#county:
ticket_county <- addmargins(table(df_clean$county_name, df_clean$stop_outcome,  useNA = "ifany"))
litchfield_ticket <- ticket_county[4,]
non_litch_ticket <- colSums(ticket_county[c(1:3, 5:9),])
ticket_county_combined <- rbind(litchfield_ticket, non_litch_ticket)
prop.test(x = as.vector(ticket_county_combined [,3]), n = as.vector(ticket_county_combined [,7]), alternative = "less")
prop.test(x = as.vector(ticket_county_combined [,5]), n = as.vector(ticket_county_combined [,7]), alternative = "greater")
prop.test(x = as.vector(ticket_county_combined [,4]), n = as.vector(ticket_county_combined [,7]), alternative = "greater")

# -> chi-square test for independence
county_so_chi <- table(df_clean$county_name, df_clean$stop_outcome, useNA = "ifany")
chisq.test(county_so_chi[c(2:9),])




#gender:
so_gender <- addmargins(table(df_clean$driver_gender, df_clean$stop_outcome,  useNA = "ifany"))
so_gender <- head(so_gender,-1)
prop.test(x = as.vector(so_gender[,2]), n = as.vector(so_gender[,7]), alternative = "less")
prop.test(x = as.vector(so_gender[,3]), n = as.vector(so_gender[,7]), alternative = "greater")
prop.test(x = as.vector(so_gender[,5]), n = as.vector(so_gender[,7]), alternative = "greater")



#race:
so_race <- addmargins(table(df_clean$driver_race_raw, df_clean$stop_outcome,  useNA = "ifany"))
so_nonasian <- colSums(so_race[c(2:5),])
so_asian <- so_race[1,]
so_asian_combined <- rbind(so_asian, so_nonasian)
prop.test(x = as.vector(so_asian_combined[,3]), n = as.vector(so_asian_combined[,7]), alternative = "greater")

so_hb <- colSums(so_race[c(2:3),])
so_nonhb <- colSums(so_race[c(1,4:5),])
so_hb_combined <- rbind(so_hb, so_nonhb)
prop.test(x = as.vector(so_hb_combined[,2]), n = as.vector(so_hb_combined[,7]), alternative = "greater")
prop.test(x = as.vector(so_hb_combined[,1]), n = as.vector(so_hb_combined[,7]), alternative = "greater")


so_nonnaw <- colSums(so_race[c(1:3),])
so_naw <- colSums(so_race[c(4:5),])
so_naw_combined <- rbind(so_naw, so_nonnaw)
prop.test(x = as.vector(so_naw_combined[,4]), n = as.vector(so_naw_combined[,7]), alternative = "greater")
prop.test(x = as.vector(so_naw_combined[,5]), n = as.vector(so_naw_combined[,7]), alternative = "greater")


so_nonwhite <- colSums(so_race[c(1:4),])
so_white <- so_race[5,]
so_white_combined <- rbind(so_white, so_nonwhite)
prop.test(x = as.vector(so_white_combined[,5]), n = as.vector(so_white_combined[,7]), alternative = "greater")



#search conducted
# -> chi-square test for independence
sc_so_chi <- table(df_clean$search_conducted, df_clean$stop_outcome, useNA = "ifany")
chisq.test(sc_so_chi)

so_sc <- addmargins(table(df_clean$search_conducted, df_clean$stop_outcome,  useNA = "ifany"))
so_sctrue <- so_sc[2,]
so_scfalse <- so_sc[1,]
so_sc_combined <- rbind(so_sctrue, so_scfalse)
prop.test(x = as.vector(so_sc_combined[,2]), n = as.vector(so_sc_combined[,7]), alternative = "greater")

prop.test(x = as.vector(so_sc_combined[,3]), n = as.vector(so_sc_combined[,7]), alternative = "less")

prop.test(x = as.vector(so_sc_combined[,4]), n = as.vector(so_sc_combined[,7]), alternative = "less")

prop.test(x = as.vector(so_sc_combined[,5]), n = as.vector(so_sc_combined[,7]), alternative = "less")



#contraband found
so_cf <- addmargins(table(df_search_conducted$contraband_found, df_search_conducted$stop_outcome,  useNA = "ifany"))
so_cftrue <- so_cf[2,]
so_cffalse <- so_cf[1,]
so_cf_combined <- rbind(so_cftrue, so_cffalse)
prop.test(x = as.vector(so_cf_combined[,3]), n = as.vector(so_cf_combined[,7]), alternative = "greater")
prop.test(x = as.vector(so_cf_combined[,2]), n = as.vector(so_cf_combined[,7]), alternative = "less")
prop.test(x = as.vector(so_cf_combined[,4]), n = as.vector(so_cf_combined[,7]), alternative = "less")
prop.test(x = as.vector(so_cf_combined[,5]), n = as.vector(so_cf_combined[,7]), alternative = "less")

df_search_conducted$Stop_Outcome2 <- as.character(df_search_conducted$stop_outcome)
df_search_conducted$Stop_Outcome2[df_search_conducted$Stop_Outcome %in% c("Arrest", "Summons")] <- "Worse"
df_search_conducted$Stop_Outcome2[df_search_conducted$Stop_Outcome %in% c("Verbal Warning", "Written Warning")] <- "Best"
so_cf2 <- addmargins(table(df_search_conducted$contraband_found, df_search_conducted$Stop_Outcome2,  useNA = "ifany"))
so_cf2 <- head(so_cf2, -1)
prop.test(x = as.vector(so_cf2[,3]), n = as.vector(so_cf2[,5]))



#search type
so_st <- addmargins(table(df_search_conducted$search_type, df_search_conducted$stop_outcome,  useNA = "ifany"))
so_consent <- so_st[1,]
so_nonconsent <- colSums(so_st[c(2:4),])
so_consent_combined <- rbind(so_consent, so_nonconsent)
prop.test(x = as.vector(so_consent_combined[,2]), n = as.vector(so_consent_combined[,7]), alternative = "greater")
prop.test(x = as.vector(so_consent_combined[,4]), n = as.vector(so_consent_combined[,7]), alternative = "greater")
prop.test(x = as.vector(so_consent_combined[,5]), n = as.vector(so_consent_combined[,7]), alternative = "greater")

so_inv <- so_st[3,]
so_noninv <- colSums(so_st[c(1:2, 4),])
so_inv_combined <- rbind(so_inv, so_noninv)
prop.test(x = as.vector(so_inv_combined[,3]), n = as.vector(so_inv_combined[,7]), alternative = "greater")




#stop duration
so_sd <- addmargins(table(df_clean$stop_duration, df_clean$stop_outcome,  useNA = "ifany"))
so_1_15 <- so_sd[1,]
so_non1_15 <- colSums(so_sd[c(2:3),])
so_1_15_combined <- rbind(so_1_15, so_non1_15)
prop.test(x = as.vector(so_1_15_combined[,3]), n = as.vector(so_1_15_combined[,7]), alternative = "greater")
prop.test(x = as.vector(so_1_15_combined[,4]), n = as.vector(so_1_15_combined[,7]), alternative = "greater")
prop.test(x = as.vector(so_1_15_combined[,5]), n = as.vector(so_1_15_combined[,7]), alternative = "greater")

so_16_30 <- so_sd[2,]
so_non16_30 <- colSums(so_sd[c(1,3),])
so_16_30_combined <- rbind(so_16_30, so_non16_30)
prop.test(x = as.vector(so_16_30_combined[,2]), n = as.vector(so_16_30_combined[,7]), alternative = "greater")




#violation count:
so_vc <- addmargins(table(df_clean$violation_count, df_clean$stop_outcome,  useNA = "ifany"))
so_4_5 <- colSums(so_vc[c(4:5),])
so_non4_5 <- colSums(so_vc[c(1:3),])
so_4_5_combined <- rbind(so_4_5, so_non4_5)
prop.test(x = as.vector(so_4_5_combined[,2]), n = as.vector(so_4_5_combined[,7]), alternative = "greater")

so_1 <- so_vc[1,]
so_non1 <- colSums(so_vc[c(2:5),])
so_1_combined <- rbind(so_1, so_non1)
prop.test(x = as.vector(so_1_combined[,3]), n = as.vector(so_1_combined[,7]), alternative = "greater")
prop.test(x = as.vector(so_1_combined[,4]), n = as.vector(so_1_combined[,7]), alternative = "greater")
prop.test(x = as.vector(so_1_combined[,5]), n = as.vector(so_1_combined[,7]), alternative = "greater")


df_clean$violation_count2 <- as.character(df_clean$violation_count)
df_clean$violation_count2[df_clean$violation_count > 1] <- "> 1"
so_vc2 <- addmargins(table(df_clean$violation_count2, df_clean$stop_outcome,  useNA = "ifany"))
so_vc2 <- head(so_vc2,-1)
prop.test(x = as.vector(so_vc2[,1]), n = as.vector(so_vc2[,7]), alternative = "greater")
prop.test(x = as.vector(so_vc2[,2]), n = as.vector(so_vc2[,7]), alternative = "greater")




#violation:
v_so <- addmargins(table(df_split3$violation_new, df_split3$stop_outcome, useNA = "ifany"))
eqv <- v_so[4,]
not_eqv <- colSums(v_so[c(1:3, 5:14),])
eqv_combined <- rbind(eqv, not_eqv)
prop.test(x = as.vector(eqv_combined[,4]), n = as.vector(eqv_combined[,7]), alternative = "greater")




#age:
so_age <- addmargins(table(df_clean$driver_age, df_clean$stop_outcome,  useNA = "ifany"))
so_1617 <- colSums(so_age[c(2:3),])
so_non1617 <- colSums(so_age[c(1,4:6),])
so_1617_combined <- rbind(so_1617, so_non1617)
prop.test(x = as.vector(so_1617_combined[,5]), n = as.vector(so_1617_combined[,7]), alternative = "greater")


# so_non45 <- colSums(so_age[c(1, 4:66),])
# so_45 <- colSums(so_age[c(2:3),])
# so_45_combined <- rbind(so_45, so_non45)
