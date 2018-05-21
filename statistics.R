library(ggplot2)
library(dplyr)
library(tidyr)
#library(lubridate)
library(plyr)
#library(ggpubr)
#library(gridExtra)
library(corrplot)
library(gplots)

# Read in file
df_clean <- read.csv("E:/Learning/Springboard Intro to Data Science/capstone/CT_cleaned_edit.csv")
df_split <- read.csv("E:/Learning/Springboard Intro to Data Science/capstone/CT_cleaned_split.csv")

#Dataframe only of arrests
df_arrests <- df_clean %>% filter(is_arrested == TRUE)



#gender:
#-> chi-square test for independence
gender_arrest_chi <- table(df_clean$driver_gender, df_clean$is_arrested, useNA = "ifany")
chisq1 <- chisq.test(gender_arrest_chi)
chisq1
# Pearson's Chi-squared test
# 
# data:  gender_arrest_con
# X-squared = 440.32, df = 2, p-value < 2.2e-16

#-> 2-sample test to compare proportions
#frequency table with margin sums
addmargins(table(df_clean$driver_gender, df_clean$is_arrested, useNA = "ifany"))
# FALSE   TRUE   <NA>    Sum
# F   103593   1697   1494 106784
# M   202408   5615   3862 211885
# Sum 306001   7312   5356 318669

#male arrests/stops = 5615/211885 = 0.02650022
#female arrests/stops = 1697/106784 = 0.01589189
# is the difference in these proportions statistically significant?
res1 <- prop.test(x = c(5615, 1697), n = c(211885, 106784))
res1
# 2-sample test for equality of proportions with continuity correction
# 
# data:  c(5615, 1697) out of c(211885, 106784)
# X-squared = 355.93, df = 1, p-value < 2.2e-16
# alternative hypothesis: two.sided
# 95 percent confidence interval:
#   0.009586238 0.011630422
# sample estimates:
#   prop 1     prop 2 
# 0.02650022 0.01589189 

# -> Is the observed proportion of arrests in males more than the observed proportion
# of arrests in females?
res4 <- prop.test(x = c(5615, 1697), n = c(211885, 106784), alternative = "greater")
res4
# 2-sample test for equality of proportions with continuity correction
# 
# data:  c(5615, 1697) out of c(211885, 106784)
# X-squared = 355.93, df = 1, p-value < 2.2e-16
# alternative hypothesis: greater
# 95 percent confidence interval:
#   0.009749431 1.000000000
# sample estimates:
#   prop 1     prop 2 
# 0.02650022 0.01589189 

# -> plots
png("./stats_images/4_gender_residuals.png")
corrplot(chisq1$residuals, is.corr = FALSE)
dev.off()

png("./stats_images/3_gender_contribution.png")
corrplot(100*chisq1$residuals^2/chisq1$statistic, is.corr = FALSE)
dev.off()



# race:
# -> chi-square test for independence
race_arrest_chi <- table(df_clean$driver_race_raw, df_clean$is_arrested, useNA = "ifany")
chisq2 <- chisq.test(race_arrest_chi)
chisq2
# Pearson's Chi-squared test
# 
# data:  race_arrest_chi
# X-squared = 507.62, df = 8, p-value < 2.2e-16

# -> 2-sample test to compare proportions
race_arrest_ft <- addmargins(table(df_clean$driver_race_raw, df_clean$is_arrested, useNA = "ifany"))
race_arrest_ft2 <- head(race_arrest_ft, -1)
res2 <- prop.test(x = as.vector(race_arrest_ft2[,2]), n = as.vector(race_arrest_ft2[,4]))
res2
# 5-sample test for equality of proportions without continuity correction
# 
# data:  as.vector(race_arrest_ft2[, 2]) out of as.vector(race_arrest_ft2[, 4])
# X-squared = 442.07, df = 4, p-value < 2.2e-16
# alternative hypothesis: two.sided
# sample estimates:
#   prop 1     prop 2     prop 3     prop 4     prop 5 
# 0.01344538 0.02455756 0.03912443 0.01485714 0.02090786  

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
# 2-sample test for equality of proportions with continuity correction
# 
# data:  as.vector(race_table2[, 2]) out of as.vector(race_table2[, 4])
# X-squared = 402.41, df = 1, p-value < 2.2e-16
# alternative hypothesis: greater
# 95 percent confidence interval:
#   0.01605447 1.00000000
# sample estimates:
#   prop 1     prop 2 
# 0.03912443 0.02119216 

# -> Alternatively, is the observed proportion of arrests for Asians more than the 
# observed proportion of arrests in the other races?
non_asian <- colSums(race_table[c(2,3,4,5),])
race_table3 <- rbind(race_table, non_asian)
race_table3 <- race_table3[c(1,6),]
race_table3 <- addmargins(race_table3)
race_table3 <- race_table3[c(1,2),]
res6 <- prop.test(x = as.vector(race_table3[,2]), n = as.vector(race_table2[,4]), alternative = "less")
res6
# 2-sample test for equality of proportions with continuity correction
# 
# data:  as.vector(race_table3[, 2]) out of as.vector(race_table2[, 4])
# X-squared = 638.64, df = 1, p-value < 2.2e-16
# alternative hypothesis: less
# 95 percent confidence interval:
#   -1.00000000 -0.02189515
# sample estimates:
#   prop 1      prop 2 
# 0.002567641 0.025153733 

# -> plots
# jpeg("./stats_images/1_race_residuals.jpeg")
# corrplot(chisq2$residuals, is.corr = FALSE)
# dev.off()
# corrplot(chisq2$residuals, is.corr = FALSE)
# dev.print(pdf, "./stats_images/1_race_residuals.jpeg")
png("./stats_images/1_race_residuals.png")
corrplot(chisq2$residuals, is.corr = FALSE)
dev.off()

png("./stats_images/2_race_contribution.png")
corrplot(100*chisq2$residuals^2/chisq2$statistic, is.corr = FALSE)
dev.off()



#race and gender:
# -> chi-square test for independence
# race_gender_arrest_chi <- table(df_clean$driver_race_raw, df_clean$driver_gender, df_clean$is_arrested, useNA = "ifany")
# chisq <- chisq.test(race_gender_arrest_chi)
# chisq
# gender_race_by_arrest3 <- setNames(data.frame(table(df_clean$driver_gender, df_clean$driver_race_raw, df_clean$is_arrested, exclude = NULL)), c("Gender", "Race", "Arrested", "count")) 
# gender_race_by_arrest3 <- gender_race_by_arrest %>% mutate(gender_race = paste(Race, Gender))
# race_totals4 <- data.frame(gender_race_by_arrest3 %>% group_by(gender_race) %>% dplyr::summarise(sum_race_gender = sum(count)))
# gender_race_by_arrest3 <- left_join(gender_race_by_arrest3, race_totals4, by = "gender_race")
# gender_race_by_arrest3 <- gender_race_by_arrest3 %>% mutate(percent_of_race_gender  = count/sum_race_gender)
# 
#race_gender_arrest_chi2 <- gender_race_by_arrest3 %>% select(c(5, 3, 4)) %>% spread(Arrested, count)
# arrest_prop <- as.vector(gender_race_by_arrest3[,7])
# gender_race_heatmap <- matrix(arrest_prop, ncol = 5, byrow = TRUE)
# colnames(gender_race_heatmap) <- as.vector(gender_race_by_arrest3[,2])

df_clean <- df_clean %>% mutate(gender_race = paste(driver_race_raw, driver_gender))
race_gender_arrest_chi <- table(df_clean$gender_race, df_clean$is_arrested, useNA = "ifany")
chisq5 <- chisq.test(race_gender_arrest_chi)
chisq5
# Pearson's Chi-squared test
# 
# data:  race_gender_arrest_chi
# X-squared = 1038.5, df = 18, p-value < 2.2e-16

# -> Do Native American women have a statistically significant larger proportion
# of arrests compared to the other female groups?
# df_clean_female <- df_clean %>% filter(driver_gender == "F")
# race_gender_arrest_p <- addmargins(table(df_clean_female$gender_race, df_clean_female$is_arrested, useNA = "ifany"))
# race_gender_table <- head(race_gender_arrest_p, -1)
# res7 <- prop.test(x = as.vector(race_gender_table[,2]), n = as.vector(race_gender_table[,4]), alternative = "greater")
# res7
df_clean_female <- df_clean %>% filter(driver_gender == "F")
race_gender_arrest_p <- addmargins(table(df_clean_female$gender_race, df_clean_female$is_arrested, useNA = "ifany"))
race_gender_table <- head(race_gender_arrest_p, -1)
non_na <- colSums(race_gender_table[c(1,2,3,5),])
race_gender_table2 <- rbind(race_gender_table, non_na)
race_gender_table2 <- race_gender_table2[c(4,6),]
res7 <- prop.test(x = as.vector(race_gender_table2[,2]), n = as.vector(race_gender_table2[,4]), alternative = "greater")
res7
# 2-sample test for equality of proportions with continuity correction
# 
# data:  as.vector(race_gender_table2[, 2]) out of as.vector(race_gender_table2[, 4])
# X-squared = 0.38745, df = 1, p-value = 0.2668
# alternative hypothesis: greater
# 95 percent confidence interval:
#   -0.008208883  1.000000000
# sample estimates:
#   prop 1     prop 2 
# 0.02122016 0.01587302 

# -> Do Native American women have a statistically significant larger proportion
# of arrests compared to their male counterparts?
df_clean_na <- df_clean %>% filter(driver_race_raw == "Native American")
na_arrests <- addmargins(table(df_clean_na$gender_race, df_clean_na$is_arrested, useNA = "ifany"))
na_arrests <- head(na_arrests, -1)
res9 <- prop.test(x = as.vector(na_arrests[,2]), n = as.vector(na_arrests[,4]), alternative = "greater")
res9
# 2-sample test for equality of proportions with continuity correction
# 
# data:  as.vector(na_arrests[, 2]) out of as.vector(na_arrests[, 4])
# X-squared = 0.83287, df = 1, p-value = 0.1807
# alternative hypothesis: greater
# 95 percent confidence interval:
#   -0.006791985  1.000000000
# sample estimates:
#   prop 1     prop 2 
# 0.02122016 0.01310998 

# -> Do Hispanic males have a statistically significant larger proportion
# of arrests than all other gender/race groups?
race_gender_arrest_p2 <- addmargins(table(df_clean$gender_race, df_clean$is_arrested, useNA = "ifany"))
race_gender_table3 <- head(race_gender_arrest_p2, -1)
non_hispm <- colSums(race_gender_table3[c(1,2,3,4,5,7,8,9,10),])
race_gender_table3 <- rbind(race_gender_table3, non_hispm)
race_gender_table3 <- race_gender_table3[c(6,11),]
res8 <- prop.test(x = as.vector(race_gender_table3[,2]), n = as.vector(race_gender_table3[,4]), alternative = "greater")
res8
# 2-sample test for equality of proportions with continuity correction
# 
# data:  as.vector(race_gender_table3[, 2]) out of as.vector(race_gender_table3[, 4])
# X-squared = 566.4, df = 1, p-value < 2.2e-16
# alternative hypothesis: greater
# 95 percent confidence interval:
#   0.02207378 1.00000000
# sample estimates:
#   prop 1     prop 2 
# 0.04558083 0.02118167

# -> heatmap plot
# race_gender_arrest_heatmap <- gender_race_by_arrest3 %>% filter(Arrested == TRUE) %>% select(c(1,2,7)) %>% spread(Race, percent_of_race_gender)
# race_gender_arrest_heatmap2 <- data.frame(race_gender_arrest_heatmap, row.names = race_gender_arrest_heatmap$Gender)
# race_gender_arrest_heatmap2 <- race_gender_arrest_heatmap2[,-1]
gender_race_by_arrest3 <- setNames(data.frame(table(df_clean$driver_gender, df_clean$driver_race_raw, df_clean$is_arrested, exclude = NULL)), c("Gender", "Race", "Arrested", "count"))
gender_race_by_arrest3 <- gender_race_by_arrest %>% mutate(gender_race = paste(Race, Gender))
race_totals4 <- data.frame(gender_race_by_arrest3 %>% group_by(gender_race) %>% dplyr::summarise(sum_race_gender = sum(count)))
gender_race_by_arrest3 <- left_join(gender_race_by_arrest3, race_totals4, by = "gender_race")
gender_race_by_arrest3 <- gender_race_by_arrest3 %>% mutate(percent_of_race_gender  = count/sum_race_gender)
ggplot(gender_race_by_arrest3 %>%  filter(Arrested == TRUE), aes(x=Race, y = Gender)) +
  geom_tile(aes(fill = percent_of_race_gender)) +
  scale_fill_gradient(low = "#56B1F7", high = "#132B43")
ggsave("./stats_images/5_race_gender_arrest_prop.png")



#age:
age_arrest_chi <- table(df_clean$driver_age, df_clean$is_arrested, useNA = "ifany")
chisq3 <- chisq.test(age_arrest_chi)
chisq3
#warning message because expected values might be below threshold(i.e. <5)
# for much smaller counts, use Fisher's exact test instead:
fisher <- fisher.test(age_arrest_chi)
# some counts are too large for fisher

age_table <- addmargins(table(df_clean$driver_age, df_clean$is_arrested, useNA = "ifany"))
res4 <- prop.test(x = as.vector(age_table[,2]), n = as.vector(age_table[,4]))
res4
#same issue as the chi-square test

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
# Pearson's Chi-squared test
# 
# data:  age2_arrest_chi
# X-squared = 1654, df = 26, p-value < 2.2e-16

# -> 2-sample test to compare proportions
agegrp_table <- addmargins(table(df_clean$AGEGRP, df_clean$is_arrested, useNA = "ifany"))
res3 <- prop.test(x = as.vector(agegrp_table[,2]), n = as.vector(agegrp_table[,4]))
res3
# 15-sample test for equality of proportions without continuity correction
# 
# data:  as.vector(age_table[, 2]) out of as.vector(age_table[, 4])
# X-squared = 210.69, df = 14, p-value < 2.2e-16
# alternative hypothesis: two.sided
# sample estimates:
#   prop 1     prop 2     prop 3     prop 4     prop 5     prop 6     prop 7     prop 8 
# 0.02411765 0.02039367 0.02169690 0.01692490 0.01610208 0.01506342 0.01250625 0.01160950 
# prop 9    prop 10    prop 11    prop 12    prop 13    prop 14    prop 15 
# 0.02165316 0.02564103 0.02757701 0.02660653 0.02316654 0.02155540 0.02294544 



#county:
# -> chi-square test for independence
county_arrest_chi <- table(df_clean$county_name, df_clean$is_arrested, useNA = "ifany")
chisq6 <- chisq.test(county_arrest_chi[-1,])
# the blank county names have such a low number, we are ignoring them
chisq6
# Pearson's Chi-squared test
# 
# data:  county_arrest_chi[-1, ]
# X-squared = 494.99, df = 14, p-value < 2.2e-16

# -> 2-sample test to compare proportions
county_arrest_ft <- addmargins(table(df_clean$county_name, df_clean$is_arrested, useNA = "ifany"))
county_arrest_ft2 <- tail(head(county_arrest_ft, -1),-1)
res10 <- prop.test(x = as.vector(county_arrest_ft2[,2]), n = as.vector(county_arrest_ft2[,4]))
res10
# 8-sample test for equality of proportions without continuity correction
# 
# data:  as.vector(county_arrest_ft2[, 2]) out of as.vector(county_arrest_ft2[, 4])
# X-squared = 192.94, df = 7, p-value < 2.2e-16
# alternative hypothesis: two.sided
# sample estimates:
#   prop 1     prop 2     prop 3     prop 4     prop 5     prop 6     prop 7     prop 8 
# 0.02363084 0.02452567 0.02231024 0.01550512 0.02372092 0.02847196 0.01951358 0.02501310 

# -> Is the arrest/stop proportion of New London significantly greater than
# the proportions of the other counties
non_newlondon <- colSums(county_arrest_ft2[c(1,2,3,4,5,7,8),])
county_table <- rbind(county_arrest_ft2, non_newlondon)
county_table <- county_table[c(6,9),]
res11 <- prop.test(x = as.vector(county_table[,2]), n = as.vector(county_table[,4]), alternative = "greater")
res11
# 2-sample test for equality of proportions with continuity correction
# 
# data:  as.vector(county_table[, 2]) out of as.vector(county_table[, 4])
# X-squared = 74.823, df = 1, p-value < 2.2e-16
# alternative hypothesis: greater
# 95 percent confidence interval:
#   0.005132913 1.000000000
# sample estimates:
#   prop 1     prop 2 
# 0.02847196 0.02198025 

# -> Is the proportion of Asians arrests in Litchfield statistically significant?
df_litchfield <- df_clean %>% filter(county_name == "Litchfield County")
litchfield_race <- addmargins(table(df_litchfield$driver_race_raw, df_litchfield$is_arrested, useNA = "ifany"))
litchfield_race <- head(litchfield_race, -1)
non_asian2 <- colSums(litchfield_race[c(2,3,4,5),])
litchfield_race2 <- rbind(litchfield_race, non_asian2)
litchfield_race2 <- litchfield_race2[c(1,6),]
res13 <- prop.test(x = as.vector(litchfield_race2[,2]), n = as.vector(litchfield_race2[,4]), alternative = "greater")
res13
# 2-sample test for equality of proportions with continuity correction
# 
# data:  as.vector(litchfield_race2[, 2]) out of as.vector(litchfield_race2[, 4])
# X-squared = 1.8591, df = 1, p-value = 0.08636
# alternative hypothesis: greater
# 95 percent confidence interval:
#   -0.009326125  1.000000000
# sample estimates:
#   prop 1     prop 2 
# 0.04046243 0.02219541 



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
# Pearson's Chi-squared test
# 
# data:  mtyr_arrest_chi
# X-squared = 359.76, df = 34, p-value < 2.2e-16

# -> 2-sample test to compare proportions
mtyr_arrest_ft <- addmargins(table(df_clean$month_year, df_clean$is_arrested, useNA = "ifany"))
mtyr_arrest_ft2 <- head(mtyr_arrest_ft, -1)
res12 <- prop.test(x = as.vector(mtyr_arrest_ft2[,2]), n = as.vector(mtyr_arrest_ft2[,4]))
res12
# 18-sample test for equality of proportions without continuity correction
# 
# data:  as.vector(mtyr_arrest_ft2[, 2]) out of as.vector(mtyr_arrest_ft2[, 4])
# X-squared = 301.07, df = 17, p-value < 2.2e-16
# alternative hypothesis: two.sided
# sample estimates:
#   prop 1     prop 2     prop 3     prop 4     prop 5     prop 6     prop 7     prop 8     prop 9 
# 0.01811777 0.02188026 0.02473887 0.02297433 0.02968790 0.02136108 0.02056186 0.01979363 0.02191453 
# prop 10    prop 11    prop 12    prop 13    prop 14    prop 15    prop 16    prop 17    prop 18 
# 0.01840014 0.02218557 0.01984243 0.02331590 0.03865615 0.02594900 0.02519733 0.02720717 0.02252014 

# Is the proportion of arrests/stops in Winter statistically significant from
# the proportion in non-winter months?
winter_months <- addmargins(table(df_clean$month_year, df_clean$is_arrested, useNA = "ifany"))
winter_months <- head(winter_months, -1)
non_winter <- colSums(winter_months[c(1,2,6:14, 18),])
winter <- colSums(winter_months[c(3:5, 15:17),])
winter_months2 <- rbind(winter, non_winter)
res14 <- prop.test(x = as.vector(winter_months2[,2]), n = as.vector(winter_months2[,4]), alternative = "greater")
res14
# 2-sample test for equality of proportions with continuity correction
# 
# data:  as.vector(winter_months2[, 2]) out of as.vector(winter_months2[, 4])
# X-squared = 36.774, df = 1, p-value = 6.633e-10
# alternative hypothesis: greater
# 95 percent confidence interval:
#   0.002715148 1.000000000
# sample estimates:
#   prop 1     prop 2 
# 0.02584058 0.02204773 



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
# Pearson's Chi-squared test
# 
# data:  dom_arrest_chi
# X-squared = 130.14, df = 60, p-value = 4.263e-07

# -> 2-sample test to compare proportions
dom_arrest_ft <- addmargins(table(df_clean$day_of_month, df_clean$is_arrested, useNA = "ifany"))
dom_arrest_ft <- head(dom_arrest_ft, -1)
res13 <- prop.test(x = as.vector(dom_arrest_ft[,2]), n = as.vector(dom_arrest_ft[,4]))
res13
# 31-sample test for equality of proportions without continuity correction
# 
# data:  as.vector(dom_arrest_ft[, 2]) out of as.vector(dom_arrest_ft[, 4])
# X-squared = 98.824, df = 30, p-value = 2.845e-09
# alternative hypothesis: two.sided
# sample estimates:
#   prop 1     prop 2     prop 3     prop 4     prop 5     prop 6     prop 7     prop 8     prop 9 
# 0.02768538 0.02397787 0.02177024 0.02370917 0.01945487 0.02368527 0.02241569 0.02192293 0.02297448 
# prop 10    prop 11    prop 12    prop 13    prop 14    prop 15    prop 16    prop 17    prop 18 
# 0.02352232 0.02140733 0.02500000 0.02592630 0.02886861 0.02648091 0.02408581 0.02242195 0.02298741 
# prop 19    prop 20    prop 21    prop 22    prop 23    prop 24    prop 25    prop 26    prop 27 
# 0.02236476 0.02270968 0.02216344 0.02884969 0.02071079 0.02142658 0.02063375 0.02082910 0.02105857 
# prop 28    prop 29    prop 30    prop 31 
# 0.02583096 0.01879910 0.01911147 0.01837235 

# Is the proportion of arrests/stops from days 21-31 lower than the other days
# of the month (roughly the last 3rd)?
end_of_month <- addmargins(table(df_clean$day_of_month, df_clean$is_arrested, useNA = "ifany"))
end_of_month <- head(end_of_month, -1)
end <- colSums(end_of_month[c(21:31),])
beginning <- colSums(end_of_month[c(1:20),])
end_of_month2 <- rbind(end, beginning)
res15 <- prop.test(x = as.vector(end_of_month2[,2]), n = as.vector(end_of_month2[,4]), alternative = "less")
res15
# 2-sample test for equality of proportions with continuity correction
# 
# data:  as.vector(end_of_month2[, 2]) out of as.vector(end_of_month2[, 4])
# X-squared = 11.748, df = 1, p-value = 0.0003046
# alternative hypothesis: less
# 95 percent confidence interval:
#   -1.0000000000 -0.0009956766
# sample estimates:
#   prop 1     prop 2 
# 0.02172794 0.02362915 


