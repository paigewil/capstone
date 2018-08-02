### Use Case:

#### Using machine learning algorithms as classifiers to create a model to predict if a traffic stop will end in arrest.

#### Data Wrangling:

Load in necessary libraries and datasets:

``` r
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)
library(caTools)
library(lubridate)
library(DMwR)
library(randomForest)
library(ROSE)
library(dplyr)
library(party)
library(irr)
library(klaR)
library(ROCR)
library(Boruta)
library(caretEnsemble)
library(gbm)

# Capstone data
stops_split <- read.csv("E:/Learning/Springboard Intro to Data Science/capstone/CT_cleaned_split.csv")

stops_split_col <- stops_split[,c(3,4,6,10,12,13,15,17,18,20,21,22,23,24,27,29:42)]
stops_split_col$stop_date <- as.POSIXct(stops_split_col$stop_date, "%Y-%m-%d", tz = "America/New_York")
```

Here, we used the split dataframe which one hot-encoded the violation\_raw column. This makes it easier to use the violation\_raw information in a machine learning algorithm.

We need to do some data wrangling before we can apply the machine learning algorithms. For example, we need to regroup factor features so that they don't have too many levels, since some algorithms can't handle high-level factors. We also need to split up some features, extracting seperate pieces of information into new features:

``` r
# Making new attributes for ML algorithms
stops_split_edit <- stops_split_col

# making hour attribute
stops_split_edit$stop_time_hour <- stops_split_edit$stop_time
stops_split_edit$stop_time_hour[stops_split_edit$stop_time_hour == "0:00"] <- NA
stops_split_edit$stop_time_hour <- sub(":.*", "", stops_split_edit$stop_time_hour)

# grouping hour into times of day since 24 levels might be too granular
stops_split_edit$stop_time_hour_numeric <- as.numeric(stops_split_edit$stop_time_hour)
stops_split_edit$stop_hour_part_of_day <- vector(mode = "character", length = nrow(stops_split_edit))
stops_split_edit$stop_hour_part_of_day[stops_split_edit$stop_time_hour_numeric >= 0 & stops_split_edit$stop_time_hour_numeric < 6] <- "time_block1"
stops_split_edit$stop_hour_part_of_day[stops_split_edit$stop_time_hour_numeric >= 6 & stops_split_edit$stop_time_hour_numeric < 12] <- "time_block2"
stops_split_edit$stop_hour_part_of_day[stops_split_edit$stop_time_hour_numeric >= 12 & stops_split_edit$stop_time_hour_numeric < 18] <- "time_block3"
stops_split_edit$stop_hour_part_of_day[stops_split_edit$stop_time_hour_numeric >= 18 & stops_split_edit$stop_time_hour_numeric <= 23] <- "time_block4"
# -> replacing blanks with NAs
stops_split_edit$stop_hour_part_of_day[stops_split_edit$stop_hour_part_of_day == ""] <- NA
# -> turning into factor
stops_split_edit$stop_hour_part_of_day <- factor(stops_split_edit$stop_hour_part_of_day)
```

``` r
# making stop month attribute 
stops_split_edit$stop_month <- month(stops_split_edit$stop_date)

# -> making seasons variable since month variable might be too granular
stops_split_edit$stop_month_numeric <- as.numeric(stops_split_edit$stop_month)
stops_split_edit$stop_season <- vector(mode = "character", length = nrow(stops_split_edit))
stops_split_edit$stop_season[stops_split_edit$stop_month_numeric >= 3 & stops_split_edit$stop_month_numeric <= 5] <- "spring"
stops_split_edit$stop_season[stops_split_edit$stop_month_numeric >= 6 & stops_split_edit$stop_month_numeric <= 8] <- "summer"
stops_split_edit$stop_season[stops_split_edit$stop_month_numeric >= 9 & stops_split_edit$stop_month_numeric <= 11] <- "autumn"
stops_split_edit$stop_season[stops_split_edit$stop_month_numeric == 12 | stops_split_edit$stop_month_numeric == 1 | stops_split_edit$stop_month_numeric == 2] <- "winter"
stops_split_edit$stop_season <- factor(stops_split_edit$stop_season)
```

``` r
# -> making stop day of month attribute
a <- c("01", "02", "03", "04", "05", "06", "07", "08", "09")
b <- as.character(10:31)
days_label <- c(a, b)
stops_split_edit$day_of_month <- format(stops_split_edit$stop_date, "%d")

# -> breaking day of month in rough thirds since stats investigation showed the last third of the month has a lower arrest/stop ratio
  #1-10
  #11-20
  #21-31
stops_split_edit$day_of_month_numeric <- as.numeric(stops_split_edit$day_of_month)
stops_split_edit$stop_dom <- vector(mode = "character", length = nrow(stops_split_edit))
stops_split_edit$stop_dom[stops_split_edit$day_of_month_numeric >= 1 & stops_split_edit$day_of_month_numeric <= 10] <- "first_third"
stops_split_edit$stop_dom[stops_split_edit$day_of_month_numeric >= 11 & stops_split_edit$day_of_month_numeric <= 20] <- "second_third"
stops_split_edit$stop_dom[stops_split_edit$day_of_month_numeric >= 21 & stops_split_edit$day_of_month_numeric <= 31] <- "third_third"
stops_split_edit$stop_dom <- factor(stops_split_edit$stop_dom)
```

``` r
# add day of week
stops_split_edit$day_of_week <- weekdays(stops_split_edit$stop_date)
stops_split_edit$day_of_week <- factor(stops_split_edit$day_of_week)
```

Cleaning up the dataset from all the wrangling steps:

``` r
# Trim of attributes created to create other attributes
# Also remove Officer ID since we don't want to create an algorithm with an ID column
# Removing original attributes that grouped/aggregated:
  # -> stop_time
  # -> stop_date
  # -> violation_raw
stops_split_small <- stops_split_edit[, c(-1,-2, -7, -13, -30, -31, -33, -34, -36, -37)]
stops_split_small_clustering <- stops_split_edit[, c(-1,-2, -13, -30, -31, -33, -34, -36, -37)]
# (for clustering-- at the bottom of the script because tested and didn't provide
#                   better performance) 
```

We need to turn some features into factors for easier handling in the machine learning algorithms:

``` r
for (i in c(5, 7, 9, 11:25)){
  stops_split_small[[i]] <- as.factor(stops_split_small[[i]])
}

for (i in c(6, 8, 10:26)){
  stops_split_small_clustering[[i]] <- as.factor(stops_split_small_clustering[[i]])
}
```

We need to handle NAs in the dataset explicitly instead of leaving it up to the machine learning algorithms. We will do so in a way to ensure minimum loss of information:

``` r
# NAs
  # -> county_name
  # -> driver_age
  # -> search_type_raw
  # -> stop_hour_part_of_day
  # -> stop_outcome
  # -> is_arrested
summary(stops_split_small)

stops_clean <- stops_split_small
```

We will populate driver\_age NAs with the median because it ends up being a whole number (like our other driver\_age ages) and is very similar to the mean:

``` r
#driver_age
med <- median(stops_clean$driver_age, na.rm = TRUE)
stops_clean$driver_age[is.na(stops_clean$driver_age) == TRUE] <- med
```

For search\_type\_raw, when no search was conducted, the value is NA. Instead, we'll use a different factor level, "None". In addition, there are some NAs where a search was conducted. Those are bucketed into the pre-existing "Other" category.

``` r
#search_type_raw
# -> replacing blanks with NAs
stops_clean$search_type_raw[stops_clean$search_type_raw == ""] <- NA
table(stops_clean$search_type_raw, stops_clean$search_conducted, useNA = "always")
# -> 313337 are from search_conducted = FALSE
# -> 486 where search_conducted = TRUE but no search type indicated
# adding level to factor to make a "None" level for the search types where no search was conducted
stops_clean$search_type_raw <- factor(stops_clean$search_type_raw, levels = c(levels(stops_clean$search_type_raw), "None"))
stops_clean$search_type_raw[(is.na(stops_clean$search_type_raw) == TRUE & stops_clean$search_conducted == FALSE)] <- "None"

table(stops_clean$search_type_raw, stops_clean$is_arrested, useNA = "always")
# 151/7312  = 0.02065098 are to is_arrested == TRUE, so can't remove with out 
#                        significant loss of information
# => will identify as "Other"" since that is the mode of is_arrested = TRUE and a 
#    generic "catch all" level
stops_clean$search_type_raw[(is.na(stops_clean$search_type_raw) == TRUE)] <- "Other"
```

county\_name and stop\_hour\_part\_of\_day have small amounts of NAs so those rows will be removed since there is no sensible way to populate them:

``` r
# county_name
stops_clean$county_name[stops_clean$county_name == ""] <- NA
#get rid of blank level
stops_clean$county_name <- factor(stops_clean$county_name, levels = levels(stops_clean$county_name)[2:9])
table(stops_clean$county_name, stops_clean$is_arrested, useNA = "always")
# 4/7312 = .000547046 (percentage removed from is_arrested == TRUE)
# 10/306001 = 3.267963e-05
# No sizeable removal amount

# stop_hour_part_of_day
table(stops_clean$stop_hour_part_of_day, stops_clean$is_arrested, useNA = "always")
table(stops_clean$stop_hour_part_of_day, stops_clean$county_name, useNA = "always")
# -> no overlapping NAs with county_name
# 8/7308 = 0.001094691 (percentage removed from is_arrested == TRUE after remove
#                       county_name NAs)
# 212/305991 = 0.0006928308

# Since neither county_name or stop_hour_part_of_day NAs account for a significant
# percentage of our outcome variables, we will remove the rows with those NAs.
stops_clean2 <- stops_clean[!is.na(stops_clean$stop_hour_part_of_day),]
stops_clean2 <- stops_clean2[!is.na(stops_clean2$county_name),]
```

There are NAs in our outcome variable, is\_arrested. Since our algorithm might become biased if we assume an arrest status for the NAs, we must remove the rows where is\_arrested = NA:

``` r
# How are NAs distributed throughout is_arrested?
table(stops_clean2$is_arrested, useNA = "always")
# 5352 NAs 

stops_clean2 <- stops_clean2[!is.na(stops_clean2$is_arrested),]
stops_clean3 <- stops_clean2
```

Before we start running some algorithms, we need to remove the stop\_outcome variable to only focus on predicting is\_arrested:

``` r
stops_arrested <- stops_clean3[,-8]
```

#### Algorithms:

Let's split our dataset into a test and training set:

``` r
# Since I have a lot of data, I want to make my testing sample a little larger
# to have better tests to make sure I'm choosing the best algorithm

# Splitting data using createDataPartition since it works better with unbalanced data
set.seed(3000)
spl_cdp = createDataPartition(stops_arrested$is_arrested, p = 0.75, list = FALSE)
train_arrest = stops_arrested[spl_cdp,]
test_arrest = stops_arrested[-spl_cdp,]
```

One hurdle in the dataset is that it is signficantly imbalanced, meaning we have a lot more non-arrests than arrests. Specifically, non-arrests make up 97.67% of the data. To account for the imbalance such that the machine learning algorithm doesn't predict False for every observation to get a high accuracy, we will test a number of solutions with a number of algorithms to pick the best performing model.

Using SMOTE to fix imbalance with the CART algorithm:

``` r
set.seed(111)
train.smote <- SMOTE(is_arrested ~., data = train_arrest)
tree.smote <- rpart(is_arrested ~ ., data = train.smote)
prp(tree.smote)
```

![](machine_learning_report_files/figure-markdown_github/unnamed-chunk-15-1.png)

When predicting on the test set, we see that the Kappa is low at 0.2214 with the AUC, not bad at 0.77:

``` r
pred.tree.smote2 <- predict(tree.smote, newdata = test_arrest, type = "class")
confusionMatrix(table(test_arrest$is_arrested, pred.tree.smote2))$overall['Kappa']
```

    ##     Kappa 
    ## 0.2214208

``` r
roc.curve(test_arrest$is_arrested, pred.tree.smote2)
```

![](machine_learning_report_files/figure-markdown_github/unnamed-chunk-16-1.png)

    ## Area under the curve (AUC): 0.770

Using over-sampling to fix imbalance with the Random Forest algorithm:

``` r
set.seed(111)
train_arrest_over <- ovun.sample(is_arrested ~., data = train_arrest, method = "over", N = 2*229335)$data
rf.over <- randomForest(is_arrested ~ ., data = train_arrest_over, ntree = 50)
pred.rf.over <- predict(rf.over, newdata = test_arrest, type = "class")
confusionMatrix(table(test_arrest$is_arrested, pred.rf.over))$overall['Kappa']
```

    ##    Kappa 
    ## 0.322508

``` r
roc.curve(test_arrest$is_arrested, pred.rf.over)
```

![](machine_learning_report_files/figure-markdown_github/unnamed-chunk-17-1.png)

    ## Area under the curve (AUC): 0.732

The Kappa is slightly higher, but still low at 0.3134 and the AUC is not bad at 0.733:

Using SMOTE to fix imbalance with the Naive Bayes algorithm:

``` r
set.seed(111)
nb.model.smote <- naiveBayes(is_arrested ~., data = train.smote, laplace = 1)
nb.predict.smote <- predict(nb.model.smote, newdata = test_arrest, type = "class")
confusionMatrix(table(test_arrest$is_arrested, nb.predict.smote))$overall['Kappa']
```

    ##    Kappa 
    ## 0.256684

``` r
roc.curve(test_arrest$is_arrested, nb.predict.smote)
```

![](machine_learning_report_files/figure-markdown_github/unnamed-chunk-18-1.png)

    ## Area under the curve (AUC): 0.744

The Kappa is still low at 0.258102 with the AUC at 0.743.

Now, instead of balancing the unbalanced dataset by adding manufactured obserations or removing them, we can also use a Penalty Matrix. Here, we want a higher penalty for false negatives, i.e., where the model labels an observation as having no arrest when there was one, because we're most concerned with accurately labeling arrests. Thus, our Penalty Matrix will weight those error more heavily.

``` r
PenaltyMatrix = matrix(c(0,1,6,0), byrow = TRUE, nrow = 2)
```

Here, we use CART with our Penalty Matrix:

``` r
stops_arrest_tree_penalty <- rpart(is_arrested ~., data = train_arrest, parms = list(loss = PenaltyMatrix))
prp(stops_arrest_tree_penalty)
```

![](machine_learning_report_files/figure-markdown_github/unnamed-chunk-20-1.png)

``` r
PredictCARTPenalty2 = predict(stops_arrest_tree_penalty, newdata = test_arrest, type = "class")
confusionMatrix(table(test_arrest$is_arrested, PredictCARTPenalty2))$overall['Kappa']
```

    ##     Kappa 
    ## 0.3537442

``` r
roc.curve(test_arrest$is_arrested, PredictCARTPenalty2)
```

![](machine_learning_report_files/figure-markdown_github/unnamed-chunk-20-2.png)

    ## Area under the curve (AUC): 0.693

This model, so far, performs the best with a Kappa at 0.3537442 and an AUC at 0.693.

To help us widdle down the model options, we'll perform a 10-fold CV test and see what Kappa and AUC values are returned.

SMOTE with CART:

``` r
set.seed(123)
folds <- createFolds(stops_arrested$is_arrested, k = 10)
cv_results <- lapply(folds, function(x) {
  arrest_train <- stops_arrested[-x, ]
  arrest_test <- stops_arrested[x, ]
  arrest.train.smote <- SMOTE(is_arrested ~., data = arrest_train)
  tree.model <- rpart(is_arrested ~ ., data = arrest.train.smote)
  arrest.pred <- predict(tree.model, arrest_test, type = "class")
  auc_val <- roc.curve(arrest_test$is_arrested, arrest.pred)$auc
  kappa <- kappa2(data.frame(arrest_test$is_arrested, arrest.pred))$value
  return(c(auc_val, kappa))
})
```

``` r
df_cv_results <- data.frame(cv_results)
#AUC
mean(unlist(df_cv_results[1,]))
#Kappa
mean(unlist(df_cv_results[2,]))
```

Penalty Matrix with CART:

``` r
set.seed(123)
cv_resultsPM <- lapply(folds, function(x) {
  arrest_train <- stops_arrested[-x, ]
  arrest_test <- stops_arrested[x, ]
  tree.model <- rpart(is_arrested ~ ., data = arrest_train, parms = list(loss = PenaltyMatrix))
  arrest.pred <- predict(tree.model, arrest_test, type = "class")
  auc_val <- roc.curve(arrest_test$is_arrested, arrest.pred)$auc
  kappa <- kappa2(data.frame(arrest_test$is_arrested, arrest.pred))$value
  return(c(auc_val, kappa))
})
```

``` r
df_cv_resultsPM <- data.frame(cv_resultsPM)
#AUC
mean(unlist(df_cv_resultsPM[1,]))
```

    ## [1] 0.7018223

``` r
#Kappa
mean(unlist(df_cv_resultsPM[2,]))
```

    ## [1] 0.3649853

SMOTE with RandomForest:

``` r
set.seed(123)
cv_results_rf <- lapply(folds, function(x) {
  arrest_train <- stops_arrested[-x, ]
  arrest_test <- stops_arrested[x, ]
  arrest.train.smote <- SMOTE(is_arrested ~., data = arrest_train)
  tree.model <- randomForest(is_arrested ~ ., data = arrest.train.smote)
  arrest.pred <- predict(tree.model, arrest_test, type = "class")
  auc_val <- roc.curve(arrest_test$is_arrested, arrest.pred)$auc
  kappa <- kappa2(data.frame(arrest_test$is_arrested, arrest.pred))$value
  return(c(auc_val, kappa))
})
```

``` r
df_cv_results_rf <- data.frame(cv_results_rf)
#AUC
mean(unlist(df_cv_results_rf[1,]))
#Kappa
mean(unlist(df_cv_results_rf[2,]))
```

Over-sampling with RandomForest:

``` r
f1.train <- stops_arrested[-folds$Fold01, ]
f1.test <- stops_arrested[folds$Fold01, ]
f1.ovun <- ovun.sample(is_arrested ~., data = f1.train, method = "over", N = 2*nrow(f1.train[f1.train$is_arrested == FALSE,]))$data
f1.model <- randomForest(is_arrested ~ ., data = f1.ovun, ntree = 50)
f1.pred <- predict(f1.model, f1.test, type = "class")
f1_auc_val <- roc.curve(f1.test$is_arrested, f1.pred)$auc
```

``` r
f1_kappa <- kappa2(data.frame(f1.test$is_arrested, f1.pred))$value

f2.train <- stops_arrested[-folds$Fold02, ]
f2.test <- stops_arrested[folds$Fold02, ]
f2.ovun <- ovun.sample(is_arrested ~., data = f2.train, method = "over", N = 2*nrow(f2.train[f2.train$is_arrested == FALSE,]))$data
f2.model <- randomForest(is_arrested ~ ., data = f2.ovun, ntree = 50)
f2.pred <- predict(f2.model, f2.test, type = "class")
f2_auc_val <- roc.curve(f2.test$is_arrested, f2.pred)$auc
```

``` r
f2_kappa <- kappa2(data.frame(f2.test$is_arrested, f2.pred))$value

f3.train <- stops_arrested[-folds$Fold03, ]
f3.test <- stops_arrested[folds$Fold03, ]
f3.ovun <- ovun.sample(is_arrested ~., data = f3.train, method = "over", N = 2*nrow(f3.train[f3.train$is_arrested == FALSE,]))$data
f3.model <- randomForest(is_arrested ~ ., data = f3.ovun, ntree = 50)
f3.pred <- predict(f3.model, f3.test, type = "class")
f3_auc_val <- roc.curve(f3.test$is_arrested, f3.pred)$auc
```

``` r
f3_kappa <- kappa2(data.frame(f3.test$is_arrested, f3.pred))$value

f4.train <- stops_arrested[-folds$Fold04, ]
f4.test <- stops_arrested[folds$Fold04, ]
f4.ovun <- ovun.sample(is_arrested ~., data = f4.train, method = "over", N = 2*nrow(f4.train[f4.train$is_arrested == FALSE,]))$data
f4.model <- randomForest(is_arrested ~ ., data = f4.ovun, ntree = 50)
f4.pred <- predict(f4.model, f4.test, type = "class")
f4_auc_val <- roc.curve(f4.test$is_arrested, f4.pred)$auc
```

``` r
f4_kappa <- kappa2(data.frame(f4.test$is_arrested, f4.pred))$value

f5.train <- stops_arrested[-folds$Fold05, ]
f5.test <- stops_arrested[folds$Fold05, ]
f5.ovun <- ovun.sample(is_arrested ~., data = f5.train, method = "over", N = 2*nrow(f5.train[f5.train$is_arrested == FALSE,]))$data
f5.model <- randomForest(is_arrested ~ ., data = f5.ovun, ntree = 50)
f5.pred <- predict(f5.model, f5.test, type = "class")
f5_auc_val <- roc.curve(f5.test$is_arrested, f5.pred)$auc
```

``` r
f5_kappa <- kappa2(data.frame(f5.test$is_arrested, f5.pred))$value

f6.train <- stops_arrested[-folds$Fold06, ]
f6.test <- stops_arrested[folds$Fold06, ]
f6.ovun <- ovun.sample(is_arrested ~., data = f6.train, method = "over", N = 2*nrow(f6.train[f6.train$is_arrested == FALSE,]))$data
f6.model <- randomForest(is_arrested ~ ., data = f6.ovun, ntree = 50)
f6.pred <- predict(f6.model, f6.test, type = "class")
f6_auc_val <- roc.curve(f6.test$is_arrested, f6.pred)$auc
```

``` r
f6_kappa <- kappa2(data.frame(f6.test$is_arrested, f6.pred))$value

f7.train <- stops_arrested[-folds$Fold07, ]
f7.test <- stops_arrested[folds$Fold07, ]
f7.ovun <- ovun.sample(is_arrested ~., data = f7.train, method = "over", N = 2*nrow(f7.train[f7.train$is_arrested == FALSE,]))$data
f7.model <- randomForest(is_arrested ~ ., data = f7.ovun, ntree = 50)
f7.pred <- predict(f7.model, f7.test, type = "class")
f7_auc_val <- roc.curve(f7.test$is_arrested, f7.pred)$auc
```

``` r
f7_kappa <- kappa2(data.frame(f7.test$is_arrested, f7.pred))$value

f8.train <- stops_arrested[-folds$Fold08, ]
f8.test <- stops_arrested[folds$Fold08, ]
f8.ovun <- ovun.sample(is_arrested ~., data = f8.train, method = "over", N = 2*nrow(f8.train[f8.train$is_arrested == FALSE,]))$data
f8.model <- randomForest(is_arrested ~ ., data = f8.ovun, ntree = 50)
f8.pred <- predict(f8.model, f8.test, type = "class")
f8_auc_val <- roc.curve(f8.test$is_arrested, f8.pred)$auc
```

``` r
f8_kappa <- kappa2(data.frame(f8.test$is_arrested, f8.pred))$value

f9.train <- stops_arrested[-folds$Fold09, ]
f9.test <- stops_arrested[folds$Fold09, ]
f9.ovun <- ovun.sample(is_arrested ~., data = f9.train, method = "over", N = 2*nrow(f9.train[f9.train$is_arrested == FALSE,]))$data
f9.model <- randomForest(is_arrested ~ ., data = f9.ovun, ntree = 50)
f9.pred <- predict(f9.model, f9.test, type = "class")
f9_auc_val <- roc.curve(f9.test$is_arrested, f9.pred)$auc
```

``` r
f9_kappa <- kappa2(data.frame(f9.test$is_arrested, f9.pred))$value

f10.train <- stops_arrested[-folds$Fold10, ]
f10.test <- stops_arrested[folds$Fold10, ]
f10.ovun <- ovun.sample(is_arrested ~., data = f10.train, method = "over", N = 2*nrow(f10.train[f10.train$is_arrested == FALSE,]))$data
f10.model <- randomForest(is_arrested ~ ., data = f10.ovun, ntree = 50)
f10.pred <- predict(f10.model, f10.test, type = "class")
f10_auc_val <- roc.curve(f10.test$is_arrested, f10.pred)$auc
```

``` r
f10_kappa <- kappa2(data.frame(f10.test$is_arrested, f10.pred))$value


combined_auc <- c(f1_auc_val, f2_auc_val, f3_auc_val, f4_auc_val, f5_auc_val, f6_auc_val, f7_auc_val, f8_auc_val, f9_auc_val, f10_auc_val)
combined_kappa <- c(f1_kappa, f2_kappa, f3_kappa, f4_kappa, f5_kappa, f6_kappa, f7_kappa, f8_kappa, f9_kappa, f10_kappa)
```

``` r
#AUC
mean(combined_auc)
```

    ## [1] 0.7337325

``` r
#Kappa
mean(combined_kappa)
```

    ## [1] 0.3045106

SMOTE with Naive Bayes:

``` r
set.seed(123)
cv_results_nb <- lapply(folds, function(x) {
  arrest_train <- stops_arrested[-x, ]
  arrest_test <- stops_arrested[x, ]
  arrest.train.smote <- SMOTE(is_arrested ~., data = arrest_train)
  tree.model <- naiveBayes(is_arrested ~., data = arrest.train.smote, laplace = 1)
  arrest.pred <- predict(tree.model, arrest_test, type = "class")
  auc_val <- roc.curve(arrest_test$is_arrested, arrest.pred)$auc
  kappa <- kappa2(data.frame(arrest_test$is_arrested, arrest.pred))$value
  return(c(auc_val, kappa))
})
```

``` r
df_cv_results_nb <- data.frame(cv_results_nb)
#AUC
mean(unlist(df_cv_results_nb[1,]))
#Kappa
mean(unlist(df_cv_results_nb[2,]))
```

From the resulting Kappas and AUCs, we will fine-tune the top three performing models using cross-validation and pick our top performing of them to use as our overall model.