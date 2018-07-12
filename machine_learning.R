# load libraries
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

# Capstone data
# stops <- read.csv("E:/Learning/Springboard Intro to Data Science/capstone/CT_cleaned_edit.csv")
# str(stops)

stops_split <- read.csv("E:/Learning/Springboard Intro to Data Science/capstone/CT_cleaned_split.csv")

stops_split_col <- stops_split[,c(3,4,6,10,12,13,15,17,19,20,21,22,23,24,27,29:42)]
stops_split_col$stop_date <- as.POSIXct(stops_split_col$stop_date, "%Y-%m-%d", tz = "America/New_York")

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

# -> making stop day of month attribute
a <- c("01", "02", "03", "04", "05", "06", "07", "08", "09")
b <- as.character(10:31)
days_label <- c(a, b)
stops_split_edit$day_of_month <- format(stops_split_edit$stop_date, "%d")

# ->breaking of day of month in rough thirds to match stats
  #1-10
  #11-20
  #21-31
stops_split_edit$day_of_month_numeric <- as.numeric(stops_split_edit$day_of_month)
stops_split_edit$stop_dom <- vector(mode = "character", length = nrow(stops_split_edit))
stops_split_edit$stop_dom[stops_split_edit$day_of_month_numeric >= 1 & stops_split_edit$day_of_month_numeric <= 10] <- "first_third"
stops_split_edit$stop_dom[stops_split_edit$day_of_month_numeric >= 11 & stops_split_edit$day_of_month_numeric <= 20] <- "second_third"
stops_split_edit$stop_dom[stops_split_edit$day_of_month_numeric >= 21 & stops_split_edit$day_of_month_numeric <= 31] <- "third_third"
stops_split_edit$stop_dom <- factor(stops_split_edit$stop_dom)

# add day of week
stops_split_edit$day_of_week <- weekdays(stops_split_edit$stop_date)
stops_split_edit$day_of_week <- factor(stops_split_edit$day_of_week)


# Trim of attributes created to create other attributes
# Also remove Officer ID since we don't want to create an alogirthm with an ID column
# Removing original attributes that groups/aggregated:
  # -> stop_time
  # -> stop_date
  # -> violation_raw
stops_split_small <- stops_split_edit[, c(-1,-2, -7, -13, -30, -31, -33, -34, -36, -37)]
# -> making the right data type
for (i in c(5, 7, 9, 11:25)){
  stops_split_small[[i]] <- as.factor(stops_split_small[[i]])
}
str(stops_split_small)


# NAs
  # -> county_name
  # -> driver_age
  # -> search_type
  # -> stop_hour_part_of_day
  # -> stop_outcome
  # -> is_arrested
summary(stops_split_small)

stops_clean <- stops_split_small

#driver_age
# Will populate age NAs with median per investigate in capstone_data_wrangling.R
med <- median(stops_clean$driver_age, na.rm = TRUE)
stops_clean$driver_age[is.na(stops_clean$driver_age) == TRUE] <- med


#search_type
table(stops_clean$search_type, stops_clean$search_conducted, useNA = "always")
# -> 313337 are from search_conducted = FALSE
# -> 486 where search_conducted = TRUE but no search type indicated
# adding level to factor to make a "None" level for the search types where no search was conducted
stops_clean$search_type <- factor(stops_clean$search_type, levels = c(levels(stops_clean$search_type), "None"))
stops_clean$search_type[(is.na(stops_clean$search_type) == TRUE & stops_clean$search_conducted == FALSE)] <- "None"

table(stops_clean$search_type, stops_clean$is_arrested, useNA = "always")
# 151/7312  = 0.02065098 are to is_arrested == TRUE, so can't remove with out 
#                        significant loss of information
# => will identify as Other since that is the mode of is_arrested = TRUE and a 
#    generic "catch all" level
stops_clean$search_type[(is.na(stops_clean$search_type) == TRUE)] <- "Other"



# How are NAs distributed throughout stop_outcome and is_arrested?
table(stops_clean$is_arrested, useNA = "always")
table(stops_clean$stop_outcome, useNA = "always")
table(stops_clean$is_arrested, stops_clean$stop_outcome, useNA = "always")
# 5356 NAs in both stop_outcome and is_arrested -> will remove before ML

# county_name
stops_clean$county_name[stops_clean$county_name == ""] <- NA
table(stops_clean$county_name, stops_clean$is_arrested, useNA = "always")
# 4/7312 = .000547046 (percentage removed from is_arrested == TRUE)
# 10/306001 = 3.267963e-05
table(stops_clean$county_name, stops_clean$stop_outcome, useNA = "always")
# No sizeable removal amount

# stop_hour_part_of_day
table(stops_clean$stop_hour_part_of_day, stops_clean$is_arrested, useNA = "always")
table(stops_clean$stop_hour_part_of_day, stops_clean$county_name, useNA = "always")
# -> no overlapping NAs with county_name
# 8/7308 = 0.001094691 (percentage removed from is_arrested == TRUE after remove
#                       county_name NAs)
# 212/305991 = 0.0006928308
table(stops_clean$stop_hour_part_of_day, stops_clean$stop_outcome, useNA = "always")


# Since neither county_name or stop_hour_part_of_day NAs account for a significant
# percentage of our outcome variables, we will remove the rows with those NAs.
#stops_clean2 <- stops_clean[which(is.na(stops_clean$stop_hour_part_of_day) != TRUE | is.na(stops_clean$county_name) != TRUE),]
#stops_clean2 <- subset(stops_clean, is.na(stop_hour_part_of_day) != TRUE | is.na(county_name) != TRUE)
#stops_clean2 <- stops_clean[!is.na(stops_clean$stop_hour_part_of_day) | !is.na(stops_clean$county_name),]
stops_clean2 <- stops_clean[!is.na(stops_clean$stop_hour_part_of_day),]
stops_clean2 <- stops_clean2[!is.na(stops_clean2$county_name),]

# Since we can't tell if our prediction is right for outcomes with NAs, we'll
# remove the rows where is_arrested and stop_outcome = NA, which are the same
# rows for both

stops_clean2 <- stops_clean2[!is.na(stops_clean2$is_arrested),]
summary(stops_clean2)
str(stops_clean2)




# Algorithm testing time!

# Split into is_arrested and stop_outcome dataframes to can run algorithms
# to predict both seperately
stops_so <- stops_clean2[,-9]
stops_arrested <- stops_clean2[,-8]


#### is_arrested

# Split the data into test and training data sets
# -> since I have a lot of data, I want to make my testing sample a little larger
#    to have better tests to make sure I'm choosing the best algorithm
# set.seed(3000)
# spl = sample.split(stops_arrested$is_arrested, SplitRatio = 0.6)
# train_arrest = subset(stops_arrested, spl==TRUE)
# test_arrest = subset(stops_arrested, spl==FALSE)

# Splitting data using createDataPartition since works better with unbalanced data
set.seed(3000)
spl_cdp = createDataPartition(stops_arrested$is_arrested, p = 0.75, list = FALSE)
train_arrest = stops_arrested[spl_cdp,]
test_arrest = stops_arrested[-spl_cdp,]

#Applying CART algorithm without handling unbalanced data
stops_arrest_tree <- rpart(is_arrested ~., data = train_arrest, method = "class", minbucket = 1000)
prp(stops_arrest_tree)  #plot the tree
print(stops_arrest_tree)
# -> There are no splits-- all bucketed to FALSE

#trying to change parameter, minbucket, for better results
stops_arrest_tree2 <- rpart(is_arrested ~., data = train_arrest, method = "class", minbucket = 100)
prp(stops_arrest_tree2)
print(stops_arrest_tree2)
# -> No improvement


# Make predictions
PredictCART = predict(stops_arrest_tree, type = "class")
confusionMatrix(table(train_arrest$is_arrested, PredictCART))
# accuracy = 0.9767
# precision/specificity = NA
# recall/sensitivity  = 0.9767
# Kappa = 0
roc.curve(train_arrest$is_arrested, PredictCART)
# -> 0.500
# => Just buckets all as is_arrested == TRUE


# Using cross-validation to choose parameters and hopefully get better results
# Define cross-validation experiment
numFolds = trainControl(method = "cv", number = 10)
cpGrid = expand.grid(.cp = seq(0.1,1,0.1))
train(is_arrested ~ ., data = train_arrest, method = "rpart", trControl = numFolds, tuneGrid = cpGrid)
# -> cp = 1

# Create a new CART model
stops_arrest_treecv = rpart(is_arrested ~ ., data = train_arrest, method="class", cp = 1)
prp(stops_arrest_treecv)
# -> still no splits-- all bucketed to FALSE


# Using SMOTE to fix imbalanced data to get better results from algorithms
train.smote <- SMOTE(is_arrested ~., data = train_arrest)
tree.smote <- rpart(is_arrested ~ ., data = train.smote)
prp(tree.smote)

#on training set
pred.tree.smote <- predict(tree.smote, type = "class")
confusionMatrix(table(train.smote$is_arrested, pred.tree.smote))
# Kappa = 0.7693
roc.curve(train.smote$is_arrested, pred.tree.smote)
# -> 0.882
# AUC improved so using SMOTE is a better model thus far

#on test set
pred.tree.smote2 <- predict(tree.smote, newdata = test_arrest, type = "class")
confusionMatrix(table(test_arrest$is_arrested, pred.tree.smote2))
# Kappa = 0.2214
roc.curve(test_arrest$is_arrested, pred.tree.smote2)
# -> 0.770



# ROSE as a sampler
train.rose <- ROSE(is_arrested ~ ., data = train_arrest, seed = 1)$data
tree.rose <- rpart(is_arrested ~ ., data = train.rose)
prp(tree.rose)

pred.tree.rose <- predict(tree.rose, type = "class")
confusionMatrix(table(train.rose$is_arrested, pred.tree.rose))
# Kappa = 0.5552
roc.curve(train.rose$is_arrested, pred.tree.rose)
# -> 0.778

pred.tree.rose2 <- predict(tree.rose, newdata = test_arrest, type = "class")
confusionMatrix(table(test_arrest$is_arrested, pred.tree.rose2))
# Kappa = 0.2214; not any better than SMOTE
roc.curve(test_arrest$is_arrested, pred.tree.rose2)
# -> 0.770



# Under-sampling as a sampler
table(train_arrest$is_arrested)
train_arrest_under <- ovun.sample(is_arrested ~., data = train_arrest, method = "under", N = 2*5475)$data
tree.under <- rpart(is_arrested ~ ., data = train_arrest_under)
pred.tree.under <- predict(tree.under, newdata = test_arrest, type = "class")
confusionMatrix(table(test_arrest$is_arrested, pred.tree.under))
# Kappa = 0.2214
roc.curve(test_arrest$is_arrested, pred.tree.under)
# -> 0.770



# Over-sampling as a sampler
train_arrest_over <- ovun.sample(is_arrested ~., data = train_arrest, method = "over", N = 2*229335)$data
tree.over <- rpart(is_arrested ~ ., data = train_arrest_over)
pred.tree.over <- predict(tree.over, newdata = test_arrest, type = "class")
confusionMatrix(table(test_arrest$is_arrested, pred.tree.over))
# Kappa = 0.2214
roc.curve(test_arrest$is_arrested, pred.tree.over)
# -> 0.770



# Seeing if RandomForest can improve model
rf.smote <- randomForest(is_arrested ~ ., data = train.smote)
pred.rf.smote <- predict(rf.smote, type = "class")
confusionMatrix(table(train.smote$is_arrested, pred.rf.smote))
# Kappa = 0.8
roc.curve(train.smote$is_arrested, pred.rf.smote)
# -> 0.895; best result so far

pred.rf.smote2 <- predict(rf.smote, newdata = test_arrest, type = "class")
confusionMatrix(table(test_arrest$is_arrested, pred.rf.smote2))
# Kappa = 0.2896 
roc.curve(test_arrest$is_arrested, pred.rf.smote2)
# -> 0.761


# vizualizing tree; understand decision making in RF, one tree at a time
getTree(rf.smote)

#variable importance
importance(rf.smote)
varImpPlot(rf.smote)
# -> we see that stop duration is the most important variable, aligning with
#    what we saw in the CART model
# -> search_type, search_conducted, and contraband_found are also important variables,
#    also seen in the CART model



# RF with ROSE data instead
rf.rose <- randomForest(is_arrested ~ ., data = train.rose, ntree = 100)
pred.rf.rose <- predict(rf.rose, newdata = test_arrest, type = "class")
confusionMatrix(table(test_arrest$is_arrested, pred.rf.rose))
# Kappa = 0.2721
roc.curve(test_arrest$is_arrested, pred.rf.rose)
# -> 0.761



# RF with under-sampled data instead
rf.under <- randomForest(is_arrested ~ ., data = train_arrest_under)
pred.rf.under <- predict(rf.under, newdata = test_arrest, type = "class")
confusionMatrix(table(test_arrest$is_arrested, pred.rf.under))
# Kappa = 0.2045
roc.curve(test_arrest$is_arrested, pred.rf.under)
# -> 0.780



# RF with over-sampled data instead
rf.over <- randomForest(is_arrested ~ ., data = train_arrest_over, ntree = 100)
pred.rf.over <- predict(rf.over, newdata = test_arrest, type = "class")
confusionMatrix(table(test_arrest$is_arrested, pred.rf.over))
# Kappa = 0.3125
roc.curve(test_arrest$is_arrested, pred.rf.over)
# -> 0.735



#Naive Bayes
# with SMOTE
nb.model.smote <- naiveBayes(is_arrested ~., data = train.smote, laplace = 1)
nb.predict.smote <- predict(nb.model.smote, newdata = test_arrest, type = "class")
confusionMatrix(table(test_arrest$is_arrested, nb.predict.smote))
# Kappa = 0.2599
roc.curve(test_arrest$is_arrested, nb.predict.smote)
# -> 0.742


# with ROSE
nb.model.rose <- naiveBayes(is_arrested ~., data = train.rose, laplace = 1)
nb.predict.rose <- predict(nb.model.rose, newdata = test_arrest, type = "class")
confusionMatrix(table(test_arrest$is_arrested, nb.predict.rose))
# Kappa = 0.1741
roc.curve(test_arrest$is_arrested, nb.predict.rose)
# -> 0.769


# with Under-sampling
nb.model.under <- naiveBayes(is_arrested ~., data = train_arrest_under, laplace = 1)
nb.predict.under <- predict(nb.model.under, newdata = test_arrest, type = "class")
confusionMatrix(table(test_arrest$is_arrested, nb.predict.under))
# Kappa = 0.1735  
roc.curve(test_arrest$is_arrested, nb.predict.under)
# -> 0.768


# with over-sampling
nb.model.over <- naiveBayes(is_arrested ~., data = train_arrest_over, laplace = 1)
nb.predict.over <- predict(nb.model.over, newdata = test_arrest, type = "class")
confusionMatrix(table(test_arrest$is_arrested, nb.predict.over))
# Kappa = 0.1736
roc.curve(test_arrest$is_arrested, nb.predict.over)
# -> 0.768



# Testing using a Penalty Matrix instead of balancing the unbalanced dataset
# -> Want higher penalty for false negative, i.e., where say no arrest when there
#    was. Want this because we need to accurately label arrests.
PenaltyMatrix = matrix(c(0,1,6,0), byrow = TRUE, nrow = 2)

# with CART
stops_arrest_tree_penalty <- rpart(is_arrested ~., data = train_arrest, parms = list(loss = PenaltyMatrix))
prp(stops_arrest_tree_penalty)
print(stops_arrest_tree_penalty)

PredictCARTPenalty = predict(stops_arrest_tree_penalty, type = "class")
confusionMatrix(table(train_arrest$is_arrested, PredictCARTPenalty))
# Kappa = 0.3686
roc.curve(train_arrest$is_arrested, PredictCARTPenalty)
# -> 0.705

PredictCARTPenalty2 = predict(stops_arrest_tree_penalty, newdata = test_arrest, type = "class")
confusionMatrix(table(test_arrest$is_arrested, PredictCARTPenalty2))
# Kappa = 0.3537
roc.curve(test_arrest$is_arrested, PredictCARTPenalty2)
# -> 0.693

# Using ROC to fine tune threshold
PredictCARTPenalty3 = predict(stops_arrest_tree_penalty, newdata = test_arrest, type = "p")
confusionMatrix(table(test_arrest$is_arrested, PredictCARTPenalty3[,2] >= 0.1))
# Kappa = 0.3537
roc.curve(test_arrest$is_arrested, PredictCARTPenalty3[,2])
# -> 0.771
ROCRpred2 <- prediction(PredictCARTPenalty3[,2], test_arrest$is_arrested)
ROCRperf2 <- performance(ROCRpred2, "tpr", "fpr")
plot(ROCRperf2)
plot(ROCRperf2, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
plot(ROCRperf2, colorize=TRUE, print.cutoffs.at=seq(0,0.1,by=0.01), text.adj=c(-0.2,1.7))


# with RF
stops_arrest_rf_penalty <- randomForest(is_arrested ~., data = train_arrest, ntree= 200, parms = list(loss = PenaltyMatrix))
PredictRFPenalty = predict(stops_arrest_rf_penalty, newdata = test_arrest, type = "class")
confusionMatrix(table(test_arrest$is_arrested, PredictRFPenalty))
# Kappa = 0.2757
roc.curve(test_arrest$is_arrested, PredictRFPenalty)
# -> 0.587


# with NB
stops_arrest_nb_penalty <- naiveBayes(is_arrested ~., data = train_arrest, laplace = 1, parms = list(loss = PenaltyMatrix))
PredictNBPenalty = predict(stops_arrest_nb_penalty, newdata = test_arrest, type = "class")
confusionMatrix(table(test_arrest$is_arrested, PredictNBPenalty))
# Kappa = 0.2867
roc.curve(test_arrest$is_arrested, PredictNBPenalty)
# -> 0.632




### Trying logistic regression model
# With SMOTE
log.smote <- glm(is_arrested ~ ., data = train.smote, family = "binomial")
model.log.smote <- predict(log.smote, newdata = test_arrest, type = "response")
confusionMatrix(table(test_arrest$is_arrested, model.log.smote >= 0.5))
# Kappa = 0.2716
roc.curve(test_arrest$is_arrested, model.log.smote >=0.5)
# -> 0.759
summary(log.smote)
# -> testing with smaller threshold
confusionMatrix(table(test_arrest$is_arrested, model.log.smote >= 0.3))
# Kappa = 0.2033 
confusionMatrix(table(test_arrest$is_arrested, model.log.smote >= 0.7))
# Kappa = 0.3063; higher kappa but lower is_arrested == TRUE correct

# with no imbalanced adjustment
log.blank <- glm(is_arrested ~ ., data = train_arrest, family = "binomial")
model.log.blank <- predict(log.blank, newdata = test_arrest, type = "response")
confusionMatrix(table(test_arrest$is_arrested, model.log.blank >= 0.04))
ROCRpred <- prediction(model.log.blank, test_arrest$is_arrested)
ROCRperf <- performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf)
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,0.1,by=0.01), text.adj=c(-0.2,1.7))
# -> Based on the ROC plot, since I want to maximize prediciting is_arrested ==
#    TRUE (TF) correctly, I'll accept a lower true positive rate. 
#    This makes a threshold of 0.04 a good value
roc.curve(test_arrest$is_arrested, model.log.smote >= 0.04)
# -> 0.503



# Using 10-fold CV to widdle down the top 5 performing algorithms
# -> CART with SMOTE
# -> RF with SMOTE
# -> RF with Over
# -> NB with SMOTE
# -> CART with Penalty Matrix


# CART with SMOTE
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

df_cv_results <- data.frame(cv_results)
str(cv_results)
#AUC
mean(unlist(df_cv_results[1,]))
# -> 0.7754418
#Kappa
mean(unlist(df_cv_results[2,]))
# -> 0.2234328




# RF wtih SMOTE
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

df_cv_results_rf <- data.frame(cv_results_rf)
#AUC
mean(unlist(df_cv_results_rf[1,]))
# -> 0.7688412
#Kappa
mean(unlist(df_cv_results_rf[2,]))
# -> 0.293108




#### TEST
# RF with Over
cv_results_rf_over <- lapply(folds, function(x) {
  arrest_train <- stops_arrested[-x, ]
  arrest_test <- stops_arrested[x, ]
  #.env <- environment()
  #environment(ovun.sample) <- environment()
  environment() <- environment()
  #num =  2*nrow(arrest_train %>% filter(is_arrested == FALSE))
  num = 2*nrow(arrest_train[arrest_train$is_arrested == FALSE,])
  #num = 550402
  arrest.train.over <- ovun.sample(is_arrested ~., data = arrest_train, method = "over", N = num)$data
  tree.model <- randomForest(is_arrested ~ ., data = arrest.train.over, ntree = 100)
  arrest.pred <- predict(tree.model, arrest_test, type = "class")
  auc_val <- roc.curve(arrest_test$is_arrested, arrest.pred)$auc
  kappa <- kappa2(data.frame(arrest_test$is_arrested, arrest.pred))$value
  return(c(auc_val, kappa))
})

df_cv_results_rf_over <- data.frame(cv_results_rf_over)
#AUC
mean(unlist(df_cv_results_rf_over[1,]))
# -> 
#Kappa
mean(unlist(df_cv_results_rf_over[2,]))
# -> 




# NB with SMOTE
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

df_cv_results_nb <- data.frame(cv_results_nb)
#AUC
mean(unlist(df_cv_results_nb[1,]))
# -> 0.7493672
#Kappa
mean(unlist(df_cv_results_nb[2,]))
# -> 0.2620019




# CART with Penalty Matrix
# PenaltyMatrix = matrix(c(0,1,6,0), byrow = TRUE, nrow = 2)
cv_resultsPM <- lapply(folds, function(x) {
  arrest_train <- stops_arrested[-x, ]
  arrest_test <- stops_arrested[x, ]
  tree.model <- rpart(is_arrested ~ ., data = arrest_train, parms = list(loss = PenaltyMatrix))
  arrest.pred <- predict(tree.model, arrest_test, type = "class")
  auc_val <- roc.curve(arrest_test$is_arrested, arrest.pred)$auc
  kappa <- kappa2(data.frame(arrest_test$is_arrested, arrest.pred))$value
  return(c(auc_val, kappa))
})

df_cv_resultsPM <- data.frame(cv_resultsPM)
#AUC
mean(unlist(df_cv_resultsPM[1,]))
# -> 0.7018223
#Kappa
mean(unlist(df_cv_resultsPM[2,]))
# -> 0.3649853




#### Using train() to fine tune parameters of best models
# -> Using test split from earlier as validation set

# Random Forest with SMOTE
ctrl_rf <- trainControl(method = "cv", number = 10, sampling = "smote")
grid_rf <- expand.grid(.mtry = c(3, 7, 14, 27))
set.seed(13)
m_rf <- train(is_arrested ~., data = train_arrest,
                method = "rf",
                metric = "Kappa",
                trControl = ctrl_rf,
                tuneGrid = grid_rf)
m_rf
# -> best mtry = 3

predict_rf <- predict(m_rf, newdata = test_arrest)
table(test_arrest$is_arrested, predict_rf)
#       predict_rf
#       FALSE  TRUE
# FALSE 73740  2704
# TRUE    838   987
kappa_rf <- kappa2(data.frame(test_arrest$is_arrested, predict_rf))$value
# -> 0.3371849



# CART with Penalty Matrix
# PenaltyMatrix = matrix(c(0,1,6,0), byrow = TRUE, nrow = 2)
ctrl_cart <- trainControl(method = "cv", number = 10)
grid_cart <- expand.grid(.cp = seq(0.1,1,0.1))
set.seed(13)
m_cart <- train(is_arrested ~., data = train_arrest,
                method = "rpart",
                metric = "Kappa",
                trControl = ctrl_cart,
                tuneGrid = grid_cart,
                parms = list(loss = PenaltyMatrix))
m_cart
# -> best cp is 0.1

predict_cart <- predict(m_cart, newdata = test_arrest)
table(test_arrest$is_arrested, predict_cart)
#       predict_cart
#       FALSE  TRUE
# FALSE 75571   873
# TRUE   1340   485
kappa_cart <- kappa2(data.frame(test_arrest$is_arrested, predict_cart))$value
# -> 0.2906303


# second attempt with different cp values
ctrl_cart2 <- trainControl(method = "cv", number = 10)
grid_cart2 <- expand.grid(.cp = seq(0.01,0.1,0.01))
set.seed(13)
m_cart2 <- train(is_arrested ~., data = train_arrest,
                method = "rpart",
                metric = "Kappa",
                trControl = ctrl_cart2,
                tuneGrid = grid_cart2,
                parms = list(loss = PenaltyMatrix))
m_cart2
# -> best cp is 0.01

predict_cart2 <- predict(m_cart2, newdata = test_arrest)
table(test_arrest$is_arrested, predict_cart2)
#       predict_cart2
#       FALSE  TRUE
# FALSE 74453  1991
# TRUE   1020   805
kappa_cart2 <- kappa2(data.frame(test_arrest$is_arrested, predict_cart2))$value
# -> 0.3294899



# 
# # Naive Bayes:
# ctrl_nb <- trainControl(method = "cv", number = 10, sampling = "smote")
# grid_nb <- expand.grid(.fL = c(0,1), .usekernel = c(TRUE, FALSE), .adjust = c(.1,1,10))
# set.seed(13)
# m_nb <- train(is_arrested ~., data = train_arrest_2,
#               method = "nb",
#               metric = "Kappa",
#               trControl = ctrl_nb,
#               tuneGrid = grid_nb)
# m_nb
# # -> best fL = 1
# # -> best usekernel = FALSE
# # best adjust = 1
# 
# predict_nb <- predict(m_nb, newdata = validate_arrest_2)
# table(validate_arrest_2$is_arrested, predict_nb)
# kappa_nb <- kappa2(data.frame(validate_arrest_2$is_arrested, predict_nb))$value
# # -> 0.282255





#### Using results (optimized parameters) of train() to build models

# RF with SMOTE
train_arrest_smote <- SMOTE(is_arrested ~., data = train_arrest)
model.smote.rf <- randomForest(is_arrested ~ ., data = train_arrest_smote, mtry = 3)
pred.rf.smote <- predict(model.smote.rf, newdata = test_arrest, type = "class")
confusionMatrix(table(test_arrest$is_arrested, pred.rf.smote))
# kappa = 0.2792
# Here are the important variables in the RF model
importance(model.smote.rf)
varImpPlot(model.smote.rf)
## Top 5:
#  -> stop_duration
#  -> search_conducted
#  -> search_type
#  -> contraband_found
#  -> violation_count

## Lowest 5:
#  -> violation_raw_Equipment.Violation
#  -> violation_raw_Traffic.Control.Signal
#  -> violation_raw_Window.Tint
#  -> violation_raw_Stop.Sign
#  -> violation_raw_Defective.Lights



# CART with Penalty Matrix
cart.penalty.ft <- rpart(is_arrested ~ ., data = train_arrest, cp = 0.01, parms = list(loss = PenaltyMatrix))
prp(cart.penalty.ft)
pred.cart.penalty.ft <- predict(cart.penalty.ft, newdata = test_arrest, type = "class")
confusionMatrix(table(test_arrest$is_arrested, pred.cart.penalty.ft))
# Kappa = 0.3537





# ==> best model (based on Kappa) is shown to be CART with the Penalty Matrix,
#     to handle imbalanced data.




#### Feature selecting
control_fs <- rfeControl(functions = rfFuncs,
                         method = "cv",
                         number = 10)
feature_selection_arrest <- rfe(is_arrested ~., 
                                data = train_arrest,
                                rfeControl = control_fs,
                                ntree = 100,
                                parms = list(loss = PenaltyMatrix))

feature_selection_arrest


#-----------------------------------------------------------------------
### Investigating demographics and arrest status


# Since want to see if driver characteristics effect arrest status, let's remove
# some features that don't pertain to the driver
arrest_driver <- stops_arrested[,c(2:4,8)]

# Split
set.seed(1993)
spl_driver = createDataPartition(arrest_driver$is_arrested, p = 0.75, list = FALSE)
train_arrest_driver = arrest_driver[spl_driver,]
test_arrest_driver = arrest_driver[-spl_driver,]

# CART with SMOTE
train.driver.smote <- SMOTE(is_arrested ~., data = train_arrest_driver)
cart.driver.smote <- rpart(is_arrested ~ ., data = train.driver.smote)
prp(cart.driver.smote)
pred.cart.driver.smote <- predict(cart.driver.smote, newdata = test_arrest_driver, type = "class")
confusionMatrix(table(test_arrest_driver$is_arrested, pred.cart.driver.smote))
# Kappa = 0.0177
roc.curve(test_arrest_driver$is_arrested, pred.cart.driver.smote)
# -> 0.545


# CART with ROSE
train.driver.rose <- ROSE(is_arrested ~ ., data = test_arrest_driver, seed = 1)$data
cart.driver.rose <- rpart(is_arrested ~ ., data = train.driver.rose)
prp(cart.driver.rose)
pred.cart.driver.rose <- predict(cart.driver.rose, newdata = test_arrest_driver, type = "class")
confusionMatrix(table(test_arrest_driver$is_arrested, pred.cart.driver.rose))
# Kappa = 0.0161
roc.curve(test_arrest_driver$is_arrested, pred.cart.driver.rose)
# -> 0.573


# CART with Under-Sampling
table(train_arrest_driver$is_arrested)
train.driver.under <- ovun.sample(is_arrested ~., data = train_arrest_driver, method = "under", N = 2*5475)$data
cart.driver.under <- rpart(is_arrested ~ ., data = train.driver.under)
prp(cart.driver.under)
pred.cart.driver.under <- predict(cart.driver.under, newdata = test_arrest_driver, type = "class")
confusionMatrix(table(test_arrest_driver$is_arrested, pred.cart.driver.under))
# Kappa = 0.0177
roc.curve(test_arrest_driver$is_arrested, pred.cart.driver.under)
# -> 0.573


# CART with Over-Sampling
train.driver.over <- ovun.sample(is_arrested ~., data = train_arrest_driver, method = "over", N = 2*229335)$data
cart.driver.over <- rpart(is_arrested ~ ., data = train.driver.over)
prp(cart.driver.over)
pred.cart.driver.over <- predict(cart.driver.over, newdata = test_arrest_driver, type = "class")
confusionMatrix(table(test_arrest_driver$is_arrested, pred.cart.driver.over))
# Kappa = 0.0177
roc.curve(test_arrest_driver$is_arrested, pred.cart.driver.over)
# -> 0.573


# CART with Penalty Matrix
#PenaltyMatrix = matrix(c(0,1,6,0), byrow = TRUE, nrow = 2)
PenaltyMatrix2 = matrix(c(0,1,50,0), byrow = TRUE, nrow = 2)
arrest_driver_cart_penalty <- rpart(is_arrested ~., data = train_arrest_driver, parms = list(loss = PenaltyMatrix2))
prp(arrest_driver_cart_penalty)
PredictCART_driverPenalty = predict(arrest_driver_cart_penalty, newdata = test_arrest_driver, type = "class")
confusionMatrix(table(test_arrest_driver$is_arrested, PredictCART_driverPenalty))
# Kappa = 0.0075
roc.curve(test_arrest_driver$is_arrested, PredictCART_driverPenalty)
# -> 0.554



# RF with SMOTE
rf.driver.smote <- randomForest(is_arrested ~ ., data = train.driver.smote)
pred.rf.driver.smote <- predict(rf.driver.smote, newdata = test_arrest_driver, type = "class")
confusionMatrix(table(test_arrest_driver$is_arrested, pred.rf.driver.smote))
# Kappa = 0.0193
roc.curve(test_arrest_driver$is_arrested, pred.rf.driver.smote)
# -> 0.547



# RF with Penalty Matrix
#PenaltyMatrix2 = matrix(c(0,1,50,0), byrow = TRUE, nrow = 2)
arrest_driver_rf_penalty <- randomForest(is_arrested ~., data = train_arrest_driver, ntree = 100, parms = list(loss = PenaltyMatrix2))
PredictRF_driverPenalty = predict(arrest_driver_rf_penalty, newdata = test_arrest_driver, type = "class")
confusionMatrix(table(test_arrest_driver$is_arrested, PredictRF_driverPenalty))
# Kappa = 0
roc.curve(test_arrest_driver$is_arrested, PredictRF_driverPenalty)
# -> 0.500



# NB with SMOTE
nb.driver.smote <- naiveBayes(is_arrested ~ ., data = train.driver.smote, laplace = 1)
pred.nb.driver.smote <- predict(nb.driver.smote, newdata = test_arrest_driver, type = "class")
confusionMatrix(table(test_arrest_driver$is_arrested, pred.nb.driver.smote))
# Kappa = 0.0178
roc.curve(test_arrest_driver$is_arrested, pred.nb.driver.smote)
# -> 0.542



# Logistic Regression
log.demo <- glm(is_arrested ~ ., data = train_arrest_driver, family = "binomial")
model.log.demo <- predict(log.demo, newdata = test_arrest_driver, type = "response")
confusionMatrix(table(test_arrest_driver$is_arrested, model.log.demo >= 0.01))
# Kappa = 0
roc.curve(test_arrest_driver$is_arrested, model.log.demo >=0.01)
# -> 0.500
summary(log.demo)
# => only certain thresholds bucket probs into TRUE--- not a good model



### => None of the algorithms returned great predictors indicating that 
###    demographics aren't the best way to determine arrest status.


#---------------------------------------------------------------------------
### Removing important variables to see if can improve models

## Top 5 from fine-tuned RF with SMOTE:
#  -> stop_duration
#  -> search_conducted
#  -> search_type
#  -> contraband_found
#  -> violation_count

### Removing stop_duration
stops_arrested2 <- stops_arrested[,-9]

set.seed(3456)
spl2 = createDataPartition(stops_arrested2$is_arrested, p = 0.75, list = FALSE)
train_arrest2 = stops_arrested2[spl2,]
test_arrest2 = stops_arrested2[-spl2,]

# RF with SMOTE
train.smote2 <- SMOTE(is_arrested ~., data = train_arrest2)
rf.smote2 <- randomForest(is_arrested ~ ., data = train.smote2)
pred.rf.smote2 <- predict(rf.smote2, newdata = test_arrest2, type = "class")
confusionMatrix(table(test_arrest2$is_arrested, pred.rf.smote2))
# Kappa = 0.2041
roc.curve(test_arrest2$is_arrested, pred.rf.smote2)
# -> 0.640



### Removing stop_duration and search_conducted
stops_arrested3 <- stops_arrested[, c(-5, -9)]

set.seed(3456)
spl3 = createDataPartition(stops_arrested3$is_arrested, p = 0.75, list = FALSE)
train_arrest3 = stops_arrested3[spl3,]
test_arrest3 = stops_arrested3[-spl3,]

# RF with SMOTE
train.smote3 <- SMOTE(is_arrested ~., data = train_arrest3)
rf.smote3 <- randomForest(is_arrested ~ ., data = train.smote3)
pred.rf.smote3 <- predict(rf.smote3, newdata = test_arrest3, type = "class")
confusionMatrix(table(test_arrest3$is_arrested, pred.rf.smote3))
# Kappa = 0.19
roc.curve(test_arrest3$is_arrested, pred.rf.smote3)
# -> 0.652



### Removing stop_duration, search_conducted, and search_type
stops_arrested4 <- stops_arrested[, c(-5, -6, -9)]

set.seed(3456)
spl4 = createDataPartition(stops_arrested4$is_arrested, p = 0.75, list = FALSE)
train_arrest4 = stops_arrested4[spl4,]
test_arrest4 = stops_arrested4[-spl4,]

# RF with SMOTE
train.smote4 <- SMOTE(is_arrested ~., data = train_arrest4)
rf.smote4 <- randomForest(is_arrested ~ ., data = train.smote4)
pred.rf.smote4 <- predict(rf.smote4, newdata = test_arrest4, type = "class")
confusionMatrix(table(test_arrest4$is_arrested, pred.rf.smote4))
# Kappa = 0.1392
roc.curve(test_arrest4$is_arrested, pred.rf.smote4)
# -> 0.630



### Removing stop_duration, search_conducted, search_type, and contraband_found
stops_arrested5 <- stops_arrested[, c(-5, -6, -7, -9)]

set.seed(3456)
spl5 = createDataPartition(stops_arrested5$is_arrested, p = 0.75, list = FALSE)
train_arrest5 = stops_arrested5[spl5,]
test_arrest5 = stops_arrested5[-spl5,]

# RF with SMOTE
train.smote5 <- SMOTE(is_arrested ~., data = train_arrest5)
rf.smote5 <- randomForest(is_arrested ~ ., data = train.smote5)
pred.rf.smote5 <- predict(rf.smote5, newdata = test_arrest5, type = "class")
confusionMatrix(table(test_arrest5$is_arrested, pred.rf.smote5))
# Kappa = 0.1142
roc.curve(test_arrest5$is_arrested, pred.rf.smote5)
# -> 0.603



### Removing stop_duration, search_conducted, search_type, contraband_found, and violation_count
stops_arrested6 <- stops_arrested[, c(-5, -6, -7, -9, -10)]

set.seed(3456)
spl6 = createDataPartition(stops_arrested6$is_arrested, p = 0.75, list = FALSE)
train_arrest6 = stops_arrested6[spl6,]
test_arrest6 = stops_arrested6[-spl6,]

# RF with SMOTE
train.smote6 <- SMOTE(is_arrested ~., data = train_arrest6)
rf.smote6 <- randomForest(is_arrested ~ ., data = train.smote6)
pred.rf.smote6 <- predict(rf.smote6, newdata = test_arrest6, type = "class")
confusionMatrix(table(test_arrest6$is_arrested, pred.rf.smote6))
# Kappa = 0.08
roc.curve(test_arrest6$is_arrested, pred.rf.smote6)
# -> 0.635


### => Kappa and AUC decrease, indicating this is not a good way to improve the model





#----------------------------------------------------------------------------
### => Removing the important variables seemed to decrease performance

### Let's remove some of the least important variables
## Lowest 5:
#  -> violation_raw_Equipment.Violation
#  -> violation_raw_Traffic.Control.Signal
#  -> violation_raw_Window.Tint
#  -> violation_raw_Stop.Sign
#  -> violation_raw_Defective.Lights


### Removing violation_raw_Equipment.Violation
stops_arrested7 <- stops_arrested[,-14]

set.seed(3456)
spl7 = createDataPartition(stops_arrested7$is_arrested, p = 0.75, list = FALSE)
train_arrest7 = stops_arrested7[spl7,]
test_arrest7 = stops_arrested7[-spl7,]

# RF with SMOTE
train.smote7 <- SMOTE(is_arrested ~., data = train_arrest7)
rf.smote7 <- randomForest(is_arrested ~ ., data = train.smote7)
pred.rf.smote7 <- predict(rf.smote7, newdata = test_arrest7, type = "class")
confusionMatrix(table(test_arrest7$is_arrested, pred.rf.smote7))
# Kappa = 0.2876
roc.curve(test_arrest7$is_arrested, pred.rf.smote7)
# -> 0.764


### Removing violation_raw_Equipment.Violation and violation_raw_Traffic.Control.Signal
stops_arrested8 <- stops_arrested[, c(-14,-23)]

set.seed(3456)
spl8 = createDataPartition(stops_arrested8$is_arrested, p = 0.75, list = FALSE)
train_arrest8 = stops_arrested8[spl8,]
test_arrest8 = stops_arrested8[-spl8,]

# RF with SMOTE
train.smote8 <- SMOTE(is_arrested ~., data = train_arrest8)
rf.smote8 <- randomForest(is_arrested ~ ., data = train.smote8)
pred.rf.smote8 <- predict(rf.smote8, newdata = test_arrest8, type = "class")
confusionMatrix(table(test_arrest8$is_arrested, pred.rf.smote8))
# Kappa = 0.2829
roc.curve(test_arrest8$is_arrested, pred.rf.smote8)
# -> 0.767


### Removing violation_raw_Equipment.Violation, violation_raw_Traffic.Control.Signal, 
#   violation_raw_Window.Tint, violation_raw_Stop.Sign, and 
#   violation_raw_Defective.Lights
stops_arrested9 <- stops_arrested[, c(-12, -14, -21, -23, -24)]

set.seed(3456)
spl9 = createDataPartition(stops_arrested9$is_arrested, p = 0.75, list = FALSE)
train_arrest9 = stops_arrested9[spl9,]
test_arrest9 = stops_arrested9[-spl9,]

# RF with SMOTE
train.smote9 <- SMOTE(is_arrested ~., data = train_arrest9)
rf.smote9 <- randomForest(is_arrested ~ ., data = train.smote9)
pred.rf.smote9 <- predict(rf.smote9, newdata = test_arrest9, type = "class")
confusionMatrix(table(test_arrest9$is_arrested, pred.rf.smote9))
# Kappa = 0.288
roc.curve(test_arrest9$is_arrested, pred.rf.smote9)
# -> 0.769


# Removing more "violation_raw" features:
# violation_raw_Equipment.Violation, violation_raw_Traffic.Control.Signal, 
# violation_raw_Window.Tint, violation_raw_Stop.Sign, violation_raw_Defective.Lights,
# violation_raw_Seatbelt, violaiton_raw_Display.of.Plates, and 
# violation_raw_Cell.Phones (based on RF importance plot)
stops_arrested10 <- stops_arrested[, c(-11, -12, -13, -14, -19, -21, -23, -24)]

set.seed(3456)
spl10 = createDataPartition(stops_arrested10$is_arrested, p = 0.75, list = FALSE)
train_arrest10 = stops_arrested10[spl10,]
test_arrest10 = stops_arrested10[-spl10,]

# RF with SMOTE
train.smote10 <- SMOTE(is_arrested ~., data = train_arrest10)
rf.smote10 <- randomForest(is_arrested ~ ., data = train.smote10)
pred.rf.smote10 <- predict(rf.smote10, newdata = test_arrest10, type = "class")
confusionMatrix(table(test_arrest10$is_arrested, pred.rf.smote10))
# Kappa = 0.2901
roc.curve(test_arrest10$is_arrested, pred.rf.smote10)
# -> 0.767



# Removing all "violation_raw" features
stops_arrested11 <- stops_arrested[, c(-11:-24)]

set.seed(3456)
spl11 = createDataPartition(stops_arrested11$is_arrested, p = 0.75, list = FALSE)
train_arrest11 = stops_arrested11[spl11,]
test_arrest11 = stops_arrested11[-spl11,]

# RF with SMOTE
train.smote11 <- SMOTE(is_arrested ~., data = train_arrest11)
rf.smote11 <- randomForest(is_arrested ~ ., data = train.smote11)
pred.rf.smote11 <- predict(rf.smote11, newdata = test_arrest11, type = "class")
confusionMatrix(table(test_arrest11$is_arrested, pred.rf.smote11))
# Kappa = 0.2653 
roc.curve(test_arrest11$is_arrested, pred.rf.smote11)
# -> 0.763


# => That didn't make it better. Let's remove the bottom 10 features, which includes
#    driver_gender and stop_dom
stops_arrested12 <- stops_arrested[, c(-2, -11, -12, -13, -14, -19, -21, -23, -24, -27)]

set.seed(3456)
spl12 = createDataPartition(stops_arrested12$is_arrested, p = 0.75, list = FALSE)
train_arrest12 = stops_arrested12[spl12,]
test_arrest12 = stops_arrested12[-spl12,]

# RF with SMOTE
train.smote12 <- SMOTE(is_arrested ~., data = train_arrest12)
rf.smote12 <- randomForest(is_arrested ~ ., data = train.smote12)
pred.rf.smote12 <- predict(rf.smote12, newdata = test_arrest12, type = "class")
confusionMatrix(table(test_arrest12$is_arrested, pred.rf.smote12))
# Kappa = 0.2815
roc.curve(test_arrest12$is_arrested, pred.rf.smote12)
# -> 0.767




### => The change from removing the lowest important variables didn't change
###    the performance much at all.





#----------------------------------------------------------
#### stop_outcome



### In an attempt to avoid imbalanced data and look more holistically at stop outcome,
### let's look past just arrests.

# Splitting stops_so$stop_outcome into "good" and "bad" outcomes
stops_so2 <- stops_so
stops_so2$stop_outcome_bucket <- vector(mode = "character", length = nrow(stops_so))
stops_so2$stop_outcome_bucket[stops_so2$stop_outcome == "Arrest" | 
                              stops_so2$stop_outcome == "Summons" |
                              stops_so2$stop_outcome == "Ticket" ] <- "Bad"
stops_so2$stop_outcome_bucket[stops_so2$stop_outcome == "Verbal Warning" |
                              stops_so2$stop_outcome == "Written Warning" ] <- "Good"

stops_so2 <- stops_so2[,-8]
str(stops_so2)
stops_so2$stop_outcome_bucket <- factor(stops_so2$stop_outcome_bucket)

# Splitting the data
set.seed(3030)
spl_so2 = createDataPartition(stops_so2$stop_outcome_bucket, p = 0.75, list = FALSE)
train_so2 = stops_so2[spl_so2,]
test_so2 = stops_so2[-spl_so2,]


# CART
stops_so2_cart <- rpart(stop_outcome_bucket ~., data = train_so2, method = "class", minbucket = 100)
prp(stops_so2_cart)
print(stops_so2_cart)
PredictCART_so2 = predict(stops_so2_cart, newdata = test_so2, type = "class")
confusionMatrix(table(test_so2$stop_outcome_bucket, PredictCART_so2))
# Kappa = 0.1425
roc.curve(test_so2$stop_outcome_bucket, PredictCART_so2)
# AUC = 0.551



#RF
rf.so2 <- randomForest(stop_outcome_bucket ~ ., data = train_so2, ntree = 50)
pred.rf.so2 <- predict(rf.so2, newdata = test_so2, type = "class")
confusionMatrix(table(test_so2$stop_outcome_bucket, pred.rf.so2))
# Kappa = 0.2711
roc.curve(test_so2$stop_outcome_bucket, pred.rf.so2)
# -> 0.605


# => Still doesn't perform great. Will SMOTE help?


#CART with SMOTE
train.smote.so2 <- SMOTE(stop_outcome_bucket ~., data = train_so2)
stops_so2_cart_smote <- rpart(stop_outcome_bucket ~., data = train.smote.so2, method = "class", minbucket = 100)
prp(stops_so2_cart_smote)
PredictCART_so2_smote = predict(stops_so2_cart_smote, newdata = test_so2, type = "class")
confusionMatrix(table(test_so2$stop_outcome_bucket, PredictCART_so2_smote))
# Kappa = 0.1686
roc.curve(test_so2$stop_outcome_bucket, PredictCART_so2_smote)
# AUC = 0.564

# => SMOTE doesn't help.




#-------------------------------------------------------------------------

##### Work in progress...

# Clustering to see if I can identify any trends for high factor variables
# https://www.r-bloggers.com/clustering-mixed-data-types-in-r/

library(cluster) 
stops_arrest_cluster <- stops_arrest
stops_arrest_cluster$search_conducted <- as.factor(stops_arrest_cluster$search_conducted)
stops_arrest_cluster$contraband_found <- as.factor(stops_arrest_cluster$contraband_found)
stops_arrest_cluster$stop_date <- as.factor(stops_arrest_cluster$stop_date)
#removing stop_date, stop_time, and officerID because of size/memory alocation
stops_arrest_cluster2 <- stops_arrest_cluster[, c(-1,-2,-12, -14, -15, -16)]
#str(stops_arrest_cluster)
stops_arrest_custer3 <- stops_arrest_cluster2[,-3]

gower_dist <- daisy(stops_arrest_custer3[,-8],
                    metric = "gower")
#...can't get it to work, error: "Error: cannot allocate vector of size 378.3 Gb"
