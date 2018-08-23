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
library(Boruta)
library(caretEnsemble)
library(gbm)

# Capstone data
# stops <- read.csv("E:/Learning/Springboard Intro to Data Science/capstone/CT_cleaned_edit.csv")
# str(stops)

#Using the split dataframe because it one-hot encoded violation_raw
stops_split <- read.csv("E:/Learning/Springboard Intro to Data Science/capstone/CT_cleaned_split.csv")

stops_split_col <- stops_split[,c(3,4,6,10,12,13,15,17,18,20,21,22,23,24,27,29:42)]
stops_split_col$stop_date <- as.POSIXct(stops_split_col$stop_date, "%Y-%m-%d", tz = "America/New_York")

# Making new attributes for ML algorithms
stops_split_edit <- stops_split_col

# making hour attribute
stops_split_edit$stop_time_hour <- stops_split_edit$stop_time
stops_split_edit$stop_time_hour[stops_split_edit$stop_time_hour == "0:00"] <- NA
stops_split_edit$stop_time_hour <- sub(":.*", "", stops_split_edit$stop_time_hour)

# grouping hour into times of day since 24 levels might be too granular and
# some machine learning algorithms can't handle high-level factors.
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

# ->breaking of day of month in rough thirds to match stats investigation
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
# Also remove Officer ID since we don't want to create an alogrithm with an ID column
# Removing original attributes that groups/aggregated:
  # -> stop_time
  # -> stop_date
  # -> violation_raw
stops_split_small <- stops_split_edit[, c(-1,-2, -7, -13, -30, -31, -33, -34, -36, -37)]
stops_split_small_clustering <- stops_split_edit[, c(-1,-2, -13, -30, -31, -33, -34, -36, -37)]
# (for clustering-- at the bottom of the script because tested and didn't provide
#                   better performance) 

# -> making the right data type, i.e. factors
for (i in c(5, 7, 9, 11:25)){
  stops_split_small[[i]] <- as.factor(stops_split_small[[i]])
}
str(stops_split_small)

for (i in c(6, 8, 10:26)){
  stops_split_small_clustering[[i]] <- as.factor(stops_split_small_clustering[[i]])
}
str(stops_split_small_clustering)


# NAs
  # -> county_name
  # -> driver_age
  # -> search_type_raw
  # -> stop_hour_part_of_day
  # -> stop_outcome
  # -> is_arrested
summary(stops_split_small)

stops_clean <- stops_split_small

#driver_age
# Will populate age NAs with median per investigate in capstone_data_wrangling.R
med <- median(stops_clean$driver_age, na.rm = TRUE)
stops_clean$driver_age[is.na(stops_clean$driver_age) == TRUE] <- med


#search_type_raw
# -> replacing blanks with NAs
stops_clean$search_type_raw[stops_clean$search_type_raw == ""] <- NA
table(stops_clean$search_type_raw, stops_clean$search_conducted, useNA = "always")
# -> 313337 are from search_conducted = FALSE
# -> 486 where search_conducted = TRUE but no search type indicated
# adding level to factor to make a "None" level for the search types where no search was conducted
# and removing the "blank" level
stops_clean$search_type_raw <- factor(stops_clean$search_type_raw, levels = c(levels(stops_clean$search_type_raw)[2:4], "None"))
stops_clean$search_type_raw[(is.na(stops_clean$search_type_raw) == TRUE & stops_clean$search_conducted == FALSE)] <- "None"


table(stops_clean$search_type_raw, stops_clean$is_arrested, useNA = "always")
# 151/7312  = 0.02065098 are to is_arrested == TRUE, so can't remove with out 
#                        significant loss of information
# => will identify as Other since that is the mode of is_arrested = TRUE and a 
#    generic "catch all" level
stops_clean$search_type_raw[(is.na(stops_clean$search_type_raw) == TRUE)] <- "Other"


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

# How are NAs distributed throughout is_arrested?
table(stops_clean2$is_arrested, useNA = "always")
# 5352 NAs 
# Since we can't tell if our prediction is right for outcomes with NAs, we'll
# remove the rows where is_arrested = NA

stops_clean2 <- stops_clean2[!is.na(stops_clean2$is_arrested),]
summary(stops_clean2)
str(stops_clean2)
stops_clean3 <- stops_clean2

#---------------------------------------------------------------------
# Algorithm testing time!

# Remove stop_outcome so can predict is_arrested accurately
stops_arrested <- stops_clean3[,-8]


# Split the data into test and training data sets
# -> since I have a lot of data, I want to make my testing sample a little larger
#    to have better tests to make sure I'm choosing the best algorithm

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
# => Just buckets all as is_arrested == TRUE because that has a good accuracy


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





#----------------------------------------------------------------
# Fixing imbalanced data

# Using SMOTE to fix imbalanced data to get better results from algorithms
set.seed(111)
train.smote <- SMOTE(is_arrested ~., data = train_arrest)
tree.smote <- rpart(is_arrested ~ ., data = train.smote)
prp(tree.smote)
# branched on:
# -> stop_duration
# -> search_type_raw
# -> search_conducted
# -> contraband_found


# predict on training set
pred.tree.smote <- predict(tree.smote, type = "class")
confusionMatrix(table(train.smote$is_arrested, pred.tree.smote))
# Kappa = 0.7738
roc.curve(train.smote$is_arrested, pred.tree.smote)
# -> 0.885
# AUC improved so using SMOTE is a better model thus far

# predict on test set
pred.tree.smote2 <- predict(tree.smote, newdata = test_arrest, type = "class")
confusionMatrix(table(test_arrest$is_arrested, pred.tree.smote2))
confusionMatrix(table(test_arrest$is_arrested, pred.tree.smote2))$overall['Kappa']
# Kappa = 0.2214208
roc.curve(test_arrest$is_arrested, pred.tree.smote2)
# -> 0.770



# ROSE as a sampler
set.seed(111)
train.rose <- ROSE(is_arrested ~ ., data = train_arrest, seed = 1)$data
tree.rose <- rpart(is_arrested ~ ., data = train.rose)
prp(tree.rose)
# -> stop_duration
# -> search_conducted

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
# FALSE   TRUE 
# 229335   5475
set.seed(111)
train_arrest_under <- ovun.sample(is_arrested ~., data = train_arrest, method = "under", N = 2*5475)$data
tree.under <- rpart(is_arrested ~ ., data = train_arrest_under)
pred.tree.under <- predict(tree.under, newdata = test_arrest, type = "class")
confusionMatrix(table(test_arrest$is_arrested, pred.tree.under))
# Kappa = 0.2214
roc.curve(test_arrest$is_arrested, pred.tree.under)
# -> 0.770



# Over-sampling as a sampler
set.seed(111)
train_arrest_over <- ovun.sample(is_arrested ~., data = train_arrest, method = "over", N = 2*229335)$data
tree.over <- rpart(is_arrested ~ ., data = train_arrest_over)
pred.tree.over <- predict(tree.over, newdata = test_arrest, type = "class")
confusionMatrix(table(test_arrest$is_arrested, pred.tree.over))
# Kappa = 0.2214
roc.curve(test_arrest$is_arrested, pred.tree.over)
# -> 0.770



### Seeing if RandomForest can improve model
set.seed(111)
rf.smote <- randomForest(is_arrested ~ ., data = train.smote)
pred.rf.smote <- predict(rf.smote, type = "class")
confusionMatrix(table(train.smote$is_arrested, pred.rf.smote))
# Kappa = 0.8025
roc.curve(train.smote$is_arrested, pred.rf.smote)
# -> 0.897

pred.rf.smote2 <- predict(rf.smote, newdata = test_arrest, type = "class")
confusionMatrix(table(test_arrest$is_arrested, pred.rf.smote2))$overall['Kappa']
# Kappa = 0.2812014
roc.curve(test_arrest$is_arrested, pred.rf.smote2)
# -> 0.762


# vizualizing tree; understand decision making in RF, one tree at a time
getTree(rf.smote)

#variable importance
randomForest::importance(rf.smote)
varImpPlot(rf.smote)
# -> we see that stop duration is the most important variable, aligning with
#    what we saw in the CART model
# -> search_type_raw, search_conducted, and contraband_found are also important variables,
#    also seen in the CART model
# -> stop_duration
# -> search_type_raw
# -> search_conducted
# -> contraband_found


# RF with ROSE data instead
set.seed(111)
rf.rose <- randomForest(is_arrested ~ ., data = train.rose, ntree = 100)
pred.rf.rose <- predict(rf.rose, newdata = test_arrest, type = "class")
confusionMatrix(table(test_arrest$is_arrested, pred.rf.rose))
# Kappa = 0.273
roc.curve(test_arrest$is_arrested, pred.rf.rose)
# -> 0.764



# RF with under-sampled data instead
set.seed(111)
rf.under <- randomForest(is_arrested ~ ., data = train_arrest_under)
pred.rf.under <- predict(rf.under, newdata = test_arrest, type = "class")
confusionMatrix(table(test_arrest$is_arrested, pred.rf.under))
# Kappa = 0.196
roc.curve(test_arrest$is_arrested, pred.rf.under)
# -> 0.780



# RF with over-sampled data instead
set.seed(111)
train_arrest_over <- ovun.sample(is_arrested ~., data = train_arrest, method = "over", N = 2*229335)$data
rf.over <- randomForest(is_arrested ~ ., data = train_arrest_over, ntree = 50)
pred.rf.over <- predict(rf.over, newdata = test_arrest, type = "class")
confusionMatrix(table(test_arrest$is_arrested, pred.rf.over))$overall['Kappa']
# Kappa = 0.322508
roc.curve(test_arrest$is_arrested, pred.rf.over)
# -> 0.732



###Naive Bayes
# with SMOTE
set.seed(111)
nb.model.smote <- naiveBayes(is_arrested ~., data = train.smote, laplace = 1)
nb.predict.smote <- predict(nb.model.smote, newdata = test_arrest, type = "class")
confusionMatrix(table(test_arrest$is_arrested, nb.predict.smote))$overall['Kappa']
# Kappa = 0.256684
roc.curve(test_arrest$is_arrested, nb.predict.smote)
# -> 0.744  



# with ROSE
set.seed(111)
nb.model.rose <- naiveBayes(is_arrested ~., data = train.rose, laplace = 1)
nb.predict.rose <- predict(nb.model.rose, newdata = test_arrest, type = "class")
confusionMatrix(table(test_arrest$is_arrested, nb.predict.rose))$overall['Kappa']
# Kappa = 0.1741055
roc.curve(test_arrest$is_arrested, nb.predict.rose)
# -> 0.769


# with Under-sampling
set.seed(111)
nb.model.under <- naiveBayes(is_arrested ~., data = train_arrest_under, laplace = 1)
nb.predict.under <- predict(nb.model.under, newdata = test_arrest, type = "class")
confusionMatrix(table(test_arrest$is_arrested, nb.predict.under))$overall['Kappa']
# Kappa = 0.1760479  
roc.curve(test_arrest$is_arrested, nb.predict.under)
# -> 0.770


# with over-sampling
set.seed(111)
nb.model.over <- naiveBayes(is_arrested ~., data = train_arrest_over, laplace = 1)
nb.predict.over <- predict(nb.model.over, newdata = test_arrest, type = "class")
confusionMatrix(table(test_arrest$is_arrested, nb.predict.over))$overall['Kappa']
# Kappa = 0.1744117
roc.curve(test_arrest$is_arrested, nb.predict.over)
# -> 0.768



### Testing using a Penalty Matrix instead of balancing the unbalanced dataset
# -> Want higher penalty for false negative, i.e., where say no arrest when there
#    was. Want this because we need to accurately label arrests.
PenaltyMatrix = matrix(c(0,1,6,0), byrow = TRUE, nrow = 2)

# with CART
set.seed(111)
stops_arrest_tree_penalty <- rpart(is_arrested ~., data = train_arrest, parms = list(loss = PenaltyMatrix))
prp(stops_arrest_tree_penalty)
print(stops_arrest_tree_penalty)
# -> stop_duration
# -> violation_raw_Registration
# -> stop_hour_part_of_day
# -> violation_raw_Other
# -> violation_raw_Moving.Violation

PredictCARTPenalty = predict(stops_arrest_tree_penalty, type = "class")
confusionMatrix(table(train_arrest$is_arrested, PredictCARTPenalty))$overall['Kappa']
# Kappa = 0.3686228
roc.curve(train_arrest$is_arrested, PredictCARTPenalty)
# -> 0.705

PredictCARTPenalty2 = predict(stops_arrest_tree_penalty, newdata = test_arrest, type = "class")
confusionMatrix(table(test_arrest$is_arrested, PredictCARTPenalty2))$overall['Kappa']
# Kappa = 0.3537442
roc.curve(test_arrest$is_arrested, PredictCARTPenalty2)
# -> 0.693

# Using ROC to fine tune threshold
PredictCARTPenalty3 = predict(stops_arrest_tree_penalty, newdata = test_arrest, type = "p")
ROCRpred2 <- prediction(PredictCARTPenalty3[,2], test_arrest$is_arrested)
ROCRperf2 <- performance(ROCRpred2, "tpr", "fpr")
plot(ROCRperf2)
plot(ROCRperf2, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
plot(ROCRperf2, colorize=TRUE, print.cutoffs.at=seq(0,0.1,by=0.01), text.adj=c(-0.2,1.7))
confusionMatrix(table(test_arrest$is_arrested, PredictCARTPenalty3[,2] >= 0.1))
# Kappa = 0.3537
roc.curve(test_arrest$is_arrested, PredictCARTPenalty3[,2]>= 0.1)
# -> 0.693

# Generally want to pick threshold that is at the "elbow" of the ROC curve, which
# which is at around 0.065 here.
confusionMatrix(table(test_arrest$is_arrested, PredictCARTPenalty3[,2] >= 0.065))$overall['Kappa']
# -> 0.2849794
roc.curve(test_arrest$is_arrested, PredictCARTPenalty3[,2] >= 0.065)
# -> 0.720

# with RF
set.seed(111)
stops_arrest_rf_penalty <- randomForest(is_arrested ~., data = train_arrest, ntree= 100, parms = list(loss = PenaltyMatrix))
PredictRFPenalty = predict(stops_arrest_rf_penalty, newdata = test_arrest, type = "class")
confusionMatrix(table(test_arrest$is_arrested, PredictRFPenalty))$overall['Kappa']
# Kappa = 0.2803174 
roc.curve(test_arrest$is_arrested, PredictRFPenalty)
# -> 0.588


# with NB
set.seed(111)
stops_arrest_nb_penalty <- naiveBayes(is_arrested ~., data = train_arrest, laplace = 1, parms = list(loss = PenaltyMatrix))
PredictNBPenalty = predict(stops_arrest_nb_penalty, newdata = test_arrest, type = "class")
confusionMatrix(table(test_arrest$is_arrested, PredictNBPenalty))$overall['Kappa']
# Kappa = 0.2867085
roc.curve(test_arrest$is_arrested, PredictNBPenalty)
# -> 0.632



### Trying logistic regression model
# With SMOTE
set.seed(111)
log.smote <- glm(is_arrested ~ ., data = train.smote, family = "binomial")
model.log.smote <- predict(log.smote, newdata = test_arrest, type = "response")
confusionMatrix(table(test_arrest$is_arrested, model.log.smote >= 0.5))$overall['Kappa']
# Kappa = 0.2680036
roc.curve(test_arrest$is_arrested, model.log.smote >=0.5)
# -> 0.759
# summary(log.smote)

# -> testing with smaller threshold
ROCRpredglm <- prediction(model.log.smote, test_arrest$is_arrested)
ROCRperfglm <- performance(ROCRpredglm, "tpr", "fpr")
plot(ROCRperfglm)
plot(ROCRperfglm, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
confusionMatrix(table(test_arrest$is_arrested, model.log.smote >= 0.4))$overall['Kappa']
# Kappa = 0.2362598 
# original threshold is also around elbow
confusionMatrix(table(test_arrest$is_arrested, model.log.smote >= 0.7))
# Kappa = 0.3029; higher kappa but lower is_arrested == TRUE correct


# with no imbalanced adjustment
set.seed(111)
log.blank <- glm(is_arrested ~ ., data = train_arrest, family = "binomial")
model.log.blank <- predict(log.blank, newdata = test_arrest, type = "response")
confusionMatrix(table(test_arrest$is_arrested, model.log.blank >= 0.05))$overall['Kappa']
# -> 0.2726147
ROCRpred <- prediction(model.log.blank, test_arrest$is_arrested)
ROCRperf <- performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf)
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,0.1,by=0.01), text.adj=c(-0.2,1.7))
# -> Based on the ROC plot, since I want to maximize prediciting is_arrested ==
#    TRUE (TF) correctly, I'll accept a lower true positive rate. 
#    This makes a threshold of 0.05 a good value
roc.curve(test_arrest$is_arrested, model.log.smote >= 0.05)
# -> 0.524



#-----------------------------------------------------------------------
# Using 10-fold CV to widdle down the top 5 performing algorithms
# -> CART with SMOTE
# -> RF with SMOTE
# -> RF with Over
# -> NB with SMOTE
# -> CART with Penalty Matrix
# -> SMOTE with logisitc regression (extra)


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
set.seed(123)
cv_results_rf <- lapply(folds, function(x) {
  arrest_train <- stops_arrested[-x, ]
  arrest_test <- stops_arrested[x, ]
  arrest.train.smote <- SMOTE(is_arrested ~., data = arrest_train)
  tree.model <- randomForest(is_arrested ~ ., data = arrest.train.smote, ntree = 50)
  arrest.pred <- predict(tree.model, arrest_test, type = "class")
  auc_val <- roc.curve(arrest_test$is_arrested, arrest.pred)$auc
  kappa <- kappa2(data.frame(arrest_test$is_arrested, arrest.pred))$value
  return(c(auc_val, kappa))
})

df_cv_results_rf <- data.frame(cv_results_rf)
#AUC
mean(unlist(df_cv_results_rf[1,]))
# -> 0.7677302
#Kappa
mean(unlist(df_cv_results_rf[2,]))
# -> 0.2910813




# RF with Over- redo
# cv_results_rf_over <- lapply(folds, function(x) {
#   arrest_train <- stops_arrested[-x, ]
#   arrest_test <- stops_arrested[x, ]
#   #.env <- environment()
#   #environment(ovun.sample) <- environment()
#   #environment() <- environment()
#   #num =  2*nrow(arrest_train %>% filter(is_arrested == FALSE))
#   #num = 2*nrow(arrest_train[arrest_train$is_arrested == FALSE,])
#   #num = 550402
#   num = 550402
#   # => num = 2*0.9*nrow(stops_arrested[stops_arrested$is_arrested == FALSE,])
#   #arrest.train.over <- ovun.sample(is_arrested ~., data = arrest_train, method = "over", N = num)$data
#   arrest.train.over <- ovun.sample(is_arrested ~., data = stops_arrested[-x, ], method = "over", N = num)$data
#   tree.model <- randomForest(is_arrested ~ ., data = arrest.train.over, ntree = 100)
#   arrest.pred <- predict(tree.model, arrest_test, type = "class")
#   auc_val <- roc.curve(arrest_test$is_arrested, arrest.pred)$auc
#   kappa <- kappa2(data.frame(arrest_test$is_arrested, arrest.pred))$value
#   return(c(auc_val, kappa))
# })

# df_cv_results_rf_over <- data.frame(cv_results_rf_over)
# #AUC
# mean(unlist(df_cv_results_rf_over[1,]))
# # -> 
# #Kappa
# mean(unlist(df_cv_results_rf_over[2,]))
# # -> 

set.seed(123)
f1.train <- stops_arrested[-folds$Fold01, ]
f1.test <- stops_arrested[folds$Fold01, ]
f1.ovun <- ovun.sample(is_arrested ~., data = f1.train, method = "over", N = 2*nrow(f1.train[f1.train$is_arrested == FALSE,]))$data
f1.model <- randomForest(is_arrested ~ ., data = f1.ovun, ntree = 30)
f1.pred <- predict(f1.model, f1.test, type = "class")
f1_auc_val <- roc.curve(f1.test$is_arrested, f1.pred)$auc
f1_kappa <- kappa2(data.frame(f1.test$is_arrested, f1.pred))$value

f2.train <- stops_arrested[-folds$Fold02, ]
f2.test <- stops_arrested[folds$Fold02, ]
f2.ovun <- ovun.sample(is_arrested ~., data = f2.train, method = "over", N = 2*nrow(f2.train[f2.train$is_arrested == FALSE,]))$data
f2.model <- randomForest(is_arrested ~ ., data = f2.ovun, ntree = 30)
f2.pred <- predict(f2.model, f2.test, type = "class")
f2_auc_val <- roc.curve(f2.test$is_arrested, f2.pred)$auc
f2_kappa <- kappa2(data.frame(f2.test$is_arrested, f2.pred))$value

f3.train <- stops_arrested[-folds$Fold03, ]
f3.test <- stops_arrested[folds$Fold03, ]
f3.ovun <- ovun.sample(is_arrested ~., data = f3.train, method = "over", N = 2*nrow(f3.train[f3.train$is_arrested == FALSE,]))$data
f3.model <- randomForest(is_arrested ~ ., data = f3.ovun, ntree = 30)
f3.pred <- predict(f3.model, f3.test, type = "class")
f3_auc_val <- roc.curve(f3.test$is_arrested, f3.pred)$auc
f3_kappa <- kappa2(data.frame(f3.test$is_arrested, f3.pred))$value

f4.train <- stops_arrested[-folds$Fold04, ]
f4.test <- stops_arrested[folds$Fold04, ]
f4.ovun <- ovun.sample(is_arrested ~., data = f4.train, method = "over", N = 2*nrow(f4.train[f4.train$is_arrested == FALSE,]))$data
f4.model <- randomForest(is_arrested ~ ., data = f4.ovun, ntree = 30)
f4.pred <- predict(f4.model, f4.test, type = "class")
f4_auc_val <- roc.curve(f4.test$is_arrested, f4.pred)$auc
f4_kappa <- kappa2(data.frame(f4.test$is_arrested, f4.pred))$value

f5.train <- stops_arrested[-folds$Fold05, ]
f5.test <- stops_arrested[folds$Fold05, ]
f5.ovun <- ovun.sample(is_arrested ~., data = f5.train, method = "over", N = 2*nrow(f5.train[f5.train$is_arrested == FALSE,]))$data
f5.model <- randomForest(is_arrested ~ ., data = f5.ovun, ntree = 30)
f5.pred <- predict(f5.model, f5.test, type = "class")
f5_auc_val <- roc.curve(f5.test$is_arrested, f5.pred)$auc
f5_kappa <- kappa2(data.frame(f5.test$is_arrested, f5.pred))$value

f6.train <- stops_arrested[-folds$Fold06, ]
f6.test <- stops_arrested[folds$Fold06, ]
f6.ovun <- ovun.sample(is_arrested ~., data = f6.train, method = "over", N = 2*nrow(f6.train[f6.train$is_arrested == FALSE,]))$data
f6.model <- randomForest(is_arrested ~ ., data = f6.ovun, ntree = 30)
f6.pred <- predict(f6.model, f6.test, type = "class")
f6_auc_val <- roc.curve(f6.test$is_arrested, f6.pred)$auc
f6_kappa <- kappa2(data.frame(f6.test$is_arrested, f6.pred))$value

f7.train <- stops_arrested[-folds$Fold07, ]
f7.test <- stops_arrested[folds$Fold07, ]
f7.ovun <- ovun.sample(is_arrested ~., data = f7.train, method = "over", N = 2*nrow(f7.train[f7.train$is_arrested == FALSE,]))$data
f7.model <- randomForest(is_arrested ~ ., data = f7.ovun, ntree = 30)
f7.pred <- predict(f7.model, f7.test, type = "class")
f7_auc_val <- roc.curve(f7.test$is_arrested, f7.pred)$auc
f7_kappa <- kappa2(data.frame(f7.test$is_arrested, f7.pred))$value

f8.train <- stops_arrested[-folds$Fold08, ]
f8.test <- stops_arrested[folds$Fold08, ]
f8.ovun <- ovun.sample(is_arrested ~., data = f8.train, method = "over", N = 2*nrow(f8.train[f8.train$is_arrested == FALSE,]))$data
f8.model <- randomForest(is_arrested ~ ., data = f8.ovun, ntree = 30)
f8.pred <- predict(f8.model, f8.test, type = "class")
f8_auc_val <- roc.curve(f8.test$is_arrested, f8.pred)$auc
f8_kappa <- kappa2(data.frame(f8.test$is_arrested, f8.pred))$value

f9.train <- stops_arrested[-folds$Fold09, ]
f9.test <- stops_arrested[folds$Fold09, ]
f9.ovun <- ovun.sample(is_arrested ~., data = f9.train, method = "over", N = 2*nrow(f9.train[f9.train$is_arrested == FALSE,]))$data
f9.model <- randomForest(is_arrested ~ ., data = f9.ovun, ntree = 30)
f9.pred <- predict(f9.model, f9.test, type = "class")
f9_auc_val <- roc.curve(f9.test$is_arrested, f9.pred)$auc
f9_kappa <- kappa2(data.frame(f9.test$is_arrested, f9.pred))$value

f10.train <- stops_arrested[-folds$Fold10, ]
f10.test <- stops_arrested[folds$Fold10, ]
f10.ovun <- ovun.sample(is_arrested ~., data = f10.train, method = "over", N = 2*nrow(f10.train[f10.train$is_arrested == FALSE,]))$data
f10.model <- randomForest(is_arrested ~ ., data = f10.ovun, ntree = 30)
f10.pred <- predict(f10.model, f10.test, type = "class")
f10_auc_val <- roc.curve(f10.test$is_arrested, f10.pred)$auc
f10_kappa <- kappa2(data.frame(f10.test$is_arrested, f10.pred))$value


combined_auc <- c(f1_auc_val, f2_auc_val, f3_auc_val, f4_auc_val, f5_auc_val, f6_auc_val, f7_auc_val, f8_auc_val, f9_auc_val, f10_auc_val)
combined_kappa <- c(f1_kappa, f2_kappa, f3_kappa, f4_kappa, f5_kappa, f6_kappa, f7_kappa, f8_kappa, f9_kappa, f10_kappa)

ovun.rf.auc <- mean(combined_auc)
# -> 0.7341902
ovun.rf.kappa <- mean(combined_kappa)
# -> 0.2981418



# NB with SMOTE
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

df_cv_results_nb <- data.frame(cv_results_nb)
#AUC
mean(unlist(df_cv_results_nb[1,]))
# -> 0.749346
#Kappa
mean(unlist(df_cv_results_nb[2,]))
# -> 0.2618779




# CART with Penalty Matrix
# PenaltyMatrix = matrix(c(0,1,6,0), byrow = TRUE, nrow = 2)
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

df_cv_resultsPM <- data.frame(cv_resultsPM)
#AUC
mean(unlist(df_cv_resultsPM[1,]))
# -> 0.7018223
#Kappa
mean(unlist(df_cv_resultsPM[2,]))
# -> 0.3649853



# GLM with SMOTE
set.seed(123)
folds2 <- createFolds(stops_arrested$is_arrested, k = 5) #memory error with k = 10
glm_results <- lapply(folds2, function(x) {
  arrest_train <- stops_arrested[-x, ]
  arrest_test <- stops_arrested[x, ]
  arrest.train.smote <- SMOTE(is_arrested ~., data = arrest_train)
  glm.model <- glm(is_arrested ~ ., data = arrest.train.smote, family = "binomial")
  arrest.pred <- predict(glm.model, newdata = arrest_test, type = "response")
  auc_val <- roc.curve(arrest_test$is_arrested, arrest.pred>=0.5)$auc
  kappa <- kappa2(data.frame(arrest_test$is_arrested, arrest.pred>=0.5))$value
  return(c(auc_val, kappa))
})

df_glm_results <- data.frame(glm_results)
str(glm_results)
#AUC
mean(unlist(df_glm_results[1,]))
# -> 0.7676267
#Kappa
mean(unlist(df_glm_results[2,]))
# -> 0.1328235



#---------------------------------------------------------------------
#### Using train() to fine tune parameters of best models
# -> Using test split from earlier as validation set

# Random Forest with SMOTE
ctrl_rf <- trainControl(method = "cv", number = 10, sampling = "smote")
grid_rf <- expand.grid(.mtry = c(3, 7, 14, 27))
set.seed(13)
m_rf <- train(is_arrested ~., data = train_arrest,
                method = "rf",
                metric = "Kappa",
                ntree = 30,
                trControl = ctrl_rf,
                tuneGrid = grid_rf)
m_rf
# -> best mtry = 3

predict_rf <- predict(m_rf, newdata = test_arrest)
table(test_arrest$is_arrested, predict_rf)
#       predict_rf
#       FALSE  TRUE
# FALSE 73720  2724
# TRUE    836   989
kappa_rf <- kappa2(data.frame(test_arrest$is_arrested, predict_rf))$value
# -> 0.3274127
confusionMatrix(table(test_arrest$is_arrested, predict_rf))$overall['Kappa']
roc.curve(test_arrest$is_arrested, predict_rf)
#-> 0.742

# Adding mtry =2
ctrl_rf3 <- trainControl(method = "cv", number = 10, sampling = "smote")
grid_rf3 <- expand.grid(.mtry = c(2, 3, 7, 14, 27))
set.seed(13)
m_rf3 <- train(is_arrested ~., data = train_arrest,
              method = "rf",
              metric = "Kappa",
              ntree = 30,
              trControl = ctrl_rf3,
              tuneGrid = grid_rf3)
m_rf3
# -> best mtry = 3

predict_rf3 <- predict(m_rf3, newdata = test_arrest)
table(test_arrest$is_arrested, predict_rf3)
# predict_rf3
#       FALSE  TRUE
# FALSE 73182  3262
# TRUE    833   992
kappa_rf3 <- kappa2(data.frame(test_arrest$is_arrested, predict_rf3))$value
# -> 0.3036447
confusionMatrix(table(test_arrest$is_arrested, predict_rf3))$overall['Kappa']
roc.curve(test_arrest$is_arrested, predict_rf3)
#-> 0.750



# Random Forest with Over/Up-sampling
ctrl_rf2 <- trainControl(method = "cv", number = 10, sampling = "up")
grid_rf2 <- expand.grid(.mtry = c(3, 7, 14, 27))
set.seed(13)
m_rf2 <- train(is_arrested ~., data = train_arrest,
              method = "rf",
              metric = "Kappa",
              ntree = 20,
              trControl = ctrl_rf2,
              tuneGrid = grid_rf2)
m_rf2
# -> best mtry = 27

predict_rf2 <- predict(m_rf2, newdata = test_arrest)
table(test_arrest$is_arrested, predict_rf2)
#       predict_rf2
#       FALSE  TRUE
# FALSE 75324  1120
# TRUE   1218   607
kappa_rf2 <- kappa2(data.frame(test_arrest$is_arrested, predict_rf2))$value
# -> 0.3282244
confusionMatrix(table(test_arrest$is_arrested, predict_rf2))$overall['Kappa']
roc.curve(test_arrest$is_arrested, predict_rf2)
#-> 0.640



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
roc.curve(test_arrest$is_arrested, predict_cart)
# -> 0.627


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
roc.curve(test_arrest$is_arrested, predict_cart2)
# -> 0.708





#------------------------------------------------------------------------
#### Using results (optimized parameters) of train() to build models

# RF with SMOTE
set.seed(444)
train_arrest_smote <- SMOTE(is_arrested ~., data = train_arrest)
model.smote.rf <- randomForest(is_arrested ~ ., data = train_arrest_smote, mtry = 3)
pred.rf.smote <- predict(model.smote.rf, newdata = test_arrest, type = "class")
confusionMatrix(table(test_arrest$is_arrested, pred.rf.smote))$overall['Kappa']
# kappa = 0.2728642
roc.curve(test_arrest$is_arrested, pred.rf.smote)
# -> 0.764
# Here are the important variables in the RF model
#importance(model.smote.rf)
varImpPlot(model.smote.rf)
## Top 5:
#  -> stop_duration
#  -> search_conducted
#  -> search_type_raw
#  -> contraband_found
#  -> violation_count

## Lowest 5:
#  -> violation_raw_Equipment.Violation
#  -> violation_raw_Traffic.Control.Signal
#  -> violation_raw_Window.Tint
#  -> violation_raw_Stop.Sign
#  -> violation_raw_Defective.Lights


# RF with oversampling
#using over-sampling from before, train_arrest_over
set.seed(555)
model.over.rf <- randomForest(is_arrested ~ ., data = train_arrest_over, mtry = 27, ntree = 30)
pred.rf.over.finetuned <- predict(model.over.rf, newdata = test_arrest, type = "class")
confusionMatrix(table(test_arrest$is_arrested, pred.rf.over.finetuned))$overall['Kappa']
# kappa = 0.326788
roc.curve(test_arrest$is_arrested, pred.rf.over.finetuned)
# -> 0.633
# Here are the important variables in the RF model
varImpPlot(model.smote.rf)
## Top 5:
#  -> stop_duration
#  -> search_conducted
#  -> search_type_raw
#  -> contraband_found
#  -> violation_count



# CART with Penalty Matrix
cart.penalty.ft <- rpart(is_arrested ~ ., data = train_arrest, cp = 0.01, parms = list(loss = PenaltyMatrix))
prp(cart.penalty.ft)
pred.cart.penalty.ft <- predict(cart.penalty.ft, newdata = test_arrest, type = "class")
confusionMatrix(table(test_arrest$is_arrested, pred.cart.penalty.ft))$overall['Kappa']
# Kappa = 0.3537442
roc.curve(test_arrest$is_arrested, pred.cart.penalty.ft)
# -> 0.693
# Variables used:
# -> stop_duration
# -> stop_hour_part_of_day
# -> violation_raw_Other
# -> violation_raw_Moving.Violation

# ==> best model (based on Kappa) is shown to be Random Forest with SMOTE
#     to handle imbalanced data.






#---------------------------------------------------------------------------
#### Feature selecting

# Using caret package
# RFE is a Recursive Feature Elimination algorithm that uses Random Forest.
control_fs <- rfeControl(functions = rfFuncs,
                         method = "cv",
                         number = 10)
set.seed(444)
feature_selection_arrest <- rfe(is_arrested ~., 
                                data = train_arrest,
                                rfeControl = control_fs,
                                ntree = 100,
                                parms = list(loss = PenaltyMatrix))

feature_selection_arrest
# Recursive feature selection
# 
# Outer resampling method: Cross-Validated (10 fold) 
# 
# Resampling performance over subset size:
#   
#   Variables Accuracy    Kappa AccuracySD  KappaSD Selected
# 4   0.9767 0.003104  6.334e-05 0.009815         
# 8   0.9774 0.110597  1.810e-04 0.036141         
# 16   0.9788 0.255107  6.876e-04 0.025593         
# 52   0.9792 0.273453  3.606e-04 0.014116        *
#   
#   The top 5 variables (out of 52):
#   stop_duration30+ min, stop_duration16-30 min, stop_hour_part_of_daytime_block2, stop_hour_part_of_daytime_block3, violation_raw_Other1

#using Kappa
set.seed(444)
feature_selection_arrest2 <- rfe(is_arrested ~., 
                                     data = train_arrest,
                                     rfeControl = control_fs,
                                     metric = "Kappa",
                                     ntree = 100,
                                     parms = list(loss = PenaltyMatrix))

feature_selection_arrest2
# Recursive feature selection
# 
# Outer resampling method: Cross-Validated (10 fold) 
# 
# Resampling performance over subset size:
#   
#   Variables Accuracy    Kappa AccuracySD  KappaSD Selected
# 4   0.9767 0.003104  6.334e-05 0.009815         
# 8   0.9774 0.110597  1.810e-04 0.036141         
# 16   0.9788 0.255107  6.876e-04 0.025593         
# 52   0.9792 0.273453  3.606e-04 0.014116        *
#   
#   The top 5 variables (out of 52):
#   stop_duration30+ min, stop_duration16-30 min, stop_hour_part_of_daytime_block2, stop_hour_part_of_daytime_block3, violation_raw_Other1

control_fs2 <- rfeControl(functions = rfFuncs,
                         method = "cv",
                         number = 10)
set.seed(444)
feature_selection_arrest3 <- rfe(is_arrested ~., 
                                 data = train_arrest,
                                 rfeControl = control_fs2,
                                 metric = "Kappa",
                                 ntree = 100,
                                 sampling = "smote")

feature_selection_arrest3
# Recursive feature selection
# 
# Outer resampling method: Cross-Validated (10 fold) 
# 
# Resampling performance over subset size:
#   
#   Variables Accuracy    Kappa AccuracySD  KappaSD Selected
# 4   0.9767 0.003104  6.334e-05 0.009815         
# 8   0.9774 0.110597  1.810e-04 0.036141         
# 16   0.9788 0.255107  6.876e-04 0.025593         
# 52   0.9792 0.273453  3.606e-04 0.014116        *
#   
#   The top 5 variables (out of 52):
#   stop_duration30+ min, stop_duration16-30 min, stop_hour_part_of_daytime_block2, stop_hour_part_of_daytime_block3, violation_raw_Other1



#Using Boruta package
# Boruta is a feature selection wrapper algorithm around Random Forest
set.seed(4567)
boruta.train <- Boruta(is_arrested ~., data = train.smote, doTrace = 2)
print(boruta.train)
# Boruta performed 99 iterations in 3.400914 hours.
# 25 attributes confirmed important: contraband_found, county_name,
# day_of_week, driver_age, driver_gender and 20 more;
# 1 attributes confirmed unimportant: violation_raw_Equipment.Violation;
# 1 tentative attributes left: stop_dom;
getSelectedAttributes(boruta.train, withTentative = F)
# [1] "county_name"                          "driver_gender"                       
# [3] "driver_age"                           "driver_race_raw"                     
# [5] "search_conducted"                     "search_type_raw"                     
# [7] "contraband_found"                     "stop_duration"                       
# [9] "violation_count"                      "violation_raw_Cell.Phone"            
# [11] "violation_raw_Defective.Lights"       "violation_raw_Display.of.Plates"     
# [13] "violation_raw_Moving.Violation"       "violation_raw_Other"                 
# [15] "violation_raw_Other.Error"            "violation_raw_Registration"          
# [17] "violation_raw_Seatbelt"               "violation_raw_Speed.Related"         
# [19] "violation_raw_Stop.Sign"              "violation_raw_Suspended.License"     
# [21] "violation_raw_Traffic.Control.Signal" "violation_raw_Window.Tint"           
# [23] "stop_hour_part_of_day"                "stop_season"                         
# [25] "day_of_week"     

jpeg('boruta_plot.jpg')
plot(boruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
  boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
       at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)
dev.off()
# Top five important variables:
# -> violation_count
# -> contraband_found
# -> search_type_raw
# -> search_conducted
# -> stop_duration

# High run-time; didn't finish
# set.seed(4567)
# boruta.train2 <- Boruta(is_arrested ~., data = train_arrest, doTrace = 2)
# print(boruta.train2)
# 
# getSelectedAttributes(boruta.train2, withTentative = F)
# 
# jpeg('boruta_plot2.jpg')
# plot(boruta.train2, xlab = "", xaxt = "n")
# lz<-lapply(1:ncol(boruta.train2$ImpHistory),function(i)
#   boruta.train2$ImpHistory[is.finite(boruta.train2$ImpHistory[,i]),i])
# names(lz) <- colnames(boruta.train2$ImpHistory)
# Labels <- sort(sapply(lz,median))
# axis(side = 1,las=2,labels = names(Labels),
#      at = 1:ncol(boruta.train2$ImpHistory), cex.axis = 0.7)
# dev.off()





#-----------------------------------------------------------------------
# Based on the results of the machine learning algorithms and some feature 
# selection, some top variables are:
# -> violation_count
# -> contraband_found
# -> search_type_raw
# -> search_conducted
# -> stop_duration

### Running CART with Penalty Matrix (one of the top performing algorithms) 
### only on dataset with top 5 variables
stops_arrested_top <- stops_arrested[, c(5:10)]

set.seed(133)
spl_top = createDataPartition(stops_arrested_top$is_arrested, p = 0.75, list = FALSE)
train_arrest_top = stops_arrested_top[spl_top,]
test_arrest_top = stops_arrested_top[-spl_top,]

stops_arrest_top_pm <- rpart(is_arrested ~., data = train_arrest_top, parms = list(loss = PenaltyMatrix))
prp(stops_arrest_top_pm)

predict_top_pm = predict(stops_arrest_top_pm, newdata = test_arrest_top, type = "class")
confusionMatrix(table(test_arrest_top$is_arrested, predict_top_pm))$overall['Kappa']
# Kappa = 0.3407142 
roc.curve(test_arrest_top$is_arrested, predict_top_pm)
# -> 0.667


# Use fine-tuned CART with PM
set.seed(133)
stops_arrest_top_pm2 <- rpart(is_arrested ~., data = train_arrest_top, cp = 0.01, parms = list(loss = PenaltyMatrix))
prp(stops_arrest_top_pm2)

predict_top_pm2 = predict(stops_arrest_top_pm2, newdata = test_arrest_top, type = "class")
confusionMatrix(table(test_arrest_top$is_arrested, predict_top_pm2))$overall['Kappa']
# Kappa = 0.3407142 
roc.curve(test_arrest_top$is_arrested, predict_top_pm2)
# -> 0.667



# RF with SMOTE
set.seed(133)
stops_arrest_top_smote <- SMOTE(is_arrested ~., data = train_arrest_top)
stops_arrest_top_rf <- randomForest(is_arrested ~ ., data = stops_arrest_top_smote, mtry = 3)
stops_arrest_top_rf_predict <- predict(stops_arrest_top_rf, newdata = test_arrest_top, type = "class")
confusionMatrix(table(test_arrest_top$is_arrested, stops_arrest_top_rf_predict))$overall['Kappa']
# -> 0.2165419
roc.curve(test_arrest_top$is_arrested, stops_arrest_top_rf_predict)
# -> 0.775


# => not much improvement over other methods


### Using top features from RFE
# -> stop_duration
# -> stop_hour_part_of_day
# -> violation_raw_Other

stops_arrested_top_rfe <- stops_arrested[, c(8,9,16,25)]
set.seed(133)
spl_top_rfe = createDataPartition(stops_arrested_top_rfe$is_arrested, p = 0.75, list = FALSE)
train_arrest_top_rfe = stops_arrested_top_rfe[spl_top_rfe,]
test_arrest_top_rfe = stops_arrested_top_rfe[-spl_top_rfe,]

# Use fine-tuned CART with PM
set.seed(133)
stops_arrest_top_pmrfe <- rpart(is_arrested ~., data = train_arrest_top_rfe, cp = 0.01, parms = list(loss = PenaltyMatrix))
prp(stops_arrest_top_pm2)
predict_top_pmrfe = predict(stops_arrest_top_pmrfe, newdata = test_arrest_top_rfe, type = "class")
confusionMatrix(table(test_arrest_top_rfe$is_arrested, predict_top_pmrfe))$overall['Kappa']
# Kappa = 0.3495803 
roc.curve(test_arrest_top_rfe$is_arrested, predict_top_pmrfe)
# -> 0.687


# RF with SMOTE
set.seed(133)
stops_arrest_top_smote_rfe <- SMOTE(is_arrested ~., data = train_arrest_top_rfe)
stops_arrest_top_rf_rfe <- randomForest(is_arrested ~ ., data = stops_arrest_top_smote_rfe, mtry = 3, ntree=50)
stops_arrest_top_rf_predict_rfe <- predict(stops_arrest_top_rf_rfe, newdata = test_arrest_top_rfe, type = "class")
confusionMatrix(table(test_arrest_top_rfe$is_arrested, stops_arrest_top_rf_predict_rfe))$overall['Kappa']
# -> 0.1835247 
roc.curve(test_arrest_top_rfe$is_arrested, stops_arrest_top_rf_predict_rfe)
# -> 0.768




# -> stop_hour_part_of_day
# -> contraband_found
# -> search_type_raw
# -> search_conducted
# -> stop_duration
stops_arrested_top_combo <- stops_arrested[, c(5:9, 25)]

set.seed(133)
spl_top_combo = createDataPartition(stops_arrested_top_combo$is_arrested, p = 0.75, list = FALSE)
train_arrest_top_combo = stops_arrested_top_combo[spl_top_combo,]
test_arrest_top_combo = stops_arrested_top_combo[-spl_top_combo,]

# Use fine-tuned CART with PM
set.seed(133)
stops_arrest_top_pm_combo <- rpart(is_arrested ~., data = train_arrest_top_combo, cp = 0.01, parms = list(loss = PenaltyMatrix))
predict_top_pm_combo = predict(stops_arrest_top_pm_combo, newdata = test_arrest_top_combo, type = "class")
confusionMatrix(table(test_arrest_top_combo$is_arrested, predict_top_pm_combo))$overall['Kappa']
# Kappa = 0.3388805
roc.curve(test_arrest_top_combo$is_arrested, predict_top_pm_combo)
# -> 0.715


# RF with SMOTE
set.seed(133)
stops_arrest_top_smote_combo <- SMOTE(is_arrested ~., data = train_arrest_top_combo)
stops_arrest_top_rf_combo <- randomForest(is_arrested ~ ., data = stops_arrest_top_smote_combo, mtry = 3)
stops_arrest_top_rf_predict_combo <- predict(stops_arrest_top_rf_combo, newdata = test_arrest_top_combo, type = "class")
confusionMatrix(table(test_arrest_top_combo$is_arrested, stops_arrest_top_rf_predict_combo))$overall['Kappa']
# -> 0.228653
roc.curve(test_arrest_top_combo$is_arrested, stops_arrest_top_rf_predict_combo)
# -> 0.777




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
set.seed(1993)
train.driver.smote <- SMOTE(is_arrested ~., data = train_arrest_driver)
cart.driver.smote <- rpart(is_arrested ~ ., data = train.driver.smote)
prp(cart.driver.smote)
pred.cart.driver.smote <- predict(cart.driver.smote, newdata = test_arrest_driver, type = "class")
confusionMatrix(table(test_arrest_driver$is_arrested, pred.cart.driver.smote))
# Kappa = 0.0177
roc.curve(test_arrest_driver$is_arrested, pred.cart.driver.smote)
# -> 0.545


# CART with ROSE
set.seed(1993)
train.driver.rose <- ROSE(is_arrested ~ ., data = test_arrest_driver, seed = 1)$data
cart.driver.rose <- rpart(is_arrested ~ ., data = train.driver.rose)
prp(cart.driver.rose)
pred.cart.driver.rose <- predict(cart.driver.rose, newdata = test_arrest_driver, type = "class")
confusionMatrix(table(test_arrest_driver$is_arrested, pred.cart.driver.rose))
# Kappa = 0.0161
roc.curve(test_arrest_driver$is_arrested, pred.cart.driver.rose)
# -> 0.573


# CART with Under-Sampling
set.seed(1993)
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
set.seed(1993)
train.driver.over <- ovun.sample(is_arrested ~., data = train_arrest_driver, method = "over", N = 2*229335)$data
cart.driver.over <- rpart(is_arrested ~ ., data = train.driver.over)
prp(cart.driver.over)
pred.cart.driver.over <- predict(cart.driver.over, newdata = test_arrest_driver, type = "class")
confusionMatrix(table(test_arrest_driver$is_arrested, pred.cart.driver.over))
# Kappa = 0.0177
roc.curve(test_arrest_driver$is_arrested, pred.cart.driver.over)
# -> 0.573


# CART with Penalty Matrix
set.seed(1993)
#PenaltyMatrix = matrix(c(0,1,6,0), byrow = TRUE, nrow = 2)
# all gets bucketed to FALSE with old PM
PenaltyMatrix2 = matrix(c(0,1,50,0), byrow = TRUE, nrow = 2)
arrest_driver_cart_penalty <- rpart(is_arrested ~., data = train_arrest_driver, parms = list(loss = PenaltyMatrix2))
prp(arrest_driver_cart_penalty)
PredictCART_driverPenalty = predict(arrest_driver_cart_penalty, newdata = test_arrest_driver, type = "class")
confusionMatrix(table(test_arrest_driver$is_arrested, PredictCART_driverPenalty))
# Kappa = 0.0075
roc.curve(test_arrest_driver$is_arrested, PredictCART_driverPenalty)
# -> 0.554



# RF with SMOTE
set.seed(1993)
rf.driver.smote <- randomForest(is_arrested ~ ., data = train.driver.smote, ntree = 50)
pred.rf.driver.smote <- predict(rf.driver.smote, newdata = test_arrest_driver, type = "class")
confusionMatrix(table(test_arrest_driver$is_arrested, pred.rf.driver.smote))
# Kappa = 0.0197
roc.curve(test_arrest_driver$is_arrested, pred.rf.driver.smote)
# -> 0.542



# RF with Penalty Matrix
set.seed(1993)
PenaltyMatrix3 = matrix(c(0,1,75,0), byrow = TRUE, nrow = 2)
arrest_driver_rf_penalty <- randomForest(is_arrested ~., data = train_arrest_driver, ntree = 30, parms = list(loss = PenaltyMatrix3))
PredictRF_driverPenalty = predict(arrest_driver_rf_penalty, newdata = test_arrest_driver, type = "class")
confusionMatrix(table(test_arrest_driver$is_arrested, PredictRF_driverPenalty))
# Kappa = 0
roc.curve(test_arrest_driver$is_arrested, PredictRF_driverPenalty)
# -> 0.500



# NB with SMOTE
set.seed(1993)
nb.driver.smote <- naiveBayes(is_arrested ~ ., data = train.driver.smote, laplace = 1)
pred.nb.driver.smote <- predict(nb.driver.smote, newdata = test_arrest_driver, type = "class")
confusionMatrix(table(test_arrest_driver$is_arrested, pred.nb.driver.smote))
# Kappa = 0.0183
roc.curve(test_arrest_driver$is_arrested, pred.nb.driver.smote)
# -> 0.543



# Logistic Regression
set.seed(1993)
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
#  -> search_type_raw
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
rf.smote2 <- randomForest(is_arrested ~ ., data = train.smote2, mtry = 3)
pred.rf.smote2 <- predict(rf.smote2, newdata = test_arrest2, type = "class")
confusionMatrix(table(test_arrest2$is_arrested, pred.rf.smote2))$overall['Kappa']
# Kappa = 0.1997728
roc.curve(test_arrest2$is_arrested, pred.rf.smote2)
# -> 0.621



### Removing stop_duration and search_conducted
stops_arrested3 <- stops_arrested[, c(-5, -9)]

set.seed(3456)
spl3 = createDataPartition(stops_arrested3$is_arrested, p = 0.75, list = FALSE)
train_arrest3 = stops_arrested3[spl3,]
test_arrest3 = stops_arrested3[-spl3,]

# RF with SMOTE
train.smote3 <- SMOTE(is_arrested ~., data = train_arrest3)
rf.smote3 <- randomForest(is_arrested ~ ., data = train.smote3, mtry = 3)
pred.rf.smote3 <- predict(rf.smote3, newdata = test_arrest3, type = "class")
confusionMatrix(table(test_arrest3$is_arrested, pred.rf.smote3))
# Kappa = 0.1949
roc.curve(test_arrest3$is_arrested, pred.rf.smote3)
# -> 0.640



### Removing stop_duration, search_conducted, and search_type
stops_arrested4 <- stops_arrested[, c(-5, -6, -9)]

set.seed(3456)
spl4 = createDataPartition(stops_arrested4$is_arrested, p = 0.75, list = FALSE)
train_arrest4 = stops_arrested4[spl4,]
test_arrest4 = stops_arrested4[-spl4,]

# RF with SMOTE
train.smote4 <- SMOTE(is_arrested ~., data = train_arrest4)
rf.smote4 <- randomForest(is_arrested ~ ., data = train.smote4, mtry = 3)
pred.rf.smote4 <- predict(rf.smote4, newdata = test_arrest4, type = "class")
confusionMatrix(table(test_arrest4$is_arrested, pred.rf.smote4))$overall['Kappa']
# Kappa = 0.1402628
roc.curve(test_arrest4$is_arrested, pred.rf.smote4)
# -> 0.627



### Removing stop_duration, search_conducted, search_type, and contraband_found
stops_arrested5 <- stops_arrested[, c(-5, -6, -7, -9)]

set.seed(3456)
spl5 = createDataPartition(stops_arrested5$is_arrested, p = 0.75, list = FALSE)
train_arrest5 = stops_arrested5[spl5,]
test_arrest5 = stops_arrested5[-spl5,]

# RF with SMOTE
train.smote5 <- SMOTE(is_arrested ~., data = train_arrest5)
rf.smote5 <- randomForest(is_arrested ~ ., data = train.smote5, mtry = 3)
pred.rf.smote5 <- predict(rf.smote5, newdata = test_arrest5, type = "class")
confusionMatrix(table(test_arrest5$is_arrested, pred.rf.smote5))$overall['Kappa']
# Kappa = 0.1135755
roc.curve(test_arrest5$is_arrested, pred.rf.smote5)
# -> 0.601



### Removing stop_duration, search_conducted, search_type, contraband_found, and violation_count
stops_arrested6 <- stops_arrested[, c(-5, -6, -7, -9, -10)]

set.seed(3456)
spl6 = createDataPartition(stops_arrested6$is_arrested, p = 0.75, list = FALSE)
train_arrest6 = stops_arrested6[spl6,]
test_arrest6 = stops_arrested6[-spl6,]

# RF with SMOTE
train.smote6 <- SMOTE(is_arrested ~., data = train_arrest6)
rf.smote6 <- randomForest(is_arrested ~ ., data = train.smote6, mtry = 3)
pred.rf.smote6 <- predict(rf.smote6, newdata = test_arrest6, type = "class")
confusionMatrix(table(test_arrest6$is_arrested, pred.rf.smote6))$overall['Kappa']
# Kappa = 0.0776011
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
# Taken from importance() of Random Forest model


### Removing violation_raw_Equipment.Violation
stops_arrested7 <- stops_arrested[,-14]

set.seed(3456)
spl7 = createDataPartition(stops_arrested7$is_arrested, p = 0.75, list = FALSE)
train_arrest7 = stops_arrested7[spl7,]
test_arrest7 = stops_arrested7[-spl7,]

# RF with SMOTE
train.smote7 <- SMOTE(is_arrested ~., data = train_arrest7)
rf.smote7 <- randomForest(is_arrested ~ ., data = train.smote7, mtry = 3)
pred.rf.smote7 <- predict(rf.smote7, newdata = test_arrest7, type = "class")
confusionMatrix(table(test_arrest7$is_arrested, pred.rf.smote7))$overall['Kappa']
# Kappa = 0.2788613
roc.curve(test_arrest7$is_arrested, pred.rf.smote7)
# -> 0.765


### Removing violation_raw_Equipment.Violation and violation_raw_Traffic.Control.Signal
stops_arrested8 <- stops_arrested[, c(-14,-23)]

set.seed(3456)
spl8 = createDataPartition(stops_arrested8$is_arrested, p = 0.75, list = FALSE)
train_arrest8 = stops_arrested8[spl8,]
test_arrest8 = stops_arrested8[-spl8,]

# RF with SMOTE
train.smote8 <- SMOTE(is_arrested ~., data = train_arrest8)
rf.smote8 <- randomForest(is_arrested ~ ., data = train.smote8, mtry = 3)
pred.rf.smote8 <- predict(rf.smote8, newdata = test_arrest8, type = "class")
confusionMatrix(table(test_arrest8$is_arrested, pred.rf.smote8))$overall['Kappa']
# Kappa = 0.2760278 
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
rf.smote9 <- randomForest(is_arrested ~ ., data = train.smote9, mtry = 3)
pred.rf.smote9 <- predict(rf.smote9, newdata = test_arrest9, type = "class")
confusionMatrix(table(test_arrest9$is_arrested, pred.rf.smote9))$overall['Kappa']
# Kappa = 0.2814082 
roc.curve(test_arrest9$is_arrested, pred.rf.smote9)
# -> 0.767

# CART with PM for comparison
set.seed(3456)
cart.lower.removed <- rpart(is_arrested ~., data = train_arrest9, cp = 0.01, parms = list(loss = PenaltyMatrix))
pred.cart.lower <- predict(cart.lower.removed, newdata = test_arrest9, type = "class")
confusionMatrix(table(test_arrest9$is_arrested, pred.cart.lower))$overall['Kappa']
# Kappa = 0.3482405 
roc.curve(test_arrest9$is_arrested, pred.cart.lower)
# -> 0.702


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
rf.smote10 <- randomForest(is_arrested ~ ., data = train.smote10, mtry = 3, ntree= 50)
pred.rf.smote10 <- predict(rf.smote10, newdata = test_arrest10, type = "class")
confusionMatrix(table(test_arrest10$is_arrested, pred.rf.smote10))$overall['Kappa']
# Kappa = 0.2824498 
roc.curve(test_arrest10$is_arrested, pred.rf.smote10)
# -> 0.768


# CART with PM for comparison
set.seed(3456)
cart.lower.removed2 <- rpart(is_arrested ~., data = train_arrest10, cp = 0.01, parms = list(loss = PenaltyMatrix))
pred.cart.lower2 <- predict(cart.lower.removed2, newdata = test_arrest10, type = "class")
confusionMatrix(table(test_arrest10$is_arrested, pred.cart.lower2))$overall['Kappa']
# Kappa = 0.3482405
roc.curve(test_arrest10$is_arrested, pred.cart.lower2)
# -> 0.702


# Removing all "violation_raw" features
stops_arrested11 <- stops_arrested[, c(-11:-24)]

set.seed(3456)
spl11 = createDataPartition(stops_arrested11$is_arrested, p = 0.75, list = FALSE)
train_arrest11 = stops_arrested11[spl11,]
test_arrest11 = stops_arrested11[-spl11,]

# RF with SMOTE
train.smote11 <- SMOTE(is_arrested ~., data = train_arrest11)
rf.smote11 <- randomForest(is_arrested ~ ., data = train.smote11, mtry = 3, ntree = 50)
pred.rf.smote11 <- predict(rf.smote11, newdata = test_arrest11, type = "class")
confusionMatrix(table(test_arrest11$is_arrested, pred.rf.smote11))$overall['Kappa']
# Kappa = 0.2626828 
roc.curve(test_arrest11$is_arrested, pred.rf.smote11)
# -> 0.764


# => That didn't make it better. Let's remove the bottom 10 features, which includes
#    driver_gender and stop_dom
stops_arrested12 <- stops_arrested[, c(-2, -11, -12, -13, -14, -19, -21, -23, -24, -27)]

set.seed(3456)
spl12 = createDataPartition(stops_arrested12$is_arrested, p = 0.75, list = FALSE)
train_arrest12 = stops_arrested12[spl12,]
test_arrest12 = stops_arrested12[-spl12,]

# RF with SMOTE
train.smote12 <- SMOTE(is_arrested ~., data = train_arrest12)
rf.smote12 <- randomForest(is_arrested ~ ., data = train.smote12, mtry = 3, ntree = 50)
pred.rf.smote12 <- predict(rf.smote12, newdata = test_arrest12, type = "class")
confusionMatrix(table(test_arrest12$is_arrested, pred.rf.smote12))
# Kappa = 0.2719
roc.curve(test_arrest12$is_arrested, pred.rf.smote12)
# -> 0.767


# CART with PM for comparison
set.seed(3456)
cart.lower.removed3 <- rpart(is_arrested ~., data = train_arrest12, cp = 0.01, parms = list(loss = PenaltyMatrix))
pred.cart.lower3 <- predict(cart.lower.removed3, newdata = test_arrest12, type = "class")
confusionMatrix(table(test_arrest12$is_arrested, pred.cart.lower3))$overall['Kappa']
# Kappa = 0.3482405 
roc.curve(test_arrest12$is_arrested, pred.cart.lower3)
# -> 0.702



### => The change from removing the lowest important variables didn't change
###    the performance much at all.






#-------------------------------------------------------------------------
##### Ensemble algorithm: stacking CART, RF, and NB

set.seed(123)
#Testing with PM models
#train_arrest_ensemble <- train_arrest_smote
train_arrest_ensemble <- train_arrest
test_arrest_ensemble <- test_arrest
train_arrest_ensemble$rf_ensemble <- predict(stops_arrest_rf_penalty, type = "prob")[,2]
test_arrest_ensemble$rf_ensemble <- predict(stops_arrest_rf_penalty, newdata = test_arrest, type = "prob")[,2]
# train_arrest_ensemble$rf_ensemble_train <- predict(model.smote.rf, type = "prob")[,2]
# test_arrest_ensemble$rf_ensemble_test <- predict(model.smote.rf, newdata = test_arrest, type = "prob")[,2]
train_arrest_ensemble$cart_ensemble <- predict(cart.penalty.ft, type = "prob")[,2]
test_arrest_ensemble$cart_ensemble <- predict(cart.penalty.ft, newdata = test_arrest, type = "prob")[,2]
# Renaming levels to avoid error
levels(train_arrest_ensemble$is_arrested) <- c("F", "T")
levels(test_arrest_ensemble$is_arrested) <- c("F", "T")


predictors_top <- c('rf_ensemble', 'cart_ensemble')
# predictors_top <- c('rf_ensemble_train', 'cart_ensemble_train')
# predictors_top_test <- c('rf_ensemble_test', 'cart_ensemble_test')

control <- trainControl(method="cv", number=10, savePredictions= 'final', classProbs=TRUE)

model_glm <- train(train_arrest_ensemble[,predictors_top],train_arrest_ensemble[,'is_arrested'],
                   method='glm',
                   trControl=control)

test_arrest_ensemble$glm_stacked <- predict(model_glm,
                                            test_arrest_ensemble[,predictors_top])

confusionMatrix(table(test_arrest_ensemble$is_arrested, test_arrest_ensemble$glm_stacked))
# Kappa = 0.3092
roc.curve(test_arrest_ensemble$is_arrested, test_arrest_ensemble$glm_stacked)
# -> 0.604


# redoing with all 3 models using Penalty Matrix
# adding NB
set.seed(123)
train_arrest_ensemble$nb_ensemble <- predict(stops_arrest_nb_penalty, newdata = train_arrest, type = "raw")[,2]
test_arrest_ensemble$nb_ensemble <- predict(stops_arrest_nb_penalty, newdata = test_arrest, type = "raw")[,2]

predictors_top2 <- c('rf_ensemble', 'cart_ensemble', 'nb_ensemble')
model_glm2 <- train(train_arrest_ensemble[,predictors_top2],train_arrest_ensemble[,'is_arrested'],
                   method='glm',
                   trControl=control)

test_arrest_ensemble$glm_stacked2 <- predict(model_glm2,
                                            test_arrest_ensemble[,predictors_top2])

confusionMatrix(table(test_arrest_ensemble$is_arrested, test_arrest_ensemble$glm_stacked2))$overall['Kappa']
# Kappa = 0.3112706
roc.curve(test_arrest_ensemble$is_arrested, test_arrest_ensemble$glm_stacked2)
# -> 0.606

set.seed(123)
model_glm3 <- train(train_arrest_ensemble[,predictors_top2],train_arrest_ensemble[,'is_arrested'],
                    method='glm',
                    metric = "Kappa",
                    trControl=control)

test_arrest_ensemble$glm_stacked3 <- predict(model_glm3,
                                             test_arrest_ensemble[,predictors_top2])

confusionMatrix(table(test_arrest_ensemble$is_arrested, test_arrest_ensemble$glm_stacked3))$overall['Kappa']
# Kappa = 0.3112706
roc.curve(test_arrest_ensemble$is_arrested, test_arrest_ensemble$glm_stacked3)
# -> 0.606


# using CART instead of glm
set.seed(123)
model_rpart <- train(train_arrest_ensemble[,predictors_top2],train_arrest_ensemble[,'is_arrested'],
                    method='rpart',
                    metric = "Kappa",
                    trControl=control)

test_arrest_ensemble$rpart_stacked <- predict(model_rpart,
                                             test_arrest_ensemble[,predictors_top2])

confusionMatrix(table(test_arrest_ensemble$is_arrested, test_arrest_ensemble$rpart_stacked))$overall['Kappa']
# Kappa = 0.3042669
roc.curve(test_arrest_ensemble$is_arrested, test_arrest_ensemble$rpart_stacked)
# -> 0.599



# using gbm instead of glm
set.seed(123)
model_gbm <- train(train_arrest_ensemble[,predictors_top2],train_arrest_ensemble[,'is_arrested'],
                     method='gbm',
                     metric = "Kappa",
                     trControl=control)

test_arrest_ensemble$gbm_stacked <- predict(model_gbm,
                                              test_arrest_ensemble[,predictors_top2])

confusionMatrix(table(test_arrest_ensemble$is_arrested, test_arrest_ensemble$gbm_stacked))$overall['Kappa']
# Kappa = 0.3513425 
roc.curve(test_arrest_ensemble$is_arrested, test_arrest_ensemble$gbm_stacked)
# -> 0.628



# Using majority vote instead of algorithm
set.seed(123)
test_arrest_ensemble$rf_ensemble_class <- predict(stops_arrest_rf_penalty, newdata = test_arrest, type = "class")
test_arrest_ensemble$cart_ensemble_class <- predict(cart.penalty.ft, newdata = test_arrest, type = "class")
test_arrest_ensemble$nb_ensemble_class <- predict(stops_arrest_nb_penalty, newdata = test_arrest, type = "class")
levels(test_arrest_ensemble$rf_ensemble_class) <- c("F", "T")
levels(test_arrest_ensemble$cart_ensemble_class) <- c("F", "T")
levels(test_arrest_ensemble$nb_ensemble_class) <- c("F", "T")

# test_arrest_ensemble$pred_maj <- as.factor(ifelse(test_arrest_ensemble$rf_ensemble_class == "TRUE" & test_arrest_ensemble$cart_ensemble_class == "TRUE", "TRUE",
#                                            ifelse(test_arrest_ensemble$rf_ensemble_class == "TRUE" & test_arrest_ensemble$nb_ensemble_class == "TRUE", "TRUE",
#                                            ifelse(test_arrest_ensemble$cart_ensemble_class == "TRUE" & test_arrest_ensemble$nb_ensemble_class == "TRUE", "TRUE", "FALSE"))))
test_arrest_ensemble$pred_maj <- as.factor(ifelse(test_arrest_ensemble$rf_ensemble_class == "T" & test_arrest_ensemble$cart_ensemble_class == "T", "T",
                                                  ifelse(test_arrest_ensemble$rf_ensemble_class == "T" & test_arrest_ensemble$nb_ensemble_class == "T", "T",
                                                         ifelse(test_arrest_ensemble$cart_ensemble_class == "T" & test_arrest_ensemble$nb_ensemble_class == "T", "T", "F"))))


confusionMatrix(table(test_arrest_ensemble$is_arrested, test_arrest_ensemble$pred_maj))$overall['Kappa']
# Kappa = 0.3336945
roc.curve(test_arrest_ensemble$is_arrested, test_arrest_ensemble$pred_maj)
# -> 0.625

# => no significant improvement from using ensemble stacking




#-------------------------------------------------------------------------
##### Clustering violation_raw column

## Trying K-modes
# cleaning dataset for clustering by replicating what did for other dataset:

summary(stops_split_small_clustering)
# NAs/Blanks
# -> county_name
# -> driver_age
# -> search_type_raw
# -> stop_hour_part_of_day
# -> stop_outcome
# -> is_arrested

stops_clean_clustering <- stops_split_small_clustering

#driver_age
stops_clean_clustering$driver_age[is.na(stops_clean_clustering$driver_age) == TRUE] <- med

#search_type_raw
stops_clean_clustering$search_type_raw[stops_clean_clustering$search_type_raw == ""] <- NA
table(stops_clean_clustering$search_type_raw, stops_clean_clustering$search_conducted, useNA = "always")
stops_clean_clustering$search_type_raw <- factor(stops_clean_clustering$search_type_raw, levels = c(levels(stops_clean_clustering$search_type_raw)[2:4], "None"))
stops_clean_clustering$search_type_raw[(is.na(stops_clean_clustering$search_type_raw) == TRUE & stops_clean_clustering$search_conducted == FALSE)] <- "None"

table(stops_clean_clustering$search_type_raw, stops_clean_clustering$is_arrested, useNA = "always")
stops_clean_clustering$search_type_raw[(is.na(stops_clean_clustering$search_type_raw) == TRUE)] <- "Other"

# county_name
stops_clean_clustering$county_name[stops_clean_clustering$county_name == ""] <- NA
stops_clean_clustering$county_name <- factor(stops_clean_clustering$county_name, levels = levels(stops_clean_clustering$county_name)[2:9])

# Removing county_name and stop_hour_part_of_day NAs
stops_clean_clustering2 <- stops_clean_clustering[!is.na(stops_clean_clustering$stop_hour_part_of_day),]
stops_clean_clustering2 <- stops_clean_clustering2[!is.na(stops_clean_clustering2$county_name),]

# Check if violation_raw has blanks or NAs
summary(stops_clean_clustering2)
table(stops_clean_clustering2$violation_raw, useNA = "always")
stops_clean_clustering2$violation_raw[stops_clean_clustering2$violation_raw == ""]
stops_clean_clustering2$violation_raw[is.na(stops_clean_clustering2$violation_raw) == TRUE]
# -> None

# How are NAs distributed throughout is_arrested?
table(stops_clean_clustering2$is_arrested, useNA = "always")
# 5352 NAs 
# Since we can't tell if our prediction is right for outcomes with NAs, we'll
# remove the rows where is_arrested = NA

stops_clean_clustering2 <- stops_clean_clustering2[!is.na(stops_clean_clustering2$is_arrested),]
summary(stops_clean_clustering2)
str(stops_clean_clustering2)
stops_clean_clustering3 <- stops_clean_clustering2

stops_split_small_clustering3 <- stops_clean_clustering2[, c(1:8, 10:12, 27:30)]


# Clustering
arrest_cluster <- kmodes(stops_split_small_clustering3[,-9], 3, iter.max = 10, weighted = FALSE)
# => Clusters are identified by "Speed Related" and "Other", so try bucketing
#    by those two

cluster_output <- cbind(stops_split_small_clustering3, arrest_cluster$cluster)
cnames <- colnames(cluster_output)
cnames[16] <- "cluster"
colnames(cluster_output) <- cnames
cluster_output2 <- cluster_output
cluster_output2$cluster_viol <- vector(mode = "character", length = nrow(cluster_output2))
cluster_output2$cluster_viol[cluster_output2$cluster == 1 | cluster_output2$cluster == 2] <- "Speed Related"
cluster_output2$cluster_viol[cluster_output2$cluster == 3] <- "Other"
cluster_output2$cluster_viol <- factor(cluster_output2$cluster_viol)

# Run algorithm on results
cluster_ds <- cluster_output2[, c(-5, -16)]

set.seed(333)
spl_cluster = createDataPartition(cluster_ds$is_arrested, p = 0.75, list = FALSE)
train_cluster = cluster_ds[spl_cluster,]
test_cluster = cluster_ds[-spl_cluster,]

cluster_penalty_model <- rpart(is_arrested ~., data = train_cluster, parms = list(loss = PenaltyMatrix), cp = 0.01)
prp(cluster_penalty_model)
#print(stops_arrest_tree_penalty)
predict_cluster_penalty = predict(cluster_penalty_model, newdata = test_cluster, type = "class")
confusionMatrix(table(test_cluster$is_arrested, predict_cluster_penalty))$overall['Kappa']
# Kappa = 0.3321654
roc.curve(test_cluster$is_arrested, predict_cluster_penalty)
# -> 0.686

train_cluster_smote <- SMOTE(is_arrested ~., data = train_cluster)
cluster_rf_model <- randomForest(is_arrested ~., data = train_cluster_smote, mtry = 3)
varImpPlot(cluster_rf_model)
predict_rf_cluster = predict(cluster_rf_model, newdata = test_cluster, type = "class")
confusionMatrix(table(test_cluster$is_arrested, predict_rf_cluster))$overall['Kappa']
# Kappa = 0.2727099 
roc.curve(test_cluster$is_arrested, predict_rf_cluster)
# -> 0.765



# Cluster with k = 10 to see if difference
arrest_cluster10 <- kmodes(stops_split_small_clustering3[,-9], 10, iter.max = 10, weighted = FALSE)
cluster_output10 <- cbind(stops_split_small_clustering3, arrest_cluster10$cluster)
cnames10 <- colnames(cluster_output10)
cnames10[16] <- "cluster"
colnames(cluster_output10) <- cnames10
cluster_output10_2 <- cluster_output10

cluster_output10_2$cluster_viol <- vector(mode = "character", length = nrow(cluster_output10_2))
cluster_output10_2$cluster_viol[cluster_output10_2$cluster == 1 | cluster_output10_2$cluster == 3 | cluster_output10_2$cluster == 5 | cluster_output10_2$cluster == 7 | cluster_output10_2$cluster == 10] <- "Speed Related"
cluster_output10_2$cluster_viol[cluster_output10_2$cluster == 2 | cluster_output10_2$cluster == 6] <- "Other"
cluster_output10_2$cluster_viol[cluster_output10_2$cluster == 4] <- "Moving Violation"
cluster_output10_2$cluster_viol[cluster_output10_2$cluster == 8 | cluster_output10_2$cluster == 9] <- "Registration"


cluster_ds_10 <- cluster_output10_2[, c(-5, -16)]
cluster_ds_10$cluster_viol <- factor(cluster_ds_10$cluster_viol)

set.seed(3313)
spl_cluster10 = createDataPartition(cluster_ds_10$is_arrested, p = 0.75, list = FALSE)
train_cluster10 = cluster_ds_10[spl_cluster10,]
test_cluster10 = cluster_ds_10[-spl_cluster10,]

cluster_penalty_model10 <- rpart(is_arrested ~., data = train_cluster10, parms = list(loss = PenaltyMatrix), cp = 0.01)
prp(cluster_penalty_model10)
predict_cluster_penalty10 = predict(cluster_penalty_model10, newdata = test_cluster10, type = "class")
confusionMatrix(table(test_cluster10$is_arrested, predict_cluster_penalty10))$overall['Kappa']
# Kappa = 0.336835
roc.curve(test_cluster10$is_arrested, predict_cluster_penalty10)
# -> 0.689


train_cluster_smote10 <- SMOTE(is_arrested ~., data = train_cluster10)
cluster_rf_model10 <- randomForest(is_arrested ~., data = train_cluster_smote10, mtry = 3)
varImpPlot(cluster_rf_model10)
predict_rf_cluster10 = predict(cluster_rf_model10, newdata = test_cluster10, type = "class")
confusionMatrix(table(test_cluster10$is_arrested, predict_rf_cluster10))$overall['Kappa']
# Kappa = 0.2722677 
roc.curve(test_cluster10$is_arrested, predict_rf_cluster10)
# -> 0.772


# => Not much changed in terms of performance or the important variables identified



#---------------------------------------------------------------------------
# The final model:
# CART with Penalty Matrix
set.seed(111)
# cart.penalty.ft <- rpart(is_arrested ~ ., data = train_arrest, cp = 0.01, parms = list(loss = PenaltyMatrix))
#pred.cart.penalty.ft <- predict(cart.penalty.ft, newdata = test_arrest, type = "class")
final_model_pred <- predict(cart.penalty.ft, newdata = test_arrest, type = "p")
final_model_ROC <-  prediction(final_model_pred[,2], test_arrest$is_arrested)
ROCperf_finalmodel <- performance(final_model_ROC, "tpr", "fpr")
plot(ROCperf_finalmodel, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
plot(ROCperf_finalmodel, colorize=TRUE, print.cutoffs.at=seq(0,0.1,by=0.01), text.adj=c(-0.2,1.7))
confusionMatrix(table(test_arrest$is_arrested, final_model_pred[,2] >= 0.065))$overall['Kappa']
# -> 0.2849794
roc.curve(test_arrest$is_arrested, final_model_pred[,2]>= 0.065)
# -> 0.720
prp(final_model_pred)


















#-------------------------------------------------------------
# Testing for layered ROC plots


# CART with Penalty Matrix
# PenaltyMatrix = matrix(c(0,1,6,0), byrow = TRUE, nrow = 2)
set.seed(123)
cv_resultsPM2 <- lapply(folds, function(x) {
  arrest_train <- stops_arrested[-x, ]
  arrest_test <- stops_arrested[x, ]
  tree.model <- rpart(is_arrested ~ ., data = arrest_train, parms = list(loss = PenaltyMatrix))
  arrest.pred <- predict(tree.model, arrest_test, type = "prob")
  arrest.pred2 <- ROCR::prediction(arrest.pred[,2], arrest_test$is_arrested)
  #auc_val <- roc.curve(arrest_test$is_arrested, arrest.pred)$auc
  auc_val2 <- ROCR::performance(arrest.pred2,  'tpr',  'fpr')
  #kappa <- kappa2(data.frame(arrest_test$is_arrested, arrest.pred))$value
  #return(c(auc_val, kappa))
  return(c(auc_val2))
})

df_cv_resultsPM <- data.frame(cv_resultsPM)
#AUC
mean(unlist(df_cv_resultsPM[1,]))
# -> 0.7018223
#Kappa
mean(unlist(df_cv_resultsPM[2,]))
# -> 0.3649853

test <- unlist(cv_resultsPM2)
#test_df <- data.frame(test)
test$Fold01@x.name
test$Fold01@x.values

fold01_df <- data.frame(fpr = test$Fold01@x.values, tpr = test$Fold01@y.values)
colnames(fold01_df) <- c("fpr", "tpr")
fold01_df$method <- "1"
fold02_df <- data.frame(fpr = test$Fold02@x.values, tpr = test$Fold02@y.values)
colnames(fold02_df) <- c("fpr", "tpr")
fold02_df$method <- "2"

rbind(fold01_df, fold02_df) %>%
  ggplot(data = ., aes(x = fpr, y = tpr, linetype = method, color = method)) + 
  geom_line() +
  geom_abline(a = 1, b = 0, linetype = 2) +
  scale_x_continuous(labels = scales::percent, lim = c(0,1)) +
  scale_y_continuous(labels = scales::percent, lim = c(0,1)) +
  theme(legend.position = c(0.8,0.2), legend.title = element_blank())





set.seed(123)
cv_resultsPM3 <- lapply(folds, function(x) {
  arrest_train <- stops_arrested[-x, ]
  arrest_test <- stops_arrested[x, ]
  tree.model <- rpart(is_arrested ~ ., data = arrest_train, parms = list(loss = PenaltyMatrix))
  arrest.pred <- predict(tree.model, arrest_test, type = "prob")
  arrest.pred2 <- ROCR::prediction(arrest.pred[,2], arrest_test$is_arrested)
  #auc_val <- roc.curve(arrest_test$is_arrested, arrest.pred)$auc
  auc_val2 <- ROCR::performance(arrest.pred2,  'tpr',  'fpr')
  #kappa <- kappa2(data.frame(arrest_test$is_arrested, arrest.pred >= 0.5))$value
  kappa <- confusionMatrix(table(arrest_test$is_arrested, arrest.pred[,2] >= 0.5))$overall['Kappa']
  return(c(auc_val, kappa))
  #return(c(auc_val2))
})

# Fold01
arrest_train <- stops_arrested[-folds$Fold01, ]
arrest_test <- stops_arrested[folds$Fold01, ]
tree.model <- rpart(is_arrested ~ ., data = arrest_train, parms = list(loss = PenaltyMatrix))
arrest.pred <- predict(tree.model, arrest_test, type = "prob")
arrest.pred2 <- ROCR::prediction(arrest.pred[,2], arrest_test$is_arrested)
#auc_val <- roc.curve(arrest_test$is_arrested, arrest.pred)$auc
auc_val2 <- ROCR::performance(arrest.pred2,  'tpr',  'fpr')
#kappa <- kappa2(data.frame(arrest_test$is_arrested, arrest.pred >= 0.5))$value
kappa_test <- as.factor(arrest.pred[,2] >= 0.5)
kappa_test2 <- factor(arrest.pred[,2] >= 0.5, levels = c(FALSE, TRUE))
#stops_clean_clustering$county_name <- factor(stops_clean_clustering$county_name, levels = levels(stops_clean_clustering$county_name)[2:9])
levels(kappa_test)
class(kappa_test)
levels(kappa_test2)
class(kappa_test2)
table(kappa_test2)
kappa <- confusionMatrix(table(arrest_test$is_arrested, kappa_test2))$overall['Kappa']


#Fold02
arrest_train2 <- stops_arrested[-folds$Fold02, ]
arrest_test2 <- stops_arrested[folds$Fold02, ]
tree.model2 <- rpart(is_arrested ~ ., data = arrest_train2, parms = list(loss = PenaltyMatrix))
arrest.pred2 <- predict(tree.model2, arrest_test2, type = "prob")
arrest.pred22 <- ROCR::prediction(arrest.pred2[,2], arrest_test2$is_arrested)
auc_val22 <- ROCR::performance(arrest.pred22,  'tpr',  'fpr')
kappa_test22 <- factor(arrest.pred2[,2] >= 0.5, levels = c(FALSE, TRUE))
table(kappa_test22)
kappa <- kappa2(data.frame(arrest_test2$is_arrested, arrest.pred2 >= 0.5))$value
#kappa <- confusionMatrix(table(arrest_test2$is_arrested, kappa_test22))$overall['Kappa']
a <- arrest.pred2[,2] >= 0.5
table(a)
test <- data.frame(arrest_test2$is_arrested, arrest.pred2[,2] >= 0.5)
table(test$arrest_test2.is_arrested)
table(test$TRUE.)
table(test$FALSE.)
test_a <- data.frame(arrest_test2$is_arrested, a)


set.seed(123)
cv_resultsPM_redo <- lapply(folds, function(x) {
  arrest_train <- stops_arrested[-x, ]
  arrest_test <- stops_arrested[x, ]
  tree.model <- rpart(is_arrested ~ ., data = arrest_train, parms = list(loss = PenaltyMatrix))
  arrest.pred <- predict(tree.model, arrest_test, type = "prob")
  arrest.pred2 <- ROCR::prediction(arrest.pred[,2], arrest_test$is_arrested)
  auc_val <- ROCR::performance(arrest.pred2,  'tpr',  'fpr')
  kappa_pred <- factor(arrest.pred[,2] >= 0.5, levels = c(FALSE, TRUE))
  kappa <- confusionMatrix(table(arrest_test$is_arrested, kappa_pred))$overall['Kappa']
  return(c(auc_val, kappa))
})

cv_resultsPM_redo2 <- unlist(cv_resultsPM_redo)
#cv_resultsPM_redo2$Fold01@x.values

PMfold01_df <- data.frame(fpr = cv_resultsPM_redo2$Fold01@x.values, tpr = cv_resultsPM_redo2$Fold01@y.values)
colnames(PMfold01_df) <- c("fpr", "tpr")
PMfold01_df$method <- "1"
PMfold02_df <- data.frame(fpr = cv_resultsPM_redo2$Fold02@x.values, tpr = cv_resultsPM_redo2$Fold02@y.values)
colnames(PMfold02_df) <- c("fpr", "tpr")
PMfold02_df$method <- "2"
PMfold03_df <- data.frame(fpr = cv_resultsPM_redo2$Fold03@x.values, tpr = cv_resultsPM_redo2$Fold03@y.values)
colnames(PMfold03_df) <- c("fpr", "tpr")
PMfold03_df$method <- "3"
PMfold04_df <- data.frame(fpr = cv_resultsPM_redo2$Fold04@x.values, tpr = cv_resultsPM_redo2$Fold04@y.values)
colnames(PMfold04_df) <- c("fpr", "tpr")
PMfold04_df$method <- "4"
PMfold05_df <- data.frame(fpr = cv_resultsPM_redo2$Fold05@x.values, tpr = cv_resultsPM_redo2$Fold05@y.values)
colnames(PMfold05_df) <- c("fpr", "tpr")
PMfold05_df$method <- "5"
PMfold06_df <- data.frame(fpr = cv_resultsPM_redo2$Fold06@x.values, tpr = cv_resultsPM_redo2$Fold06@y.values)
colnames(PMfold06_df) <- c("fpr", "tpr")
PMfold06_df$method <- "6"
PMfold07_df <- data.frame(fpr = cv_resultsPM_redo2$Fold07@x.values, tpr = cv_resultsPM_redo2$Fold07@y.values)
colnames(PMfold07_df) <- c("fpr", "tpr")
PMfold07_df$method <- "7"
PMfold08_df <- data.frame(fpr = cv_resultsPM_redo2$Fold08@x.values, tpr = cv_resultsPM_redo2$Fold08@y.values)
colnames(PMfold08_df) <- c("fpr", "tpr")
PMfold08_df$method <- "8"
PMfold09_df <- data.frame(fpr = cv_resultsPM_redo2$Fold09@x.values, tpr = cv_resultsPM_redo2$Fold09@y.values)
colnames(PMfold09_df) <- c("fpr", "tpr")
PMfold09_df$method <- "9"
PMfold10_df <- data.frame(fpr = cv_resultsPM_redo2$Fold10@x.values, tpr = cv_resultsPM_redo2$Fold10@y.values)
colnames(PMfold10_df) <- c("fpr", "tpr")
PMfold10_df$method <- "10"

rbind(PMfold01_df, PMfold02_df, PMfold03_df, PMfold04_df, PMfold05_df, PMfold06_df, PMfold07_df, PMfold08_df, PMfold09_df, PMfold10_df) %>%
  ggplot(data = ., aes(x = fpr, y = tpr, linetype = method, color = method)) + 
  geom_line() +
  geom_abline(a = 1, b = 0, linetype = 2) +
  scale_x_continuous(labels = scales::percent, lim = c(0,1)) +
  scale_y_continuous(labels = scales::percent, lim = c(0,1)) +
  theme(legend.position = c(0.8,0.2), legend.title = element_blank())


mean(cv_resultsPM_redo2$Fold01.Kappa,
     cv_resultsPM_redo2$Fold02.Kappa, 
     cv_resultsPM_redo2$Fold03.Kappa,
     cv_resultsPM_redo2$Fold04.Kappa,
     cv_resultsPM_redo2$Fold05.Kappa,
     cv_resultsPM_redo2$Fold06.Kappa,
     cv_resultsPM_redo2$Fold07.Kappa,
     cv_resultsPM_redo2$Fold08.Kappa,
     cv_resultsPM_redo2$Fold09.Kappa,
     cv_resultsPM_redo2$Fold10.Kappa)

df_cv_resultsPM <- data.frame(cv_resultsPM)
#AUC
mean(unlist(df_cv_resultsPM[1,]))
# -> 0.7018223
#Kappa
mean(unlist(df_cv_resultsPM[2,]))
# -> 0.3649853

first_list <- cv_resultsPM_redo[[1]]



# Trying to fix Kappa
set.seed(123)
cv_resultsPM_redo3 <- lapply(folds, function(x) {
  arrest_train <- stops_arrested[-x, ]
  arrest_test <- stops_arrested[x, ]
  tree.model <- rpart(is_arrested ~ ., data = arrest_train, parms = list(loss = PenaltyMatrix))
  arrest.pred.class <- predict(tree.model, arrest_test, type = "class")
  arrest.pred <- predict(tree.model, arrest_test, type = "prob")
  arrest.pred2 <- ROCR::prediction(arrest.pred[,2], arrest_test$is_arrested)
  auc_val <- ROCR::performance(arrest.pred2,  'tpr',  'fpr')
  #kappa_pred <- factor(arrest.pred[,2] >= 0.5, levels = c(FALSE, TRUE))
  kappa <- confusionMatrix(table(arrest_test$is_arrested, arrest.pred.class))$overall['Kappa']
  return(c(auc_val, kappa))
})

cv_resultsPM_redo4 <- unlist(cv_resultsPM_redo3)

PMfold01_df <- data.frame(fpr = cv_resultsPM_redo4$Fold01@x.values, tpr = cv_resultsPM_redo4$Fold01@y.values)
colnames(PMfold01_df) <- c("fpr", "tpr")
PMfold01_df$method <- "1"
PMfold02_df <- data.frame(fpr = cv_resultsPM_redo4$Fold02@x.values, tpr = cv_resultsPM_redo4$Fold02@y.values)
colnames(PMfold02_df) <- c("fpr", "tpr")
PMfold02_df$method <- "2"
PMfold03_df <- data.frame(fpr = cv_resultsPM_redo4$Fold03@x.values, tpr = cv_resultsPM_redo4$Fold03@y.values)
colnames(PMfold03_df) <- c("fpr", "tpr")
PMfold03_df$method <- "3"
PMfold04_df <- data.frame(fpr = cv_resultsPM_redo4$Fold04@x.values, tpr = cv_resultsPM_redo4$Fold04@y.values)
colnames(PMfold04_df) <- c("fpr", "tpr")
PMfold04_df$method <- "4"
PMfold05_df <- data.frame(fpr = cv_resultsPM_redo4$Fold05@x.values, tpr = cv_resultsPM_redo4$Fold05@y.values)
colnames(PMfold05_df) <- c("fpr", "tpr")
PMfold05_df$method <- "5"
PMfold06_df <- data.frame(fpr = cv_resultsPM_redo4$Fold06@x.values, tpr = cv_resultsPM_redo4$Fold06@y.values)
colnames(PMfold06_df) <- c("fpr", "tpr")
PMfold06_df$method <- "6"
PMfold07_df <- data.frame(fpr = cv_resultsPM_redo4$Fold07@x.values, tpr = cv_resultsPM_redo4$Fold07@y.values)
colnames(PMfold07_df) <- c("fpr", "tpr")
PMfold07_df$method <- "7"
PMfold08_df <- data.frame(fpr = cv_resultsPM_redo4$Fold08@x.values, tpr = cv_resultsPM_redo4$Fold08@y.values)
colnames(PMfold08_df) <- c("fpr", "tpr")
PMfold08_df$method <- "8"
PMfold09_df <- data.frame(fpr = cv_resultsPM_redo4$Fold09@x.values, tpr = cv_resultsPM_redo4$Fold09@y.values)
colnames(PMfold09_df) <- c("fpr", "tpr")
PMfold09_df$method <- "9"
PMfold10_df <- data.frame(fpr = cv_resultsPM_redo4$Fold10@x.values, tpr = cv_resultsPM_redo4$Fold10@y.values)
colnames(PMfold10_df) <- c("fpr", "tpr")
PMfold10_df$method <- "10"

rbind(PMfold01_df, PMfold02_df, PMfold03_df, PMfold04_df, PMfold05_df, PMfold06_df, PMfold07_df, PMfold08_df, PMfold09_df, PMfold10_df) %>%
  ggplot(data = ., aes(x = fpr, y = tpr, linetype = method, color = method)) + 
  geom_line() +
  geom_abline(a = 1, b = 0, linetype = 2) +
  scale_x_continuous(labels = scales::percent, lim = c(0,1)) +
  scale_y_continuous(labels = scales::percent, lim = c(0,1)) +
  theme(legend.position = c(0.8,0.2), legend.title = element_blank())


mean(cv_resultsPM_redo4$Fold01.Kappa,
     cv_resultsPM_redo4$Fold02.Kappa, 
     cv_resultsPM_redo4$Fold03.Kappa,
     cv_resultsPM_redo4$Fold04.Kappa,
     cv_resultsPM_redo4$Fold05.Kappa,
     cv_resultsPM_redo4$Fold06.Kappa,
     cv_resultsPM_redo4$Fold07.Kappa,
     cv_resultsPM_redo4$Fold08.Kappa,
     cv_resultsPM_redo4$Fold09.Kappa,
     cv_resultsPM_redo4$Fold10.Kappa)


# Just code to construct the plots
set.seed(123)
cv_resultsPM_redo5 <- lapply(folds, function(x) {
  arrest_train <- stops_arrested[-x, ]
  arrest_test <- stops_arrested[x, ]
  tree.model <- rpart(is_arrested ~ ., data = arrest_train, parms = list(loss = PenaltyMatrix))
  arrest.pred <- predict(tree.model, arrest_test, type = "prob")
  arrest.pred2 <- ROCR::prediction(arrest.pred[,2], arrest_test$is_arrested)
  auc_val <- ROCR::performance(arrest.pred2,  'tpr',  'fpr')
  return(auc_val)
})

cv_resultsPM_redo6 <- unlist(cv_resultsPM_redo5)

PMfold01_df <- data.frame(fpr = cv_resultsPM_redo6$Fold01@x.values, tpr = cv_resultsPM_redo6$Fold01@y.values)
colnames(PMfold01_df) <- c("fpr", "tpr")
PMfold01_df$method <- "1"
PMfold02_df <- data.frame(fpr = cv_resultsPM_redo6$Fold02@x.values, tpr = cv_resultsPM_redo6$Fold02@y.values)
colnames(PMfold02_df) <- c("fpr", "tpr")
PMfold02_df$method <- "2"
PMfold03_df <- data.frame(fpr = cv_resultsPM_redo6$Fold03@x.values, tpr = cv_resultsPM_redo6$Fold03@y.values)
colnames(PMfold03_df) <- c("fpr", "tpr")
PMfold03_df$method <- "3"
PMfold04_df <- data.frame(fpr = cv_resultsPM_redo6$Fold04@x.values, tpr = cv_resultsPM_redo6$Fold04@y.values)
colnames(PMfold04_df) <- c("fpr", "tpr")
PMfold04_df$method <- "4"
PMfold05_df <- data.frame(fpr = cv_resultsPM_redo6$Fold05@x.values, tpr = cv_resultsPM_redo6$Fold05@y.values)
colnames(PMfold05_df) <- c("fpr", "tpr")
PMfold05_df$method <- "5"
PMfold06_df <- data.frame(fpr = cv_resultsPM_redo6$Fold06@x.values, tpr = cv_resultsPM_redo6$Fold06@y.values)
colnames(PMfold06_df) <- c("fpr", "tpr")
PMfold06_df$method <- "6"
PMfold07_df <- data.frame(fpr = cv_resultsPM_redo6$Fold07@x.values, tpr = cv_resultsPM_redo6$Fold07@y.values)
colnames(PMfold07_df) <- c("fpr", "tpr")
PMfold07_df$method <- "7"
PMfold08_df <- data.frame(fpr = cv_resultsPM_redo6$Fold08@x.values, tpr = cv_resultsPM_redo6$Fold08@y.values)
colnames(PMfold08_df) <- c("fpr", "tpr")
PMfold08_df$method <- "8"
PMfold09_df <- data.frame(fpr = cv_resultsPM_redo6$Fold09@x.values, tpr = cv_resultsPM_redo6$Fold09@y.values)
colnames(PMfold09_df) <- c("fpr", "tpr")
PMfold09_df$method <- "9"
PMfold10_df <- data.frame(fpr = cv_resultsPM_redo6$Fold10@x.values, tpr = cv_resultsPM_redo6$Fold10@y.values)
colnames(PMfold10_df) <- c("fpr", "tpr")
PMfold10_df$method <- "10"

rbind(PMfold01_df, PMfold02_df, PMfold03_df, PMfold04_df, PMfold05_df, PMfold06_df, PMfold07_df, PMfold08_df, PMfold09_df, PMfold10_df) %>%
  ggplot(data = ., aes(x = fpr, y = tpr, linetype = method, color = method)) + 
  geom_line() +
  geom_abline(a = 1, b = 0, linetype = 2) +
  scale_x_continuous(labels = scales::percent, lim = c(0,1)) +
  scale_y_continuous(labels = scales::percent, lim = c(0,1)) +
  theme(legend.position = c(0.8,0.2), legend.title = element_blank())

