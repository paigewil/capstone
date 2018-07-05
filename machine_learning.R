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


# is_arrested

# Split the data into test and training data sets
# -> since I have a lot of data, I want to make my testing sample a little larger
#    to have better tests to make sure I'm choosing the best algorithm
set.seed(3000)
spl = sample.split(stops_arrested$is_arrested, SplitRatio = 0.6)
train_arrest = subset(stops_arrested, spl==TRUE)
test_arrest = subset(stops_arrested, spl==FALSE)


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

#using cross-validation to choose parameters and hopefully get better results
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
stops_arrest_smote <- train_arrest
str(stops_arrest_smote) #SMOTE can only be used with factors and ints

train.smote <- SMOTE(is_arrested ~., data = stops_arrest_smote)
tree.smote <- rpart(is_arrested ~ ., data = train.smote)
prp(tree.smote)

pred.tree.smote <- predict(tree.smote, type = "class")
confusionMatrix(table(train.smote$is_arrested, pred.tree.smote))
# accuracy = 0.8892
# precision/specificity = 0.8891
# recall/sensitivity  = 0.8893
# Kappa = 0.7725
roc.curve(train.smote$is_arrested, pred.tree.smote)
# -> 0.884
# AUC improved so using SMOTE is a better model thus far


# Showing that ROSE performs worse as a sampler
# Rose also needs the variables as factors
train.rose <- ROSE(is_arrested ~ ., data = stops_arrest_smote, seed = 1)$data
tree.rose <- rpart(is_arrested ~ ., data = train.rose)
prp(tree.rose)

pred.tree.rose <- predict(tree.rose, type = "class")
confusionMatrix(table(train.rose$is_arrested, pred.tree.rose))
# Kappa = 0.5526
roc.curve(train.rose$is_arrested, pred.tree.rose)
# -> 0.776


# Seeing if RandomForest can improve model
rf.smote <- randomForest(is_arrested ~ ., data = train.smote)
pred.rf.smote <- predict(rf.smote, type = "class")
confusionMatrix(table(train.smote$is_arrested, pred.rf.smote))
# Kappa = 0.8002
roc.curve(train.smote$is_arrested, pred.rf.smote)
# -> 0.895; best result so far

# vizualizing tree; understand decision making in RF, one tree at a time
getTree(rf.smote)

#variable importance
importance(rf.smote)
varImpPlot(rf.smote)
# -> we see that stop duration is the most important variable, aligning with
#    what we saw in the CART model
# -> search_type, search_conducted, and contraband_found are also important variables,
#    also seen in the CART model

#trying RF with party package to leverage plots
# cForestMod <- cforest(is_arrested ~ ., data = train.smote)
# -> runtime error!!!



# Testing using a Penalty Matrix instead of balancing the unbalanced dataset
# -> Want higher penalty for false negative, i.e., where say no arrest when there
#    was. Want this because we need to accurately label arrests.
PenaltyMatrix = matrix(c(0,1,6,0), byrow = TRUE, nrow = 2)
stops_arrest_tree_penalty <- rpart(is_arrested ~., data = train_arrest, method = "class", minbucket = 10, parms = list(loss = PenaltyMatrix))
prp(stops_arrest_tree_penalty)
print(stops_arrest_tree_penalty)

PredictCARTPenalty = predict(stops_arrest_tree_penalty, type = "class")
confusionMatrix(table(train_arrest$is_arrested, PredictCARTPenalty))
# accuracy = 0.9593 
# precision/specificity = 0.2781
# recall/sensitivity  = 0.9870
# Kappa = 0.3535
roc.curve(train_arrest$is_arrested, PredictCARTPenalty)
# -> 0.706; SMOTE still performs  better



#Naive Bayes
# Turning some variables back into factors because need all categorical
# variables
nb.train <- train.smote
# nb.train$stop_time_hour <- factor(nb.train$stop_time_hour,
#                                             ordered = TRUE,
#                                             levels = as.character(0:23))
# 
# nb.train$day_of_month <- factor(nb.train$day_of_month,
#                                     ordered = TRUE,
#                                     levels = days_label)
# 
# nb.train$stop_month <- factor(nb.train$stop_month,
#                                         ordered = TRUE,
#                                         levels = as.character(1:12))

#nb.train$driver_age <- as.factor(nb.train$driver_age)
# Since truning driver_age into a factor will give too many levels,
# I'm going to group it into the age groups from the census, which I've used
# previously.
nb.train$driver_age[nb.train$driver_age >= 15 & nb.train$driver_age < 20] <- 4
nb.train$driver_age[nb.train$driver_age >= 20 & nb.train$driver_age < 25] <- 5
nb.train$driver_age[nb.train$driver_age >= 25 & nb.train$driver_age < 30] <- 6
nb.train$driver_age[nb.train$driver_age >= 30 & nb.train$driver_age < 35] <- 7
nb.train$driver_age[nb.train$driver_age >= 35 & nb.train$driver_age < 40] <- 8
nb.train$driver_age[nb.train$driver_age >= 40 & nb.train$driver_age < 45] <- 9
nb.train$driver_age[nb.train$driver_age >= 45 & nb.train$driver_age < 50] <- 10
nb.train$driver_age[nb.train$driver_age >= 50 & nb.train$driver_age < 55] <- 11
nb.train$driver_age[nb.train$driver_age >= 55 & nb.train$driver_age < 60] <- 12
nb.train$driver_age[nb.train$driver_age >= 60 & nb.train$driver_age < 65] <- 13
nb.train$driver_age[nb.train$driver_age >= 65 & nb.train$driver_age < 70] <- 14
nb.train$driver_age[nb.train$driver_age >= 70 & nb.train$driver_age < 75] <- 15
nb.train$driver_age[nb.train$driver_age >= 75 & nb.train$driver_age < 80] <- 16
# nb.train$driver_age <- factor(nb.train$driver_age,
#                               ordered = TRUE,
#                               levels = c(4:16))
nb.train$driver_age <- factor(nb.train$driver_age)

str(nb.train)
#get rid of is_arrested column so not used in prediction 
nb.train2 <- nb.train[,-8]

nb.model <- naiveBayes(nb.train2, nb.train$is_arrested, laplace = 1)
nb.predict <- predict(nb.model, nb.train2)

confusionMatrix(table(nb.train$is_arrested, nb.predict))
# Kappa = 0.7774
roc.curve(nb.train$is_arrested, nb.predict)
# -> 0.883


# NB can use continuous variables
nb.train3 <- train.smote
str(nb.train3)
#get rid of is_arrested column so not used in prediction 
nb.train4 <- nb.train3[,-8]

nb.model2 <- naiveBayes(nb.train4, nb.train3$is_arrested, laplace = 1)
nb.predict2 <- predict(nb.model2, nb.train4)

confusionMatrix(table(nb.train3$is_arrested, nb.predict2))
# Kappa = 0.7767
roc.curve(nb.train3$is_arrested, nb.predict2)
# -> 0.883; same AUC

# <=> Equivalent and easier way to write
nb.model3 <- naiveBayes(is_arrested ~., data = train.smote, laplace = 1)
nb.predict3 <- predict(nb.model3,train.smote)

confusionMatrix(table(train.smote$is_arrested, nb.predict3))
# Kappa = 0.7767
roc.curve(train.smote$is_arrested, nb.predict3)
# -> 0.883



# 10-fold CV with CART
set.seed(123)
folds <- createFolds(stops_arrested$is_arrested, k = 10)
cv_results <- lapply(folds, function(x) {
  arrest_train <- stops_arrested[-x, ]
  arrest_test <- stops_arrested[x, ]
  arrest.train.smote <- SMOTE(is_arrested ~., data = arrest_train)
  tree.model <- rpart(is_arrested ~ ., data = arrest.train.smote)
  arrest.pred <- predict(tree.model, arrest_test, type = "class")
  auc_val <- roc.curve(arrest_test$is_arrested, arrest.pred)$auc
  return(auc_val)
})

str(cv_results)
mean(unlist(cv_results))
# -> 0.7754418

# 10-fold CV with RF
# set.seed(123)
# folds <- createFolds(stops_arrested$is_arrested, k = 10)
cv_results_rf <- lapply(folds, function(x) {
  arrest_train <- stops_arrested[-x, ]
  arrest_test <- stops_arrested[x, ]
  arrest.train.smote <- SMOTE(is_arrested ~., data = arrest_train)
  tree.model <- randomForest(is_arrested ~ ., data = arrest.train.smote)
  arrest.pred <- predict(tree.model, arrest_test, type = "class")
  auc_val <- roc.curve(arrest_test$is_arrested, arrest.pred)$auc
  return(auc_val)
})

str(cv_results_rf)
mean(unlist(cv_results_rf))
# -> 0.7688412

# 10-fold CV with Naive Bayes
cv_results_nb <- lapply(folds, function(x) {
  arrest_train <- stops_arrested[-x, ]
  arrest_test <- stops_arrested[x, ]
  arrest.train.smote <- SMOTE(is_arrested ~., data = arrest_train)
  tree.model <- naiveBayes(is_arrested ~., data = arrest.train.smote, laplace = 1)
  arrest.pred <- predict(tree.model, arrest_test, type = "class")
  auc_val <- roc.curve(arrest_test$is_arrested, arrest.pred)$auc
  return(auc_val)
})

str(cv_results_nb)
mean(unlist(cv_results_nb))
# -> 0.75007


### -> Best results from the 10-fold CV testing is CART model


#Using kappa as performance measure because works best with imbalanced data
#Using test dataset to inform what to expect when do 10-fold CV with kappa

#CART
pred.tree.smote2 <- predict(tree.smote, test_arrest, type = "class")
confusionMatrix(table(test_arrest$is_arrested, pred.tree.smote2))
# kappa = 0.2205

#Precision
prec <- posPredValue(pred.tree.smote2, test_arrest$is_arrested, positive = "TRUE")
# -> 0.1557044
#Sensitivity
rec <- sensitivity(pred.tree.smote2, test_arrest$is_arrested, positive = "TRUE")
# ->2 *  0.6291096
# => F-measure
f <- (2* prec * rec)/ (prec + rec)
# -> 0.2496263

#RF
pred.rf.smote2 <- predict(rf.smote, test_arrest, type = "class")
confusionMatrix(table(test_arrest$is_arrested, pred.rf.smote2))
# kappa = 0.2793

#NB
nb.predict.test <- predict(nb.model3, test_arrest)
confusionMatrix(table(test_arrest$is_arrested, nb.predict.test))
# kappa = 0.2641


# Re-doing 10-fold CV with kappa
# 10-fold CV with CART
set.seed(123)
folds_k <- createFolds(stops_arrested$is_arrested, k = 10)
cv_results_k <- lapply(folds_k, function(x) {
  arrest_train <- stops_arrested[-x, ]
  arrest_test <- stops_arrested[x, ]
  arrest.train.smote <- SMOTE(is_arrested ~., data = arrest_train)
  tree.model <- rpart(is_arrested ~ ., data = arrest.train.smote)
  arrest.pred <- predict(tree.model, arrest_test, type = "class")
  kappa <- kappa2(data.frame(arrest_test$is_arrested, arrest.pred))$value
  return(kappa)
})

str(cv_results_k)
mean(unlist(cv_results_k))
# -> 0.2234328


# 10-fold CV with RF
cv_results_rf_k <- lapply(folds_k, function(x) {
  arrest_train <- stops_arrested[-x, ]
  arrest_test <- stops_arrested[x, ]
  arrest.train.smote <- SMOTE(is_arrested ~., data = arrest_train)
  tree.model <- randomForest(is_arrested ~ ., data = arrest.train.smote)
  arrest.pred <- predict(tree.model, arrest_test, type = "class")
  kappa <- kappa2(data.frame(arrest_test$is_arrested, arrest.pred))$value
  return(kappa)
})

str(cv_results_rf_k)
mean(unlist(cv_results_rf_k))
# -> 0.293108



# 10-fold CV with Naive Bayes
cv_results_nb_k <- lapply(folds_k, function(x) {
  arrest_train <- stops_arrested[-x, ]
  arrest_test <- stops_arrested[x, ]
  arrest.train.smote <- SMOTE(is_arrested ~., data = arrest_train)
  tree.model <- naiveBayes(is_arrested ~., data = arrest.train.smote, laplace = 1)
  arrest.pred <- predict(tree.model, arrest_test, type = "class")
  kappa <- kappa2(data.frame(arrest_test$is_arrested, arrest.pred))$value
  return(kappa)
})

str(cv_results_nb_k)
mean(unlist(cv_results_nb_k))
# -> 0.2623718


### -> Best results with Kappa statistics is RF model.


### Fine tuning using caret package to try and improve results

# Splitting data in to test/train and validation (25%)
set.seed(1234)
spl_2 = sample.split(stops_arrested$is_arrested, SplitRatio = 0.75)
train_arrest_2 = subset(stops_arrested, spl_2==TRUE)
validate_arrest_2 = subset(stops_arrested, spl_2==FALSE)

# CART:
ctrl_cart <- trainControl(method = "cv", number = 10, sampling = "smote")
grid_cart <- expand.grid(.cp = seq(0.1,1,0.1))
set.seed(13)
m_cart <- train(is_arrested ~., data = train_arrest_2,
                method = "rpart",
                metric = "Kappa",
                trControl = ctrl_cart,
                tuneGrid = grid_cart)
m_cart
# -> best cp is 0.2

predict_cart <- predict(m_cart, newdata = validate_arrest_2)
table(validate_arrest_2$is_arrested, predict_cart)
kappa_cart <- kappa2(data.frame(validate_arrest_2$is_arrested, predict_cart))$value
# -> 0.2223313


#### WARNING: Don't use train() for rf or nb unless need to find fine tuned
#             parameters. The computational time is large.

# # Random Forest:
# ctrl_rf <- trainControl(method = "cv", number = 10, sampling = "smote")
# grid_rf <- expand.grid(.mtry = c(3, 7, 14, 27))
# set.seed(13)
# m_rf <- train(is_arrested ~., data = train_arrest_2,
#                 method = "rf",
#                 metric = "Kappa",
#                 trControl = ctrl_rf,
#                 tuneGrid = grid_rf)
# m_rf
# # -> best mtry = 3
# 
# predict_rf <- predict(m_rf, newdata = validate_arrest_2)
# table(validate_arrest_2$is_arrested, predict_rf)
# kappa_rf <- kappa2(data.frame(validate_arrest_2$is_arrested, predict_rf))$value
# # -> 0.329846
# 
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


# Using results (optimized parameters) of train() to build models

# Splitting data using createDataPartition to mimic train()
set.seed(3000)
spl_cdp =createDataPartition(stops_arrested$is_arrested, p = 0.75, list = FALSE)
train_arrest_cdp = stops_arrested[spl_cdp,]
test_arrest_cdp = stops_arrested[-spl_cdp,]

#apply SMOTE
train_arrest_smote <- SMOTE(is_arrested ~., data = train_arrest_cdp)

# CART
# tree.smote2 <- rpart(is_arrested ~ ., data = train.smote, cp = 0.2)
# prp(tree.smote2)
# pred.tree.smote2 <- predict(tree.smote2, newdata = test_arrest, type = "class")
# confusionMatrix(table(test_arrest$is_arrested, pred.tree.smote2))
# # Kappa = 0.2186
tree.smotecdp <- rpart(is_arrested ~ ., data = train_arrest_smote, cp = 0.2)
prp(tree.smotecdp)
pred.tree.smotecdp <- predict(tree.smotecdp, newdata = test_arrest_cdp, type = "class")
confusionMatrix(table(test_arrest_cdp$is_arrested, pred.tree.smotecdp))
# Kappa = 0.2215


# RF
tree.smote.rfcdp <- randomForest(is_arrested ~ ., data = train_arrest_smote, mtry = 3)
pred.rf.smotecdp <- predict(tree.smote.rfcdp, newdata = test_arrest_cdp, type = "class")
confusionMatrix(table(test_arrest_cdp$is_arrested, pred.rf.smotecdp))
# kappa = 0.2721


# NB
smote.nbcdp <- NaiveBayes(is_arrested ~., data = train_arrest_smote, fL = 1, usekernel = FALSE, adjust = 1)
nb.predict4cdp <- predict(smote.nbcdp, newdata = test_arrest_cdp)
confusionMatrix(table(test_arrest_cdp$is_arrested, nb.predict4cdp$class))
# Kappa = 0.2582


# Random Forest is shown to provide the best results.
# Here are the important variables in the RF model
importance(tree.smote.rfcdp)
varImpPlot(tree.smote.rfcdp)















#---------------------------------------------------------------------------
# stop_outcome

# Split the data into test and training data sets
set.seed(3)
# spl = sample.split(stops_so$stop_outcome, SplitRatio = 0.6)
# train_so = subset(stops_so, spl==TRUE)
# test_so = subset(stops_so, spl==FALSE)
spl_so =createDataPartition(stops_so$stop_outcome, p = 0.75, list = FALSE)
train_so = stops_so[spl_so,]
test_so = stops_so[-spl_so,]

prop.table(table(stops_so$stop_outcome))

#Applying CART algorithm without SMOTE
stops_so_tree <- rpart(stop_outcome ~., data = train_so, method = "class", minbucket = 1000)
prp(stops_so_tree)  
print(stops_so_tree)
#PredictCART_so = predict(stops_so_tree, type = "class")
PredictCART_so = predict(stops_so_tree, newdata = test_so, type = "class")
confusionMatrix(table(test_so$stop_outcome, PredictCART_so))
test_df <- data.frame(test_so$stop_outcome, PredictCART_so)
colnames(test_df) <- c("obs", "pred")
multiClassSummary(test_df, lev = levels(test_df$obs))
mat <- table(test_so$stop_outcome, PredictCART_so)
# Kappa = 0.091
#roc.curve(train_so$stop_outcome, PredictCART_so)
# error with ROC
#library(pROC)
#multiclass.roc(train_so$stop_outcome, PredictCART_so)
# -> No Arrests, Summons, or Written Warnings classified.

accuracy <- sum(diag(mat)) / sum(mat)
# -> 0.7069652


# Random Forest
rf.so <- randomForest(stop_outcome ~ ., data = train_so)
# -> can't allocate memory

#pred.rf.so <- predict(rf.so, type = "class")
#mat2 <- table(train_so$stop_outcome, pred.rf.so)
#roc.curve(train_so$stop_outcome, pred.rf.so)


#Naive bayes
nb.model.so <- naiveBayes(stop_outcome ~., data = train_so, laplace = 1)
nb.predict.so <- predict(nb.model.so, newdata = test_so)
# mat.nb <- table(test_so$stop_outcome, nb.predict.so)
# sum(diag(mat.nb)) / sum(mat.nb)
# # -> 0.7159086
test_df_nb <- data.frame(test_so$stop_outcome, nb.predict.so)
colnames(test_df_nb) <- c("obs", "pred")
multiClassSummary(test_df_nb, lev = levels(test_df_nb$obs))
# Kappa = 0.2319432


# Trying to apply SMOTE
train_so_smote <- SMOTE(stop_outcome ~., data = train_so)

# Redo CART with SMOTE
stops_so_tree_smote <- rpart(stop_outcome ~., data = train_so_smote, method = "class", minbucket = 1000)
prp(stops_so_tree_smote)  
print(stops_so_tree_smote)
PredictCART_so_smote = predict(stops_so_tree_smote, newdata = test_so, type = "class")
confusionMatrix(table(test_so$stop_outcome, PredictCART_so_smote))
# Kappa = 0.0901


# Redo NB with SMOTE
nb.model.so.smote <- naiveBayes(stop_outcome ~., data = train_so_smote, laplace = 1)
nb.predict.so.smote <- predict(nb.model.so.smote, newdata = test_so)
confusionMatrix(table(test_so$stop_outcome, nb.predict.so.smote))
# Kappa = 0.2193


# Undersampling
# -> smallest group is Arrest with 7300
# train_under <- ovun.sample(stop_outcome ~., data = train_so, method = "under", N = 5*7300, seed = 1)$data
# can't do; need 2 levels only


# Trying to fine tune NB parameters
#ctrl_so_nb <- trainControl(method = "cv", number = 10, sampling = "smote")
ctrl_so_nb <- trainControl(method = "cv", number = 10)
grid_so_nb <- expand.grid(.fL = c(0,1), .usekernel = c(TRUE, FALSE), .adjust = c(.1,1,10))
set.seed(13)
m_so_nb <- train(stop_outcome ~., data = train_so,
              method = "nb",
              metric = "Kappa",
              trControl = ctrl_so_nb,
              tuneGrid = grid_so_nb)


m_so_nb
# -> best fL = 0
# -> best usekernel = TRUE
# -> best adjust = 0.11

predict_so_nb <- predict(m_so_nb, newdata = test_so)
table(test_so$stop_outcome, predict_so_nb)
# -> Ticket is only outcome predicted
kappa_so_nb <- kappa2(data.frame(test_so$stop_outcome, predict_so_nb))$value
# -> 0

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
