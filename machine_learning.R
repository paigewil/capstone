#Notes:
# -> seems like stop_time is influential, but there are so many levels, the results are 
#    hard to parse, will try using hours instead
# -> Now officer ID seems to have a big impact, because it's hard to parse,
#    I'll remove for now to see the tree clearer

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

# Capstone data
stops <- read.csv("E:/Learning/Springboard Intro to Data Science/capstone/CT_cleaned_edit.csv")
str(stops)

# keeping cleaned columns
stops_col <- stops[,c(3,4,6,10,12,13,15,17,19,20,21,22,23,24)]
str(stops_col)
stops_col$stop_date <- as.POSIXct(stops_col$stop_date, "%Y-%m-%d", tz = "America/New_York")

# removing stop_outcome to run algorithm on predicting is_arrested
stops_arrest <- stops_col[,-11]

#making hour attribute
stops_arrest$stop_time_hour <- stops_arrest$stop_time
stops_arrest$stop_time_hour[stops_arrest$stop_time_hour == "0:00"] <- NA
stops_arrest$stop_time_hour <- sub(":.*", "", stops_arrest$stop_time_hour)
stops_arrest$stop_time_hour <- factor(stops_arrest$stop_time_hour, 
                                      ordered = TRUE,
                                      levels = as.character(0:23))
#stops_arrest$stop_time_hour_numeric <- as.numeric(stops_arrest$stop_time_hour) - 1

# making stop month attribute
stops_arrest$stop_month <- month(stops_arrest$stop_date)
stops_arrest$stop_month <- factor(stops_arrest$stop_month, 
                                  ordered = TRUE,
                                  levels = as.character(1:12))

#making stop day of month attribute
a <- c("01", "02", "03", "04", "05", "06", "07", "08", "09")
b <- as.character(10:31)
days_label <- c(a, b)
stops_arrest$day_of_month <- format(stops_arrest$stop_date, "%d")
stops_arrest$day_of_month <- factor(stops_arrest$day_of_month,
                                    ordered = TRUE,
                                    levels = days_label)

#removing columns with high factors for the time being until figure out how
#to deal with them
#list of removed:
# -> stop_time
# -> stop_date
# -> officer_ID
# -> violation_raw
stops_arrest_small <- stops_arrest[, c(-1, -2, -7, -12)]    


# Split the data
# -> since I have a lot of data, I want to make my testing sample a little larger
#    to have better tests to make sure I'm choosing the best algorithm
set.seed(3000)
spl = sample.split(stops_arrest_small$is_arrested, SplitRatio = 0.6)
train_small = subset(stops_arrest_small, spl==TRUE)
test_small = subset(stops_arrest_small, spl==FALSE)


#Applying CART algorithm without handling unbalanced data
stops_arrest_tree <- rpart(is_arrested ~., data = train_small, method = "class", minbucket = 1000)
prp(stops_arrest_tree)  #plot the tree
print(stops_arrest_tree)
# -> There are no splits-- all bucketed to FALSE

#trying to change parameter, minbucket, for better results
stops_arrest_tree2 <- rpart(is_arrested ~., data = train_small, method = "class", minbucket = 100)
prp(stops_arrest_tree2)
print(stops_arrest_tree2)
# -> No improvement


# Make predictions
PredictCART = predict(stops_arrest_tree, type = "class")
confusionMatrix(table(train_small$is_arrested, PredictCART))
# accuracy = 0.9767
# precision/specificity = NA
# recall/sensitivity  = 0.9767
roc.curve(train_small$is_arrested, PredictCART)
# -> 0.500

#using cross-validation to choose parameters and hopefully get better results
#CV won't work with NAs
stops_arrest_treecv <- stops_arrest_small[complete.cases(stops_arrest_small),]
set.seed(3000)
spl2 = sample.split(stops_arrest_treecv$is_arrested, SplitRatio = 0.6)
train_small2 = subset(stops_arrest_treecv, spl2==TRUE)
test_small2 = subset(stops_arrest_treecv, spl2==FALSE)

# Define cross-validation experiment
numFolds = trainControl(method = "cv", number = 10)
cpGrid = expand.grid(.cp = seq(0.1,1,0.1))
train(as.factor(is_arrested) ~ ., data = train_small2, method = "rpart", trControl = numFolds, tuneGrid = cpGrid)
# -> cp = 1

# Create a new CART model
stops_arrest_treecv2 = rpart(is_arrested ~ ., data = train_small2, method="class", cp = 1)
prp(stops_arrest_treecv2)
# -> still no splits-- all bucketed to FALSE


# Using SMOTE to fix imbalanced data to get better results from algorithms
#casting as ints and factors to use in SMOTE
# stops_arrest_smote <- stops_arrest_small
# stops_arrest_smote$search_conducted <- as.factor(stops_arrest_smote$search_conducted)
# stops_arrest_smote$contraband_found <- as.factor(stops_arrest_smote$contraband_found)
# stops_arrest_smote$is_arrested <- as.factor(stops_arrest_smote$is_arrested)
# stops_arrest_smote$stop_time_hour <- as.integer(stops_arrest_smote$stop_time_hour)
# stops_arrest_smote$stop_month <- as.integer(stops_arrest_smote$stop_month)
# stops_arrest_smote$day_of_month <- as.integer(stops_arrest_smote$day_of_month)

#redoing because SMOTE should only be performed on testing data set
stops_arrest_smote <- train_small
stops_arrest_smote$search_conducted <- as.factor(train_small$search_conducted)
stops_arrest_smote$contraband_found <- as.factor(train_small$contraband_found)
stops_arrest_smote$is_arrested <- as.factor(train_small$is_arrested)
stops_arrest_smote$stop_time_hour <- as.integer(train_small$stop_time_hour)
stops_arrest_smote$stop_month <- as.integer(train_small$stop_month)
stops_arrest_smote$day_of_month <- as.integer(train_small$day_of_month)

# set.seed(3000)
# spl3 = sample.split(stops_arrest_smote$is_arrested, SplitRatio = 0.6)
# train_smote = subset(stops_arrest_smote, spl3==TRUE)
# test_smote = subset(stops_arrest_smote, spl3==FALSE)

train.smote <- SMOTE(is_arrested ~., data = stops_arrest_smote)
tree.smote <- rpart(is_arrested ~ ., data = train.smote)
prp(tree.smote)

pred.tree.smote <- predict(tree.smote, type = "class")
confusionMatrix(table(train.smote$is_arrested, pred.tree.smote))
# accuracy = 0.8208
# precision/specificity = 0.8170
# recall/sensitivity  = 0.8232
roc.curve(train.smote$is_arrested, pred.tree.smote)
# -> 0.812
# AUC improved so using SMOTE is a better model thus far


# Showing that ROSE performs worse as a sampler
# Rose also needs the variables as factors
train.rose <- ROSE(is_arrested ~ ., data = stops_arrest_smote, seed = 1)$data
tree.rose <- rpart(is_arrested ~ ., data = train.rose)
prp(tree.rose)

pred.tree.rose <- predict(tree.rose, type = "class")
#confusionMatrix(table(train.rose$is_arrested, pred.tree.rose))
roc.curve(train.rose$is_arrested, pred.tree.rose)
# -> 0.705


# Seeing in RandomForest can improve model
# Need to remove NAs first
stops_arrest_rf <- train.smote
stops_arrest_rf <- stops_arrest_rf[complete.cases(stops_arrest_rf),]
# set.seed(3000)
# spl_rf = sample.split(stops_arrest_rf$is_arrested, SplitRatio = 0.6)
# train_rf = subset(stops_arrest_rf, spl_rf==TRUE)
# test_rf = subset(stops_arrest_rf, spl_rf==FALSE)


rf.smote <- randomForest(is_arrested ~ ., data = stops_arrest_rf)
pred.rf.smote <- predict(rf.smote, type = "class")
roc.curve(stops_arrest_rf$is_arrested, pred.rf.smote)
# -> 0.576; not an improvement

# redoing RF with na.action to simplify code
rf.smote2 <- randomForest(is_arrested ~ ., data = train.smote, na.action=na.omit)
pred.rf.smote2 <- predict(rf.smote2, type = "class")
roc.curve(train.smote$is_arrested[complete.cases(train.smote)], pred.rf.smote2)
# -> 0.575


# Testing using a Penalty Matrix instead of balancing the unbalanced dataset
# -> Want higher penalty for false negative, i.e., where say no arrest when there
#    was. Want this because we need to accurately label arrests.
PenaltyMatrix = matrix(c(0,1,6,0), byrow = TRUE, nrow = 2)
stops_arrest_tree_penalty <- rpart(is_arrested ~., data = train_small, method = "class", minbucket = 10, parms = list(loss = PenaltyMatrix))
prp(stops_arrest_tree_penalty)
print(stops_arrest_tree_penalty)

PredictCARTPenalty = predict(stops_arrest_tree_penalty, type = "class")
confusionMatrix(table(train_small$is_arrested, PredictCARTPenalty))
# accuracy = 0.9593 
# precision/specificity = 0.2781
# recall/sensitivity  = 0.9870
roc.curve(train_small$is_arrested, PredictCARTPenalty)
# -> 0.718


