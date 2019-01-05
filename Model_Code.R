# required packages
install.packages('tidyverse')
library(lime)       # ML local interpretation
library(vip)        # ML global interpretation
library(pdp)        # ML global interpretation
library(ggplot2)    # visualization pkg leveraged by above packages
library(caret)      # ML model building
library(h2o)        # ML model building
library(data.table)
library(mltools)
library(caTools)
library(DMwR)
library(dplyr)
library(DALEX)
library(caTools)

#Initialize the h2o platform
h2o.init()
h2o.no_progress()
h2o.shutdown()

# classification data
claim <- read.csv("claims.csv", header = TRUE)

#Feature selection and 
chisq.test(claim$RepNumber,claim$FraudFound_P)
t.test(claim$RepNumber~claim$FraudFound_P) 

names(claim)
#Feature Engineered 
claim_sub <- subset(claim, select = -c(PolicyNumber,RepNumber,WeekOfMonth,DayOfWeek,DayOfWeekClaimed,WeekOfMonthClaimed,
                                       MaritalStatus,DriverRating,Days_Policy_Claim,PoliceReportFiled,WitnessPresent,
                                       NumberOfCars,Month,MonthClaimed,Year))

#Split the dataset
set.seed(123)
split = sample.split(claim_sub$FraudFound_P, SplitRatio = 0.75)
claim1_train = subset(claim_sub, split == TRUE)
claim1_test = subset(claim_sub, split == FALSE)

#OHE for test set
claim1_test <- one_hot(as.data.table(claim1_test))
names(claim1_test)
names(claim1_cat)
#Sample the train dataset

claim1_train$FraudFound_P <- as.factor(claim1_train$FraudFound_P)
table(claim1_train$FraudFound_P)


claim_Smote <- SMOTE(FraudFound_P ~ ., claim1_train, perc.over = 600,perc.under=118)

table(claim_Smote$FraudFound_P)

#One hot encoding
claim_Smote$FraudFound_P <- as.integer(claim_Smote$FraudFound_P)
claim_cat <- one_hot(as.data.table(claim_Smote))

claim_cat <- as.data.frame(claim_cat)

table(claim_cat$FraudFound_P)



# Encoding the target feature as factor only after one-hot encoding
claim_cat$FraudFound_P <- as.factor(claim_cat$FraudFound_P)
typeof(claim_cat$FraudFound_P)
levels(claim_cat$FraudFound_P) <- c('0','1')
levels(claim_cat$FraudFound_P)
# convert to h2o object
library(readr)
claim1_train <- read.csv("claim_API.csv")
names(claim1_train)
df.h2o <- as.h2o(claim1_train)
library(h2o)

h2o.init()
df.h2o$FraudFound_P <- as.factor(df.h2o$FraudFound_P)
# create train, validation, and test splits
set.seed(123)
splits <- h2o.splitFrame(df.h2o, ratios = c(.75, .10), destination_frames = c("trainset","validset","testset"))
names(splits) <- c("trainset","validset","testset")


# variable names for response & features
y <- "FraudFound_P"
x <- setdiff(names(df.h2o), y)

############################################################################################################
max <- .7
aml <- h2o.automl(y = y, x = x,
                  training_frame = splits$train,
                  max_models = 10,
                  stopping_metric = "AUTO",
                  seed = 123,
                  nfolds = 10,
                  keep_cross_validation_predictions = TRUE)

##AML Leaderboard view
aml@leader
lb <- aml@leaderboard
print(lb, n = nrow(lb))


perf <- h2o.performance(aml@leader,test)
print(perf)
h2o.precision(perf)
h2o.recall(perf)
h2o.accuracy(perf)
h2o.auc(perf)
plot(perf,type="roc")

gbm <-  h2o.gbm(
  x = x, 
  y = y,
  training_frame = splits$train,
  validation_frame = splits$valid,
  ntrees = 500,
  max_depth = 7,
  stopping_metric = "AUC",    
  stopping_rounds = 10,         
  stopping_tolerance = 0.005,
  seed = 123,
  ignore_const_cols = FALSE,
  nfolds = 10,
  keep_cross_validation_predictions = TRUE
)

h2o.auc(gbm, valid = TRUE)
#[1] 0.9745241
perf_gbm <- h2o.performance(gbm,splits$testset)
print(perf_gbm)
h2o.precision(perf_gbm)
h2o.recall(perf_gbm)
h2o.accuracy(perf_gbm)
h2o.auc(perf_gbm)
plot(perf_gbm,type="roc")
#[1] 0.9723354
############################Totally new real world test set##########

perf_gbm_r <- h2o.performance(gbm,test)
print(perf_gbm_r)
h2o.precision(perf_gbm)
h2o.recall(perf_gbm)
h2o.accuracy(perf_gbm)
h2o.auc(perf_gbm)
plot(perf_gbm,type="roc")

perf_gbm_t <- h2o.performance(gbm,valid = TRUE)
print(perf_gbm_t)
perf_gbm <- h2o.performance(gbm,splits$testset)
print(perf_gbm)
perf_gbm_r <- h2o.performance(gbm,test)
print(perf_gbm_r)

###################################
#XGBOOST
xgb <-  h2o.xgboost(
  x = x, 
  y = y,
  training_frame = splits$train,
  validation_frame = splits$valid,
  ntrees = 500,
  max_depth = 7,
  stopping_metric = "AUC",    
  stopping_rounds = 10,         
  stopping_tolerance = 0.005,
  seed = 123,
  ignore_const_cols = FALSE,
  nfolds = 10,
  keep_cross_validation_predictions = TRUE
)

h2o.auc(xgb, valid = TRUE)
#[1] 0.9745241
perf_xgb_t <- h2o.performance(xgb,valid = TRUE)
print(perf_xgb_t)
perf_xgb <- h2o.performance(xgb,splits$testset)
print(perf_xgb)
perf_xgb_r <- h2o.performance(xgb,test)
print(perf_xgb_r)
h2o.precision(perf_gbm)
h2o.recall(perf_gbm)
h2o.accuracy(perf_gbm)
h2o.auc(perf_gbm)
plot(perf_gbm,type="roc")

#####
library(caTools)
real <- read.csv("claims.csv")
names(claim1_train)
real_sub <- subset(real, select = c(AccidentArea,Sex,MaritalStatus,Age,
                           Fault,VehicleCategory,
                           FraudFound_P,Deductible,AgentType,         
                           AddressChange_Claim,BasePolicy))
set.seed(123)
split = sample.split(real_sub$FraudFound_P, SplitRatio = 0.95)
claim1_train = subset(real_sub, split == TRUE)
claim1_test = subset(real_sub, split == FALSE)


#########################################
#META LEARNING
stack <- h2o.stackedEnsemble(x = x,
                             y = y,
                             training_frame = splits$trainset,
                             base_models = list(gbm, xgb))
h2o.auc(h2o.performance(stack, test))
# 0.7570171

# Train a stacked ensemble using GBM as the metalearner algorithm
# The metalearner will use GBM default values
stack_gbm <- h2o.stackedEnsemble(x = x,
                                 y = y,
                                 training_frame = splits$trainset,
                                 base_models = list(gbm, xgb),
                                 metalearner_algorithm = "gbm")

stack_deep <- h2o.stackedEnsemble(x = x,
                                 y = y,
                                 training_frame = splits$trainset,
                                 base_models = list(gbm, xgb),
                                 metalearner_algorithm = "deeplearning")
perf_stack <- h2o.performance(stack, test)
print(perf_xgb_r)
h2o.auc(perf_stack)
h2o.auc(perf_gbm_r)
h2o.auc(perf_xgb_r)
h2o.auc(perf_xgb)
print(perf_xgb)
h2o.accuracy(perf_xgb_r)
h2o.F1(perf_xgb)
h2o.F1(perf_xgb_r)
h2o.auc(perf_stack_gbm)
h2o.auc(perf_stack_deep)
h2o.F1(perf_xgb_r)
h2o.confusionMatrix(perf_xgb_r)
h2o.confusionMatrix(perf_gbm_r)
perf_stack_gbm <- h2o.performance(stack_gbm, test)
print(perf_stack_gbm)
plot(perf_xgb_r)
perf_stack_deep <- h2o.performance(stack_deep, test)
print(perf_stack_deep)
h2o.recall(perf_xgb_r)
########################
install.packages("devtools") 
install_github("AppliedDataSciencePartners/xgboostExplainer")

glm <- h2o.glm(
  x = x, 
  y = y, 
  training_frame = splits$train,
  validation_frame = splits$valid,
  family = "binomial",
  balance_classes = TRUE,
  seed = 123,
  ignore_const_cols = FALSE,
  nfolds = 10,
  keep_cross_validation_predictions = TRUE
)




perf_glm_t <- h2o.performance(glm,valid = TRUE)
print(perf_glm_t)
perf_glm <- h2o.performance(glm,splits$testset)
print(perf_glm)
perf_glm_r <- h2o.performance(glm,test)
print(perf_glm_r)

dnn <- h2o.deeplearning(
  x = x, 
  y = y, 
  training_frame = splits$train,
  validation_frame = splits$valid,
  balance_classes = TRUE,
  activation="TanhWithDropout",
  stopping_metric = "AUC",
  hidden=c(100,100),       ## default: 2 hidden layers with 200 neurons each
  epochs=2,
  ignore_const_cols = FALSE,
  variable_importances=T,
  nfolds = 10,
  keep_cross_validation_predictions = TRUE
)

perf_dnn_t <- h2o.performance(dnn,valid = TRUE)
print(perf_dnn_t)
perf_dnn <- h2o.performance(dnn,splits$testset)
print(perf_dnn)
perf_dnn_r <- h2o.performance(dnn,test)
print(perf_dnn_r)

rf <- h2o.randomForest(
  x = x, 
  y = y,
  training_frame = splits$train,
  validation_frame = splits$valid,
  balance_classes = TRUE,
  ntrees = 1000,
  stopping_metric = "f1",    
  stopping_rounds = 10,         
  stopping_tolerance = 0.005,
  seed = 123,
  ignore_const_cols = FALSE,
  nfolds = 10,
  keep_cross_validation_predictions = TRUE
)


perf_rf_t <- h2o.performance(rf,valid = TRUE)
print(perf_rf_t)
perf_rf <- h2o.performance(rf,splits$testset)
print(perf_rf)
perf_rf_r <- h2o.performance(rf,test)
print(perf_rf_r)
