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
