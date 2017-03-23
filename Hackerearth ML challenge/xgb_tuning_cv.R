library(doParallel)
library(xgboost)
library(caret)
library(plyr)

set.seed(1951)
registerDoParallel(3,cores=4)
getDoParWorkers()

mat=rbind(train,test)
mat=data.matrix()
cl=class

# xgboost fitting with arbitrary parameters
xgb_params_1 = list(
  objective = "binary:logistic",                                               # binary classification
  eta = 0.3,                                                                  # learning rate
  max.depth = 10,
  nthread = 2, 
  min_child_weight = 1, 
  subsample = 0.5, 
  colsample_bytree = 1, 
  num_parallel_tree = 1,
  gamma = 3,
  # max tree depth
  eval_metric = "auc"                                                          # evaluation/loss metric
)


# fit the model with the arbitrary parameters specified above
xgb_1 = xgboost(data = train,
                label = cl,
                params = xgb_params_1,
                nrounds = 400,                                                 # max number of trees to build
                verbose = TRUE,                                         
                print.every.n = 1,
                early.stop.round = 10                                          # stop if no improvement within 10 trees
)

pred=predict(xgb_1,test)

# cross-validate xgboost to get the accurate measure of error
xgb_cv_1 = xgb.cv(params = xgb_params_1,
                  data = train,
                  label = cl,
                  nrounds = 50, 
                  nfold = 5,                                                   # number of folds in K-fold
               #   prediction = TRUE,                                           # return the prediction using the final model 
                  showsd = TRUE,                                               # standard deviation of loss across folds
                  stratified = TRUE,                                           # sample is unbalanced; use stratified sampling
                  verbose = TRUE,
                  print.every.n = 1, 
                  early.stop.round = 10
)


# set up the cross-validated hyper-parameter search
xgb_grid_1 = expand.grid(
  nrounds = 10,
  eta = c(0.01, 0.001, 0.0001),
  max_depth = c(2, 4, 6, 8, 10),
  gamma = 1,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 0.5
)

# pack the training control parameters
xgb_trcontrol_1 = trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = "all",                                                        # save losses across all models
  classProbs = TRUE,                                                           # set to TRUE for AUC to be computed
  summaryFunction = twoClassSummary,
  allowParallel = TRUE
)


# train the model for each parameter combination in the grid, 
#   using CV to evaluate
cl=make.names(cl)
xgb_train_1 = train(
  x = train,
  y = cl,
  trControl = xgb_trcontrol_1,
  tuneGrid = xgb_grid_1,
  method = "xgbTree",
  verbose = TRUE
)