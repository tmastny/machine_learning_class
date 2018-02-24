
knitr::opts_knit$set(root.dir = here::here())

library(tidyverse)
library(recipes)
library(caret)

library(doMC)
registerDoMC(cores = 4)

paintings <- read_csv('Contest1/train.csv')
testing_data <- read_csv('Contest1/test.csv')

xg_grid = expand.grid(
  nrounds = 10000,
  eta = 0.3,
  max_depth = 6:20,
  gamma = 0,
  colsample_bytree = 0.7, 
  min_child_weight = c(0.5, 1, 1.5),
  subsample = 0.8
)


tr_control = trainControl(
  method = "repeatedcv",
  number = 4, 
  repeats = 3, 
  returnData = FALSE,
  returnResamp = "all",
  classProbs = TRUE)

caret_model <- train(
  class ~ ., 
  data = paintings, 
  method = 'xgbTree',
  trControl = tr_control)

saveRDS(caret_model, file = 'Contest1/models/no_correction/xgboost.RDS')

write_csv(data.frame(id = 1:63, class = predict(caret_model, testing_data)), 
          'Contest1/models/no_correction/xgboost.csv')






