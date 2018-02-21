
knitr::opts_knit$set(root.dir = here::here())

library(tidyverse)
library(recipes)
library(caret)

library(doMC)
registerDoMC(cores = 4)

paintings <- read_csv('Contest1/models/pca_data.csv')
testing_data <- read_csv('Contest1/models/testing_data.csv')

xg_grid = expand.grid(
  nrounds = 1000,
  eta = c(0.01, 0.001, 0.0001),
  max_depth = c(2, 4, 6, 8, 10),
  gamma = 1,
  colsample_bytree = c(0.4, 0.7, 1.0), 
  min_child_weight = c(0.5, 1, 1.5),
  subsample = 1
)


tr_control = trainControl(
  method = "cv",
  number = 5,
  returnResamp = "all",                                                        # save losses across all models
  classProbs = TRUE
)

caret_model <- train(
  class ~ ., 
  data = paintings, 
  method = 'xgbTree',
  trControl = tr_control,
  tuneGrid = xg_grid)

saveRDS(caret_model, file = 'Contest1/models/xgboost.RDS')

write_csv(data.frame(id = 1:63, class = predict(caret_model, testing_data)), 
          'Contest1/models/pca_xgboost.csv')





