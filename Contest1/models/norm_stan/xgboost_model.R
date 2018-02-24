
knitr::opts_knit$set(root.dir = here::here())

library(tidyverse)
library(recipes)
library(caret)

library(doMC)
registerDoMC(cores = 4)

paintings <- read_csv('Contest1/train.csv')
test_data <- read_csv('Contest1/test.csv')

paint_recipe <- recipe(class ~ ., data = head(paintings))

paint_recipe <- paint_recipe %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())

std_paint_trained <- prep(paint_recipe, training = paintings)

training_data <- bake(std_paint_trained, newdata = paintings)
testing_data <- bake(std_paint_trained, newdata = test_data)

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
  method = "cv",
  number = 5,
  returnResamp = "all",
  classProbs = TRUE
)

caret_model <- train(
  class ~ ., 
  data = training_data, 
  method = 'xgbTree',
  trControl = tr_control,
  tuneGrid = xg_grid)

saveRDS(caret_model, file = 'Contest1/models/norm_stan/xgboost_model.RDS')

caret_model

write_csv(data.frame(id = 1:63, class = predict(caret_model, testing_data)), 
          'Contest1/models/norm_stan/xgboost_norm_std_sub')

