
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

ada_grid <- expand.grid(
  maxdepth = c(10, 15, 25),
  mfinal = c(50, 100, 150)
)

tr_control = trainControl(
  method = "repeatedcv",
  number = 5,
  returnResamp = "all",
  classProbs = TRUE
)

caret_model <- train(
  class ~ ., 
  data = training_data, 
  method = 'AdaBag',
  trControl = tr_control,
  tuneGrid = ada_grid)


caret_model

write_csv(data.frame(id = 1:63, class = predict(caret_model, testing_data)), 
          'Contest1/models/norm_stan/adabag_norm_std_sub')

