
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

tr_grid <- expand.grid(
  alpha = c(0.008, 0.009, 0.01, .0125, 0.015, 0.0175, 0.2),
  lambda = seq(0.0001, 1, length = 100)
)

cvCtrl = trainControl(method = "repeatedcv", number = 10, 
                      repeats = 10, classProbs = TRUE)
caret_model <- train(
  class ~ ., 
  data = training_data, 
  method = 'glmnet', 
  trControl = cvCtrl,
  tuneGrid = tr_grid)

caret_model

write_csv(data.frame(id = 1:63, class=predict(caret_model, testing_data)), 
          'Contest1/models/norm_stan/glmnet_norm_stan.csv')


# current best parameters with 10-fold 10-repeat CV:
# alpha = 0.01 and lambda = 0.3031273/0.222
# alpha 0.0175 and lambda = 0.0708



