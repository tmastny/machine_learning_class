
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
  model = 'rules',
  winnow = FALSE,
  trials = c(1, 2, 3, 4, 5, 10, 15, 20, 35))

cvCtrl = trainControl(method = "repeatedcv", number = 10, 
                      repeats = 3, classProbs = TRUE)
caret_model <- train(
  class ~ ., 
  data = training_data, 
  method = 'C5.0', 
  trControl = cvCtrl,
  tuneGrid = tr_grid)

caret_model

write_csv(data.frame(id = 1:63, class=predict(caret_model, testing_data)), 
          'Contest1/models/norm_stan/c50_norm_stan.csv')

# best trails = 20, model = rules, winnow = FALSE





