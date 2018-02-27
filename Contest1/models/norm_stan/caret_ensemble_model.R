
knitr::opts_knit$set(root.dir = here::here())

library(tidyverse)
library(recipes)
library(caret)
library(caretEnsemble)

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


control <- trainControl(
  method="repeatedcv", 
  number=10, 
  repeats=3, 
  savePredictions=TRUE, 
  classProbs=TRUE)

algorithmList <- c('rf', 'glmnet', 'knn', 'svmRadial')
model_list <- caretList(
  class ~ .,
  data = training_data, 
  trControl = control, 
  methodList = algorithmList)

results <- resamples(model_list)
summary(results)

modelCor(results)
splom(results)


pred_train <- purrr::map_df(model_list, ~predict(., training_data))
pred_train$class <- training_data$class

pred_test <- purrr::map_df(model_list, ~predict(., testing_data))

ensemble <- train(
  class ~ .,
  data = pred_train,
  method = 'rf',
  trControl = control
)

ensemble

write_csv(data.frame(id = 1:63, class = predict(ensemble, pred_test)),
          'Contest1/models/norm_stan/ensemble_norm_stan.csv')

# public leaderboard: 0.49206













