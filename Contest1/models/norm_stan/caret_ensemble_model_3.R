
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

algorithmList <- c('rf', 'glmnet', 'xgbTree', 'C5.0')
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

prob_data <- function(model_list, data) {
  agg_data <- purrr::map(
    model_list, 
    ~as.matrix(predict(., data, type = 'prob')))
  
  agg_data <- as.tibble(Reduce('+', agg_data)/8)
  agg_data$class <- data$class
  agg_data
}

prob_train <- prob_data(model_list, training_data)
prob_test  <- prob_data(model_list, testing_data)

alt_prob_data <- function(model_list, data) {
  agg_data <- purrr::map_dfc(
    model_list, 
    ~predict(., data, type = 'prob'))
  agg_data$class <- data$class
  agg_data
}

alt_prob_train <- alt_prob_data(model_list, training_data)
alt_prob_test <- alt_prob_data(model_list, testing_data)

ensemble <- train(
  class ~ .,
  data = pred_train,
  method = 'rf',
  trControl = control
)

ensemble


write_csv(data.frame(id = 1:63, class = predict(ensemble, pred_test)),
          'Contest1/models/norm_stan/alt_prob_ensemble_norm_stan.csv')

# saveRDS(model_list, 'Contest1/models/norm_stan/model_list.RDS')
# public leaderboard: 0.47619
# alt_prob = 0.4444












