
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
  step_scale(all_predictors()) %>%
  step_corr(all_predictors())

std_paint_trained <- prep(paint_recipe, training = paintings)

training_data <- bake(std_paint_trained, newdata = paintings)
testing_data <- bake(std_paint_trained, newdata = test_data)


control <- trainControl(
  method = "cv", 
  number = 10, 
  savePredictions = TRUE, 
  classProbs = TRUE)

algorithmList <- c('rf', 'glmnet', 'xgbTree', 'C5.0')
model_list <- caretList(
  class ~ .,
  data = training_data, 
  trControl = control, 
  methodList = algorithmList)

saveRDS(model_list, 'Contest1/models/norm_stan/corr_model_list.RDS')

results <- resamples(model_list)
summary(results)

modelCor(results)
splom(results)

pred_grab <- function(models, train_data, test_data = NULL) {
  if (!is.null(test_data)) {
    agg_data <- purrr::map_dfc(
      models, 
      ~predict(., test_data))
    return(agg_data)
  }
  agg_data <- purrr::map_dfc(
    models, 
    ~.$pred$pred[order(.$pred$rowIndex)])
  agg_data$class <- train_data$class
  agg_data
}

pred_train <- pred_grab(model_list, training_data)
pred_test <- pred_grab(model_list, test_data = testing_data)

prob_grab <- function(models, train_data, test_data = NULL) {
  if (!is.null(test_data)) {
    agg_data <- purrr::map_dfc(
      models, 
      ~predict(., test_data, type = 'prob'))
    return(agg_data)
  }
  cols <- c('cold', 'dusk', 'flowers', 'impressions', 'oval', 'scene', 'trees', 'water')
  agg_data <- purrr::map_dfc(
    models, 
    ~.$pred[order(.$pred$rowIndex), cols])
  agg_data$class <- train_data$class
  agg_data
}

prob_train <- prob_grab(model_list, training_data)
prob_test <- prob_grab(model_list, test_data = testing_data)

ensemble <- train(
  class ~ .,
  data = prob_train,
  method = 'xgbTree',
  trControl = control
)

ensemble

write_csv(data.frame(id = 1:63, class = predict(ensemble, prob_test)),
          'Contest1/models/norm_stan/ensemble_norm_stan_corr.csv')

# best training accurary = 0.5
# public leaderboard = 0.36507












