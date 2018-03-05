#'+r setup
knitr::opts_chunk$set(warning = FALSE, message = FALSE, cache = TRUE)

#'+r
library(tidyverse)
library(recipes)
library(caret)
library(caretEnsemble)

library(doMC)
registerDoMC(cores = 4)

paintings <- read_csv('../../train.csv')
test_data <- read_csv('../../test.csv')

scale_data <- function(data) {
  temp <- apply(data, 2, function(x) {x - mean(x)})
  as.tibble(apply(temp, 2, function(x) {x/sd(x)}))
}
training_data <- scale_data(paintings[,-1])
training_data$class <- paintings$class
testing_data <- scale_data(test_data)

#' Individual models:

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

saveRDS(model_list, 'model_list_row.RDS')
#model_list <- readRDS('model_list_row.RDS')

results <- resamples(model_list)
summary(results)

modelCor(results)
splom(results)

#' ## Out of fold

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

#' Ensemble model

control <- trainControl(
  method = "cv",
  number = 10,
  savePredictions = TRUE,
  classProbs = TRUE)

ensemble <- train(
  class ~ .,
  data = pred_train,
  method = 'rf',
  trControl = control
)
#' ## Results

max(ensemble$results$Accuracy)

write_csv(data.frame(id = 1:63, class = predict(ensemble, pred_test)),
          'ensemble_row.csv')

#' Best public: 0.49206
saveRDS(pred_train, "pred_train.RDS")









