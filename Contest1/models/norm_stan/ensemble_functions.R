oof_grab <- function(models, train_data, test_data = NULL, type = 'raw') {
  if (!is.null(test_data)) {
    if (is.data.frame(test_data)) test_data = rep(list(test_data), length(models))
    agg_data <- purrr::map2_dfc(
      models,
      test_data,
      ~predict(.x, .y, type = type))
    agg_data$class <- test_data$class # need to check [[1]] for all cases
    return(agg_data)
  }
  if (type == 'raw') {
    if (is.data.frame(train_data)) train_data = list(train_data) 
    agg_data <- purrr::map_dfc(
      models,
      ~.$pred$pred[order(.$pred$rowIndex)])
    agg_data$class <- train_data$class
    return(agg_data)
  }
  cols <- c('cold', 'dusk', 'flowers', 'impressions', 'oval', 'scene', 'trees', 'water')
  agg_data <- purrr::map_dfc(
    models,
    ~.$pred[order(.$pred$rowIndex), cols])
  agg_data$class <- train_data$class
  agg_data
}

confusion_matrix <- function(model, data) {
  pred <- predict(model, newdata = data)
  actual <- data$class
  t <- as.matrix(table(pred, actual))
  
  acc <- sum(diag(t))/sum(t)
  acc_class <- diag(t)/apply(t, 1, sum)
  pre_class <- diag(t)/apply(t, 2, sum)
  summmary_table <- data.frame(
    correct = diag(t),
    total = apply(t, 2, sum),
    acc = acc_class, pre = pre_class)

  list(matrix = t, model = model$method, accuracy = acc, summary = summmary_table)
}


## transformations

apply_recipe <- function(rec_obj, train_data, test_data) {
  rec_trained <- prep(rec_obj, training = train_data)
  list(
    training = bake(rec_trained, newdata = train_data),
    testing = bake(rec_trained, newdata = test_data)
  )
}

scaled <- function(train_data, test_data) {
  rec_obj <- recipe(class ~ ., data = head(train_data)) %>%
    step_center(all_predictors()) %>%
    step_scale(all_predictors())
  
  apply_recipe(rec_obj, train_data, test_data)
}

corred <- function(train_data, test_data) {
  rec_obj <- recipe(class ~ ., data = head(train_data)) %>%
    step_center(all_predictors()) %>%
    step_scale(all_predictors()) %>%
    step_corr(all_predictors())
  
  apply_recipe(rec_obj, train_data, test_data)
}

row_scaled <- function(train_data, test_data) {
  scale_data <- function(data) {
    temp <- apply(data, 2, function(x) {x - mean(x)})
    as.tibble(apply(temp, 2, function(x) {x/sd(x)}))
  }
  training_data <- scale_data(train_data[,-1])
  training_data$class <- train_data$class
  
  ## need to fix include/not include column for testing vs. validation data
  testing_data <- scale_data(test_data)
  testing_data$class <- test_data$class
  list(training = training_data, testing = testing_data)
}