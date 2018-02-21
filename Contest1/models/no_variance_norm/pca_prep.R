

library(tidyverse)
library(recipes)
library(caret)

paintings <- read_csv('Contest1/train.csv')
test_data <- read_csv('Contest1/test.csv')

paint_recipe <- recipe(class ~ ., data = head(paintings))

paint_recipe <- paint_recipe %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  step_pca(all_predictors(), threshold = 0.75)

pca_paint_trained <- prep(paint_recipe, training = paintings)

training_data <- bake(pca_paint_trained, newdata = paintings)

write_csv(training_data, 'Contest1/models/no_variance_norm/training_data.csv')

testing_data <- bake(pca_paint_trained, newdata = test_data)
write_csv(testing_data, 'Contest1/models/no_variance_norm/testing_data.csv')


