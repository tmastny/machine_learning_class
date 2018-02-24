
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
  step_center(matches("([rgb][0][1-4][0-9]{2})|([rgb][0-9]{2}[0][1-4])")) %>%
  step_scale(matches("([rgb][0][1-4][0-9]{2})|([rgb][0-9]{2}[0][1-4])")) %>%
  step_center(all_predictors(), -matches("([rgb][0][1-4][0-9]{2})|([rgb][0-9]{2}[0][1-4])")) %>%
  step_scale(all_predictors(), -matches("([rgb][0][1-4][0-9]{2})|([rgb][0-9]{2}[0][1-4])"))
          
paint_trained <- prep(paint_recipe, training = paintings)

training_data <- bake(paint_trained, newdata = paintings)
testing_data <- bake(paint_trained, newdata = test_data)


cvCtrl = trainControl(method = "repeatedcv", number = 10, 
                      repeats = 3, classProbs = TRUE)
caret_model <- train(
  class ~ ., 
  data = training_data, 
  method = 'rf', 
  trControl = cvCtrl)

caret_model

saveRDS(caret_model, 'rf_norm_stan_edge.RDS')

write_csv(data.frame(id = 1:63, class=predict(caret_model, testing_data)), 
          'Contest1/models/norm_stan/rf_norm_stan_edge.csv')

## very similar, test scores, slightly lower





