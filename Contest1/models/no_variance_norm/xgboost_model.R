
knitr::opts_knit$set(root.dir = here::here())

library(tidyverse)
library(recipes)
library(caret)

library(doMC)
registerDoMC(cores = 4)

paintings <- read_csv('Contest1/models/no_variance_norm/training_data.csv')
testing_data <- read_csv('Contest1/models/no_variance_norm/testing_data.csv')

tr_control = trainControl(
  method = "repeatedcv",
  number = 10, 
  repeats = 3, 
  classProbs = TRUE)

caret_model <- train(
  class ~ ., 
  data = paintings, 
  method = 'xgbTree',
  trControl = tr_control)

saveRDS(caret_model, file = 'Contest1/models/no_variance_norm/xgboost.RDS')

write_csv(data.frame(id = 1:63, class = predict(caret_model, testing_data)), 
          'Contest1/models/no_variance_norm/xgboost.csv')





