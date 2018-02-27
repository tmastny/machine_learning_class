
knitr::opts_knit$set(root.dir = here::here())

library(tidyverse)
library(recipes)
library(caret)

library(doMC)
registerDoMC(cores = 4)

paintings <- read_csv('Contest1/train.csv')
test_data <- read_csv('Contest1/test.csv')

scale_data <- function(data) {
  temp <- apply(data, 2, function(x) {x - mean(x)})
  as.tibble(apply(data, 2, function(x) {x/sd(x)}))
}
training_data <- scale_data(paintings[,-1])
training_data$class <- paintings$class
testing_data <- scale_data(test_data)


control <- trainControl(
  method = "cv", 
  number = 10, 
  savePredictions = TRUE, 
  classProbs = TRUE)

caret_model <- train(
  class ~ ., 
  data = training_data, 
  method = 'xgbTree', 
  trControl = control)

caret_model

saveRDS(caret_model, 'Contest1/models/norm_stan/xgboost_row.RDS')
#caret_model <- readRDS('Contest1/models/norm_stan/xgboost_row.RDS')


write_csv(data.frame(id = 1:63, class=predict(caret_model, testing_data)), 
          'Contest1/models/norm_stan/xgboost_norm_stan_row.csv')

## training: 0.484
## public: 0.38095





