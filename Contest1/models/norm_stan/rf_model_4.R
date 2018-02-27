
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


cvCtrl = trainControl(method = "cv", number = 10, 
                      repeats = 3, classProbs = TRUE)
caret_model <- train(
  class ~ ., 
  data = training_data, 
  method = 'rf', 
  trControl = cvCtrl)

caret_model

write_csv(data.frame(id = 1:63, class=predict(caret_model, testing_data)), 
          'Contest1/models/norm_stan/rf_norm_stan_row.csv')

## public: 0.50793
## start building model list





