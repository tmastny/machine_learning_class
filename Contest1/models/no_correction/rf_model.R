
knitr::opts_knit$set(root.dir = here::here())

library(tidyverse)
library(recipes)
library(caret)

library(doMC)
registerDoMC(cores = 4)

paintings <- read_csv('Contest1/train.csv')
testing_data <- read_csv('Contest1/test.csv')

rf_grid = expand.grid(mtry = c(1, 2, 50, 100, 300, 500, 800, 1000, 1200))

cvCtrl = trainControl(method = "repeatedcv", number = 10, 
                      repeats = 3, classProbs = TRUE)
caret_model <- train(
  class ~ ., 
  data = paintings, 
  method = 'rf', 
  trControl = cvCtrl,
  tuneGrid = rf_grid)



write_csv(data.frame(id = 1:63, class=predict(caret_model, testing_data)), 
          'Contest1/models/no_correction/rf_sub.csv')

# current winner

