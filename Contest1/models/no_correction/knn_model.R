
knitr::opts_knit$set(root.dir = here::here())

library(tidyverse)
library(recipes)
library(caret)

library(doMC)
registerDoMC(cores = 4)

paintings <- read_csv('Contest1/train.csv')
testing_data <- read_csv('Contest1/test.csv')

cvCtrl = trainControl(method = "repeatedcv", number = 10, 
                      repeats = 3, classProbs = TRUE)
caret_model <- train(
  class ~ ., 
  data = paintings, 
  method = 'kknn', 
  trControl = cvCtrl)



write_csv(data.frame(id = 1:63, class=predict(caret_model, testing_data)), 
          'Contest1/models/no_correction/knn_sub.csv')


# good attempt, second place after RF. Should do more tunning
# try scaling and centering

