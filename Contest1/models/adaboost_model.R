
knitr::opts_knit$set(root.dir = here::here())

library(tidyverse)
library(recipes)
library(caret)

library(doMC)
registerDoMC(cores = 5)

paintings <- read_csv('Contest1/models/pca_data.csv')

testing_data <- read_csv('Contest1/models/testing_data.csv')

cvCtrl = trainControl(method = "repeatedcv", number = 10, 
                      repeats = 3, classProbs = TRUE)
caret_model <- train(class ~ ., data = paintings, method = 'AdaBoost.M1', 
                     trControl = cvCtrl)

saveRDS(caret_model, file = 'Contest1/models/pca_adaboost.RDS')

write_csv(data.frame(id = 1:63, class = predict(caret_model, testing_data)), 
          'Contest1/models/pca_adaboost.csv')

