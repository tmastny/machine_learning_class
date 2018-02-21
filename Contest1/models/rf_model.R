
knitr::opts_knit$set(root.dir = here::here())

library(tidyverse)
library(recipes)
library(caret)

library(doMC)
registerDoMC(cores = 4)

paintings <- read_csv('Contest1/models/pca_data.csv')

cvCtrl = trainControl(method = "repeatedcv", number = 10, 
                      repeats = 3, classProbs = TRUE)
caret_model <- train(y ~ ., data = paintings, method = 'rf', 
                     trControl = cvCtrl)



write_csv(data.frame(id = 1:63, class=predict(caret_model, test_data)), 
          'Contest1/models/pca_rf.csv')

