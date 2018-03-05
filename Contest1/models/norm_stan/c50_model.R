
knitr::opts_knit$set(root.dir = here::here())

library(tidyverse)
library(recipes)
library(caret)

library(doMC)
registerDoMC(cores = 4)

set.seed(123)
seeds <- vector(mode = "list", length = 11)

for(i in 1:10) seeds[[i]]<- sample.int(n=1000, 4)
seeds[[11]] <- sample.int(1000, 1)


paintings <- read_csv('Contest1/train.csv')
test_data <- read_csv('Contest1/test.csv')

scale_data <- function(data) {
  temp <- apply(data, 2, function(x) {x - mean(x)})
  as.tibble(apply(data, 2, function(x) {x/sd(x)}))
}
training_data <- scale_data(paintings[,-1])
training_data$class <- paintings$class
testing_data <- scale_data(test_data)

tr_grid <- expand.grid(
  model = c('rules', 'tree'),
  winnow = c(TRUE, FALSE),
  trials = c(1, 2, 3, 4, 5, 10, 15, 20, 35))

control <- trainControl(
  method = "cv", 
  number = 10, 
  savePredictions = 'final',
  returnResamp = 'final',
  classProbs = TRUE,
  seeds = seeds)

caret_model <- train(
  class ~ ., 
  data = training_data, 
  method = 'C5.0', 
  trControl = control,
  tuneGrid = tr_grid)

caret_model

saveRDS(caret_model, 'Contest1/models/norm_stan/c50_row.RDS')

write_csv(data.frame(id = 1:63, class=predict(caret_model, testing_data)), 
          'Contest1/models/norm_stan/c50_norm_stan.csv')

# best trails = 20, model = rules, winnow = FALSE





