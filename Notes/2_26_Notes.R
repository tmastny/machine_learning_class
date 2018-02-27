library(Matrix)
library(ISLR)
library(xgboost)
data("Default")

set.seed(10)
train <- sample(1:nrow(Default), nrow(Default)/2)

shrink = (1:4)/10
trees = c(10,100,500)
depth = 1:8

pred.err = expand.grid(shrink=shrink,tree=trees,depth=depth)
pred.err$error = NA

inputTrain = sparse.model.matrix(~.,Default[train,-1])
outputTrain = (Default[train,1]=="Yes")
inputTest = sparse.model.matrix(~.,Default[-train,-1])
outputTest = (Default[-train,1]=="Yes")

for (i in 1:nrow(pred.err)) {
  f = xgboost(inputDat,label=outputDat,nrounds=pred.err$tree[i],verbose=0,
              eta=pred.err$shrink[i],max.depth=pred.err$depth[i])
  res = predict(f,newdata=inputDat)
  table((res>0.5),Default$default)
  
}



