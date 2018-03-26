library(keras)



model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 32, input_shape = c(784)) %>% 
  layer_activation('relu') %>% 
  layer_dense(units = 10) %>% 
  layer_activation('softmax')

summary(model)