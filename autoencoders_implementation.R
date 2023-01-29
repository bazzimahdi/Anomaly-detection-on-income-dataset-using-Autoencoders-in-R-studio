library(tidyverse)
# y is the same as x, since the model will try to reconstruct the data after reduction in hidden layer in auto-encoder
y_train <- df_train_norm

# test subset to validate the model against over-fitting
y_test <- df_test_norm

# transform the data to matrix for auto-encoders
y_train <- as.matrix(y_train)
y_test <- as.matrix(y_test)

# auto-encoder architecture
library(keras)

model <- keras_model_sequential()

model %>% 
  # the encoder stage
  layer_dense(units = 64,activation = 'relu',input_shape = (ncol(y_train))) %>%
  # layer_batch_normalization() %>% 
  layer_dense(units = 32,activation = 'relu') %>% 
  # layer_batch_normalization() %>%
  layer_dense(units = 16,activation = 'relu') %>% 
  # layer_batch_normalization() %>%
  layer_dense(units = 8,activation = 'relu') %>%
  # layer_batch_normalization() %>%
  # layer_dense(units = 4,activation = 'relu') %>%
  
  # the decoder stage
  # layer_dense(units = 8,activation = 'relu') %>% 
  layer_dense(units = 16,activation = 'relu') %>%
  # layer_batch_normalization() %>%
  layer_dense(units = 32,activation = 'relu') %>% 
  # layer_batch_normalization() %>%
  layer_dense(units = 64,activation = 'relu') %>% 
  # layer_batch_normalization() %>%
  layer_dense(units = ncol(y_train))

summary(model)

# compile the mode
model %>% 
  compile(loss = 'mean_squared_error',
          optimizer = 'adam',
          metrics = c('acc')
         )

# add early stopping to monitor the over fitting of the model and decide the optimal number of epochs
early_stopping <- callback_early_stopping(monitor ='val_loss', patience = 6)

# add checkpoint to save best model weights after each epoch
checkpoint <- callback_model_checkpoint(
  filepath = "D:\\model\\model.hdf5", 
  save_best_only = TRUE, 
  save_freq = 'epoch',
  verbose = 1,
  
)

# fit the model to train``
model %>%
  fit(
    x = y_train,
    y = y_train,
    verbose = 1,  # verbose is set 1 to show the training process and metrics
    epochs = 1000,
    batch_size = 512,
    validation_data = list(y_test,y_test),
    callbacks = list(checkpoint,early_stopping)
  )

# load the trained model to save time and memmory each time we run the code
load_model_weights_tf(model, "D:\\model\\model.hdf5")

# calculate the mse for each instance in the data using apply function to create an iterator
pred_train <- predict(model,y_train)
mse_train <- apply((y_train - pred_train)^2, 1,sum) #return the mse for each instance in the sample using iteration

pred_test <- predict(model,y_test)
mse_test <- apply((y_test - pred_test)^2, 1,sum) #return the mse for each instance in the sample using iteration

## plot the density plot of the mse of the train data
# code from https://www.statmethods.net/graphs/density.html
x <- mse_train
h <- hist(x,breaks = 100, col = 'red', xlab = "MSE", main = 'Histogram with Normal Curve for train data')
xfit <- seq(min(x),max(x),length = 40)
# fit the normal distribution curve
yfit <- dnorm(xfit,mean = mean(x),sd = sd(x))
# scale the curve for the length of the data
yfit <- yfit*diff(h$mids[1:2]*length(x))
# plot the density curve line
lines(xfit,yfit,col = 'blue',lwd =2)
# boxplot showing outliers
boxplot(mse_train)

x_2 <- mse_test
h_2 <- hist(x_2,breaks = 100,col= 'red', xlab = "MSE", main = "Histogram with Normal Curve for test data")
xfit_2 <- seq(min(x_2),max(x_2),length = 40)
yfit_2 <- dnorm(xfit_2,mean = mean(x_2),sd = sd(x_2))
yfit_2 <- yfit_2*diff(h_2$mids[1:2]*length(x_2))
lines(xfit_2,yfit_2,col = 'blue',lwd = 2)

mean(mse_test)
mean(mse_train) # --> mean of errors for train and test subset is similar, hence overfitting is not taking place

# calculate thresholds from train data based on 99% percentile
threshold = quantile(mse_train, probs = 0.999,)
threshold
plot(mse_train,col='blue')
abline(a = threshold,b = 0,col= 'red',lw = 4) # plot the anomalies boundary line on the scatter plot

plot(mse_test,col = 'blue')
abline(a = threshold,b = 0,col= 'red',lw = 4)

re_train <- as.data.frame(mse_train)
re_test <- as.data.frame(mse_test)

re_train %>% 
  filter(mse_train>threshold) %>% 
  count() # 34 anomalies are found in train subset

anomalies_train <- re_train %>% # create anomalies train subset 
  filter(mse_train>threshold)
  
re_test %>% 
  filter(mse_test>threshold) %>% 
  count() # 8 anomalies are found in test subset

anomalies_test <- re_test %>% # create anomalies test subset
  filter(mse_test>threshold)

anomalies_index_train <- as.numeric(rownames(anomalies_train)) # get the index of anomaly points in train subset
anomalies_index_test <- as.numeric(rownames(anomalies_test)) # get the index of anomaly points in test subset
mydata_anomalies <- mydata[append(anomalies_index_test,anomalies_index_train),] # get all anomalies in the original dataset by concatenating the indexes of train and test anomalies
write.csv(mydata_anomalies,"D:\\data\\anomalies.csv")
