
attach(mydata)

# label encode for ordinal categorical variableS
library(tidyverse)
mydata2 <- mydata

mydata2$income <- factor(as.integer(mydata$income), levels = c(1,2), ordered = T)
mydata2$education <- factor(as.integer(mydata$education), levels = c(1,2,3,4,5,6,7), ordered = T)

mydata2$education <- as.integer(mydata$education)
mydata2$income <- as.integer(mydata$income)

# one-hot-encode for nominal categorical variables
# https://stackoverflow.com/questions/52539750/r-how-to-one-hot-encoding-a-single-column-while-keep-other-columns-still
mydata2 <- mydata2 %>% 
  mutate(value = 1) %>% spread(workclass, value, fill = 0) %>% 
  mutate(value = 1) %>% spread(marital.status, value, fill = 0) %>% 
  mutate(value = 1) %>% spread(occupation, value, fill = 0) %>% 
  mutate(value = 1) %>% spread(race, value, fill = 0) %>% 
  mutate(value = 1) %>% spread(gender, value, fill = 0) %>% 
  mutate(value = 1) %>% spread(native.country, value, fill = 0)

# min-max normalization for numerical values
min_max_norm <- function(x){
  (x-min(x))/(max(x)-min(x))
}

mydata_norm <- mydata2 %>% 
  select(everything()) %>% 
  mutate(age = min_max_norm(age)) %>% 
  mutate(fnlwgt = min_max_norm(fnlwgt)) %>% 
  mutate(educational.num = min_max_norm(educational.num)) %>% 
  mutate(capital.gain= min_max_norm(capital.gain)) %>% 
  mutate(capital.loss = min_max_norm(capital.loss)) %>% 
  mutate(hours.per.week = min_max_norm(hours.per.week))
# split data to train and test
library(caret)
set.seed(1234)

train_index <- createDataPartition(mydata_norm$income,p = .8,list = F) # this function is used since it creates stratified folds by default
df_train_norm <- mydata_norm[train_index,]
df_test_norm <- mydata_norm[-train_index,]


