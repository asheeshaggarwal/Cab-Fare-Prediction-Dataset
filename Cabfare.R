rm(list = ls())

x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')

#install.packages(x)
lapply(x, require, character.only = TRUE)
rm(x)

require(dplyr)
require(lubridate)
require(geosphere)

getwd()
#setwd("cab fare pred")

train = read.csv('train_cab.csv')
test=read.csv('test.csv')
str(train)
str(test)

train$fare_amount = as.numeric(as.character(train$fare_amount))

missing_val = data.frame(apply(train,2,function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)

train=na.omit(train)
sum(is.na(train))

summary(train)

sum(train$fare_amount<0)

train = train %>% 

  filter(fare_amount>0,fare_amount<200,
         passenger_count>=1,passenger_count<7,
   pickup_latitude!=0,pickup_latitude<90, pickup_latitude> -90,
         dropoff_latitude!=0,dropoff_latitude<90,dropoff_latitude> -90,
         pickup_longitude!=0,
         dropoff_longitude!=0)

train = mutate(train, id = row_number())

for (i in (train$id[train$pickup_longitude>0])){
  n=train$pickup_longitude[train$id==i]
  train$pickup_longitude[train$id==i] = train$pickup_latitude[train$id==i]
  train$pickup_latitude[train$id==i]=n
  
  n=train$dropoff_longitude[train$id==i]
  train$dropoff_longitude[train$id==i] = train$dropoff_latitude[train$id==i]
  train$dropoff_latitude[train$id==i]=n
}

train[train$pickup_latitude<39,]
train[train$dropoff_longitude>-72.99,]

train = train %>% 
  filter(pickup_latitude>=39,
         dropoff_latitude>=39,
         pickup_longitude< -73,
         dropoff_longitude< -73)
str(train)

#Feature engineering
train$pickup_datetime = ymd_hms(train$pickup_datetime)
test$pickup_datetime = ymd_hms(test$pickup_datetime)

train = train %>% 
  filter(!is.na(pickup_datetime))

train = train %>% 
mutate(
  year = as.numeric(year(pickup_datetime)),
  month = as.numeric(month(pickup_datetime)),
  day = as.numeric(day(pickup_datetime)),
  dayofweek = as.numeric(wday(pickup_datetime)),
  hour = as.numeric(hour(pickup_datetime)))

test = test %>% 
  mutate(
    year = as.numeric(year(pickup_datetime)),
    month = as.numeric(month(pickup_datetime)),
    day = as.numeric(day(pickup_datetime)),
    dayofweek = as.numeric(wday(pickup_datetime)),
    hour = as.numeric(hour(pickup_datetime)))

str(test)

train = train %>% 
mutate(dist = distHaversine(cbind(pickup_longitude, pickup_latitude), cbind(dropoff_longitude, dropoff_latitude), r = 6371))

test = test %>% 
  mutate(dist = distHaversine(cbind(pickup_longitude, pickup_latitude), cbind(dropoff_longitude, dropoff_latitude), r = 6371))


train = train %>% 
  filter(dist < 200, dist > 0)

train = train[,-8]
train = train[,-2]
test = test[,-1]
str(train)


#Model development

train.index = createDataPartition(train$fare_amount, p = .70, list = FALSE)
training = train[ train.index,]
testing  = train[-train.index,]


#----------------linear reg-----------


lr_model= lm(fare_amount~.,training)
summary(lr_model)


#check model performance on test data-
lr_test= predict(lr_model,testing[-c(1)])

#RMSE calculation for test data-
RMSE(testing[,1],lr_test)
#RMSE_test=7.763067

regr.eval(testing[,1],lr_test,stats = 'mae')
#3.12301
#-------------------Decision tree------------

library(rpart)    #Library for regression model
DT_model= rpart(fare_amount~.,training,method="anova")

DT_test=predict(DT_model,testing[-1])

RMSE(testing[,1],DT_test)
#RMSE_test=5.1154

regr.eval(testing[,1],DT_test,stats = 'mae')
#MAE 2.64258

#-----------------------_Random forest---------------------------

library(randomForest)
RF_model = randomForest(fare_amount~ ., training, importance = TRUE, ntree = 64)

#Predict test data using random forest model
RF_Predictions = predict(RF_model, testing[,-1])

RMSE(testing[,1],RF_Predictions)
#RMSE_test=  4.22

regr.eval(testing[,1],RF_Predictions,stats = 'mae')
#MAE 2.045551

test$fare_amount = predict(RF_model, test)

write.csv(test,file='Cab Fare predictions.csv',col.names = TRUE,row.names = FALSE)
