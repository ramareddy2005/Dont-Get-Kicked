install.packages("RWeka")
library(caret)
library(rpart)
install.packages("caTools")
install.packages("dplyr")
library(caTools)
library(dplyr)
setwd("C:/Users/Ramakrishna/Desktop/Dont Get Kicked")
cars_train = read.csv(file = "train.csv",na.strings= c(""," ","NA"), header = TRUE)
dim(cars_train)
str(cars_train)
summary(cars_train)


cars_train$IsBadBuy = as.factor(cars_train$IsBadBuy)
cars_train$PurchDate = as.Date(cars_train$PurchDate, "%m/%d/%Y")
cars_train$VehYear = as.factor(cars_train$VehYear)
cars_train$VehicleAge = as.factor(cars_train$VehicleAge)
cars_train$MMRAcquisitionAuctionAveragePrice = as.integer(cars_train$MMRAcquisitionAuctionAveragePrice)
cars_train$MMRAcquisitionAuctionCleanPrice = as.integer(cars_train$MMRAcquisitionAuctionCleanPrice)
cars_train$MMRAcquisitionRetailAveragePrice = as.integer(cars_train$MMRAcquisitionRetailAveragePrice)
cars_train$MMRAcquisitonRetailCleanPrice = as.integer(cars_train$MMRAcquisitonRetailCleanPrice)
cars_train$MMRCurrentAuctionAveragePrice = as.integer(cars_train$MMRCurrentAuctionAveragePrice)
cars_train$MMRCurrentAuctionCleanPrice = as.integer(cars_train$MMRCurrentAuctionCleanPrice)
cars_train$MMRCurrentRetailAveragePrice = as.integer(cars_train$MMRCurrentRetailAveragePrice)
cars_train$MMRCurrentRetailCleanPrice = as.integer(cars_train$MMRCurrentRetailCleanPrice)
cars_train$BYRNO = as.factor(cars_train$BYRNO)
cars_train$VNZIP1 = as.factor(cars_train$VNZIP1)
cars_train$IsOnlineSale = as.factor(cars_train$IsOnlineSale)

##Seperating the kicked and non-kicked cars.
---------
Data_Kicked <- subset(cars_train, IsBadBuy=="1")
table(cars_train$IsBadBuy)
dim(Data_Kicked)
tail(Data_Kicked$IsBadBuy, 100)
head(Data_Kicked$IsBadBuy, 1000)

Data_NotKicked = subset(cars_train, IsBadBuy == "0")
dim(Data_NotKicked)
head(Data_NotKicked$IsBadBuy, 1000)
tail(Data_NotKicked$IsBadBuy)


#Taking 50% of data from
trim_data=sample_frac(Data_NotKicked,size = 0.5)
dim(trim_data)
class(trim_data)

cars_train1 = rbind(trim_data,Data_Kicked)
class(cars_train1)
dim(cars_train1)


----------------------------------------------------------------------------

set.seed(150)
rpart_grid = expand.grid(.cp = seq(0,1, 0.1))
tr_ctrl = trainControl(method = "CV", number = 10)

rpart_model = train(cars_train1[,c("VehYear","PurchDate", "VehicleAge","MMRAcquisitonRetailCleanPrice","MMRAcquisitionRetailAveragePrice","MMRAcquisitionAuctionCleanPrice","MMRAcquisitionAuctionAveragePrice","Make","IsOnlineSale")], cars_train1$IsBadBuy, method = "rpart", trControl = tr_ctrl, tuneGrid =rpart_grid  )

cars_test = read.csv("test.csv", na.strings = c("NA",""," "), header = TRUE)
dim(cars_test)
str(cars_test)
cars_test$PurchDate = as.Date(cars_test$PurchDate, "%m/%d/%Y")
cars_test$VehYear = as.factor(cars_test$VehYear)
cars_test$VehicleAge = as.factor(cars_test$VehicleAge)
cars_test$MMRAcquisitionAuctionAveragePrice = as.integer(cars_test$MMRAcquisitionAuctionAveragePrice)
cars_test$MMRAcquisitionAuctionCleanPrice = as.integer(cars_test$MMRAcquisitionAuctionCleanPrice)
cars_test$MMRAcquisitionRetailAveragePrice = as.integer(cars_test$MMRAcquisitionRetailAveragePrice)
cars_test$MMRAcquisitonRetailCleanPrice = as.integer(cars_test$MMRAcquisitonRetailCleanPrice)
cars_test$MMRCurrentAuctionAveragePrice = as.integer(cars_test$MMRCurrentAuctionAveragePrice)
cars_test$MMRCurrentAuctionCleanPrice = as.integer(cars_test$MMRCurrentAuctionCleanPrice)
cars_test$MMRCurrentRetailAveragePrice = as.integer(cars_test$MMRCurrentRetailAveragePrice)
cars_test$MMRCurrentRetailCleanPrice = as.integer(cars_test$MMRCurrentRetailCleanPrice)
cars_test$BYRNO = as.factor(cars_test$BYRNO)
cars_test$VNZIP1 = as.factor(cars_test$VNZIP1)
cars_test$IsOnlineSale = as.factor(cars_test$IsOnlineSale)

cars_test$IsBadBuy = predict(rpart_model, cars_test[,c("VehYear","PurchDate", "VehicleAge","MMRAcquisitonRetailCleanPrice","MMRAcquisitionRetailAveragePrice","MMRAcquisitionAuctionCleanPrice","MMRAcquisitionAuctionAveragePrice","Make","IsOnlineSale")])
write.csv(cars_test[,c("RefId","IsBadBuy")],"submission_rpat1.csv", row.names = F)


