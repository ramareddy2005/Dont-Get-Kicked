library(ggplot2)
library(caret)
library(doParallel)

c1 = makeCluster(detectCores())
registerDoParallel(c1)


setwd("C:/Users/Ramakrishna/Desktop/Dont Get Kicked")
cars_train = read.csv(file = "train.csv",na.strings= c(""," ","NA"), header = TRUE)
str(cars_train)
summary(cars_train)
dim(cars_train)
head(cars_train$VehicleAge,1000)
tail(cars_train$VehicleAge,10000)
summary(cars_train$VehicleAge)
str(cars_train$VehicleAge)
summary(cars_train$Trim)
boxplot(cars_train$VehicleAge)
boxplot(cars_train$VehicleAge, col = "red")

cars_train$IsBadBuy = as.factor(cars_train$IsBadBuy)
cars_train$PurchDate = as.Date(cars_train$PurchDate, "%m/%d/%y")
cars_train$VehYear = as.factor(cars_train$VehYear)
cars_train$VehicleAge1 = as.factor(cars_train$VehicleAge)

str(cars_train$VehicleAge1)
xtabs(~Trim+IsBadBuy,data = cars_train)
xtabs(~PurchDate+IsBadBuy,data = cars_train)
cor()

xtabs(~VehicleAge1+IsBadBuy1, data = cars_train)
xtabs(~VehicleAge1+IsBadBuy1, data = cars_train)
xtabs(~Auction+IsBadBuy1, data = cars_train)
xtabs(~VehYear+IsBadBuy1, data = cars_train)
str(cars_train$VehYear)
xtabs(~PurchDate+VehYear+IsBadBuy1, data = cars_train)
str(cars_train$PurchDate)

#Badbuy = cars_train$IsBadBuy1
# Badbuy2=table(Badbuy)
# percentage = round(100*Badbuy2/sum(Badbuy2),1)
# png(file = "PieChart3.jpg")
# pie(Badbuy2, radius= .8,main = "IsBadBuy", labels = percentage, col = rainbow(length(Badbuy2)))
# legend("topright",c("Can Buy","Can't Buy"),cex= 1,fill= rainbow(length(Badbuy2)))
# dev.off()
# stopCluster(c1)

#Vehicle Age Feature


VehicleAge2=table(cars_train$VehicleAge1)
percentage = round(100*VehicleAge2/sum(VehicleAge2),1)
# png(file = "PieChart3.jpg")
pie(VehicleAge2, radius= .8,main = "VehicleAge", labels = percentage, col = rainbow(length(VehicleAge2)))
plot(cars_train$VehicleAge,cars_train$IsBadBuy,xlim =c(0,5), ylim = c(0,5))
scatter.smooth(cars_train$VehicleAge1,cars_train$IsBadBuy)
# legend("topright",c("0","1"),cex= 1,fill= rainbow(length(VehicleAge2)))
# dev.off()

set.seed(200)
tr_ctrl = trainControl(method = "CV", n = 10)
rf_model = train(IsBadBuy1~ VehicleAge, cars_train, method = "rf", trControl = tr_ctrl)
