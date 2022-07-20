#Set working directory
setwd("C:/Users/User/Documents/Seminar/Data")

#Install packages
install.packages('caret', dependencies = TRUE)
install.packages('Metrics')
install.packages('gradDescent')
install.packages('caret')
install.packages("recipes", dependencies = TRUE)
install.packages('TSstudio')
install.packages('splitTools')
install.packages('ranger')
install.packages('gradDescentRData')
install.packages('MLmetrics')
install.packages('crossval')
install.packages('fpp')
install.packages('forecast')
install.packages('dprep')
install.packages("installr")

library(installr)
library(dprep)
library(readr)
library(e1071)
library(TSstudio)
library(caret)
library(caTools)
library(Metrics)
library("readxl")
library(gradDescent)
library(gradDescentRData)
library(quantmod)
library(dplyr)
library(splitTools)
library(ranger)
library(MLmetrics)

#Determine input data
data <- read_excel("mydata.xlsx")

#Input as dataframe
df <- data.frame(
  Date = data$Date2,
  Price = data$Price,
  
  stringsAsFactors = FALSE
)

#Plot input data 
plot(df, pch = 18, col = "black", xlab = "Day", ylab = "Price")
summary(df)

#Normalize input data
normalize <- function(x, min, max) (x-min)/(max-min)
denormalize <- function(x, min, max) x*(max-min)+min

price.norm <- df$Price
price.range <- range(df$Price)

#Apply min-max normalization
df$Price <- normalize(price.norm, price.range[1], price.range[2])

#Input data train test splitting
train_indices <- seq_len(length.out = floor(x = 0.7 * nrow(x = df)))
train <- df[train_indices,]
test <- df[-train_indices,]

#First SVR model using training data (kernel types available = linear, polynomial, radial)
modelsvr = svm(Price~Date,train, kernel = "radial", cost = 1, gamma = 1, epsilon = 0.1)
predYsvr = predict(modelsvr, train) #to build the model
y_pred = predict(modelsvr, data = test$Date)

#Overlay SVR predictions on scatter plot
x = 1:length(train$Price)
price.denorm <- denormalize(train$Price, price.range[1], price.range[2])
plot(x, price.denorm, xlab = "Day", ylab = "Price",pch=18, col="black")
pred.denorm <- denormalize(predYsvr, price.range[1], price.range[2])
lines(x, pred.denorm, lwd="2", col="blue")
legend("topleft", legend=c("RBF Kernel"),col=c("blue"), lty=1, cex=1.2)

#Evaluate the model by RMSE
RMSEsvr=rmse(predYsvr,train$Price)
RMSEsvr


#Model optimization

#Set parameter interval 
tuneGrid <- expand.grid(
  C = c(2^(-5), 2^(9)),
  sigma = 2^seq(-15,3,2)
)

#Apply TS-CV 
cts <- createTimeSlices(train$Price, initialWindow = 351, horizon = 76, fixedWindow = FALSE, skip = 76)
training <- cts[[1]]
testing <- cts[[2]]

modeltime <- train(Price~.,
                   data = train[training[[1]],],
                   method = "svmRadial",
                   preProc = c("center", "scale"),
                   tuneGrid = tuneGrid)
pred <- predict(modeltime,train[testing[[1]],])
print(modeltime$bestTune) #print best parameter values

#Optimal epsilon value using best model
eps = seq(0,1,0.05)
for(i in 1:length(eps)){
  modelsvr = svm(Price~Date,train,type = "eps-regression", kernel = "radial", cost = 512, gamma = 8, epsilon = eps[i])
  predYsvr = predict(modelsvr, train)
  RMSEBst=rmse(predYsvr,train$Price)
  print(RMSEBst)
} 



#Build optimal SVR model
modelsvropt = svm(Price~Date,test,type = "eps-regression", kernel = "radial", cost = 512, gamma = 8, epsilon = 0.15) #Optimal values = cost: 512, eps: 0.15, gamma: 8
predYsvropt = predict(modelsvropt, test)


#Plot optimal SVR model on testing data
lentest = 1:length(test$Price)
testprice.denorm <- denormalize(test$Price, price.range[1], price.range[2])
plot(lentest, testprice.denorm, xlab = "Day", ylab = "Price",pch=18, col="black")
testpred.denorm <- denormalize(predYsvropt, price.range[1], price.range[2])
lines(lentest, testpred.denorm, lwd="2", col="blue")
legend("topright", legend=c("RBF Kernel"),col=c("blue"), lty=1, cex=1.2)

#Predict for 20 days ahead
datepred <- data.frame(Date = c(1536:1555))
pricepred = predict(modelsvropt, newdata = datepred)
pricepred = denormalize(pricepred, price.range[1], price.range[2])

#Plot optimal model and prediction value
xdate = 462:481
lentest = 1:length(test$Price)
testprice.denorm <- denormalize(test$Price, price.range[1], price.range[2])
plot(lentest2, testprice.denorm, xlab = "Day", ylab = "Price", xlim = c(0,500), pch=18, col="black")
testpred.denorm <- denormalize(predYsvropt, price.range[1], price.range[2])
lines(lentest2, testpred.denorm, lwd="2", col="blue")
points(xdate, pricepred, col = "red", pch=18)
legend("topright", legend=c("RBF Kernel"),col=c("blue"), lty=1, cex=1.2)

#Evaluate optimal model with RMSE and MAPE
#RMSE
RMSEBst=rmse(predYsvropt,test$Price)
RMSEBst
#MAPE
MAPEBst = MAPE(predYsvropt, test$Price)
MAPEBst
