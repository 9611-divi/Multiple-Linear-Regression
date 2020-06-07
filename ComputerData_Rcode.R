library(readr)
data<-read.csv("Data Science Assignments/Multiple Linear regression/Computer_Data.csv")
#dataset details and EDA
colnames(data)
computerData<-data[, c(2:11)]
View(computerData)
attach(computerData)
dim(computerData)
sum(is.null(computerData))
library(plyr)
computerData$cd<-revalue(computerData$cd, c("yes"="1", "no"="2"))
computerData$multi<-revalue(computerData$multi, c("yes"="1", "no"="2"))
computerData$premium<-revalue(computerData$premium, c("yes", "no"="2"))
summary(computerData)
head(computerData)
# checking for multicollinearity
plot(computerData) 
computerData$cd<-as.numeric(computerData$cd)
computerData$multi<-as.numeric(computerData$multi)
computerData$premium<-as.numeric(computerData$premium)
cor(computerData)
# pairwise correlation between predictor variables are low
# so there is no multicollinearity

library(corpcor)
cor2pcor(cor(computerData)) 
# finding influence factors
model<-lm(price ~., data=computerData)
summary(model)
library(car)
influenceIndexPlot(model)
influencePlot(model)  
model1<-lm(price ~., data=computerData[-c(1441,1701,3784,4478),])
summary(model1)
vif(model1)
# vif<10 so there is no multicollinearity
avPlots(model1)
# fitting of multiple linear regression model
model2<-lm(price ~ .-computerData$cd-computerData$multi, data=computerData[-c(1441,1701,3784,4478)])
summary(model2)
predicted_values<-predict(model2)
head(predicted_values)
# here p values <0.05
#multiple R-squared value is 0.7756
#Adjusted R-squared value is 0.7752
par(mfrow=c(2,2))
plot(model2)
