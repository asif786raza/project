rm(list=ls())
setwd("C:/users/user")
getwd()

#load data
data= read.csv("day.csv")

#dimension of data
dim(data)

#LOAD Libraries
x=c("ggplt2", "corrgram", "DMwR", "Caret", "RandomForest", "unbalance", "C50", "dummies", "e1071", "information", "MASS", "rpart", "gbm", "ROSE")
lapply(x, require, character.only=TRUE)

names(data)

#library for ploting the graph
library(scales)
library(psych)
library(gplots)
library(ggplot2)

#explore the data
str(data)


#missing value analysis
missing_val= data.frame(apply(data, 2, function(x)(sum(is.na(x)))))

View(missing_val)

#calculate how much missing value in particular variables
sum(is.na(data))

#view and table of all 
View(data)
table(data$yr)
table(data$season)
table(data$mnth)
table(data$holiday)
table(data$weekday)
table(data$workingday)
table(data$weathersit)
table(data$temp)
table(data$atemp)
table(data$hum)
table(data$windspeed)
range(data$weathersit)
range(data$workingday)
range(data$temp)
range(data$atemp)
range(data$hum)
range(data$windspeed)
range(data$casual)
range(data$registered)
range(data$cnt)

data[1:5, 1:8]
data[1:5, 9:14]
data[1:5, 15:16]

#histogram plot
ggplot(data, aes(x=data$temp))+geom_histogram(fill="DarkSlateBlue", colour="black")+xlab("Temperature")+ggtitle("Temperature")
ggplot(data, aes(x=data$atemp))+geom_histogram(fill="DarkSlateBlue", colour="black")+ggtitle("Normalized Temperature in Celsius")
ggplot(data, aes(x=data$hum))+geom_histogram(fill="DarkSlateBlue", colour="black")+xlab("Humidity")+ggtitle("Humidity")
ggplot(data, aes(x=data$windspeed))+geom_histogram(fill="DarkSlateBlue", colour="black")+xlab("Wind Speed")+ggtitle("Wind speed")
ggplot(data, aes(x=data$casual))+geom_histogram(fill="DarkSlateBlue", colour="black")+ggtitle("casual")
ggplot(data, aes(x=data$registered))+geom_histogram(fill="DarkSlateBlue", colour="black")+ggtitle("registered")
ggplot(data, aes(x=data$cnt))+geom_histogram(fill="DarkSlateBlue", colour="black")+ggtitle("Total Count")


#boxplot distribution and outlier analysis
numeric_index= sapply(data, is.numeric)
numeric_index
numeric_data= data[, numeric_index]
cnames= colnames(numeric_data)
cnames

#detect and delete the outliners from all numerical variable by iterating the loop
for (i in cnames){
  print(i)
  val=data[,i][data[,i] %in% boxplot.stats(data[,i])$out]
  print(length(val))
  data=data[which(!data[,i] %in% val),]
}

library(ggplot2)
for (i in 1:length(cnames)) {
  assign(paste0("gn", i), ggplot(aes_string((cnames[i]),x= "cnt"), data= subset(data))+
           stat_boxplot(geom= "errorbar", width=0.5)+
           geom_boxplot(outlier.colour="red", fill="grey", outlier.shape=18,
                        outlier.size=1, notch=FALSE)+
           theme(legend.position="bottom")+
           labs(y=cnames[i], x="Bike Rented count")+
           ggtitle(paste("box plot for", cnames[i])))
}

gridExtra::grid.arrange(gn9,gn10,gn11, ncol=3)
gridExtra::grid.arrange(gn12,gn13, gn14, ncol=3)


#detect and delete the outliners from all numerical variable by iterating the loop
for (i in cnames){
  print(i)
  val=data[,i][data[,i] %in% boxplot.stats(data[,i])$out]
  print(length(val))
  data[,i][data[,i] %in% val] =NA
}



sum(is.na(data))
View(data)
data=knnImputation(data, k=5)

#boxplot analysis after detect and impute outliers
for (i in 1:length(cnames)) {
  assign(paste0("gn", i), ggplot(aes_string((cnames[i]),x= "cnt"), data= subset(data))+
           stat_boxplot(geom= "errorbar", width=0.5)+
           geom_boxplot(outlier.colour="red", fill="grey", outlier.shape=18,
                        outlier.size=1, notch=FALSE)+
           theme(legend.position="bottom")+
           labs(y=cnames[i], x="Bike rented Count")+
           ggtitle(paste("box plot for", cnames[i])))
}
gridExtra::grid.arrange(gn9,gn10,gn11, ncol=3)
gridExtra::grid.arrange(gn12,gn13, gn14, ncol=3)


#chec shape of data
dim(data)

data1= subset(data, select= c(temp, atemp, hum, windspeed, casual, registered))
data2= subset(data, select=-c(temp, atemp,hum, windspeed, casual, registered))

#correlation plot
corrgram(data1, order=F,
         upper.panel=panel.pie, text.panel=panel.txt, main="correlation plot")

#chi sqaure of independence and selecting only categorical variable
factor_index=sapply(data, is.factor)
factor_data= data[, factor_index]
factor_index
View(factor_data)

data
#chi-square test data
#chi-square test
for (i in 1:9){
  print(names(data2)[i])
  print(chisq.test(table(data2$cnt, data2[,i])))
}

#dimension Reduction Method
data= subset(data, select= -c(instant, dteday, atemp))


#normalizaion method
cnames1=c( "casual", "registered") 
for (i in cnames1){
  print(i)
  data[,i]= (data[,i]-min(data[,i]))/(max(data[,i]-min(data[,i])))
} 
View(data)
dim(data)
#Build a model
#decision tree alogorithm(regression problm)
library(rpart)
library(MASS)
library(randomForest)

#sampling technique
train_index= sample(1:nrow(data), 0.8*nrow(data))
train= data[train_index,]
test=data[-train_index,]

#rpart for regression
fit= rpart(cnt~., data=train, method="anova")
fit

#predict for new test case
prediction_DT= predict(fit, test[,-13])
prediction_DT


#calculate MAPE
MAPE= function(y, yhat){
  mean(abs((y-yhat)/y))*100
}
MAPE(test[,13], prediction_DT )

#alternative method
regr.eval(test[,13], prediction_DT, stats = c("mae", "rmse", 'mape', 'mse'))


#linear regression method
#check multicollinearity
library(usdm)
vif(data[,-13])
vifcor(data[,-13], th= 0.9)

#Run regression model
lm_model= lm(cnt~., data = train)
summary(lm_model)

#Predict the target variable with absence of actual target variable 
prediction_LR=predict(lm_model, test[,1:12])
MAPE(test[,13], prediction_LR)
prediction_LR

#Performance of model
regr.eval(test[,13], prediction_LR, stats = c("mae", "rmse", 'mape', 'mse'))

#random forest algorithm
RF_model= randomForest(cnt ~., train, importance=TRUE, ntree=500)

#extract rules from random forest
library(inTrees)
treelist= RF2List(RF_model)
exec=extractRules(treelist, train[,-13])
exec[1:2,]

#make rules more readable
readablerules= presentRules(exec, colnames(train))
readablerules[1:2,]
rulemetrix=getRuleMetric(exec, train[,-13], train$cnt)
rulemetrix[1:2,]
RF_prediction= predict(RF_model, test[, -13])
RF_prediction

#Performance of model
regr.eval(test[,13], RF_prediction, stats = c("mae", "rmse", 'mape', 'mse'))

#KNN(k nearest neighbour) classification
library(class)
library(e1071)

knn_predictions= knn(train[,1:13], test[,1:13], train$cnt, k=7)

knn_predictions

#Performance of model
regr.eval(test[,13], knn_predictions, stats = c("mae", "rmse", 'mape', 'mse'))





