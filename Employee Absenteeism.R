rm(list=ls())
setwd("C:/users/user")
getwd()

#load data
data= read.csv("RF_Absenteeism_at_work_Project.csv")

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
sum(is.na(data$ID))
sum(is.na(data$Reason.for.absence))

#convert the data into a proper shape
missing_val$columns=row.names(missing_val)
row.names(missing_val)= NULL
 
#LET us rename the 1st variable
names(missing_val)[1]="missing-percentage"

#calcualte the percentage of missing value
missing_val$`missing-percentage`=(missing_val$`missing-percentage`/nrow(data))*100


#convert into descending order
missing_val= missing_val[order(-missing_val$`missing-percentage`),]
View(missing_val)

#reaaranging the columns
missing_val=missing_val[, c(2,1)]

#Plot the missing percentage in the histogram
ggplot(data = missing_val[1:21,], aes(x= reorder(columns, -`missing-percentage`), y= `missing-percentage`))+
  geom_bar(stat="identity", fill= "grey")+xlab("parameter")+ 
  ggtitle("missing_data percentage (train)") + theme_bw()


#Method of imputing the missing value
# mean method
data[71,1]

data$ID[is.na(data$ID)]= mean(data$ID, na.rm = T)
data[71,1]=NA
data[71,1]

data= function(x){sum(is.na(x))/length(x)*100}
apply(data, 2, data)
#median method
data$ID[is.na(data$ID)]=median(data$ID, na.rm = T)
is.na(data)
sum(is.na(data))
data$Q1_Answer[data$Q1_Answer==834] <- NA

#KNN Imputation method
data=knnImputation(data, k=5)
View(data)


#histogram plot
ggplot(data, aes(x=data$Month.of.absence))+geom_histogram(fill="DarkSlateBlue", colour="black")+ggtitle("month of absence")
ggplot(data, aes(x=data$Transportation.expense))+geom_histogram(fill="DarkSlateBlue", colour="black")+ggtitle("transportation expences")
ggplot(data, aes(x=data$Distance.from.Residence.to.Work))+geom_histogram(fill="DarkSlateBlue", colour="black")+ggtitle("distance from residence to work")
ggplot(data, aes(x=data$Service.time))+geom_histogram(fill="DarkSlateBlue", colour="black")+ggtitle("service time")
ggplot(data, aes(x=data$Age))+geom_histogram(fill="DarkSlateBlue", colour="black")+ggtitle("age")
ggplot(data, aes(x=data$Hit.target))+geom_histogram(fill="DarkSlateBlue", colour="black")+ggtitle("hit target")
ggplot(data, aes(x=data$Son))+geom_histogram(fill="DarkSlateBlue", colour="black")+ggtitle("son")
ggplot(data, aes(x=data$Pet))+geom_histogram(fill="DarkSlateBlue", colour="black")+ggtitle("pet")
ggplot(data, aes(x=data$Weight))+geom_histogram(fill="DarkSlateBlue", colour="black")+ggtitle("weight")
ggplot(data, aes(x=data$Height))+geom_histogram(fill="DarkSlateBlue", colour="black")+ggtitle("height")
ggplot(data, aes(x=data$Absenteeism.time.in.hours))+geom_histogram(fill="DarkSlateBlue", colour="black")+ggtitle("absenteeism time in hours")
ggplot(data, aes(x=data$Body.mass.index))+geom_histogram(fill="DarkSlateBlue", colour="black")+ggtitle("body mass index")


#boxplot distribution and outlier analysis
numeric_index= sapply(data, is.numeric)
numeric_index
numeric_data= data[, numeric_index]
cnames= colnames(numeric_data)
cnames

library(ggplot2)
for (i in 1:length(cnames)) {
  assign(paste0("gn", i), ggplot(aes_string((cnames[i]),x= "Absenteeism.time.in.hours"), data= subset(data))+
           stat_boxplot(geom= "errorbar", width=0.5)+
           geom_boxplot(outlier.colour="red", fill="grey", outlier.shape=18,
                        outlier.size=1, notch=FALSE)+
           theme(legend.position="bottom")+
           labs(y=cnames[i], x="Absenteeism employee")+
           ggtitle(paste("box plot for", cnames[i])))
}
gridExtra::grid.arrange(gn1, gn3, gn6, ncol=3)
gridExtra::grid.arrange(gn7,gn8, gn9, ncol=3)
gridExtra::grid.arrange(gn10,gn12, ncol=2)
gridExtra::grid.arrange(gn13,gn16, gn17, ncol=3)
gridExtra::grid.arrange(gn18,gn19, ncol=2)

#delete those observation which contain outliers
val=data$area.code[data$area.code %in% boxplot.stats(data$area.code)$out]
data=data[which(data$area.code %in% val),]
data=data[which(!data$area.code %in% val),]

data=df

#detect and delete the outliners from all numerical variable by iterating the loop
for (i in cnames){
  print(i)
  val=data[,i][data[,i] %in% boxplot.stats(data[,i])$out]
  print(length(val))
  data[,i][data[,i] %in% val] =NA
}

sum(is.na(data))
View(data)
data=knnImputation(data, k=3)

#boxplot analysis after detect and impute outliers
for (i in 1:length(cnames)) {
  assign(paste0("gn", i), ggplot(aes_string((cnames[i]),x= "Absenteeism.time.in.hours"), data= subset(data))+
           stat_boxplot(geom= "errorbar", width=0.5)+
           geom_boxplot(outlier.colour="red", fill="grey", outlier.shape=18,
                        outlier.size=1, notch=FALSE)+
           theme(legend.position="bottom")+
           labs(y=cnames[i], x="Absenteeism employee")+
           ggtitle(paste("box plot for", cnames[i])))
}
gridExtra::grid.arrange(gn1, gn3, gn6, ncol=3)
gridExtra::grid.arrange(gn7,gn8, gn9, ncol=3)
gridExtra::grid.arrange(gn10,gn12, ncol=2)
gridExtra::grid.arrange(gn13,gn16, gn17, ncol=3)
gridExtra::grid.arrange(gn18,gn19, ncol=2)
  
#chec shape of data
dim(data)

#Split numerical and categorical variable
catnames= subset(data, select= -c(Reason.for.absence, Seasons, Day.of.the.week, Disciplinary.failure,
                                  Seasons,Education, Social.drinker, Social.smoker))
#drop variables
data= subset(data, select= -c(Education, Weight, Work.load.Average.day))

#correlation plot
corrgram(data[,numeric_index], order=F,
         upper.panel=panel.pie, text.panel=panel.txt, main="correlation plot")

numeric_index= sapply(catnames, is.numeric)
numeric_index

#chi sqaure of independence and selecting only categorical variable
factor_index=sapply(data, is.factor)
factor_data= data[, factor_index]
View(factor_data)
factor_index=subset[factor_index]


#normalizaion method
cnames1=c("ID", "Month.of.absence", "Transportation.expense", "Distance.from.Residence.to.Work",
          "Service.time", "Age", "Hit.target", "Son",
          "Pet", "Height", "Body.mass.index") 
for (i in cnames1){
  print(i)
  data[,i]= (data[,i]-min(data[,i]))/(max(data[,i]-min(data[,i])))
} 


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
fit= rpart(Absenteeism.time.in.hours~., data=train, method="anova")
fit

#predict for new test case
prediction_DT= predict(fit, test[,-18])
prediction_DT


#calculate MAPE
MAPE= function(y, yhat){
  mean(abs((y-yhat)/y))*100
}
MAPE(test[,18], prediction_DT )

#alternative method
regr.eval(test[,18], prediction_DT, stats = c("mae", "rmse", 'mape', 'mse'))


#linear regression method
#check multicollinearity
library(usdm)
vif(data[,-18])
vifcor(data[,-18], th= 0.9)

#Run regression model
lm_model= lm(Absenteeism.time.in.hours~., data = train)
summary(lm_model)

#Predict the target variable with absence of actual target variable 
prediction_LR=predict(lm_model, test[,1:17])
MAPE(test[,18], prediction_LR)
prediction_LR

#Performance of model
regr.eval(test[,18], prediction_LR, stats = c("mae", "rmse", 'mape', 'mse'))

#random forest algorithm
RF_model= randomForest(Absenteeism.time.in.hours ~., train, importance=TRUE, ntree=500)

#extract rules from random forest
library(inTrees)
treelist= RF2List(RF_model)
exec=extractRules(treelist, train[,-18])
exec[1:2,]

#make rules more readable
readablerules= presentRules(exec, colnames(train))
readablerules[1:2,]
rulemetrix=getRuleMetric(exec, train[,-18], train$Absenteeism.time.in.hours)
rulemetrix[1:2,]
RF_prediction= predict(RF_model, test[, -18])
RF_prediction

#Performance of model
regr.eval(test[,18], RF_prediction, stats = c("mae", "rmse", 'mape', 'mse'))



