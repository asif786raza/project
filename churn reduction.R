rm(list=ls())
setwd("C:/Users/user")
getwd()

#LOAD Libraries
x=c("ggplt2", "corrgram", "DMwR", "Caret", "RandomForest", "unbalance", "C50", "dummies", "e1071", "information", "MASS", "rpart", "gbm", "ROSE")
lapply(x, require, character.only=TRUE)
#LOAD Data
Train_data=read.csv("Train_data.csv", header=T)
Test_data=read.csv("Test_data.csv", header = T )
names(Train_data)

#library for ploting the graph
library(scales)
library(psych)
library(gplots)
library(ggplot2)

#histogram of all predictors variable
ggplot(Train_data, aes(x=Train_data$account.length))+geom_histogram(fill="DarkSlateBlue", colour="black")+ggtitle("account.length")
ggplot(Train_data, aes(x=Train_data$area.code))+geom_histogram(fill="DarkSlateBlue", colour="black")+ggtitle("area code")
ggplot(Train_data, aes(x=Train_data$number.vmail.messages))+geom_histogram(fill="DarkSlateBlue", colour="black")+ggtitle("account.length")
ggplot(Train_data, aes(x=Train_data$total.day.minutes))+geom_histogram(fill="DarkSlateBlue", colour="black")+ggtitle("total day minutes")
ggplot(Train_data, aes(x=Train_data$total.day.calls))+geom_histogram(fill="DarkSlateBlue", colour="black")+ggtitle("total day calls")
ggplot(Train_data, aes(x=Train_data$total.day.charge))+geom_histogram(fill="DarkSlateBlue", colour="black")+ggtitle("total day charge")
ggplot(Train_data, aes(x=Train_data$total.eve.minutes))+geom_histogram(fill="DarkSlateBlue", colour="black")+ggtitle("total evening minutes")
ggplot(Train_data, aes(x=Train_data$total.eve.calls))+geom_histogram(fill="DarkSlateBlue", colour="black")+ggtitle("total evening calls")
ggplot(Train_data, aes(x=Train_data$total.eve.charge))+geom_histogram(fill="DarkSlateBlue", colour="black")+ggtitle("total evening charge")
ggplot(Train_data, aes(x=Train_data$total.night.minutes))+geom_histogram(fill="DarkSlateBlue", colour="black")+ggtitle("total night minutes")
ggplot(Train_data, aes(x=Train_data$total.night.calls))+geom_histogram(fill="DarkSlateBlue", colour="black")+ggtitle("total night calls")
ggplot(Train_data, aes(x=Train_data$total.night.charge))+geom_histogram(fill="DarkSlateBlue", colour="black")+ggtitle("total night charge")
ggplot(Train_data, aes(x=Train_data$total.intl.minutes))+geom_histogram(fill="DarkSlateBlue", colour="black")+ggtitle("total international minutes")
ggplot(Train_data, aes(x=Train_data$total.intl.calls))+geom_histogram(fill="DarkSlateBlue", colour="black")+ggtitle("total international calls")
ggplot(Train_data, aes(x=Train_data$total.intl.charge))+geom_histogram(fill="DarkSlateBlue", colour="black")+ggtitle("total international charge")
ggplot(Train_data, aes(x=Train_data$number.customer.service.calls))+geom_histogram(fill="DarkSlateBlue", colour="black")+ggtitle("service customer calls")


#check missing value analysis
#explore the data
str(Train_data)

#create dataframe with missing percentage
missing_val= data.frame(apply(Train_data, 2, function(x)(sum(is.na(x)))))
missing_val

#check unique value
unique(Train_data$state)
length(unique(Train_data$state))
table(Train_data$state)

#check outlier analysis of numerical variable
ggplot(Train_data, aes(x=Train_data$account.length, fill= Train_data$account.length))+geom_boxplot(outlier.colour = "red", outlier.size = 3)+ggtitle("Outlier analysis- Account Length")

#check outliers anlysis(boxplot of numerical variable with dependent variable)
ggplot(Train_data, aes(x=Train_data$Churn, y=Train_data$account.length, fill= Train_data$Churn))+geom_boxplot(outlier.colour = "red", outlier.size = 3)+ggtitle("Outlier analysis- Account Length")
ggplot(Train_data, aes(x=Train_data$Churn, y=Train_data$voice.mail.plan, fill= Train_data$Churn))+geom_boxplot(outlier.colour = "red", outlier.size = 3)+ggtitle("Outlier analysis- no of voice mail message")
ggplot(Train_data, aes(x=Train_data$Churn, y=Train_data$total.day.calls, fill= Train_data$Churn))+geom_boxplot(outlier.colour = "red", outlier.size = 3)+ggtitle("Outlier analysis- total day calls")
ggplot(Train_data, aes(x=Train_data$Churn, y=Train_data$total.day.minutes, fill= Train_data$Churn))+geom_boxplot(outlier.colour = "red", outlier.size = 3)+ggtitle("Outlier analysis- total day minutes")
ggplot(Train_data, aes(x=Train_data$Churn, y=Train_data$total.day.charge, fill= Train_data$Churn))+geom_boxplot(outlier.colour = "red", outlier.size = 3)+ggtitle("Outlier analysis- total day charge")
ggplot(Train_data, aes(x=Train_data$Churn, y=Train_data$total.eve.calls, fill= Train_data$Churn))+geom_boxplot(outlier.colour = "red", outlier.size = 3)+ggtitle("Outlier analysis- total evening calls")
ggplot(Train_data, aes(x=Train_data$Churn, y=Train_data$total.eve.minutes, fill= Train_data$Churn))+geom_boxplot(outlier.colour = "red", outlier.size = 3)+ggtitle("Outlier analysis- total evening minutes")
ggplot(Train_data, aes(x=Train_data$Churn, y=Train_data$total.eve.charge, fill= Train_data$Churn))+geom_boxplot(outlier.colour = "red", outlier.size = 3)+ggtitle("Outlier analysis- total evening charge")
ggplot(Train_data, aes(x=Train_data$Churn, y=Train_data$total.night.calls, fill= Train_data$Churn))+geom_boxplot(outlier.colour = "red", outlier.size = 3)+ggtitle("Outlier analysis- total night calls")
ggplot(Train_data, aes(x=Train_data$Churn, y=Train_data$total.night.minutes, fill= Train_data$Churn))+geom_boxplot(outlier.colour = "red", outlier.size = 3)+ggtitle("Outlier analysis- total night minutes")
ggplot(Train_data, aes(x=Train_data$Churn, y=Train_data$total.night.charge, fill= Train_data$Churn))+geom_boxplot(outlier.colour = "red", outlier.size = 3)+ggtitle("Outlier analysis- total night charge")
ggplot(Train_data, aes(x=Train_data$Churn, y=Train_data$total.intl.calls, fill= Train_data$Churn))+geom_boxplot(outlier.colour = "red", outlier.size = 3)+ggtitle("Outlier analysis- total international calls")
ggplot(Train_data, aes(x=Train_data$Churn, y=Train_data$total.intl.minutes, fill= Train_data$Churn))+geom_boxplot(outlier.colour = "red", outlier.size = 3)+ggtitle("Outlier analysis- total international minutes")
ggplot(Train_data, aes(x=Train_data$Churn, y=Train_data$total.intl.charge, fill= Train_data$Churn))+geom_boxplot(outlier.colour = "red", outlier.size = 3)+ggtitle("Outlier analysis- total international charge")
ggplot(Train_data, aes(x=Train_data$Churn, y=Train_data$number.customer.service.calls, fill= Train_data$Churn))+geom_boxplot(outlier.colour = "red", outlier.size = 3)+ggtitle("Outlier analysis- service customer calls")


View(Train_data)
unique(Train_data$international.plan)
unique(Train_data$area.code)
unique(Train_data$number.vmail.messages)
unique(Train_data$voice.mail.plan)
unique(Train_data$Churn)
unique(Train_data$state)

#Data Manipulation convert string categorical into factor numeric
for(i in 1:ncol(Train_data)){
  if (class(Train_data[,i])== 'factor'){
    Train_data[,i]=factor(Train_data[,i], labels = (1:length(levels(factor(Train_data[,i])))))
  }
}

View(Train_data)
numeric_index= sapply(Train_data, is.numeric)
numeric_index
numeric_data= Train_data[, numeric_index]
numeric_data
cnames= colnames(numeric_data)
cnames
View(numeric_data)

# another method of plotting box plot
install.packages("ggplot2")
library(ggplot2)
for (i in 1:length(cnames)) {
  assign(paste0("gn", i), ggplot(aes_string((cnames[i]),x= "Churn"), data= subset(Train_data))+
           stat_boxplot(geom= "errorbar", width=0.5)+
           geom_boxplot(outlier.colour="red", fill="grey", outlier.shape=18,
                        outlier.size=1, notch=FALSE)+
           theme(legend.position="bottom")+
           labs(y=cnames[i], x="Churn")+
           ggtitle(paste("box plot of Churn for", cnames[i])))
}
gridExtra::grid.arrange(gn1, gn2, gn3, ncol=3)
gridExtra::grid.arrange(gn4,gn5, gn6, ncol=3)
gridExtra::grid.arrange(gn7,gn8, gn9, ncol=3)
gridExtra::grid.arrange(gn10,gn11, gn12, ncol=3)
gridExtra::grid.arrange(gn13,gn14, gn15, ncol=3)
gridExtra::grid.arrange(gn16, ncol=1)

df=Train_data

val=Train_data$area.code[Train_data$area.code %in% boxplot.stats(Train_data$area.code)$out]
Train_data=Train_data[which(Train_data$area.code %in% val),]
Train_data=Train_data[which(!Train_data$area.code %in% val),]

marketing_train=df

#detect and delete the outliners from all numerical variable by iterating the loop
for (i in cnames){
  print(i)
  val=Train_data[,i][Train_data[,i] %in% boxplot.stats(Train_data[,i])$out]
  print(length(val))
  Train_data=Train_data[which(!Train_data[,i] %in% val),]
}

#checking boxplot for outlier after doing removing outlier
ggplot(Train_data, aes(x=Train_data$Churn, y=Train_data$account.length, fill= Train_data$Churn))+geom_boxplot(outlier.colour = "red", outlier.size = 3)+ggtitle("Without outlier- Account Length")
ggplot(Train_data, aes(x=Train_data$Churn, y=Train_data$area.code, fill= Train_data$Churn))+geom_boxplot(outlier.colour = "red", outlier.size = 3)+ggtitle("Outlier analysis- arear code")
ggplot(Train_data, aes(x=Train_data$Churn, y=Train_data$voice.mail.plan, fill= Train_data$Churn))+geom_boxplot(outlier.colour = "red", outlier.size = 3)+ggtitle("Without Outlier- no of voice mail message")
ggplot(Train_data, aes(x=Train_data$Churn, y=Train_data$total.day.calls, fill= Train_data$Churn))+geom_boxplot(outlier.colour = "red", outlier.size = 3)+ggtitle("Without Outlier- total day calls")
ggplot(Train_data, aes(x=Train_data$Churn, y=Train_data$total.day.minutes, fill= Train_data$Churn))+geom_boxplot(outlier.colour = "red", outlier.size = 3)+ggtitle("Without Outlier- total day minutes")
ggplot(Train_data, aes(x=Train_data$Churn, y=Train_data$total.day.charge, fill= Train_data$Churn))+geom_boxplot(outlier.colour = "red", outlier.size = 3)+ggtitle("Without Outlier- total day charge")
ggplot(Train_data, aes(x=Train_data$Churn, y=Train_data$total.eve.calls, fill= Train_data$Churn))+geom_boxplot(outlier.colour = "red", outlier.size = 0.1)+ggtitle("Without Outlier- total evening calls")
ggplot(Train_data, aes(x=Train_data$Churn, y=Train_data$total.eve.minutes, fill= Train_data$Churn))+geom_boxplot(outlier.colour = "black", outlier.size = 3)+ggtitle("Without Outlier- total evening minutes")
ggplot(Train_data, aes(x=Train_data$Churn, y=Train_data$total.eve.charge, fill= Train_data$Churn))+geom_boxplot(outlier.colour = "red", outlier.size = 3)+ggtitle("Without Outlier- total evening charge")
ggplot(Train_data, aes(x=Train_data$Churn, y=Train_data$total.night.calls, fill= Train_data$Churn))+geom_boxplot(outlier.colour = "red", outlier.size = 3)+ggtitle("Without Outlier- total night calls")
ggplot(Train_data, aes(x=Train_data$Churn, y=Train_data$total.night.minutes, fill= Train_data$Churn))+geom_boxplot(outlier.colour = "red", outlier.size = 3)+ggtitle("Without Outlier- total night minutes")
ggplot(Train_data, aes(x=Train_data$Churn, y=Train_data$total.night.charge, fill= Train_data$Churn))+geom_boxplot(outlier.colour = "red", outlier.size = 3)+ggtitle("Without Outlier- total night charge")
ggplot(Train_data, aes(x=Train_data$Churn, y=Train_data$total.intl.calls, fill= Train_data$Churn))+geom_boxplot(outlier.colour = "red", outlier.size = 3)+ggtitle("Without Outlier- total international calls")
ggplot(Train_data, aes(x=Train_data$Churn, y=Train_data$total.intl.minutes, fill= Train_data$Churn))+geom_boxplot(outlier.colour = "red", outlier.size = 3)+ggtitle("Without Outlier- total international minutes")
ggplot(Train_data, aes(x=Train_data$Churn, y=Train_data$total.intl.charge, fill= Train_data$Churn))+geom_boxplot(outlier.colour = "red", outlier.size = 3)+ggtitle("Without Outlier- total international charge")
ggplot(Train_data, aes(x=Train_data$Churn, y=Train_data$number.customer.service.calls, fill= Train_data$Churn))+geom_boxplot(outlier.colour = "red", outlier.size = 3)+ggtitle("Without Outlier- service customer calls")

marketing_train=df



#replace the outlier with NA
for (i in cnames){
  val= Train_data[,i][Train_data[,i] %in% boxplot.stats(Train_data[,i])$out]
  #print(length(val))
  Train_data[,i][Train_data[,i] %in% val] =NA
}

sum(is.na(Train_data))
View(Train_data)
library
Train_data=knnImputation(Train_data, k=3)

#correlation plot
corrgram(Train_data[,numeric_index], order=F,
         upper.panel=panel.pie, text.panel=panel.txt, main="correlation plot")

#chi sqaure of independence and selecting only categorical variable
factor_index=sapply(Test_data, is.factor)
factor_data= Test_data[, factor_index]
factor_index
View(factor_data)
factor_index=subset(factor_index, select=-phone.number)

#chi-square test data
for (i in 1:4){
  print(names(factor_data)[i])
  print(chisq.test(table(factor_data$Churn, factor_data[,i])))
}


#chi square for train data
factor_index=sapply(Train_data, is.factor)
factor_data= Train_data[, factor_index]
factor_index
View(factor_data)
factor_index=subset(factor_index, select=-phone.number)

#chi-square test
for (i in 1:1){
  print(names(factor_data)[i])
  print(chisq.test(table(factor_data$Churn, factor_data[,i])))
}

#dimension reduction operation 
Train_data_new= subset(Train_data, 
                       select = -c(total.day.minutes, total.eve.minutes, total.night.minutes, total.intl.minutes, phone.number))
View(Train_data_new)

#feature scaling
#Check the data normality
qqnorm(Train_data_new$total.day.calls)
hist(Train_data_new$total.day.calls)

#normalizaion mehod
cnames1=c("account.length", "area.code", "number.vmail.messages", "total.day.calls",
          "total.day.charge", "total.eve.calls", "total.eve.charge", "total.night.calls",
          "total.night.charge", "total.intl.calls", "total.intl.charge", "number.customer.service.calls")
for (i in cnames1){
  print(i)
  Train_data_new[,i]= (Train_data_new[,i]-min(Train_data_new[,i]))/(max(Train_data_new[,i]-min(Train_data_new[,i])))
}
range(Train_data_new$total.intl.charge)

#test data pre processing technique before feedinf into model
#check outlier analysis
ggplot(Test_data, aes(x=Test_data$Churn, y=Test_data$account.length, fill= Test_data$Churn))+geom_boxplot(outlier.colour = "red", outlier.size = 3)+ggtitle("Outlier analysis- Account Length")
  
#Data Manipulation convert string categorical into factor numeric
for(i in 1:ncol(Test_data)){
  if (class(Test_data[,i])== 'factor'){
    Test_data[,i]=factor(Test_data[,i], labels = (1:length(levels(factor(Test_data[,i])))))
  }
}

View(Train_data)
numeric_index= sapply(Test_data, is.numeric)
numeric_index
numeric_data= Test_data[, numeric_index]
cnames= colnames(numeric_data)

#replace the outlier with NA
for (i in cnames){
  val= Train_data[,i][Train_data[,i] %in% boxplot.stats(Train_data[,i])$out]
  #print(length(val))
  Train_data[,i][Train_data[,i] %in% val] =NA
}

sum(is.na(Train_data))
View(Train_data)
library
Train_data=knnImputation(Train_data, k=3)

#correlation plot
corrgram(Test_data[,numeric_index], order=F,
         upper.panel=panel.pie, text.panel=panel.txt, main="correlation plot")

#chi sqaure of independence and selecting only categorical variable
factor_index=sapply(Test_data, is.factor)
factor_data= Test_data[, factor_index]
factor_index
View(factor_data)
factor_index=subset(factor_index, select=-phone.number)

#chi-square test
for (i in 1:4){
  print(names(factor_data)[i])
  print(chisq.test(table(factor_data$Churn, factor_data[,i])))
}


#chi square for train data
factor_index=sapply(Train_data, is.factor)
factor_data= Train_data[, factor_index]
factor_index
View(factor_data)
factor_index=subset(factor_index, select=-phone.number)

#chi-square test
for (i in 1:4){
  print(names(factor_data)[i])
  print(chisq.test(table(factor_data$Churn, factor_data[,i])))
}

#dimension reduction operation 
Test_data_new= subset(Test_data, 
                       select = -c(total.day.minutes, total.eve.minutes, total.night.minutes, total.intl.minutes, phone.number))
View(Train_data_new)

#feature scaling
#Check the data normality
qqnorm(Train_data_new$total.day.calls)
hist(Train_data_new$total.day.calls)

#normalizaion mehod
cnames1=c("account.length", "area.code", "number.vmail.messages", "total.day.calls",
          "total.day.charge", "total.eve.calls", "total.eve.charge", "total.night.calls",
          "total.night.charge", "total.intl.calls", "total.intl.charge", "number.customer.service.calls")
for (i in cnames1){
  print(i)
  Test_data_new[,i]= (Test_data_new[,i]-min(Test_data_new[,i]))/(max(Test_data_new[,i]-min(Test_data_new[,i])))
}
range(Test_data_new$total.intl.charge)


#building Decision tree on train data
#decision tree classificatio
#develop modle on the training data
library(C50)
C50_model= C5.0(Churn ~., Train_data_new, trails=100, rules= TRUE)
summary(C50_model)
write(capture.output(summary(C50_model)), "C50rules2.txt")
C50_model_prediction=predict(C50_model, Test_data_new[-16], type="class")
C50_model_prediction

#error matrix
#evaluate the performance of model
confmatrix_C50=table(Test_data_new$Churn, C50_model_prediction)
confmatrix_C50

#accuracy
(1341+147)/(1341+102+77+147)

#False negative rate
77/(77+147)

#performance of model
library(ROCR)
pred=predict(C50_model, Test_data_new, type='prob')
C50_model_prediction= prediction(C50_model_prediction, Test_data_new$Churn)
eval=performance(pred, "acc")
plot(eval)

#Random forest algorithm
library(randomForest)
RF_model= randomForest(Churn ~., Train_data_new, importance=TRUE, ntree=100)

#extract rules from random forest
library(inTrees)
treelist= RF2List(RF_model)
exec=extractRules(treelist, Train_data_new[,-16])
exec[1:2,]

#make rules more readable
readablerules= presentRules(exec, colnames(Train_data_new))
readablerules[1:2,]
rulemetrix=getRuleMetric(exec, Train_data_new[,-16], Train_data_new$Churn)
rulemetrix[1:2,]
RF_prediction= predict(RF_model, Test_data_new[, -16])
RF_prediction
confmatrix_RF= table(Test_data_new$Churn, RF_prediction)
confmatrix_RF
#accuracy
(1312+163)/(1312+131+61+163)
#False negative rate
61/(61+163)


#logistic regression
logit_model= glm(Churn ~., Train_data_new, family = 'binomial')
summary(logit_model)

#predict logistic regression
logit_prediction= predict(logit_model, Test_data_new, type="response")
logit_prediction
logit_prediction=ifelse(logit_prediction > 0.5, 1, 0)
logit_prediction
#confusion matrix
confmatrix_LR= table(Test_data_new$Churn, logit_prediction)
confmatrix_LR
confmatrix_LR

#ROC Curve
logit_prediction=prediction(logit_prediction, Test_data_new$Churn)
eval= performance(logit_prediction, "acc")
plot(eval)
abline(h=0.85, v=1)
eval

#identity best value
max= which.max(slot(eval, "y.values")[[1]])
acc= slot(eval, "y.values")[[1]][max]
acc
cut=slot(eval, "x.values")[[1]][max]
cut
print(c(Accuracy=acc, cutoff=cut))

#receiver operating characteristics (ROC) CURVE
roc=performance(logit_prediction, "tpr", "fpr")
plot(roc,
     colorize=T,
     main="ROC CURVE",
     xlab="sensitivity",
     ylab="1-Specificity")
abline(a=0, b=1)

#area under AUC(auc)
auc=performance(logit_prediction, "auc")
auc=unlist(slot(auc, "y.values"))
auc
auc=round(auc, 4)
auc
legend(0.6, 0.2, auc, title="AUC", cex = 0.8)
#accuracy

(1374+66)/(1374+69+158+66)

#False negative rate
158/(158+66)

#KNN(k nearest neighbour) classification
library(class)
knn_predictions= knn(Train_data_new[,1:15], Test_data_new[,1:15], Train_data_new$Churn, k=7)
knn_predictions
confmatrix_knn=table(knn_predictions, Test_data_new$Churn)
confmatrix_knn
#accuracy
sum(diag(confmatrix_knn))/nrow(Test_data_new)
#false negative rate
51/(51+43)


#naive bayes
library(e1071)
NB_model=naiveBayes(Churn ~., Train_data_new)
NB_prediction= predict(NB_model, Test_data_new[,1:15], type='class')
confmatrix_NB= table(Test_data_new[,16], NB_prediction)
confmatrix_NB
#accuracy
sum(diag(confmatrix_NB))/nrow(Test_data_new)
#False negative rate
120/(120+104)

# scatter plot
ggplot(Train_data, aes(x= Train_data$total.day.minutes, y= Train_data$total.day.calls))+geom_point(aes(colour=Train_data$Churn), size=2)+
  theme_bw()+ggtitle("main")+ theme(text=element_text(size = 10))+scale_shape_discrete(name="international")+
  scale_color_discrete(name="Churn")

ggplot(Train_data, aes(x= Train_data$account.length, y= Train_data$number.vmail.messages))+geom_point(aes(colour=Train_data$Churn), size=2)+
  theme_bw()+ggtitle("main")+ theme(text=element_text(size = 10))+scale_shape_discrete(name="international.plan")+
  scale_color_discrete(name="Churn")

ggplot(Train_data, aes(x= Train_data$total.intl.minutes, y= Train_data$total.intl.calls))+geom_point(aes(colour=Train_data$Churn), size=2)+
  theme_bw()+ggtitle("main")+ theme(text=element_text(size = 10))+scale_shape_discrete(name="international.plan")+
  scale_color_discrete(name="Churn")

ggplot(Train_data, aes(x= Train_data$area.code, y= Train_data$number.vmail.messages))+geom_point(aes(colour=Train_data$Churn), size=2)+
  theme_bw()+ggtitle("main")+ theme(text=element_text(size = 10))+scale_shape_discrete(name="international.plan")+
  scale_color_discrete(name="Churn")

ggplot(Train_data, aes(x= Train_data$total.eve.minutes, y= Train_data$total.eve.calls))+geom_point(aes(colour=Train_data$Churn), size=2)+
  theme_bw()+ggtitle("main")+ theme(text=element_text(size = 10))+scale_shape_discrete(name="international.plan")+
  scale_color_discrete(name="Churn")

ggplot(Train_data, aes(x= Train_data$total.night.calls, y= Train_data$total.night.minutes))+geom_point(aes(colour=Train_data$Churn), size=2)+
  theme_bw()+ggtitle("main")+ theme(text=element_text(size = 10))+scale_shape_discrete(name="international.plan")+
  scale_color_discrete(name="Churn")




