#Problem Statement: 
"Further, after having sufficient knowledge about the attributes you will perform a predictive task 
of classification to predict whether an individual makes over 50K a year or less, by using different Machine Learning Algorithms.
 "

#Reading a data set
read.csv("D:\\R\\FinalProject\\census-income_ _1_ (1).csv")-> census_ds
View(census_ds)

#Decision Tree##############################--------------------------------------

##importing the required libraries 
library(tree)
library(caTools)
library(rpart)
library(rpart.plot)
library(dplyr)
library(corrplot)
library(ggplot2)
library(ggcorrplot)

str(census_ds)
#Dividing the data set
sample.split(census_ds$X, SplitRatio = 0.7)->split_ds
subset(census_ds, split_ds == T) ->census_train
subset(census_ds, split_ds == F) ->census_test
nrow(census_train)
nrow(census_test)
dim(census_train)
dim(census_test)
prop.table(table(census_train$X))
prop.table(table(census_test$X))

#creating a model
census_mod <- rpart(X~. ,data = census_train, method = "class")
#plotting the decision tree
rpart.plot(census_mod)
summary(census_mod)

#prediction
predict(census_mod, newdata = census_test, type = "class") ->census_predict
table(census_predict)
#confusion matrix
table(census_test$X, census_predict) ->census_matrix
census_matrix
#accuracy calculation
sum(diag(census_matrix))/sum(census_matrix) ->accuracy_dt
accuracy_dt
#comparing actual data with predicted data
data.frame(Actual_data = census_test$X,Prdicted_data = census_predict)->compare_dt
View(compare_dt)


#Random Forest####################----------------------------------------------

#loading the required libraries
library(randomForest)
library(superml)
library(caTools)

str(census_ds)

#i)split the data set
sample.split(census_ds$X, SplitRatio = 0.8) ->split_RF
subset(census_ds, split_RF == T) ->train_RF
subset(census_ds, split_RF == F) ->test_RF
dim(train_RF)
dim(test_RF)
prop.table(table(train_RF$X))
prop.table(table(test_RF$X))

#ii)creating random forest model
randomForest(as.factor(X)~. ,data = train_RF, mtry = 2, ntree=300) ->model_RF
importance(model_RF)

#iii)predict
predict(model_RF, newdata = test_RF, type = "class" )->predict_RF
table(predict_RF)

#iv)Confusion matrix
table(test_RF$X, predict_RF)->matrix_RF
matrix_RF

#accuracy
sum(diag(matrix_RF))/sum(matrix_RF) ->accuracy_RF
accuracy_RF

#comparing actual data with predicted data
data.frame(Actual_dataRF = test_RF$X, Predict_dataRF = predict_RF) ->compare_RF
View(compare_RF)

#Linear Regression##########################----------------------------

#loading the required libraries
library(caTools) #required lib are already loaded

#i)Divide the data set
sample.split(census_ds$hours.per.week, SplitRatio = 0.70) ->split_LR
subset(census_ds, split_LR == T) ->train_LR
subset(census_ds, split_LR == F) ->test_LR
nrow(train_LR)
nrow(test_LR)
dim(train_LR)
dim(test_LR)

#ii)Build linear model on test set
lm(hours.per.week~education.num, data = test_LR) ->model_LR
summary(model_LR)

#iii)predict values on train set
predict(model_LR, newdata = train_LR) ->predict_LR
data.frame(Actual_LR = train_LR$hours.per.week , preicted_val_LR = predict_LR) ->compare_LR
View(compare_LR)
#error in prediction
compare_LR$Actual_LR - compare_LR$preicted_val_LR ->error_LR
View(error_LR)
as.data.frame(error_LR) -> error_LR
View(error_LR)
cbind(compare_LR, error_LR) ->final_LR
View(final_LR)

#iv)RMSE - root mean square error
sqrt(mean(final_LR$error_LR)^2) ->RMSE_LR
RMSE_LR

#Logistic Regression###############-------------------------
#5.A)
#importing the required libraries
library(dplyr)
library(caTools)
library(datasets)

#i)Divide the data set
sample.split(census_ds$X, SplitRatio = 0.65) ->split_logR
subset(census_ds, split_logR==T) ->train_logR
subset(census_ds, split_logR==F) ->test_logR
dim(train_logR)
dim(test_logR)

#ii)Logistic regression model
glm(as.factor(X)~occupation, data = train_logR, family = "binomial" ) ->model_logR
summary(model_logR)

#iii)predict values on test set
predict(model_logR, newdata = test_logR, type = "response") ->predict_logR

#iv)plot accuracy vs cutoff
library(ROCR)
prediction(predict_logR,test_logR$X)-> predict_log_roc
predict_log_roc

acc<-performance(predict_log_roc,'acc')
plot(acc)

lm.pred<-ifelse(predict_logR>0.47,'>50K','<50K')
lm.pred

#V)Confusion matrix
table(lm.pred,test_logR$X)-> matrix_logRA
matrix_logRA 

#accuracy
sum(diag(matrix_logRA))/sum(matrix_logRA) ->accuracy_logR
accuracy_logR

#vi)ROC curve
performance(predict_log_roc,"tpr","fpr")-> roc
plot(roc)
#AUC
install.packages("pROC")
library(pROC)
auc(test_logR$X,predict_logR)

#5.B)
#i)Divide the data set
sample.split(census_ds$X, SplitRatio = 0.80) ->split_logRB
subset(census_ds,split_logRB==T)->train_logRB
subset(census_ds, split_logRB==F)->test_logRB

#ii)Logistic regression model
glm(as.factor(X)~age+workclass+education, data = train_logRB  , family = "binomial") ->model_logRB

#iii)predict values on test set
predict(model_logRB,newdata = test_logRB, type = "response") ->predict_logRB

#iv)plot accuracy vs cutoff
library(ROCR)
prediction(predict_logRB, test_logRB$X)-> predict_log_rocB
predict_log_rocB

accB<-performance(predict_log_rocB ,'acc')
plot(accB)
range(predict_logRB)

#V)Confusion matrix
table(test_logRB$X,predict_logRB>0.47) -> matrix_logRB
matrix_logRB

#accuracy
sum(diag(matrix_logRB))/sum(matrix_logRB) ->accuracy_logRB
accuracy_logRB

#vi)ROC curve
performance(predict_log_rocB ,"tpr","fpr")-> rocB
plot(roc)
#AUC
install.packages("pROC")
library(pROC)
auc(test_logRB$X,predict_logRB)