#Name: R.Ramakrishna Raju
#Batch:09012021
##########################################################################################
#problemStatement1_solution
data_train=read.csv("F:/360/NB/SalaryData_Train.csv",stringsAsFactors = T)
data_test=read.csv("F:/360/NB/SalaryData_Test.csv",stringsAsFactors = T)
str(data_train)
colSums(is.na(data_train))
colSums(is.na(data_test))
#no need to retain numeric variable educationno as it is descritized into education column 
data_train=data_train[,-4]
data_test=data_test[,-4]
summary(data_train$age)
summary(data_test$age)
quantile(data_train$age,probs = c(0.2,0.4,0.6,0.8,1.0))
data_train$age=cut(data_train$age,
                   breaks = c(17.0,26.0,35.0,45.0,60.0,90.0),
                   labels = c("young","young adult","adult","old adult","old"))
data_test$age=cut(data_test$age,
                  breaks = c(17.0,26.0,35.0,45.0,60.0,90.0),
                  labels = c("young","young adult","adult","old adult","old"))
data_train$capitalgain[(data_train$capitalgain)>0]="Yes"
data_train$capitalgain[(data_train$capitalgain)==0]="No"
data_train$capitalgain=as.factor(data_train$capitalgain)
data_test$capitalgain[(data_test$capitalgain)>0]="Yes"
data_test$capitalgain[(data_test$capitalgain)==0]="No"
data_test$capitalgain=as.factor(data_test$capitalgain)
data_train$capitalloss[(data_train$capitalloss)>0]="Yes"
data_train$capitalloss[(data_train$capitalloss)==0]="No"
data_train$capitalloss=as.factor(data_train$capitalloss)
data_test$capitalloss[(data_test$capitalloss)>0]="Yes"
data_test$capitalloss[(data_test$capitalloss)==0]="No"
data_test$capitalloss=as.factor(data_test$capitalloss)
quantile(data_train$hoursperweek,probs = c(0.0,0.2,0.4,0.6,0.8,1.0))
data_train$hoursperweek=cut(data_train$hoursperweek,
                            breaks = c(1.0,36.0,40.0,50.0,99.0),
                            labels = c("low","medium","high","very high"))
data_test$hoursperweek=cut(data_test$hoursperweek,
                           breaks = c(1.0,36.0,40.0,50.0,99.0),
                           labels = c("low","medium","high","very high"))
str(data_train)
str(data_test)
library(e1071)
data_classifier <- naiveBayes(data_train[,c(1:12)], data_train$Salary)
data_classifier
#getting prediction for test data
data_test_pred <- predict(data_classifier, data_test[,c(1:12)])
#calculating accuracy
confusion_test = table(x = data_test$Salary, y = data_test_pred)
Accuracy <- sum(diag(confusion_test))/sum(confusion_test)
#getting prediction for train data
data_train_pred <- predict(data_classifier, data_train[,c(1:12)])
#calculating accuracy
confusion_test = table(x = data_train$Salary, y = data_train_pred)
Accuracy <- sum(diag(confusion_test))/sum(confusion_test)
#getting accuracy difference
accuracy_diff=(mean(data_train_pred==data_train$Salary)-
                 mean(data_test_pred==data_test$Salary))

################################################################################################
#problemStatement2_solution
#retrive dataset
data=read.csv("F:/360/NB/NB_Car_Ad.csv")
#user id is irrelevent in classification
data=data[,-1]
#cheack for missing
colSums(is.na(data))
#convert target value to factor (yes=purchased, no = not purchased)
data$Purchased=factor(data$Purchased,levels = c(1,0),labels = c("yes","no"))
#dataset contain categorical as well as continuous variable
#NB for continuous works when distribution is normal however default distribution is gussian 
#so we need to convert continuous variables to categorical using descritization 
summary(data$EstimatedSalary)
quantile(data$EstimatedSalary,probs = c(0.2,0.4,0.6,0.8,1.0))
data$EstimatedSalary=cut(data$EstimatedSalary,
                         breaks = c(14000,37800,59000,75000,95200,151000),
                         labels = c("very low","low","medium","high","very high"))
summary(data$Age)
data$Age=cut(data$Age,
             breaks = c(12.0,29.75,37.0,46.0,61.0),
             labels = c("young","young adult","adult","old adult"))
data$Gender=as.factor(data$Gender)
str(data)
#splitting dataset to train and test data
library(caTools)
set.seed(123)
split = sample.split(data$Purchased, SplitRatio = 0.7) 
train_data = subset(data, split == "TRUE"); test_data = subset(data, split == "FALSE") 
prop.table(table(data$Purchased))
prop.table(table(train_data$Purchased))
prop.table(table(test_data$Purchased))
#building classifier
library(e1071)
data_classifier <- naiveBayes(train_data[,c(1:3)], train_data$Purchased)
data_classifier
#getting prediction for test data
data_test_pred <- predict(data_classifier, test_data[,c(1:3)])
#calculating accuracy
confusion_test = table(x = test_data$Purchased, y = data_test_pred)
Accuracy <- sum(diag(confusion_test))/sum(confusion_test)
#getting prediction for train data
data_train_pred <- predict(data_classifier, train_data[,c(1:3)])
#calculating accuracy
confusion_test = table(x = train_data$Purchased, y = data_train_pred)
Accuracy <- sum(diag(confusion_test))/sum(confusion_test)
#getting accuracy difference
accuracy_diff=(mean(data_train_pred==train_data$Purchased)-
                 mean(data_test_pred==test_data$Purchased))


