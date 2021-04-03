#Problem Statement: -
#A glass manufacturing plant, uses different Earth elements to design a new glass based on customer requirements for that they would like to automate the process of classification as it's a tedious job to manually classify it, help the company reach its objective by correctly classifying the Earth elements, by using KNN Algorithm

glass <- read.csv(file.choose())
mydata <- glass[ , ]
table(mydata$Type)

str(mydata$Type)
# recode diagnosis as a factor
mydata$Type <- factor(mydata$Type, levels = c(1,2,3,5,6,7), labels = c("one" , "two" , "three" , "five" , "six" , "seven"))

# table or proportions with more informative labels
round(prop.table(table(mydata$Type)) * 100, digits = 2)

# summarize any three numeric features
summary(mydata)

# create normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# test normalization function - result should be identical
normalize(c(0.01, 0.02, 0.03, 0.04, 0.05))
normalize(c(10, 20, 30, 40, 50))

mydata$Type <- factor(mydata$Type, levels = c("one" , "two" , "three" , "five" , "six" , "seven"), labels = c(1,2,3,5,6,7))

as.numeric(mydata$Type)

# normalize the mydata data
mydata_n <- as.data.frame(lapply(mydata[,1:9], normalize))

# confirm that normalization worked
summary(mydata_n)

# create training and test data
mydata_train <- mydata_n[1:164, ]
mydata_test <- mydata_n[165:214, ]

# create labels for training and test data

mydata_train_labels <- mydata[1:164, 10]
#mydata_train_labels <- mydata_train_labels[["Type"]] 

mydata_test_labels <- mydata[164:214, 10]
#mydata_test_labels <- mydata_test_labels[["Type"]]
#---- Training a model on the data ----

# load the "class" library
install.packages("class")
library(class)

mydata_test_pred <- knn(train = mydata_train, test = mydata_test,cl = mydata_train_labels, k = 19)

## ---- Evaluating model performance ---- ##
confusion_test <- table(x = mydata_train_labels, y = mydata_train_pred)
confusion_test

Accuracy <- sum(diag(confusion_test))/sum(confusion_test)
Accuracy 

# Training Accuracy to compare against test accuracy
mydata_train_pred <- knn(train = mydata_train, test = mydata_train, cl = mydata_train_labels, k=20)

confusion_train <- table(x = mydata_train_labels, y = mydata_train_pred)
confusion_train

Accuracy_train <- sum(diag(confusion_train))/sum(confusion_train)
Accuracy_train

## Improving model performance ----

# use the scale() function to z-score standardize a data frame
mydata_z <- as.data.frame(scale(mydata[-10]))

# confirm that the transformation was applied correctly
summary(mydata_z$area_mean)

# create training and test datasets
mydata_train <- mydata_z[1:164, ]
mydata_test <- mydata_z[165:214, ]

# re-classify test cases
mydata_test_pred <- knn(train = mydata_train, test = mydata_test,cl = mydata_train_labels, k=20)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = mydata_test_labels, y = mydata_test_pred, prop.chisq=FALSE)

# try several different values of k
mydata_train <- mydata_n[1:164, ]
mydata_test <- mydata_n[165:214, ]

install.packages("Rtools")
library(gmodels)
mydata_test_pred <- knn(train = mydata_train, test = mydata_test, cl = mydata_train_labels, k=1)
CrossTable(x = mydata_test_labels, y = mydata_test_pred, prop.chisq=FALSE)

mydata_test_pred <- knn(train = mydata_train, test = mydata_test, cl = mydata_train_labels, k=5)
CrossTable(x = mydata_test_labels, y = mydata_test_pred, prop.chisq=FALSE)

mydata_test_pred <- knn(train = mydata_train, test = mydata_test, cl = mydata_train_labels, k=11)
CrossTable(x = mydata_test_labels, y = mydata_test_pred, prop.chisq=FALSE)

mydata_test_pred <- knn(train = mydata_train, test = mydata_test, cl = mydata_train_labels, k=17)
CrossTable(x = mydata_test_labels, y = mydata_test_pred, prop.chisq=FALSE)

mydata_test_pred <- knn(train = mydata_train, test = mydata_test, cl = mydata_train_labels, k=20)
CrossTable(x = mydata_test_labels, y = mydata_test_pred, prop.chisq=FALSE)

mydata_test_pred <- knn(train = mydata_train, test = mydata_test, cl = mydata_train_labels, k=27)
CrossTable(x = mydata_test_labels, y = mydata_test_pred, prop.chisq=FALSE)

########################################################
pred.train <- NULL
pred.val <- NULL
error_rate.train <- NULL
error_rate.val <- NULL
accu_rate.train <- NULL
accu_rate.val <- NULL
accu.diff <- NULL
error.diff <- NULL

for (i in 1:39) {
  pred.train <- knn(train = mydata_train, test = mydata_train, cl = mydata_train_labels, k = i)
  pred.val <- knn(train = mydata_train, test = mydata_test, cl = mydata_train_labels, k = i)
  error_rate.train[i] <- mean(pred.train!=mydata_train_labels)
  error_rate.val[i] <- mean(pred.val != mydata_test_labels)
  accu_rate.train[i] <- mean(pred.train == mydata_train_labels)
  accu_rate.val[i] <- mean(pred.val == mydata_test_labels)  
  accu.diff[i] = accu_rate.train[i] - accu_rate.val[i]
  error.diff[i] = error_rate.val[i] - error_rate.train[i]
}

knn.error <- as.data.frame(cbind(k = 1:39, error.train = error_rate.train, error.val = error_rate.val, error.diff = error.diff))
knn.accu <- as.data.frame(cbind(k = 1:39, accu.train = accu_rate.train, accu.val = accu_rate.val, accu.diff = accu.diff))
library(ggplot2)
errorPlot = ggplot() + 
  geom_line(data = knn.error[, -c(3,4)], aes(x = k, y = error.train), color = "blue") +
  geom_line(data = knn.error[, -c(2,4)], aes(x = k, y = error.val), color = "red") +
  geom_line(data = knn.error[, -c(2,3)], aes(x = k, y = error.diff), color = "black") +
  xlab('knn') +
  ylab('ErrorRate')
accuPlot = ggplot() + 
  geom_line(data = knn.accu[,-c(3,4)], aes(x = k, y = accu.train), color = "blue") +
  geom_line(data = knn.accu[,-c(2,4)], aes(x = k, y = accu.val), color = "red") +
  geom_line(data = knn.accu[,-c(2,3)], aes(x = k, y = accu.diff), color = "black") +
  xlab('knn') +
  ylab('AccuracyRate')
# Plot for Accuracy
plot(knn.accu[, c(4)], type = "b", xlab = "K-Value", ylab = "DifferenceInAccu") 
# Plot for Error
plot(knn.error[, c(4)], type = "b", xlab = "K-Value", ylab = "DifferenceInError")

#Problem Statement: -
#  A National Park, in India is dealing with a problem of segregation of its species based on the different attributes it has so that they can have cluster of species together rather than  manually classify them, they have taken painstakingly  collected the data and would like you to help them out with a classification model for their  business objective to be achieved, by using KNN Algorithm  classify the different species and draft your inferences in the documentation.

zoo <- read.csv(file.choose())

mydata <- zoo[ , ]
mydata <- mydata[-1]
table(mydata$type)

str(mydata$type)
# recode diagnosis as a factor
mydata$type <- factor(mydata$type, levels = c(1,2,3,4,5,6,7), labels = c("one" , "two" , "three" , "four"  , "five" , "six" , "seven"))

# table or proportions with more informative labels
round(prop.table(table(mydata$type)) * 100, digits = 2)

# summarize any three numeric features
summary(mydata)

# create normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# test normalization function - result should be identical
normalize(c(0.01, 0.02, 0.03, 0.04, 0.05))
normalize(c(10, 20, 30, 40, 50))

mydata$type <- factor(mydata$type, levels = c("one" , "two" , "three" , "four" , "five" , "six" , "seven"), labels = c(1,2,3,4,5,6,7))

as.numeric(mydata$type)

# normalize the mydata data
mydata_n <- as.data.frame(lapply(mydata[,1:16], normalize))

# confirm that normalization worked
summary(mydata_n)

# create training and test data
mydata_train <- mydata_n[1:51, ]
mydata_test <- mydata_n[52:101, ]

# create labels for training and test data

mydata_train_labels <- mydata[1:51, 17]
#mydata_train_labels <- mydata_train_labels[["type"]] 

mydata_test_labels <- mydata[52:101, 17]
#mydata_test_labels <- mydata_test_labels[["type"]]
#---- Training a model on the data ----

# load the "class" library
install.packages("class")
library(class)

mydata_test_pred <- knn(train = mydata_train, test = mydata_test,cl = mydata_train_labels, k = 35)

## ---- Evaluating model performance ---- ##
confusion_test <- table(x = mydata_test_labels, y = mydata_test_pred)
confusion_test

Accuracy <- sum(diag(confusion_test))/sum(confusion_test)
Accuracy 

# Training Accuracy to compare against test accuracy
mydata_train_pred <- knn(train = mydata_train, test = mydata_train, cl = mydata_train_labels, k=20)

confusion_train <- table(x = mydata_train_labels, y = mydata_train_pred)
confusion_train

Accuracy_train <- sum(diag(confusion_train))/sum(confusion_train)
Accuracy_train

## Improving model performance ----

# use the scale() function to z-score standardize a data frame
mydata_z <- as.data.frame(scale(mydata[-17]))

# confirm that the transformation was applied correctly
summary(mydata_z)

# create training and test datasets
mydata_train <- mydata_z[1:51, ]
mydata_test <- mydata_z[52:101, ]

# re-classify test cases
mydata_test_pred <- knn(train = mydata_train, test = mydata_test,cl = mydata_train_labels, k=20)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = mydata_test_labels, y = mydata_test_pred, prop.chisq=FALSE)

# try several different values of k
mydata_train <- mydata_n[1:51, ]
mydata_test <- mydata_n[52:101, ]

install.packages("Rtools")
library(gmodels)
mydata_test_pred <- knn(train = mydata_train, test = mydata_test, cl = mydata_train_labels, k=1)
CrossTable(x = mydata_test_labels, y = mydata_test_pred, prop.chisq=FALSE)

mydata_test_pred <- knn(train = mydata_train, test = mydata_test, cl = mydata_train_labels, k=5)
CrossTable(x = mydata_test_labels, y = mydata_test_pred, prop.chisq=FALSE)

mydata_test_pred <- knn(train = mydata_train, test = mydata_test, cl = mydata_train_labels, k=11)
CrossTable(x = mydata_test_labels, y = mydata_test_pred, prop.chisq=FALSE)

mydata_test_pred <- knn(train = mydata_train, test = mydata_test, cl = mydata_train_labels, k=17)
CrossTable(x = mydata_test_labels, y = mydata_test_pred, prop.chisq=FALSE)

mydata_test_pred <- knn(train = mydata_train, test = mydata_test, cl = mydata_train_labels, k=20)
CrossTable(x = mydata_test_labels, y = mydata_test_pred, prop.chisq=FALSE)

mydata_test_pred <- knn(train = mydata_train, test = mydata_test, cl = mydata_train_labels, k=27)
CrossTable(x = mydata_test_labels, y = mydata_test_pred, prop.chisq=FALSE)

########################################################
pred.train <- NULL
pred.val <- NULL
error_rate.train <- NULL
error_rate.val <- NULL
accu_rate.train <- NULL
accu_rate.val <- NULL
accu.diff <- NULL
error.diff <- NULL

for (i in 1:39) {
  pred.train <- knn(train = mydata_train, test = mydata_train, cl = mydata_train_labels, k = i)
  pred.val <- knn(train = mydata_train, test = mydata_test, cl = mydata_train_labels, k = i)
  error_rate.train[i] <- mean(pred.train!=mydata_train_labels)
  error_rate.val[i] <- mean(pred.val != mydata_test_labels)
  accu_rate.train[i] <- mean(pred.train == mydata_train_labels)
  accu_rate.val[i] <- mean(pred.val == mydata_test_labels)  
  accu.diff[i] = accu_rate.train[i] - accu_rate.val[i]
  error.diff[i] = error_rate.val[i] - error_rate.train[i]
}

knn.error <- as.data.frame(cbind(k = 1:39, error.train = error_rate.train, error.val = error_rate.val, error.diff = error.diff))
knn.accu <- as.data.frame(cbind(k = 1:39, accu.train = accu_rate.train, accu.val = accu_rate.val, accu.diff = accu.diff))
library(ggplot2)
errorPlot = ggplot() + 
  geom_line(data = knn.error[, -c(3,4)], aes(x = k, y = error.train), color = "blue") +
  geom_line(data = knn.error[, -c(2,4)], aes(x = k, y = error.val), color = "red") +
  geom_line(data = knn.error[, -c(2,3)], aes(x = k, y = error.diff), color = "black") +
  xlab('knn') +
  ylab('ErrorRate')
accuPlot = ggplot() + 
  geom_line(data = knn.accu[,-c(3,4)], aes(x = k, y = accu.train), color = "blue") +
  geom_line(data = knn.accu[,-c(2,4)], aes(x = k, y = accu.val), color = "red") +
  geom_line(data = knn.accu[,-c(2,3)], aes(x = k, y = accu.diff), color = "black") +
  xlab('knn') +
  ylab('AccuracyRate')
# Plot for Accuracy
plot(knn.accu[, c(4)], type = "b", xlab = "K-Value", ylab = "DifferenceInAccu") 
# Plot for Error
plot(knn.error[, c(4)], type = "b", xlab = "K-Value", ylab = "DifferenceInError")





























