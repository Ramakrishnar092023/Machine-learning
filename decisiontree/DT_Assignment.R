#problemstatement1
# Load the Data
# company.csv

company = read.csv(file.choose())

##Exploring and preparing the data ----
str(company)

library(caTools)
set.seed(0)
split <- sample.split(company$Sales, SplitRatio = 0.8)
company_train <- subset(company, split == TRUE)
company_test <- subset(company, split == FALSE)

library(rpart)
model <- rpart(company_train$Sales ~ ., data = company_train,
               control = rpart.control(cp = 0, maxdepth = 3))

# Plot Decision Tree
library(rpart.plot)
rpart.plot(model, box.palette = "auto", digits = -3)

# Measure the RMSE on Test data
test_pred <- predict(model, newdata = company_test, type = "vector")

# RMSE
accuracy1 <- sqrt(mean(company_test$Sales - test_pred)^2)
accuracy1

# Measure the RMSE on Train data
train_pred <- predict(model, newdata = company_train, type = "vector")

# RMSE
accuracy_train <- sqrt(mean(company_train$Sales - train_pred)^2)
accuracy_train


# Prune the Decision Tree

# Grow the full tree
fullmodel <- rpart(company_train$Sales ~ ., data = company_train,
                   control = rpart.control(cp = 0))

rpart.plot(fullmodel, box.palette = "auto", digits = -3)

# Examine the complexity plot
# Tunning parameter check the value of cp which is giving us minimum cross validation error (xerror)
printcp(fullmodel)   
plotcp(model)

mincp <- model$cptable[which.min(model$cptable[, "xerror"]), "CP"]

# Prune the model based on the optimal cp value
model_pruned_1 <- prune(fullmodel, cp = mincp)
rpart.plot(model_pruned_1, box.palette = "auto", digits = -3)

model_pruned_2 <- prune(fullmodel, cp = 0.02)
rpart.plot(model_pruned_2, box.palette = "auto", digits = -3)


# Measure the RMSE using Full tree
test_pred_fultree <- predict(fullmodel, newdata = company_test, type = "vector")
# RMSE
accuracy_f <- sqrt(mean(company_test$Sales - test_pred_fultree)^2)
accuracy_f

# Measure the RMSE using Prune tree - model1
test_pred_prune1 <- predict(model_pruned_1, newdata = company_test, type = "vector")
# RMSE
accuracy_prune1 <- sqrt(mean(company_test$Sales - test_pred_prune1)^2)
accuracy_prune1

# Measure the RMSE using Prune tree - model2
test_pred_prune2 <- predict(model_pruned_2, newdata = company_test, type = "vector")
# RMSE
accuracy_prune2 <- sqrt(mean(company_test$Sales - test_pred_prune2)^2)
accuracy_prune2

# Prediction for trained data result
train_pred_fultree <- predict(fullmodel, company_train, type = 'vector')

# RMSE on Train Data
train_accuracy_fultree <- sqrt(mean(company_train$Sales - train_pred_fultree)^2)
train_accuracy_fultree


# Prediction for trained data result
train_pred_prune1 <- predict(model_pruned_1, company_train, type = 'vector')

# RMSE on Train Data
train_accuracy_fultree2 <- sqrt(mean(company_train$Sales - train_pred_prune1)^2)
train_accuracy_fultree2

# Prediction for trained data result
train_pred_prune2 <- predict(model_pruned_2, company_train, type = 'vector')

# RMSE on Train Data
train_accuracy_fultree2 <- sqrt(mean(company_train$Sales - train_pred_prune2)^2)
train_accuracy_fultree2

########################################################################################################################


#problemstatement2

#Loading relevant libraries
library(mlbench) # Diabetes dataset
library(rpart) # Decision tree
library(rpart.plot) # Plotting decision tree
library(caret) # Accuracy estimation
library(Metrics) # For diferent model evaluation metrics
#Loading dataset
# load the diabetes dataset
Diabetes = read.csv(file.choose())
##Exploring and preparing the data ----
str(Diabetes)

library(caTools)
set.seed(0)
split <- sample.split(Diabetes$Class.variable, SplitRatio = 0.8)
Diabetes_train <- subset(Diabetes, split == TRUE)
Diabetes_test <- subset(Diabetes, split == FALSE)

library(rpart)
model <- rpart(Diabetes_train$Class.variable ~ ., data = Diabetes_train,
               control = rpart.control(cp = 0, maxdepth = 3))

# Plot Decision Tree
library(rpart.plot)
rpart.plot(model, box.palette = "auto", digits = -3)

# Measure the RMSE on Test data
test_pred <- predict(model, newdata = Diabetes_test, type = "vector")

# RMSE
accuracy1 <- sqrt(mean(Diabetess_test$Class.variable - test_pred)^2)
accuracy1

# Measure the RMSE on Train data
train_pred <- predict(model, newdata = Diabetes_train, type = "vector")

# RMSE
accuracy_train <- sqrt(mean(Diabetes_train$Class.variable - train_pred)^2)
accuracy_train


# Prune the Decision Tree

# Grow the full tree
fullmodel <- rpart(Diabetes_train$Class.variable ~ ., data = Diabetes_train,
                   control = rpart.control(cp = 0))

rpart.plot(fullmodel, box.palette = "auto", digits = -3)

# Examine the complexity plot
# Tunning parameter check the value of cp which is giving us minimum cross validation error (xerror)
printcp(fullmodel)   
plotcp(model)

mincp <- model$cptable[which.min(model$cptable[, "xerror"]), "CP"]

# Prune the model based on the optimal cp value
model_pruned_1 <- prune(fullmodel, cp = mincp)
rpart.plot(model_pruned_1, box.palette = "auto", digits = -3)

model_pruned_2 <- prune(fullmodel, cp = 0.02)
rpart.plot(model_pruned_2, box.palette = "auto", digits = -3)


# Measure the RMSE using Full tree
test_pred_fultree <- predict(fullmodel, newdata = Diabetes_test, type = "vector")
# RMSE
accuracy_f <- sqrt(mean(Diabetess_test$Class.variable - test_pred_fultree)^2)
accuracy_f

# Measure the RMSE using Prune tree - model1
test_pred_prune1 <- predict(model_pruned_1, newdata = Diabetes_test, type = "vector")
# RMSE
accuracy_prune1 <- sqrt(mean(Diabetes_test$Class.variable - test_pred_prune1)^2)
accuracy_prune1

# Measure the RMSE using Prune tree - model2
test_pred_prune2 <- predict(model_pruned_2, newdata = Diabetes_test, type = "vector")
# RMSE
accuracy_prune2 <- sqrt(mean(Diabetes_test$Class.variable - test_pred_prune2)^2)
accuracy_prune2

# Prediction for trained data result
train_pred_fultree <- predict(fullmodel, Diabetes_train, type = 'vector')

# RMSE on Train Data
train_accuracy_fultree <- sqrt(mean(Diabetes_train$Class.variable - train_pred_fultree)^2)
train_accuracy_fultree


# Prediction for trained data result
train_pred_prune1 <- predict(model_pruned_1, Diabetes_train, type = 'vector')

# RMSE on Train Data
train_accuracy_fultree2 <- sqrt(mean(Diabetes_train$Class.variable - train_pred_prune1)^2)
train_accuracy_fultree2

# Prediction for trained data result
train_pred_prune2 <- predict(model_pruned_2, Diabetes_train, type = 'vector')

# RMSE on Train Data
train_accuracy_fultree2 <- sqrt(mean(Diabetes_train$Class.variable - train_pred_prune2)^2)
train_accuracy_fultree2

##############################################################################################################
#problemstatement4
# Load the Data
# fraud.csv

fraud = read.csv(file.choose())

##Exploring and preparing the data ----
str(fraud)

library(caTools)
set.seed(0)
split <- sample.split(fraud$TAxble.Income, SplitRatio = 0.8)
fraud_train <- subset(fraud, split == TRUE)
fraud_test <- subset(fraud, split == FALSE)

library(rpart)
model <- rpart(fraud_train$TAxble.Income ~ ., data = fraud_train,
               control = rpart.control(cp = 0, maxdepth = 3))

# Plot Decision Tree
library(rpart.plot)
rpart.plot(model, box.palette = "auto", digits = -3)

# Measure the RMSE on Test data
test_pred <- predict(model, newdata = fraud_test, type = "vector")

# RMSE
accuracy1 <- sqrt(mean(fraud_test$TAxble.Income - test_pred)^2)
accuracy1

# Measure the RMSE on Train data
train_pred <- predict(model, newdata = fraud_train, type = "vector")

# RMSE
accuracy_train <- sqrt(mean(fraud_train$TAxble.Income - train_pred)^2)
accuracy_train


# Prune the Decision Tree

# Grow the full tree
fullmodel <- rpart(fraud_train$TAxble.Income ~ ., data = fraud_train,
                   control = rpart.control(cp = 0))

rpart.plot(fullmodel, box.palette = "auto", digits = -3)

# Examine the complexity plot
# Tunning parameter check the value of cp which is giving us minimum cross validation error (xerror)
printcp(fullmodel)   
plotcp(model)

mincp <- model$cptable[which.min(model$cptable[, "xerror"]), "CP"]

# Prune the model based on the optimal cp value
model_pruned_1 <- prune(fullmodel, cp = mincp)
rpart.plot(model_pruned_1, box.palette = "auto", digits = -3)

model_pruned_2 <- prune(fullmodel, cp = 0.02)
rpart.plot(model_pruned_2, box.palette = "auto", digits = -3)


# Measure the RMSE using Full tree
test_pred_fultree <- predict(fullmodel, newdata = fraud_test, type = "vector")
# RMSE
accuracy_f <- sqrt(mean(fraud_test$TAxble.Income - test_pred_fultree)^2)
accuracy_f

# Measure the RMSE using Prune tree - model1
test_pred_prune1 <- predict(model_pruned_1, newdata = fraud_test, type = "vector")
# RMSE
accuracy_prune1 <- sqrt(mean(fraud_test$TAxble.Income - test_pred_prune1)^2)
accuracy_prune1

# Measure the RMSE using Prune tree - model2
test_pred_prune2 <- predict(model_pruned_2, newdata = fraud_test, type = "vector")
# RMSE
accuracy_prune2 <- sqrt(mean(fraud_test$TAxble.Income - test_pred_prune2)^2)
accuracy_prune2

# Prediction for trained data result
train_pred_fultree <- predict(fullmodel, fraud_train, type = 'vector')

# RMSE on Train Data
train_accuracy_fultree <- sqrt(mean(fraud_train$TAxble.Income - train_pred_fultree)^2)
train_accuracy_fultree


# Prediction for trained data result
train_pred_prune1 <- predict(model_pruned_1, fraud_train, type = 'vector')

# RMSE on Train Data
train_accuracy_fultree2 <- sqrt(mean(fraud_train$TAxble.Income - train_pred_prune1)^2)
train_accuracy_fultree2

# Prediction for trained data result
train_pred_prune2 <- predict(model_pruned_2, fraud_train, type = 'vector')

# RMSE on Train Data
train_accuracy_fultree2 <- sqrt(mean(fraud_train$TAxble.Income - train_pred_prune2)^2)
train_accuracy_fultree2
############################################################################################################################
#problemstatement4
# Load the Data
# HR_DT.csv

HR_DT = read.csv(file.choose())

##Exploring and preparing the data ----
str(HR_DT)

library(caTools)
set.seed(0)
split <- sample.split(HR_DT$monthlyincomeofemployee, SplitRatio = 0.8)
HR_DT_train <- subset(HR_DT, split == TRUE)
HR_DT_test <- subset(HR_DT, split == FALSE)

library(rpart)
model <- rpart(HR_DT_train$monthlyincomeofemployee ~ ., data = HR_DT_train,
               control = rpart.control(cp = 0, maxdepth = 3))

# Plot Decision Tree
library(rpart.plot)
rpart.plot(model, box.palette = "auto", digits = -3)

# Measure the RMSE on Test data
test_pred <- predict(model, newdata = HR_DT_test, type = "vector")

# RMSE
accuracy1 <- sqrt(mean(HR_DT_test$monthlyincomeofemployee - test_pred)^2)
accuracy1

# Measure the RMSE on Train data
train_pred <- predict(model, newdata = HR_DT_train, type = "vector")

# RMSE
accuracy_train <- sqrt(mean(HR_DT_train$monthlyincomeofemployee - train_pred)^2)
accuracy_train


# Prune the Decision Tree

# Grow the full tree
fullmodel <- rpart(HR_DT_train$monthlyincomeofemployee ~ ., data = HR_DT_train,
                   control = rpart.control(cp = 0))

rpart.plot(fullmodel, box.palette = "auto", digits = -3)

# Examine the complexity plot
# Tunning parameter check the value of cp which is giving us minimum cross validation error (xerror)
printcp(fullmodel)   
plotcp(model)

mincp <- model$cptable[which.min(model$cptable[, "xerror"]), "CP"]

# Prune the model based on the optimal cp value
model_pruned_1 <- prune(fullmodel, cp = mincp)
rpart.plot(model_pruned_1, box.palette = "auto", digits = -3)

model_pruned_2 <- prune(fullmodel, cp = 0.02)
rpart.plot(model_pruned_2, box.palette = "auto", digits = -3)


# Measure the RMSE using Full tree
test_pred_fultree <- predict(fullmodel, newdata = HR_DT_test, type = "vector")
# RMSE
accuracy_f <- sqrt(mean(HR_DT_test$monthlyincomeofemployee - test_pred_fultree)^2)
accuracy_f

# Measure the RMSE using Prune tree - model1
test_pred_prune1 <- predict(model_pruned_1, newdata = HR_DT_test, type = "vector")
# RMSE
accuracy_prune1 <- sqrt(mean(HR_DT_test$monthlyincomeofemployee - test_pred_prune1)^2)
accuracy_prune1

# Measure the RMSE using Prune tree - model2
test_pred_prune2 <- predict(model_pruned_2, newdata = HR_DT_test, type = "vector")
# RMSE
accuracy_prune2 <- sqrt(mean(HR_DT_test$monthlyincomeofemployee - test_pred_prune2)^2)
accuracy_prune2

# Prediction for trained data result
train_pred_fultree <- predict(fullmodel, HR_DT_train, type = 'vector')

# RMSE on Train Data
train_accuracy_fultree <- sqrt(mean(HR_DT_train$monthlyincomeofemployee - train_pred_fultree)^2)
train_accuracy_fultree


# Prediction for trained data result
train_pred_prune1 <- predict(model_pruned_1, HR_DT_train, type = 'vector')

# RMSE on Train Data
train_accuracy_fultree2 <- sqrt(mean(HR_DT_train$monthlyincomeofemployee - train_pred_prune1)^2)
train_accuracy_fultree2

# Prediction for trained data result
train_pred_prune2 <- predict(model_pruned_2, HR_DT_train, type = 'vector')

# RMSE on Train Data
train_accuracy_fultree2 <- sqrt(mean(HR_DT_train$monthlyincomeofemployee - train_pred_prune2)^2)
train_accuracy_fultree2



