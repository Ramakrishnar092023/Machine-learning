#Name: R.Ramakrishna Raju
#Batch: 01092021
#Module: Kmeans clustering


#Q1 Perform K means clustering for the airlines data to obtain optimum number of clusters.
# Draw the inferences from the clusters obtained.

library(plyr)
library(readr)
library(animation)

airlines_data <- read_csv(file.choose())

data <- airlines_data[ ,-1]

normalized_data <- scale(data)
normalized_data


twss <- NULL
for (i in 2:6) {
  twss <- c(twss, kmeans(normalized_data, centers = i)$tot.withinss)
}
twss


plot(2:6, twss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")
title(sub = "K-Means Clustering Scree-Plot")

# From scree plot we find optimal k=3

fit_data <- kmeans(normalized_data, 3) 

fit_data$cluster
final_data <- data.frame(fit_data$cluster, data) # Append cluster membership

aggregate(data, by = list(fit_data$cluster), FUN = mean)

write_csv(final_data, "assignment_airlines_data_kmeans.csv")
getwd()

#Q2###Perform K-Means Clustering for the crime data and identify the number of clusters formed and draw inferences.


library(plyr)
library(readr)
library(animation)

crime_data <- read_csv(file.choose())

data <- crime_data[ , 2:5]


normalized_data <- scale(data)
normalized_data


twss <- NULL
for (i in 2:5) {
  twss <- c(twss, kmeans(normalized_data, centers = i)$tot.withinss)
}
twss


plot(2:5, twss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")
title(sub = "K-Means Clustering Scree-Plot")

# From scree plot we find, optimal k=4

fit_data <- kmeans(normalized_data, 4) 

fit_data$cluster
final_data <- data.frame(fit_data$cluster, data) # Append cluster membership

aggregate(data, by = list(fit_data$cluster), FUN = mean)

write_csv(final_data, "assignment_crime_data_kmeans.csv")
getwd()




#Q3 Analyze the information given in the following 'Insurance Policy dataset' to create clusters of persons falling in the same type.



library(plyr)
library(readr)
library(animation)

insurance_data <- read_csv(file.choose())

normalized_data <- scale(insurance_data)
normalized_data


twss <- NULL
for (i in 2:8) {
  twss <- c(twss, kmeans(normalized_data, centers = i)$tot.withinss)
}
twss


plot(2:8, twss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")
title(sub = "K-Means Clustering Scree-Plot")

# From Scree Plot we see optimal k=4

fit_data <- kmeans(normalized_data, 4) 

fit_data$cluster
final_data <- data.frame(fit_data$cluster, insurance_data) # Append cluster membership

aggregate(insurance_data, by = list(fit_data$cluster), FUN = mean)

write_csv(final_data, "assignment_insurance_data_kmeans.csv")
getwd()

#Q44.)	Perform clustering analysis on the telecom dataset. The data is a mixture of both categorical and numerical data. It consists the number of customers who churn. Derive insights and get possible information on factors that may affect the churn decision. Refer to Telco_customer_churn.xlsx dataset.

library(readxl)
library(fastDummies)
input <- read_excel(file.choose())
summary(input)
mydata = subset(input, select = c(Count,Quarter,Number_of_Referrals,Referred_a_Friend))
summary(mydata)
data_dummy <- dummy_cols(mydata, select_columns = c("Offer","Phone Service","Multiple Lines","Internet Service","Internet Type","Online Security","Online Backup","Device Protection Plan","Premium Tech Support","Streaming TV","Streaming Movies","Streaming Music","Unlimited Data","Contract","Paperless Billing","Payment Method"),
                         remove_first_dummy = TRUE,remove_most_frequent_dummy = FALSE,remove_selected_columns = TRUE)
normalized_data <- scale(data_dummy[,2:34])
# Elbow curve to decide the k value
twss <- NULL
for (i in 2:35) {
  twss <- c(twss, kmeans(normalized_data, centers = i)$tot.withinss)
}
twss
# Look for an "elbow" in the scree plot
plot(1:34, twss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")
title(sub = "K-Means Clustering Scree-Plot")
# 15 Cluster Solution
fit <- kmeans(normalized_data, 15) 
str(fit)
fit$cluster
final <- data.frame(fit$cluster, input) # Append cluster membership

aggregate(input[,],by =list(fit$cluster), FUN = mean)



telco <- read_excel(file.choose())
boxplot(telco$`Tenure in Months`)
hist(telco$`Tenure in Months`)
plot(x = telco$`Phone Service`,y = telco$Offer,
     xlab = "Tenure in Months",
     ylab = "Offer",
     main = "Tenure in Months vs Offer"
)


#Q5 5.)	Perform clustering on mixed data convert the categorical  variables to numeric by using dummies or Label Encoding and perform normalization techniques. The data set consists details of customers related to auto insurance. Refer to Autoinsurance.csv dataset.
library(fastDummies)
input <- read.csv(file.choose())
summary(input)
data_dummy <- dummy_cols(input, select_columns =       c("State","Response","Coverage","Education","Effective.To.Date","EmploymentStatus","Gender","Location.Code","Marital.Status","Policy.Type","Policy","Renew.Offer.Type","Sales.Channel","Vehicle.Class","Vehicle.Size"),
                         remove_first_dummy = TRUE,remove_most_frequent_dummy = FALSE,remove_selected_columns = TRUE)
#newdata <- cbind(data, position_new)
# Normalize the data
normalized_data <- scale(data_dummy[,2:110]) # Excluding the university name
summary(normalized_data)
twss <- NULL
for (i in 2:111) {
  twss <- c(twss, kmeans(normalized_data, centers = i)$tot.withinss)
}
twss
# Look for an "elbow" in the scree plot
plot(1:110, twss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")
title(sub = "K-Means Clustering Scree-Plot")
# 55 Cluster Solution
fit <- kmeans(normalized_data, 55) 
str(fit)
fit$cluster
final <- data.frame(fit$cluster, input) # Append cluster membership
aggregate(input[,],by =list(fit$cluster), FUN = mean)
