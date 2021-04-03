#Name:R.Ramakrishna Raju
#Batch: 09012021
#Q1) Perform Principal component analysis and perform clustering using 
#first 3 principal component scores (both Hierarchical & K-Mean clustering).
#Use Scree plot or elbow curve and obtain optimum number of clusters and check whether
#we have obtained same number of clusters with the original data
# Load the dataset
library(readxl)
input <- read_excel(file.choose())
input <- read.csv(file.choose())
mydata <- input[ , ]
summary(mydata)
# Normalize the data
normalized_data <- scale(mydata[, 2:14]) # Excluding the university name
summary(normalized_data)

#hclustering
# Distance matrix
d <- dist(normalized_data, method = "euclidean") 
fit <- hclust(d, method = "complete")
# Display dendrogram
plot(fit) 
plot(fit, hang = -1)
groups <- cutree(fit, k = 4) # Cut tree into 4 clusters
rect.hclust(fit, k = 4, border = "red")
membership <- as.matrix(groups)
final <- data.frame(membership, mydata)
aggregate(mydata[, 2:14], by = list(final$membership), FUN = mean)
library(readr)
write_csv(final, "hclustoutput.csv")
getwd()

#kmeans clustering
twss <- NULL
for (i in 2:13) {
  twss <- c(twss, kmeans(normalized_data, centers = i)$tot.withinss)
}
twss
# Look for an "elbow" in the scree plot
plot(1:12, twss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")
title(sub = "K-Means Clustering Scree-Plot")
# 3 Cluster Solution
fit <- kmeans(normalized_data, 15) 
str(fit)
fit$cluster
final <- data.frame(fit$cluster, input) # Append cluster membership
aggregate(input[,],by =list(fit$cluster), FUN = mean)

wine <- read.csv(file.choose())
boxplot(wine$Alcohol)
hist(wine$Malic)
plot(x = wine$Alcohol,y = wine$Malic,
     xlab = "Alcohol",
     ylab = "malic",
     main = "Alcohol vs malic"
)


##############################################################################################
#Q2 A Pharmaceutical drug manufacturing company is studying on a new medicine to treat 
#Heart diseases, it has gathered data from its secondary sources, and it would like you 
#to provide high level analytical insights on the data, its aim is to segregate
#patients depending on their age group and other factors as given in the data, 
#perform PCA and Clustering Machine learning Algorithm on the dataset given, and 
#check if the clusters formed before and after PCA are same and provide 
#a brief report on your model. You can also explore more on ways to improve your model. 
#########################################################################################
# Loading heart disease dataset


library(readxl)
input <- read_excel(file.choose())
input <- read.csv(file.choose())
mydata <- input[ , ]
summary(mydata)
# Normalize the data
normalized_data <- scale(mydata[, 2:14]) # Excluding the university name
summary(normalized_data)

#hclustering
# Distance matrix
d <- dist(normalized_data, method = "euclidean") 
fit <- hclust(d, method = "complete")
# Display dendrogram
plot(fit) 
plot(fit, hang = -1)
groups <- cutree(fit, k = 4) # Cut tree into 4 clusters
rect.hclust(fit, k = 4, border = "red")
membership <- as.matrix(groups)
final <- data.frame(membership, mydata)
aggregate(mydata[, 2:14], by = list(final$membership), FUN = mean)
library(readr)
write_csv(final, "hclustoutput.csv")
getwd()

#kmeans clustering
twss <- NULL
for (i in 2:13) {
  twss <- c(twss, kmeans(normalized_data, centers = i)$tot.withinss)
}
twss
# Look for an "elbow" in the scree plot
plot(1:12, twss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")
title(sub = "K-Means Clustering Scree-Plot")
# 3 Cluster Solution
fit <- kmeans(normalized_data, 15) 
str(fit)
fit$cluster
final <- data.frame(fit$cluster, input) # Append cluster membership
aggregate(input[,],by =list(fit$cluster), FUN = mean)

#PCA Clustering
data <- mydata[ , ]
attach(data)
?princomp
pcaObj <- princomp(data, cor = TRUE, scores = TRUE, covmat = NULL)
str(pcaObj)
summary(pcaObj)
loadings(pcaObj)
plot(pcaObj) # graph showing importance of principal components 
biplot(pcaObj)
plot(cumsum(pcaObj$sdev * pcaObj$sdev) * 100 / (sum(pcaObj$sdev * pcaObj$sdev)), type = "b")
pcaObj$scores
pcaObj$scores[, 1:3]
# Top 3 pca scores 
final <- cbind(input[, 1], pcaObj$scores[, 1:3])
View(final)
# Scatter diagram
plot(final$Comp.1, final$Comp.2)

heart <- read.csv(file.choose())
boxplot(heart$sex)
hist(heart$sex)
plot(x = heart$sex,y = heart$cp,
     xlab = "sex",
     ylab = "cp",
     main = "sex vs cp"
)

