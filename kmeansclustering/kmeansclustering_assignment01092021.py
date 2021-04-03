# K-Means Clustering
#R.Ramakrishna Raju
# Batch ID 09012021
#############################################################################
import pandas as pd
import numpy as np
import matplotlib.pylab as plt

####################################################################
#1.)	Perform clustering (K means clustering) for the airlines data to obtain optimum number of clusters. Draw the inferences from the clusters obtained. Refer to EastWestAirlines.xlsx dataset.
###########################################################################
from sklearn.cluster import	KMeans
import pandas as pd
# from scipy.spatial.distance import cdist 


# Kmeans on airline dataset 
ewa= pd.read_excel("F:/360/kmeans/EastWestAirlines.xlsx")
en=ewa.drop("ID#",axis=1)
en.describe()

en.columns
# get the number of missing data points per column
missing_values_count = ewa.isnull().sum()

# Scaling and Normalization
from scipy import stats
# for min_max scaling
from mlxtend.preprocessing import minmax_scaling
# plotting modules
import seaborn as sns
import matplotlib.pyplot as plt

# scaling and normalization 
fig, ax = plt.subplots(1,2)
sns.distplot(en.Balance,ax=ax[0])
ax[0].set_title("Original Data_Balance")
scaled_Balance = minmax_scaling(en.Balance, columns=[0])
sns.distplot(scaled_Balance, ax=ax[1])
ax[1].set_title("scaled_Balancedata")
#----------------------------------------
fig, ax = plt.subplots(1,2)
sns.distplot(en.Bonus_miles,ax=ax[0])
ax[0].set_title("Original_Bonus_miles data ")
scaled = minmax_scaling(en.Bonus_miles, columns=[0])
sns.distplot(scaled, ax=ax[1])
ax[1].set_title("scaled_Bonus_miles data ")
#--------------------------------------------------
fig, ax = plt.subplots(1,2)
sns.distplot(en.Days_since_enroll,ax=ax[0])
ax[0].set_title("Original_Days_since_enroll data ")
scaled = minmax_scaling(en.Days_since_enroll, columns=[0])
sns.distplot(scaled, ax=ax[1])
ax[1].set_title("scaled_Days_since_enroll data")
#-------------------------------------------------------------------
# normalizing
from sklearn import preprocessing
def norm_func(i):
    x = (i - i.min())	/ (i.max() - i.min())
    return (x)
df_norm = norm_func(en)
df_norm.describe()



###### scree plot or elbow curve ############
TWSS = []
k = list(range(2, 7))

for i in k:
    kmeans = KMeans(n_clusters = i)
    kmeans.fit(df_norm)
    TWSS.append(kmeans.inertia_)
    
TWSS
# Scree plot 
plt.plot(k, TWSS, 'ro-');plt.xlabel("No_of_Clusters");plt.ylabel("total_within_SS")
# Selecting 4 clusters from the above scree plot which is the optimum number of clusters 
model = KMeans(n_clusters = 4)
model.fit(df_norm)

model.labels_ # getting the labels of clusters assigned to each row 
mb = pd.Series(model.labels_)  # converting numpy array into pandas series object 
en['clust'] = mb # creating a  new column and assigning it to new column 

en.head()
df_norm.head()
en =en.iloc[:,[11,0,1,2,3,4,5,6,7,8,9,10]]
en.head()

en.iloc[:, 2:8].groupby(en.clust).mean()

en.to_csv("Kmeans_airlines.csv", encoding = "utf-8")

import os
os.getcwd()
##############################################################################
#2.)	Perform clustering for the crime data and identify the number of clusters    
#        formed and draw inferences. Refer to crime_data.csv dataset.
##############################################################################
from sklearn.cluster import	KMeans
import pandas as pd
# from scipy.spatial.distance import cdist 


# Kmeans on crime dataset 
crime_data= pd.read_csv("F:/360/kmeans/crime_data.csv")
crime=crime_data.drop("Unnamed: 0",axis=1)
crime.describe()
x=crime.columns

# get the number of missing data points per column
missing_values_count = crime.isnull().sum()


# Scaling and Normalization
from scipy import stats
# for min_max scaling
from mlxtend.preprocessing import minmax_scaling
# plotting modules
import seaborn as sns
import matplotlib.pyplot as plt

# scaling and normalization 
fig, ax = plt.subplots(1,2)
sns.distplot(crime.Murder,ax=ax[0])
ax[0].set_title("Original Murder Data")
scaled_Murder = minmax_scaling(crime.Murder, columns=[0])
sns.distplot(scaled_Murder, ax=ax[1])
ax[1].set_title("scaled_crime_Murder data")
#----------------------------------------
fig, ax = plt.subplots(1,2)
sns.distplot(crime.Assault,ax=ax[0])
ax[0].set_title("Original Assault Data")
scaled_Assault = minmax_scaling(crime.Assault, columns=[0])
sns.distplot(scaled_Assault, ax=ax[1])
ax[1].set_title("scaled_Assault_Data")
#--------------------------------------------------
fig, ax = plt.subplots(1,2)
sns.distplot(crime.UrbanPop,ax=ax[0])
ax[0].set_title("Original_Urbanpop_data ")
scaled = minmax_scaling(crime.UrbanPop, columns=[0])
sns.distplot(scaled, ax=ax[1])
ax[1].set_title("scaled_Urbanpop_data")
#-------------------------------------------------------------------
fig, ax = plt.subplots(1,2)
sns.distplot(crime.Rape,ax=ax[0])
ax[0].set_title("Original_Rape_data ")
scaled = minmax_scaling(crime.Rape, columns=[0])
sns.distplot(scaled, ax=ax[1])
ax[1].set_title("scaled_Rape_data")
#--------------------------------------------
# normalizing
from sklearn import preprocessing
def norm_func(i):
    x = (i - i.min())	/ (i.max() - i.min())
    return (x)
crimedf_norm = norm_func(crime)
crimedf_norm.describe()



###### scree plot or elbow curve ############
TWSS = []
k = list(range(2, 7))

for i in k:
    kmeans = KMeans(n_clusters = i)
    kmeans.fit(crimedf_norm)
    TWSS.append(kmeans.inertia_)
    
TWSS
# Scree plot 
plt.plot(k, TWSS, 'ro-');plt.xlabel("No_of_Clusters");plt.ylabel("total_within_SS")
# Selecting 4 clusters from the above scree plot which is the optimum number of clusters 
model = KMeans(n_clusters = 4)
model.fit(crimedf_norm)

model.labels_ # getting the labels of clusters assigned to each row 
mb = pd.Series(model.labels_)  # converting numpy array into pandas series object 
crime['clust'] = mb # creating a  new column and assigning it to new column 

crime.head()
crimedf_norm.head()
##############################################################################
#3.)	Analyze the information given in the following ‘Insurance Policy dataset’ to             create clusters of persons falling in the same type. Refer to Insurance Dataset.csv
############################################################################
from sklearn.cluster import	KMeans
import pandas as pd
# from scipy.spatial.distance import cdist 


# Kmeans on insurance dataset 
insurance_data= pd.read_csv("F:/360/kmeans/Insurance Dataset.csv")

insurance_data.describe()
x=insurance_data.columns
x

# get the number of missing data points per column
missing_values_count = insurance_data.isnull().sum()
missing_values_count

# Scaling and Normalization
from scipy import stats
# for min_max scaling
from mlxtend.preprocessing import minmax_scaling
# plotting modules
import seaborn as sns
import matplotlib.pyplot as plt


fig, ax = plt.subplots(1,2)

sns.distplot(insurance_data.PremiumsPaid,ax=ax[0])
ax[0].set_title("Original premiumspaid data")
scaled_PremiumsPaid = minmax_scaling(insurance_data.PremiumsPaid , columns=[0])
sns.distplot(scaled_PremiumsPaid, ax=ax[1])
ax[1].set_title("scaled_premiumspaid data")
#----------------------------------------
fig, ax = plt.subplots(1,2)
sns.distplot(insurance_data.Age,ax=ax[0])
ax[0].set_title("Original age data ")
scaled_Age = minmax_scaling(insurance_data.Age, columns=[0])
sns.distplot(scaled_Age, ax=ax[1])
ax[1].set_title("scaled_age data")
#--------------------------------------------------
fig, ax = plt.subplots(1,2)
sns.distplot(insurance_data.DaystoRenew,ax=ax[0])
ax[0].set_title("Original days to renew data ")
scaled = minmax_scaling(insurance_data.DaystoRenew, columns=[0])
sns.distplot(scaled, ax=ax[1])
ax[1].set_title("scaled_DaystoRenew data")
#-------------------------------------------------------------
fig, ax = plt.subplots(1,2)
sns.distplot(insurance_data.Claimsmade,ax=ax[0])
ax[0].set_title("Original claimade data ")
scaled = minmax_scaling(insurance_data.Claimsmade, columns=[0])
sns.distplot(scaled, ax=ax[1])
ax[1].set_title("scaled_Claimsmade data")
#-------------------------------------------------------------------
fig, ax = plt.subplots(1,2)
sns.distplot(insurance_data.Income,ax=ax[0])
ax[0].set_title("Original income ")
scaled = minmax_scaling(insurance_data.Income, columns=[0])
sns.distplot(scaled, ax=ax[1])
ax[1].set_title("scaled_Income data")
#----------------------------------------------------------------------
# normalizing
from sklearn import preprocessing
def norm_func(i):
    x = (i - i.min())	/ (i.max() - i.min())
    return (x)
insurancedf_norm = norm_func(insurance_data)
insurancedf_norm.describe()

###### scree plot or elbow curve ############
TWSS = []
k = list(range(2, 9))

for i in k:
    kmeans = KMeans(n_clusters = i)
    kmeans.fit(insurancedf_norm)
    TWSS.append(kmeans.inertia_)
    
TWSS
# Scree plot 
plt.plot(k, TWSS, 'ro-');plt.xlabel("No_of_Clusters");plt.ylabel("total_within_SS")
# Selecting 4 clusters from the above scree plot which is the optimum number of clusters 
model = KMeans(n_clusters = 7)
model.fit(insurancedf_norm)

model.labels_ # getting the labels of clusters assigned to each row 
mb = pd.Series(model.labels_)  # converting numpy array into pandas series object 
insurance_data['clust'] = mb # creating a  new column and assigning it to new column 

insurance_data.head()
insurancedf_norm.head()

insurance_data = en.iloc[:,[7,0,1,2,3,4,5,6]]
insurance_data.head()

en.iloc[:, 2:8].groupby(en.clust).mean()

en.to_csv("Kmeans_airlines.csv", encoding = "utf-8")

import os
os.getcwd()
############################################################################
#4.)	Perform clustering analysis on the telecom dataset. The data is a mixture of both categorical and numerical data. It consists the number of customers who churn. Derive insights and get possible information on factors that may affect the churn decision. Refer to Telco_customer_churn.xlsx dataset.
#Hint: 
#•	Perform EDA and remove unwanted columns.
#•	Use Gower dissimilarity matrix and  In R use daisy() function.

# Data Preprocessing Tools

# Importing the libraries
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

# Importing the dataset
telcodata= pd.read_excel("F:/360/kmeans/Telco_customer_churn.xlsx")
telco_data=telcodata.drop()

telco_data.describe()
y=telco_data.columns
y

# get the number of missing data points per column
missing_values_count = insurance_data.isnull().sum()
missing_values_count

# Scaling and Normalization
from scipy import stats
# for min_max scaling
from mlxtend.preprocessing import minmax_scaling
# plotting modules
import seaborn as sns
import matplotlib.pyplot as plt

# scaling and normalization 
fig, ax = plt.subplots(1,2)

sns.distplot(insurance_data.PremiumsPaid,ax=ax[0])
ax[0].set_title("Original premiumspaid data")
scaled_PremiumsPaid = minmax_scaling(insurance_data.PremiumsPaid , columns=[0])
sns.distplot(scaled_PremiumsPaid, ax=ax[1])
ax[1].set_title("scaled_premiumspaid data")
#----------------------------------------
fig, ax = plt.subplots(1,2)
sns.distplot(insurance_data.Age,ax=ax[0])
ax[0].set_title("Original age data ")
scaled_Age = minmax_scaling(insurance_data.Age, columns=[0])
sns.distplot(scaled_Age, ax=ax[1])
ax[1].set_title("scaled_age data")
#--------------------------------------------------
fig, ax = plt.subplots(1,2)
sns.distplot(insurance_data.DaystoRenew,ax=ax[0])
ax[0].set_title("Original days to renew data ")
scaled = minmax_scaling(insurance_data.DaystoRenew, columns=[0])
sns.distplot(scaled, ax=ax[1])
ax[1].set_title("scaled_DaystoRenew data")
#-------------------------------------------------------------
fig, ax = plt.subplots(1,2)
sns.distplot(insurance_data.Claimsmade,ax=ax[0])
ax[0].set_title("Original claimade data ")
scaled = minmax_scaling(insurance_data.Claimsmade, columns=[0])
sns.distplot(scaled, ax=ax[1])
ax[1].set_title("scaled_Claimsmade data")
#-------------------------------------------------------------------
fig, ax = plt.subplots(1,2)
sns.distplot(insurance_data.Income,ax=ax[0])
ax[0].set_title("Original income ")
scaled = minmax_scaling(insurance_data.Income, columns=[0])
sns.distplot(scaled, ax=ax[1])
ax[1].set_title("scaled_Income data")
#----------------------------------------------------------------------
# normalizing
from sklearn import preprocessing
def norm_func(i):
    x = (i - i.min())	/ (i.max() - i.min())
    return (x)
insurancedf_norm = norm_func(insurance_data)
insurancedf_norm.describe()

###### scree plot or elbow curve ############
TWSS = []
k = list(range(2, 9))

for i in k:
    kmeans = KMeans(n_clusters = i)
    kmeans.fit(insurancedf_norm)
    TWSS.append(kmeans.inertia_)
    
TWSS
# Scree plot 
plt.plot(k, TWSS, 'ro-');plt.xlabel("No_of_Clusters");plt.ylabel("total_within_SS")
# Selecting 4 clusters from the above scree plot which is the optimum number of clusters 
model = KMeans(n_clusters = 7)
model.fit(insurancedf_norm)

model.labels_ # getting the labels of clusters assigned to each row 
mb = pd.Series(model.labels_)  # converting numpy array into pandas series object 
insurance_data['clust'] = mb # creating a  new column and assigning it to new column 

insurance_data.head()
insurancedf_norm.head()

insurance_data = en.iloc[:,[7,0,1,2,3,4,5,6]]
insurance_data.head()

en.iloc[:, 2:8].groupby(en.clust).mean()

en.to_csv("Kmeans_airlines.csv", encoding = "utf-8")

import os
os.getcwd()


