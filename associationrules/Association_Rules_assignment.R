#Name:R.Ramakrishna Raju
#Batch:09012021

#Problem Statement: - 
#Kitabi Duniya , a famous book store in India, which was established before Independence, 
#the growth of the company was incremental year by year, but due to online selling of books and wide 
#spread Internet access its annual growth started to collapse, seeing sharp downfalls, you as a Data Scientist
#help this heritage book store gain its popularity back and increase footfall of customers and provide ways 
#the business can improve exponentially, apply Association Rule Algorithm, explain the rules, and visualize
#the graphs for clear understanding of solution.


install.packages("arules")
data<-read.csv(file.choose())
attach(data)
books<-data
books


library("arules") # Used for building association rules i.e. apriori algorithm



books[1:5] # showing only top 10 transactions


summary(books)

# making rules using apriori algorithm 
# Keep changing support and confidence values to obtain different rules

# Building rules using apriori algorithm
arules <- apriori(books, parameter = list(support = 0.002, confidence = 0.75, minlen = 2))
arules

# Viewing rules based on lift value
inspect(head(sort(arules, by = "lift"))) # to view we use inspect 

# Overal quality 
head(quality(arules))

# install.packages("arueslViz")
library("arulesViz") # for visualizing rules

# Different Ways of Visualizing Rules
plot(arules)

windows()
plot(arules, method = "grouped")
plot(arules[1:10], method = "graph") # for good visualization try plotting only few rules

write(arules, file = "books_a_rules.csv", sep = ",")

getwd()


#Problem Statement: 
##The Departmental Store, has gathered the data of the products it sells on a 
##Daily basis. Using Association Rules concepts, provide the insights on the rules and the plots.
#2.) Groceries.csv

install.packages("arules")
groceries<-read.csv(file.choose())
attach(groceries)
groceries


library("arules") # Used for building association rules i.e. apriori algorithm



books[1:5] # showing only top 10 transactions


summary(groceries)

# making rules using apriori algorithm 
# Keep changing support and confidence values to obtain different rules

# Building rules using apriori algorithm
arules <- apriori(groceries, parameter = list(support = 0.002, confidence = 0.75, minlen = 2))
arules

# Viewing rules based on lift value
inspect(head(sort(arules, by = "lift"))) # to view we use inspect 

# Overal quality 
head(quality(arules))

# install.packages("arueslViz")
library("arulesViz") # for visualizing rules

# Different Ways of Visualizing Rules
plot(arules)

windows()
plot(arules, method = "grouped")
plot(arules[1:10], method = "graph") # for good visualization try plotting only few rules

write(arules, file = "groceries_a_rules.csv", sep = ",")

getwd()

#Problem Statement: 
#A film distribution company wants to target audience based on their likes and dislikes, you as a Chief Data Scientist Analyze the data and come up with different rules of movie list so that the business objective is achieved.
#3.) my_movies.csv

install.packages("arules")
my_movies<-read.csv(file.choose())
attach(my_movies)
my_movies


library("arules") # Used for building association rules i.e. apriori algorithm



my_movies[1:5] # showing only top 10 


summary(my_movies)

# making rules using apriori algorithm 
# Keep changing support and confidence values to obtain different rules

# Building rules using apriori algorithm
arules <- apriori(my_movies, parameter = list(support = 0.002, confidence = 0.75, minlen = 2))
arules

# Viewing rules based on lift value
inspect(head(sort(arules, by = "lift"))) # to view we use inspect 

# Overal quality 
head(quality(arules))

# install.packages("arueslViz")
library("arulesViz") # for visualizing rules

# Different Ways of Visualizing Rules
plot(arules)

windows()
plot(arules, method = "grouped")
plot(arules[1:10], method = "graph") # for good visualization try plotting only few rules

write(arules, file = "my_movies_a_rules.csv", sep = ",")

getwd()

#Problem Statement: -
#A Mobile Phone manufacturing company wants to launch its three brand new phone into the market, 
#but before going with its traditional marketing approach this time it want to analyze 
#the data of its previous model sales in different regions and you have been hired as 
#an Data Scientist to help them out, use the Association rules concept and provide your insights
#to the company's marketing team to improve its sales.
#4.) myphonedata.csv

install.packages("arules")
myphonedata<-read.csv(file.choose())
attach(myphonedata)
myphonedata


library("arules") # Used for building association rules i.e. apriori algorithm



myphonedata[1:5] # showing only top 10 


summary(myphonedata)

# making rules using apriori algorithm 
# Keep changing support and confidence values to obtain different rules

# Building rules using apriori algorithm
arules <- apriori(myphonedata, parameter = list(support = 0.002, confidence = 0.75, minlen = 2))
arules

# Viewing rules based on lift value
inspect(head(sort(arules, by = "lift"))) # to view we use inspect 

# Overal quality 
head(quality(arules))

# install.packages("arueslViz")
library("arulesViz") # for visualizing rules

# Different Ways of Visualizing Rules
plot(arules)

windows()
plot(arules, method = "grouped")
plot(arules[1:10], method = "graph") # for good visualization try plotting only few rules

write(arules, file = "myphonedata_a_rules.csv", sep = ",")

getwd()

#Problem Statement: - 
#A retail store in India, has its transaction data, and it would like to know the buying pattern of the consumers in its locality, you have been assigned this task to provide the manager with rules on how the placement of products needs to be there in shelves so that it can improve the buying patterns of consumes and increase customer footfall. 
#5.) transaction_retail.csv

install.packages("arules")
transaction_retail<-read.csv(file.choose())
attach(transaction_retail)
transaction_retail


library("arules") # Used for building association rules i.e. apriori algorithm



transaction_retail[1:5] # showing only top 10 


summary(transaction_retail)

# making rules using apriori algorithm 
# Keep changing support and confidence values to obtain different rules

# Building rules using apriori algorithm
arules <- apriori(transaction_retail, parameter = list(support = 0.002, confidence = 0.75, minlen = 2))
arules

# Viewing rules based on lift value
inspect(head(sort(arules, by = "lift"))) # to view we use inspect 

# Overal quality 
head(quality(arules))

# install.packages("arueslViz")
library("arulesViz") # for visualizing rules

# Different Ways of Visualizing Rules
plot(arules)

windows()
plot(arules, method = "grouped")
plot(arules[1:10], method = "graph") # for good visualization try plotting only few rules

write(arules, file = "transaction_retail_a_rules.csv", sep = ",")

getwd()



