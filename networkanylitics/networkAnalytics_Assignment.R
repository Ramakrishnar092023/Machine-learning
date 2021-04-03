# Problem Statement: -
#There are two dataset consists of information for the connecting routes and flight hault. Create network analytics model on both the datasets separately and measure degree Centrality, degree of closeness centrality and degree of in-between centrality respectively.

#*important note 

#.	Perform both R and python code for the above problem
#.	Create network using edge list matrix: directed only
#.	Columns to be used in R:
  
#  Flight_hault=c("ID","Name","City","Country","IATA_FAA","ICAO","Latitude","Longitude","Altitude","Time","DST","Tz database time")
  
#  connecting routes=c("flights", " ID", "main Airport", "main Airport ID", "Destination ","Destination  ID","haults","machinary")
  


library("igraph")
routes <- read.csv("F:/360/net/routes.csv", header = FALSE)
hault <- read.csv("F:/360/net/flight_hault.csv", header = FALSE)
colnames(hault) <- c("ID","Name","City","Country","IATA_FAA","ICAO","Latitude","Longitude","Altitude","Time","DST","Tz database time")
colnames(routes) <- c("flights", " ID", "main Airport", "main Airport ID", "Destination ","Destination  ID","haults","machinary")
head(hault)
head(routes)
AirlineNW <- graph.edgelist(as.matrix(routes[, c(3, 5)]), directed = TRUE)
plot(AirlineNW)
## How many hault are there in the network?
?vcount
vcount(AirlineNW)
## How many connections are there in the network?
?ecount
ecount(AirlineNW)
# Which airport has most flights coming in, and how many?
?degree
indegree <- degree(AirlineNW, mode = "in")
max(indegree)
index <- which(indegree == max(indegree))
indegree[index]
which(hault$IATA_FAA == "ATL")
hault[3584, ]
# Which airport has most flights going out of, and how many?
outdegree <- degree(AirlineNW, mode = "out")
max(outdegree)
index <- which(outdegree == max(outdegree))
outdegree[index]
which(hault$IATA_FAA == "ATL")
hault[3584, ]
# Which airport is close to most of the hault (in terms of number of flights)
?closeness
closeness_in <- closeness(AirlineNW, mode = "in", normalized = TRUE)
max(closeness_in)
index <- which(closeness_in == max(closeness_in))
closeness_in[index]
which(hault$IATA_FAA == "FRA")
hault[338, ]
# Which airport comes in between most of the routes and hence is an important international hub?
?betweenness
btwn <- betweenness(AirlineNW, normalized = TRUE)
max(btwn)
index <- which(btwn == max(btwn))
btwn[index]
which(hault$IATA_FAA == "LAX")
hault[3386,]
# Degree, closeness, and betweenness centralities together
centralities <- cbind(indegree, outdegree, closeness_in, btwn)
colnames(centralities) <- c("inDegree","outDegree","closenessIn","betweenness")
head(centralities)
# correlations of the centralities
cor(centralities)
# Any pair with low correlation?
plot(centralities[, "closenessIn"], centralities[, "betweenness"])
?subset
subset(centralities, (centralities[,"closenessIn"] > 0.015) & (centralities[,"betweenness"] > 0.06))
hault[which(hault$IATA_FAA == "LAX"), ]
hault[which(hault$IATA_FAA == "CDG"), ]
hault[which(hault$IATA_FAA == "ANC"), ]
subset(centralities, (centralities[, "closenessIn"] < 0.005) & (centralities[, "betweenness"] < 0.02))
# Which is one of the most important airport in the world (the Google way)?
?eigen_centrality
eigenv <- eigen_centrality(AirlineNW, directed = TRUE, scale = FALSE, weights = NULL)
eigenv$vector
max(eigenv$vector)
index <- which(eigenv$vector == max(eigenv$vector))
eigenv$vector[index]
which(hault$IATA_FAA == "ATL")
hault[3584, ]
?page_rank
pg_rank <- page_rank(AirlineNW, damping = 0.999) # do not put damping=1; the solution not necessarily converges; put a value close to 1.
pg_rank$vector
max(pg_rank$vector)
index <- which(pg_rank$vector == max(pg_rank$vector))
pg_rank$vector[index]
which(hault$IATA_FAA == "ATL")
hault[3584, ]
#########################################################################################################################
#Social   network	

#Problem statement
#There are three datasets given such as Facebook, Instagram and LinkedIn. Construct and visualize the following networks:
#  .	circular network for Facebook
#.	star network for Instagram
#.	star network for LinkedIn

#*important note

#Perform R code only for the below Facebook, Instagram and Linked datasets
#create a network using adjacency matrix: undirected only, the snapshots of those datasets are given below: - 
  

#for FB
facebook <- read.csv("F:/360/net/facebook.csv", header = TRUE)
fnw <- graph.adjacency(as.matrix(facebook), mode="undirected", weighted=TRUE)
plot(fnw, edge.color=c("dark red", "slategrey"),
     vertex.color="gray40", layout=layout.circle)

#foe LN
linkedin <- read.csv("F:/360/net/linkedin.csv", header = TRUE)
lW <- graph.adjacency(as.matrix(linkedin), mode="undirected", weighted=TRUE)
plot(lW, edge.color=c("dark red", "slategrey"),
     vertex.color="gray40", layout=layout.star)



#for IGM
igm <- read.csv("F:/360/net/instagram.csv", header = TRUE)
ln <- graph.adjacency(as.matrix(igm), mode="undirected", weighted=TRUE)
plot(ln, edge.color=c("dark red", "slategrey"),
     vertex.color="gray40", layout=layout.star)
im <- read.csv("F:/360/net/AnotherStar.csv", header = TRUE)
iw <- graph.adjacency(as.matrix(im), mode="undirected", weighted=TRUE)
plot(iw, edge.color=c("dark red", "slategrey"),
     vertex.color="gray40", layout=layout.star)


