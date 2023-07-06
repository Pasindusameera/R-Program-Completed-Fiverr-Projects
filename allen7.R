##1.	Create a network chart (undirected) 
##	Load packages (library(), igraph, readr, haven, ggplot2)
library(igraph)
library(readr)
library(haven)
library(ggplot2)

# Import data file
data <- read.csv(file.choose(), header = T)
data

# Create a graph object (graph.data.frame) – “undirected”
graph <- graph.data.frame(data, directed = FALSE)
graph


## 2.Modify the layout of the network chart.
# Set seed 
set.seed(1234)
# Define layout ofruchterman.reingold layout
layout <- layout.fruchterman.reingold(graph)
# Plot network with modified layout and colors
plot(graph, layout = layout, vertex.color = "green", edge.color = "grey")



##3.Calculate and Answer the following the following information
# o	Calcuate average path length (average geodesic distance). What does this measure mean?
avg_path_length <- mean_distance(graph)
cat("Average path length:", avg_path_length, "\n")
# The average path length is a measure of the average distance between any two nodes in a network. It reflects the efficiency of communication in the network and can be used to evaluate the network's ability to transfer information or resources. A lower average path length indicates a more connected and efficient network, while a higher average path length indicates a less efficient network with less communication between nodes.

# Calculate degree centrality
degree_centrality <- degree(graph)
cat("Degree centrality:", degree_centrality, "\n")

# Calculate degree centrality
degree_centrality <- degree(graph)
cat("Degree centrality:", degree_centrality, "\n")

# Create data frame using degree centrality
degree_df <- as.data.frame(degree_centrality)
degree_df

# Identify minimum, maximum, and average degree
min_degree <- min(degree_centrality)
max_degree <- max(degree_centrality)
avg_degree <- mean(degree_centrality)
cat("Minimum degree:", min_degree, "\n")
cat("Maximum degree:", max_degree, "\n")
cat("Average degree:", avg_degree, "\n")

#	What is the difference between average path length and the average degree? 
#Why is the result different?
#The average path length measures the average number of steps along the shortest paths for all possible pairs of nodes in a network, providing a measure of the network's efficiency of communication.On the other hand, the average degree measures the average number of connections that a node has in a network, providing a measure of the network's density or connectedness. These two measures capture different aspects of the network structure, and their values may not be strongly correlated. The difference in their values is due to the fact that they are measuring different aspects of the network.




##4.Modify the layout of the network chart by degree centrality. Specifically, change the color of the vertex with degree centrality of higher than 4 to Yellow.
# Calculate the degree centrality
degree_centrality <- degree(graph)
degree_centrality
# Create a vector of colors based on degree centrality
node_colors <- ifelse(degree_centrality > 4, "yellow", "green")
node_colors
# Modify the layout of the network chart
plot(graph, layout = layout.fruchterman.reingold, vertex.color = node_colors, edge.color = "grey")



##5.	Calculate the Closeness Centrality. (1.5 pt)
#	Create a data frame for the closeness centrality (*Hint:data.frame)
closeness <- closeness(graph)
closeness_df <- data.frame(names = V(graph)$name, closeness)
closeness_df
# Identify vertex with highest closeness centrality
highest_closeness <- V(graph)$name[which.max(closeness)]
highest_closeness

##6.Calculate the Betweenness Centrality. (1.5 pt)
#	Create a data frame for the betweenness centrality (*Hint: as.data.frame)
betweenness <- betweenness(graph)
betweenness_df <- as.data.frame(betweenness)
betweenness_df
# Identify vertex with highest betweenness centrality
highest_betweenness <- V(graph)$name[which.max(betweenness)]
highest_betweenness







##7.
# Calculate eigenvector centrality
eigen <- eigen_centrality(graph)$vector
eigen_df <- as.data.frame(eigen)
eigen_df
# Find the node with the lowest Eigenvector Centrality
lowest_eigen <- V(graph)$name[which.min(eigen)]
lowest_eigen



#8.	Repeat the Q1-Q11 process. But this time, create a DIRECTED network chart
#(*Hint: Do not need to re-write the entire thing. Only the assignment, in the beginning, needs to change!)
#	How is the result different from the Undirected network? Choose one of the centrality metrics to look into (e.g., degree centrality, closeness centrality, betweenness centrality, eigenvector centrality). Is the result different from the Undirected network?
#Based on the results, would your decision of the influencer change from Q12?
#Take a Screenshot of the outcome

graph <- graph.data.frame(data, directed = TRUE)
graph
set.seed(1234)
plot(graph, layout = layout_with_fr(graph),
     vertex.color = ifelse(degree(graph) > 4, "yellow", "green"),
     edge.color = "grey")

dc <- degree(graph, mode = "in")
dc_df <- data.frame(name = V(graph)$name, degree_centrality = dc)
dc_df 
#	How is the result different from the Undirected network? Choose one of the centrality metrics to look into (e.g., degree centrality, closeness centrality, betweenness centrality, eigenvector centrality). Is the result different from the Undirected network?

#If we compare this to the degree centrality in the undirected network, we can see that the values are different. In the directed network, we have separate measures for the in-degree and out-degree of each vertex, which can result in different values than in the undirected case.
#Based on the results, would your decision of the influencer change from Q12?
#Based on the results, our decision on the influencer might change depending on the centrality metric we are using. For example, if we focus on in-degree centrality, we can see that vertex 1 has the highest value, which indicates that it receives the most incoming connections. However, if we look at betweenness centrality, vertex 5 has the highest value, which indicates that it plays a critical role in connecting different parts of the network.
#Therefore, it is important to carefully choose the appropriate centrality metric depending on the research question and the characteristics of the network.