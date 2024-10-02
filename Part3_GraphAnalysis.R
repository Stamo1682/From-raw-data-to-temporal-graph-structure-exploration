library(igraph)
library(readr)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(data.table)
library(ggraph)
library(tidygraph)
library(RColorBrewer)

setwd("C:\\Users\\fani_\\OneDrive\\Υπολογιστής\\Business_Analytics\\Spring Quarter\\Social Network Analysis\\Projects\\Project 2\\f2822308\\Coauthorships")

# List all CSV files
csv_files <- list.files(pattern = "*.csv")

# Read all CSV files into a list
data_list <- lapply(csv_files, read.csv)

# Optionally, combine them into a single data frame
combined_data <- do.call(rbind, data_list)




##step2
# Initialize data frame to store metrics
metrics_df <- data.frame(Year = integer(),
                         Vertices = integer(),
                         Edges = integer(),
                         Diameter = numeric(),
                         AvgDegree = numeric())

# Function to calculate graph metrics
calculate_metrics <- function(graph, Year) {
  vertices <- vcount(graph)
  edges <- ecount(graph)
  graph_diameter <- diameter(graph, directed = FALSE)
  avg_degree <- mean(degree(graph, mode = "all"))
  
  data.frame(Year = Year,
             Vertices = vertices,
             Edges = edges,
             Diameter = graph_diameter,
             AvgDegree = avg_degree)
}

# Read all CSV files and combine them into a single data frame
combined_data <- do.call(rbind, lapply(csv_files, read.csv))

# Split the combined_data by Year and calculate metrics for each group
metrics_list <- split(combined_data, combined_data$Year) %>%
  lapply(function(x) {
    graph <- graph_from_data_frame(x, directed = FALSE)
    calculate_metrics(graph, unique(x$Year))
  })

metrics_df <- rbindlist(metrics_list, fill = TRUE)


# Plotting the metrics
vertices <- ggplot(metrics_df, aes(x = Year, y = Vertices)) +
  geom_line() +
  geom_point() +
  ggtitle("Number of Vertices Over Time") +
  theme_minimal()

edges <- ggplot(metrics_df, aes(x = Year, y = Edges)) +
  geom_line() +
  geom_point() +
  ggtitle("Number of Edges Over Time") +
  theme_minimal()

diameter <- ggplot(metrics_df, aes(x = Year, y = Diameter)) +
  geom_line() +
  geom_point() +
  ggtitle("Diameter Over Time") +
  theme_minimal()

avg_degree <- ggplot(metrics_df, aes(x = Year, y = AvgDegree)) +
  geom_line() +
  geom_point() +
  ggtitle("Average Degree Over Time") +
  theme_minimal()

# Arrange plots in a grid
grid.arrange(vertices, edges, diameter, avg_degree, ncol = 2)

##step3

# Initialize data frames to store top-10 authors for degree and PageRank
top_degree_df <- data.frame(Year = integer(),
                            Author = character(),
                            Degree = integer())

top_pagerank_df <- data.frame(Year = integer(),
                              Author = character(),
                              PageRank = numeric())

# Function to calculate top-10 authors by degree
calculate_top_degree <- function(graph, year) {
  degree_values <- degree(graph, mode = "all")
  top_10 <- sort(degree_values, decreasing = TRUE)[1:10]
  authors <- names(top_10)
  data.frame(Year = year,
             Author = authors,
             Degree = top_10)
}

# Function to calculate top-10 authors by PageRank
calculate_top_pagerank <- function(graph, year) {
  pagerank_values <- page.rank(graph)$vector
  top_10 <- sort(pagerank_values, decreasing = TRUE)[1:10]
  authors <- names(top_10)
  data.frame(Year = year,
             Author = authors,
             PageRank = top_10)
}

# Split the combined_data by Year and calculate top-10 authors for each group
for (year in unique(combined_data$Year)) {
  data <- combined_data[combined_data$Year == year, ]
  graph <- graph_from_data_frame(data, directed = FALSE)
  
  # Calculate and store top-10 authors by degree
  top_degree <- calculate_top_degree(graph, year)
  top_degree_df <- rbind(top_degree_df, top_degree)
  
  # Calculate and store top-10 authors by PageRank
  top_pagerank <- calculate_top_pagerank(graph, year)
  top_pagerank_df <- rbind(top_pagerank_df, top_pagerank)
}

# Print the results
print("Top-10 Authors by Degree:")
print(top_degree_df)

print("Top-10 Authors by PageRank:")
print(top_pagerank_df)

write.csv(top_degree_df, "top_10_authors_by_degree.csv", row.names = FALSE)
write.csv(top_pagerank_df, "top_10_authors_by_pagerank.csv", row.names = FALSE)


###step4

comm_fast_greedy <- cluster_fast_greedy(graph)
comm_infomap <- cluster_infomap(graph)
comm_louvain <- cluster_louvain(graph)

# compare fast_greedy communities with infomap communities
compare(comm_fast_greedy, comm_infomap)

# compare fast_greedy communities with louvain communities
compare(comm_fast_greedy, comm_louvain)

# compare infomap communities with louvain communities
compare(comm_infomap, comm_louvain)


user <- c("Yong Li 0008")

# subset membership of communities fast greedy by interesting users
membership(comm_fast_greedy)[user]

# subset membership of communities infomap by interesting users
membership(comm_infomap)[user]

# subset membership of communities louvain by interesting users
membership(comm_louvain)[user]


#******comm_louvain*****

# color vertices with communithy membership as a factor
V(graph)$color <- factor(membership(comm_louvain))

# does the edge cross between communities?
# set edge linetype: solid for crossings, dotted otherwise
is_crossing <- crossing(graph, communities = comm_louvain)
E(graph)$lty <- ifelse(is_crossing, "solid", "dotted")

# get the sizes of each community
community_size <- sizes(comm_louvain)
range(community_size)

ggplot(data.frame(community_size), aes(x = community_size)) + 
  geom_histogram(bins = 10, color = "black", fill = "lightblue") + 
  labs(title = "Histogram of Community Sizes", x = "Community Size", y = "Frequency") + 
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, max(community_size), by = 10))

# some mid_size communities
in_mid_comm <- unlist(comm_louvain[community_size > 30 & community_size < 40])

#include a subgraph of graph using in_mid_comm
g_subgraph <- induced_subgraph(graph, in_mid_comm)

g_tbl <- as_tbl_graph(g_subgraph)


ggraph(as_tbl_graph(g_subgraph), layout = 'fr') + 
  geom_edge_link(aes(linetype = E(g_subgraph)$lty), 
                 arrow = arrow(length = unit(2, 'mm'), type = 'closed'), 
                 width = 0.4,
                 show.legend = FALSE) +
  geom_node_point(aes(color = V(g_subgraph)$color), size = 3) +
  theme_void() +
  scale_color_manual(name = "Community Membership", 
                     values = brewer.pal(length(unique(V(g_subgraph)$color)), "Paired")) +
  labs(title = "Mid-sized Communities According to 'Louvain' Clustering")



#******comm_infomap*****

# color vertices with communithy membership as a factor
V(graph)$color <- factor(membership(comm_infomap))


# does the edge cross between communities?
is_crossing <- crossing(graph, communities = comm_infomap)

# set edge linetype: solid for crossings, dotted otherwise
E(graph)$lty <- ifelse(is_crossing, "solid", "dotted")

# get the sizes of each community
community_size <- sizes(comm_infomap)


# Plot histogram of community sizes
ggplot(data.frame(community_size), aes(x = community_size)) + 
  geom_histogram(bins = 30, color = "black", fill = "lightblue") + 
  labs(title = "Histogram of Community Sizes", x = "Community Size", y = "Frequency") + 
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, max(community_size), by = 2))


# some mid_size communities
in_mid_comm <- unlist(comm_infomap[community_size > 20 & community_size < 25])

#include a subgraph of graph using in_mid_comm
g_subgraph <- induced_subgraph(graph, in_mid_comm)


g_tbl <- as_tbl_graph(g_subgraph)


ggraph(as_tbl_graph(g_subgraph), layout = 'fr') + 
  geom_edge_link(aes(linetype = E(g_subgraph)$lty), 
                 arrow = arrow(length = unit(2, 'mm'), type = 'closed'), 
                 width = 0.4,
                 show.legend = FALSE) +
  geom_node_point(aes(color = V(g_subgraph)$color), size = 3) +
  theme_void() +
  scale_color_manual(name = "Community Membership", 
                     values = brewer.pal(length(unique(V(g_subgraph)$color)), "Paired")) +
  labs(title = "Mid-sized Communities According to 'Infomap' Clustering")


#******comm_fast_greedy*****

# color vertices with communithy membership as a factor
V(graph)$color <- factor(membership(comm_fast_greedy))


# does the edge cross between communities?
is_crossing <- crossing(graph, communities = comm_fast_greedy)

# set edge linetype: solid for crossings, dotted otherwise
E(graph)$lty <- ifelse(is_crossing, "solid", "dotted")

# get the sizes of each community
community_size <- sizes(comm_fast_greedy)
range(community_size)


ggplot(data.frame(community_size), aes(x = community_size)) + 
  geom_histogram(bins = 10, color = "black", fill = "lightblue") + 
  labs(title = "Histogram of Community Sizes", x = "Community Size", y = "Frequency") + 
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, max(community_size), by = 20))


# some mid_size communities
in_mid_comm <- unlist(comm_fast_greedy[community_size > 20 & community_size < 28])

#include a subgraph of graph using in_mid_comm
g_subgraph <- induced_subgraph(graph, in_mid_comm)


g_tbl <- as_tbl_graph(g_subgraph)

ggraph(as_tbl_graph(g_subgraph), layout = 'fr') + 
  geom_edge_link(aes(linetype = E(g_subgraph)$lty), 
                 arrow = arrow(length = unit(2, 'mm'), type = 'closed'), 
                 width = 0.4,
                 show.legend = FALSE) +
  geom_node_point(aes(color = V(g_subgraph)$color), size = 3) +
  theme_void() +
  scale_color_manual(name = "Community Membership", 
                     values = brewer.pal(length(unique(V(g_subgraph)$color)), "Paired")) +
  labs(title = "Mid-sized Communities According to 'Fast Greedy' Clustering")
