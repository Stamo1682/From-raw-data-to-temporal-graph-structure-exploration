This project involves analyzing the evolution of co-authorship networks from the DBLP dataset, focusing on five specific conferences. Temporal graphs are created and explored over the last five years, providing insights into collaboration patterns, influential authors, and community structures.

Key steps include:

Data Preprocessing: Filtering conference-specific records.
Graph Construction: Creating undirected weighted graphs for each year.
Graph Analysis: Calculating metrics such as vertices, edges, diameter, and average degree.
Node Ranking: Identifying top authors by degree and PageRank.
Community Detection: Applying clustering algorithms (Fast Greedy, Infomap, Louvain) to analyze collaboration communities.

Outputs
* CSV files for each year representing co-authorship networks.
* Plots depicting the evolution of graph metrics.
* Ranked lists of top authors by degree and PageRank.
* Visualizations of detected communities.
