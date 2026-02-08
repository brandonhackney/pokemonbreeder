library(tidyverse)
library(igraph)

# Convert egg group table to a graph structure
buildGraph <- function(eggs){
	# Get just the logical columns (i.e. egg groups) to be our "edges"
	logicals <- eggs %>% 
		select(-(Number:Ratio))
	
	# Convert logical table into a list of the TRUE indices (ie row/col)
	# This defines graph edges between each Pokemon and their group indices
	# It does NOT yet define edges between individual Pokemon in those groups
	edges <- logicals %>% 
		as.matrix() %>% 
		which(arr.ind = TRUE)
	
	# Convert indices to names
	edgesNamed <- data.frame(
		Pokemon = eggs$Name[edges[,1]],
		EggGroup = names(logicals)[edges[,2]]
	)
	
	# Convert to a bipartite graph format
	graph_from_data_frame(edgesNamed, directed = FALSE)
}

# Project bipartite graph to row-row graph,
# which should convert group connections to interconnections
# ...currently fails because 'types' is not properly set
#proj <- bipartite_projection(newGraph)
