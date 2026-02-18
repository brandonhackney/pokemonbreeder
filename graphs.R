library(tidyverse)
library(igraph)

# Convert table of egg group memberships into a graph structure
# Each Pokemon is a node, and edges imply breeding compatibility
buildGraph <- function(eggs){
	# Get just the logical columns (i.e. egg groups) to be our "edges"
	logicals <- eggs %>% 
		select(-(Number:Ratio)) %>% 
		rename(GDitto = Ditto)
	# Rename the Ditto group so that you have unique node names
	
	
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
	
	# Flatten out into a single-dimension vector, how igraph expects edge lists
	edgeVec <- as.vector(t(edgesNamed))

	# Edges are between pokemon and egg groups,
	# so groups are considered vertices, indistinguishable from Pokemon.
	# Thus, you need to indicate which "type" of vertex each one is.
	numPok <- nrow(eggs)
	numGrp <- ncol(logicals)
	type <- c(rep(FALSE,numPok), rep(TRUE,numGrp))
	names(type) <- c(eggs$Name, colnames(logicals))
	
	# Create the igraph structure
	# "Bipartite" means edges only go from one type (or "part") to another
	bipGraph <- make_bipartite_graph(type, edgeVec, directed = FALSE)
	
	# Implement the unusual rules, e.g. two all-male species cannot interbreed
	bipGraph <- editGraph(bipGraph, eggs)
	
	# Project bipartite graph to row-row graph,
	# which should convert pok-group edges to pok-pok edges, ie interconnect
	proj <- bipartite_projection(bipGraph, types = type)
	
	# proj has two graphs: one showing connections bw Pokemon, the other bw groups
	# We only care about the former.
	return(proj[[1]])
}

editGraph <- function(bipGraph, eggs){
	# Find edges to be disconnected
	# Everything in NoEggs and Neuter
	noEggEdges <- incident(bipGraph, "NoEgg")
	neuterEdges <- incident(bipGraph, "Neuter")
	# All-female species should not connect to each other
	# Same for all-male species
	femaleIDs <- eggs$Name[eggs$Ratio == 1]
	maleIDs <- eggs$Name[eggs$Ratio == 2]
	# Find the edges connecting any of these nodes to each other
	ffEdges <- E(bipGraph)[femaleIDs %--% femaleIDs]
	mmEdges <- E(bipGraph)[maleIDs %--% maleIDs]
	# Now delete all those bad edges
	badEdges <- c(noEggEdges, neuterEdges,mmEdges,ffEdges)
	newGraph <- delete_edges(bipGraph, badEdges)
	return(newGraph)
}