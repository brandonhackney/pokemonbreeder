library(tidyverse)
library(igraph)

# Convert table of egg group memberships into a graph structure
# Each Pokemon is a node, and edges imply breeding compatibility
buildGraph <- function(){
	eggs <- getEggs()
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
	
	# Project bipartite graph to row-row graph,
	# which should convert pok-group edges to pok-pok edges, ie interconnect
	proj <- bipartite_projection(bipGraph, types = type)
	
	# proj has two graphs: one showing connections bw Pokemon, the other bw groups
	# We only care about the former.
	proj <- proj[[1]]
	
	# Convert the edges from undirected to mutually-directed
	# Facilitates pathing later on
	proj <- as_directed(proj, mode = "mutual")
	# Implement the unusual rules, e.g. two all-male species cannot interbreed
	proj <- editGraph(proj)
	
	return(proj)
}

editGraph <- function(bipGraph){
	eggs <- getEggs()
	# Reinsert missing self-self edges
	# (e.g. Bulbasaur CAN actually breed with Bulbasaur)
	bipGraph <- add_edges(bipGraph, rep(V(bipGraph), each = 2))
	
	# Completely disconnect everything in NoEggs AND Neuter
	noEggIDs <- eggs$Name[eggs$NoEgg == TRUE]
	neuterIDs <- eggs$Name[eggs$Neuter == TRUE]
	noEggEdges <- E(bipGraph)[noEggIDs %--% noEggIDs]
	neuterEdges <- E(bipGraph)[neuterIDs %--% neuterIDs]
	
	# All-female species cannot "father" anything
	# All-male species cannot "mother" anything
	femaleIDs <- eggs$Name[eggs$Ratio == 1]
	maleIDs <- eggs$Name[eggs$Ratio == 2]
	# Find edges where females are "fathers" and vice versa
	ffEdges <- E(bipGraph)[.from(femaleIDs)]
	mmEdges <- E(bipGraph)[.to(maleIDs)]

	# Now delete all the bad edges
	badEdges <- c(noEggEdges, neuterEdges,mmEdges,ffEdges)
	newGraph <- delete_edges(bipGraph, badEdges)
	
	# Finally, add edges for everything that can breed with Ditto
	toDittoIDs <- eggs$Name[eggs$NoEgg == FALSE & eggs$Ditto == FALSE]
	newEdges <- as.vector(rbind(toDittoIDs, "Ditto"))
	newGraph <- add_edges(newGraph, newEdges)
	
	return(newGraph)
}

# Return results from the graph
getMates <- function(Pokemon){
	eggGraph <- getGraph()
	l <- neighbors(eggGraph, Pokemon)
	result <- V(eggGraph)$name[l]
	if (is_empty(result)){
		return(0)
	} else{
		return(result)
	}
}

# Find path from X to Y
# Returns a character list giving the names of each vertex from X to Y
# But if Y is unreachable from X, it returns 0 instead.
getPath <- function(A, B){
	# This warns that the unweighted algorithm ignores weights
	# but I don't supply weights, so I don't care about the warning.
	suppressWarnings(
	result <- shortest_paths(getGraph(),
								 A,
								 to = B,
								 algorithm = "unweighted",
								 output = "vpath"
								 )
	)
	# Even though we specify output = vpath,
	# the returned object is a list where vpath is element 1,
	# and then it has null values for the other 3 output options.
	# So we have to slice that first element to actually get vpath.
	# Then access the "name" attribute from within THAT
	return(result$vpath[[1]]$name)
}