library(tidyverse)
library(igraph)
library(visNetwork)

# Convert table of egg group memberships into a graph structure
# Each Pokemon is a node, and edges imply breeding compatibility
buildGraph <- function(){
	eggs <- getEggs()
	
	# Get a two-column dataframe where each row is an edge between cols 1 and 2
	edgesNamed <- eggs %>% 
		unnest(EggGroups) %>% 
		select(Name, EggGroups)
	
	groupList <- unique(edgesNamed$EggGroups)
	# Flatten out into a single-dimension vector, how igraph expects edge lists
	edgeVec <- as.vector(t(edgesNamed))

	# Edges are between pokemon and egg groups,
	# so groups are considered vertices, indistinguishable from Pokemon.
	# Thus, you need to indicate which "type" of vertex each one is.
	numPok <- nrow(eggs)
	numGrp <- length(groupList)
	type <- c(rep(FALSE,numPok), rep(TRUE,numGrp))
	names(type) <- c(eggs$Name, groupList)
	
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
	noEggIDs <- eggs %>%
		unnest(EggGroups) %>%
		filter(EggGroups == "no-eggs") %>%
		pull(Name) %>% 
		unique()
	neuterIDs <- eggs %>%
		unnest(EggGroups) %>% 
		filter(GenderRatio == -1 & !(EggGroups == "no-eggs")) %>% 
		pull(Name) %>% 
		unique()
	noEggEdges <- E(bipGraph)[noEggIDs %--% eggs$Name]
	neuterEdges <- E(bipGraph)[neuterIDs %--% eggs$Name]
	
	# All-female species cannot "father" anything
	# All-male species cannot "mother" anything
	femaleIDs <- eggs$Name[eggs$GenderRatio == 8]
	maleIDs <- eggs$Name[eggs$GenderRatio == 0]
	# Find edges where females are "fathers" and vice versa
	ffEdges <- E(bipGraph)[.from(femaleIDs)]
	mmEdges <- E(bipGraph)[.to(maleIDs)]

	# Now delete all the bad edges
	badEdges <- c(noEggEdges, neuterEdges,mmEdges,ffEdges)
	newGraph <- delete_edges(bipGraph, badEdges)
	
	# Finally, strip out any edges pointing to Ditto
	# This makes the graph illegible
	# toDittoIDs <- eggs %>%
	# 	filter(!(Name %in% c(noEggIDs))) %>% 
	# 	filter(Name != "Ditto") %>% 
	# 	pull(Name)
	# dittoEdges <- as.vector(rbind(toDittoIDs, "Ditto"))
	toDittoEdges <- E(newGraph)[eggs$Name %--% "Ditto"]
	fromDittoEdges <- E(newGraph)["Ditto" %--% eggs$Name]
	newGraph <- delete_edges(newGraph, c(toDittoEdges, fromDittoEdges))
	
	E(newGraph)$type <- "breeding" # to be differentiated from evolution
	
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

insertSprites <- function(nodes){
	# Add fields to the node data to display sprites
	nodes$shape = "image"
	for (i in 1:nrow(nodes)){
		nodes$image[i] = getSprite(nodes$label[i])
	}
	return(nodes)
}

insertEvoEdges <- function(eggGraph){
	# Consult the Pokedex to establish who evolves into who
	# Add those as a new type of edge to the igraph object
	edges <- getEvoEdges()
	# Flatten from table to paired vector
	edgeList <- edges %>% 
		as.matrix() %>% 
		t() %>% 
		as.vector()
	# Assert that these edges define evolutionary lines
	attrList <- list(
		type = rep("evolution", nrow(edges)))
	# insert into existing graph
	eggGraph <- eggGraph %>%
		add_edges(edgeList, attr = attrList)
	
	return(eggGraph)
}

getEvoEdges <- function(){
	# Construct a list of edges to be inserted into an igraph object
	# Specifically comes from the big table listing who evolves into who
	
	pokedex <- getEggs()
	# Ignore anything with an NA, reduce to fname and EvolvesFrom,
	# then convert "slug" names in both cols to pretty ones from Name
	# output needs cols "from" and "to" to set edges
	pokedex %>% 
		filter(!is.na(EvolvesFrom)) %>%
		left_join(
			pokedex %>% select(fname, Name),
			by = c("EvolvesFrom" = "fname")
		) %>% 
		transmute(
			from = Name.y,
			to = Name.x
		)
}

# Visualize the graph
renderGraph <- function(eggGraph){
	eggGraph <- insertEvoEdges(eggGraph)
	data <- toVisNetworkData(eggGraph)
	nodes <- insertSprites(data$nodes)
	
	data$edges$color.color <- 
		ifelse(data$edges$type == "evolution", "#FFA500", "#1E90FF")
	data$edges$width <-
		ifelse(data$edges$type == "evolution", 4, 0.01) # makes breedinglines real thin
	data$edges$length <-
		ifelse(data$edges$type == "evolution", 30, 500)
	data$edges$arrows.to.scaleFactor <-
		ifelse(data$edges$type == "evolution", 1, 0.5)
	
	repList <- list(nodeDistance = 150, springLength = 200, springConstant = .005, centralGravity = .02)
	stabil <- list(enabled = TRUE, iterations = 300)
	
	visNetwork(nodes = nodes, edges = data$edges) %>% 
		visIgraphLayout(layout = "layout_in_circle", physics = TRUE) %>%
		visNodes(size = 40, font = list(size = 20)) %>% 
		visEdges(arrows = "to") %>% # smooth = TRUE would be nice but my laptop can't handle it
		visPhysics(solver = "repulsion", repulsion = repList) %>%
		visOptions(highlightNearest = list(enabled = T, hover =T, algorithm = "hierarchical"))
}