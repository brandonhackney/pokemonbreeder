# Load libraries here
library(tidyverse)
library(dplyr)

# Define an environment variable to contain backend data, with getter functions
.pokeData <- new.env(parent = emptyenv())
initData <- function(){
	.pokeData$eggs <- getEggGroups()
	.pokeData$graph <- buildGraph()
}
getEggs <- function(){
	.pokeData$eggs
}
getGraph <- function(){
	.pokeData$graph
}

## Function 1: Get a tibble listing which egg groups each Pokemon belongs to
getEggGroups <- function(){
# Read in the list of Pokemon
# This should be a 251x2 list of Pokemon names and numbers
f1 <- 'dex.tsv'
pokedex <- read.delim(f1, header = TRUE, sep = '\t')

# Read in the ugly table of egg groups
# This should have 16 columns with unequal numbers of rows
# Each column is a list of Pokemon in an "egg group"
f2 <- 'groups.csv'
bgroups <- read.csv(f2, header = TRUE)

# Initialize a 0x16 table
# This will basically checkmark whether the Pokemon in row x belongs to group y
eggMatrix <- bgroups[FALSE,]

# Write into the boolean table cell by cell
# Check each column in bgroups for this row's value, return a boolean
for (i in seq_along(pokedex$Name)) {
	pok <- pokedex$Name[i]
	for (j in seq_along(colnames(bgroups))) {
		eggMatrix[i,j] <- any(str_detect(bgroups[,j], pok))
	}
}

# Append the matrix of booleans to the Pokedex so that the rows are labeled
Eggs <- bind_cols(pokedex, eggMatrix)
# Convert to true booleans, since original columns were chars
Eggs <- Eggs %>% 
	mutate(
		across(
			-(Number:Ratio),
			~ .x == "TRUE"
		)
	)
return(Eggs)
# done!
}

## Function 2: Generate tile contents based on Pokedex number
getCard <- function(i){
	eggs <- getEggs()
	# validate input
	if (!is.numeric(i)){
		i <- name2num(i, eggs)
	}
	if (i == 0) {
		# No results: return a unique card
		myCard <- card(
			# Some sort of NO icon
			card_body(
				bsicons::bs_icon("ban", size = "85px"),
				style = "display: block; margin-left: auto; margin-right: auto;",
			),
			card_footer("Cannot breed")
		)
	}
	else {
		myCard <- card(
			card_image(
				sprintf("https://serebii.net/pokearth/sprites/gold/%03i.png",eggs$Number[i])
			),
			card_footer(eggs$Name[i])
		)
	}
}

## Function 3: Output a list of Pokemon numbers based on input Pokemon name
getNumbers <- function(Pokemon){
	eggs <- getEggs()
	# Find the columns used by this Pokemon
	active_cols <- eggs %>% 
		filter(Name == Pokemon) %>% 
		select(-(Number:Ratio)) %>% 
		select(where(isTRUE)) %>% 
		colnames()
	# Intercept if it belongs to certain groups
	# Ditto is #132
	# Ditto can breed with anything besides Ditto and NoEggs
	
	# NoEggs cannot breed at all, whether with itself or with Ditto
	if ("NoEgg" %in% active_cols) {
		return(0)
	}
	# Neuter can only breed with Ditto, not other members
	else if ("Neuter" %in% active_cols) {
		return(132)
	}
	# Ditto can breed with anything besides NoEggs and other Ditto
	else if ("Ditto" %in% active_cols) {
		active_cols <- eggs %>% 
			select(-c(Number, Name, Ratio, NoEgg, Ditto)) %>% 
			colnames()
	}
	# Everything else can also breed with Ditto
	else {
		active_cols <- active_cols %>% 
			append("Ditto")
	}
	# If we didn't break early, get that Pokemon's egg groups AND Ditto
	candidates <- eggs %>% 
		filter(if_any(all_of(active_cols), ~ .x)) %>% 
		pull(Number)
	
	# Final check: All-male species cannot breed with other all-male species,
	# and the same goes for all-female species
	inputRatio <- eggs$Ratio[eggs$Name == Pokemon]
	if (inputRatio == 0){
		return(candidates) 
	}
	else {
		# Skip anything in the same ratio if it's 1 or 2
		candidates <- candidates[eggs$Ratio[candidates] != inputRatio]
	}
	return(candidates)
}

## Function 4: Find the shortest breeding chain from P1 to P2, if possible.
# If multiple options of the same length exist, returns them all.
findChain <- function(P1, P2){
	# Given the NUMBERS of two Pokemon P1 and P2,
	# return a list of numbers showing how to chain from P1 to P2
	# If there are multiple options of the same length, returns a list of lists
	# if P2 is unreachable from P1, returns NULL instead
	
	eggs <- getEggs()
	# Defensive coding
	if (!is.numeric(P1)) P1 <- name2num(P1, eggs)
	if (!is.numeric(P2)) P2 <- name2num(P2, eggs)
	if (P1 == 0 || P2 == 0) return(NULL)
	if (P1 == P2) return(list(c(P1)))
	
	# Initialize search variables
	queue <- c(P1)
	distance <- rep(Inf, max(eggs$Number))
	distance[P1] <- 0
	parents <- vector("list", max(eggs$Number))
	found_depth <- Inf
	
	while (length(queue) > 0) {
		# Read off the top of a rolling queue
		current <- queue[1]
		queue <- queue[-1]
		
		# If you've found P2, avoid looping backward to its parents
		if (distance[current] >= found_depth) next
		
		# See if P2 is in the list of mates for the current check
		# If so, mark it as "found"
		mateList <- getNumbers(eggs$Name[current])
		if (P2 %in% mateList) {
			parents[[P2]] <- c(parents[[P2]], current)
			found_depth <- distance[current] + 1
		}
		
		# If P2 not found in this layer, restrict search to those who cross groups
		mateList <- checkMultiGroup(mateList)
		
		for (neighbor in mateList) {
			new_dist <- distance[current] + 1
			
			# Where "current" is a parent and "neighbor" is a child:
			# If this is the first time considering this child,
			# mark its distance from P1 and who the parent is,
			# then add it to the queue to be considered on the next while loop
			if (distance[neighbor] == Inf) {
				distance[neighbor] <- new_dist
				parents[[neighbor]] <- current
				queue <- c(queue, neighbor)
			}
			# If we've already considered this child in this layer,
			# don't duplicate it in the queue,
			# but do add this new parent-child occurrence to the 'parents' object
			else if (distance[neighbor] == new_dist) {
				parents[[neighbor]] <- c(parents[[neighbor]], current)
			}
			# The third, unspecified option is to avoid looping backward:
			# if this child has a SHORTER distance from P1 than the current parent,
			# that means we encountered it in a previous loop. So skip it.
		}
	}
	
	# The while loop will eventually quit by running out of unconsidered nodes
	# If P2 was never found, return NULL
	if (is.infinite(found_depth)) return(NULL)
	
	# Otherwise, return a list of lists containing all paths from P1 to P2
	# Start at P2 and work backwards, through the "parents" list, to P1
	# `parents` is a list of 251 lists of parent nodes that led to that row,
	# i.e. the only parents listed are potential children of P1
	# This inherently contains dead ends from P1's perspective,
	# BUT if we start at P2 and work backward, we only consider valid paths to P1.
	paths <- list(c(P2))
	for (i in 1:found_depth){
		new_paths <- list()
		for (path in paths) {
			head_node <- path[1]
			for (p in parents[[head_node]]) {
						new_paths <- append(new_paths, list(c(p, path)))
					}
			} # end for path
		paths <- new_paths
	} # end while
	
	return(paths)
}


## FUNCTION 5: Filter a list of Pokemon to only those in multiple egg groups
checkMultiGroup <- function(checklist){
		getEggs() %>% 
		filter(Number %in% checklist) %>% 
		filter(rowSums(across(Dragon:Ditto)) > 1) %>% 
		pull(Number)
		
}

## Convert a list of Pokemon names to Pokemon numbers
name2num <- function(nameList){
	getEggs() %>% 
		filter(Name %in% nameList) %>% 
		pull(Number)
}