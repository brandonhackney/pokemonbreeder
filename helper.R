## Load libraries
library(tidyverse)
library(dplyr)
library(httr2)
library(jsonlite)

## Set environment variables
# Define something persistent to contain backend data, fill it with said data,
# and define getter functions that access the data
.pokeData <- new.env(parent = emptyenv())
initData <- function(){
	.pokeData$eggs <- getSpeciesTable()
	.pokeData$graph <- buildGraph()
}
getEggs <- function(){
	.pokeData$eggs
}
getGraph <- function(){
	.pokeData$graph
}

setActiveGen <- function(generation){
	.pokeData$generation <- generation
	initData()
}
getActiveGen <- function(){
	.pokeData$generation
}

setActiveVersion <- function(vg){
	.pokeData$vg <- vg
}
getActiveVersion <- function(){
	.pokeData$vg
}

# This happens at an earlier step, so it gets its own function
initGens <- function(){
	.pokeData$genList <- loadGens()
}
getGens <- function(){
	.pokeData$genList
}

## Data manipulation functions
# Once we have data loaded into memory, do something with it
getCard <- function(i){
	# Generate UI tile contents based on input Pokedex number
	eggs <- getEggs()
	# validate input
	if (!is.numeric(i)){
		i <- name2num(i)
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
				getSprite(eggs$Number[i])
			),
			card_footer(eggs$Name[i])
		)
	}
}

getSprite <- function(number){
	# Determine the URL for the selected Pokemon's sprite
	# Depends on selected game generation, potentially game version as well
	if (!is.numeric(number)){
		number <- name2num(number)
	}
	generation <- getActiveGen()
	vg <- getActiveVersion()
	if (generation == "generation-ii" && vg == "gold-silver"){
		# It has options other than the version group, so manually pick one
		vg = "gold"
	}
	webPath <- sprintf("https://serebii.net/pokearth/sprites/%s/%03i.png", vg, number)
	
	return(webPath)
}

getEggNames <- function(groupName){
	# Given the big species table, which contains egg group names in lists,
	# subset the table to just those in that group.
	# Requires expanding the lists and then filtering
	getEggs() %>% 
		unnest(EggGroups) %>% 
		filter(EggGroups == groupName)
}

getNumbers <- function(Pokemon){
	# Output a list of Pokemon numbers based on input Pokemon name
	# Represents the list of breeding "neighbors", i.e. compatibility
	# These are undirected connections:
	# doesn't capture the fact that e.g. Jynx can't produce a baby Abra
	eggs <- getEggs() %>% 
		unnest(EggGroups)
	# Table is expanded so that pokemon in two groups take up two rows
	# This makes it a lot easier to index out of, trust me.
	
	# Get its gender ratio, where -1 means neuter, otherwise x/8 chance female
	genderRatio <- eggs$GenderRatio[eggs$Name == Pokemon] %>% unique()
	
	# Find the columns used by this Pokemon
	activeGroups <- eggs %>% 
		filter(Name == Pokemon) %>% 
		pull(EggGroups)
	# Intercept if it belongs to certain groups
	# Ditto is #132
	# Ditto can breed with anything besides Ditto and NoEggs
	
	# NoEggs cannot breed at all, whether with itself or with Ditto
	if ("no-eggs" %in% activeGroups) {
		return(0)
	}
	# Neuter group can only breed with Ditto, not other members
	else if (genderRatio == -1 && !("no-eggs" %in% activeGroups)) {
		# Ditto's number should be 132 but I'll use filtering anyway
		n <- eggs %>% filter(Name == "Ditto") %>% pull(Number)
		return(n)
	}
	# Ditto can breed with anything besides NoEggs and other Ditto
	else if ("ditto" %in% activeGroups) {
		activeGroups <- eggs %>% 
			select(EggGroups) %>% 
			unique() %>% 
			filter(!(EggGroups %in% c("ditto", "no-eggs"))) %>% 
			pull(EggGroups)
		
	}
	# Everything else can also breed with Ditto
	else {
		activeGroups <- activeGroups %>% 
			append("ditto")
	}
	# If we didn't break early, get everything in the active groups
	candidates <- eggs %>% 
		filter(EggGroups %in% activeGroups) %>% 
		pull(Number) %>% 
		unique()
	
	# Final check: All-male species cannot breed with other all-male species,
	# and the same goes for all-female species
	if (genderRatio %in% c(0, 8)){
		# Skip anything in the same ratio if it has a 0/8 or 8/8 prob of female
		candidates <- eggs %>% 
			filter(GenderRatio != genderRatio) %>% 
			filter(Name %in% candidates) %>% 
			pull(Name) %>% 
			unique()
	}
	return(candidates)
}

findChain <- function(P1, P2){
	# Given the NUMBERS of two Pokemon P1 and P2,
	# return a list of numbers giving the shortest breeding chain from P1 to P2
	# If there are multiple options of the same length, returns a list of lists
	# if P2 is unreachable from P1, returns NULL instead
	
	eggs <- getEggs()
	# Defensive coding
	if (!is.numeric(P1)) P1 <- name2num(P1)
	if (!is.numeric(P2)) P2 <- name2num(P2)
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

checkMultiGroup <- function(checklist){
	# Filter a list of Pokemon to only those in multiple egg groups
	getEggs() %>% 
		filter(Number %in% checklist) %>% 
		unnest(EggGroups) %>% 
		filter(duplicated(Name)) %>% 
		pull(Number)
}

name2num <- function(nameList){
	# Convert a list of Pokemon names to Pokemon numbers
	getEggs() %>% 
		filter(Name %in% nameList) %>% 
		pull(Number)
}


## Data access functions
# Functions that decide whether to read from disk or pull from the internet
loadGens <- function(){
	# Get the list of game generations, e.g. Red/Blue is "generation-i"
	fname <- "generations.rds"
	if (file.exists(fname)){
		gens <- readRDS(fname)
	} else {
		gens <- getGensFromAPI()
		saveRDS(gens, fname)
	}
	return(gens)
}

loadMovesets <- function(generation, subgroup){
	# Get moveset data, e.g. how Pikachu can learn Thunderbolt but not Fly
	# Input 1 is a string from getGens()
	# Input 2 is a string from getVG()
	
	if (missing(subgroup)){
		# Grab the first available subgroup by default
		vg <- getVGfromAPI(generation)
		subgroup <- vg$name[1]
	}
	
	fname <- paste0("movesets_", subgroup, ".rds")
	if (file.exists(fname)){
		moves <- readRDS(fname)
	} else {
		moves <- getMovesFromAPI(generation, subgroup)
		saveRDS(moves, fname)
	}
	return(moves)
}

## Data acquisition functions
# Functions that specifically pull from the internet

hitAPI <- function(pURL){
	# Given a URL that points to a JSON file, grab the data
	# Establish a local cache to save bandwidth
	cacheDir <- ".cache"
	if (!dir.exists(cacheDir)){
		dir.create(cacheDir)
	}
	# httr2 format specifying cacheing
	resp <- pURL %>% 
		request() %>% 
		req_cache(path = cacheDir) %>% 
		req_perform() 
	# Parse response
	ctype <- resp %>% resp_content_type()
	if (ctype == "application/json"){
	output <- resp %>% 
		resp_body_json(simplifyDataFrame = TRUE)
	return(output)
	}
	else {
		stop("Received unexpected API content of type ", ctype)
	}
}

getGensFromAPI <- function(){
	# Pull a list of Pokemon game "generations" from the internet
	# e.g. Red, Blue, Yellow are considered "Generation 1"
	glist <- "https://pokeapi.co/api/v2/generation/" %>% 
		hitAPI()
	glist$results
	# output has properties $name and $url
	# output$name[1] should be 'generation-i'
}

getVGfromAPI <- function(generation){
	# Get the list of "version groups" available in this generation
	# For example, Gen2 has `gold-silver` and `crystal`
	dat <- getGenURL %>% 
		hitAPI()
	dat$version_groups
	# Result should be a table with name and url components
	# The url will be used later by getDataFromAPI
}

getMovesFromAPI <- function(generation, subgroup){
	# Pull a list of legal moves per Pokemon in a given game generation
	# Input 1 should be a string as pulled from getGenList()
	# Input 2 is a string specifying a "version group", e.g. "red-blue"
	
	# First, get the list of legal pokemon by concatenating successive generations
	dex <- getPokedex(generation)
	
	# Next, iterate through each pokemon in dex to get its legal moves
	results <- list()
	for (p in dex) {
		message("Getting moves for ", p)
		dat <- paste0("https://pokeapi.co/api/v2/pokemon/", p) %>% 
			hitAPI()
		for (j in 1:nrow(dat$moves)) {
			# this will have all moves that are legal in ANY game
			# subset to just the ones that are legal for the selected versions
			move_name <- dat$moves$move$name[j]
			version_details <- dat$moves$version_group_details[[j]]
			
			valid_entries <- version_details[
				version_details$version_group$name == subgroup,
			]
			if (nrow(valid_entries) > 0) {
				# iteratively write each legal pokemon-move combo into a tall dataframe
				for (i in seq_len(nrow(valid_entries))) {
					
					results[[length(results) + 1]] <- data.frame(
						pokemon = p,
						move = move_name,
						method = valid_entries$move_learn_method$name[i],
						stringsAsFactors = FALSE
					)
				} # for each move legal to this version of the game
			}
		} # for all possible moves this pokemon has
	} # for each pokemon in the dex
	
	# some final step
	do.call(rbind, results) %>% return()
	
}

getPokedex <- function(generation){
	# Load or download a list of available Pokemon in the given generation
	fname <- sprintf('pokedex_%s.rds', generation)
	if (file.exists(fname)){
		# Load it if we already have it
		dex <- readRDS(fname)
	} else {
		# Pull information from the internet and save to disk
		message("Building Pokedex for ", generation)
		dex <- getPokedexFromAPI(generation)
		saveRDS(dex, fname)
	}
	return(dex)
}

getPokedexFromAPI <- function(generation){
	# Given an input generation, e.g. "generation-ii", build a "Pokedex"
	# Returns an unordered, unformatted list of possible Pokemon
	# Used by other functions to grab more data, build dataframes, etc.
	genList <- getCumulativeGens(generation)
	dex <- c()
	for (gen in genList) {
		dat <- getGenURL(gen) %>% 
			hitAPI()
		for (i in dat$pokemon_species$name){
			dex <- c(dex, i)
			}
	}
	dex <- unique(dex)
}

getSpeciesTable <- function(){
	generation <- getActiveGen()
	# Build or load the "species table", a dataframe with more info than the Dex
	fname <- sprintf('evotable_%s.rds', generation)
	if (file.exists(fname)){
		# Load it if we already have it
		dex <- readRDS(fname)
	} else {
		# Pull information from the internet and save to disk
		message("Building Species Table for ", generation)
		dex <- buildSpeciesTable(generation)
		saveRDS(dex, fname)
	}
	return(dex)
}

buildSpeciesTable <- function(generation){
	# Given an input generation, e.g. "generation-ii", build a "Pokedex"
	# Returns a tibble listing Pokemon names, numbers, egg groups, and evolutions
	# Start with the list of available Pokemon
	dex <- getPokedex(generation)
	master <- tibble(
		fname = "",
		Name = "",
		Number = 0,
		GenderRatio = 0,
		EggGroups = list(),
		EvolutionChain = 0,
		EvolvesFrom = "",
		EvolveDetails = list()
	)
	# Now iterate through that list to extract key info
	for (pok in dex){
		# pok is a "reference name" used by PokeAPI
		# use it to extract a formatted name for display, along with other info
		message(pok)
		dat <- paste0("https://pokeapi.co/api/v2/pokemon-species/", pok) %>% 
			hitAPI()
		# evolution chain number: an index specific to PokeAPI
		evChNum <- dat$evolution_chain$url %>%
			strsplit("/") %>%
			unlist() %>% 
			tail(n = 1) %>%
			as.numeric()
		# Identify previous evolution form and what it takes to evolve
		prevForm <- dat$evolves_from_species$name
		if (!is.null(prevForm) && prevForm %in% dex){
			evoDat <- getEvoDetails(evChNum, pok)
		} else {
			# If the identified evolution isn't valid for this gen,
			# replace with default values
			prevForm = NULL
			evoDat = NULL
		}
		
		# Insert data into output var
		master <- master %>% 
			add_row(
				fname = pok,
				Name = dat$names %>% 
					filter(language$name == "en") %>% 
					pull(name),
				Number = dat$id,
				GenderRatio = dat$gender_rate,
				EggGroups = dat$egg_groups$name %>% list(),
				EvolutionChain = evChNum,
				EvolvesFrom = prevForm,
				EvolveDetails = evoDat
			)
	}
	# Sort table by pokedex number then output
	master %>% 
		arrange(Number) %>% 
		return()
}

getEvoDetails <- function(evoNum, pok){
	# Get info about evolution requirements to get to the target pokemon
	# e.g. for Jolteon, use a Thunder Stone on Eevee
	# Needs the number of the "evolution_chain" pulled from the species JSON
	# Output is another JSON with lots of fields, which needs to be parsed later
	
	# Get evolution chain JSON
	evo <- paste0("https://pokeapi.co/api/v2/evolution-chain/",evoNum) %>% 
		hitAPI()
	# Find out how the selected pokemon evolves from its previous form
	if (evo$chain$species$name == pok){
		# then this is the base form. return a default value
		return(list(NA))
	} else {
		# Step through the chain to find the target pokemon's details
		searchEvoStack(evo$chain, pok) %>% 
			return()
	}
}

# Recursive function to step through all possible nodes in a chain
searchEvoStack <- function(chain, target){
	# Check if the current node in an evolutionary chain has the target Pokemon
	# If so, return the evolution details field
	# Otherwise, step through any nodes nested under the current one
	currentName <- chain$species$name
	if (currentName == target){
		result <- chain$evolution_details
		return(result) # may be a list
	}
	# If the current node isn't the target, check if we can expand it
	nextChain <- chain$evolves_to
	if (!"evolves_to" %in% colnames(nextChain)){
		# assume it's a 1x1 list that needs to be coerced to dataframe
		nextChain <- nextChain %>% as.data.frame()
	}
	n <- nextChain %>% nrow()
	if (is.null(n) || n == 0){
		# This branch has hit a dead end
		return(NA)
	}
	# If the current node is not the target, but has 1+ nested nodes to expand,
	# recursively call this function for each option
	# this guarantees drilling down until you find the target
	
		# If there are options, step through them
		for (i in 1:n){
			# Handle odd nesting
			if (n == 1){
				nc <- nextChain
			} else{
				nc <- nextChain[i,]
			}
			result <- searchEvoStack(nc, target)
			# If current iteration is the target, return it
			# but is.na() may have multiple results, so defensive coding is:
			if (any(!is.na(result))){
				return(result)
			}
		} # end for branches
	# If you reach this point, then none of the nested nodes had the target
	# Return NA and let the previous layer move on to its next nested node
	return(NA)
}

getGenList <- function(){
	# Returns a list of game generation names, e.g. 'generation-i'
	gens <- getGens()
	return(gens$name)
}

getCumulativeGens <- function(targetGen){
	gens <- getGens()
	ind <- which(gens$name == targetGen)
	gens$name[1:ind]
}

getGenURL <- function(generation){
	getGens() %>% 
		filter(name == generation) %>% 
		pull(url)
}
