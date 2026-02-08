# Load libraries here
library(tidyverse)
library(dplyr)

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
getCard <- function(i, eggs){
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
getNumbers <- function(Pokemon, eggs){
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
findChain <- function(P1, P2, eggs){
	# Given the NUMBERS of two Pokemon P1 and P2,
	# Return a "PATH" that consists of multiple nodes
	# Each node is a "step" in the breeding chain from P1 to P2
	# Nodes may contain multiple Pokemon if multiple chains exist
	# This is essentially a breadth-first search, since we want the shortest path
	
	TESTED <- P1
	PATH <- list(P1)
	depth <- 0
	found <- FALSE
	while (!found) {
		depth <- depth + 1
		numDeadEnds = 0
		# Check results for each Pokemon in the next layer of PATH
		for (mon in PATH[[depth]]) {
			TESTED <- append(TESTED, mon) # avoid recursion
			mateList <- getNumbers(eggs$Name[mon], eggs)
			if (P2 %in% mateList){
				# If the target Pokemon can mate with the active check
				PATH[depth+1] <- P2
				found <- TRUE
				# But don't break out of the for loop: check for all possibilities
			} else {
				# If the target Pokemon is NOT in the current mate list,
				# then add all untested mates of the active mon to our next test layer,
				# filtered to just those who span multiple egg groups.
				mateList <- checkMultiGroup(mateList, eggs) # filter to group jumpers
				mateList <- discard(mateList, ~ .x %in% TESTED) # drop anything we've tested already
				if (length(mateList) == 0){
					numDeadEnds = numDeadEnds + 1
					# But if active mon is a dead-end, remove it from all layers of Path
					PATH <- discard(PATH, ~ .x %in% mon)
				} else{
						# Add mate list to the next layer
						PATH[depth+1] <- PATH[depth+1] %>% 
							append(mateList) %>% 
							unique() %>% 
							discard(is.null)
				} # end if matelist is empty
				
			} # end if P2 is in matelist
		} # end for each pokemon at this depth
		
		# If everything in this layer is a dead end, then no path exists. Exit.
		if (numDeadEnds == length(PATH[depth])){
			PATH <- NULL
			found <- TRUE
		}
	} # end while not found
	return(PATH)
} # end function

## FUNCTION 5: Filter a list of Pokemon to only those in multiple egg groups
checkMultiGroup <- function(checklist, eggs){
	eggs %>% 
		filter(Number %in% checklist) %>% 
		filter(rowSums(across(Dragon:Ditto)) > 1) %>% 
		pull(Number)
		
}