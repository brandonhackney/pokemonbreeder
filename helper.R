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
			-(Number:Name),
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
		select(-(Number:Name)) %>% 
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
			select(-c(Number, Name, NoEgg, Ditto)) %>% 
			colnames()
	}
	# Everything else can also breed with Ditto
	else {
		active_cols <- active_cols %>% 
			append("Ditto")
	}
	# Everything else, display that Pokemon's egg groups AND Ditto
	#if (length(active_cols) == 0) return(integer(0))
	eggs %>% 
		filter(if_any(all_of(active_cols), ~ .x)) %>% 
		pull(Number)
		
}