# Load libraries here
library(tidyverse)

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
return(Eggs)
# done!
}