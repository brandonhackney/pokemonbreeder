# Pokemon Breeding Aide

The point of this project is to practice some data science skills in my off time.
I have a list of all 251 Pokemon in Gen2, along with a list of the 16 different 
"egg groups" they fall into.
The goal is to organize this data in several steps:

1. Combine the 251 $\times$ 1 Pokedex with the _n_ $\times$ 16 group list to get a
251 $\times$ 16 table that indicates which groups each Pokemon belongs to.
1. Create a Shiny app that can filter the table on the fly, so that you can pick
a Pokemon and get a report of everyone it can directly breed with.
1. Identify the critical Pokemon that belong to multiple egg groups.
1. Long-term, write some code that lets you pick TWO Pokemon at a time,
then analyzes whether you can form a chain from one to the other.
    - For example, if we assume that Gyarados can pass Dragon Rage on to its children, 
and its children's children, etc., could you theoretically pass it to a Stantler
through a long chain of breeding?
    - I assume this will use the A* search algorithm we discussed in Zyg's class.

## Manifest

* `dex.tsv`: a list of all Pokemon in numerical order.
* `groups.csv` has each egg group as a column, with an unequal number of rows per column,
listing the Pokemon in each group.
    * This is far from an ideal format; the entire point of this project is to
    get this data into a more usable state.
* `readme.md`: this document.
* `myFunctions.R`: R code to transform the data
* `app.R`: R code controlling the Shiny app (GUI and execution)
