# Pokemon Breeding Assistant

I built a simple app using [Shiny for R](https://shiny.posit.co/)
that allows you to pick one of the 251 Pokemon in Gold & Silver versions,
then returns a visual report of which other Pokemon it can "breed" with.
The game has certain rules about breeding that make this slightly more complicated
than just filtering a table. Long term, I would also like to implement a search algorithm (like A*)
that lets you pick TWO Pokemon at a time,
then analyzes whether you can chain-breed from one to the other,
and returns a graph of the shortest path.
    
## How to run

Required libraries: shiny, bslib, tidyverse

Clone this repo, open `app.R` in [RStudio](https://posit.co/download/rstudio-desktop/),
and click the "Run App" button.

Below is an example of what the app looks like:

![](assets/dash.png)

## Manifest

* `dex.tsv`: a list of all Pokemon in numerical order.
* `groups.csv` has each egg group as a column, with an unequal number of rows per column,
listing the Pokemon in each group by name.
    * This is far from an ideal format; the entire point of this project was to
    transform this file as a data science exercise.
* `readme.md`: this document.
* `helper.R`: R code for transforming the data
* `app.R`: R code controlling the GUI
