# Pokemon Breeding Assistant

I built a simple app using [Shiny for R](https://shiny.posit.co/)
that allows you to pick one of the 251 Pokemon in Gold & Silver versions,
then returns a visual report of which other Pokemon it can "breed" with.
The game has certain rules about breeding that make this slightly more complicated
than just filtering a table.

Data is initially pulled via [PokeAPI](https://pokeapi.co/); sprites from [Serebii.net](https://serebii.net/).
Everything is then cached locally to improve performance.
    
## How to run

Required libraries: `shiny`, `bslib`, `tidyverse`, `igraph`, `visNetwork`, `httr2`, `jsonlite`

Clone this repo, open `app.R` in [RStudio](https://posit.co/download/rstudio-desktop/),
and click the "Run App" button.

Below is an example of what the app looks like:

![](assets/dash.png)

## Future plans

Long term, I would also like to implement a search algorithm (like A*)
that lets you pick TWO Pokemon at a time,
then analyzes whether you can chain-breed from one to the other,
and returns a graph of the shortest path.

This is just a data science exercise for me, so I'm unlikely to add support for newer games.
But feel free to fork and expand for your own purposes.

## Manifest

* `app.R`: R code controlling the GUI
* `helper.R`: R code for accessing and transforming the data
* `graphs.R`: R code specific to graph network methods
* `readme.md`: this document.
