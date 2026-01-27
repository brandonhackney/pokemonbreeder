library(shiny)
library(bslib)
source("myFunctions.R")

eggs <- getEggGroups()

myList <- eggs$Number[as.logical(eggs$Dragon)]
# A function that organizes the UI elements of the shiny app	
uiF <- page_sidebar(
	title = "title panel",
	sidebar = sidebar("Bulbasaur"),
	
			layout_column_wrap(
				getCard(myList[1],eggs),
				getCard(myList[2],eggs),
				getCard(myList[3],eggs)
			)
	
)

# A function that runs the actual code
serverF <- function(input, output) {
	
}

# Activate the server with the defined UI
shinyApp(ui = uiF, server = serverF)