library(shiny)
library(bslib)
source("myFunctions.R")

eggs <- getEggGroups()

myList <- eggs$Number[eggs$Dragon]
# A function that organizes the UI elements of the shiny app	
uiF <- page_sidebar(
	title = "Pokemon Gen2 Egg Group Analyzer",
	sidebar = sidebar(
		helpText("List all possible mates for the selected Pokemon"),
		selectInput(
			"Dropdown",
			label = "Select a Pokemon",
			choices = eggs$Name
		)
	),

		uiOutput("cardContainer")

	
)

# A function that runs code based on UI selections
serverF <- function(input, output) {
	# Decide which egg groups to display
	# As a first step, just display the Pokemon's egg groups
	
	displayList <- reactive({
		getNumbers(input$Dropdown, eggs) # example list of Pokemon numbers
	})
	
	# Generate a "tag list" referencing the objects, used by uiOutput()
	output$cardContainer <- renderUI({
		card_list <- lapply(displayList(), getCard, eggs = eggs)
		card_list$cellArgs <- list(
			style = "
			width: 140px;
			height: auto;
			margin: 10px;
			"
		)
		do.call(flowLayout, card_list)
	})
}

# Activate the server with the defined UI
shinyApp(ui = uiF, server = serverF)