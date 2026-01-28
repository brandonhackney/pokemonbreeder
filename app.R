library(shiny)
library(bslib)
source("myFunctions.R")

eggs <- getEggGroups()

myList <- eggs$Number[eggs$Dragon]
# A function that organizes the UI elements of the shiny app	
uiF <- page_sidebar(
	title = "Pokemon Breeding Assistant",
	sidebar = sidebar(
		helpText("List all possible mates for the selected Pokemon"),
		selectInput(
			"Dropdown",
			label = "Select a Pokemon",
			choices = eggs$Name
		),
		uiOutput("selectionCard")
	),

		uiOutput("cardContainer")

	
)

# A function that runs code based on UI selections
serverF <- function(input, output) {
	# Decide which egg groups to display based on input name
	displayList <- reactive({
		getNumbers(input$Dropdown, eggs) # outputs a list of Pokemon numbers
	})
	
	# Get the number of the selected Pokemon
	selectionNumber <- reactive({
		 eggs$Number[eggs$Name == input$Dropdown]
	})
	
	output$selectionCard <- renderUI({
		selCard <- getCard(selectionNumber(), eggs)
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