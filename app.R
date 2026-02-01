library(shiny)
library(bslib)
source("helper.R")

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
		textOutput("Tally"),
		uiOutput("cardContainer")

	
)

# A function that runs code based on UI selections
serverF <- function(input, output) {
	# Decide which egg groups to display based on input name
	displayList <- reactive({
		getNumbers(input$Dropdown, eggs) # outputs a list of Pokemon numbers
	})
	
	# Tally number of mates, but if result contains 0, that means none, not 1
	getCount <- reactive({
		y <- displayList()
		if (length(y) == 1 && y[1] == 0){
			0
		}
		else {
			length(y)
		}
	})
	# Count the number of results and display
	getName <- reactive({
		c(input$Dropdown, "has", getCount(), "potential mates")
	})
	output$Tally <-  renderText({
		getName()
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