library(shiny)
library(bslib)
source("helper.R")
source("graphs.R")

initGens()
setActiveVersion("gold-silver")
setActiveGen("generation-ii")

eggs <- getEggs()

# A function that organizes the UI elements of the shiny app	
uiF <- page_navbar(
	title = "Select mode:",
	id = "mainNavbar",
	position = "fixed-bottom",
	nav_panel(
		title = "List",
		page_sidebar(
			title = "Pokemon Breeding Assistant",
			sidebar = sidebar(
				selectInput(
					"Dropdown",
					label = "Selected Pokemon:",
					choices = eggs$Name
				),
				uiOutput("selectionCard"),
				helpText("List all possible mates for the selected Pokemon.
								 This displays general compatibility, without considering
								 whether the selected Pokemon acts as the father or mother.")
			),
			textOutput("Tally"),
			uiOutput("cardContainer")
		)
	), # nav_panel 1
	
	nav_panel(
		title = "Graph",
		page_sidebar(
			title = "Pokemon Breeding Assistant",
			sidebar = sidebar(
				helpText("You can interact with this graph! Scroll to zoom.
								 Hovering over a Pokemon highlights its compatible mates.
								 Clicking locks this selection.
								 You can click and drag nodes to help rearrange as needed.",
								 p(),
								 "Blue lines represent breeding compatibility,
								 yellow lines represent evolutionary chains."),
			),
			visNetworkOutput("fullGraph")
		)
		
	), # nav_panel 2
)

# A function that runs code based on UI selections
serverF <- function(input, output) {
	# Decide which egg groups to display based on input name
	displayList <- reactive({
		getNumbers(input$Dropdown) # outputs a list of Pokemon numbers
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
		 name2num(input$Dropdown)
	})
	
	output$selectionCard <- renderUI({
		selCard <- getCard(selectionNumber())
	}) %>% bindCache(selectionNumber(), getActiveGen())
	
	# Generate a "tag list" referencing the objects, used by uiOutput()
	output$cardContainer <- renderUI({
		card_list <- lapply(displayList(), getCard)
		card_list$cellArgs <- list(
			style = "
			width: 140px;
			height: auto;
			margin: 10px;
			"
		)
		do.call(flowLayout, card_list)
	})
	
	# Render the visNetwork graph
	output$fullGraph <- renderVisNetwork({
		getGraph() %>% renderGraph()
	})
}

# Activate the server with the defined UI
shinyApp(ui = uiF, server = serverF)