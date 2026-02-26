library(shiny)
library(bslib)
source("helper.R")
source("graphs.R")

initGens()
vgList <- getVGNested()

setActiveVersion("gold-silver")

# eggs <- getEggs()
# genList <- getGens() %>% pull(name) 

# A function that organizes the UI elements of the shiny app	
uiF <- page_sidebar(
	# Persistent collapsible sidebar to select game generation
	sidebar = sidebar(
		selectInput(
			inputId = "genRadio",
			label = "Select a game generation",
			choices = vgList[-1]
		),
		helpText("You can collapse this sidebar after making a selection")
	),
	title = "Pokemon Breeding Assistant",
	# Nav pane on bottom to switch between modes e.g. list, graph
	navset_card_pill(
		title = "Select mode:",
		id = "modeSelector",
		# Content page 1: List mode
		nav_panel(
			title = "List",
			page_sidebar(
				sidebar = sidebar(
					open = "always",
					selectizeInput("Dropdown", choices = "Bulbasaur", label = "Selected Pokemon:"),
					uiOutput("selectionCard"),
					helpText("List all possible mates for the selected Pokemon.
									 This displays general compatibility, without considering
									 whether the selected Pokemon acts as the father or mother.")
				),
				textOutput("Tally"),
				uiOutput("cardContainer")
			)
		), # nav_panel 1
		# Content page 2: graph mode
		nav_panel(
			title = "Graph",
			page_sidebar(
				# title = "Pokemon Breeding Assistant",
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
)
# A function that runs code based on UI selections
serverF <- function(input, output) {
	# Perform the following actions ONLY AFTER the game version changes
	observeEvent({input$genRadio},{
		# Update the backend data
		setActiveVersion(input$genRadio)
	})
	
	observeEvent({input$genRadio},{
		updateSelectizeInput(
			session = getDefaultReactiveDomain(),
			inputId = 'Dropdown', 
			choices = getEggs()$Name, 
			server = TRUE,
			options = list(maxItems = 1)
		)
	})
	
	# Decide which egg groups to display based on input name
	displayList <- reactive({
		req(input$genRadio, input$Dropdown)
		tmp <- input$genRadio # dummy call so it checks this var
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
		req(input$Dropdown)
		c(input$Dropdown, "has", getCount(), "potential mates")
	})
	
	output$Tally <-  renderText({
		getName()
	})
	
	# Get the number of the selected Pokemon
	selectionNumber <- reactive({
		req(input$Dropdown)
		 name2num(input$Dropdown)
	})
	
	# Create a card of the selected Pokemon
	output$selectionCard <- renderUI({
		req(input$genRadio)
		tmp <- input$genRadio # dummy call so it checks this var
		selCard <- getCard(selectionNumber())
	})
	
	# Generate a "tag list" referencing the objects, used by uiOutput()
	output$cardContainer <- renderUI({
		tmp <- input$genRadio # dummy call so it checks this var
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
	# Leave this as reactive so it only updates when viewed
	output$fullGraph <- renderVisNetwork({
		# tmp <- input$genRadio
		getGraph() %>% renderGraph()
	}) %>% 
		bindCache(input$genRadio)
}

# Activate the server with the defined UI
shinyApp(ui = uiF, server = serverF)