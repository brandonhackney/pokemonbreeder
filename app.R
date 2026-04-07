# Load/install all required packages
source("setup.R")
setup("shiny", "bslib", "tidyverse", "igraph", "visNetwork", "httr2", "jsonlite", "this.path")
# Load local functions
source("helper.R")
source("graphs.R")
source("selectorUI.R")

vgList <- getVGNested()
dflt <- "gold-silver"
setActiveVersion(dflt)

# A function that organizes the UI elements of the shiny app	
uiF <- page_sidebar(

	# Persistent collapsible sidebar to select game generation
	sidebar = sidebar(
		selectInput(
			inputId = "genSelect",
			label = "Select a game version",
			choices = vgList[-1]
		),
		input_task_button("genActivator", "Activate"),
		helpText("You must push the button for changes to take effect.
						 You can collapse this sidebar afterwards.")
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
					cardUI("listName"),
					radioButtons(inputId = "genderSwitch",
											 label = "Selected gender:",
											 choices = c("♂" = "out", "♀" = "in", "Either" = "all"),
											 selected = "all",
											 inline = TRUE
											 ),
					helpText("List all possible mates for the selected Pokemon,
									 given the selected gender. For example, Bulbasaur♀
									 can breed with Nidoran♂, but not Nidoran♀.",
									 p(),
									 p(),
									 "Ditto is a special case and is always treated as a mother here.")
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
									 p(),
									 "Blue lines represent breeding compatibility,
									 yellow lines represent evolutionary chains."),
					uiOutput("graphName")
				),
				visNetworkOutput("fullGraph")
			)
			
		), # nav_panel 2
		# Content page 3: move selector
		nav_panel(
			title = "Moves",
			page_sidebar(
				# Part 1: Selection area
				sidebar = sidebar(
					open = "always",
					width = 350,
					 helpText("Check whether the source Pokemon's selected move
					 				 can be chain-bred to the target Pokemon.
					 				 Displays all possible chains with the fewest steps."),
					layout_columns(
						cardUI("Source", "Source Pokemon:"),
						cardUI("Target", "Target Pokemon:"),
					),
					selectizeInput("movePicker", choices = "-", label = "Select a move"),
					input_task_button("buttonMoves", "Check if possible"),
				 ),
				
				# Part 2: Output area
					"Results:",
					visNetworkOutput("chainResults")
			)
		)
	)
)
# A function that runs code based on UI selections
serverF <- function(input, output) {
	# Perform the following actions ONLY AFTER the game version changes
	observeEvent({input$genActivator},{
		# Update the backend data
		setActiveVersion(input$genSelect)
		genToServer(input$genSelect)
	})
	
	genToServer <- reactiveVal(dflt)
	
	# These outputs are reactives, so get the value using e.g. listPok()
	listPok <- cardServer("listName", genToServer)
	sourcePok <- cardServer("Source", genToServer)
	targetPok <- cardServer("Target", genToServer)
	
	
	# Decide which egg groups to display based on input name
	displayList <- reactive({
		req(genToServer(), listPok())
		tmp <- genToServer() # dummy call so it checks this var
		# getNumbers(input$Dropdown) # outputs a list of Pokemon numbers
		getMates(listPok(), input$genderSwitch)
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
		req(listPok())
		c(listPok(), "has", getCount(), "potential mates")
	})
	
	output$Tally <-  renderText({
		getName()
	})
	
	# Generate a "tag list" referencing the objects, used by uiOutput()
	output$cardContainer <- renderUI({
		tmp <- genToServer() # dummy call so it checks this var
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
		# tmp <- genToServer()
		getGraph() %>% renderGraph() %>%
			visLegend()
	}) %>% 
		bindCache(genToServer())
	
	# Clicking a node in the graph updates the selector
	chosenNode <- reactive({input$fullGraph_selected})
	output$graphName <- renderUI({
		req(chosenNode())
		chosenNode() %>% getCard()
	})
	
	# Get the list of moves choices for the source Pokemon
	# Update dropdown options based on selected generation
	observe({
		req(genToServer(), sourcePok())
		updateSelectizeInput(
			session = getDefaultReactiveDomain(),
			inputId = 'movePicker', 
			choices = listMoves(sourcePok()), 
			server = TRUE,
			options = list(maxItems = 1)
		)
	}) %>% 
		bindEvent(genToServer(), sourcePok())
	
	# When user pushes the button, calculate possible chains from Source to Target
	doMoveCheck <- reactive({input$buttonMoves})
	
	# Display the list of chains from Source to Target
	output$chainResults <- renderVisNetwork({
		result <- getPath(sourcePok(), targetPok(), input$movePicker)
		if (is.null(result)){
			# No data - Display message
			visNetwork(nodes = data.frame(id=1, label="No Results", 
																		shape="text", font.size=30)) %>%
				visEdges(hidden = TRUE)
		} else {
			result %>% 
				renderChains(sourcePok())
		}
		}) %>% bindEvent(doMoveCheck(), ignoreInit = TRUE)
	
}

# Activate the server with the defined UI
shinyApp(ui = uiF, server = serverF)