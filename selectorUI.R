library(shiny)

cardUI <- function(id, labelText = "Selected Pokemon:"){
	tagList(
		selectizeInput(NS(id, "Dropdown"), choices = NULL, label = labelText),
		uiOutput(NS(id, "selectionCard")),
	)
}

cardServer <- function(id, genRadio){
	moduleServer(id, function(input, output, session){
		# Update dropdown options based on selected generation
		observeEvent({genRadio()},{
			updateSelectizeInput(
				session = getDefaultReactiveDomain(),
				inputId = 'Dropdown', 
				choices = getEggs()$Name, 
				server = TRUE,
				options = list(maxItems = 1)
			)
		})
		# Get the number of the selected Pokemon
		selectionNumber <- reactive({
			req(input$Dropdown)
			name2num(input$Dropdown)
		})
		
		# Create a card of the selected Pokemon
		output$selectionCard <- renderUI({
			req(genRadio)
			tmp <- genRadio # dummy call so it checks this var
			selCard <- getCard(selectionNumber())
		})
		
		# Define outputs of cardServer here
		reactive(input$Dropdown) # the name of the selected Pokemon
	}) # moduleServer()

}