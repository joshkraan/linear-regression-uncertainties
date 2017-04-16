

library(shiny)
library(colourpicker)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Linear Regression With Uncertainties"),
  
  fluidRow(
    # Sidebar with file input options.
    column(3,
        wellPanel(
          #This is the file options panel.
          h4("File Options", align = "center"),
          tags$hr(),
          fileInput("csvFile", "Select a CSV file to graph", 
                    accept = c(
                        'text/csv',
                        'text/comma-separated-values',
                        '.csv'
                    )
          ),
          # Add a horizontal line
          tags$hr(),
          checkboxInput('header', 'Header', FALSE)
        ),
        
        wellPanel(
          #This is the graph options panel. 
          h4("Graph Options", align = "center"),
          tags$hr(),
          #TODO: Figure out a max for Number Input
          numericInput("setNumber", "Generated Sets", min = 100, value = 100),
          tags$hr(),
          checkboxInput("showSpread", "Show Spread", value = FALSE),
          conditionalPanel(
            condition = "input.showSpread == true",
            colourInput("spreadColor", "Spread Color", value = "grey")
          ),
          checkboxInput("showGenerated", "Show Generated Data", value = FALSE),
          conditionalPanel(
            condition = "input.showGenerated == true",
            colourInput("dataColor", "Generated Data Color", value = "red")
          ),
          tags$hr(),
          actionButton("plotData", "Calculate Fit", width = '100%')
        )
    ),
    
    # Show a plot of the generated distribution
    column(9,
      tabsetPanel(tabPanel("Plot", plotOutput("scatterPlot")),
                  tabPanel("Table", tableOutput("dataTable")))
    )
  )
))
