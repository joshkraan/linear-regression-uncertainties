library(shiny)
library(colourpicker)
library(shinydashboard)

sidebar = dashboardSidebar(
  sidebarMenu(id = "menu",
    menuItem("Graph", tabName = "graph", icon = icon("line-chart")),
    menuItem("Table", tabName = "table", icon = icon("table"))
  ),
  div(style = "height: 75px", fileInput("csvFile", "Select a CSV file to graph",
            accept = c(
              'text/csv',
              'text/comma-separated-values',
              '.csv'
            )
  )),
  checkboxInput('header', 'Header', FALSE),
  numericInput("setNumber", "Number of Generated Sets", min = 100, max = 100000, value = 100),
  div(style = "text-align: center", actionButton("calculateFit", "Calculate Fit", width = '85%'))
)

body = dashboardBody(
  tabItems(
    tabItem(tabName = "graph", {
      fluidRow(
        box(width = "9", plotOutput("scatterPlot")),
        box(width = "3",
          checkboxInput("showMaxMin", "Show Max/Min Lines", value = FALSE),
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
          checkboxInput("showEquationFloat", "Show Equation On Graph", value = FALSE),
          conditionalPanel(
            condition = "input.showEquationFloat == true",
            sliderInput("equationVertical", "Vertical Distance", min = 0, max = 100, value = 0, post = "%")
          ),
          conditionalPanel(
            condition = "input.showEquationFloat == true",
            sliderInput("equationHorizontal", "Horizontal Distance", min = 0, max = 100, value = 0, post = "%")
          )
        )
      )
    }),
    tabItem(tabName = "table", tableOutput("dataTable"))
  )
)

dashboardPage(
  dashboardHeader(title = "Linear Regression"),
  sidebar,
  body
)

# shinyUI(fluidPage(
#   
#   # Application title
#   titlePanel("Linear Regression With Uncertainties"),
#   
#   fluidRow(
#     # Sidebar with file input options.
#     column(3,
#         wellPanel(
#           #This is the file options panel.
#           h4("File Options", align = "center"),
#           tags$hr(),
          # fileInput("csvFile", "Select a CSV file to graph",
          #           accept = c(
          #               'text/csv',
          #               'text/comma-separated-values',
          #               '.csv'
          #           )
          # ),
          # # Add a horizontal line
          # tags$hr(),
          # checkboxInput('header', 'Header', FALSE)
#         ),
#         
#         wellPanel(
#           #This is the regression options panel
#           h4("Regression Options", align = "center"),
          # tags$hr(),
          # #TODO: Figure out a max for Number Input
          # numericInput("setNumber", "Generated Sets", min = 100, value = 100),
          # tags$hr(),
          # actionButton("calculateFit", "Calculate Fit", width = '100%')
#         ),
#         
#         wellPanel(
#           #This is the graph options panel. 
#           h4("Graph Options", align = "center"),
#           tags$hr(),
          # checkboxInput("showMaxMin", "Show Max/Min Lines", value = FALSE),
          # checkboxInput("showSpread", "Show Spread", value = FALSE),
          # conditionalPanel(
          #   condition = "input.showSpread == true",
          #   colourInput("spreadColor", "Spread Color", value = "grey")
          # ),
          # checkboxInput("showGenerated", "Show Generated Data", value = FALSE),
          # conditionalPanel(
          #   condition = "input.showGenerated == true",
          #   colourInput("dataColor", "Generated Data Color", value = "red")
          # ),
          # checkboxInput("showEquationFloat", "Show Equation On Graph", value = FALSE),
          # conditionalPanel(
          #   condition = "input.showEquationFloat == true",
          #   sliderInput("equationVertical", "Vertical Distance", min = 0, max = 100, value = 0, post = "%")
          # ),
          # conditionalPanel(
          #   condition = "input.showEquationFloat == true",
          #   sliderInput("equationHorizontal", "Horizontal Distance", min = 0, max = 100, value = 0, post = "%")
          # )
#         )
#     ),
#     
#     # Show a plot of the generated distribution
#     column(9,
#       tabsetPanel(tabPanel("Plot", 
#                            {
#                              if(is.null(plotOutput("scatterPlot"))){
#                                h4("test")
#                              } else {
#                                plotOutput("scatterPlot")
#                                #h4("test", align = "center")
#                              }
#                            }),
#                   tabPanel("Table", tableOutput("dataTable")))
#     )
#   )
# ))
