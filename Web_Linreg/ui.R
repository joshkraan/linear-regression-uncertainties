library(shiny)
library(colourpicker)
library(shinydashboard)
library(shinyBS)
library(latex2exp)

sidebar = dashboardSidebar(
  sidebarMenu(id = "menu",
    menuItem("Graph", tabName = "graph", icon = icon("line-chart")),
    menuItem("Table", tabName = "table", icon = icon("table"))
  ),
  div(style = "height: 75px", fileInput("csvFile", "Upload a CSV file to graph",
            accept = c(
              'text/csv',
              'text/comma-separated-values',
              '.csv'
            )
  )),
  checkboxInput('header', 'Header', FALSE),
  bsTooltip(id = 'header', title = 'Select this if the CSV file provided has a header row.', placement = 'right'),
  numericInput("setNumber", "Number of Generated Sets", min = 100, max = 100000, value = 100),
  bsTooltip(id = 'setNumber', title = 
              "With more sets, the accuracy will increase, but the calculations will take longer. The minimum is 100 and the maximum is 100,000.", 
            placement = 'top'),
  div(style = "text-align: center", actionButton("calculateFit", "Calculate Fit", width = '85%'))
)

body = dashboardBody(
  tabItems(
    tabItem(tabName = "graph", {
      fluidRow(
        box(width = "9", plotOutput("scatterPlot", height = "auto")),
        box(width = "3",
          #h5(tags$b("Aspect Ratio")),
          # splitLayout(cellWidths = c("65px", "10px", "80px"), 
          #   div(style = "height: 30px", numericInput("setAspectRatio1", "", value = 1, width = '60px', min = 1, max = 99)),
          #   h3(":"),
          #   numericInput("setAspectRatio2", "", value = 1, width = '60px', min = 1, max = 99)
          # ),
          textInput("xLabel", "X Axis Label", value = "X"),
          bsPopover(id = "xLabel", title = "Using LaTeX", 
                    content = paste0("The X and Y axis labels are processed using the R package latex2exp. ", 
                                     "This means that many regular LaTeX math formulas can be used. Rather than one ", 
                                     "backslash two should be used for commands. See the latex2exp documentation for more information."),
                    placement = "left"),
          textInput("yLabel", "Y Axis Label", value = "Y"),
          bsPopover(id = "yLabel", title = "Using LaTeX", 
                   content = paste0("The X and Y axis labels are processed using the R package latex2exp. ", 
                                    "This means that many regular LaTeX math formulas can be used. Rather than one ", 
                                    "backslash two should be used for commands. See the latex2exp documentation for more information."),
                   placement = "left"),
          splitLayout(
            numericInput('xMin', 'X Min', 50),
            numericInput('xMax', 'X Max', 200)
          ),
          splitLayout(
            numericInput('yMin', 'Y Min', 50),
            numericInput('yMax', 'Y Max', 200)
          ),
          tags$hr(),
          selectInput("aspectRatio", "Aspect Ratio", 
                      c("16:9" = 9/16, "4:3" = 3/4, "1:1" = 1)),
          selectInput("graphResolution", "Resolution", 
                      c("1080p", "720p", "480p")),
          numericInput("setPPI", "PPI", value = 100, min = 50, max = 250),
          tags$hr(),
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
          ),
          tags$hr(),
          selectInput("fileFormat", "Download File Format", 
                      c("PDF", "SVG", "PNG")),
          actionButton("downloadPlot", "Download", width = '100%')
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
