library(shiny)
library(colourpicker)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(latex2exp)
library(DT)

sidebar = dashboardSidebar(
  sidebarMenu(id = "menu",
              menuItem("Data", tabName = "table", icon = icon("table")),
              menuItem("Graph", tabName = "graph", icon = icon("line-chart")),
              # menuItemOutput("graphOutput"),
              hr(),
              menuItem("Source", icon = icon("github"), href = "https://github.com/joshkraan/linear-regression-uncertainties")
  )
)

body = dashboardBody(
  tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),
  shinyjs::useShinyjs(),
  tabItems(
    tabItem(tabName = "table", {
      fluidRow(
        box(width = "9",
            title = "Data Table",
            solidHeader = TRUE,
            status = "primary",
            conditionalPanel(
              condition = "output.dataFile == false",
              "Upload a CSV data file using the ", tags$b("Data File"), "box.", tags$br(),
              "If you have not used this application before",
              tags$a("click here", href = "https://github.com/joshkraan/linear-regression-uncertainties/wiki#basic-usage"),
              "for a tutorial."
            ),
            conditionalPanel(
              condition = "output.dataFile == true",
              DT::dataTableOutput("dataTable")
            )),
        box(width = "3",
            title = "Data File",
            solidHeader = TRUE,
            status = "primary",
            helpText( a(icon("question"), "Click here for help with file formatting.", href = "https://github.com/joshkraan/linear-regression-uncertainties/wiki#file-formatting", target = "_blank")),
            fileInput("csvFile", "Upload a CSV data file", accept = c('text/csv', 'text/comma-separated-values', '.csv')),
            checkboxInput('header', 'Header Row', FALSE),
            div(style = "text-align: center", actionButton("graphData", "Graph Data", width = '100%')),
            bsAlert("alert")
            )
      )
    }),
    tabItem(tabName = "graph", {
      fluidRow(
        box(width = "9", plotOutput("scatterPlot", width = "auto", height = "auto", click = "plot_click")),
        box(width = "3",
            textInput("graphTitle", "Title*", value = NULL, width = "100%"),
            textInput("xLabel", "X Axis Label*", value = "X", width = "100%"),
            textInput("xUnits", "X Axis Units*", value = NULL, width = "100%"),
            textInput("yLabel", "Y Axis Label*", value = "Y", width = "100%"),
            textInput("yUnits", "Y Axis Units*", value = NULL, width = "100%"),
            helpText( a("*Click here for help with symbol usage.", href = "https://github.com/joshkraan/linear-regression-uncertainties/wiki#symbol-usage", target = "_blank")),
            hr(),
            conditionalPanel(
              condition = "input.advancedSettings%2 == 1",
              splitLayout(
                numericInput('xMin', 'X Min', ''),
                numericInput('yMin', 'Y Min', '')
              ),
              splitLayout(
                numericInput('xMax', 'X Max', ''),
                numericInput('yMax', 'Y Max', '')
              ),
              actionButton("setAxisToZero", "Include Origin", width = "100%"),
              hr(),
              numericInput("setNumber", "Number of Generated Sets", min = 100, max = 1000, value = 100, width = "100%"),
              bsTooltip(id = 'setNumber', title =
                          "With more sets, the accuracy will increase, but the calculations will take longer. The minimum is 100 and the maximum is 1,000.",
                        placement = 'top')
            ),
            actionButton("calculateFit", "Calculate Fit", width = '100%'),
            verbatimTextOutput("fitResult"),
            conditionalPanel(
              condition = "input.advancedSettings%2 == 1",
              hr(),
              hr(),
              selectInput("aspectRatio", "Aspect Ratio", 
                          c("16:9" = 9/16, "4:3" = 3/4, "1:1" = 1), width = "100%"),
              sliderInput("setPPI", "Element Scale", min = 50, max = 200, value = 100, post = "%", width = "100%"),
              selectInput("selectTheme", "Theme", 
                          c("Black & White" = "theme_bw()", "Grey" = "theme_grey()", "Base" = "theme_base()", "Google Docs" = "theme_gdocs()", "LibreOffice" = "theme_calc()"), width = "100%"),
              tags$hr(),
              checkboxInput("showMaxMin", "Show Max/Min Lines", value = TRUE),
              checkboxInput("showSpread", "Show Spread", value = FALSE),
              conditionalPanel(
                condition = "input.showSpread == true",
                colourpicker::colourInput("spreadColor", "Spread Color", value = "grey")
              ),
              checkboxInput("showGenerated", "Show Generated Data", value = FALSE),
              conditionalPanel(
                condition = "input.showGenerated == true",
                colourpicker::colourInput("dataColor", "Generated Data Color", value = "red")
              ),
              checkboxInput("showEquationFloat", "Show Equation On Graph", value = TRUE),
              conditionalPanel(
                condition = "input.showEquationFloat == true",
                helpText("Position the equation by clicking on the graph."),
                sliderInput("setLabelScale", "Label Scale", min = 50, max = 150, value = 100, post = "%", width = "100%")
              ),
              hr(),
              hr(),
              selectInput("fileFormat", "Download File Format", 
                          c("PDF" = "pdf", "SVG" = "svg", "PNG" = "png"),
                          selected = "png"),
              conditionalPanel(
                condition = "input.fileFormat == 'png'",
                selectInput("downloadResolution", "Download Resolution", 
                            c("1080p" = 1920, "720p" = 1280, "480p" = 640),
                            selected = 1280)
              )
            ),
            downloadButton("downloadPlot", "Download"),
            tags$style(type='text/css', '#downloadPlot { width:100%; align: center'),
            actionLink("advancedSettings", "Advanced Settings")
        )
      )
    })
  )
)

dashboardPage(
  dashboardHeader(title = "Linear Regression"),
  sidebar,
  body
)
