library(shiny)
library(colourpicker)
library(shinydashboard)
library(shinyBS)
library(latex2exp)
library(DT)

sidebar = dashboardSidebar(
  sidebarMenu(id = "menu",
              menuItem("Data", tabName = "table", icon = icon("table")),
              menuItem("Graph", tabName = "graph", icon = icon("line-chart"))
  ),
  # div(style = "height: 75px", fileInput("csvFile", "Upload a CSV file to graph",
  #           accept = c(
  #             'text/csv',
  #             'text/comma-separated-values',
  #             '.csv'
  #           )
  # )),
  # checkboxInput('header', 'Header', FALSE),
  
  bsTooltip(id = 'header', title = 'Select this if the CSV file provided has a header row.', placement = 'right'),
  numericInput("setNumber", "Number of Generated Sets", min = 100, max = 100000, value = 100),
  bsTooltip(id = 'setNumber', title = 
              "With more sets, the accuracy will increase, but the calculations will take longer. The minimum is 100 and the maximum is 100,000.", 
            placement = 'top'),
  div(style = "text-align: center", actionButton("calculateFit", "Calculate Fit", width = '85%'))
)

body = dashboardBody(
  shinyjs::useShinyjs(),
  tabItems(
    tabItem(tabName = "table", {
      fluidRow(
        box(width = "9", DT::dataTableOutput("dataTable")),
        box(width = "3",
            fileInput("csvFile", "Upload a CSV data file", accept = c('text/csv', 'text/comma-separated-values', '.csv')),
            checkboxInput('header', 'Header Row', FALSE),
            div(style = "text-align: center", actionButton("graphData", "Graph Data", width = '100%')),
            bsAlert("alert")
            )
      )
    }),
    tabItem(tabName = "graph", {
      fluidRow(
        box(width = "9", plotOutput("scatterPlot", height = "auto", click = "plot_click")),
        box(width = "3",
          textInput("graphTitle", "Title", value = NULL),
          bsPopover(id = "graphTitle", title = "Using LaTeX", 
                    content = paste0("The graph labels are processed using the R package latex2exp. ", 
                                     "This means that many regular LaTeX math formulas can be used. Rather than one ", 
                                     "backslash two should be used for commands. See the latex2exp documentation for more information."),
                    placement = "left"),
          textInput("xLabel", "X Axis Label", value = "X"),
          bsPopover(id = "xLabel", title = "Using LaTeX", 
                    content = paste0("The graph labels are processed using the R package latex2exp. ", 
                                     "This means that many regular LaTeX math formulas can be used. Rather than one ", 
                                     "backslash two should be used for commands. See the latex2exp documentation for more information."),
                    placement = "left"),
          textInput("yLabel", "Y Axis Label", value = "Y"),
          bsPopover(id = "yLabel", title = "Using LaTeX", 
                   content = paste0("The graph axis labels are processed using the R package latex2exp. ", 
                                    "This means that many regular LaTeX math formulas can be used. Rather than one ", 
                                    "backslash two should be used for commands. See the latex2exp documentation for more information."),
                   placement = "left"),
          splitLayout(
            numericInput('xMin', 'X Min', ''), #TODO: Fix issues with steps and invalid value error message on mouseover
            numericInput('xMax', 'X Max', '')
          ),
          splitLayout(
            numericInput('yMin', 'Y Min', ''),
            numericInput('yMax', 'Y Max', '')
          ),
          tags$hr(),
          selectInput("aspectRatio", "Aspect Ratio", 
                      c("16:9" = 9/16, "4:3" = 3/4, "1:1" = 1)),
          numericInput("setPPI", "PPI", value = 100, min = 50, max = 250),
          selectInput("selectTheme", "Theme", 
                      c("Black & White" = "theme_bw()", "Grey" = "theme_grey()", "Base" = "theme_base()", "Google Docs" = "theme_gdocs()", "LibreOffice" = "theme_calc()")),
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
          selectInput("downloadResolution", "Download Resolution", 
                      c("1080p", "720p", "480p")),
          selectInput("fileFormat", "Download File Format", 
                      c("PDF" = "pdf", "SVG" = "svg", "PNG" = "png")),
          downloadButton("downloadPlot", "Download"),
          tags$style(type='text/css', '#downloadPlot { width:100%; align: center')
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
