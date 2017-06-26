library(shiny)
library(colourpicker)
library(shinydashboard)
library(shinyBS)
library(latex2exp)
library(DT)

sidebar = dashboardSidebar(
  sidebarMenu(id = "menu",
              menuItem("Data", tabName = "table", icon = icon("table")),
              menuItem("Graph", tabName = "graph", icon = icon("line-chart")),
              hr(),
              menuItem("Github", icon = icon("github"), href = "https://github.com/joshkraan/linear-regression-uncertainties")
  )
)

body = dashboardBody(
  shinyjs::useShinyjs(),
  tabItems(
    tabItem(tabName = "table", {
      fluidRow(
        box(width = "9", DT::dataTableOutput("dataTable")),
        box(width = "3",
            helpText( a(icon("question"), "File formatting", href = "https://github.com/joshkraan/linear-regression-uncertainties/wiki/Web-Version-Usage", target = "_blank")),
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
        tabBox(width = "3",
          tabPanel(icon("cogs"),
                   textInput("graphTitle", "Title", value = NULL, width = "100%"),
                   bsPopover(id = "graphTitle", title = "Using LaTeX", 
                             content = paste0("The graph labels are processed using the R package latex2exp. ", 
                                              "This means that many regular LaTeX math formulas can be used. Rather than one ", 
                                              "backslash two should be used for commands. See the latex2exp documentation for more information."),
                             placement = "left"),
                   textInput("xLabel", "X Axis Label", value = "X", width = "100%"),
                   bsPopover(id = "xLabel", title = "Using LaTeX", 
                             content = paste0("The graph labels are processed using the R package latex2exp. ", 
                                              "This means that many regular LaTeX math formulas can be used. Rather than one ", 
                                              "backslash two should be used for commands. See the latex2exp documentation for more information."),
                             placement = "left"),
                   textInput("yLabel", "Y Axis Label", value = "Y", width = "100%"),
                   bsPopover(id = "yLabel", title = "Using LaTeX", 
                             content = paste0("The graph axis labels are processed using the R package latex2exp. ", 
                                              "This means that many regular LaTeX math formulas can be used. Rather than one ", 
                                              "backslash two should be used for commands. See the latex2exp documentation for more information."),
                             placement = "left"),
                   splitLayout(
                     numericInput('xMin', 'X Min', ''), #TODO: Fix issues with steps and invalid value error message on mouseover
                     numericInput('yMin', 'Y Min', '')
                   ),
                   
                   splitLayout(
                     numericInput('xMax', 'X Max', ''),
                     numericInput('yMax', 'Y Max', '')
                   ),
                   actionButton("setAxisToZero", "Include Origin", width = "100%"),
                   hr(),
                   numericInput("setNumber", "Number of Generated Sets", min = 100, max = 100000, value = 100, width = "100%"),
                   bsTooltip(id = 'setNumber', title =
                               "With more sets, the accuracy will increase, but the calculations will take longer. The minimum is 100 and the maximum is 100,000.",
                             placement = 'top'),
                   actionButton("calculateFit", "Calculate Fit", width = '100%')),
          tabPanel(icon("paint-brush"),
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
                     colourpicker::colourInput("spreadColor", "Spread Color", value = "grey")
                   ),
                   checkboxInput("showGenerated", "Show Generated Data", value = FALSE),
                   conditionalPanel(
                     condition = "input.showGenerated == true",
                     colourpicker::colourInput("dataColor", "Generated Data Color", value = "red")
                   ),
                   checkboxInput("showEquationFloat", "Show Equation On Graph", value = FALSE),
                   conditionalPanel(
                     condition = "input.showEquationFloat == true",
                     helpText("Position the equation by clicking on the graph.")
                   )),
          tabPanel(icon("download"),
                   selectInput("downloadResolution", "Download Resolution", 
                               c("1080p", "720p", "480p")),
                   selectInput("fileFormat", "Download File Format", 
                               c("PDF" = "pdf", "SVG" = "svg", "PNG" = "png")),
                   downloadButton("downloadPlot", "Download"),
                   tags$style(type='text/css', '#downloadPlot { width:100%; align: center'))
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
