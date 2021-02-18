library(shiny)

shinyUI(
  navbarPage(
    title = "NEPSscaling ",
    inverse = TRUE,

    # print output to shiny: https://stackoverflow.com/questions/30474538/possible-to-show-console-messages-written-with-message-in-a-shiny-ui/30490698#3049069844446666666
    shinyjs::useShinyjs(),

    tabPanel(
      icon("laptop"),
      sidebarLayout(
        sidebarPanel(
          "Load Data", fileInput("bgdata_file", h3("File Input")),
          "Load State",
          fileInput("state_file", h3("File Input"))
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Manage", plotOutput("plot")),
            tabPanel("Estimate Plausible Values", verbatimTextOutput("summary")),
            tabPanel("Visualize Estimates", tableOutput("table")),
            tabPanel("Explore Estimates", tableOutput("table")),
            tabPanel("Summary Statistics", tableOutput("table"))
          )
        )
      )
    ),

    navbarMenu(
      icon("power-off"),
      tabPanel(
        "Stop",
        DT::dataTableOutput("table")
      ),
      tabPanel(
        "Save State",
        DT::dataTableOutput("table")
      ),
      tabPanel(
        "Leave",
        DT::dataTableOutput("table")
      )
    ),
    navbarMenu(
      icon("question-circle"),
      tabPanel(
        "Help",
        DT::dataTableOutput("table")
      ),
      tabPanel(
        "About",
        DT::dataTableOutput("table")
      ),
      tabPanel(
        "Useful Links",
        DT::dataTableOutput("table")
      ),
      tabPanel(
        "Background Information Plausible Values",
        DT::dataTableOutput("table")
      ),
      tabPanel(
        "Background Information CART",
        DT::dataTableOutput("table")
      ),
      tabPanel(
        "Report Issue",
        DT::dataTableOutput("table")
      )
    )
  )
)
