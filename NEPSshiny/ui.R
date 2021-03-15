library(shinythemes)
library(shiny)
library(shinydashboard)
library(bslib)



shinyUI(
  navbarPage(
    theme = bs_theme(
      bg = "#E8E6F4", fg = "black", primary = "#24195D", secondary = "#1D0E46",
      base_font = font_google("Open Sans"),
      code_font = font_google("Open Sans")
    ),
    title = "NEPSscaling",
    titlePanel(div(
      column(width = 1),
      column(width = 1, tags$img(src = "NEPS_Logo_web_de.jpg", height = 40, width = 100))
    )),
    titlePanel(div(
      column(width = 1),
      column(width = 1, tags$img(src = "LIfBi_Logo_solo _RZ_RGB.jpg", height = 50, width = 55))
    )),
    inverse = FALSE,
    tags$head(
      tags$style(
        HTML(".shiny-notification {
             position:fixed;
             top: calc(50%);
             left: calc(50%);
             }
             "
        )
      )
    ),

    # print output to shiny: https://stackoverflow.com/questions/30474538/possible-to-show-console-messages-written-with-message-in-a-shiny-ui/30490698#3049069844446666666
    shinyjs::useShinyjs(),
    ## ------------------------------Header-----------------------------------------------------------------
    navbarMenu(
      icon("power-off"),
      tabPanel(
        "Stop",
        DT::dataTableOutput("table1")
      ),
      tabPanel(
        "Save State",
        DT::dataTableOutput("table2")
      ),
      tabPanel(
        "Leave",
        DT::dataTableOutput("table3")
      )
    ),
    navbarMenu(
      icon("question-circle"),
      tabPanel(
        "Help",
        DT::dataTableOutput("table4")
      ),
      tabPanel(
        "About",
        DT::dataTableOutput("table5")
      ),
      tabPanel(
        "Useful Links",
        DT::dataTableOutput("table6")
      ),
      tabPanel(
        "Background Information Plausible Values",
        DT::dataTableOutput("table7")
      ),
      tabPanel(
        "Background Information CART",
        DT::dataTableOutput("table8")
      ),
      tabPanel(
        "Report Issue",
        DT::dataTableOutput("table9")
      )
    ),
    tabPanel(
      icon("laptop"),
      ## ---------------------------------sidebar----------------------------------
      sidebarLayout(
        sidebarPanel(
          # Conditional Panel for Manage
          ## https://shiny.rstudio.com/reference/shiny/1.6.0/conditionalPanel.html
          ## To do: Filter Data hinzufügen
          ## Conditional panel in conditional panel
          conditionalPanel(
            condition = "input.conditionedPanels== 1",
            fileInput(inputId = "import_state", label = h3("Import pv_obj"),
                      multiple = FALSE, accept = ".rds"),
            hr(),
            fileInput(inputId = "import_bgdata",
                      label = h3("Import background data"),
                      multiple = FALSE, accept = c(".rds", ".sav", ".dta")),
            checkboxInput(inputId = "metric", label = "All variables are metric.",
                          value = FALSE),
            selectInput(inputId = "ordinal", label = "Select ordinal variables",
                        choices = "No data uploaded yet", multiple = TRUE),
            selectInput(inputId = "nominal", label = "Select nominal variables",
                        choices = "No data uploaded yet", multiple = TRUE),
            hr(),
            h3("Download pv_obj"),
            textInput(
              "pv_obj_name", label = "Choose file name (pv_obj)",
              value = paste0("pv_obj_", gsub(":", "-", gsub(" ", "_", Sys.time())))
            ),
            downloadButton("download_pv_obj", label = "Download pv_obj (.rds)"),
            selectInput("export_format", label = "Select export format",
                        choices = c("SPSS", "Stata", "Mplus")),
            downloadButton("export_pv_obj", label = "Export pv_obj"),
            actionButton("display_bgdata", "Display bgdata"),
            selectInput("bgdata_select_cols", "Select columns", choices = "",
                        multiple = TRUE),
            textInput("bgdata_filter_rows", "Filter"),
            selectInput("bgdata_sort_cases", "Sort by", choices = ""),
            checkboxInput("bgdata_ascending", "Ascending", value = TRUE)
          ),

          # Conditional Panel for Input Parameter
          conditionalPanel(
            condition = "input.conditionedPanels==2",
            h3("Arguments for Plausible Values Estimation"),
            selectInput("select_starting_cohort",
              label = h3("Starting Cohort"),
              choices = 1:6,
              selected = ""
            ),
            selectInput("select_domain",
              label = h3("Domain"),
              choices = c(
                "Mathematics" = "MA", "Reading" = "RE", "Science" = "SC",
                "Information and Communication Technology" = "IC",
                "Listening Comprehension" = "LI",
                "English as a Foreign Language" = "EF",
                "Native Russian" = "NR", "Native Turkish" = "NT",
                "Orthography A" = "ORA","Orthography B" = "ORB",
                "Scientific Thinking" = "ST", "Business Administration" = "BA",
                "Cognitive Development" = "CD", "Grammar" = "GR",
                "Vocabulary" = "VO"
              ),
              selected = ""
            ),
            selectInput("select_wave",
              label = h3("Wave"),
              choices = 1:12,
              selected = ""
            ),
            textInput(inputId = "path_to_data", label = "Directory with SUFs",
                      value = getwd()),
            checkboxInput("longitudinal", label = "Longitudinal?", value = FALSE),
            checkboxInput("rotation", label = "Rotation?", value = TRUE),
            checkboxInput("adjust_school_context",
                          label = "Adjust for school context?", value = TRUE),
            numericInput("npv", label = h3("Number of plausible values"),
                         value = 10),
            numericInput("nmi", label = h3("Number of imputations"),
                         value = 10),
            numericInput("min_valid",
                         label = h3("Minimum of valid answers to competence test"),
                         value = 3),
            checkboxInput("include_nr",
                          label = "Include number of not-reached missing values as proxy for processing speed?",
                          value = TRUE),
            checkboxInput("WLE",
                          label = "Estimate WLEs?",
                          value = FALSE),
            checkboxInput("EAP",
                          label = "Return EAPs?",
                          value = FALSE),
            checkboxInput("verbose",
                          label = "Progress reports?",
                          value = TRUE),
            # other controls: not changeable!,
            hr(),
            actionButton("estimate_pv_obj", label = "Start estimation")
          ),

          # Conditional Panel for Visualize Estimates
          conditionalPanel(
            condition = "input.conditionedPanels==3",
            checkboxGroupInput("checkGroup1",
              label = h3("Visualizations"),
              choices = list(
                "Distribution of plausible values" = 1,
                "Imputations" = 2, "Regression weights" = 3
              ),
              selected = 1
            ),
            # Weiteres Conditional Panel
            checkboxGroupInput("checkGroup2",
              label = h3("Visualizations"),
              choices = list(
                "Lables" = 1,
                "Style" = 2
              ),
              selected = 1
            ),
            h3("Distribution plot"),
            selectInput(inputId = "geom", label = "Select plot type",
                        choices = c("Histogram", "Density plot", "Scatter plot")),
            selectInput("x", label = "Select variable on x-axis",
                        choices = ""),
            selectInput("y", label = "Select variable on y-axis",
                        choices = ""),
            selectInput("fill", label = "Select variable for color coding",
                        choices = ""),
            textInput(inputId = "title", label = "Plot title"),
            textInput(inputId = "xlab", label = "Label of x-axis"),
            textInput(inputId = "ylab", label = "Label of y-axis"),
            selectInput("theme", label = "Select plot theme",
                        choices = c("Gray", "Black and white", "Linedraw", "Light",
                                    "Dark", "Minimal", "Classic", "Void")),
            actionButton("plot", label = "Display plot"),
            h3("CART plots"),
            selectInput(inputId = "imputation", label = "Select imputation",
                        choices = ""),
            selectInput(inputId = "variable", label = "Select variable",
                        choices = ""),
            actionButton(inputId = "cart_plot", label = "Display tree plot"),
            actionButton(inputId = "variable_importance_plot", label = "Display variable importance plot")
          ),

          # Conditional Panel for Explore Estimates
          conditionalPanel(
            condition = "input.conditionedPanels==4",
            "Comparison of mean values",
            "Quantiles",
            "Measure Location",
            "Correlations",
            "Simple linear regressions"
          ),

          # Conditional Panel for Summary Statistics
          conditionalPanel(
            condition = "input.conditionedPanels==5",
            checkboxGroupInput("checkGroup3",
              label = h3("Summaries for"),
              choices = list(
                "Plausible values" = 1,
                "Imputations" = 2, "Model summaries" = 3
              ),
              selected = 1
            ),
            hr(),
            checkboxGroupInput("checkGroup4",
              label = h3(),
              choices = list(
                "Lables" = 1,
                "Style" = 2
              ),
              selected = 1
            ),
            textInput("title", label = h3("Title"), value = "Enter text..."),

            hr(),
            textInput("subtitle", label = h3("Subtitle"), value = "Enter text..."),

            hr(),
            textInput("caption", label = h3("Caption"), value = "Enter text...")
          )
        ),
        ## --------------------------------Main Panel-----------------------------------------------------------
        mainPanel(
          tabsetPanel(
            tabPanel("Manage", value = 1,
                     verbatimTextOutput("summary"),
                     dataTableOutput("bgdata_display")),
            tabPanel("Estimate Plausible Values", value = 2,
                     h3(textOutput("plausible_values_progress"))),
            tabPanel("Visualize Estimates", value = 3,
                     tableOutput("regression_table"),
                     textInput("regression_name", label = "Table name",
                               value = paste0("regression_",
                                              gsub(":", "-", gsub(" ", "_", Sys.time())))),
                     # selectInput("regression_format",
                     #             label = "Select export format",
                     #             choices = c("LaTeX", "tsv")),
                     downloadButton(outputId = "download_regression",
                                    label = "Download table"),

                     plotOutput("plot"),
                     textInput("plot_name", label = "Plot name",
                               value = paste0("plot_",
                                              gsub(":", "-", gsub(" ", "_", Sys.time())))),
                     selectInput("plot_format",
                                 label = "Select export format",
                                 choices = c("png", "RData")),
                     downloadButton(outputId = "download_plot",
                                    label = "Download plot"),

                     plotOutput("cart_plot"),
                     textInput("cart_name", label = "Plot name",
                               value = paste0("cart_",
                                              gsub(":", "-", gsub(" ", "_", Sys.time())))),
                     selectInput("cart_format",
                                 label = "Select export format",
                                 choices = c("png", "RData")),
                     downloadButton(outputId = "download_cart",
                                    label = "Download plot"),

                     plotOutput("variable_importance_plot"),
                     textInput("variable_importance_name", label = "Plot name",
                               value = paste0("variable_importance_",
                                              gsub(":", "-", gsub(" ", "_", Sys.time())))),
                     selectInput("variable_importance_format",
                                 label = "Select export format",
                                 choices = c("png", "RData")),
                     downloadButton(outputId = "download_variable_importance",
                                    label = "Download plot")
            ),
            tabPanel("Explore Estimates", value = 4,
                     dataTableOutput("imputations_display")),
            tabPanel("Summary Statistics", value = 5,
                     tableOutput("imputation_table"),
                     textInput("descriptive_name", label = "Table name",
                               value = paste0("descriptives_",
                                              gsub(":", "-", gsub(" ", "_", Sys.time())))),
                     # selectInput("descriptive_format",
                     #             label = "Select export format",
                     #             choices = c("LaTeX", "tsv")),
                     downloadButton(outputId = "download_descriptive",
                                    label = "Download table"),
                     tableOutput("item_difficulties")),
            id = "conditionedPanels"
          )
        )
      ),
    )
  )
)
