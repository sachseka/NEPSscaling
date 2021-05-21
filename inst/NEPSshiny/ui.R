#library(shinythemes)
#library(shiny)
#library(shinydashboard)
#library(shinydashboardPlus)
library(bslib)
library(shinyBS)


shinyUI(
  navbarPage(id="navbar",
   theme = bs_theme(
      bg = "#E8E6F4", fg = "black", primary = "#24195D", secondary = "#1D0E46",
      "font-size-base" = "1.25rem",
      base_font = font_google("Open Sans"),
      code_font = font_google("Open Sans")
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
            shinyBS::tipify(actionButton("btn", icon = icon("info"),"Information"), "Upload size up to 30KB", placement="bottom", trigger = "hover"),

            hr(),
            fileInput(inputId = "import_bgdata",
                      label = h3("Import background data"),
                      multiple = FALSE, accept = c(".rds", ".sav", ".dta")),
            shinyBS::tipify(actionButton("btn2", icon = icon("info"),"Information"), "Upload size up to 30KB", placement="bottom", trigger = "hover"),
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
                        label = h5("Starting Cohort"),
                        choices = 1:6,
                        selected = ""
            ),
            selectInput("select_domain",
                        label = h5("Domain"),
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
                        label = h5("Wave"),
                        choices = 1:12,
                        selected = ""
            ),
            textInput(inputId = "path_to_data", label = "Directory with SUFs",
                      value = getwd()),
            checkboxInput("longitudinal", label = "Longitudinal?", value = FALSE),
            checkboxInput("rotation", label = "Rotation?", value = TRUE),
            checkboxInput("adjust_school_context",
                          label = "Adjust for school context?", value = TRUE),
            numericInput("npv", label = h5("Number of plausible values"),
                         value = 10, min= 1),
            numericInput("nmi", label = h5("Number of imputations"),
                         value = 10, min= 1),
            numericInput("min_valid",
                         label = h5("Minimum of valid answers to competence test"),
                         value = 3, min= 1),
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
            radioButtons("checkGroup1",
                         label = h3("Visualizations"),
                         choices = list(
                           "Distribution of plausible values and imputations" = 1,
                           "Regression weights" = 2
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
            h3("Imputation tree structures"),
            selectInput(inputId = "imputation", label = "Select imputation",
                        choices = ""),
            selectInput(inputId = "variable", label = "Select variable",
                        choices = ""),
            actionButton(inputId = "cart_plot", label = "Display tree plot"),
            actionButton(inputId = "variable_importance_plot", label = "Display variable importance plot")
          ),

          # Conditional Panel for Summary Statistics
          conditionalPanel(
            condition = "input.conditionedPanels==5",
            radioButtons("checkGroup3",
                         label = h3("Summaries for"),
                         choices = list(
                           "Plausible values and imputations" = 1,
                           "Model summaries" = 2
                         ),
                         selected = 1
            ),
            hr(),
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
                     
                     conditionalPanel(
                       condition = "input.checkGroup1==1",
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
                     conditionalPanel(
                       condition = "input.checkGroup1==2",
                       tableOutput("regression_table"),
                       textInput("regression_name", label = "Table name",
                                 value = paste0("regression_",
                                                gsub(":", "-", gsub(" ", "_", Sys.time())))),
                       # selectInput("regression_format",
                       #             label = "Select export format",
                       #             choices = c("LaTeX", "tsv")),
                       downloadButton(outputId = "download_regression",
                                      label = "Download table")
                       )),
            tabPanel("Summary", value = 5,
                     conditionalPanel(
                       condition = "input.checkGroup3==1",
                       tableOutput("imputation_table"),
                       textInput("descriptive_name", label = "Table name",
                                 value = paste0("descriptives_",
                                                gsub(":", "-", gsub(" ", "_", Sys.time())))),
                       # selectInput("descriptive_format",
                       #             label = "Select export format",
                       #             choices = c("LaTeX", "tsv")),
                       # uiOutput("download_descriptive"),
                       downloadButton(outputId = "download_descriptive",
                                      label = "Download Descriptives")
                     ),
                     conditionalPanel(
                       condition = "input.checkGroup3==2",
                       tableOutput("item_difficulties")
                       )),
            id = "conditionedPanels"
          )
        )
      ),
    ),
    title = "NEPSscaling",
    tabPanel(
    fluidRow(uiOutput("tab")),
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
    )),

    # print output to shiny: https://stackoverflow.com/questions/30474538/possible-to-show-console-messages-written-with-message-in-a-shiny-ui/30490698#3049069844446666666
    shinyjs::useShinyjs(),
    ## ------------------------------Header-----------------------------------------------------------------
      navbarMenu(
      icon("question-circle"),
      tabPanel(
        "Contact",
        fluidRow("Contact")
      ),
      tabPanel(
        "Background Information Plausible Values",
        fluidRow(
          tags$dl(
            tags$dt("Plausible Values"),
            tags$dd(
              tags$ul(
                tags$li("Estimators for latent constructs such as competencies"),
                tags$li("Set of random draws out of individual respondent's latent competence distribution"),
                tags$li("Derived from competence test and respondent characteristics (e.g., gender, socio-economic status)"),
                tags$li("Uncertainty in random draws reflects uncertainty in competence estimation"),
                tags$li("Background variables should at least contain all variables used for later analysis"),
                tags$li("Unbiased on a population level, but biased on the individual level because of respondent information (i.e., group-level information)"),
                tags$li("Special case of multiple imputation: statistical analyses with plausible values have to be performed accordingly")
              )
            )
          ),
          tags$img(style="max-width: 500px; width: 40%; height: auto;",
                   src = "structural_model_pvs.png", alt = "Structural model"),
          tags$dl(
            tags$dt("Recommended Reading"),
            tags$dd(
              tags$ol(
                tags$li("Scharl, A., Carstensen, C.H., & Gnambs, T. (2020). Estimating Plausible Values with NEPS Data: An Example Using Reading Competence in Starting Cohort 6 (NEPS Survey Paper No. 71). Bamberg: Leibniz Institute for Educational Trajectories, National Educational Panel Study. doi:10.5157/NEPS:SP71:1.0"),
                tags$li("von Davier, M., Gonzalez, E., & Mislevy, R. (2009). What are plausible values and why are they useful. IERI Monograph Series, 2, 9–36."),
                tags$li("Lüdtke, O., & Robitzsch, A. (2017). Eine Einführung in die Plausible-Values-Technik für die psychologische Forschung. Diagnostica, 63(3), 193–205. doi:10.1026/0012-1924/a000175"),
                tags$li("Rubin, D. B. (1987). Multiple imputation for nonresponse in surveys. doi:10.1002/9780470316696"),
                tags$li("Mislevy, R. J. (1991). Randomization-based inference about latent variables from complex samples. Psychometrika, 56(2), 177–196. doi:10.1007/BF02294457"),
                tags$li("Meng, X.-L. (1994). Multiple-imputation inferences with uncongenial sources of input. Statistical Science, 538–558. doi:10.1214/ss/1177010269")
              )
            )
          )
        )
      ),
      tabPanel(
        "Background Information CART",
        fluidRow(
          tags$dl(
            tags$dt("Classification and Regression Trees"),
            tags$dd(
              tags$ul(
                tags$li("Background variables for plausible values cannot contain missingness, but non-response is pervasive in large scale assessments and surveys"),
                tags$li("Multiple imputation as an approach to fill in randomly missing data without introducing further bias"),
                tags$li("Decision trees (e.g., classification and regression trees, CART) can be used to identify a set of plausible responses for the missing data"),
                tags$li("Variable with missingness is recursively split into subsets; each subset has to be more homogenous than the superset"),
                tags$li("Splits are made according to a value on a predictor variable (e.g., being female, being older than X years) until a node purity criterion is reached"),
                tags$li("Prediction for missing values are drawn by following the tree's branches to its nodes and choosing a value from the node following an algorithm"),
                tags$li("CART is a non-parametric approach and automatically incorporates non-linear relationships in the predicted and predictor variables")
              )
            )
          ),
          tags$img(style="max-width: 500px; width: 40%; height: auto;",
                   src = "binary_tree.png", alt = "Binary decision tree"),
          tags$dl(
            tags$dt("Recommended Reading"),
            tags$dd(
              tags$ol(
                tags$li("Scharl, A., Carstensen, C.H., & Gnambs, T. (2020). Estimating Plausible Values with NEPS Data: An Example Using Reading Competence in Starting Cohort 6 (NEPS Survey Paper No. 71). Bamberg: Leibniz Institute for Educational Trajectories, National Educational Panel Study. doi:10.5157/NEPS:SP71:1.0"),
                tags$li("Aßmann, C., Gaasch, C., Pohl, S., & Carstensen, C. H. (2016). Estimation of plausible values considering partially missing background information: A data augmented MCMC approach. In H.-P. Blossfeld, J. Skopek, J. Maurice, & M. Bayer (Eds.), Methodological Issues of Longitudinal Surveys (pp. 503–521). Springer."),
                tags$li("Loh, W.-Y. (2011). Classification and regression trees. Wiley Interdisciplinary Reviews: Data Mining and Knowledge Discovery, 1(1), 14–23. doi:10.1002/widm.8"),
                tags$li("Rubin, D. B. (1987). Multiple imputation for nonresponse in surveys. doi:10.1002/9780470316696")
              )
            )
          )
        )
      )),
    tabPanel(
      fluidRow(
        column(1, offset = 1, img(height = 50, width = 100, src = "NEPS_reduziert_RGB_v01.png")),
        column(1, offset = 5, img(height = 50, width = 55, src = "LIfBi_Logo_solo_RZ.png"))))
    )
)


enableBookmarking(store = "url")
#shinyApp(ui, server)




