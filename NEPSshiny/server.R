#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#### ACHTUNG
# Environment weglassen: nur 1 bgdata hochladen und 1 pv_obj je session
# Plots/Summaries etc. für Imputationen und PVs an einer Stelle anbieten (= Unterseite weglassen)
# --> Arbeiten mit denselben Methoden auf derselben Datenstruktur


library(shiny)

shinyServer(function(input, output, session) {

  # ---------------------------- UPLOAD PREVIOUS STATE -----------------------
  # previous state == pv_obj as rds
  observeEvent(input$import_state, { # actionButton!
    req(input$state_file)

    if (!grepl("rds$", tolower(tools::file_ext(input$state_file$name)))) {
      validate("Previous estimation not stored as RDS file.")
    }
    pv_obj <- readRDS(file = input$state_file$datapath) # fileInput input
  })

  # ---------------------------- UPLOAD BGDATA -------------------------------
  # https://mastering-shiny.org/action-transfer.html
  bgdata <- eventReactive(input$import_bgdata, { # actionButton!
    req(input$bgdata_filepath)
    filetype <- tools::file_ext(input$bgdata_file$name)

    if (grepl("rds$", tolower(filetype))) {
      readRDS(file = input$bgdata_file$datapath)
    } else if (grepl("sav$", tolower(filetype))) {
      haven::read_spss(file = input$bgdata_file$datapath)
    } else if (grepl("dta$", tolower(filetype))) {
      haven::read_dta(file = input$bgdata_file$datapath)
    } else {
      validate(paste0(
        "Format of bgdata (", filetype, ") not recognized.\n",
        "Needs: RDS, SPSS or Stata format."
      ))
    }
  })


  # ---------------------------- DISPLAY BGDATA ------------------------------
  # select columns, filter by values, select rows
  # paged table
  bgdata_display <- eventReactive(input$display_bgdata, {
    req(input$filter_var)
    out <- bgdata()
    # TODO: mehrere Variablen in Vektorform bringen!
    if (TRUE) { # variables for selection have been chosen
      sel <- names(out)[names(out %in% input$select_var)]
      out <- out %>% dplyr::select(sel)
    }
    # TODO: Loop über mehrere Filter, evtl. erst in Vektorform parsen
    # kommt auf Format von input$filter_var an: Liste von Filtern? String von Filtern?
    if (TRUE) { # variables for filter, test each, then filter
      filter_op <- stringr::str_extract(input$filter_var, "[<>=!]+")
      filter_var <- stringr::word(input$filter_var, 1, sep = "[ <>!=]")
      filter_val <- sub(".*[ <>!=]", "", input$filter_var)
      switch(
        filter_op,
        "<" = out <- out %>%
          dplyr::filter(rlang::.data[[filter_var]] < filter_val),
        ">" = out <- out %>%
          dplyr::filter(rlang::.data[[filter_var]] > filter_val),
        ">=" = out <- out %>%
          dplyr::filter(rlang::.data[[filter_var]] >= filter_val),
        "<=" = out <- out %>%
          dplyr::filter(rlang::.data[[filter_var]] <= filter_val),
        "==" = out <- out %>%
          dplyr::filter(rlang::.data[[filter_var]] == filter_val),
        "!=" = out <- out %>%
          dplyr::filter(rlang::.data[[filter_var]] != filter_val),
        showNotification("Filter operator not valid.")
      )
    }
    # TODO: Sortierung noch mit einbauen (dplyr::arrange())
    out
  })
  output$bgdata_display <- renderTable(bgdata_display())

  # ---------------------------- ESTIMATE PVS --------------------------------

  pv_obj <- eventReactive(input$estimate, {
    # TODO: input$path: what format? character or fileInput?
    req(
      bgdata(), input$SC, input$domain, input$wave, input$path, input$npv,
      input$longitudinal, input$rotation, input$min_valid,
      input$include_nr, input$verbose, input$adjust_school_context,
      input$control
    )
    # print output to shiny to monitor progress:
    # https://stackoverflow.com/questions/30474538/possible-to-show-console-messages-written-with-message-in-a-shiny-ui/30490698#30490698
    withCallingHandlers(
      {
        shinyjs::html("plausible_values_progress", "")
        plausible_values(
          SC = input$SC, domain = input$domain, wave = input$wave,
          path = input$path, bgdata = bgdata(), npv = input$npv,
          longitudinal = input$longitudinal,
          rotation = input$rotation, min_valid = input$min_valid,
          include_nr = input$include_nr, verbose = input$verbose,
          adjust_school_context = input$adjust_school_context,
          control = input$control
        )
      },
      message = function(m) {
        # TODO: Konsolen-Output in NEPSscaling evtl. anpassen z.B. in CART
        # https://stackoverflow.com/questions/5953718/overwrite-current-output-in-the-r-console/62059670#62059670
        # Farben anpassen für message: gerade selbe Farbe wie Errors -- r package crayon works
        shinyjs::html(id = "plausible_values_progress", html = m$message)
      }
    )
  })

  # --------------------------- CREATE CART Plot -----------------------------
  # TODO: add customization of treeplot?
  cart_plot <- eventReactive(input$display_cart, {
    req(pv_obj, input$imputation_number, input$imputed_variable)
    display_tree(pv_obj, input$imputation_number, input$imputed_variable)
  })
  output$cart_plot <- renderPlot(cart_plot())

  # --------------------------- CREATE DISTR Plots for pv_obj$pvs ------------
  # one page suffices if columns (incl. PV columns) can be selected here!
  # only!!!! averaged values!!!!
  distribution_plot <- eventReactive(input$display_distribution, {
    req(pv_obj)
    # build plot
  })
  output$distribution_plot <- renderPlot(distribution_plot())

  # output$plot <- renderPlot({
  #     if (req(input$plotType) == "histogram") {
  #         hist(dataset())
  #     } else if (input$plotType == "scatter") {
  #         qplot(dataset(), aes(x = x, y = y))
  #     }
  # })

  # --------------------------- CREATE REGRESSION TABLES ---------------------
  # formats: LaTeX, data.frame


  # --------------------------- CREATE SUMMARY STATISTICS --------------------
  # tables, formats: LaTeX, data.frame

  # ------------------------ SAVE SHINY STATE --------------------------------
  # == pv_obj as RDS
  output$save_state <- downloadHandler(
    filename = "pv_obj.rds",
    content = function(fname) {
      req(pv_obj)
      saveRDS(pv_obj, file = input$state_file$datapath)
    }
  )

  # ------------------------ EXPORT PV_OBJ -----------------------------------
  # formats: spss, stata, mplus
  # https://stackoverflow.com/questions/28228892/download-multiple-csv-files-in-a-zipped-folder-in-shiny
  output$export_pv_obj <- downloadHandler(
    filename = "export_pv_obj.zip",
    content = function(fname) {
      req(pv_obj)
      # get vector of file names (different for each file extension)
      files <- c()
      write_pv(pv_obj = pv_obj, path = input$state_file$datapath, ext = input$ext)

      zip(zipfile = fname, files = files)
    },
    contentType = "application/zip"
  )


  # ------------------------ SAVE GGPLOTS ------------------------------------
  # png, jpeg
  # easier: save button just below the create button for each plot
  output$download_distribution_plot <- downloadHandler(
    filename = function() {
      req(input$plotname)
      paste0(input$plotname, ".png")
    },
    content = function(fname) {
      ggplot2::ggsave(filename = fname, plot = distribution_plot())
    }
  )
  output$download_cart_plot <- downloadHandler(
    filename = function() {
      req(input$imputation_number, input$imputed_variable)
      paste0(
        "tree_imputation_", input$imputation_number, "_",
        input$imputed_variable, ".png"
      )
    },
    content = function(fname) {
      ggplot2::ggsave(filename = fname, plot = distribution_plot())
    }
  )

  # ------------------------ SAVE TABLES -------------------------------------
  # tsv? LaTex? Excel?
})
