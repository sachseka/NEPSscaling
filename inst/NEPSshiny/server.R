
library(shiny)
library(xtable)
# library(NEPSscaling)

Start <-function(input, output, session) {
  onclick(input$Laptop, {
    session$sendCustomMessage(type = 'testmessage',
                              message = 'Thank you for clicking')
  })
}

Help<- function(input, output, session) {
 onclick(input$Help, {
    session$sendCustomMessage(type = 'testmessage',
                              message = 'Thank you for clicking')
  })
}

Information_size1 <- function(input, output, session) {
  addTooltip(session=session,id="btn",title="Hover pop-up.")
}
Information_size2 <- function(input, output, session) {
  addTooltip(session=session,id="btn2",title="Hello! This is a hover pop-up. You'll have to hover to see the next one.")
}

filter_data <- function(filter_op, filter_var, filter_val, out) {
  switch(filter_op,
    "<" = dplyr::filter(out, .data[[filter_var]] < filter_val),
    ">" = dplyr::filter(out, .data[[filter_var]] > filter_val),
    "<=" = dplyr::filter(out, .data[[filter_var]] <= filter_val),
    ">=" = dplyr::filter(out, .data[[filter_var]] >= filter_val),
    "==" = dplyr::filter(out, .data[[filter_var]] == filter_val),
    "!=" = dplyr::filter(out, .data[[filter_var]] != filter_val),
    showNotification(paste("Filter operator", filter_op, "not valid."),
                     type = "error")
  )
}
options(shiny.maxRequestSize = 30*1024^2)

export_files <- function(format, name) {
  switch(format,
         "SPSS" = paste0(name, "_", 1:10, ".sav"),
         "Stata" = paste0(name, "_", 1:10, ".dta"),
         "Mplus" = c(paste0(name, "_", 1:10, ".dat"), "content_file.dat"))
}

#https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode/8189441#8189441
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

shinyServer(function(input, output, session) {
  values <- reactiveValues(
    pv_obj = NULL
  )



  ############################################################################
  #                                    UPLOAD
  ############################################################################

  # ------------------------------ UPLOAD STATE ------------------------------
  # previous state == pv_obj as rds
  observe({
    req(input$import_state)
    validate(need(tools::file_ext(input$import_state$datapath) == "rds",
                  "pv_obj must be stored as '.rds' file."))

    out <- readRDS(file = input$import_state$datapath)

    if (class(out) != "pv_obj") {
      showNotification("pv_obj must be of class 'pv_obj'.", type = "error")
    } else {
      values$pv_obj <- out
    }

    if (!input$metric & !isTruthy(input$nominal) & !isTruthy(input$ordinal)) {
      updateSelectInput(session = session, inputId = "ordinal",
                        label = "Select ordinal variables",
                        choices = names(out$pv[[1]]))
      updateSelectInput(session = session, inputId = "nominal",
                        label = "Select nominal variables",
                        choices = names(out$pv[[1]]))
      showNotification("Please specify the scale levels of the data under 'Manage'.",
                       type = "message")
    }

    updateSelectInput(session, inputId = "imputation",
                      choices = names(out$treeplot),
                      selected = "")
    updateSelectInput(session, inputId = "variable",
                      choices = names(out$treeplot[[1]]),
                      selected = "")
    updateSelectInput(session, inputId = "fill",
                      choices = unique(c(input$nominal, input$ordinal)),
                      selected = "")
    updateSelectInput(session, inputId = "x",
                      choices = names(out$pv[[1]]),
                      selected = "")
    updateSelectInput(session, inputId = "y",
                      choices = names(out$pv[[1]]),
                      selected = "")
  })

  # ---------------------------- UPLOAD BGDATA -------------------------------
  # https://mastering-shiny.org/action-transfer.html
  bgdata_raw <- reactive({
    req(input$import_bgdata)
    filetype <- tools::file_ext(input$import_bgdata$datapath)

    out <- switch(
      filetype,
      rds = readRDS(file = input$import_bgdata$datapath),
      sav = haven::read_spss(file = input$import_bgdata$datapath),
      dta = haven::read_dta(file = input$import_bgdata$datapath),
      validate(paste0(
        "Format of bgdata (", filetype, ") not recognized.\n",
        "Needs: R object (.rds), SPSS (.sav) or Stata (.dta) format."
      ))
    )

    if (!is.data.frame(out)) {
      showNotification("bgdata must be a data.frame.", type = "error")
      return(NULL)
    }

    updateSelectInput(session = session, inputId = "ordinal",
                      label = "Select ordinal variables", choices = names(out))
    updateSelectInput(session = session, inputId = "nominal",
                      label = "Select nominal variables", choices = names(out))

    updateSelectInput(session = session, inputId = "bgdata_select_cols",
                      label = "Select columns", choices = names(out),
                      selected = "")
    updateSelectInput(session = session, inputId = "bgdata_sort_cases",
                      label = "Sort by", choices = names(out),
                      selected = "")

    out
  })

  bgdata <- reactive({
    req(bgdata_raw())
    nominal <- input$nominal
    ordinal <- input$ordinal
    out <- bgdata_raw()

    if (!input$metric & is.null(nominal) & is.null(ordinal)) {
      showNotification("Please specify the scale levels of the data.",
                       type = "message")
      return(NULL)
    }

    if (!is.null(ordinal) | !is.null(nominal)) {
      sel <- unique(c(ordinal, nominal))
      if (length(sel) == 1) {
        out[[sel]] <- as.factor(out[[sel]])
      } else {
        out[, sel] <- lapply(out[, sel], as.factor)
      }
    }

    out
  })
  # output$bgdata <- renderDataTable(head(bgdata()))

  ############################################################################
  #                            MANIPULATE
  ############################################################################

  # ---------------------------- DISPLAY BGDATA ------------------------------
  # select columns, filter by values, select rows
  # paged table
  bgdata_display <- reactive({
    req(bgdata())
    out <- bgdata()

    if (isTruthy(input$bgdata_select_cols)) { # variables for selection have been chosen
      sel <- names(out)[names(out) %in% input$bgdata_select_cols]
      out <- out[, sel, drop = FALSE]
    }

    # bildet nur &, nicht | ab, wobei man | auch als & umformulieren kann
    if (isTruthy(input$bgdata_filter_rows)) {
      for (f in input$bgdata_filter_rows) { # swap f for input$bgdata_filter_rows if no loop is required!
        filter_op <- stringr::str_extract(f, "[<>=!]+")
        filter_var <- stringr::word(f, 1, sep = "[ <>!=]")
        filter_val <- sub(".*[ <>!=]", "", f)
        out <- filter_data(filter_op, filter_var, filter_val, out)
      }
    }

    if (isTruthy(input$bgdata_sort_cases)) {
      if (input$bgdata_ascending) {
        out <- dplyr::arrange(out, .data[[input$bgdata_sort_cases]])
      } else {
        out <- dplyr::arrange(out, dplyr::desc(.data[[input$bgdata_sort_cases]]))
      }
    }
    out
  })
  output$bgdata_display <- renderDataTable(
    bgdata_display(),
    options = list(pageLength = 25)
  )

  # ---------------------------- ESTIMATE PVS --------------------------------

  observeEvent(input$estimate_pv_obj, {

    req(
      bgdata(), input$select_starting_cohort, input$select_domain,
      input$select_wave, input$path_to_data
    )

    # print output to shiny to monitor progress:
    # https://stackoverflow.com/questions/30474538/possible-to-show-console-messages-written-with-message-in-a-shiny-ui/30490698#30490698
    withCallingHandlers(
      {
        shinyjs::html("plausible_values_progress", "")
        out <- NEPSscaling::plausible_values(
          SC = as.numeric(input$select_starting_cohort),
          domain = input$select_domain,
          wave = as.numeric(input$select_wave),
          path = gsub("\\\\", "/", input$path_to_data),
          bgdata = bgdata(),
          npv = as.numeric(input$npv),
          longitudinal = input$longitudinal,
          rotation = input$rotation,
          min_valid = as.numeric(input$min_valid),
          include_nr = input$include_nr,
          verbose = input$verbose,
          adjust_school_context = input$adjust_school_context,
          control = list(WLE = input$WLE, EAP = input$EAP,
                         ML = list(nmi = input$nmi))
        )
      },
      message = function(m) {
        shinyjs::html(id = "plausible_values_progress", html = m$message)
      }#,
      # error = function(e) print(sys.calls())
    )

    values$pv_obj <- out

    updateSelectInput(session, inputId = "imputation",
                      choices = names(out$treeplot),
                      selected = "")
    updateSelectInput(session, inputId = "variable",
                      choices = names(out$treeplot[[1]]),
                      selected = "")
    updateSelectInput(session, inputId = "fill",
                      choices = unique(c(input$nominal, input$ordinal)),
                      selected = "")
    updateSelectInput(session, inputId = "x",
                      choices = names(out$pv[[1]]),
                      selected = "")
    updateSelectInput(session, inputId = "y",
                      choices = names(out$pv[[1]]),
                      selected = "")

  })

  # ------------------------- SUMMARY OF PV_OBJ ------------------------------

  output$summary <- renderText({
    req(values$pv_obj)
    paste(capture.output(print(values$pv_obj)), collapse = "\n")
  })

  # ------------------------- ITEM DIFFICULTIES ------------------------------

  output$item_difficulties <- renderTable({
    req(values$pv_obj)
    if (NEPSscaling::get_type(pv_obj = values$pv_obj) == "longitudinal") {
      new_items <- paste0("items_w", NEPSscaling::get_wave(values$pv_obj))
      new_xsi <- paste0("xsi_w", NEPSscaling::get_wave(values$pv_obj))
      NEPSscaling::get_item_difficulties(values$pv_obj) %>%
        purrr::map(.f = function(mat) {
          colnames(mat) <- base::make.names(colnames(mat), unique = TRUE)
          mat}) %>%
        purrr::map(tibble::as_tibble, rownames = "items") %>%
        purrr::map(dplyr::rename, "pos" = "...1") %>%
        purrr::map2(.y = new_items, ~dplyr::rename(.x, !!.y := "items")) %>%
        purrr::map2(.y = new_xsi, ~dplyr::rename(.x, !!.y := "xsi")) %>%
        purrr::reduce(dplyr::full_join, by = "pos") %>%
        dplyr::select(-.data$pos) %>%
        dplyr::mutate_if(.predicate = is.numeric,
                         .funs = round, digits = 3) %>%
        as.data.frame()
    } else {
      items <- data.frame(rownames(NEPSscaling::get_item_difficulties(values$pv_obj)),
                          round(NEPSscaling::get_item_difficulties(values$pv_obj), 3))
      names(items) <- c("Items", "xsi", "se")
      items
    }
  },
    caption = "Item Difficulty Parameters. SE of fixed parameters is set to 0.",
    caption.placement = getOption("xtable.caption.placement", "top"),
    caption.width = getOption("xtable.caption.width", NULL)
  )


  # ------------------------ AVERAGE IMPUTED DATA ----------------------------

  average_pvs <- reactive({
    req(values$pv_obj)

    out <- values$pv_obj$pv[[1]]

    tmp <- replicate(length(names(out)[-1]),
                     data.frame(ID_t = out[["ID_t"]]), simplify = FALSE)
    names(tmp) <- names(out)[-1]
    for (i in seq(NEPSscaling::get_npv(values$pv_obj))) {
      for (var in names(out)[-1]) {
        tmp[[var]] <- dplyr::left_join(tmp[[var]],
                                       values$pv_obj$pv[[i]][, c("ID_t", var)],
                                       by = "ID_t")
      }
    }
    tmp <- lapply(names(tmp), function(x) {
      if (x %in% input$ordinal) {
        as.factor(apply(tmp[[x]][, -1], 1, median, na.rm = TRUE))
      } else if (x %in% input$nominal) {
        as.factor(apply(tmp[[x]][, -1], 1, Mode))
      } else {
        apply(tmp[[x]][, -1], 1, mean, na.rm = TRUE)
      }
    })
    out[, -1] <- as.data.frame(tmp)
    out
  })

  # ----------------------- DISPLAY AVERAGE IMPUTATIONS ----------------------
  # select columns, filter by values, select rows
  # paged table
  imputations_display <- reactive({
    req(average_pvs())
    out <- average_pvs()

    if (isTruthy(input$imputations_select_cols)) { # variables for selection have been chosen
      sel <- names(out)[names(out) %in% input$imputations_select_cols]
      out <- out[, sel, drop = FALSE]
    }

    # bildet nur &, nicht | ab, wobei man | auch als & umformulieren kann
    if (isTruthy(input$imputations_filter_rows)) {
      for (f in input$imputations_filter_rows) { # swap f for input$imputations_filter_rows if no loop is required!
        filter_op <- stringr::str_extract(f, "[<>=!]+")
        filter_var <- stringr::word(f, 1, sep = "[ <>!=]")
        filter_val <- sub(".*[ <>!=]", "", f)
        out <- filter_data(filter_op, filter_var, filter_val, out)
      }
    }

    if (isTruthy(input$imputations_sort_cases)) {
      if (input$imputations_ascending) {
        out <- dplyr::arrange(out, .data[[input$imputations_sort_cases]])
      } else {
        out <- dplyr::arrange(out, dplyr::desc(.data[[input$imputations_sort_cases]]))
      }
    }
    out
  })
  output$imputations_display <- renderDataTable(
    imputations_display(),
    options = list(pageLength = 50)
  )

  # --------------------------- CREATE CART PLOT -----------------------------

  cart_plot <- eventReactive(input$cart_plot, {
    req(values$pv_obj, input$imputation, input$variable)
    tryCatch(
      NEPSscaling::display_tree(values$pv_obj, input$imputation, input$variable),
      error = function(e) {
        showNotification(e$message, type = "error")
      }
    )
  })
  output$cart_plot <- renderPlot(cart_plot())

  # ------------------- CREATE VARIABLE IMPORTANCE PLOT ----------------------

  variable_importance_plot <- eventReactive(input$variable_importance_plot, {
    req(values$pv_obj, input$imputation, input$variable)
    NEPSscaling::display_variable_importance(values$pv_obj, input$imputation, input$variable)
  })
  output$variable_importance_plot <- renderPlot(variable_importance_plot())

  # -------------------- CREATE DISTR Plots for pv_obj$pvs -------------------

  plot_geom <- reactive({
    switch(input$geom,
           "Histogram" = ggplot2::geom_histogram(),
           "Density plot" = ggplot2::geom_density(),
           "Scatter plot" = ggplot2::geom_point()
    )
  })

  plot_theme <- reactive({
    switch(input$theme,
           "Gray" = ggplot2::theme_gray(),
           "Black and white" = ggplot2::theme_bw(),
           "Linedraw" = ggplot2::theme_linedraw(),
           "Light" = ggplot2::theme_light(),
           "Dark" = ggplot2::theme_dark(),
           "Minimal" = ggplot2::theme_minimal(),
           "Classic" = ggplot2::theme_classic(),
           "Void" = ggplot2::theme_void()
    )
  })

  plot_title <- reactive({ggplot2::ggtitle(input$title)})

  plot_xlab <- reactive({
    if (isTruthy(input$xlab)) {
      return(ggplot2::xlab(input$xlab))
    } else if (isTruthy(input$x)) {
      return(ggplot2::xlab(input$x))
    }
  })

  plot_ylab <- reactive({
    if (isTruthy(input$ylab)) {
      return(ggplot2::ylab(input$ylab))
    } else if (isTruthy(input$y)) {
      return(ggplot2::ylab(input$y))
    }
  })

  imputation_plot <- eventReactive(input$plot, {
    req(average_pvs(), input$geom, input$x)

    # aesthetics: x, y
    gplot <- ggplot2::ggplot(
      average_pvs(),
      ggplot2::aes(x = .data[[input$x]])
    )
    if (isTruthy(input$y) & input$geom == "Scatter plot") {
      gplot <- gplot + ggplot2::aes(y = .data[[input$y]])
    }

    # aesthetics: fill
    if (isTruthy(input$fill)) {
      gplot <- gplot + ggplot2::aes(fill = .data[[input$fill]],
                                    color = .data[[input$fill]])
    }

    # build plot
    gplot <- gplot +
      plot_geom() +
      plot_title() +
      plot_xlab() +
      plot_ylab() +
      plot_theme()

    gplot
  })
  output$plot <- renderPlot(imputation_plot())

  # --------------------------- CREATE REGRESSION TABLES ---------------------
  regression_table <- reactive({
    req(values$pv_obj)
    tmp <- NEPSscaling::get_regression_coefficients(values$pv_obj)
    if (NEPSscaling::get_type(values$pv_obj) == "longitudinal") {
      tmp <- lapply(tmp, function(x) x[,-1]) %>%
        purrr::reduce(`+`) / length(tmp)
      tab <- data.frame(
        Variable = paste(tmp[[1]]$Variable, "Wave",
                         rep(NEPSscaling::get_wave(values$pv_obj), each = nrow(tmp[[1]]))),
        N = as.character(rep(NEPSscaling::get_n_testtakers(values$pv_obj), each = nrow(tmp[[1]]))),
        b = unname(unlist(tmp[, seq(1, ncol(tmp), 3)])),
        beta = unname(unlist(tmp[, seq(2, ncol(tmp), 3)])),
        se = unname(unlist(tmp[, seq(3, ncol(tmp), 3)]))
      )
    } else {
      tab <- data.frame(
        Variable = tmp$Variable,
        N = as.character(NEPSscaling::get_n_testtakers(values$pv_obj)),
        b = rowMeans(tmp[, grepl("_coeff$", names(tmp))]),
        beta = rowMeans(tmp[, grepl("_std$", names(tmp))]),
        se = rowMeans(tmp[, grepl("_se$", names(tmp))])
      )
    }
    tab[["95% CI of b"]] <- paste0("[", round(tab$b - 1.96 * tab$se, 3),"; ",
                                   round(tab$b + 1.96 * tab$se, 3), "]")
    # tab[["95% CI of beta"]] <- paste0("[", round(tab$beta - 1.96 * tab$se, 3),"; ",
    #                                round(tab$beta + 1.96 * tab$se, 3), "]")
    tab$b <- as.character(round(tab$b, 3))
    tab$beta <- as.character(round(tab$beta, 3))
    tab$se <- as.character(round(tab$se, 3))
    tab
  })
  output$regression_table <- renderTable({
      regression_table()
    },
    caption = "Latent Regression Weights with 95% CI based on normal distribution",
    caption.placement = getOption("xtable.caption.placement", "top"),
    caption.width = getOption("xtable.caption.width", NULL)
  )

  # --------------------------- CREATE SUMMARY STATISTICS --------------------
  imputation_table <- reactive({
    req(average_pvs())
    out <- psych::describe(average_pvs())
    out$vars <- rownames(out)
    out
  })
  output$imputation_table <- renderTable({
      imputation_table()
    },
    caption = "Descriptive Statistics of Average Imputated Data Sets",
    caption.placement = getOption("xtable.caption.placement", "top"),
    caption.width = getOption("xtable.caption.width", NULL)
  )


  ############################################################################
  #                            DOWNLOAD
  ############################################################################

  # ------------------------- DOWNLOAD PV_OBJ --------------------------------
  # == pv_obj as RDS
  output$download_pv_obj <- downloadHandler(
    filename = function() {
      req(input$pv_obj_name)
      paste0(input$pv_obj_name, ".rds")
    },
    content = function(file) {
      req(values$pv_obj)
      saveRDS(values$pv_obj, file = file)
    }
  )

  # --------------------------- EXPORT PV_OBJ --------------------------------
  # formats: spss, stata, mplus
  # https://stackoverflow.com/a/43939912
  output$export_pv_obj <- downloadHandler(
    filename = function() {
      rep(input$pv_obj_name)
      paste0(input$pv_obj_name, ".zip")
    },
    content = function(zipfile) {
      req(values$pv_obj, input$export_format)
      owd <- setwd(tempdir())
      on.exit(setwd(owd))

      # vector of file names for pv_obj$pv data.frames
      files <- export_files(input$export_format, input$pv_obj_name)
      if (input$export_format == "SPSS") {
        for (i in seq(length(values$pv_obj$pv))) {
          haven::write_sav(values$pv_obj$pv[[i]], path = files[i])
        }
      } else if (input$export_format == "Stata") {
        for (i in seq(length(values$pv_obj$pv))) {
          colnames(values$pv_obj[["pv"]][[i]]) <-
            gsub("[[:punct:]]", "_", colnames(values$pv_obj[["pv"]][[i]]))
          haven::write_dta(values$pv_obj$pv[[i]], path = files[i])
        }
      } else if (input$export_format == "Mplus") {
        for (i in 1:length(values$pv_obj[["pv"]])) {
          write.table(values$pv_obj[["pv"]][[i]], file = files[i],
                      dec = ".", sep = ",", row.names = FALSE)
        }
        write(x = paste0(files[-length(files)], collapse = "\n"),
              file = files[length(files) - 1])
        write(names(values$pv_obj[["pv"]][[1]]), file = "variable_names.txt")

        write(x = paste0(files[-length(files)], collapse = "\n"),
              file = files[length(files)])
      }

      zip(zipfile = zipfile, files = files)
    },
    contentType = "application/zip"
  )


  # ------------------------ SAVE GGPLOTS ------------------------------------

  # distribution plots for imputations / plausible values
  output$download_plot <- downloadHandler(
    filename = function() {
      req(input$plot_name, input$plot_format)
      ext <- switch(input$plot_format,
                    "png" = ".png",
                    "RData" = ".RData")
      paste0(input$plot_name, ext)
    },
    content = function(file) {
      if (input$plot_format == "RData") {
        gplot <- imputation_plot()
        save(gplot, file = file)
      } else {
        ggplot2::ggsave(filename = file, plot = imputation_plot())
      }
    }
  )

  # cart plot for single variables' imputation
  output$download_cart <- downloadHandler(
    filename = function() {
      req(input$cart_name, input$cart_format)
      ext <- switch(input$cart_format,
                    "png" = ".png",
                    "RData" = ".RData")
      paste0(input$cart_name, ext)
    },
    content = function(file) {
      if (input$cart_format == "RData") {
        gplot <- cart_plot()
        save(gplot, file = file)
      } else {
        ggplot2::ggsave(filename = file, plot = cart_plot())
      }
    }
  )

  # variable_importance plot for single variables' imputation
  output$download_variable_importance <- downloadHandler(
    filename = function() {
      req(input$variable_importance_name, input$variable_importance_format)
      ext <- switch(input$variable_importance_format,
                    "png" = ".png",
                    "RData" = ".RData")
      paste0(input$variable_importance_name, ext)
    },
    content = function(file) {
      if (input$variable_importance_format == "RData") {
        gplot <- variable_importance_plot()
        save(gplot, file = file)
      } else {
        ggplot2::ggsave(filename = file, plot = variable_importance_plot())
      }
    }
  )

  # ------------------------ SAVE TABLES -------------------------------------

  # output$download_descriptive <- renderUI({
  #   downloadButton('download_descriptive', label = 'Download Descriptives') })

  output$download_descriptive <- downloadHandler(
    filename = function() {
      # req(input$descriptive_name, input$descriptive_format)
      # ext <- switch(input$descriptive_format,
      #               "tsv" = ".tsv",
      #               "LaTeX" = ".tex")
      # paste0(input$descriptive_name, ext)
      req(input$descriptive_name)
      paste0(input$descriptive_name, ".tsv")
    },
    content = function(file) {
      # if (input$descriptive_format == "tsv") {
      #   vroom::vroom_write(imputation_table(), file, progress = FALSE)
      # } else {
      #   vroom::vroom_write_lines(
      #     xtable(imputation_table(),
      #            caption = "Descriptive statistics",
      #            label = "tab:desc"),
      #     file)
      # }
      write.table(x = imputation_table(), file = file, sep = "\t",
                  quote = FALSE, row.names = FALSE)
    }
  )


  output$download_regression <- downloadHandler(
    filename = function() {
      req(input$regression_name)
      paste0(input$regression_name, ".tsv")
    },
    content = function(file) {
      write.table(x = regression_table(), file = file, sep = "\t",
                  quote = FALSE, row.names = FALSE)
    }
  )
})
