#' perform multiple imputation via cart
#'
#' @param bgdata ...
#' @param verbose ...
#' @param control ...
#'
#' @noRd

impute_missing_data <- function(bgdata, verbose, control) {
  imp <- frmY <- NULL
  if (!is.null(bgdata)) {
    if (any(is.na(bgdata))) {
      if (verbose) {
        cat(
          "Begin multiple imputation of missing background data... ",
          paste(Sys.time()), "\n"
        )
        flush.console()
      }
      imp <- CART(
        X = bgdata, itermcmc = control$ML$itermcmc,
        burnin = control$ML$burnin, nmi = control$ML$nmi,
        thin = control$ML$thin, cartctrl1 = control$ML$cartctrl1,
        cartctrl2 = control$ML$cartctrl2, verbose = verbose
      )
    } else {
      bgdata <- as.data.frame(lapply(bgdata, as.numeric))
      imp <- NULL
      frmY <-
        as.formula(
          paste("~", paste(colnames(bgdata[, -which(colnames(bgdata) == "ID_t"),
            drop = FALSE
          ]),
          collapse = "+"
          ))
        )
    }
  }
  list(imp = imp, frmY = frmY, bgdata = bgdata)
}
