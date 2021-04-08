#' perform multiple imputation via cart
#'
#' @param bgdata data.frame of background data (incl. ID_t) or NULL
#' @param verbose logical; whether stuff should be printed to the console
#' @param control list of arguments for mice algorithm
#'
#' @noRd

impute_missing_data <- function(bgdata, verbose, control) {
  imp <- frmY <- treeplot <- variable_importance <- NULL
  if (!is.null(bgdata)) {
    if (any(is.na(bgdata))) {
      if (verbose) {
        message(
          "Begin multiple imputation of missing background data... ",
          paste(Sys.time())
        )
        flush.console()
      }
      res <- CART(X = bgdata, minbucket = control$ML$minbucket,
                  cp = control$ML$cp, nmi = control$ML$nmi, verbose = verbose)
      imp <- res$imp
      treeplot <- res$treeplot
      variable_importance <- res$variable_importance
    } else {
      imp <- NULL
      frmY <- create_formula(bgdata)
    }
  }
  list(imp = imp, frmY = frmY, bgdata = bgdata, treeplot = treeplot,
       variable_importance = variable_importance)
}

reformat_bgdata_as_numeric <- function(dat) {
  as.data.frame(lapply(dat, as.numeric))
}


#' #' perform multiple imputation via cart
#' #'
#' #' @param bgdata data.frame of background data (incl. ID_t) or NULL
#' #' @param verbose logical; whether stuff should be printed to the console
#' #' @param control list of arguments for mice algorithm
#' #'
#' #' @importFrom mice mice complete
#' #' @noRd
#'
#' impute_missing_data <- function(bgdata, verbose, control) {
#'   imp <- frmY <- loggedEvents <- NULL
#'   if (!is.null(bgdata)) {
#'     if (any(is.na(bgdata))) {
#'       if (verbose) {
#'         cat(
#'           "Begin multiple imputation of missing background data... ",
#'           paste(Sys.time()), "\n"
#'         )
#'         flush.console()
#'       }
#'       predictorMatrix <- matrix(1, ncol = ncol(bgdata), nrow = ncol(bgdata))
#'       diag(predictorMatrix) <- 0
#'       predictorMatrix[, which(names(bgdata) == "ID_t")] <- 0
#'       imp <- mice(data = bgdata, m = control$ML$nmi, method = "cart",
#'                   minbucket = control$ML$minbucket, cp = control$ML$cp,
#'                   printFlag = verbose, predictorMatrix = predictorMatrix)
#'       loggedEvents <- imp$loggedEvents
#'       imp <- complete(imp, "all")
#'     } else {
#'       bgdata <- as.data.frame(lapply(bgdata, as.numeric))
#'       imp <- NULL
#'       frmY <-
#'         as.formula(
#'           paste("~", paste(colnames(bgdata[, -which(colnames(bgdata) == "ID_t"),
#'                                            drop = FALSE
#'           ]),
#'           collapse = "+"
#'           ))
#'         )
#'     }
#'   }
#'   list(imp = imp, frmY = frmY, bgdata = bgdata, loggedEvents = loggedEvents)
#' }
