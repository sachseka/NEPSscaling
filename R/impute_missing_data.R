#' perform multiple imputation via cart
#'
#' @param bgdata ...
#' @param verbose ...
#' @param control ...
#'
#' @importFrom mice mice complete
#' @noRd

impute_missing_data <- function(bgdata, verbose, control) {
  imp <- frmY <- loggedEvents <- NULL
  if (!is.null(bgdata)) {
    if (any(is.na(bgdata))) {
      if (verbose) {
        cat(
          "Begin multiple imputation of missing background data... ",
          paste(Sys.time()), "\n"
        )
        flush.console()
      }
      predictorMatrix <- matrix(1, ncol = ncol(bgdata), nrow = ncol(bgdata))
      diag(predictorMatrix) <- 0
      predictorMatrix[, which(names(bgdata) == "ID_t")] <- 0
      imp <- mice(data = bgdata, m = control$ML$nmi, method = "cart",
                  minbucket = control$ML$minbucket, cp = control$ML$cp,
                  printFlag = verbose, predictorMatrix = predictorMatrix)
      loggedEvents <- imp$loggedEvents
      imp <- complete(imp, "all")
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
  list(imp = imp, frmY = frmY, bgdata = bgdata, loggedEvents = loggedEvents)
}
