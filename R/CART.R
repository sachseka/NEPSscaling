#' Sequential Classification and Regression Trees
#' adapted from package LaRA by J.-C. Gaasch (2017)
#'
#' Multiply imputes missing data using the CART algorithm.
#' @param X data frame containing person-level covariates. They can be numeric or factor
#' variables and contain missing values coded as \code{NA}. Rows of \code{X} correspond to
#' persons and columns correspond to covariates. \code{X} has to contain a person
#' identifier called \code{ID_t}.
#' @param nmi number of multiple imputations; defaults to 10.
#' @param itermcmc number of MCMC iterations.
#' @param burnin number of burnin iterations.
#' @param thin thinning interval, i.e., retain only every \code{thin}th iteration (if argument
#' \code{thin} is used, \code{itermcmc*thin} and \code{burnin*thin} yield the total number of
#' MCMC and burnin iterations).
#' @param cartctrl1 minimum number of observations in any terminal CART node during covariates
#' imputation cycles.
#' @param cartctrl2 complexity parameter. Any CART split that does not decrease the overall
#' lack of fit by a factor of \code{control2} is not attempted during covariates imputation
#' cycles.
#' @details Partially observed variables are imputed in each
#' sampling iteration. Sequential CART (Burgette & Reiter, 2010) are utilized as approximations
#' to the full conditional distributions of missing values in \code{X}.
#' @return list with \code{nmi} imputed data sets.
#' @references Burgette, L. F., & Reiter, J. P. (2010). Multiple imputation for missing data
#' via sequential regression trees. \emph{American Journal of Epidemiology}, \emph{172}(9),
#' 1070-1076.
#' @importFrom stats model.matrix runif rgamma rnorm pnorm qnorm predict
#' @importFrom ucminf ucminf
#' @importFrom rpart rpart rpart.control
#' @importFrom utils setTxtProgressBar txtProgressBar
#'
#' @export
CART <- function(
                 X,
                 itermcmc,
                 burnin,
                 nmi = 10,
                 thin = 1,
                 cartctrl1 = 5,
                 cartctrl2 = 1e-04) {
  if (is.null(X)) {
    stop("X == NULL. No data to impute.")
  }
  if (!any(is.na(X))) {
    stop("X is already complete.")
  }

  X <- data.frame(X)
  ID_t <- X$ID_t
  X <- X[, -which(names(X) == "ID_t"), drop = FALSE]

  XOBS <- !is.na(X)
  XMIS <- is.na(X)
  xmisord <- names(sort(colSums(XMIS)))[sort(colSums(XMIS)) > 0]
  for (k in xmisord) {
    if (length(X[XOBS[, k], k]) == 0) {
      stop(paste(
        k, "does not contain any observed values for the",
        "subsample of test takers. Please remove it from the",
        "background data set."
      ))
    }
    X[XMIS[, k], k] <- sample(X[XOBS[, k], k], sum(XMIS[, k]),
      replace = TRUE
    )
  }

  savemis <- sort(sample((burnin + 1):itermcmc, nmi))
  datalist <- vector("list", nmi)
  names(datalist) <- paste0("Iteration", savemis)
  # CART
  pb <- txtProgressBar(min = 0, max = itermcmc, style = 3)
  for (ii in 1:itermcmc) {
    for (iii in 1:thin) {
      X <- as.data.frame(
        seqcart(X, xmisord, XOBS, XMIS, cartctrl1, cartctrl2)
      )
    }
    # save CART draws
    if (ii %in% savemis) {
      sel <- which(names(datalist) == paste0("Iteration", ii))
      datalist[[sel]] <- data.frame(ID_t = ID_t, X)
    }
    setTxtProgressBar(pb, ii)
  }
  close(pb)
  names(datalist) <- NULL
  return(datalist)
}
