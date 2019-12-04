#' complement control lists for missing data handling
#'   and plausible values estimation
#'
#' @param c_E control list for EAP return
#' @param c_W control list for WLE estimation and return
#' @param c_T control list for TAM estimation
#'
#' @noRd

complement_control_lists <- function( # c_WLE, c_C,
                                     c_E, c_W, c_T) {
  res <- list()

  # default values
  # control_Bayes = list(itermcmc = 10000, burnin = 2000, thin = 1, tdf = 10,
  #                     cartctrl1 = 5, cartctrl2 = 0.0001, est.alpha = FALSE)
  control_ML <- list(
    nmi = 10L, ntheta = 2000, normal.approx = FALSE,
    samp.regr = FALSE, theta.model = FALSE, np.adj = 8,
    na.grid = 5, itermcmc = 100, burnin = 50, thin = 1,
    cartctrl1 = 5, cartctrl2 = 0.0001
  )
  # control_Bayes[names(c_C)] <- c_C
  control_ML[names(c_T)] <- c_T

  if (is.null(c_E)) {
    res[["EAP"]] <- FALSE
  } else {
    res[["EAP"]] <- c_E
  }
  if (is.null(c_W)) {
    res[["WLE"]] <- FALSE
  } else {
    res[["WLE"]] <- c_W
  }
  # res[['Bayes']] <- control_Bayes
  res[["ML"]] <- control_ML


  res
}
