#' model post-processing in cross-sectional estimation
#'
#' @param mod estimated TAM model
#' @param npv number of plausible values to return
#' @param control control list for TAM functions
#' @param Y background data
#' @param Y.pid ID returned by TAM
#' @param eap list of eap values
#' @param i current iteration over background data imputations
#' @param EAP.rel list of eap reliability values
#' @param regr.coeff list of regression coefficients of latent regression
#' @param pvs list of estimated plausible values
#' @param bgdata background data (can be NULL)
#'
#' @noRd

post_process_cross_tam_results <- function(mod, npv, control, Y, Y.pid, eap, i,
                                      EAP.rel, regr.coeff, pvs, bgdata) {

  quiet <- function(x) {
    sink(tempfile())
    on.exit(sink())
    invisible(force(x))
  }

  # impute plausible values
  pmod <- TAM::tam.pv(mod,
    nplausible = npv,
    ntheta = control$ML$ntheta,
    normal.approx = control$ML$normal.approx,
    samp.regr = control$ML$samp.regr,
    theta.model = control$ML$theta.model,
    np.adj = control$ML$np.adj,
    na.grid = control$ML$na.grid,
    verbose = FALSE
  )
  tmp_pvs <- TAM::tampv2datalist(pmod, Y.pid = Y.pid, Y = Y, pvnames = "PV")
  eap[[i]] <-
    dplyr::left_join(eap[[i]],
      mod$person[, grep(
        "pid|EAP",
        names(mod$person)
      )],
      by = c("ID_t" = "pid")
    )
  EAP.rel <- c(EAP.rel, mod$EAP.rel)
  if (i == 1) {
    regr.coeff <- quiet(TAM::tam.se(mod)$beta)
  } else if (i > 1) {
    regr.coeff <- cbind(regr.coeff, quiet(TAM::tam.se(mod)$beta))
  }
  pvs[[i]] <- lapply(tmp_pvs, function(x) {
    x[, -which(colnames(x) == "pweights")]
  })
  if (is.null(bgdata)) {
    for (n in 1:npv) {
      names(pvs[[i]][[n]])[which(names(pvs[[i]][[n]]) == "pid")] <-
        "ID_t"
    }
  }
  colnames(eap[[i]]) <- c("ID_t", "eap", "se")

  list(
    eap = eap, regr.coeff = regr.coeff, pvs = pvs, mod = mod,
    EAP.rel = EAP.rel
  )
}
